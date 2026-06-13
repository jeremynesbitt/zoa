! zoa-server.f90
! ZeroMQ-based headless Zoa server for Jupyter integration
!
! Listens on a REP socket for command strings, executes them
! via PROCESKDP, captures all output, and sends it back.
!
! Protocol:
!   Client sends: command string (e.g. "FIR" or "RES doublet; LIS")
!   Server sends: captured output text (all lines joined with newlines)
!   Special commands:
!     "__QUIT__"  — shut down the server
!     "__PING__"  — server responds with "PONG"

program zoa_server
  use zmq_binding
  use zoa_headless
  use zoa_output, only: zoa_set_output_handler
  use zoa_test_capture, only: capture_handler, clear_capture, &
                               get_num_captured
  use zoa_plot_output, only: plot_was_generated, last_plot_file, &
                              clear_plot_output
  use result_builder, only: result_has_data, result_to_json, result_clear
  use iso_c_binding
  implicit none

  type(c_ptr) :: ctx, sock
  integer(c_int) :: rc, nbytes, linger_val
  character(len=8192) :: cmd_buf
  character(len=1024) :: endpoint
  character(len=:), allocatable :: reply
  logical :: running
  integer :: i

  ! Default endpoint
  endpoint = 'tcp://*:5555'

  ! Check command-line arguments for custom port
  if (command_argument_count() >= 1) then
    call get_command_argument(1, endpoint)
  end if

  ! Initialize headless Zoa engine
  ! Redirect stdout to /dev/null during init to suppress PRINT * noise
  open(unit=6, file='/dev/null', status='old')
  call zoa_headless_init()
  open(unit=6, file='/dev/stdout', status='old')

  ! Register capture handler for all output
  call zoa_set_output_handler(capture_handler)

  ! Set up ZeroMQ
  ctx = zmq_ctx_new()
  if (.not. c_associated(ctx)) then
    write(*, '(A)') 'ERROR: Failed to create ZMQ context'
    stop 2
  end if

  sock = zmq_socket(ctx, ZMQ_REP)
  if (.not. c_associated(sock)) then
    write(*, '(A)') 'ERROR: Failed to create ZMQ socket'
    rc = zmq_ctx_term(ctx)
    stop 2
  end if

  rc = zmq_bind(sock, trim(endpoint)//c_null_char)
  if (rc /= 0) then
    write(*, '(A,A)') 'ERROR: Failed to bind to ', trim(endpoint)
    rc = zmq_close(sock)
    rc = zmq_ctx_term(ctx)
    stop 2
  end if

  write(*, '(A,A)') 'Zoa server listening on ', trim(endpoint)
  write(*, '(A)') 'Send "__QUIT__" to shut down'

  ! Main server loop
  running = .true.
  do while (running)
    ! Wait for next command
    nbytes = zmq_recv_string(sock, cmd_buf, 0_c_int)

    if (nbytes <= 0) then
      ! Send error reply and continue
      rc = zmq_send_string(sock, 'ERROR: empty request', 0_c_int)
      cycle
    end if

    ! Check for special commands
    if (trim(cmd_buf) == '__QUIT__') then
      rc = zmq_send_string(sock, 'BYE', 0_c_int)
      running = .false.
      cycle
    end if

    if (trim(cmd_buf) == '__PING__') then
      rc = zmq_send_string(sock, 'PONG', 0_c_int)
      cycle
    end if

    ! __JSON__ <command> — run command and return structured JSON result
    if (cmd_buf(1:9) == '__JSON__ ') then
      call handle_json_command(cmd_buf(10:), sock)
      cycle
    end if

    ! Execute the command(s) and capture output
    call clear_capture()
    call clear_plot_output()

    ! Redirect stdout during command execution to suppress PRINT * noise
    open(unit=6, file='/dev/null', status='old')
    call execute_commands(trim(cmd_buf))
    open(unit=6, file='/dev/stdout', status='old')

    ! Build reply from captured output
    ! If a plot was generated, append __IMAGE__ marker
    reply = build_reply()
    if (plot_was_generated) then
      if (len_trim(reply) > 0) then
        reply = trim(reply) // char(10) // '__IMAGE__' // trim(last_plot_file)
      else
        reply = '__IMAGE__' // trim(last_plot_file)
      end if
    end if
    rc = zmq_send_string(sock, reply, 0_c_int)
    deallocate(reply)
  end do

  ! Clean up
  linger_val = 0
  rc = zmq_setsockopt(sock, ZMQ_LINGER, linger_val, &
       int(c_sizeof(linger_val), c_size_t))
  rc = zmq_close(sock)
  rc = zmq_ctx_term(ctx)

  write(*, '(A)') 'Zoa server shut down'

contains

  ! Execute one or more semicolon- or newline-separated commands
  subroutine execute_commands(cmd_string)
    use zoa_ui_callbacks, only: notify_replot_flush
    character(len=*), intent(in) :: cmd_string
    character(len=256) :: single_cmd
    integer :: pos, pos_semi, pos_nl, start, slen

    slen = len_trim(cmd_string)
    start = 1

    do while (start <= slen)
      ! Find next semicolon or newline — whichever comes first
      pos_semi = index(cmd_string(start:slen), ';')
      pos_nl   = index(cmd_string(start:slen), char(10))

      if (pos_semi == 0 .and. pos_nl == 0) then
        single_cmd = adjustl(cmd_string(start:slen))
        start = slen + 1
      else if (pos_semi == 0) then
        single_cmd = adjustl(cmd_string(start:start+pos_nl-2))
        start = start + pos_nl
      else if (pos_nl == 0) then
        single_cmd = adjustl(cmd_string(start:start+pos_semi-2))
        start = start + pos_semi
      else
        ! Both found — use whichever is earlier
        pos = min(pos_semi, pos_nl)
        single_cmd = adjustl(cmd_string(start:start+pos-2))
        start = start + pos
      end if

      ! Skip empty commands
      if (len_trim(single_cmd) == 0) cycle

      ! Execute via PROCESKDP
      call PROCESKDP(trim(single_cmd))

      ! Drain any pending replot so lens-modifying commands record an undo snapshot
      call notify_replot_flush()
    end do
  end subroutine

  ! Handle __JSON__ <command>: run the command, return JSON if structured data
  ! was produced, else fall back to text or a small JSON error object.
  !
  ! For "SEI" specifically we call MMAB3_NEW directly (same as execTHO) so that
  ! CSeidel is populated in headless mode.  For all other commands we run them
  ! through execute_commands() which may or may not call result_begin.
  subroutine handle_json_command(real_cmd, sock)
    use global_widgets, only: sysConfig
    character(len=*),  intent(in) :: real_cmd
    type(c_ptr),       intent(in) :: sock
    character(len=:), allocatable :: json_reply
    character(len=256) :: ucmd
    integer :: k, rc2
    interface
      subroutine MMAB3_NEW(YFLAG, idxWV, printTable)
        logical, intent(in) :: YFLAG
        integer, intent(in) :: idxWV
        logical, optional, intent(in) :: printTable
      end subroutine
    end interface

    ! Clear any stale structured result from a previous command
    call result_clear()

    ! Upper-case first token for dispatch check
    ucmd = adjustl(real_cmd)
    do k = 1, len_trim(ucmd)
      if (ucmd(k:k) >= 'a' .and. ucmd(k:k) <= 'z') &
        ucmd(k:k) = char(ichar(ucmd(k:k)) - 32)
    end do

    call clear_capture()
    call clear_plot_output()
    open(unit=6, file='/dev/null', status='old')

    ! SEI / THO both use MMAB3_NEW; call it directly to work headlessly
    if (trim(ucmd) == 'SEI' .or. trim(ucmd) == 'THO') then
      call MMAB3_NEW(.TRUE., sysConfig%refWavelengthIndex, .TRUE.)
    else if (trim(ucmd) == 'FITZERN') then
      ! Run the full prerequisite sequence before the fit so the caller
      ! does not have to issue FOB/CAPFN manually first.  This mirrors
      ! what zern_go() does in plot-functions.f90.
      !   FOB 0.0   — select on-axis field point
      !   CAPFN     — trace ray grid, populate OPD data
      !   FITZERN,N — SVD fit using reference wavelength N
      block
        character(len=32) :: wv_str
        write(wv_str, '(I0)') sysConfig%refWavelengthIndex
        call execute_commands('FOB 0.0')
        call execute_commands('CAPFN')
        call execute_commands('FITZERN, '//trim(wv_str))
      end block
      call ZERNIKE_BUILD_RESULT()
    else if (trim(ucmd) == 'WAVEFRONT' .or. trim(ucmd) == 'OPD') then
      ! Run FOB 0.0 and CAPFN headlessly to populate DSPOTT with OPD data,
      ! then assemble the 2-D OPD map (KKK x KKK, values in waves) and
      ! serialize it as a base64-f64-le grid.
      !   __JSON__ WAVEFRONT   — on-axis OPD map at reference wavelength
      !   __JSON__ OPD         — alias
      call execute_commands('FOB 0.0')
      call execute_commands('CAPFN')
      call WAVEFRONT_BUILD_RESULT()
    else
      call execute_commands(trim(real_cmd))
    end if

    open(unit=6, file='/dev/stdout', status='old')

    if (result_has_data) then
      json_reply = result_to_json()
    else
      ! Fall back: return a small JSON error with the captured text appended
      json_reply = '{"schema":"zoa.result/1","type":"error","messages":' // &
                   '["no structured data for: ' // trim(real_cmd) // '"]}'
    end if

    rc2 = zmq_send_string(sock, json_reply, 0_c_int)
    deallocate(json_reply)
  end subroutine handle_json_command

  ! Build a single reply string from all captured output lines
  function build_reply() result(reply_str)
    use zoa_test_capture, only: get_num_captured, get_captured_line_from_buffer
    character(len=:), allocatable :: reply_str
    character(len=512) :: line_buf
    integer :: i, n, total_len

    n = get_num_captured()
    if (n == 0) then
      reply_str = ''
      return
    end if

    ! Calculate total length needed
    total_len = 0
    do i = 1, n
      call get_captured_line_from_buffer(i, line_buf)
      total_len = total_len + len_trim(line_buf) + 1  ! +1 for newline
    end do

    allocate(character(len=total_len) :: reply_str)
    reply_str = ''

    do i = 1, n
      call get_captured_line_from_buffer(i, line_buf)
      if (i == 1) then
        reply_str = trim(line_buf)
      else
        reply_str = trim(reply_str) // char(10) // trim(line_buf)
      end if
    end do
  end function

end program zoa_server
