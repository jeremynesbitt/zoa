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

  ! Execute one or more semicolon-separated commands
  subroutine execute_commands(cmd_string)
    character(len=*), intent(in) :: cmd_string
    character(len=256) :: single_cmd
    integer :: pos, start, slen

    slen = len_trim(cmd_string)
    start = 1

    do while (start <= slen)
      ! Find next semicolon
      pos = index(cmd_string(start:slen), ';')
      if (pos == 0) then
        single_cmd = adjustl(cmd_string(start:slen))
        start = slen + 1
      else
        single_cmd = adjustl(cmd_string(start:start+pos-2))
        start = start + pos
      end if

      ! Skip empty commands
      if (len_trim(single_cmd) == 0) cycle

      ! Execute via PROCESKDP
      call PROCESKDP(trim(single_cmd))
    end do
  end subroutine

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
