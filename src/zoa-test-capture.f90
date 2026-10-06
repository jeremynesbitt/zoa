! zoa-test-capture.f90
! Output capture module for Zoa test infrastructure
!
! Provides an output handler that captures all zoa_emit output
! into an in-memory buffer for later comparison against reference files.

module zoa_test_capture
  use zoa_output, only: output_handler_iface
  implicit none
  private
  public :: capture_handler, clear_capture, get_num_captured
  public :: write_captured_to_file, compare_with_reference
  public :: get_captured_line_from_buffer, get_captured_line

  ! Lines of any length: Data tabs print whole data rows (an image row per
  ! line), which must be captured and compared completely.
  integer, parameter :: MAX_LINES = 100000
  integer, parameter :: MAX_LINE_LEN = 512   ! token buffers only
  type :: text_line
    character(len=:), allocatable :: s
  end type
  type(text_line), allocatable :: captured(:)
  integer :: num_captured = 0

contains

  subroutine capture_handler(text, color)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: color
    if (.not. allocated(captured)) allocate(captured(MAX_LINES))
    if (num_captured < MAX_LINES) then
      num_captured = num_captured + 1
      captured(num_captured)%s = text
    end if
  end subroutine

  subroutine clear_capture()
    num_captured = 0
  end subroutine

  function get_num_captured() result(n)
    integer :: n
    n = num_captured
  end function

  subroutine write_captured_to_file(filename)
    character(len=*), intent(in) :: filename
    integer :: i, iu, ios
    open(newunit=iu, file=filename, status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'ERROR: Cannot open output file: '//trim(filename)
      return
    end if
    do i = 1, num_captured
      write(iu, '(A)') trim(captured(i)%s)
    end do
    close(iu)
  end subroutine

  subroutine compare_with_reference(ref_file, tolerance, passed, num_diffs)
    character(len=*), intent(in) :: ref_file
    real(8), intent(in) :: tolerance
    logical, intent(out) :: passed
    integer, intent(out) :: num_diffs

    character(len=:), allocatable :: ref_line
    integer :: iu, ios, ref_count, i
    logical :: lines_match

    passed = .true.
    num_diffs = 0
    ref_count = 0

    open(newunit=iu, file=ref_file, status='old', action='read', iostat=ios)
    if (ios /= 0) then
      write(*, '(A)') 'ERROR: Cannot open reference file: '//trim(ref_file)
      passed = .false.
      num_diffs = -1
      return
    end if

    do i = 1, num_captured
      call read_line(iu, ref_line, ios)
      if (ios /= 0) then
        ! Reference file has fewer lines
        write(*, '(A,I0,A)') 'DIFF: Output has more lines than reference (', &
          num_captured, ' vs captured so far)'
        passed = .false.
        num_diffs = num_diffs + (num_captured - i + 1)
        close(iu)
        return
      end if
      ref_count = ref_count + 1

      call compare_lines(trim(captured(i)%s), trim(ref_line), tolerance, lines_match)
      if (.not. lines_match) then
        num_diffs = num_diffs + 1
        passed = .false.
        if (num_diffs <= 20) then
          write(*, '(A,I0)') 'DIFF at line ', i
          write(*, '(A,A)') '  GOT: ', trim(captured(i)%s)
          write(*, '(A,A)') '  REF: ', trim(ref_line)
        end if
      end if
    end do

    ! Check if reference has more lines
    call read_line(iu, ref_line, ios)
    if (ios == 0) then
      ! Count remaining reference lines
      ref_count = ref_count + 1
      do
        call read_line(iu, ref_line, ios)
        if (ios /= 0) exit
        ref_count = ref_count + 1
      end do
      write(*, '(A,I0,A,I0,A)') 'DIFF: Reference has ', ref_count, &
        ' lines but output has only ', num_captured, ' lines'
      passed = .false.
      num_diffs = num_diffs + (ref_count - num_captured)
    end if

    close(iu)
  end subroutine

  subroutine compare_lines(line1, line2, tolerance, match)
    character(len=*), intent(in) :: line1, line2
    real(8), intent(in) :: tolerance
    logical, intent(out) :: match

    character(len=MAX_LINE_LEN) :: tok1, tok2
    integer :: pos1, pos2, len1, len2
    real(8) :: val1, val2, diff
    integer :: ios1, ios2

    ! First try exact match
    if (line1 == line2) then
      match = .true.
      return
    end if

    ! If not exact, try token-by-token with numeric tolerance
    pos1 = 1
    pos2 = 1
    len1 = len_trim(line1)
    len2 = len_trim(line2)
    match = .true.

    do while (pos1 <= len1 .or. pos2 <= len2)
      call next_token(line1, pos1, len1, tok1)
      call next_token(line2, pos2, len2, tok2)

      if (trim(tok1) == trim(tok2)) cycle

      ! Try numeric comparison
      read(tok1, *, iostat=ios1) val1
      read(tok2, *, iostat=ios2) val2
      if (ios1 == 0 .and. ios2 == 0) then
        ! Combined absolute+relative tolerance (like numpy isclose).
        ! abs_tol=1e-6 handles near-zero values where sign flips from
        ! compiler optimization would otherwise cause spurious failures.
        if (abs(val1 - val2) > 1.0d-6 + tolerance * max(abs(val1), abs(val2))) then
          match = .false.
          return
        end if
      else
        ! Not both numbers and not exact match
        match = .false.
        return
      end if
    end do
  end subroutine

  subroutine next_token(line, pos, line_len, token)
    character(len=*), intent(in) :: line
    integer, intent(inout) :: pos
    integer, intent(in) :: line_len
    character(len=*), intent(out) :: token
    integer :: start, actual_len

    token = ''
    actual_len = min(line_len, len(line))
    ! Skip whitespace
    do while (pos <= actual_len)
      if (line(pos:pos) /= ' ' .and. line(pos:pos) /= char(9)) exit
      pos = pos + 1
    end do
    if (pos > actual_len) return

    start = pos
    ! Read until whitespace
    do while (pos <= actual_len)
      if (line(pos:pos) == ' ' .or. line(pos:pos) == char(9)) exit
      pos = pos + 1
    end do
    token = line(start:pos-1)
  end subroutine

  subroutine get_captured_line_from_buffer(idx, line)
    integer, intent(in) :: idx
    character(len=*), intent(out) :: line
    if (idx >= 1 .and. idx <= num_captured) then
      line = captured(idx)%s
    else
      line = ''
    end if
  end subroutine

  ! The whole captured line idx, whatever its length.
  function get_captured_line(idx) result(line)
    integer, intent(in) :: idx
    character(len=:), allocatable :: line
    if (idx >= 1 .and. idx <= num_captured) then
      line = captured(idx)%s
    else
      line = ''
    end if
  end function

  ! Read one whole line of any length (non-advancing reads in chunks).
  subroutine read_line(iu, line, ios)
    integer, intent(in) :: iu
    character(len=:), allocatable, intent(out) :: line
    integer, intent(out) :: ios
    character(len=4096) :: chunk
    integer :: n
    line = ''
    do
      read(iu, '(A)', advance='no', size=n, iostat=ios) chunk
      line = line//chunk(1:n)
      if (is_iostat_eor(ios)) then
        ios = 0
        return
      end if
      if (ios /= 0) return
    end do
  end subroutine

end module zoa_test_capture
