! zoa-test-runner.f90
! Test runner for Zoa optical design program
!
! Usage: zoa_test <test_script.zoa> [reference_file.ref]
!
! Modes:
!   1 argument:  Run script, write output to stdout (for generating reference files)
!   2 arguments: Run script, compare output to reference file
!
! Exit codes:
!   0 = pass (or no reference file to compare)
!   1 = fail (output differs from reference)
!   2 = error (bad arguments, file not found, etc.)

program zoa_test_runner
  use zoa_output, only: zoa_set_output_handler
  use zoa_test_capture
  use zoa_headless, only: zoa_headless_init
  use zoa_file_handler, only: process_zoa_file
  implicit none

  character(len=512) :: test_file, ref_file
  integer :: nargs, dev_null_unit
  logical :: has_ref, passed
  integer :: num_diffs
  real(8) :: tolerance

  tolerance = 1.0d-6

  nargs = command_argument_count()
  if (nargs < 1 .or. nargs > 2) then
    write(*, '(A)') 'Usage: zoa_test <test_script.zoa> [reference_file.ref]'
    write(*, '(A)') ''
    write(*, '(A)') 'Modes:'
    write(*, '(A)') '  1 arg:  Run script, print output to stdout (generate reference)'
    write(*, '(A)') '  2 args: Run script, compare output to reference file'
    call exit(2)
  end if

  call get_command_argument(1, test_file)
  has_ref = (nargs >= 2)
  if (has_ref) call get_command_argument(2, ref_file)

  ! Suppress PRINT * noise during initialization by redirecting unit 6
  open(newunit=dev_null_unit, file='/dev/null', status='old', action='write')
  open(unit=6, file='/dev/null', status='old', action='write')

  ! Initialize engine headlessly
  ! Capture and discard init output
  call zoa_set_output_handler(capture_handler)
  call zoa_headless_init()
  call clear_capture()

  ! Execute the test script (unit 6 stays suppressed to avoid PRINT noise)
  call process_zoa_file(trim(test_file))

  ! Restore stdout for status/output reporting
  open(unit=6, file='/dev/stdout', status='old', action='write')

  if (has_ref) then
    ! Compare mode
    call compare_with_reference(trim(ref_file), tolerance, passed, num_diffs)
    if (passed) then
      write(*, '(A)') 'PASS: '//trim(test_file)
      call exit(0)
    else
      write(*, '(A,I0,A)') 'FAIL: '//trim(test_file)//' (', num_diffs, ' differences)'
      call exit(1)
    end if
  else
    ! Generate mode — write captured output to stdout
    call write_captured_to_stdout()
  end if

contains

  subroutine write_captured_to_stdout()
    call write_captured_to_file('/dev/stdout')
  end subroutine

end program zoa_test_runner
