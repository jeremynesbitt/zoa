! zoa-test-runner.f90
! Test runner for Zoa optical design program
!
! Usage: zoa_test <test_script.zoa> [reference_file.ref]
!
! Modes:
!   1 argument:  Run script, write output to stdout (for generating reference files)
!   2 arguments: Run script, compare output to reference file
!
! Plot PNGs:
!   In compare mode each rendered plot (/tmp/zoa_plot_<N>.png) is checked for a
!   valid PNG/dimensions AND compared byte-for-byte against a baseline
!   <ref>.p<N>.png (Cairo rendering is deterministic).  A mismatch fails the test
!   (PNGDIFF); a missing baseline is a non-fatal PNGNOBASE.
!   Regenerate text refs:  ./buildHB/zoa_test test/X.zoa > test/X.ref
!   Regenerate PNG baselines:  ZOA_GEN_PNG_BASELINE=1 ./buildHB/zoa_test test/X.zoa test/X.ref
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
  use zoa_plot_output, only: enable_plot_announcements
  implicit none

  character(len=512) :: test_file, ref_file
  integer :: nargs, dev_null_unit
  logical :: has_ref, passed, gen_png_baseline
  integer :: num_diffs, png_fails
  real(8) :: tolerance
  character(len=32) :: envval

  tolerance = 1.0d-6

  ! When ZOA_GEN_PNG_BASELINE is set, save each rendered plot PNG as the baseline
  ! (<ref>.p<N>.png) instead of comparing -- used to (re)generate baselines.
  call get_environment_variable('ZOA_GEN_PNG_BASELINE', envval)
  gen_png_baseline = (len_trim(envval) > 0)

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

  ! Enable PNG announcements and clean up stale PNGs from previous runs
  call enable_plot_announcements()
  call execute_command_line('rm -f /tmp/zoa_plot_*.png')

  ! Execute the test script (unit 6 stays suppressed to avoid PRINT noise)
  call process_zoa_file(trim(test_file))

  ! Restore stdout for status/output reporting
  open(unit=6, file='/dev/stdout', status='old', action='write')

  if (has_ref) then
    ! Compare mode
    call compare_with_reference(trim(ref_file), tolerance, passed, num_diffs)
    png_fails = 0
    call check_plot_pngs(png_fails, 6)
    num_diffs = num_diffs + png_fails
    if (num_diffs == 0 .and. passed) then
      write(*, '(A)') 'PASS: '//trim(test_file)
      call exit(0)
    else
      write(*, '(A,I0,A)') 'FAIL: '//trim(test_file)//' (', num_diffs, ' differences)'
      call exit(1)
    end if
  else
    ! Generate mode — write captured output to stdout
    ! PNG status to stderr (unit 0) to avoid contaminating the .ref
    png_fails = 0
    call check_plot_pngs(png_fails, 0)
    call write_captured_to_stdout()
  end if

contains

  subroutine write_captured_to_stdout()
    call write_captured_to_file('/dev/stdout')
  end subroutine

  ! Scan the capture buffer for "PLOT: <basename> [WxH]" lines and
  ! verify each PNG on disk.  Reports PNGFAIL lines to out_unit.
  ! Increments png_fails for every failing check.
  subroutine check_plot_pngs(png_fails, out_unit)
    integer, intent(inout) :: png_fails
    integer, intent(in)    :: out_unit

    integer :: i, n, funit, ios
    character(len=512) :: line, basename, dimstr
    character(len=512) :: fpath
    integer :: sp1, sp2
    integer(8) :: fsize
    logical :: fexist
    integer(1) :: hdr(24)
    integer :: w_announced, h_announced, w_ihdr, h_ihdr
    logical :: has_dim
    character(len=512) :: basepath
    logical :: have_n, base_exist

    n = get_num_captured()
    do i = 1, n
      call get_captured_line_from_buffer(i, line)
      if (line(1:6) /= 'PLOT: ') cycle

      ! Parse "PLOT: basename [WxH]"
      sp1 = 7   ! start of basename
      sp2 = index(line(sp1:), ' ')
      if (sp2 == 0) then
        basename = trim(line(sp1:))
        has_dim = .false.
      else
        sp2 = sp2 + sp1 - 2  ! position of space in line
        basename = line(sp1:sp2)
        dimstr = trim(line(sp2+2:))
        has_dim = (len_trim(dimstr) > 0)
      end if

      fpath = '/tmp/'//trim(basename)

      ! 1. Existence and size check
      fsize = 0_8
      inquire(file=trim(fpath), exist=fexist, size=fsize)
      if (.not. fexist) then
        write(out_unit, '(A)') 'PNGFAIL: '//trim(basename)//' file not found'
        png_fails = png_fails + 1
        cycle
      end if
      if (fsize < 1000_8) then
        write(out_unit, '(A,I0,A)') 'PNGFAIL: '//trim(basename)//' size too small (', fsize, ' bytes)'
        png_fails = png_fails + 1
        cycle
      end if

      ! 2. Read first 24 bytes as stream
      open(newunit=funit, file=trim(fpath), access='stream', form='unformatted', &
           status='old', action='read', iostat=ios)
      if (ios /= 0) then
        write(out_unit, '(A)') 'PNGFAIL: '//trim(basename)//' cannot open for binary read'
        png_fails = png_fails + 1
        cycle
      end if
      read(funit, pos=1, iostat=ios) hdr
      close(funit)
      if (ios /= 0) then
        write(out_unit, '(A)') 'PNGFAIL: '//trim(basename)//' cannot read header bytes'
        png_fails = png_fails + 1
        cycle
      end if

      ! 3. PNG signature: bytes 1-8 = 137 80 78 71 13 10 26 10
      if (iand(int(hdr(1)),255) /= 137 .or. iand(int(hdr(2)),255) /= 80 .or. &
          iand(int(hdr(3)),255) /= 78  .or. iand(int(hdr(4)),255) /= 71 .or. &
          iand(int(hdr(5)),255) /= 13  .or. iand(int(hdr(6)),255) /= 10 .or. &
          iand(int(hdr(7)),255) /= 26  .or. iand(int(hdr(8)),255) /= 10) then
        write(out_unit, '(A)') 'PNGFAIL: '//trim(basename)//' bad PNG signature'
        png_fails = png_fails + 1
        cycle
      end if

      ! 4. IHDR chunk type at bytes 13-16
      if (iand(int(hdr(13)),255) /= ichar('I') .or. &
          iand(int(hdr(14)),255) /= ichar('H') .or. &
          iand(int(hdr(15)),255) /= ichar('D') .or. &
          iand(int(hdr(16)),255) /= ichar('R')) then
        write(out_unit, '(A)') 'PNGFAIL: '//trim(basename)//' IHDR chunk not found'
        png_fails = png_fails + 1
        cycle
      end if

      ! 5. Dimension check if announced
      if (has_dim) then
        ! Parse "WxH" from dimstr
        sp1 = index(dimstr, 'x')
        if (sp1 > 0) then
          read(dimstr(1:sp1-1), *, iostat=ios) w_announced
          read(dimstr(sp1+1:), *, iostat=ios) h_announced
          ! IHDR width = bytes 17-20 big-endian, height = bytes 21-24
          w_ihdr = iand(int(hdr(17)),255)*16777216 + iand(int(hdr(18)),255)*65536 + &
                   iand(int(hdr(19)),255)*256 + iand(int(hdr(20)),255)
          h_ihdr = iand(int(hdr(21)),255)*16777216 + iand(int(hdr(22)),255)*65536 + &
                   iand(int(hdr(23)),255)*256 + iand(int(hdr(24)),255)
          if (w_ihdr /= w_announced .or. h_ihdr /= h_announced) then
            write(out_unit, '(A,I0,A,I0,A,I0,A,I0,A)') &
              'PNGFAIL: '//trim(basename)//' dimension mismatch: announced ', &
              w_announced, 'x', h_announced, ' got ', w_ihdr, 'x', h_ihdr, ''
            png_fails = png_fails + 1
            cycle
          end if
        end if
      end if

      ! 6. Content comparison against baseline <ref>.p<N>.png.  Cairo rendering is
      !    deterministic, so plots are compared byte-for-byte.  Baseline number N
      !    is parsed from the basename "zoa_plot_<N>.png".
      if (.not. has_ref) then
        ! Generate mode: sanity only (no reference to compare/store against).
        write(out_unit, '(A)') 'PNGOK: '//trim(basename)
        cycle
      end if

      call build_baseline_path(trim(basename), trim(ref_file), basepath, have_n)
      if (.not. have_n) then
        write(out_unit, '(A)') 'PNGOK: '//trim(basename)//' (sanity only; plot name not parsed)'
      else if (gen_png_baseline) then
        call execute_command_line('cp '//trim(fpath)//' '//trim(basepath))
        write(out_unit, '(A)') 'PNGSAVED: '//trim(basepath)
      else
        inquire(file=trim(basepath), exist=base_exist)
        if (.not. base_exist) then
          ! No baseline yet -- sanity passed, but no content check possible.
          write(out_unit, '(A)') 'PNGNOBASE: '//trim(basename)//' (no baseline '//trim(basepath)//')'
        else if (files_equal(trim(fpath), trim(basepath))) then
          write(out_unit, '(A)') 'PNGOK: '//trim(basename)
        else
          write(out_unit, '(A)') 'PNGDIFF: '//trim(basename)//' differs from baseline '//trim(basepath)
          png_fails = png_fails + 1
        end if
      end if
    end do
  end subroutine check_plot_pngs

  ! Build the baseline path <ref>.p<N>.png from a plot basename "zoa_plot_<N>.png".
  subroutine build_baseline_path(bname, refpath, bpath, ok)
    character(len=*), intent(in)  :: bname, refpath
    character(len=*), intent(out) :: bpath
    logical, intent(out) :: ok
    integer :: us, dot
    ok = .false.
    bpath = ''
    us = index(bname, 'zoa_plot_')
    if (us == 0) return
    us = us + len('zoa_plot_')
    dot = index(bname(us:), '.')
    if (dot <= 1) return
    bpath = trim(refpath)//'.p'//bname(us:us+dot-2)//'.png'
    ok = .true.
  end subroutine

  ! Byte-exact file comparison (reads both fully; PNGs are ~100 KB).
  function files_equal(a, b) result(equal)
    character(len=*), intent(in) :: a, b
    logical :: equal
    integer :: ua, ub, ios
    integer(8) :: sza, szb
    integer(1), allocatable :: bufa(:), bufb(:)
    equal = .false.
    inquire(file=a, size=sza)
    inquire(file=b, size=szb)
    if (sza /= szb .or. sza <= 0) return
    allocate(bufa(sza), bufb(szb))
    open(newunit=ua, file=a, access='stream', form='unformatted', status='old', action='read', iostat=ios)
    if (ios /= 0) return
    read(ua, iostat=ios) bufa
    close(ua)
    if (ios /= 0) return
    open(newunit=ub, file=b, access='stream', form='unformatted', status='old', action='read', iostat=ios)
    if (ios /= 0) return
    read(ub, iostat=ios) bufb
    close(ub)
    if (ios /= 0) return
    equal = all(bufa == bufb)
  end function

end program zoa_test_runner
