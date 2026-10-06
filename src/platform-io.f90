module platform_io
  use iso_c_binding, only: c_int, c_char, c_null_char
  use iso_fortran_env, only: output_unit
  implicit none
  private
  public :: silence_stdout, restore_stdout, plot_temp_path, copy_file, make_directory
  public :: absolute_path, change_directory
  public :: configure_plplot_runtime
  integer(c_int), save :: saved_stdout = -1
  interface
    subroutine configure_plplot_runtime() bind(c, name='zoa_configure_plplot_runtime')
    end subroutine
    function redirect_stdout(saved) bind(c, name='zoa_stdout_redirect') result(rc)
      import c_int
      integer(c_int), value :: saved
      integer(c_int) :: rc
    end function
    function temp_dir(buffer, capacity) bind(c, name='zoa_plot_temp_dir') result(rc)
      import c_int, c_char
      character(c_char) :: buffer(*)
      integer(c_int), value :: capacity
      integer(c_int) :: rc
    end function
    function native_abspath(path, buffer, capacity) bind(c, name='zoa_absolute_path') result(rc)
      import c_char, c_int
      character(c_char), intent(in) :: path(*)
      character(c_char) :: buffer(*)
      integer(c_int), value :: capacity
      integer(c_int) :: rc
    end function
    function native_chdir(path) bind(c, name='zoa_change_dir') result(rc)
      import c_char, c_int
      character(c_char), intent(in) :: path(*)
      integer(c_int) :: rc
    end function
    function native_mkdir(path) bind(c, name='zoa_make_dir') result(rc)
      import c_char, c_int
      character(c_char), intent(in) :: path(*)
      integer(c_int) :: rc
    end function
    function native_copy(src, dst) bind(c, name='zoa_copy_file') result(rc)
      import c_char, c_int
      character(c_char), intent(in) :: src(*), dst(*)
      integer(c_int) :: rc
    end function
  end interface
contains
  subroutine silence_stdout()
    if (saved_stdout >= 0) error stop 'stdout already suppressed'
    flush(output_unit)
#if defined(WINDOWS) && (defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER))
    open(unit=output_unit, status='scratch', action='write')
    saved_stdout = 0
#else
    saved_stdout = redirect_stdout(-1_c_int)
    if (saved_stdout < 0) error stop 'Cannot suppress stdout'
#endif
  end subroutine

  subroutine restore_stdout()
    integer(c_int) :: rc
    if (saved_stdout < 0) error stop 'stdout is not suppressed'
    flush(output_unit)
#if defined(WINDOWS) && (defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER))
    ! Intel reconnects a closed standard unit to its original device.
    close(output_unit)
    rc = 0
#else
    rc = redirect_stdout(saved_stdout)
#endif
    saved_stdout = -1
    if (rc < 0) error stop 'Cannot restore stdout'
  end subroutine

  function plot_temp_path(basename) result(path)
    character(len=*), intent(in) :: basename
    character(len=512) :: path
    character(c_char) :: buffer(512)
    integer :: i, n
    if (temp_dir(buffer, 512_c_int) /= 0) error stop 'Cannot create plot directory'
    path = ''
    n = 0
    do i = 1, size(buffer)
      if (buffer(i) == c_null_char) exit
      n = n + 1
      path(n:n) = buffer(i)
    end do
    if (n + 1 + len_trim(basename) > len(path)) error stop 'Plot path too long'
    path = path(:n)//'/'//trim(basename)
  end function

  ! The absolute, normalized form of path; stops if it cannot be formed.
  function absolute_path(path) result(abspath)
    character(len=*), intent(in) :: path
    character(len=1024) :: abspath
    character(c_char) :: buffer(1024)
    integer :: i
    if (native_abspath(trim(path)//c_null_char, buffer, 1024_c_int) /= 0) &
      error stop 'Cannot form an absolute path'
    abspath = ''
    do i = 1, size(buffer)
      if (buffer(i) == c_null_char) exit
      abspath(i:i) = buffer(i)
    end do
  end function

  ! Make path the current directory; stops if it cannot.
  subroutine change_directory(path)
    character(len=*), intent(in) :: path
    if (native_chdir(trim(path)//c_null_char) /= 0) error stop 'Cannot change directory'
  end subroutine

  ! Create a directory (and missing parents); stops if it cannot.
  subroutine make_directory(path)
    character(len=*), intent(in) :: path
    if (native_mkdir(trim(path)//c_null_char) /= 0) error stop 'Cannot create directory'
  end subroutine

  subroutine copy_file(source, destination)
    character(len=*), intent(in) :: source, destination
    if (native_copy(trim(source)//c_null_char, trim(destination)//c_null_char) /= 0) &
      error stop 'Cannot copy file'
  end subroutine
end module
