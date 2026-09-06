! Low-level binary I/O helpers for the .zin (Zemax .ZDA analog) companion
! file. Pure serialization primitives only -- no plot/GTK knowledge here.
! zoa_plot and plot_setting_manager `use` this module, so it must not
! depend on either of them (or on anything that pulls in GTK) to avoid a
! module dependency cycle.
module mod_zin_io
  use iso_fortran_env, only: int32
  use GLOBALS, only: zoaVersion
  implicit none
  private

  character(len=8), parameter, public :: ZIN_MAGIC = 'ZOAZIN  '
  integer(int32), parameter, public :: ZIN_FORMAT_VERSION = 1_int32

  ! Top-level record kind tags
  integer(int32), parameter, public :: ZIN_KIND_DATA   = 1
  integer(int32), parameter, public :: ZIN_KIND_REPLAY = 2

  ! Plot-kind tags (used by multiplot serialization to pick the dynamic type
  ! back on load)
  integer(int32), parameter, public :: ZIN_PLOT_BASIC = 1
  integer(int32), parameter, public :: ZIN_PLOT_3D    = 2
  integer(int32), parameter, public :: ZIN_PLOT_IMG   = 3
  integer(int32), parameter, public :: ZIN_PLOT_BAR   = 4

  public :: zin_write_str, zin_read_str
  public :: zin_write_logical, zin_read_logical
  public :: zin_write_header, zin_read_header

contains

  ! Length-prefixed string: int32 len_trim(s), then that many bytes.
  subroutine zin_write_str(unit, s)
    integer, intent(in) :: unit
    character(len=*), intent(in) :: s
    integer(int32) :: n

    n = int(len_trim(s), int32)
    write(unit) n
    if (n > 0) write(unit) s(1:n)
  end subroutine zin_write_str

  ! Reads a length-prefixed string into s (blank-padded). If the stored
  ! length exceeds len(s), the string is truncated to fit. ios is nonzero
  ! on any I/O error.
  subroutine zin_read_str(unit, s, ios)
    integer, intent(in) :: unit
    character(len=*), intent(out) :: s
    integer, intent(out) :: ios
    integer(int32) :: n
    character(len=:), allocatable :: buf

    s = ' '
    read(unit, iostat=ios) n
    if (ios /= 0) return
    if (n <= 0) return

    allocate(character(len=n) :: buf)
    read(unit, iostat=ios) buf
    if (ios /= 0) then
      deallocate(buf)
      return
    end if

    if (int(n) > len(s)) then
      s = buf(1:len(s))
    else
      s(1:int(n)) = buf
    end if
    deallocate(buf)
  end subroutine zin_read_str

  subroutine zin_write_logical(unit, l)
    integer, intent(in) :: unit
    logical, intent(in) :: l
    integer(int32) :: iv

    iv = 0_int32
    if (l) iv = 1_int32
    write(unit) iv
  end subroutine zin_write_logical

  subroutine zin_read_logical(unit, l, ios)
    integer, intent(in) :: unit
    logical, intent(out) :: l
    integer, intent(out) :: ios
    integer(int32) :: iv

    l = .false.
    read(unit, iostat=ios) iv
    if (ios /= 0) return
    l = (iv /= 0_int32)
  end subroutine zin_read_logical

  ! File header: ZIN_MAGIC, ZIN_FORMAT_VERSION, zoaVersion (char(10),
  ! padded), numTabs (int32). Fixed-size fields, not length-prefixed.
  subroutine zin_write_header(unit, numTabs)
    integer, intent(in) :: unit
    integer, intent(in) :: numTabs
    integer(int32) :: fv, nt
    character(len=10) :: verField

    write(unit) ZIN_MAGIC
    fv = ZIN_FORMAT_VERSION
    write(unit) fv
    verField = zoaVersion
    write(unit) verField
    nt = int(numTabs, int32)
    write(unit) nt
  end subroutine zin_write_header

  ! Reads and validates the header at pos=1. On any failure ok=.false. and
  ! a clear message is emitted -- never crashes.
  subroutine zin_read_header(unit, numTabs, ok)
    use zoa_output, only: zoa_emit
    integer, intent(in) :: unit
    integer, intent(out) :: numTabs
    logical, intent(out) :: ok
    character(len=8) :: magic
    integer(int32) :: fv, nt
    character(len=10) :: verField
    integer :: ios
    character(len=20) :: numStr

    ok = .false.
    numTabs = 0

    read(unit, pos=1, iostat=ios) magic
    if (ios /= 0) then
      call zoa_emit("ZIN: cannot read file header", "red")
      return
    end if
    if (magic /= ZIN_MAGIC) then
      call zoa_emit("ZIN: not a Zoa .zin companion file (bad magic)", "red")
      return
    end if

    read(unit, iostat=ios) fv
    if (ios /= 0) then
      call zoa_emit("ZIN: cannot read format version", "red")
      return
    end if
    if (fv /= ZIN_FORMAT_VERSION) then
      write(numStr, '(I0)') fv
      call zoa_emit("ZIN: unsupported .zin format version "//trim(numStr), "red")
      return
    end if

    read(unit, iostat=ios) verField
    if (ios /= 0) then
      call zoa_emit("ZIN: cannot read Zoa version field", "red")
      return
    end if

    read(unit, iostat=ios) nt
    if (ios /= 0) then
      call zoa_emit("ZIN: cannot read tab count", "red")
      return
    end if

    numTabs = int(nt)
    ok = .true.
  end subroutine zin_read_header

end module mod_zin_io
