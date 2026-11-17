! Through-focus range (CODE V FFO / IFO / NFO): focus positions
! FFO, FFO+IFO, ... FFO+(NFO-1)*IFO, measured along the axis from the image
! surface (positive = toward +z of the image space).  System data, like the
! fields and wavelengths: it belongs to the lens, is saved with it and is
! read by the analyses that work through focus (SPO THF; SPO honours FFO).
!
! Leaf module (no Zoa dependencies) so any analysis can read it.
module focus_settings
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: FOCUS_MAX_POSITIONS
  public :: focus_set_first, focus_set_increment, focus_set_count, focus_reset
  public :: focus_first, focus_increment, focus_count, focus_position

  ! CODE V allows up to 18 focus positions
  integer, parameter :: FOCUS_MAX_POSITIONS = 18

  real(real64) :: ffo = 0.0_real64
  real(real64) :: ifo = 0.0_real64
  integer      :: nfo = 1

contains

  subroutine focus_set_first(v)
    real(real64), intent(in) :: v
    ffo = v
  end subroutine

  subroutine focus_set_increment(v)
    real(real64), intent(in) :: v
    ifo = v
  end subroutine

  subroutine focus_set_count(n)
    integer, intent(in) :: n
    nfo = max(1, min(FOCUS_MAX_POSITIONS, n))
  end subroutine

  subroutine focus_reset()
    ffo = 0.0_real64
    ifo = 0.0_real64
    nfo = 1
  end subroutine

  real(real64) function focus_first()
    focus_first = ffo
  end function

  real(real64) function focus_increment()
    focus_increment = ifo
  end function

  integer function focus_count()
    focus_count = nfo
  end function

  ! Focus position k = 1 .. NFO
  real(real64) function focus_position(k)
    integer, intent(in) :: k
    focus_position = ffo + real(k - 1, real64)*ifo
  end function

end module focus_settings
