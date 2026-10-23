! OPD reference sphere location (CODE V RSL).
!
! The image-side reference sphere is centred where the real chief ray meets
! the image surface.  Its radius -- the distance back to the reference sphere
! (exit pupil) -- is, by default, computed per field from the exit pupil
! (RSL DEF).  RSL value uses that radius for every field (sign as for the
! computed radius: positive = the sphere lies before the image).  RSL INF is
! the limit of an ever larger sphere, evaluated in closed form by the OPD
! routines (rsl_inf_ocor): the correction becomes the ray direction dotted
! with the ray's offset from the chief ray at the image.  (Not KDP's flat
! plane through the image point, which is meant for afocal/telecentric
! cases and reads a converging beam as a huge defocus.)
!
! Leaf module (no Zoa dependencies) so the legacy OPD routines can read it.
module ref_sphere_settings
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: RSL_MODE_DEF, RSL_MODE_INF, RSL_MODE_VALUE
  public :: rsl_set, rsl_mode, rsl_value, rsl_radius, rsl_reset, rsl_inf_ocor

  integer, parameter :: RSL_MODE_DEF = 0, RSL_MODE_INF = 1, RSL_MODE_VALUE = 2

  integer      :: mode  = RSL_MODE_DEF
  real(real64) :: value = 0.0_real64

contains

  subroutine rsl_set(newMode, newValue)
    integer, intent(in) :: newMode
    real(real64), intent(in), optional :: newValue
    mode = newMode
    if (present(newValue)) value = newValue
  end subroutine

  subroutine rsl_reset()
    mode = RSL_MODE_DEF
    value = 0.0_real64
  end subroutine

  integer function rsl_mode()
    rsl_mode = mode
  end function

  real(real64) function rsl_value()
    rsl_value = value
  end function

  ! The reference sphere radius to use, given the one computed from the
  ! exit pupil.
  real(real64) function rsl_radius(computed)
    real(real64), intent(in) :: computed
    select case (mode)
    case (RSL_MODE_VALUE); rsl_radius = value
    case default;          rsl_radius = computed   ! DEF; INF: see rsl_inf_ocor
    end select
  end function

  ! OCOR for a reference sphere of infinite radius (with RCOR = 0): the
  ! large-R limit of the finite-sphere correction.  p = ray point and k = ray
  ! direction at the image; c = chief ray point there; chiefN = the chief
  ! ray's z direction cosine (its sign picks the sphere side, as in the
  ! finite case).
  real(real64) function rsl_inf_ocor(p, k, c, chiefN)
    real(real64), intent(in) :: p(3), k(3), c(3), chiefN
    rsl_inf_ocor = sign(1.0_real64, chiefN)*dot_product(k, p - c)
  end function

end module ref_sphere_settings
