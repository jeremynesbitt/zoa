! Lens modules (CODE V MOD): the per-surface store, keyed by the module's
! front surface Sk (the rear surface is always Sk+1), and the helpers the
! paraxial traces call.  The ray mapping itself is in lens_module_math.
! Typed data only -- nothing in ALENS.  Surface insert/delete keeps a module
! on its surfaces (lm_surfaces_shifted, from typed_surface_inserted/deleted).
module lens_module
  use iso_fortran_env, only: real64
  use lens_module_math, only: lm_params, lm_par_u, lm_par_y, lm_trace, LM_FIELD_ANGLE, LM_FIELD_IMG
  implicit none
  private
  public :: lm_params, LM_FIELD_ANGLE, LM_FIELD_IMG
  public :: lm_store, lm_is_front, lm_is_rear, lm_any, lm_reset, lm_remove
  public :: lm_surfaces_shifted, lm_par_refract, lm_par_transfer
  public :: lm_params_for, lm_legacy_hit
  ! set once the legacy-tracer fallback warning has been shown; cleared when
  ! the modules change
  logical, public, save :: lm_warned = .false.

  integer, parameter :: MAXS = 499
  type(lm_params), save :: lm_store(0:MAXS)

contains

  ! Surface s is the front of a lens module with optical effect (a module
  ! has none until its focal length is set).
  logical function lm_is_front(s)
    integer, intent(in) :: s
    lm_is_front = .false.
    if (s < 0 .or. s > MAXS) return
    lm_is_front = lm_store(s)%active .and. lm_store(s)%efl /= 0.0_real64
  end function

  logical function lm_is_rear(s)
    integer, intent(in) :: s
    lm_is_rear = .false.
    if (s < 1 .or. s > MAXS) return
    lm_is_rear = lm_is_front(s-1)
  end function

  logical function lm_any()
    lm_any = any(lm_store%active)
  end function

  subroutine lm_reset()
    lm_store = lm_params()
    lm_warned = .false.
  end subroutine

  subroutine lm_remove(s)
    integer, intent(in) :: s
    if (s < 0 .or. s > MAXS) return
    lm_store(s) = lm_params()
  end subroutine

  ! Surface k was inserted (dir = +1) or deleted (dir = -1).  A module whose
  ! front or rear surface is deleted goes with it.
  subroutine lm_surfaces_shifted(k, dir)
    integer, intent(in) :: k, dir
    if (k < 0 .or. k > MAXS) return
    if (dir > 0) then
      lm_store(k+1:MAXS) = lm_store(k:MAXS-1)
      lm_store(k) = lm_params()
    else
      if (k >= 1) lm_store(k-1) = lm_params()      ! k was its rear surface
      lm_store(k:MAXS-1) = lm_store(k+1:MAXS)
      lm_store(MAXS) = lm_params()
    end if
  end subroutine

  ! Paraxial refraction at surface L: the slope after it, from the height at
  ! L and the slope arriving (unchanged unless L is a module's front).
  real(real64) function lm_par_refract(L, y, u_in, u_now) result(u)
    integer, intent(in) :: L
    real(real64), intent(in) :: y, u_in, u_now
    u = u_now
    if (lm_is_front(L)) u = lm_par_u(lm_store(L), y, u_in)
  end function

  ! Paraxial transfer to surface L: y(L-1) + t(L-1)*u(L-1), except across a
  ! module (L-1 its front), where the height carries from H to H'.  u_in is
  ! the slope arriving at L-1 (used only then).
  real(real64) function lm_par_transfer(L, y_prev, u_prev, t, u_in) result(y)
    integer, intent(in) :: L
    real(real64), intent(in) :: y_prev, u_prev, t, u_in
    if (lm_is_front(L-1)) then
      y = lm_par_y(lm_store(L-1), y_prev, u_in, u_prev)
    else
      y = y_prev + t*u_prev
    end if
  end function

  ! The module at front surface s, ready to trace: lam_ref (the reference
  ! wavelength) in lens units.
  function lm_params_for(s) result(p)
    use mod_system, only: sys_wavelength, sys_wl_ref, sys_units
    integer, intent(in) :: s
    type(lm_params) :: p
    real(real64) :: um
    p = lm_store(s)
    select case (nint(sys_units()))
    case (1); um = 1.0e-3_real64/25.4_real64   ! inches
    case (2); um = 1.0e-4_real64               ! cm
    case (4); um = 1.0e-6_real64               ! m
    case default; um = 1.0e-3_real64           ! mm
    end select
    p%lam_ref = sys_wavelength(nint(sys_wl_ref()))*um
  end function

  ! The legacy tracers (HITSUR): the ray (x, y, l, m, n) has just met surface
  ! i, in its frame, at wavelength number iw.  At a module's front surface it
  ! turns, and the point where it lands on the rear surface is kept for the
  ! next call, which places it there.  The module's optical path is left out.
  ! ok = .false. if the ray cannot pass.
  subroutine lm_legacy_hit(i, iw, x, y, l, m, n, ok)
    use mod_system, only: sys_wavelength
    integer, intent(in) :: i, iw
    real(real64), intent(inout) :: x, y, l, m, n
    logical, intent(out) :: ok
    real(real64), save :: xr_next = 0.0_real64, yr_next = 0.0_real64
    real(real64) :: lo, mo, no, xr, yr, opl
    ok = .true.
    if (lm_is_front(i)) then
      call lm_trace(lm_params_for(i), x, y, l, m, n, 1000.0_real64*sys_wavelength(iw), &
                    lo, mo, no, xr, yr, opl, ok)
      if (.not. ok) return
      l = lo; m = mo; n = no
      xr_next = xr; yr_next = yr
    else if (i >= 1) then
      if (lm_is_front(i-1)) then
        x = xr_next; y = yr_next
      end if
    end if
  end subroutine

end module lens_module
