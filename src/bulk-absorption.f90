! Bulk absorption in the ray energy (SET ABS Y|N, off by default).
!
! When on, a real ray's energy (RAYRAY 25 / the engine's RR_ENERGY, which
! the spot diagram's transmission and CAPFN's pupil weights use) loses the
! internal transmittance of each glass it crosses, from the glass catalog's
! IT data (glass_catalog gc_internal_transmittance).  TRA applies the same
! absorption whatever this setting.
!
! alpha(s, w) is the absorption coefficient (per lens unit) of the glass
! after surface s at wavelength w, refreshed when the glasses are resolved
! (end of GLSRES) and when the setting changes; it is zero while off.
module bulk_absorption
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: abs_on, abs_set, abs_refresh, abs_factor, abs_alpha

  logical, save :: abs_on = .false.
  integer, parameter :: MAXS = 1000
  real(real64), save :: alpha(0:MAXS, 10) = 0.0_real64

contains

  subroutine abs_set(on)
    logical, intent(in) :: on
    abs_on = on
    call abs_refresh()
  end subroutine

  ! Recompute alpha for the current lens and wavelengths.
  subroutine abs_refresh()
    use DATLEN, only: GLANAM
    use mod_system, only: sys_last_surf, sys_wavelength, sys_units
    use glass_manager, only: gdb
    use glass_catalog, only: gc_internal_transmittance
    integer :: s, w, loc, last, savedType, savedCat, savedGlass
    real(real64) :: mm, t

    alpha = 0.0_real64
    if (.not. abs_on) return
    select case (nint(sys_units()))
    case (1); mm = 25.4_real64
    case (2); mm = 10.0_real64
    case (4); mm = 1000.0_real64
    case default; mm = 1.0_real64
    end select
    last = min(int(sys_last_surf()), MAXS)
    savedType = gdb%dataType; savedCat = gdb%curCat; savedGlass = gdb%curGlass
    do s = 0, last
      loc = 0
      if (.not. gdb%isNameInCatalog(trim(GLANAM(s,1)), loc)) cycle
      if (.not. gdb%isGlassInCatalog(trim(GLANAM(s,2)), loc)) cycle
      do w = 1, 10
        if (sys_wavelength(w) <= 0.0_real64) cycle
        ! per mm from 1 mm of glass, then per lens unit
        t = gc_internal_transmittance(gdb%curCat, gdb%curGlass, sys_wavelength(w), 1.0_real64)
        if (t > 0.0_real64 .and. t < 1.0_real64) alpha(s, w) = -log(t)*mm
      end do
    end do
    gdb%dataType = savedType; gdb%curCat = savedCat; gdb%curGlass = savedGlass
  end subroutine

  ! The coefficient of the glass after surface s at wavelength slot w.
  pure real(real64) function abs_alpha(s, w)
    integer, intent(in) :: s, w
    abs_alpha = 0.0_real64
    if (s < 0 .or. s > MAXS .or. w < 1 .or. w > 10) return
    abs_alpha = alpha(s, w)
  end function

  ! Energy factor of a ray crossing path_len (lens units) of the glass after
  ! surface s, wavelength slot w; 1 when off or not absorbing.
  real(real64) function abs_factor(s, w, path_len) result(f)
    integer, intent(in) :: s, w
    real(real64), intent(in) :: path_len
    f = 1.0_real64
    if (.not. abs_on) return
    if (abs_alpha(s, w) == 0.0_real64) return
    f = exp(-abs_alpha(s, w)*abs(path_len))
  end function

end module bulk_absorption
