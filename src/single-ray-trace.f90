! SINRAYTRA: a data-only tab (the plot infrastructure without a plot) with
! OpticStudio's Single Ray Trace: one real ray and the matching paraxial ray,
! surface by surface, for normalized field (Hx, Hy), normalized pupil
! (Px, Py) and a wavelength.
!
! Real ray: FOB Hy Hx; RAY Py Px wl (what RSI traces), read from RAYRAY in
! each surface's local coordinates after refraction -- direction cosines
! (as PRLMN) with the surface normal, angle of incidence and path length
! from the previous surface, or tangents of the slope angles (as PRREF).
!
! Paraxial ray: the paraxial trace is linear, so the ray at (Hx Hy Px Py) is
! Py * marginal + fy * chief in YZ and Px * marginal + fx * chief in XZ
! (PXTRAY/PXTRAX are the full-aperture marginal and full-field chief rays).
! fx, fy put the paraxial ray at the real ray's object point: FOB scales a
! field angle, not its tangent, so fy = Hy only for object/image heights.
! It is traced at the chosen wavelength from the object space of the stored
! rays, with the same surface step (traNextSurf) and image defocus as the
! paraxial trace.  Paraxial intercepts lie in each surface's tangent plane
! (Z = 0); its direction cosines follow from the slopes.
module single_ray_trace
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: sinraytra_go, writeSingleRayTables

contains

  subroutine sinraytra_go(psm)
    use GLOBALS, only: HEADLESS_MODE
    use tow_functions, only: initializeGoDataTab, finalizeGoDataTab
    use handlers, only: ioConfig, ID_TERMINAL_DEFAULT
    use plot_functions, only: zoaplot_setting_manager, getTabTextView
    use zoa_ui, only: ID_SINRAYTRA_TAB
    type(zoaplot_setting_manager) :: psm
    real(real64) :: hx, hy, px, py
    integer :: iw, objIdx
    logical :: useTan, replot

    call psm%getSinRayTraSettings(hx, hy, px, py, iw, useTan)
    if (HEADLESS_MODE) then
      call writeSingleRayTables(hx, hy, px, py, iw, useTan)
      return
    end if
    call initializeGoDataTab(psm, ID_SINRAYTRA_TAB, "Single Ray Trace", replot, objIdx)
    call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    call writeSingleRayTables(hx, hy, px, py, iw, useTan)
    call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    call finalizeGoDataTab(psm, replot, objIdx)
  end subroutine

  subroutine writeSingleRayTables(hx, hy, px, py, iw, useTan)
    use global_widgets, only: curr_lens_data, sysConfig
    use kdp_utils, only: OUTKDP, logColumns
    use type_utils, only: int2str, real2str
    use mod_lens_data_manager, only: ldm
    use mod_system, only: sys_last_surf, sys_wavelength
    use mod_kdp_api, only: kdp_fob, kdp_ray
    use DATLEN, only: RAYRAY, REFRY, RAYCOD, STOPP, NEWOBJ, NEWIMG, PXTRAY, PXTRAX
    real(real64), intent(in) :: hx, hy, px, py
    integer, intent(in) :: iw
    logical, intent(in) :: useTan
    real(real64), allocatable :: realT(:,:), parT(:,:)
    character(len=8), allocatable :: lbl(:)
    real(real64) :: yP(4, 0:499), xP(4, 0:499)
    integer :: i, r, n, last, nOK
    real(real64) :: fx, fy

    if (curr_lens_data%num_surfaces < 3) then
      call OUTKDP('SINRAYTRA: no lens is loaded')
      return
    end if
    if (iw < 1 .or. iw > sysConfig%numWavelengths) then
      call OUTKDP('SINRAYTRA: wavelength '//trim(int2str(iw))//' is not defined')
      return
    end if
    last = int(sys_last_surf())

    call OUTKDP('Title:        '//trim(sysConfig%lensTitle))
    call OUTKDP('Wavelength:   '//trim(int2str(iw))//' ('// &
    &  trim(real2str(1000.0_real64*sys_wavelength(iw), 2))//' nm)')
    call OUTKDP('X Field:      '//trim(real2str(hx, 4)))
    call OUTKDP('Y Field:      '//trim(real2str(hy, 4)))
    call OUTKDP('X Pupil:      '//trim(real2str(px, 4)))
    call OUTKDP('Y Pupil:      '//trim(real2str(py, 4)))
    if (useTan) then
      call OUTKDP('Coordinates:  Tangent Angle')
    else
      call OUTKDP('Coordinates:  Direction Cosines')
    end if
    call OUTKDP('')
    call OUTKDP('Coordinates are local to each surface; directions are after refraction.')
    call OUTKDP('')

    ! ---- real ray
    call kdp_fob(hy, hx)
    call kdp_ray(py, px, real(iw, real64))
    n = NEWIMG - NEWOBJ + 1
    nOK = n
    if (STOPP == 1) nOK = max(0, RAYCOD(2) - NEWOBJ)
    call OUTKDP('')
    call OUTKDP('Real ray trace data')
    if (STOPP == 1) call OUTKDP('The ray failed at surface '//trim(int2str(RAYCOD(2)))// &
    &  '; the surfaces before it are listed.')
    if (nOK > 0) then
      allocate(lbl(nOK))
      if (useTan) then
        allocate(realT(nOK, 5))
      else
        allocate(realT(nOK, 11))
      end if
      do r = 1, nOK
        i = NEWOBJ + r - 1
        lbl(r) = surfLabel(i)
        realT(r, 1:3) = RAYRAY(1:3, i)
        if (useTan) then
          realT(r, 4) = slope(RAYRAY(4, i), RAYRAY(6, i))
          realT(r, 5) = slope(RAYRAY(5, i), RAYRAY(6, i))
        else
          realT(r, 4:6) = RAYRAY(4:6, i)
          realT(r, 7:9) = RAYRAY(13:15, i)
          realT(r, 10) = acos(min(1.0_real64, abs(RAYRAY(9, i))))*180.0_real64/acos(-1.0_real64)
          realT(r, 11) = RAYRAY(8, i)
        end if
      end do
      if (useTan) then
        call logColumns([character(len=12) :: 'Surface', 'X', 'Y', 'Z', 'X-tangent', 'Y-tangent'], realT, lbl)
      else
        call logColumns([character(len=12) :: 'Surface', 'X', 'Y', 'Z', 'X-cosine', 'Y-cosine', &
        &  'Z-cosine', 'X-normal', 'Y-normal', 'Z-normal', 'Angle in', 'Path length'], realT, lbl)
      end if
      deallocate(lbl)
    end if
    call OUTKDP('')

    ! ---- paraxial ray
    fy = hy; fx = hx
    if (abs(PXTRAY(5, 0)) > 0.0_real64) fy = REFRY(2, NEWOBJ)/PXTRAY(5, 0)
    if (abs(PXTRAX(5, 0)) > 0.0_real64) fx = REFRY(1, NEWOBJ)/PXTRAX(5, 0)
    call paraxialRay(PXTRAY, fy, py, .false., yP)
    call paraxialRay(PXTRAX, fx, px, .true., xP)
    allocate(lbl(last + 1))
    if (useTan) then
      allocate(parT(last + 1, 5))
    else
      allocate(parT(last + 1, 6))
    end if
    do i = 0, last
      r = i + 1
      lbl(r) = surfLabel(i)
      parT(r, 1) = xP(1, i)
      parT(r, 2) = yP(1, i)
      parT(r, 3) = 0.0_real64
      if (useTan) then
        parT(r, 4) = xP(2, i)
        parT(r, 5) = yP(2, i)
      else
        ! N carries the direction of travel (negative after a mirror)
        parT(r, 6) = sign(1.0_real64, ldm%getSurfIndex(i, iw))/sqrt(1.0_real64 + xP(2, i)**2 + yP(2, i)**2)
        parT(r, 4) = xP(2, i)*parT(r, 6)
        parT(r, 5) = yP(2, i)*parT(r, 6)
      end if
    end do
    call OUTKDP('')
    call OUTKDP('Paraxial ray trace data')
    if (useTan) then
      call logColumns([character(len=12) :: 'Surface', 'X', 'Y', 'Z', 'X-tangent', 'Y-tangent'], parT, lbl)
    else
      call logColumns([character(len=12) :: 'Surface', 'X', 'Y', 'Z', 'X-cosine', 'Y-cosine', &
      &  'Z-cosine'], parT, lbl)
    end if

  contains

    character(len=8) function surfLabel(k)
      integer, intent(in) :: k
      surfLabel = adjustl(ldm%getSurfName(k))
    end function

    real(real64) function slope(c, cz)
      real(real64), intent(in) :: c, cz
      slope = 0.0_real64
      if (cz /= 0.0_real64) slope = c/cz
    end function

    ! ray = f * chief + p * marginal (stored rays), retraced at wavelength iw
    subroutine paraxialRay(PX, f, p, xz, out)
      use paraxial_ray_trace_test, only: traNextSurf
      use mod_surface, only: surf_thickness
      real(real64), intent(in) :: PX(1:8, 0:499), f, p
      logical, intent(in) :: xz
      real(real64), intent(out) :: out(4, 0:499)
      integer :: L
      out = 0.0_real64
      out(:, 0) = p*PX(1:4, 0) + f*PX(5:8, 0)
      out(:, 1) = traNextSurf(out(:, 0), 1, iw, useXZPlane=xz, &
      &  overridePos=p*PX(1, 1) + f*PX(5, 1))
      do L = 2, last
        out(:, L) = traNextSurf(out(:, L-1), L, iw, useXZPlane=xz)
      end do
      ! a thickness on the image surface is a defocus (THI SI)
      if (surf_thickness(last) /= 0.0_real64) out(1, last) = out(1, last) + surf_thickness(last)*out(2, last)
    end subroutine

  end subroutine

end module single_ray_trace
