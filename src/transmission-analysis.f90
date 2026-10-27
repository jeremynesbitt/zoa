! CODE V TRA: transmission analysis.
!
! For each field and weighted wavelength, rays on an NRD x NRD grid over
! the pupil are traced to the image (apertures checked) and each ray's
! transmission is the product, over the surfaces it meets, of
!   - the surface factor: a surface with a coating file (COATnnnn.DAT) uses
!     the coating; an uncoated refracting surface its bare Fresnel
!     transmission, the average of the s and p values from the indices on
!     each side and the ray's angles; mirrors are lossless;
!   - the bulk factor: the internal transmittance of the glass the ray
!     crosses on its way to the surface, from the glass catalog's IT data
!     over the ray's path length (glass_catalog gc_internal_transmittance).
! The listing gives, per wavelength, the mean over the rays that reach the
! image of the surface factors, the bulk factors and their product, and
! the spectrally weighted means.  The first field is listed in full; other
! fields give the weighted line only unless FUL Y.
!
! Options: FUL Y|N, NRD n (default 20).  PLI (polarization) is not done.
module transmission_analysis
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: tra_reset, tra_set_option, tra_go

  integer :: nrd = 20
  logical :: ful = .false.

contains

  subroutine tra_reset()
    nrd = 20
    ful = .false.
  end subroutine

  ! One option line (tokens already split): ok false with a message on error.
  subroutine tra_set_option(tok, n, msg)
    character(len=*), intent(in) :: tok(:)
    integer, intent(in) :: n
    character(len=*), intent(out) :: msg
    integer :: ios, v
    msg = ''
    select case (trim(tok(1)))
    case ('FUL')
      ful = .true.
      if (n >= 2) then
        select case (trim(tok(2)))
        case ('Y', 'YES'); ful = .true.
        case ('N', 'NO');  ful = .false.
        case default; msg = 'TRA: expected FUL Y|N'
        end select
      end if
    case ('NRD')
      if (n < 2) then
        msg = 'TRA: expected NRD n'; return
      end if
      read(tok(2), *, iostat=ios) v
      if (ios /= 0 .or. v < 1 .or. v > 500) then
        msg = 'TRA: NRD takes 1 to 500 rays across the pupil'; return
      end if
      nrd = v
    case ('PLI')
      msg = 'TRA: PLI (polarization data) is not available yet'
    case default
      msg = 'TRA: '//trim(tok(1))//' is not a TRA option'
    end select
  end subroutine

  subroutine tra_go()
    use global_widgets, only: sysConfig
    use mod_system, only: sys_last_surf, sys_wavelength, sys_wl_weight, sys_units
    use mod_ray_trace_engine, only: trace_context
    use mod_ray_trace_builder, only: build_trace_context
    use zoa_output, only: zoa_emit
    integer :: f, w, i, j, ns, nray, last, ic(0:999), ig(0:999)
    real(real64) :: p, q, tsurf, tbulk, sumS, sumB, sumT, wsum, mm
    real(real64) :: wS, wB, wT
    real(real64), allocatable :: cosi(:), cosip(:), plen(:)
    type(trace_context) :: ctx
    character(len=200) :: line
    logical :: pass

    last = int(sys_last_surf())
    call surface_glasses(last, ic, ig)
    select case (nint(sys_units()))
    case (1); mm = 25.4_real64
    case (2); mm = 10.0_real64
    case (4); mm = 1000.0_real64
    case default; mm = 1.0_real64
    end select
    allocate(cosi(0:last), cosip(0:last), plen(0:last))

    write(line, '(A,I0)') 'TRANSMISSION ANALYSIS (TRA)   rays across the pupil diameter: ', nrd
    call zoa_emit(trim(line), 'black')
    call zoa_emit('uncoated surfaces: Fresnel losses; bulk absorption: the glass catalogs'' '// &
                  'transmittance data', 'black')
    do f = 1, sysConfig%numFields
      write(line, '(A,I0,A,F8.4,A,F8.4,A)') 'FIELD ', f, '   (X ', &
        sysConfig%relativeFields(1, f), ', Y ', sysConfig%relativeFields(2, f), ' relative)'
      call zoa_emit('', 'black')
      call zoa_emit(trim(line), 'black')
      write(line, '(A,F0.8,1X,F0.8)') 'FOB ', sysConfig%relativeFields(2, f), &
        sysConfig%relativeFields(1, f)
      call PROCESSILENT(trim(line))
      call build_trace_context(ctx, check_apertures=.true.)
      if (f == 1 .or. ful) call zoa_emit('  WAVELENGTH_NM   WEIGHT    RAYS   SURFACES       BULK      TOTAL', &
                                         'black')
      wS = 0.0_real64; wB = 0.0_real64; wT = 0.0_real64; wsum = 0.0_real64
      do w = 1, 10
        if (sys_wl_weight(w) <= 0.0_real64 .or. sys_wavelength(w) <= 0.0_real64) cycle
        nray = 0; sumS = 0.0_real64; sumB = 0.0_real64; sumT = 0.0_real64
        do j = 1, nrd
          q = -1.0_real64 + (2.0_real64*j - 1.0_real64)/nrd
          do i = 1, nrd
            p = -1.0_real64 + (2.0_real64*i - 1.0_real64)/nrd
            if (p*p + q*q > 1.0_real64) cycle
            call trace_one(ctx, q, p, w, last, cosi, cosip, plen, pass)
            if (.not. pass) cycle
            call ray_transmission(w, last, ic, ig, cosi, cosip, plen*mm, tsurf, tbulk)
            nray = nray + 1
            sumS = sumS + tsurf; sumB = sumB + tbulk; sumT = sumT + tsurf*tbulk
          end do
        end do
        if (nray == 0) then
          if (f == 1 .or. ful) then
            write(line, '(F15.2,F9.1,I8,A)') sys_wavelength(w)*1000.0_real64, sys_wl_weight(w), 0, &
              '   (no ray reaches the image)'
            call zoa_emit(trim(line), 'black')
          end if
          cycle
        end if
        if (f == 1 .or. ful) then
          write(line, '(F15.2,F9.1,I8,3F11.6)') sys_wavelength(w)*1000.0_real64, sys_wl_weight(w), &
            nray, sumS/nray, sumB/nray, sumT/nray
          call zoa_emit(trim(line), 'black')
        end if
        wS = wS + sys_wl_weight(w)*sumS/nray
        wB = wB + sys_wl_weight(w)*sumB/nray
        wT = wT + sys_wl_weight(w)*sumT/nray
        wsum = wsum + sys_wl_weight(w)
      end do
      if (wsum > 0.0_real64) then
        write(line, '(A,3F11.6)') '  WEIGHTED                        ', wS/wsum, wB/wsum, wT/wsum
        if (.not. (f == 1 .or. ful)) &
          write(line, '(A,3F11.6)') '  WEIGHTED (SURFACES BULK TOTAL)  ', wS/wsum, wB/wsum, wT/wsum
        call zoa_emit(trim(line), 'black')
      end if
    end do
  end subroutine

  ! The catalog glass (ic, ig) filling the space after each surface, 0 if
  ! none (air, a model glass, a mirror).
  subroutine surface_glasses(last, ic, ig)
    use DATLEN, only: GLANAM
    use glass_manager, only: gdb
    integer, intent(in) :: last
    integer, intent(out) :: ic(0:), ig(0:)
    integer :: s, loc, savedType, savedCat, savedGlass
    savedType = gdb%dataType; savedCat = gdb%curCat; savedGlass = gdb%curGlass
    ic = 0; ig = 0
    do s = 0, last
      loc = 0
      if (.not. gdb%isNameInCatalog(trim(GLANAM(s,1)), loc)) cycle
      if (.not. gdb%isGlassInCatalog(trim(GLANAM(s,2)), loc)) cycle
      ic(s) = gdb%curCat; ig(s) = gdb%curGlass
    end do
    gdb%dataType = savedType; gdb%curCat = savedCat; gdb%curGlass = savedGlass
  end subroutine

  ! One ray at relative pupil (py, px), wavelength slot iw, to the image with
  ! apertures checked: per surface the cosines of incidence and refraction
  ! and the path length from the surface before.  The engine when the lens
  ! is supported, else the legacy RAY CAOB command.
  subroutine trace_one(ctx, py, px, iw, last, cosi, cosip, plen, pass)
    use mod_ray_trace_engine, only: trace_context, ray_request, ray_result, trace_ray, &
                                    RAY_OK, RR_COSI, RR_COSIP, RR_LEN
    use DATLEN, only: RAYRAY, RAYCOD
    type(trace_context), intent(in) :: ctx
    real(real64), intent(in) :: py, px
    integer, intent(in) :: iw, last
    real(real64), intent(out) :: cosi(0:), cosip(0:), plen(0:)
    logical, intent(out) :: pass
    type(ray_request) :: req
    type(ray_result) :: res
    character(len=200) :: line

    cosi = 1.0_real64; cosip = 1.0_real64; plen = 0.0_real64
    if (ctx%supported) then
      req%py = py; req%px = px; req%iwl = iw
      call trace_ray(ctx, req, res)
      pass = res%status == RAY_OK
      if (.not. pass) return
      cosi(0:last) = res%rr(RR_COSI, 0:last)
      cosip(0:last) = res%rr(RR_COSIP, 0:last)
      plen(0:last) = res%rr(RR_LEN, 0:last)
    else
      write(line, '(A,F0.10,1X,F0.10,1X,I0)') 'RAY CAOB ', py, px, iw
      call PROCESSILENT(trim(line))
      pass = RAYCOD(1) == 0
      if (.not. pass) return
      cosi(0:last) = RAYRAY(9, 0:last)
      cosip(0:last) = RAYRAY(10, 0:last)
      plen(0:last) = RAYRAY(8, 0:last)
    end if
  end subroutine

  ! The surface and bulk transmission of one traced ray (plen in mm).
  subroutine ray_transmission(iw, last, ic, ig, cosi, cosip, plen, tsurf, tbulk)
    use DATLEN, only: GLANAM, ABSCOEF, OLDABSCOEF
    use mod_surface, only: surf_refractive_index, surf_coating_index
    use mod_system, only: sys_wavelength
    use glass_catalog, only: gc_internal_transmittance
    integer, intent(in) :: iw, last, ic(0:), ig(0:)
    real(real64), intent(in) :: cosi(0:), cosip(0:), plen(0:)
    real(real64), intent(out) :: tsurf, tbulk
    integer :: s, c
    real(real64) :: n1, n2, ef, pol, fpa, fpe, ppa, ppe, zero
    logical :: exists
    character(len=8) :: cname

    tsurf = 1.0_real64; tbulk = 1.0_real64
    do s = 1, last
      ! bulk: the glass after surface s-1, over the path to surface s
      if (ic(s-1) > 0) tbulk = tbulk*gc_internal_transmittance(ic(s-1), ig(s-1), &
                                                                sys_wavelength(iw), plen(s))
      if (GLANAM(s,2)(1:4) == 'REFL') cycle                 ! mirror: lossless
      n1 = abs(surf_refractive_index(s-1, iw))
      n2 = abs(surf_refractive_index(s, iw))
      c = surf_coating_index(s)
      exists = .false.
      if (c >= 1 .and. c <= 1000) then
        write(cname, '(A,I4.4)') 'COAT', c
        inquire(file=cname//'.DAT', exist=exists)
      end if
      if (exists) then
        ! the coating's interface factor (path 0: bulk absorption is the
        ! catalog's, not the coating file's absorption coefficients)
        ABSCOEF = 0.0_real64; OLDABSCOEF = 0.0_real64
        zero = 0.0_real64
        call ENERGY_ADJUST(ef, s, c, acos(min(1.0_real64, abs(cosi(s)))), &
                           acos(min(1.0_real64, abs(cosip(s)))), n1, n2, iw, pol, &
                           fpa, fpe, ppa, ppe, zero)
        tsurf = tsurf*ef
      else if (abs(n1 - n2) > 1.0e-12_real64) then
        tsurf = tsurf*fresnel(n1, n2, abs(cosi(s)), abs(cosip(s)))
      end if
    end do
  end subroutine

  ! Bare-surface transmission, the mean of the s and p values.
  real(real64) function fresnel(n1, n2, ci, ct) result(t)
    real(real64), intent(in) :: n1, n2, ci, ct
    real(real64) :: rs, rp
    rs = (n1*ci - n2*ct)/(n1*ci + n2*ct)
    rp = (n2*ci - n1*ct)/(n2*ci + n1*ct)
    t = 1.0_real64 - 0.5_real64*(rs*rs + rp*rp)
  end function

end module transmission_analysis
