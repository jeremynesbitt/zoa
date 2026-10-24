! CODE V WAV: RMS wavefront error and Strehl ratio per field, at nominal
! focus and/or at the best composite focus.
!
! For each field a CAPFN (NRD rays across the pupil) gives every ray's OPD
! (waves) and its image-space direction k.  Moving the reference point by d
! changes a ray's OPD by (n/lambda) k.d to first order, so piston (per
! wavelength), tilt (per field: d_x, d_y) and focus (d_z, shared by the
! fields of a CPL group) are removed by one linear least-squares fit:
!   field f:  J_f = sum_w (s_w/S) <(W - p_w - b.d)^2>_w,  b = (n/lambda) k
! with s_w the spectral weights and <> the mean over the rays of wavelength
! w.  Tilt is solved per field for any d_z, leaving J_f quadratic in d_z;
! the best focus of a group minimises sum_f wtf_f J_f.  The RMS reported is
! sqrt(J_f) (polychromatic) and per wavelength; the Strehl ratio is the
! Marechal estimate exp(-(2 pi sigma)^2), averaged over the spectral weights.
module wavefront_analysis
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: wav_reset, wav_set_mode, wav_set_rfo, wav_set_nrd, wav_set_wtf, &
            wav_set_cpl, wav_go

  integer, parameter :: MAXF = 25, MAXW = 10
  real(real64), parameter :: TWOPI = 6.283185307179586_real64

  ! Option state (WAV ... GO).  Weights/groups unset = system weights/all 1.
  logical :: showNom = .false., showBes = .true., showThr = .false., rfo = .false.
  integer :: nrdOpt = 20
  integer :: nWtf = 0, nCpl = 0
  real(real64) :: wtfOpt(MAXF) = 1.0_real64
  integer :: cplOpt(MAXF) = 1

contains

  subroutine wav_reset()
    showNom = .false.; showBes = .true.; showThr = .false.; rfo = .false.
    nrdOpt = 20; nWtf = 0; nCpl = 0
  end subroutine

  ! which = 'BES' | 'NOM' | 'THR' (BES is reported unless only NOM is on).
  subroutine wav_set_mode(which, on)
    character(len=*), intent(in) :: which
    logical, intent(in) :: on
    select case (which)
    case ('NOM'); showNom = on
    case ('BES'); showBes = on
    case ('THR'); showThr = on
    end select
  end subroutine

  subroutine wav_set_rfo(on)
    logical, intent(in) :: on
    rfo = on
  end subroutine

  subroutine wav_set_nrd(n)
    integer, intent(in) :: n
    nrdOpt = n
  end subroutine

  subroutine wav_set_wtf(w, n)
    real(real64), intent(in) :: w(:)
    integer, intent(in) :: n
    nWtf = min(n, MAXF)
    wtfOpt(1:nWtf) = w(1:nWtf)
  end subroutine

  subroutine wav_set_cpl(g, n)
    integer, intent(in) :: g(:)
    integer, intent(in) :: n
    nCpl = min(n, MAXF)
    cplOpt(1:nCpl) = g(1:nCpl)
  end subroutine

  ! Wavelength (microns) in lens units.
  real(real64) function lens_units_per_micron()
    use mod_system, only: sys_units
    select case (nint(sys_units()))
    case (1); lens_units_per_micron = 3.93700787402d-5
    case (2); lens_units_per_micron = 1.0d-4
    case (4); lens_units_per_micron = 1.0d-6
    case default; lens_units_per_micron = 1.0d-3
    end select
  end function

  subroutine wav_go()
    use GLOBALS, only: DSPOTT
    use DATSP1, only: ITOT
    use DATSPD, only: CAPDEF, NRD, TGR, PGR
    use DATLEN, only: NEWIMG
    use global_widgets, only: sysConfig
    use mod_lens_data_manager, only: ldm
    use mod_system, only: sys_wavelength, sys_wl_weight
    use solve_manager, only: surf_has_thi_solve
    use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                           kdp_chg, kdp_lens_cmd, kdp_lens_end
    use zoa_output, only: zoa_emit
    use type_utils, only: int2str, real2str
    implicit none

    type field_rays
      integer :: n = 0
      integer, allocatable :: iw(:)
      real(real64), allocatable :: W(:), b(:,:)   ! OPD (waves), basis (waves/unit)
    end type
    type(field_rays) :: fr(MAXF)
    integer :: nf, nw, f, i, w, g, ng, savedNrd, savedTgr, savedPgr, surf
    integer :: grp(MAXF), m(MAXW)
    real(real64) :: savedCapdef, lam(MAXW), sw(MAXW), S, scale, nImg
    real(real64) :: wf(MAXF), A(MAXF), B(MAXF), G2(2,2), gv(2), h2(2), det, inv(2,2)
    real(real64) :: Gm(3,3,MAXF), hv(3,MAXF), dz(MAXF), dzg, sa, sb, thi
    real(real64) :: meanW(MAXW), meanB(3,MAXW), omega(MAXW)
    character(len=1024) :: line
    character(len=16) :: cell
    logical :: ok(MAXF)

    nf = min(sysConfig%numFields, MAXF)
    scale = lens_units_per_micron()
    nw = 0
    do w = 1, MAXW
      lam(w) = sys_wavelength(w)*scale
      sw(w) = sys_wl_weight(w)
      if (sw(w) > 0.0_real64 .and. lam(w) > 0.0_real64) nw = w
    end do
    S = sum(sw(1:max(nw,1)), mask=lam(1:max(nw,1)) > 0.0_real64)
    if (nw == 0 .or. S <= 0.0_real64) then
      call zoa_emit("WAV: no weighted wavelengths", "red"); return
    end if
    do f = 1, nf
      if (f <= nWtf) then
        wf(f) = wtfOpt(f)
      else
        wf(f) = sysConfig%fieldWeights(f)
      end if
      grp(f) = 1
      if (f <= nCpl) grp(f) = cplOpt(f)
    end do

    ! One CAPFN per field (restoring the CAPFN grid globals after).
    savedCapdef = CAPDEF; savedNrd = NRD; savedTgr = TGR; savedPgr = PGR
    ok = .false.
    do f = 1, nf
      write(line, '(A,F0.6,1X,F0.6,A,I0)') 'FOB ', sysConfig%relativeFields(2,f), &
      &  sysConfig%relativeFields(1,f), '; CAPFN, ', nrdOpt
      call PROCESSILENT(trim(line))
      if (.not. allocated(DSPOTT)) cycle
      allocate(fr(f)%iw(ITOT), fr(f)%W(ITOT), fr(f)%b(3,ITOT))
      do i = 1, ITOT - 1
        if (DSPOTT(7,i) /= 0.0_real64 .or. DSPOTT(12,i) == 0.0_real64) cycle
        w = nint(DSPOTT(16,i))
        if (w < 1 .or. w > nw) cycle
        if (sw(w) <= 0.0_real64) cycle
        nImg = ldm%getSurfIndex(NEWIMG-1, w)
        fr(f)%n = fr(f)%n + 1
        fr(f)%iw(fr(f)%n) = w
        fr(f)%W(fr(f)%n) = DSPOTT(4,i)/TWOPI
        fr(f)%b(:,fr(f)%n) = (nImg/lam(w))*DSPOTT(22:24,i)
      end do
      ok(f) = fr(f)%n >= 3
    end do
    CAPDEF = savedCapdef; NRD = savedNrd; TGR = savedTgr; PGR = savedPgr

    ! Piston: centre each wavelength's rays; then the normal equations of
    ! the field's tilt/focus fit, weighted s_w/(S m_w).
    Gm = 0.0_real64; hv = 0.0_real64
    do f = 1, nf
      if (.not. ok(f)) cycle
      m = 0; meanW = 0.0_real64; meanB = 0.0_real64
      do i = 1, fr(f)%n
        w = fr(f)%iw(i)
        m(w) = m(w) + 1
        meanW(w) = meanW(w) + fr(f)%W(i)
        meanB(:,w) = meanB(:,w) + fr(f)%b(:,i)
      end do
      omega = 0.0_real64
      do w = 1, nw
        if (m(w) == 0) cycle
        meanW(w) = meanW(w)/m(w); meanB(:,w) = meanB(:,w)/m(w)
        omega(w) = sw(w)/(S*m(w))
      end do
      do i = 1, fr(f)%n
        w = fr(f)%iw(i)
        fr(f)%W(i) = fr(f)%W(i) - meanW(w)
        fr(f)%b(:,i) = fr(f)%b(:,i) - meanB(:,w)
        Gm(:,:,f) = Gm(:,:,f) + omega(w)*spread(fr(f)%b(:,i),2,3)*spread(fr(f)%b(:,i),1,3)
        hv(:,f) = hv(:,f) + omega(w)*fr(f)%b(:,i)*fr(f)%W(i)
      end do
      ! J_f(dz) = A dz^2 - 2 B dz + const after the tilt is solved out.
      G2 = Gm(1:2,1:2,f); gv = Gm(1:2,3,f); h2 = hv(1:2,f)
      det = G2(1,1)*G2(2,2) - G2(1,2)*G2(2,1)
      if (abs(det) <= 1.0d-30*max(1.0_real64, G2(1,1)*G2(2,2))) then
        ok(f) = .false.; cycle
      end if
      inv = reshape([G2(2,2), -G2(2,1), -G2(1,2), G2(1,1)], [2,2])/det
      A(f) = Gm(3,3,f) - dot_product(gv, matmul(inv, gv))
      B(f) = hv(3,f) - dot_product(gv, matmul(inv, h2))
    end do
    if (.not. any(ok(1:nf))) then
      call zoa_emit("WAV: no rays reached the image", "red"); return
    end if

    ! Best focus per CPL group.
    dz = 0.0_real64
    ng = maxval(grp(1:nf))
    do g = 1, ng
      sa = 0.0_real64; sb = 0.0_real64
      do f = 1, nf
        if (ok(f) .and. grp(f) == g) then
          sa = sa + wf(f)*A(f); sb = sb + wf(f)*B(f)
        end if
      end do
      dzg = 0.0_real64
      if (sa > 0.0_real64) dzg = sb/sa
      where (grp(1:nf) == g) dz(1:nf) = dzg
    end do

    call zoa_emit("WAVEFRONT ANALYSIS (NRD "//trim(int2str(nrdOpt))// &
    &  "; RMS in waves, piston and tilt removed; Strehl = exp(-(2 pi RMS)^2))", "black")
    if (showNom) call report("NOMINAL FOCUS", .false.)
    if (showBes .or. .not. showNom) then
      call report("BEST COMPOSITE FOCUS", .true.)
    else
      ng = 0        ! (no focus shift to report or apply)
    end if
    if (ng == 1) then
      call zoa_emit("Focus shift = "//trim(real2str(dz(1), 6))//" "// &
      &  trim(sysConfig%getLensUnitsText()), "black")
    else
      do g = 1, ng
        do f = 1, nf
          if (grp(f) == g) then
            call zoa_emit("Group "//trim(int2str(g))//" focus shift = "// &
            &  trim(real2str(dz(f), 6))//" "//trim(sysConfig%getLensUnitsText()), "black")
            exit
          end if
        end do
      end do
    end if
    if (showThr) call zoa_emit("WAV: THR (through-focus) is not available yet", "red")

    ! RFO Y: refocus the image (THI of the last surface before the image).
    if (rfo) then
      surf = NEWIMG - 1
      if (ng == 0) then
        call zoa_emit("WAV: RFO needs BES (the best-focus shift)", "red")
      else if (ng > 1) then
        call zoa_emit("WAV: RFO needs a single focus group (CPL)", "red")
      else if (surf_has_thi_solve(surf)) then
        call zoa_emit("WAV: RFO not applied, THI S"//trim(int2str(surf))// &
        &  " has a solve", "red")
      else
        thi = ldm%getSurfThi(surf) + dz(1)
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_chg(surf)
        call kdp_lens_cmd('TH', w1=thi)
        call kdp_lens_end(refreshSurf=surf)
        call kdp_silent_end()
        call zoa_emit("RFO: THI S"//trim(int2str(surf))//" = "//trim(real2str(thi, 6)), "black")
      end if
    end if

  contains

    subroutine report(title, atBest)
      character(len=*), intent(in) :: title
      logical, intent(in) :: atBest
      real(real64) :: d(3), r, var(MAXW), cnt(MAXW), strehl, poly
      integer :: ff, ii, ww

      call zoa_emit(title, "black")
      line = 'FIELD   X FLD   Y FLD'
      do ww = 1, nw
        if (sw(ww) <= 0.0_real64) cycle
        write(cell, '(A,I0)') 'RMS W', ww
        line = trim(line)//repeat(' ', 11 - len_trim(cell))//trim(cell)
      end do
      line = trim(line)//'   RMS POLY     STREHL'
      call zoa_emit(trim(line), "black")
      do ff = 1, nf
        write(line, '(I5,2F8.3)') ff, sysConfig%relativeFields(1,ff), sysConfig%relativeFields(2,ff)
        if (.not. ok(ff)) then
          call zoa_emit(trim(line)//'   (no rays)', "black"); cycle
        end if
        ! Tilt for this focus: G2 t = h2 - g dz.
        d(3) = 0.0_real64
        if (atBest) d(3) = dz(ff)
        G2 = Gm(1:2,1:2,ff)
        det = G2(1,1)*G2(2,2) - G2(1,2)*G2(2,1)
        inv = reshape([G2(2,2), -G2(2,1), -G2(1,2), G2(1,1)], [2,2])/det
        d(1:2) = matmul(inv, hv(1:2,ff) - Gm(1:2,3,ff)*d(3))
        var = 0.0_real64; cnt = 0.0_real64
        do ii = 1, fr(ff)%n
          ww = fr(ff)%iw(ii)
          r = fr(ff)%W(ii) - dot_product(fr(ff)%b(:,ii), d)
          var(ww) = var(ww) + r*r
          cnt(ww) = cnt(ww) + 1.0_real64
        end do
        poly = 0.0_real64; strehl = 0.0_real64
        do ww = 1, nw
          if (sw(ww) <= 0.0_real64) cycle
          if (cnt(ww) > 0.0_real64) var(ww) = var(ww)/cnt(ww)
          poly = poly + sw(ww)*var(ww)/S
          strehl = strehl + sw(ww)*exp(-(TWOPI**2)*var(ww))/S
          write(cell, '(F11.5)') sqrt(var(ww))
          line = trim(line)//cell(1:11)
        end do
        write(cell, '(F11.5)') sqrt(poly)
        line = trim(line)//cell(1:11)
        write(cell, '(F11.4)') strehl
        line = trim(line)//cell(1:11)
        call zoa_emit(trim(line), "black")
      end do
    end subroutine report

  end subroutine wav_go

end module wavefront_analysis
