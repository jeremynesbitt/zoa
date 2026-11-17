submodule (plot_functions) plot_functions_fft
implicit none
contains

module procedure psf_go
  use mod_fft, only: four2, fftshift, fft2
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only:  sysConfig, curr_opd, ioConfig
  use type_utils, only: int2str
  use plplot, PI => PL_PI
  use plplot_extra
  use mod_analysis_manager
  use iso_c_binding, only: c_ptr, c_null_ptr
  use kdp_utils

character(len=1024) :: ffieldstr

integer :: xpts, ypts, pupilGrid
integer, parameter :: xdim=99, ydim=100
integer :: lambda, fldIdx
integer :: ii,jj,zz

integer, parameter :: nlevel = 10

type(c_ptr) :: canvas
type(zoaPlotImg) :: zp3d
type(multiplot) :: mplt
real(long), allocatable :: psfData(:,:), psfX(:), psfY(:), psfZ(:)
type(image_data) :: imgPSF
integer :: objIdx
logical :: replot
complex(long), allocatable :: fftData(:,:)


call initializeGoPlot(psm,ID_PLOTTYPE_PSF, "Point Spread Function", replot, objIdx)

lambda = psm%getWavelengthSetting()
fldIdx = psm%getFieldSetting()

call kdp_fob(sysConfig%relativeFields(2,fldIdx), sysConfig%relativeFields(1,fldIdx))

! Set this plot's own pupil grid, exactly as mtf_go does.  doPSF sizes its
! transform from the NRD/TGR globals, which without this were simply whatever
! the previous plot's CAPFN last left (a PSF after a 64-grid PMA came out at
! 64), and the Density setting execPSF adds was never applied at all.  CAPFN
! (not just NRD) so that NRD and the transform size TGR are set consistently.
pupilGrid = psm%getDensitySetting()
call PROCESSILENT('NRD, '//trim(int2str(pupilGrid)))
call kdp_silent_begin()
call kdp_capfn(pupilGrid)
call kdp_silent_end()

if (HEADLESS_MODE) then
  canvas = c_null_ptr
else
  canvas = hl_gtk_drawing_area_new(size=[600,600], &
  & has_alpha=FALSE)
end if

call getData("PSFK", imgPSF)

allocate(psfData, mold=imgPSF%img)
psfData = imgPSF%img
xpts = size(psfData,1)
ypts = size(psfData,2)

allocate(psfX(xpts*ypts))
allocate(psfY(xpts*ypts))
allocate(psfZ(xpts*ypts))

allocate(fftData(size(psfData,1),size(psfData,2)))
fftData = fft2(cmplx(psfData,kind=long),1)
call fftshift(fftData)
if (.not. HEADLESS_MODE) then
  call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
end if
! Three matrices, each introduced by a label line.  The plot draws
! psfData(j,i) at X = i, Y = j: a line is one Y index (first line = bottom),
! a column one X index -- in MATLAB, imagesc(A); axis xy.
call OUTKDP('PSF intensity, '//trim(int2str(xpts))//' x '//trim(int2str(ypts))// &
&           '; line k = image Y index k (first line at the bottom), column c = X index c')
call logImageData(psfData)
call OUTKDP('FFT of the PSF, real part (fftshift-ed, same layout)')
call logImageData(real(real(fftData),kind=long))
call OUTKDP('FFT of the PSF, imaginary part (fftshift-ed, same layout)')
call logImageData(real(aimag(fftData),kind=long))

if (.not. HEADLESS_MODE) call ioConfig%restoreTextView()

call mplt%initialize(canvas, 1,1)
zz=1
do ii=xpts,1,-1
do jj=1,ypts
psfX(zz)=ii
psfY(zz)=jj
psfZ(zz)=psfData(jj,ii)
zz=zz+1
end do
end do
 call zp3d%init3d(c_null_ptr, real(psfX),real(psfY), &
 & real(psfZ), xpts, ypts, &
 & xlabel='X'//c_null_char, ylabel='Y'//c_null_char, &
 & title='Point Spread Function'//c_null_char)

 call mplt%set(1,1,zp3d)

 call finalizeGoPlot_new(mplt, psm, replot, objIdx)


end procedure psf_go

module procedure pma_go

    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use global_widgets, only:  sysConfig, curr_opd, ioConfig
    use type_utils, only: int2str
    use plplot, PI => PL_PI
    use plplot_extra
    use iso_c_binding, only: c_ptr, c_null_ptr

  use plot_setting_manager, only: ID_PMA_BAR
  use kdp_utils, only: OUTKDP, log2DData
  use iso_fortran_env, only: real64

  character(len=1024) :: ffieldstr
  character(len=256) :: lineStr

  integer :: xpts, ypts
  integer, parameter :: xdim=99, ydim=100
  integer :: lambda, fldIdx
  integer :: objIdx, plotType, nZern, i
  logical :: replot

  integer, parameter :: nlevel = 10

  type(c_ptr) :: canvas
  type(zoaPlotImg) :: zp3d
  type(barchart) :: zbar
  type(multiplot) :: mplt
  real, allocatable :: termIdx(:), termVal(:)

  ! Zernike fit coefficients from the last FITZERN (COMMON/SOLU/X, same store
  ! LISTZERN and the ZRN command read).
  real(real64) :: X(1:96)
  COMMON/SOLU/X


  ! Create/find the tab up front so the Data tab can be written (matches
  ! rmsfield_go).  No-op in headless.
  call initializeGoPlot(psm, ID_PLOTTYPE_OPD, "Optical Path Difference", replot, objIdx)

  lambda = psm%getWavelengthSetting()
  fldIdx = psm%getFieldSetting()
  xpts = psm%getDensitySetting()
  ypts = xpts
  call psm%getPMASettings(plotType, nZern)

  call kdp_fob(sysConfig%relativeFields(2,fldIdx), sysConfig%relativeFields(1,fldIdx))

  ! Compute silently: CAPFN fills curr_opd (the map) and FITZERN fills the
  ! Zernike coefficients.  Their printed reports used to be dumped into the
  ! Data tab; it now gets structured data for the selected plot type instead.
  call kdp_silent_begin()
  call kdp_capfn(xpts)
  call kdp_silent_end()
  call PROCESSILENT('FITZERN, '//trim(int2str(lambda)))

  if (HEADLESS_MODE) then
    canvas = c_null_ptr
  else
    canvas = hl_gtk_drawing_area_new(size=[600,600], &
    & has_alpha=FALSE)
    call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  end if

  call mplt%initialize(canvas, 1,1)

  if (plotType == ID_PMA_BAR) then
    ! Zernike bar chart: the first nZern Fringe coefficients of the fit.
    allocate(termIdx(nZern), termVal(nZern))
    do i = 1, nZern
      termIdx(i) = real(i)
      termVal(i) = real(X(i))
    end do
    call zbar%initialize(c_null_ptr, termIdx, termVal, &
    & xlabel='Zernike Term'//c_null_char, &
    & ylabel='Coefficient [waves]'//c_null_char, &
    & title='Fringe Zernike Coefficients'//c_null_char)
    call mplt%set(1,1,zbar)

    ! Data tab: the coefficient list for this field point.
    call OUTKDP('Fringe Zernike coefficients, field '//trim(int2str(fldIdx))// &
    &           ', wavelength '//trim(int2str(lambda)))
    write(lineStr, '(A)') '   Term    Coefficient [waves]'
    call OUTKDP(trim(lineStr))
    do i = 1, nZern
      write(lineStr, '(4X,I3,4X,F18.8)') i, X(i)
      call OUTKDP(trim(lineStr))
    end do
  else
    ! Surface map (default).
    call zp3d%init3d(c_null_ptr, real(curr_opd%X),real(curr_opd%Y), &
    & real(curr_opd%Z), xpts, ypts, &
    & xlabel='X'//c_null_char, ylabel='Y'//c_null_char, &
    & title='Optical Path Difference'//c_null_char)
    call mplt%set(1,1,zp3d)

    ! Data tab: the map as (pupil X, pupil Y, OPD) points.
    call OUTKDP('Optical path difference map, field '//trim(int2str(fldIdx))// &
    &           ', wavelength '//trim(int2str(lambda))//', '// &
    &           trim(int2str(curr_opd%numPts))//' points')
    write(lineStr, '(A)') '        Pupil X          Pupil Y       OPD [waves]'
    call OUTKDP(trim(lineStr))
    do i = 1, curr_opd%numPts
      write(lineStr, '(3F16.8)') curr_opd%X(i), curr_opd%Y(i), curr_opd%Z(i)
      call OUTKDP(trim(lineStr))
    end do
  end if

  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

  call finalizeGoPlot_new(mplt, psm, replot, objIdx)



end procedure pma_go

module procedure mtf_go
  ! Polychromatic diffraction MTF for one field, from the engine's DOTF (pupil
  ! autocorrelation over the CAPFN wavefront, spectrally weighted): the Y
  ! (tangential for a Y field) and X (sagittal) responses at frequencies 0,
  ! IFR, 2*IFR, ... up to MFR, with the aberration-free curve of a circular
  ! pupil for reference.  The MTF is zero from the cutoff (shortest weighted
  ! wavelength) on, so those frequencies are not computed.  Computed at focus
  ! FFO (system data), like SPO FLD.
  !
  ! It used to be the FFT of the PSF image, which got the frequency axis
  ! (integer division, wrong scale), the modulus (|Re|^2) and the pixel size
  ! wrong -- non-zero modulation far beyond the cutoff -- and ignored MFR/IFR.
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only: ioConfig, sysConfig
  use kdp_utils, only: OUTKDP
  use type_utils, only: int2str, real2str
  use mod_system, only: sys_mode
  use focus_settings, only: focus_first
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_fortran_env, only: real64
  IMPLICIT NONE

  integer, parameter :: MAX_FREQ_POINTS = 1001
  type(c_ptr) :: canvas
  type(zoaplot) :: xyscat
  type(multiplot) :: mplt
  character(len=100) :: lineStr
  character(len=30) :: unitStr
  integer :: objIdx, iField, xpts, nPts, k
  logical :: replot, ok
  real(real64) :: maxFreq, dFreq, cutoff, delta
  real(real64), allocatable :: f(:), mtfY(:), mtfX(:), mtfDL(:)

  call initializeGoPlot(psm, ID_PLOTTYPE_MTF, "MTF", replot, objIdx)

  iField = psm%getFieldSetting()
  xpts = psm%getPowerOfTwoImageSetting()
  maxFreq = psm%getSettingValueByCode(SETTING_MAX_FREQUENCY)
  dFreq = psm%getSettingValueByCode(SETTING_FREQUENCY_INTERVAL)

  call mtfCutoff(iField, 'MTF', cutoff, ok)
  if (.not. ok) return
  if (maxFreq <= 0.0_real64) maxFreq = cutoff
  if (dFreq <= 0.0_real64) dFreq = maxFreq/100.0_real64
  nPts = int(maxFreq/dFreq + 1.0e-9_real64) + 1
  if (nPts > MAX_FREQ_POINTS) then
    call zoa_emit('MTF: MFR/IFR asks for '//trim(int2str(nPts))//' frequencies; using the first '// &
    &             trim(int2str(MAX_FREQ_POINTS)), 'red')
    nPts = MAX_FREQ_POINTS
  end if

  delta = focus_first()
  if (sys_mode() > 2.0_real64 .and. delta /= 0.0_real64) then
    call zoa_emit("MTF: focus positions (FFO/IFO/NFO) need a focal system; nominal focus used", "red")
    delta = 0.0_real64
  end if

  allocate(f(nPts), mtfY(nPts), mtfX(nPts), mtfDL(nPts))
  do k = 1, nPts
    f(k) = (k-1)*dFreq
  end do
  call mtfAtFocus(iField, xpts, delta, f, cutoff, mtfY, mtfX)
  mtfDL = mtfDiffLimit(f, cutoff)

  unitStr = mtfFreqUnits()

  ! Data tab
  if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  call OUTKDP('Polychromatic diffraction MTF, field '//trim(int2str(iField))// &
  &           ', cutoff '//trim(real2str(cutoff, 4))//' '//trim(unitStr))
  if (delta /= 0.0_real64) call OUTKDP('At focus FFO '//trim(real2str(delta, 6))// &
  &           ' '//trim(sysConfig%getLensUnitsText()))
  call OUTKDP('  Frequency      Y (tan)     X (sag)   Diff. limit')
  do k = 1, nPts
    write(lineStr, '(F11.4,3F12.6)') f(k), mtfY(k), mtfX(k), mtfDL(k)
    call OUTKDP(trim(lineStr))
  end do
  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

  if (HEADLESS_MODE) then
    canvas = c_null_ptr
  else
    canvas = hl_gtk_drawing_area_new(size=[1200,800], has_alpha=FALSE)
  end if
  call mplt%initialize(canvas, 1,1)
  call xyscat%initialize(c_null_ptr, real(f), real(mtfY), &
  & xlabel='Spatial Frequency ['//trim(unitStr)//']'//c_null_char, &
  & ylabel='Modulation'//c_null_char, &
  & title='Diffraction MTF (polychromatic)'//c_null_char)
  call xyscat%setDataColorCode(PL_PLOT_BLUE)
  call xyscat%addXYPlot(real(f), real(mtfX))
  call xyscat%setDataColorCode(PL_PLOT_RED)
  call xyscat%addXYPlot(real(f), real(mtfDL))
  call xyscat%setDataColorCode(PL_PLOT_BLACK)
  call xyscat%setLineStyleCode(2)
  ! (the shared legend clips entries to about five characters)
  call xyscat%addLegend([character(len=30) :: 'Tan', 'Sag', 'Limit'])
  call mplt%set(1,1,xyscat)

  call finalizeGoPlot_new(mplt, psm, replot, objIdx)

end procedure mtf_go

module procedure mtf_thf_go
  ! MTF THF: the polychromatic diffraction modulation at one frequency (FRE),
  ! Y (tan) and X (sag), at each focus position FFO, FFO+IFO, ... (NFO) --
  ! CODE V's MTF CHT FOC.  Each position is the MTF with the image plane
  ! moved there (mtfAtFocus); the lens itself is not changed.
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only: ioConfig, sysConfig
  use kdp_utils, only: OUTKDP
  use type_utils, only: int2str, real2str
  use mod_system, only: sys_mode
  use focus_settings, only: focus_count, focus_position
  use plot_setting_manager, only: SETTING_MTF_FRE
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_fortran_env, only: real64
  IMPLICIT NONE

  type(c_ptr) :: canvas
  type(zoaplot) :: xyscat
  type(multiplot) :: mplt
  character(len=100) :: lineStr
  character(len=30) :: unitStr, lensUnits
  integer :: objIdx, iField, xpts, nPos, k
  logical :: replot, ok
  real(real64) :: fre, cutoff, dl(1), mY(1), mX(1)
  real(real64), allocatable :: z(:), mtfY(:), mtfX(:), mtfDL(:)

  call initializeGoPlot(psm, ID_PLOTTYPE_MTF_THF, "MTF Through Focus", replot, objIdx)

  iField = psm%getFieldSetting()
  xpts = psm%getPowerOfTwoImageSetting()
  fre = psm%getSettingValueByCode(SETTING_MTF_FRE)

  call mtfCutoff(iField, 'MTF THF', cutoff, ok)
  if (.not. ok) return

  nPos = focus_count()
  if (sys_mode() > 2.0_real64) then
    call zoa_emit("MTF THF: focus positions (FFO/IFO/NFO) need a focal system; nominal focus used", "red")
    nPos = 1
  end if
  if (nPos == 1) call zoa_emit("MTF THF: one focus position (NFO 1); set NFO for a through-focus curve", "red")
  if (fre >= cutoff) call zoa_emit("MTF THF: FRE "//trim(mtfNumText(fre))//" is beyond the cutoff "// &
  &   trim(real2str(cutoff, 4))//"; the modulation is zero", "red")

  allocate(z(nPos), mtfY(nPos), mtfX(nPos), mtfDL(nPos))
  dl = mtfDiffLimit([fre], cutoff)
  do k = 1, nPos
    z(k) = 0.0_real64
    if (sys_mode() <= 2.0_real64) z(k) = focus_position(k)
    call mtfAtFocus(iField, xpts, z(k), [fre], cutoff, mY, mX)
    mtfY(k) = mY(1)
    mtfX(k) = mX(1)
    mtfDL(k) = dl(1)
  end do

  unitStr = mtfFreqUnits()
  lensUnits = sysConfig%getLensUnitsText()

  ! Data tab
  if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  call OUTKDP('Polychromatic diffraction MTF through focus, field '//trim(int2str(iField))// &
  &           ', frequency '//trim(mtfNumText(fre))//' '//trim(unitStr))
  call OUTKDP('Diffraction limit '//trim(real2str(dl(1), 6))//', cutoff '// &
  &           trim(real2str(cutoff, 4))//' '//trim(unitStr))
  call OUTKDP('      Focus      Y (tan)     X (sag)')
  do k = 1, nPos
    write(lineStr, '(F11.5,2F12.6)') z(k), mtfY(k), mtfX(k)
    call OUTKDP(trim(lineStr))
  end do
  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

  if (HEADLESS_MODE) then
    canvas = c_null_ptr
  else
    canvas = hl_gtk_drawing_area_new(size=[1200,800], has_alpha=FALSE)
  end if
  call mplt%initialize(canvas, 1,1)
  call xyscat%initialize(c_null_ptr, real(z), real(mtfY), &
  & xlabel='Focus Shift ['//trim(lensUnits)//']'//c_null_char, &
  & ylabel='Modulation'//c_null_char, &
  & title='MTF Through Focus, '//trim(mtfNumText(fre))//' '//trim(unitStr)//c_null_char)
  call xyscat%setDataColorCode(PL_PLOT_BLUE)
  call xyscat%addXYPlot(real(z), real(mtfX))
  call xyscat%setDataColorCode(PL_PLOT_RED)
  call xyscat%addXYPlot(real(z), real(mtfDL))
  call xyscat%setDataColorCode(PL_PLOT_BLACK)
  call xyscat%setLineStyleCode(2)
  call xyscat%addLegend([character(len=30) :: 'Tan', 'Sag', 'Limit'])
  call mplt%set(1,1,xyscat)

  call finalizeGoPlot_new(mplt, psm, replot, objIdx)

end procedure mtf_thf_go

! The MTF cutoff frequency (shortest weighted wavelength, current SPACE) for
! field iField at the nominal focus.  CUTTOFF reads the field's chief ray and
! working F/number, so the field's FOB runs first.
subroutine mtfCutoff(iField, who, cutoff, ok)
  use global_widgets, only: sysConfig
  use zoa_output, only: zoa_emit
  use DATSPD, only: SPACEBALL
  use iso_fortran_env, only: real64
  integer, intent(in) :: iField
  character(len=*), intent(in) :: who
  real(real64), intent(out) :: cutoff
  logical, intent(out) :: ok
  interface
    subroutine CUTTOFF(FREQ1, FREQ2, ERROR)
      import real64
      real(real64) :: FREQ1, FREQ2
      logical :: ERROR
    end subroutine CUTTOFF
  end interface
  real(real64) :: freq1, freq2
  logical :: cutErr

  cutoff = 0.0_real64
  ok = .false.
  if (iField < 1 .or. iField > sysConfig%numFields) then
    call zoa_emit(who//': no field to analyse (no lens loaded?)', 'red')
    return
  end if
  call kdp_fob(sysConfig%relativeFields(2,iField), sysConfig%relativeFields(1,iField))
  cutErr = .false.
  call CUTTOFF(freq1, freq2, cutErr)
  ok = .not. cutErr
  cutoff = 0.0_real64
  if (cutErr) then
    call zoa_emit(who//': cannot compute the cutoff frequency for this system', 'red')
    return
  end if
  if (SPACEBALL == 1) then
    cutoff = freq2
  else
    cutoff = freq1
  end if
end subroutine mtfCutoff

! Polychromatic MTF, Y (tan) and X (sag), at frequencies f for field iField,
! with the image plane moved delta along the axis (a focus position).  The
! move is the image surface's own thickness (THI SI, a defocus the tracer
! honours), set for this calculation only and restored afterwards: FOB then
! traces the chief and differential rays to the moved plane, so the reference
! sphere is centred there and the wavefront is the defocused one.  The lens
! is not changed (no lens update, no replot).
subroutine mtfAtFocus(iField, xpts, delta, f, cutoff, mtfY, mtfX)
  use global_widgets, only: sysConfig
  use type_utils, only: int2str, real2str
  use DATMAI, only: REG
  use DATLEN, only: CPFNEXT, NEWIMG
  use mod_surface, only: surf_thickness, set_surf_thickness
  use iso_fortran_env, only: real64
  integer, intent(in) :: iField, xpts
  real(real64), intent(in) :: delta, f(:), cutoff
  real(real64), intent(out) :: mtfY(:), mtfX(:)
  real(real64) :: thick0
  integer :: k

  thick0 = surf_thickness(NEWIMG)
  if (delta /= 0.0_real64) call set_surf_thickness(NEWIMG, thick0 + delta)

  call kdp_fob(sysConfig%relativeFields(2,iField), sysConfig%relativeFields(1,iField))
  call PROCESSILENT('NRD, '//trim(int2str(xpts)))
  call kdp_silent_begin()
  call kdp_capfn(xpts)
  call kdp_silent_end()

  do k = 1, size(f)
    mtfY(k) = 0.0_real64
    mtfX(k) = 0.0_real64
    if (f(k) >= cutoff) cycle
    ! DOTF clears CPFNEXT after each call; the CAPFN data it uses is still
    ! the plot's own, so mark it current again (otherwise DOTF falls into its
    ! all-fields mode).  YACC/XACC: no printout, modulus left in REG(9).
    CPFNEXT = .true.
    call PROCESSILENT('DOTF YACC '//trim(real2str(f(k), 6)))
    mtfY(k) = REG(9)
    CPFNEXT = .true.
    call PROCESSILENT('DOTF XACC '//trim(real2str(f(k), 6)))
    mtfX(k) = REG(9)
  end do

  if (delta /= 0.0_real64) then
    call set_surf_thickness(NEWIMG, thick0)
    ! the field's rays at the real image again; the CAPFN wavefront was the
    ! moved plane's, so it is no longer current
    call kdp_fob(sysConfig%relativeFields(2,iField), sysConfig%relativeFields(1,iField))
    CPFNEXT = .false.
  end if
end subroutine mtfAtFocus

! Aberration-free reference: each weighted wavelength's circular-pupil MTF at
! its own cutoff (scaled from the shortest one), weight-averaged.
function mtfDiffLimit(f, cutoff) result(dl)
  use mod_system, only: sys_wavelength, sys_wl_weight
  use iso_fortran_env, only: real64
  real(real64), intent(in) :: f(:), cutoff
  real(real64) :: dl(size(f))
  real(real64) :: shortWl, wsum, x
  integer :: w, k

  shortWl = huge(1.0_real64)
  do w = 1, 10
    if (sys_wavelength(w) > 0.0_real64 .and. sys_wl_weight(w) > 0.0_real64) &
      shortWl = min(shortWl, sys_wavelength(w))
  end do
  dl = 0.0_real64
  wsum = 0.0_real64
  do w = 1, 10
    if (.not. (sys_wavelength(w) > 0.0_real64 .and. sys_wl_weight(w) > 0.0_real64)) cycle
    wsum = wsum + sys_wl_weight(w)
    do k = 1, size(f)
      x = f(k)/(cutoff*shortWl/sys_wavelength(w))
      if (x < 1.0_real64) dl(k) = dl(k) + sys_wl_weight(w)* &
        (2.0_real64/acos(-1.0_real64))*(acos(x) - x*sqrt(1.0_real64 - x*x))
    end do
  end do
  if (wsum > 0.0_real64) dl = dl/wsum
end function mtfDiffLimit

! A frequency for a title: 100, 62.5 (not 100.0000)
function mtfNumText(x) result(s)
  use iso_fortran_env, only: real64
  real(real64), intent(in) :: x
  character(len=30) :: s
  integer :: n
  write(s, '(F20.4)') x
  s = adjustl(s)
  n = len_trim(s)
  do while (n > 1 .and. s(n:n) == '0')
    n = n - 1
  end do
  if (s(n:n) == '.') n = n - 1
  s = s(1:n)
end function mtfNumText

function mtfFreqUnits() result(u)
  use mod_system, only: sys_mode
  character(len=30) :: u
  if (sys_mode() > 2.0d0) then
    u = 'cycles/mrad'
  else
    u = 'cycles/mm'
  end if
end function mtfFreqUnits

end submodule plot_functions_fft
