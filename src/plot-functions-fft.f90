submodule (plot_functions) plot_functions_fft
implicit none
contains

module procedure psf_go
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


call initializeGoPlot(psm,ID_PLOTTYPE_PSF, "Point Spread Function", replot, objIdx)

lambda = psm%getWavelengthSetting()
fldIdx = psm%getFieldSetting()

WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,fldIdx) &
& , ' ' , sysConfig%relativeFields(1, fldIdx)
CALL PROCESKDP(trim(ffieldstr))

! Set this plot's own pupil grid, exactly as mtf_go does.  doPSF sizes its
! transform from the NRD/TGR globals, which without this were simply whatever
! the previous plot's CAPFN last left (a PSF after a 64-grid PMA came out at
! 64), and the Density setting execPSF adds was never applied at all.  CAPFN
! (not just NRD) so that NRD and the transform size TGR are set consistently.
pupilGrid = psm%getDensitySetting()
call PROCESSILENT('NRD, '//trim(int2str(pupilGrid)))
call PROCESSILENT('CAPFN, '//trim(int2str(pupilGrid)))

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

! Data tab: the peak and the X and Y sections through it (pixel indices, as
! the plot's axes).  It used to dump the whole image and its FFT, one image
! row per line -- thousands of numbers no one could read, in rows far longer
! than any line the test capture keeps.
if (.not. HEADLESS_MODE) then
  call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
end if
block
  integer :: pk(2), k
  character(len=80) :: lineStr
  pk = maxloc(psfData)
  call OUTKDP('Point spread function, field '//trim(int2str(fldIdx))// &
  &           ', wavelength '//trim(int2str(lambda))//', '// &
  &           trim(int2str(xpts))//' x '//trim(int2str(ypts))//' pixels')
  write(lineStr, '(A,I0,A,I0,A,F12.8)') 'Peak at pixel (', pk(1), ', ', pk(2), &
  &     '), relative intensity ', psfData(pk(1), pk(2))
  call OUTKDP(trim(lineStr))
  call OUTKDP('Section along the first index through the peak')
  call OUTKDP('   Pixel    Intensity')
  do k = 1, xpts
    write(lineStr, '(I8,F13.8)') k, psfData(k, pk(2))
    call OUTKDP(trim(lineStr))
  end do
  call OUTKDP('Section along the second index through the peak')
  call OUTKDP('   Pixel    Intensity')
  do k = 1, ypts
    write(lineStr, '(I8,F13.8)') k, psfData(pk(1), k)
    call OUTKDP(trim(lineStr))
  end do
end block

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

  WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,fldIdx) &
  & , ' ' , sysConfig%relativeFields(1, fldIdx)
  CALL PROCESKDP(trim(ffieldstr))

  ! Compute silently: CAPFN fills curr_opd (the map) and FITZERN fills the
  ! Zernike coefficients.  Their printed reports used to be dumped into the
  ! Data tab; it now gets structured data for the selected plot type instead.
  call PROCESSILENT('CAPFN, '//trim(int2str(xpts)))
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
  ! wavelength) on, so those frequencies are not computed.
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
  use DATMAI, only: REG
  use DATLEN, only: CPFNEXT
  use DATSPD, only: SPACEBALL
  use mod_system, only: sys_wavelength, sys_wl_weight, sys_mode
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_fortran_env, only: real64
  IMPLICIT NONE

  interface
    subroutine CUTTOFF(FREQ1, FREQ2, ERROR)
      import real64
      real(real64) :: FREQ1, FREQ2
      logical :: ERROR
    end subroutine CUTTOFF
  end interface

  integer, parameter :: MAX_FREQ_POINTS = 1001
  type(c_ptr) :: canvas
  type(zoaplot) :: xyscat
  type(multiplot) :: mplt
  character(len=230) :: ffieldstr
  character(len=100) :: lineStr
  character(len=30) :: unitStr
  integer :: objIdx, iField, xpts, nPts, k, w
  logical :: replot, cutErr
  real(real64) :: maxFreq, dFreq, freq1, freq2, cutoff, shortWl, wsum, x
  real(real64), allocatable :: f(:), mtfY(:), mtfX(:), mtfDL(:)

  call initializeGoPlot(psm, ID_PLOTTYPE_MTF, "MTF", replot, objIdx)

  iField = psm%getFieldSetting()
  xpts = psm%getPowerOfTwoImageSetting()
  maxFreq = psm%getSettingValueByCode(SETTING_MAX_FREQUENCY)
  dFreq = psm%getSettingValueByCode(SETTING_FREQUENCY_INTERVAL)

  write(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,iField), ' ', &
  &                   sysConfig%relativeFields(1,iField)
  call PROCESKDP(trim(ffieldstr))
  call PROCESSILENT('NRD, '//trim(int2str(xpts)))
  call PROCESSILENT('CAPFN, '//trim(int2str(xpts)))

  ! the cutoff frequency, in the current SPACE, at the shortest wavelength
  cutErr = .false.
  call CUTTOFF(freq1, freq2, cutErr)
  if (cutErr) then
    call zoa_emit('MTF: cannot compute the cutoff frequency for this system', 'red')
    return
  end if
  if (SPACEBALL == 1) then
    cutoff = freq2
  else
    cutoff = freq1
  end if
  if (maxFreq <= 0.0_real64) maxFreq = cutoff
  if (dFreq <= 0.0_real64) dFreq = maxFreq/100.0_real64
  nPts = int(maxFreq/dFreq + 1.0e-9_real64) + 1
  if (nPts > MAX_FREQ_POINTS) then
    call zoa_emit('MTF: MFR/IFR asks for '//trim(int2str(nPts))//' frequencies; using the first '// &
    &             trim(int2str(MAX_FREQ_POINTS)), 'red')
    nPts = MAX_FREQ_POINTS
  end if

  allocate(f(nPts), mtfY(nPts), mtfX(nPts), mtfDL(nPts))
  do k = 1, nPts
    f(k) = (k-1)*dFreq
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

  ! Aberration-free reference: each weighted wavelength's circular-pupil MTF
  ! at its own cutoff (scaled from the shortest one), weight-averaged.
  shortWl = huge(1.0_real64)
  do w = 1, 10
    if (sys_wavelength(w) > 0.0_real64 .and. sys_wl_weight(w) > 0.0_real64) &
      shortWl = min(shortWl, sys_wavelength(w))
  end do
  mtfDL = 0.0_real64
  wsum = 0.0_real64
  do w = 1, 10
    if (.not. (sys_wavelength(w) > 0.0_real64 .and. sys_wl_weight(w) > 0.0_real64)) cycle
    wsum = wsum + sys_wl_weight(w)
    do k = 1, nPts
      x = f(k)/(cutoff*shortWl/sys_wavelength(w))
      if (x < 1.0_real64) mtfDL(k) = mtfDL(k) + sys_wl_weight(w)* &
        (2.0_real64/acos(-1.0_real64))*(acos(x) - x*sqrt(1.0_real64 - x*x))
    end do
  end do
  if (wsum > 0.0_real64) mtfDL = mtfDL/wsum

  if (sys_mode() > 2.0_real64) then
    unitStr = 'cycles/mrad'
  else
    unitStr = 'cycles/mm'
  end if

  ! Data tab
  if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  call OUTKDP('Polychromatic diffraction MTF, field '//trim(int2str(iField))// &
  &           ', cutoff '//trim(real2str(cutoff, 4))//' '//trim(unitStr))
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

end submodule plot_functions_fft
