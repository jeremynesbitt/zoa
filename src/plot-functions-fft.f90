submodule (plot_functions) plot_functions_fft
implicit none
contains

module procedure psf_go
  use mod_fft, only: four2, fftshift, fft2
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only:  sysConfig, curr_opd
  use type_utils, only: int2str
  use plplot, PI => PL_PI
  use plplot_extra
  use mod_analysis_manager
  use iso_c_binding, only: c_ptr, c_null_ptr
  use kdp_utils

character(len=1024) :: ffieldstr

integer :: xpts, ypts
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

PRINT *, "fldIdx is ", fldIdx
WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,fldIdx) &
& , ' ' , sysConfig%relativeFields(1, fldIdx)
CALL PROCESKDP(trim(ffieldstr))

canvas = hl_gtk_drawing_area_new(size=[600,600], &
& has_alpha=FALSE)

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
call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
call logImageData(psfData)
call LogTermFOR("Real")
call logImageData(real(real(fftData),kind=long))
call LogTermFOR("Imag")
call logImageData(real(aimag(fftData),kind=long))

call ioConfig%restoreTextView()

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
    use global_widgets, only:  sysConfig, curr_opd
    use type_utils, only: int2str
    use plplot, PI => PL_PI
    use plplot_extra
    use iso_c_binding, only: c_ptr, c_null_ptr

  character(len=1024) :: ffieldstr

  integer :: xpts, ypts
  integer, parameter :: xdim=99, ydim=100
  integer :: lambda, fldIdx

  integer, parameter :: nlevel = 10

  type(c_ptr) :: canvas
  type(zoaPlotImg) :: zp3d
  type(multiplot) :: mplt


  lambda = psm%getWavelengthSetting()
  fldIdx = psm%getFieldSetting()
  xpts = psm%getDensitySetting()
  ypts = xpts


  PRINT *, "fldIdx is ", fldIdx
  WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,fldIdx) &
  & , ' ' , sysConfig%relativeFields(1, fldIdx)
  CALL PROCESKDP(trim(ffieldstr))

  PRINT *, "Calling CAPFN"
  call PROCESKDP('CAPFN, '//trim(int2str(xpts)))
  call PROCESKDP('FITZERN, '//trim(int2str(lambda)))

  canvas = hl_gtk_drawing_area_new(size=[600,600], &
  & has_alpha=FALSE)

  call mplt%initialize(canvas, 1,1)
  PRINT *, "size of X is ", size(curr_opd%X)
   call zp3d%init3d(c_null_ptr, real(curr_opd%X),real(curr_opd%Y), &
   & real(curr_opd%Z), xpts, ypts, &
   & xlabel='X'//c_null_char, ylabel='Y'//c_null_char, &
   & title='Optical Path Difference'//c_null_char)

   call mplt%set(1,1,zp3d)

   call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_OPD, "Optical Path Difference")



end procedure pma_go

module procedure mtf_go
  use mod_fft, only: fft2
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only:  curr_par_ray_trace, curr_lens_data, ioConfig, sysConfig, curr_mtf
  use kdp_utils, only: OUTKDP, logDataVsField, log2DData
  use type_utils, only: int2str, str2int, real2str
  use DATMAI
  use DATSPD, only: NRD
  use iso_c_binding, only: c_ptr, c_null_ptr
  use mod_analysis_manager


  IMPLICIT NONE

  integer, parameter :: nS = 7
  real, allocatable, dimension(:,:) :: seidel
  real, allocatable, dimension(:) :: surfIdx

  character(len=230) :: ffieldstr
  character(len=40) :: inputCmd
  integer :: ii, objIdx, jj
  logical :: replot
  type(c_ptr) :: canvas
  integer, dimension(nS) :: graphColors
  type(zoaplot) :: xyscat
  type(multiplot) :: mplt
  character(len=100) :: strTitle
  character(len=20), dimension(nS) :: yLabels
  character(len=23) :: cmdTxt
  integer :: iField
  character(len=80) :: charFLD
  type(image_data) :: imgPSF
  complex(long), allocatable :: fftData(:,:)
  real, allocatable :: xAxis(:), xPlt(:)
  real(long), allocatable :: yAxis(:), yPlt(:)

  integer :: lambda, fldIdx, xpts, iDiff

  real(long) :: fnum, imgNA, lambdaInUm, diffLimit


  call initializeGoPlot(psm,ID_PLOTTYPE_MTF, "MTF", replot, objIdx)

  iField = psm%getFieldSetting()
  xpts = psm%getPowerOfTwoImageSetting()
  lambda = psm%getWavelengthSetting()
  WRITE(charFLD, *) "FOB ", &
  & sysConfig%relativeFields(2,iField) &
  & , ' ' , sysConfig%relativeFields(1,iField)
  call PROCESKDP(trim(charFLD))
  call PROCESKDP('NRD, '//trim(int2str(xpts)))
  call PROCESKDP('CAPFN, '//trim(int2str(xpts)))

  call getData("PSFK", imgPSF)
  allocate(fftData(size(imgPsf%img,1),size(imgPsf%img,2)))
  fftData = fft2(cmplx(imgPsf%img,kind=long),1)

  allocate(xAxis(imgPsf%N/2))
  allocate(yAxis(imgPsf%N/2))

  imgNA = am%getImgNA()
  lambdaInUm = sysConfig%getWavelength(lambda)
  diffLimit = 2.0_long*1000.0_long*imgNA/lambdaInUm
  iDiff = 0
  do ii=1,imgPsf%N/2
    xAxis(ii) = 1000*(ii-1)/(imgPsf%N/2-1)/(imgPsf%pS)
    if (iDiff == 0 .and. xAxis(ii) > diffLimit) iDiff = ii+1
  end do
  yAxis = REAL(DABS(REAL(fftData(1,1:size(fftData,1)/2)))/DABS(REAL(fftData(1,1))),8)
  yAxis = yAxis**2

  print *, "Diff Limit is ", diffLimit
  print *, "Plot max index is ", iDiff

  allocate(xPlt(iDiff))
  allocate(yPlt(iDiff))
  xPlt(1:iDiff) = xAxis(1:iDiff)
  yPlt(1:iDiff) = yAxis(1:iDiff)

  call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  call log2DData(real(xAxis,8),yAxis)
  call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

   canvas = hl_gtk_drawing_area_new(size=[1200,800], &
   & has_alpha=FALSE)

   call mplt%initialize(canvas, 1,1)

   call xyscat%initialize(c_null_ptr, xPlt, REAL(yPlt,4), &
   & xlabel='Spatial Frequency [cycles/mm]'//c_null_char, &
   & ylabel='Modulation'//c_null_char, &
   & title='Diffraction MTF'//c_null_char)

   call mplt%set(1,1,xyscat)

   call finalizeGoPlot_new(mplt, psm, replot, objIdx)



end procedure mtf_go

end submodule plot_functions_fft
