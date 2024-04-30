module kdp_interfaces

contains

subroutine POWSYM


   use global_widgets
   use handlers, only : updateTerminalLog
    use zoa_plot
    !use mod_plotopticalsystem


  integer(kind=c_int) :: ipick, i, endSurface, ii
 character(len=100) :: ftext, strTitle
 CHARACTER(LEN=*), PARAMETER  :: FMT1 = "(I5, F10.3, F10.3)"
 CHARACTER(LEN=*), PARAMETER  :: FMTHDR = "(A12, A5, A5)"
 character(len=100) :: conLong
 real, ALLOCATABLE :: w(:), symcalc(:)
 real :: w_sum, s_sum, aplanatic, imageNA
 integer :: totalSurfaces
 integer, ALLOCATABLE ::  surfaceno(:)


   PRINT *, "New Command POWSYM in kdp_interfaces!"


  !PRINT *, "Before Mod Call, ", my_window

  !ipick = hl_gtk_file_chooser_show(new_files, &
  !       & create=FALSE, multiple=TRUE, filter=["image/*"], &
  !       & parent=my_window, all=TRUE)

  !ftext = 'LIB GET 1 '
  !ftext = 'CV2PRG DoubleGauss.seq'
  ! ftext = 'CV2PRG LithoBraat.seq'
  !
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'RTG ALL'
  ! CALL PROCESKDP(ftext)
  !
  !
  ! ftext = 'COLORSET RAYS 6'
  ! CALL PROCESKDP(ftext)
  ! call getOpticalSystemLastSurface(endSurface)
  ! call ld_settings%set_end_surface(endSurface)
  ! ftext = 'VIECO'
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'PXTY ALL'
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'OCDY'
  ! CALL PROCESKDP(ftext)


PRINT *, "Magnification is ", curr_par_ray_trace%t_mag




  ! Compute lens weight and symmetry
  allocate(w(curr_lens_data % num_surfaces-2))
  allocate(symcalc(curr_lens_data % num_surfaces-2))
  allocate(surfaceno(curr_lens_data % num_surfaces-2))

  PRINT *, "SIZE OF w is ", size(w)
  PRINT *, "SIZE of no_surfaces is ", size(surfaceno)

  WRITE(conLong, FMTHDR) "Surface", "w_j", "s_j"
  call updateTerminalLog(conLong, "black")


  do ii = 2, curr_lens_data % num_surfaces - 1
     w(ii-1) = -1/(1-curr_par_ray_trace%t_mag)
     w(ii-1) = w(ii-1) * (curr_lens_data % surf_index(ii) - curr_lens_data % surf_index(ii-1)) &
     & * curr_par_ray_trace % marginal_ray_height(ii) * curr_lens_data % curvatures(ii) &
     & / curr_lens_data % surf_index(curr_lens_data % num_surfaces) &
     & / curr_par_ray_trace % marginal_ray_angle(curr_lens_data % num_surfaces)
     w_sum = w_sum + w(ii-1)*w(ii-1)

     !PRINT *, "Check Ref Stop ", curr_lens_data % ref_stop
    !PRINT *, "SURF INDEX ", curr_lens_data % surf_index(ii)
    !PRINT *, "AOI is ", curr_par_ray_trace % chief_ray_aoi(ii)
    !allocate(symcalc(curr_lens_data % num_surfaces-2))

      totalSurfaces = curr_lens_data % num_surfaces
      aplanatic = curr_par_ray_trace % marginal_ray_angle(ii) / curr_lens_data % surf_index(ii)
      aplanatic = aplanatic - curr_par_ray_trace % marginal_ray_angle(ii-1) / curr_lens_data % surf_index(ii-1)
      imageNA = curr_lens_data%surf_index(totalSurfaces) * curr_par_ray_trace%marginal_ray_angle(totalSurfaces)

      !PRINT *, "IMAGE NA is ", imageNA
      !PRINT *, "APLANATIC IS ", aplanatic
      !PRINT *, "Marginal Ray Angle is ", curr_par_ray_trace % marginal_ray_angle(ii)
      !PRINT *, "Surface Index is ", curr_lens_data % surf_index(ii)
      symcalc(ii-1) = 1/(1-curr_par_ray_trace%t_mag)
      ! symcalc(ii-1) = symcalc(ii-1) * aplanatic * curr_lens_data % surf_index(ii) * curr_par_ray_trace % chief_ray_aoi(ii) &
      ! & / ((curr_lens_data % surf_index(curr_lens_data%ref_stop)* curr_par_ray_trace % chief_ray_aoi(curr_lens_data%ref_stop)) &
      ! & * imageNA)

      symcalc(ii-1) = symcalc(ii-1) * curr_lens_data % surf_index(ii-1) * curr_par_ray_trace % chief_ray_aoi(ii)

      !PRINT *, "FIrst Term is ", symcalc(ii-1)

      symcalc(ii-1) = symcalc(ii-1) /curr_lens_data%surf_index(curr_lens_data%ref_stop)
      symcalc(ii-1) = symcalc(ii-1) /curr_par_ray_trace%chief_ray_aoi(curr_lens_data%ref_stop)
      symcalc(ii-1) = symcalc(ii-1)*aplanatic/imageNA


      s_sum = s_sum + symcalc(ii-1)*symcalc(ii-1)

      surfaceno(ii-1) = ii-1

      WRITE(conLong, FMT1) surfaceno(ii-1), w(ii-1), symcalc(ii-1)
      call updateTerminalLog(conLong, "black")


  end do

  w_sum = SQRT(w_sum/(curr_lens_data % num_surfaces-2))
  s_sum = SQRT(s_sum/(curr_lens_data % num_surfaces-2))

  !PRINT *, " w is ", w
  WRITE(conLong, *) " w_sum is ", w_sum
  call updateTerminalLog(conLong, "black")

  !PRINT *, " s is ", symcalc
  WRITE(conLong, *) " s_sum is ", s_sum
  call updateTerminalLog(conLong, "black")

    !call barchart2(x,y)
    ! print *, "Calling Plotter"
     !plotter = barchart(drawing_area_plot,surfaceno,abs(w))
     !call plotter % initialize(drawing_area_plot,surfaceno,abs(w))

     ! call plotter % setxlabel
     ! call plotter % setMajorGridLines(TRUE)

     !call plotter % drawPlot()
    ! print *, "Done calling plotter"

  !PRINT *, "Paraxial Data tst ", curr_par_ray_trace%marginal_ray_height

! Multiplot
  !call POWSYM_PLOT(surfaceno, w, w_sum, symcalc, s_sum)
  call powsym_ideal(surfaceno, w, w_sum, symcalc, s_sum)


end subroutine POWSYM

! This routine needs to be rewritten to be similar to the
! rmsfield plot once the interface supports multiplots
! Since it is outside the desired infrastructure it doesn't
! support refresh plots or any settings
subroutine POWSYM_PLOT(surfaceno, w, w_sum, symcalc, s_sum)

   use global_widgets

  use zoa_plot
  use zoa_tab
  use gtk_draw_hl
  !use zoa_tab_manager
  use handlers, only: zoatabMgr, updateTerminalLog

  implicit none

 real, intent(in) :: w(:), symcalc(:)
 real, intent(in) :: w_sum, s_sum
 integer, intent(in) ::  surfaceno(:)

 character(len=100) :: strTitle
  type(barchart) :: bar1, bar2
  type(multiplot) :: mplt
  type(c_ptr) :: localcanvas
  type(zoatab) :: powsym_tab

    type(c_ptr) :: currPage
    integer(kind=c_int) :: currPageIndex
    integer :: idx
    character(len=3) :: outChar

  !PRINT *, "About to init POWSYM Tab"
  !PRINT *, "NOTEBOOK PTR IS ", LOC(notebook)
  call powsym_tab%initialize(notebook, "Power and Symmetry", -1)
  idx = zoatabMgr%findTabIndex()
  !call zoaTabMgr%addPlotTab("Power and Symmetry", ID_PLOTTYPE_GENERIC)

  !tabObj = zoaTabMgr%addPlotTab("Power and Symmetry", ID_PLOTTYPE_GENERIC)
  !tabObj%getCanvas() or tabObj%setCanvas(localCanvas)
  !....
  !tabObj%finalizeWindow

    localcanvas = hl_gtk_drawing_area_new(size=[1200,500], &
         & has_alpha=FALSE)

  powsym_tab%canvas = localcanvas
  !powsym_tab = zoatabMgr%addPlotTab(-1, "Power and Symmetry", localcanvas)

  !PRINT *, "POWSYM Initialized!"

  WRITE(strTitle, "(A15, F10.3)") "Power:  w = ", w_sum
  call mplt%initialize(powsym_tab%canvas, 2,1)
  !call mplt%initialize(drawing_area_plot, 2,1)
  !PRINT *, "MPLOT INITIALIZED!"
  call bar1%initialize(c_null_ptr, real(surfaceno),abs(w), &
  & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
  & title=trim(strTitle)//c_null_char)
  !PRINT *, "Bar chart color code is ", bar1%dataColorCode
  WRITE(strTitle, "(A15, F10.3)") "Symmetry:  s = ", s_sum
  call bar2%initialize(c_null_ptr, real(surfaceno),abs(symcalc), &
  & xlabel='Surface No'//c_null_char, ylabel='s'//c_null_char, &
  & title=trim(strTitle)//c_null_char)
  call bar2%setDataColorCode(PL_PLOT_BLUE)
  call mplt%set(1,1,bar1)
  call mplt%set(2,1,bar2)
  call mplt%draw()



  PRINT *, "ABOUT TO FINALIZE NEW TAB! Power Symmetry plot!"
  call powsym_tab%finalizeWindow()
  PRINT *, "After finalize window in power symmetry plot"

    ! Until this is ported to the new functionality, hard
    ! code an idx so the app doesn't crash if the user tries to
    ! close a tab
    currPageIndex = gtk_notebook_get_current_page(notebook)
    currPage = gtk_notebook_get_nth_page(notebook, currPageIndex)
    WRITE(outChar, '(I0.3)') idx
    call gtk_widget_set_name(currPage, outChar//c_null_char)

end subroutine

subroutine SPR

    USE GLOBALS
    use command_utils
    use handlers, only: zoatabMgr, updateTerminalLog
    use global_widgets, only:  sysConfig
    use zoa_ui
    use iso_c_binding, only:  c_ptr, c_null_char
    use plot_setting_manager


    IMPLICIT NONE

    character(len=23) :: ffieldstr
    character(len=40) :: inputCmd
    integer :: ii, objIdx
    integer :: numPoints = 10
    logical :: replot


    REAL, allocatable :: x(:), y(:)
    type(zoaplot_setting_manager) :: psm

    INCLUDE 'DATMAI.INC'

    !call checkCommandInput(ID_CMD_ALPHA)

    call updateTerminalLog(INPUT, "blue")
    inputCmd = INPUT

    call psm%initialize(trim(INPUT))
    numPoints = psm%addDensitySetting(10, 5, 50)
    inputCmd = trim(psm%sp%getCommand())      


    PRINT *, "numPoints is ", numPoints

    allocate(x(numPoints))
    allocate(y(numPoints))

    do ii = 0, numPoints-1
      x(ii+1) = REAL(ii)/REAL(numPoints-1)
      write(ffieldstr, *) x(ii+1)
      CALL PROCESKDP("FOB "// ffieldstr)
      CALL PROCESKDP("SPD")
      CALL PROCESKDP("SHO RMS")
      y(ii+1) = REG(9)
    end do

    replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_SPOT_VS_FIELD, objIdx)

    if (replot) then
    PRINT *, "SPOT RMS VS FIELD REPLOT REQUESTED"
    PRINT *, "Input Command was ", inputCmd
    call zoatabMgr%updateInputCommand(objIdx, inputCmd)
    !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

    call zoatabMgr%updateGenericPlotTab(objIdx, x, y)

    else
    objIdx = zoatabMgr%addGenericPlotTab(ID_PLOTTYPE_SPOT_VS_FIELD, "Spot RMS vs Field"//c_null_char, x,y, &
    & xlabel=trim(sysConfig%getFieldText())//c_null_char, &
      & ylabel="RMS ["//trim(sysConfig%getLensUnitsText())//"]"//c_null_char, &
      & title='Spot RMS Size vs Field'//c_null_char, linetypecode=-1)

    ! Add settings
    call psm%finalize(objIdx, trim(inputCmd))

    ! Create Plot + settings tab
    call zoaTabMgr%finalizeNewPlotTab(objIdx)


    end if

end subroutine

subroutine FIR
  use GLOBALS
  use parax_calcs
  use global_widgets
  use iso_fortran_env, only: real64
  use type_utils, only: real2str
  use handlers, only: updateTerminalLog

  implicit none
  !real(kind=real64) :: epRad, epPos

  include "DATMAI.INC"

  ! TODO:  This needs cleanup
  ! All these values should be calculated elsewhere
  ! Make sure that calcs are correct for all focal/afocal conditions

  !call calcExitPupil(epRad, epPos)
  call curr_lens_data%update()
  !PRINT *, "num surfaces is ", curr_lens_data%num_surfaces
  !call LogTermFOR("Exit Pupil Radius is "//real2str(epRad))
  !PRINT *, "Angle is ", curr_par_ray_trace % marginal_ray_angle(curr_lens_data % num_surfaces)

  !call logger%logText("FIR Called!")

  PRINT *, "Magnification is ", curr_par_ray_trace%t_mag

  call curr_par_ray_trace%calculateFirstOrderParameters(curr_lens_data)

  !print *, "Is Object at Infinity? ", sysConfig%isObjectAfInf()

  call updateTerminalLog("INFINITE CONJUGATES", "blue")
  call updateTerminalLog("EFL       "//trim(real2str(curr_par_ray_trace%EFL,4)),"blue")
  call updateTerminalLog("BFL       "//trim(real2str(curr_par_ray_trace%BFL,4)),"blue")
  call updateTerminalLog("FFL       "//trim(real2str(curr_par_ray_trace%FFL,4)),"blue")
  
  if (sysConfig%isObjectAfInf()) then
    call updateTerminalLog("FNO       "//trim(real2str(curr_par_ray_trace%FNUM,4)),"blue")
  call updateTerminalLog("IMG DIS   "//trim(real2str(curr_par_ray_trace%imageDistance,4)),"blue")
  call updateTerminalLog("OAL       "//trim(real2str(curr_par_ray_trace%OAL,4)),"blue")
  call updateTerminalLog("PARAXIAL IMAGE", "blue")
  ! TODO:  Move this calc somewhere else
  call PROCESKDP("GET GPCY")
  call updateTerminalLog(" HT      "//trim(real2str(reg(9),4)),"blue")
  call PROCESKDP("GET GPUCY")
  call updateTerminalLog("ANG       "//trim(real2str(reg(9),4)),"blue")
  call updateTerminalLog("ENTRANCE PUPIL", "blue")
  call updateTerminalLog("DIA       "//trim(real2str(curr_par_ray_trace%ENPUPDIA,4)),"blue")
  call updateTerminalLog("THI       "//trim(real2str(curr_par_ray_trace%ENPUPPOS,4)),"blue")
  call updateTerminalLog("EXIT PUPIL", "blue")
  call updateTerminalLog("DIA       "//trim(real2str(curr_par_ray_trace%EXPUPDIA,4)),"blue")
  call updateTerminalLog("THI       "//trim(real2str(curr_par_ray_trace%EXPUPPOS,4)),"blue")
  else
    call updateTerminalLog("FNO       "//trim(real2str(curr_par_ray_trace%EFL / &
    & curr_par_ray_trace%ENPUPDIA,4)),"blue")      
    
    call updateTerminalLog("AT USED CONJUGATES", "blue")

    call updateTerminalLog("RED       "//trim(real2str(-1*curr_par_ray_trace%t_mag,4)),"blue")
    call updateTerminalLog("FNO       "//trim(real2str(curr_par_ray_trace%imageDistance/ &
    & curr_par_ray_trace%EXPUPDIA,4)),"blue")
    call updateTerminalLog("OBJ DIS   "//trim(real2str(curr_par_ray_trace%objectDistance,4)),"blue")
    call updateTerminalLog("TT        "//trim(real2str(curr_par_ray_trace%TT,4)),"blue")
    call updateTerminalLog("IMG DIS   "//trim(real2str(curr_par_ray_trace%imageDistance,4)),"blue")
    call updateTerminalLog("OAL       "//trim(real2str(curr_par_ray_trace%OAL,4)),"blue")
  end if


  !call PROCESKDP("GET GPCY")
  !PRINT *, "GPCY IS ", reg(9)

  !call PROCESKDP("GET GPY")
  !PRINT *, "GPUY IS ", reg(9)

  !call LogTermFOR("Exit Pupil Diameter is "//real2str(curr_par_ray_trace%EXPUPDIA))
  

end subroutine

subroutine RMSFIELD
use GLOBALS
use global_widgets
use handlers, only : updateTerminalLog
 use zoa_plot
   character(len=23) :: ffieldstr
   integer :: ii
   integer :: numPoints = 10

   INCLUDE 'DATMAI.INC'

   ! do ii = 0, numPoints
   !   write(ffieldstr, *), REAL(ii)/REAL(numPoints)
   !   CALL PROCESKDP("FOB "// ffieldstr)
   !   CALL PROCESKDP("CAPFN")
   !   CALL PROCESKDP("SHO RMSOPD")
   !
   !   PRINT *, "REG9 9 is ", REG(9)
   !
   !   !PRINT *, "FOB " // ffieldstr
   !
   ! end do


   call logger%logText('RMSField Routine Starting')



   !call RMSFIELD_PLOT
   call rmsfield_plot

end subroutine


subroutine EDITOR
  use lens_editor
  use global_widgets
  use handlers, only: my_window

    if (.not. c_associated(lens_editor_window))  THEN
       PRINT *, "Call New Lens Editor Window"
       call lens_editor_new(my_window)
       PRINT *, "Lens editor call finished!"
    else
      PRINT *, "Do nothing..lens editor exists. "

    end if



end subroutine EDITOR


subroutine PLTOPD

  USE GLOBALS
  use command_utils
  use handlers, only: zoatabMgr, updateTerminalLog
  use global_widgets, only:  sysConfig, curr_opd
  use type_utils, only: int2str
  use zoa_ui
  use zoa_plot
  use iso_c_binding, only:  c_ptr, c_null_char
  use plplot, PI => PL_PI
  use plplot_extra
  use plotSettingParser
  use plot_setting_manager
  use gtk, only: gtk_expander_set_expanded


IMPLICIT NONE

character(len=23) :: ffieldstr
character(len=40) :: inputCmd
integer :: ii, i, j, objIdx
logical :: replot
type(setting_parser) :: sp
type(zoaplot_setting_manager) :: psm

! desirable commands
! n - density
! w - wavelength
! f - field
! s - surface (i=image, o=object)
! p - plot (future implementation eg image vs 3d Plot)
! azi alt - azimuth and altitude for 3d plot
! eg PLTOPD n64 w1 f3 si p0 azi30 alt60


!REAL, allocatable :: x(:), y(:)


    !   xdim is the leading dimension of z, xpts <= xdim is the leading
    !   dimension of z that is defined.
integer :: xpts, ypts
integer, parameter :: xdim=99, ydim=100 
integer :: index
character(len=80) :: tokens(40)
integer :: numTokens
integer :: lambda, fldIdx

integer, parameter :: nlevel = 10
real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)

real(kind=pl_test_flt)   :: dx, dy
type(c_ptr) :: canvas
!type(zoaPlot3d) :: zp3d 
type(zoaPlotImg) :: zp3d 
type(multiplot) :: mplt

INCLUDE 'DATMAI.INC'

PRINT *, "INPUT is ", INPUT
call updateTerminalLog(INPUT, "blue")

call psm%initialize(trim(INPUT))
lambda = psm%addWavelengthSetting()
fldIdx = psm%addFieldSetting()
xpts = psm%addDensitySetting(64, 8, 128)
ypts = xpts
inputCmd = trim(psm%sp%getCommand())
call updateTerminalLog("After PSM Cmd is "//trim(inputCmd), "blue")




!call psm%addWavelength()


!call parseCommandIntoTokens(trim(INPUT), tokens, numTokens, " ") 
!call sp%initialize(tokens(1:numTokens))
!PRINT *, "Wavelength is ", sp%getWavelength()
!PRINT *, "New CMD is ", sp%getCommand()
! Brainstorming
! A ty;e that has getters for all possible options.
! get cp%getWavelength
! if there is a wavelength token, return it 
! if not, return default
! Need a build command function as well.
! This should be tied to settings.  Every setting
! should have a command associated with it
! So another setting type with corresponding
! add commands.
! eg addWavefength(currValue)
! This would add the setting and set the value
! Then when setting changes it would be responsible for updating command

 

WRITE(INPUT, *) "FOB ", sysConfig%relativeFields(2,fldIdx) &
& , ' ' , sysConfig%relativeFields(1, fldIdx)
CALL PROCES

!CALL PROCESKDP('FOB 1')
PRINT *, "Calling CAPFN"
call PROCESKDP('CAPFN, '//trim(int2str(xpts)))
!call getOPDData(lambda)
!PRINT *, "Calling OPDLOD"
call PROCESKDP('FITZERN, '//trim(int2str(lambda)))
!call OPDLOD



 !call checkCommandInput(ID_CMD_ALPHA)

canvas = hl_gtk_drawing_area_new(size=[600,600], &
& has_alpha=FALSE)

call mplt%initialize(canvas, 1,1)
PRINT *, "size of X is ", size(curr_opd%X)
!PRINT *, "X is ", real(curr_opd%X)
 call zp3d%init3d(c_null_ptr, real(curr_opd%X),real(curr_opd%Y), & 
 & real(curr_opd%Z), xpts, ypts, & 
 & xlabel='X'//c_null_char, ylabel='Y'//c_null_char, &
 & title='Optical Path Difference'//c_null_char)

 call mplt%set(1,1,zp3d)



replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_OPD, objIdx)



if (replot) then

  call zoatabMgr%updateInputCommand(objIdx, inputCmd)
  !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
  call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

  !inputCmd = trim(psm%sp%getCommand())

 !PRINT *, "Input Command was ", inputCmd
 ! This is not the way I want to do this, but I have not come up with an elegant way to 
 ! keep track of all settings for a given plot without custom methods
 ! So here the trick is to close the plot, and open it with the new settings and expand
 ! the settings tab assuming this is what the user was doing.
 
  !call close_zoaTab()
 !objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_OPD, "Optical Path Difference"//c_null_char, mplt)


 ! Add settings
 !call psm%finalize(objIdx, trim(inputCmd))


 ! Create Plot + settings tab
! call zoaTabMgr%finalizeNewPlotTab(objIdx)
! call gtk_expander_set_expanded(zoatabMgr%tabInfo(objIdx)%tabObj%expander, TRUE)


else


 objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_OPD, "Optical Path Difference"//c_null_char, mplt)


 ! Add settings
 call psm%finalize(objIdx, trim(inputCmd))

 !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

 ! Create Plot + settings tab
 call zoaTabMgr%finalizeNewPlotTab(objIdx)



end if



end subroutine

subroutine PTSTUFF
  use type_utils, only: real2str, int2str

  integer :: i
  
  include "DATLEN.INC"
  call LogTermFOR("SYSTEM DUMP")
  do i=1,150
    call LogTermFOR("i="//trim(int2str(i)//" "//real2str(SYSTEM(i))))

  end do

end subroutine

subroutine PLTIMTST

  USE GLOBALS
  use command_utils
  use handlers, only: zoatabMgr, updateTerminalLog
  use global_widgets, only:  sysConfig
  use zoa_ui
  use zoa_plot
  use iso_c_binding, only:  c_ptr, c_null_char
  use plplot, PI => PL_PI
  use plplot_extra


IMPLICIT NONE

character(len=23) :: ffieldstr
character(len=40) :: inputCmd
integer :: ii, i, j, objIdx
integer :: numPoints = 10
logical :: replot


!REAL, allocatable :: x(:), y(:)


    !   xdim is the leading dimension of z, xpts <= xdim is the leading
    !   dimension of z that is defined.
integer, parameter :: xdim=99, ydim=100, xpts=35, ypts=45
real(kind=pl_test_flt)   :: x(xpts*ypts), y(xpts*ypts), z(xpts*ypts)
real(kind=pl_test_flt)   :: xx(xpts*ypts), yy(xpts*ypts), r
real(kind=pl_test_flt)   :: zlimited(xdim,ypts)
integer :: index
integer, parameter :: indexxmin = 1
integer, parameter :: indexxmax = xpts
integer            :: indexymin(xpts), indexymax(xpts)

! parameters of ellipse (in x, y index coordinates) that limits the data.
! x0, y0 correspond to the exact floating point centre of the index
! range.
! Note: using the Fortran convention of starting indices at 1
real(kind=pl_test_flt), parameter :: x0 = 0.5_pl_test_flt * ( xpts + 1 )
real(kind=pl_test_flt), parameter :: a  = 0.9_pl_test_flt * ( x0 - 1.0_pl_test_flt )
real(kind=pl_test_flt), parameter :: y0 = 0.5_pl_test_flt * ( ypts + 1 )
real(kind=pl_test_flt), parameter :: b  = 0.7_pl_test_flt * ( y0 - 1.0_pl_test_flt )
real(kind=pl_test_flt)            :: square_root

character (len=80) :: title(2) = &
       (/'#frPLplot Example 8 - Alt=60, Az=30 ', &
       '#frPLplot Example 8 - Alt=40, Az=-30'/)
real(kind=pl_test_flt)   :: alt(2) = (/60.0_pl_test_flt, 40.0_pl_test_flt/)
real(kind=pl_test_flt)   :: az(2)  = (/30.0_pl_test_flt,-30.0_pl_test_flt/)
integer            :: rosen
integer, parameter :: nlevel = 10
real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)

real(kind=pl_test_flt)   :: dx, dy
type(c_ptr) :: canvas
type(zoaPlotImg) :: zpImg 
type(multiplot) :: mplt

INCLUDE 'DATMAI.INC'




!   x(1:xpts) = (arange(xpts) - (xpts-1)/2.0_pl_test_flt) / ((xpts-1)/2.0_pl_test_flt)
!   y(1:ypts) = (arange(ypts) - (ypts-1)/2.0_pl_test_flt) / ((ypts-1)/2.0_pl_test_flt)
!

dx = 2.0_pl_test_flt / (xpts - 1)
dy = 2.0_pl_test_flt / (ypts - 1)

do i = 1,xpts
    x(i) = -1.0_pl_test_flt + (i-1) * dx
enddo

do j = 1,ypts
    y(j) = -1.0_pl_test_flt + (j-1) * dy
enddo

index = 1
do j=1,ypts    
    do i=1,xpts
      xx(index) = x(i)
        yy(index) = y(j)
            ! Sombrero function
            r = sqrt(xx(index)**2 + yy(index)**2)
            z(index) = exp(-r**2) * cos(2.0_pl_test_flt*PI*r)
            index = index+1
    enddo
enddo
print *, "index is ", index


 !call checkCommandInput(ID_CMD_ALPHA)

 call updateTerminalLog(INPUT, "blue")
 inputCmd = INPUT

if(cmdOptionExists('NUMPTS')) then
 numPoints = INT(getCmdInputValue('NUMPTS'))
end if


PRINT *, "numPoints is ", numPoints
canvas = hl_gtk_drawing_area_new(size=[600,600], &
& has_alpha=FALSE)

call mplt%initialize(canvas, 1,1)
!print *, "x in PLT3DTST is ", real(x)

!  call zp3d%init3d(c_null_ptr, real(xx),real(yy), real(z), xpts, ypts, &
!  & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
!  & title='Plot3dTst'//c_null_char)

 call zpImg%init3d(c_null_ptr, real(xx),real(yy), real(z), xpts, ypts, &
 & xlabel='No, an amplitude clipped sombrero'//c_null_char, ylabel=''//c_null_char, &
 & title='Saturn?'//c_null_char)
 !PRINT *, "Bar chart color code is ", bar1%dataColorCode

 call mplt%set(1,1,zpImg)



replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_PLT3DTST, objIdx)

if (replot) then
 PRINT *, "Input Command was ", inputCmd
 call zoatabMgr%updateInputCommand(objIdx, inputCmd)

 call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

else


 objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_PLT3DTST, "3D Plot Test"//c_null_char, mplt)

 ! Add settings

 zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

 ! Create Plot + settings tab
 call zoaTabMgr%finalizeNewPlotTab(objIdx)


end if



end subroutine

subroutine PLT3DTST

  USE GLOBALS
  use command_utils
  use handlers, only: zoatabMgr, updateTerminalLog
  use global_widgets, only:  sysConfig
  use zoa_ui
  use zoa_plot
  use iso_c_binding, only:  c_ptr, c_null_char
  use plplot, PI => PL_PI
  use plplot_extra


IMPLICIT NONE

character(len=23) :: ffieldstr
character(len=40) :: inputCmd
integer :: ii, i, j, objIdx
integer :: numPoints = 10
logical :: replot


!REAL, allocatable :: x(:), y(:)


    !   xdim is the leading dimension of z, xpts <= xdim is the leading
    !   dimension of z that is defined.
integer, parameter :: xdim=99, ydim=100, xpts=35, ypts=45
real(kind=pl_test_flt)   :: x(xpts*ypts), y(xpts*ypts), z(xpts*ypts)
real(kind=pl_test_flt)   :: xx(xpts*ypts), yy(xpts*ypts), r
real(kind=pl_test_flt)   :: zlimited(xdim,ypts)
integer :: index
integer, parameter :: indexxmin = 1
integer, parameter :: indexxmax = xpts
integer            :: indexymin(xpts), indexymax(xpts)

! parameters of ellipse (in x, y index coordinates) that limits the data.
! x0, y0 correspond to the exact floating point centre of the index
! range.
! Note: using the Fortran convention of starting indices at 1
real(kind=pl_test_flt), parameter :: x0 = 0.5_pl_test_flt * ( xpts + 1 )
real(kind=pl_test_flt), parameter :: a  = 0.9_pl_test_flt * ( x0 - 1.0_pl_test_flt )
real(kind=pl_test_flt), parameter :: y0 = 0.5_pl_test_flt * ( ypts + 1 )
real(kind=pl_test_flt), parameter :: b  = 0.7_pl_test_flt * ( y0 - 1.0_pl_test_flt )
real(kind=pl_test_flt)            :: square_root

character (len=80) :: title(2) = &
       (/'#frPLplot Example 8 - Alt=60, Az=30 ', &
       '#frPLplot Example 8 - Alt=40, Az=-30'/)
real(kind=pl_test_flt)   :: alt(2) = (/60.0_pl_test_flt, 40.0_pl_test_flt/)
real(kind=pl_test_flt)   :: az(2)  = (/30.0_pl_test_flt,-30.0_pl_test_flt/)
integer            :: rosen
integer, parameter :: nlevel = 10
integer :: plparseopts_rc
real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)

real(kind=pl_test_flt)   :: dx, dy
type(c_ptr) :: canvas
type(zoaPlot3d) :: zp3d 
type(multiplot) :: mplt

INCLUDE 'DATMAI.INC'

!   Process command-line arguments
plparseopts_rc = plparseopts(PL_PARSE_FULL)
if(plparseopts_rc .ne. 0) stop "plparseopts error"

rosen = 0

!   x(1:xpts) = (arange(xpts) - (xpts-1)/2.0_pl_test_flt) / ((xpts-1)/2.0_pl_test_flt)
!   y(1:ypts) = (arange(ypts) - (ypts-1)/2.0_pl_test_flt) / ((ypts-1)/2.0_pl_test_flt)
!

dx = 2.0_pl_test_flt / (xpts - 1)
dy = 2.0_pl_test_flt / (ypts - 1)

do i = 1,xpts
    x(i) = -1.0_pl_test_flt + (i-1) * dx
enddo

do j = 1,ypts
    y(j) = -1.0_pl_test_flt + (j-1) * dy
enddo

index = 1
do j=1,ypts    
    do i=1,xpts
      xx(index) = x(i)
        yy(index) = y(j)
            ! Sombrero function
            r = sqrt(xx(index)**2 + yy(index)**2)
            z(index) = exp(-r**2) * cos(2.0_pl_test_flt*PI*r)
            index = index+1
    enddo
enddo
print *, "index is ", index


 !call checkCommandInput(ID_CMD_ALPHA)

 call updateTerminalLog(INPUT, "blue")
 inputCmd = INPUT

if(cmdOptionExists('NUMPTS')) then
 numPoints = INT(getCmdInputValue('NUMPTS'))
end if


PRINT *, "numPoints is ", numPoints
canvas = hl_gtk_drawing_area_new(size=[600,600], &
& has_alpha=FALSE)

call mplt%initialize(canvas, 1,1)
!print *, "x in PLT3DTST is ", real(x)

!  call zp3d%init3d(c_null_ptr, real(xx),real(yy), real(z), xpts, ypts, &
!  & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
!  & title='Plot3dTst'//c_null_char)

 call zp3d%init3d(c_null_ptr, real(xx),real(yy), real(z), xpts, ypts, &
 & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
 & title='Plot3dTst'//c_null_char)
 !PRINT *, "Bar chart color code is ", bar1%dataColorCode

 call mplt%set(1,1,zp3d)



replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_PLT3DTST, objIdx)

if (replot) then
 PRINT *, "Input Command was ", inputCmd
 call zoatabMgr%updateInputCommand(objIdx, inputCmd)

 call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

else


 objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_PLT3DTST, "3D Plot Test"//c_null_char, mplt)

 ! Add settings

 zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

 ! Create Plot + settings tab
 call zoaTabMgr%finalizeNewPlotTab(objIdx)


end if



end subroutine


! subroutine PLT3DTST

!   USE GLOBALS
!   use command_utils
!   use handlers, only: zoatabMgr, updateTerminalLog
!   use global_widgets, only:  sysConfig
!   use zoa_ui
!   use zoa_plot
!   use iso_c_binding, only:  c_ptr, c_null_char
!   use plplot, PI => PL_PI
!   use plplot_extra


! IMPLICIT NONE

! character(len=23) :: ffieldstr
! character(len=40) :: inputCmd
! integer :: ii, i, j, objIdx
! integer :: numPoints = 10
! logical :: replot


! !REAL, allocatable :: x(:), y(:)


!     !   xdim is the leading dimension of z, xpts <= xdim is the leading
!     !   dimension of z that is defined.
! integer, parameter :: xdim=99, ydim=100, xpts=35, ypts=45
! real(kind=pl_test_flt)   :: x(xpts), y(ypts), z(xpts,ypts), xx, yy, r
! real(kind=pl_test_flt)   :: zlimited(xdim,ypts)
! integer, parameter :: indexxmin = 1
! integer, parameter :: indexxmax = xpts
! integer            :: indexymin(xpts), indexymax(xpts)

! ! parameters of ellipse (in x, y index coordinates) that limits the data.
! ! x0, y0 correspond to the exact floating point centre of the index
! ! range.
! ! Note: using the Fortran convention of starting indices at 1
! real(kind=pl_test_flt), parameter :: x0 = 0.5_pl_test_flt * ( xpts + 1 )
! real(kind=pl_test_flt), parameter :: a  = 0.9_pl_test_flt * ( x0 - 1.0_pl_test_flt )
! real(kind=pl_test_flt), parameter :: y0 = 0.5_pl_test_flt * ( ypts + 1 )
! real(kind=pl_test_flt), parameter :: b  = 0.7_pl_test_flt * ( y0 - 1.0_pl_test_flt )
! real(kind=pl_test_flt)            :: square_root

! character (len=80) :: title(2) = &
!        (/'#frPLplot Example 8 - Alt=60, Az=30 ', &
!        '#frPLplot Example 8 - Alt=40, Az=-30'/)
! real(kind=pl_test_flt)   :: alt(2) = (/60.0_pl_test_flt, 40.0_pl_test_flt/)
! real(kind=pl_test_flt)   :: az(2)  = (/30.0_pl_test_flt,-30.0_pl_test_flt/)
! integer            :: rosen
! integer, parameter :: nlevel = 10
! integer :: plparseopts_rc
! real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)

! real(kind=pl_test_flt)   :: dx, dy
! type(c_ptr) :: canvas
! type(zoaPlot3d) :: zp3d 
! type(multiplot) :: mplt

! INCLUDE 'DATMAI.INC'

! !   Process command-line arguments
! plparseopts_rc = plparseopts(PL_PARSE_FULL)
! if(plparseopts_rc .ne. 0) stop "plparseopts error"

! rosen = 0

! !   x(1:xpts) = (arange(xpts) - (xpts-1)/2.0_pl_test_flt) / ((xpts-1)/2.0_pl_test_flt)
! !   y(1:ypts) = (arange(ypts) - (ypts-1)/2.0_pl_test_flt) / ((ypts-1)/2.0_pl_test_flt)
! !

! dx = 2.0_pl_test_flt / (xpts - 1)
! dy = 2.0_pl_test_flt / (ypts - 1)

! do i = 1,xpts
!     x(i) = -1.0_pl_test_flt + (i-1) * dx
! enddo

! do j = 1,ypts
!     y(j) = -1.0_pl_test_flt + (j-1) * dy
! enddo

! do i=1,xpts
!     xx = x(i)
!     do j=1,ypts
!         yy = y(j)
!         if (rosen == 1) then
!             z(i,j) = (1._pl_test_flt - xx)**2 + 100._pl_test_flt*(yy - xx**2)**2

!             ! The log argument may be zero for just the right grid.
!             if (z(i,j) > 0._pl_test_flt) then
!                 z(i,j) = log(z(i,j))
!             else
!                 z(i,j) = -5._pl_test_flt
!             endif
!         else
!             ! Sombrero function
!             r = sqrt(xx**2 + yy**2)
!             z(i,j) = exp(-r**2) * cos(2.0_pl_test_flt*PI*r)
!         endif
!     enddo
! enddo


!  !call checkCommandInput(ID_CMD_ALPHA)

!  call updateTerminalLog(INPUT, "blue")
!  inputCmd = INPUT

! if(cmdOptionExists('NUMPTS')) then
!  numPoints = INT(getCmdInputValue('NUMPTS'))
! end if


! PRINT *, "numPoints is ", numPoints
! canvas = hl_gtk_drawing_area_new(size=[600,600], &
! & has_alpha=FALSE)

! call mplt%initialize(canvas, 1,1)
! print *, "x in PLT3DTST is ", real(x)

!  call zp3d%init3d(c_null_ptr, real(x),real(y), real(z), &
!  & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
!  & title='Plot3dTst'//c_null_char)
!  !PRINT *, "Bar chart color code is ", bar1%dataColorCode

!  call mplt%set(1,1,zp3d)



! replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_PLT3DTST, objIdx)

! if (replot) then
!  PRINT *, "Input Command was ", inputCmd
!  call zoatabMgr%updateInputCommand(objIdx, inputCmd)

!  call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

! else


!  objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_PLT3DTST, "3D Plot Test"//c_null_char, mplt)

!  ! Add settings

!  zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

!  ! Create Plot + settings tab
!  call zoaTabMgr%finalizeNewPlotTab(objIdx)


! end if



! end subroutine


subroutine MACROUI
  use zoa_macro_ui
  use global_widgets, only: macro_ui_window
  use handlers, only: my_window

  if (.not. c_associated(macro_ui_window))  THEN
    call zoa_macrooperationsUI(my_window)
 else
    call gtk_window_present(macro_ui_window)

 end if 



end subroutine MACROUI


subroutine SYSCONFIGUI
  use ui_sys_config
  use global_widgets
  use handlers, only: my_window

    if (.not. c_associated(sys_config_window))  THEN
       PRINT *, "Call New Sys Config Window"
       call sys_config_new(my_window)

    else
      PRINT *, "Do nothing..sys config window exists. "

    end if

end subroutine


SUBROUTINE RUN_WDRAWOPTICALSYSTEM
!     THIS IS THE DRIVER ROUTINE FOR SENDING GRAPHICS TO
!     A GRAPHIC WINDOW
      !USE WINTERACTER
      IMPLICIT NONE
      LOGICAL EXISD
      !INCLUDE 'DATMAI.INC'
      !CALL WDRAWOPTICALSYSTEM
      RETURN
END SUBROUTINE RUN_WDRAWOPTICALSYSTEM


subroutine powsym_ideal(surfaceno, w, w_sum, symcalc, s_sum)

  USE GLOBALS
  use command_utils
  use handlers, only: zoatabMgr, updateTerminalLog
  use global_widgets, only:  sysConfig
  use zoa_ui
  use zoa_plot
  use gtk_draw_hl 
  use iso_c_binding, only:  c_ptr, c_null_char


IMPLICIT NONE

real, intent(in) :: w(:), symcalc(:)
real, intent(in) :: w_sum, s_sum
integer, intent(in) ::  surfaceno(:)

character(len=23) :: ffieldstr
character(len=40) :: inputCmd
integer :: ii, objIdx
integer :: numPoints = 10
logical :: replot
type(c_ptr) :: canvas
type(barchart) :: bar1, bar2
type(multiplot) :: mplt
character(len=100) :: strTitle


INCLUDE 'DATMAI.INC'

 !call checkCommandInput(ID_CMD_ALPHA)

 call updateTerminalLog(INPUT, "blue")
 inputCmd = INPUT

 canvas = hl_gtk_drawing_area_new(size=[1200,500], &
 & has_alpha=FALSE)

 WRITE(strTitle, "(A15, F10.3)") "Power:  w = ", w_sum
 call mplt%initialize(canvas, 2,1)

 call bar1%initialize(c_null_ptr, real(surfaceno),abs(w), &
 & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
 & title=trim(strTitle)//c_null_char)
 !PRINT *, "Bar chart color code is ", bar1%dataColorCode
 WRITE(strTitle, "(A15, F10.3)") "Symmetry:  s = ", s_sum
 call bar2%initialize(c_null_ptr, real(surfaceno),abs(symcalc), &
 & xlabel='Surface No'//c_null_char, ylabel='s'//c_null_char, &
 & title=trim(strTitle)//c_null_char)
 call bar2%setDataColorCode(PL_PLOT_BLUE)
 call mplt%set(1,1,bar1)
 call mplt%set(2,1,bar2)



replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_POWSYM, objIdx)

if (replot) then
 PRINT *, "POWSYM REPLOT REQUESTED"
 PRINT *, "Input Command was ", inputCmd
 call zoatabMgr%updateInputCommand(objIdx, inputCmd)
 !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

 call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

else


  !call mplt%draw()


 objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_POWSYM, "Power and Symmetry"//c_null_char, mplt)

 ! Add settings
 PRINT *, "Really before crash?"
 zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
 PRINT *, "Really after crash?"

 ! Create Plot + settings tab
 call zoaTabMgr%finalizeNewPlotTab(objIdx)


end if

end subroutine

subroutine PLTFAN()

  USE GLOBALS
  use command_utils
  use handlers, only: zoatabMgr, updateTerminalLog
  use global_widgets, only:  sysConfig, curr_ray_fan_data
  use zoa_ui
  use zoa_plot
  use gtk_draw_hl 
  use iso_c_binding, only:  c_ptr, c_null_char
  use plot_setting_manager


IMPLICIT NONE


character(len=23) :: ffieldstr
character(len=40) :: inputCmd
CHARACTER(LEN=*), PARAMETER  :: FMTFAN = "(I1, A1, I3)"
integer :: ii, objIdx
integer :: numPoints, lambda 
logical :: replot
type(c_ptr) :: canvas
type(zoaplot) :: lineplot(4)
type(multiplot) :: mplt
character(len=100) :: strTitle(4)
character(len=1) :: strAxis(4)

REAL, allocatable :: x(:), y(:,:)
type(zoaplot_setting_manager) :: psm

INCLUDE 'DATMAI.INC'
INCLUDE 'DATLEN.INC'


  !call checkCommandInput(ID_CMD_ALPHA)

  call updateTerminalLog(INPUT, "blue")

  call psm%initialize(trim(INPUT))
  numPoints = psm%addDensitySetting(11, 5, 50)
  lambda = psm%addWavelengthSetting()
  inputCmd = trim(psm%sp%getCommand())      


PRINT *, "numPoints is ", numPoints

allocate(x(numPoints))
allocate(y(numPoints,4))

! Create Titles
strTitle(1) = "On Axis, X Error "
strTitle(2) = "On Axis, Y Error "
strTitle(3) = "Max Field, X Error "
strTitle(4) = "Max Field, Y Error "

strAxis(1) = 'X'
strAxis(2) = 'Y'
strAxis(3) = 'X'
strAxis(4) = 'Y'

write(ffieldstr, FMTFAN) lambda,',',numPoints

print *, "ffieldstr is ", trim(ffieldstr)

CALL PROCESKDP("FOB 0")
CALL PROCESKDP("XFAN, -1, 1, "//ffieldstr)
x = curr_ray_fan_data%relAper
y(1:numPoints,1) = curr_ray_fan_data%xyfan(1:numPoints,1)
CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
y(1:numPoints,2) = curr_ray_fan_data%xyfan(1:numPoints,2)
! FOB 1
CALL PROCESKDP("FOB 1")
CALL PROCESKDP("XFAN, -1, 1, "//ffieldstr)
y(1:numPoints,3) = curr_ray_fan_data%xyfan(1:numPoints,1)
CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
y(1:numPoints,4) = curr_ray_fan_data%xyfan(1:numPoints,2)




 canvas = hl_gtk_drawing_area_new(size=[1200,500], &
 & has_alpha=FALSE)


 call mplt%initialize(canvas, 2,2)

 do ii=1,4

 call lineplot(ii)%initialize(c_null_ptr, x,y(:,ii), &
 & xlabel='Relative '//strAxis(ii)//' Pupil Position'//c_null_char, & 
 & ylabel=strAxis(ii)//' Error ['// &
 & trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)//']'//c_null_char, &
 & title=trim(strTitle(ii))//c_null_char)
 !PRINT *, "Bar chart color code is ", bar1%dataColorCode
 end do
 
 call mplt%set(1,1,lineplot(1))
 call mplt%set(1,2,lineplot(2))
 call mplt%set(2,1,lineplot(3))
 call mplt%set(2,2,lineplot(4))




replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_FAN, objIdx)

if (replot) then
 PRINT *, "FAN REPLOT REQUESTED"
 PRINT *, "Input Command was ", inputCmd
 call zoatabMgr%updateInputCommand(objIdx, inputCmd)
 !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

 call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

else


  !call mplt%draw()


 objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_FAN, "Ray Aberration Fan"//c_null_char, mplt)

 ! Add settings
 call psm%finalize(objIdx, trim(inputCmd))

 ! Create Plot + settings tab
 call zoaTabMgr%finalizeNewPlotTab(objIdx)


end if

end subroutine

!pseudocode for rms_field minimal effort
! One method that processes inputs and provides x,y data (possible?)
! RMSFIELD_XY
! code to get x and y series from data
! ask zoaTabMgr if plotCode exists
! if yes, then this is a replot call.  Attach xy data to obj and call replot
! if no, then this is a new plot call.  create object (include title, x/y label, etc) and
! call addPlot in zoatabmgr
! For this, should move all settings into settings object and replot into zoaTab obj?
! this works if I only use commands for settings, then replot has an easier time.  Just
! assume have new xy or label data for that matter and redo.
! With this, just need to make methods and add new ID to zoa_ui.  Much cleaner.

subroutine rmsfield_plot

       USE GLOBALS
       use command_utils
       use handlers, only: zoatabMgr, updateTerminalLog
       use global_widgets, only:  sysConfig
       use zoa_ui
       use iso_c_binding, only:  c_ptr, c_null_char
       use plot_setting_manager


  IMPLICIT NONE

  character(len=23) :: ffieldstr
  character(len=40) :: inputCmd
  integer :: ii, objIdx
  integer :: numPoints = 10
  logical :: replot


    REAL, allocatable :: x(:), y(:)
    type(zoaplot_setting_manager) :: psm

    INCLUDE 'DATMAI.INC'

      !call checkCommandInput(ID_CMD_ALPHA)

      call updateTerminalLog(INPUT, "blue")

      call psm%initialize(trim(INPUT))
      numPoints = psm%addDensitySetting(10, 5, 50)
      inputCmd = trim(psm%sp%getCommand())      


    PRINT *, "numPoints is ", numPoints
    

    allocate(x(numPoints))
    allocate(y(numPoints))

    do ii = 0, numPoints-1
      x(ii+1) = REAL(ii)/REAL(numPoints-1)
      write(ffieldstr, *) x(ii+1)
      CALL PROCESKDP("FOB "// ffieldstr)
      CALL PROCESKDP("CAPFN")
      CALL PROCESKDP("SHO RMSOPD")
      x(ii+1) = x(ii+1)*sysConfig%refFieldValue(2)
      y(ii+1) = 1000.0*REG(9)
    end do

    replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_RMSFIELD, objIdx)

    if (replot) then
      PRINT *, "RMS FIELD REPLOT REQUESTED"
      PRINT *, "Input Command was ", inputCmd
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

      call zoatabMgr%updateGenericPlotTab(objIdx, x, y)

    else
      objIdx = zoatabMgr%addGenericPlotTab(ID_PLOTTYPE_RMSFIELD, "RMS vs Field"//c_null_char, x,y, &
      & xlabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
         & ylabel='RMS Error [mWaves]'//c_null_char, &
         & title='Wavefront Error vs Field'//c_null_char, linetypecode=-1)



      ! Add settings
      call psm%finalize(objIdx, trim(inputCmd))

      ! Create Plot + settings tab
      call zoaTabMgr%finalizeNewPlotTab(objIdx)


    end if

end subroutine

subroutine getOPDData(lambda)
  use iso_fortran_env, only: real64
  implicit none
  ! This is taken from PLOTCAPCO in PLOTCAD4.FOR.  
  integer :: lambda, loopFlag, I, KKV, KKK
  real(kind=real64) :: WVAL
  
  INCLUDE 'DATSP1.INC'
  INCLUDE 'DATSPD.INC'  

  loopFlag = 1

  WVAL=real(lambda)
  KKV=(ITOT-1)/NUMCOL
  do while (loopFlag > 0 )
  DO I=1,ITOT-1
!    LOAD DSPOTT(*,ID) INTO DSPOT(*)
     ID=I
     CALL SPOTIT(4)

     IF(DSPOT(16).EQ.WVAL) THEN
      PRINT *, "Found wavelength data for index ", lambda
      loopFlag = 0
      KKK=NINT(SQRT(FLOAT(KKV)))
      ! Need to define refht before calling this (add to curr_lens_data?)
      !CALL CAPPLOT(1,I,REFHT,WVAL,KKV,KKK,1)
      RETURN      
      !KVAL=I

          END IF

          END DO
        end do

end subroutine


subroutine plot_seidel()

  USE GLOBALS
  use command_utils
  use handlers, only: zoatabMgr, updateTerminalLog
  use global_widgets, only:  sysConfig, curr_lens_data,curr_par_ray_trace
  use zoa_ui
  use zoa_plot
  use gtk_draw_hl 
  use iso_c_binding, only:  c_ptr, c_null_char


IMPLICIT NONE

integer, parameter :: nS = 7 ! number of seidel terms to plot
real, allocatable, dimension(:,:) :: seidel
real, allocatable, dimension(:) :: surfIdx

character(len=23) :: ffieldstr
character(len=40) :: inputCmd
integer :: ii, objIdx, jj
logical :: replot
type(c_ptr) :: canvas
type(barchart), dimension(nS) :: barGraphs
integer, dimension(nS) :: graphColors
type(multiplot) :: mplt
character(len=100) :: strTitle
character(len=20), dimension(nS) :: yLabels
character(len=23) :: cmdTxt


INCLUDE 'DATMAI.INC'

call updateTerminalLog(INPUT, "blue")
inputCmd = INPUT

CALL PROCESKDP('MAB3 ALL')

allocate(seidel(nS,curr_lens_data%num_surfaces+1))
allocate(surfIdx(curr_lens_data%num_surfaces+1))



yLabels(1) = "Spherical"
yLabels(2) = "Coma"
yLabels(3) = "Astigmatism"
yLabels(4) = "Distortion"
yLabels(5) = "Curvature"
yLabels(6) = "Axial Chromatic"
yLabels(7) = "Lateral Chromatic"



graphColors = [PL_PLOT_RED, PL_PLOT_BLUE, PL_PLOT_GREEN, &
& PL_PLOT_MAGENTA, PL_PLOT_CYAN, PL_PLOT_GREY, PL_PLOT_BROWN]



surfIdx =  (/ (ii,ii=0,curr_lens_data%num_surfaces)/)
seidel(:,:) = curr_par_ray_trace%CSeidel(:,0:curr_lens_data%num_surfaces)

PRINT *, "lbound of surfIdx is ", lbound(surfIdx)
PRINT *, "lbound of seidel is ", lbound(seidel,2)
PRINT *, "lbound of CSeidel is ", lbound(curr_par_ray_trace%CSeidel,1)
PRINT *, "lbound of CSeidel is ", lbound(curr_par_ray_trace%CSeidel,2)



 canvas = hl_gtk_drawing_area_new(size=[1200,800], &
 & has_alpha=FALSE)


 call mplt%initialize(canvas, 1,nS)

 do jj=1,nS
  call barGraphs(jj)%initialize(c_null_ptr, real(surfIdx),seidel(jj,:), &
  & xlabel='Surface No (last item actually sum)'//c_null_char, & 
  & ylabel=trim(yLabels(jj))//c_null_char, &
  & title=' '//c_null_char)
  call barGraphs(jj)%setDataColorCode(graphColors(jj))
  barGraphs(jj)%useGridLines = .FALSE.
 end do

 do ii=1,nS
  call mplt%set(1,ii,barGraphs(ii))
 end do


replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_SEIDEL, objIdx)

if (replot) then
 PRINT *, "PLTSEI REPLOT REQUESTED"
 PRINT *, "Input Command was ", inputCmd
 call zoatabMgr%updateInputCommand(objIdx, inputCmd)
 !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

 call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)

else


  !call mplt%draw()


 objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_SEIDEL, "Seidel Aberrations"//c_null_char, mplt)

 ! Add settings
 zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
 

 ! Create Plot + settings tab
 call zoaTabMgr%finalizeNewPlotTab(objIdx)


end if

end subroutine

subroutine PLTZERN

    USE GLOBALS
    use command_utils
    use handlers, only: zoatabMgr, updateTerminalLog
    use global_widgets, only:  sysConfig
    use zoa_ui
    use zoa_plot
    use iso_c_binding, only:  c_ptr, c_null_char
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use plot_setting_manager


    IMPLICIT NONE

    character(len=23) :: ffieldstr
    character(len=40) :: inputCmd
    integer :: ii, objIdx, minZ, maxZ, lambda
    integer :: maxPlotZ = 9, numTermsToPlot, locE
    integer :: numPoints = 10
    logical :: replot
    type(multiplot) :: mplt
    type(zoaplot) :: zernplot
    type(c_ptr) :: canvas
    type(zoaplot_setting_manager) :: psm
    character(len=10) :: zernTxt

    character(len=5), allocatable :: zLegend(:)

    REAL, allocatable :: xdat(:), ydat(:,:)
    

    REAL*8 X(1:96)
    COMMON/SOLU/X

    INCLUDE 'DATMAI.INC'
    ! Max 3 terms.  Terms 1 and 2 are min and max zernikes
    ! Term 3 is the wavelength.  

    PRINT *, "INPUT is ", INPUT
   call updateTerminalLog(INPUT, "blue")

    call psm%initialize(trim(INPUT))
    lambda = psm%addWavelengthSetting()
    zernTxt = psm%addZernikeSetting("5..9")
    numPoints = psm%addDensitySetting(10,8,21)
    PRINT *, "zernTxt is ", zernTxt
    inputCmd = trim(psm%sp%getCommand())      

    ! TODO:  Need zernike parser to get minZ and maxZ
    locE = index(zernTxt, '..')
    minZ = str2int(zernTxt(1:locE-1))
    maxZ = str2int(zernTxt(locE+2:len(zernTxt)))
 


    
    ! Error checking

      if (minZ.LT.1.OR.minZ.GT.35) then
        call OUTKDP("Error:  Lower Zernike must be between 1 and 35", 1)
        call MACFAL
        return
      end if


      if (maxZ.LT.1.OR.maxZ.GT.36) then
        call OUTKDP("Error:  Upper Zernike must be between 1 and 35", 1)
        call MACFAL
        return
      end if

      numTermsToPlot = maxZ-minZ+1
      if (numTermsToPlot.GT.maxPlotZ) then
        call OUTKDP("Error:  Number of Terms must be less than or equal to 9", 1)
        call MACFAL
        return
      end if


      if (lambda.LT.1.OR.lambda.GT.10) then
      call OUTKDP("Error:  Wavelength Index must be between 1 and 10", 1)
      call MACFAL
      return
    end if      





    !call checkCommandInput(ID_CMD_ALPHA)

    call updateTerminalLog(INPUT, "blue")
    !inputCmd = "PLTZERN, "//trim(int2str(minZ))//','//trim(int2str(maxZ))//','//trim(int2str(lambda))
    !inputCmd = INPUT



    allocate(xdat(numPoints))
    allocate(ydat(numPoints,maxZ-minZ+1))
    allocate(zLegend(size(ydat,2)))

    PRINT *, "number of data columns is ", size(ydat,2)

    



    do ii = 0, numPoints-1
      xdat(ii+1) = REAL(ii)/REAL(numPoints-1)
      write(ffieldstr, *) xdat(ii+1)
      CALL PROCESKDP("FOB "// ffieldstr)
      CALL PROCESKDP("CAPFN")
      write(ffieldstr, *) lambda
      CALL PROCESKDP("FITZERN, "//ffieldstr)

      !CALL PROCESKDP("SHO RMSOPD")
      xdat(ii+1) = REAL(xdat(ii+1)*sysConfig%refFieldValue(2))
      ydat(ii+1,1:numTermsToPlot) = X(minZ:maxZ)
    end do

  

   
    canvas = hl_gtk_drawing_area_new(size=[1200,500], &
    & has_alpha=FALSE)
   
    call mplt%initialize(canvas, 1,1)
   
    call zernplot%initialize(c_null_ptr, xdat,ydat(:,1), &
    & xlabel=trim(sysConfig%getFieldText())//c_null_char, &
    & ylabel="Coefficient [waves]"//c_null_char, &
    & title='Zernike Coefficients vs Field'//c_null_char)
    zLegend(1) = 'Z'//trim(int2str(minZ))
    do ii=2,numTermsToPlot
      PRINT *, "ii is ", ii
      call zernplot%addXYPlot(xdat, ydat(:,ii))
      PRINT *, "After Zernplot add"
      call zernplot%setDataColorCode(2+ii)
      !call zernplot%setLineStyleCode(4)
      zLegend(ii) = 'Z'//trim(int2str(minZ+ii-1))
      PRINT *, "value is ", 'Z'//trim(int2str(minZ+ii-1))

    end do

    !Test
    
    call logDataVsField(xdat, ydat, zLegend)
    
    call zernplot%addLegend(zLegend)
    
    PRINT *, "zLegend is ", (zLegend)
    !PRINT *, "Final errors are ", ydat(10,:)

    call mplt%set(1,1,zernplot)

    replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx)

    if (replot) then
      PRINT *, "Zernike REPLOT REQUESTED"
      PRINT *, "Input Command was ", inputCmd
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
     
      call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)
     
     else
     
     
       !call mplt%draw()
     
     
      objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_ZERN_VS_FIELD, &
      & "Zernike vs Field"//c_null_char, mplt)

      ! Add settings
      call psm%finalize(objIdx, trim(inputCmd))

    
    ! Create Plot + settings tab
    call zoaTabMgr%finalizeNewPlotTab(objIdx)


    end if

end subroutine


end module kdp_interfaces
