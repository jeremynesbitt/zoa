module ui_spot
  use gtk
  use global_widgets
  use zoa_ui
  use iso_c_binding
  use zoa_tab


type, extends(ui_settings) :: spot_settings

   integer changed
   integer wavelength
   integer num_rings
   integer num_rand_rays
   integer rect_grid
   logical autoplotupdate
   type(idText), allocatable :: spotRays(:)
   !integer, dimension(3), allocatable :: rayDensityMinMax
   integer currSpotRaySetting
   integer currRayDensity
   integer idxWavelength, idxField
   type(c_ptr) :: canvas
   character(len=1024) :: plotCmd


 contains
    procedure, public :: is_changed => spot_is_changed

    !procedure, public, pass(self) :: set_ray_fan_wavelength
    procedure, public :: replot => spot_replot
    procedure :: buildPlotCommand
    procedure :: enableSpotUISettingsByTraceType


end type spot_settings

interface spot_settings
  module procedure :: spot_constructor
end interface spot_settings


type, extends(zoatab) :: spot_tab
contains
  procedure :: newPlot => spot_new
end type

  type(spot_settings) :: spot_struct_settings
  type(c_ptr) :: spinButton_spotRayDensity
  type(c_ptr) :: sB_rectGrid, sB_randnum, sB_ringNum, sB, raysPerRing

  !integer, parameter :: ID_SPOT_RAYDENSITY = 1604



contains

type(spot_settings) function spot_constructor(canvas) result(self)

    type(c_ptr) :: canvas
    self%autoplotupdate = .TRUE.
    !self%wavelength =

    self%canvas = canvas

    !PRINT *, "Spot Constructor Called!"
    !PRINT *, "Canvas is ", LOC(self%canvas)

    allocate(idText :: self%spotRays(3))

    self%spotRays(1)%text = "Rectangular"
    self%spotRays(1)%id = ID_SPOT_RECT


    self%spotRays(2)%text = "Random"
    self%spotRays(2)%id = ID_SPOT_RAND

    self%spotRays(3)%text = "Rings"
    self%spotRays(3)%id = ID_SPOT_RING

    self%currSpotRaySetting = ID_SPOT_RING


end function

function spot_is_changed(self) result(flag)
  class(spot_settings), intent(inout) :: self
  integer :: flag
  character(len=1024) :: tstCmd

  tstCmd = self%plotCmd
  PRINT *, "tstCmd is ", trim(tstCmd)
  call self%buildPlotCommand
  PRINT *, "plotCmd is ", trim(self%plotCmd)
  if (trim(tstCmd) /= trim(self%plotCmd)) then
     self%changed = 1
   else
     self%changed = 0
   end if

    flag = self%changed

end function

subroutine spot_replot(self)
  class(spot_settings) :: self

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=5) :: ftext
       call PROCESKDP(self%plotCmd)
       PRINT *, "About to call plot_spot for replot"
       call plot_spot(self%canvas)
!

end subroutine

subroutine buildPlotCommand(self)
  implicit none
  class(spot_settings) :: self
  character(len=9) :: fieldstr
  character(len=2) :: charWL
  character(len=80) :: charFLD
  character(len=80) :: charTrace
  integer :: i

  include "DATSP1.INC"

  self%plotCmd = 'SPOT ' ! Null it out

  call ITOA(spot_struct_settings%idxWavelength,charWL)
  call ITOA(spot_struct_settings%idxField,charFld)

  PRINT *, "charWL is ", charWL

  WRITE(charFLD, *) "FOB ", &
  & sysConfig%relativeFields(2,spot_struct_settings%idxField) &
  & , ' ' , sysConfig%relativeFields(1,spot_struct_settings%idxField)

  PRINT *, "charFLD is ", charFLD

  select case (self%currSpotRaySetting)
  case (ID_SPOT_RAND)
    write(fieldstr, '(I9)') self%num_rand_rays
    charTrace = "SPOT RAND;RANNUM "
    !self%plotCmd = "SPOT RAND;RANNUM "//trim(adjustl(fieldstr))//";SPD "//charWL
  case (ID_SPOT_RECT)
    write(fieldstr, '(I9)') self%rect_grid
    charTrace = "SPOT RECT;RECT "
    !self%plotCmd = "SPOT RECT;RECT "//trim(adjustl(fieldstr))//";SPD "//charWL
  case (ID_SPOT_RING)
    ! This is a bit of a hack.  redistribute ring number and rays per ring
    ! using KDP vars.  this should probably be moved to this type eventually
    do i=1,self%num_rings
          RINGRAD(i) = (REAL(i)/self%num_rings)*1D0
          RINGPNT(i) = INT(RINGRAD(i)*360)
    end do
    write(fieldstr, '(I9)') self%num_rings
    charTrace = "SPOT RING;RINGS "
    !self%plotCmd = "SPOT RING;RINGS "//trim(adjustl(fieldstr))//";SPD "//charWL

  end select
  self%plotCmd = trim(charFLD)//'; '//trim(charTrace)//" "// &
  & trim(adjustl(fieldstr))//";SPD "//charWL
  !PRINT *, "Plot command is ", trim(self%plotCMD)


end subroutine

subroutine enableSpotUISettingsByTraceType(self)
  class(spot_settings) :: self

  select case (self%currSpotRaySetting)

  case (ID_SPOT_RAND)
         call gtk_widget_set_sensitive(sB_randnum, TRUE)
         call gtk_widget_set_sensitive(sB_ringNum, FALSE)
         call gtk_widget_set_sensitive(sB_rectGrid, FALSE)
  case (ID_SPOT_RECT)
         call gtk_widget_set_sensitive(sB_randnum, FALSE)
         call gtk_widget_set_sensitive(sB_ringNum, FALSE)
         call gtk_widget_set_sensitive(sB_rectGrid, TRUE)
  case (ID_SPOT_RING)
         call gtk_widget_set_sensitive(sB_randnum, FALSE)
         call gtk_widget_set_sensitive(sB_ringNum, TRUE)
         call gtk_widget_set_sensitive(sB_rectGrid, FALSE)
  end select


end subroutine



subroutine spot_new(self)
  use zoa_tab
  !use ROUTEMOD
  !use kdp_draw, only: plot_ast_fc_dist
  use gtk_draw_hl
  use kdp_data_types, only: idText
  use g
  use GLOBALS
  use global_widgets, only: sysConfig
  !use handlers, only: plot_04debug
  implicit none

  type(c_ptr) :: parent_window, localcanvas, isurface
  class(spot_tab) :: self


  type(c_ptr) :: spinButton_numRays, spinButton_wavelength
  type(idText) :: spotTrace(3)
  integer :: usePLPLOT = 1
  ! Added these target parameters to have only one callback function and satisfy
  ! requirement to have a target attribute for a pointer for gtk.  I could not
  ! find a more elegant solution, and this seems better than a bunch of small
  ! callback functions
  !integer, target :: TARGET_RAYFAN_WAVELENGTH  = ID_WAVELENGTH

  !integer, target :: TARGET_SPOT_RAYDENSITY  = ID_SPOT_RAYDENSITY
  integer, target :: TARGET_SPOT_TRACE_ALGO = ID_SPOT_TRACE_ALGO
  integer, target :: TARGET_SPOT_GRID = ID_SPOT_RECT_GRID
  integer, target :: TARGET_SPOT_RANDNUM = ID_SPOT_RAND_NUMRAYS
  integer, target :: TARGET_SPOT_NUMRINGS = ID_SPOT_RING_NUMRINGS
  integer, target :: TARGET_SPOT_FIELD = ID_SPOT_FIELD
  integer, target :: TARGET_SPOT_WAVELENGTH = ID_SPOT_WAVELENGTH


  character(kind=c_char, len=20), dimension(2) :: vals_ast_fieldxy
  integer(c_int), dimension(2) :: refs_ast_fieldxy

  include "DATSP1.INC"



  !PRINT *, "Spot diagram new plot initiated!"
  !PRINT *, "DSPOTT is ", DSPOTT
  spotTrace(1)%text = "Rectangle"
  spotTrace(1)%id = ID_SPOT_RECT

  spotTrace(2)%text = "Ring"
  spotTrace(2)%id = ID_SPOT_RING

  spotTrace(3)%text = "Random"
  spotTrace(3)%id = ID_SPOT_RAND

  ! Get the current object field position



  !This has to be true now, but earlier this could be plotted
  !Both in PlPlot and using the original KDP plotting code
  if (usePLPLOT == 1) THEN
    PRINT *, "Plotting Spot Diagram via PL PLOT!"
    self%canvas = hl_gtk_drawing_area_new(size=[700,500], &
          & has_alpha=FALSE)

          spot_struct_settings = spot_settings(self%canvas)
          spot_struct_settings%idxWavelength = sysConfig%refWavelengthIndex


          call plot_spot(self%canvas)


  else

    !call gtk_drawing_area_set_draw_func(astfcdist_tab%canvas, &
    !                & c_funloc(ROUTEDRAWING), c_loc(TARGET_PLOTTYPE_AST), c_null_funptr)
  end if

  spinButton_spotRayDensity = gtk_spin_button_new (gtk_adjustment_new(value=10d0, &
                                                              & lower=1d0, &
                                                              & upper=1000d0, &
                                                              & step_increment=1d0, &
                                                              & page_increment=1d0, &
                                                              & page_size=1d0),climb_rate=1d0, &
                                                              & digits=0_c_int)

  !sB_rectGrid, sB_randnum, sB_ringNum, sB, raysPerRing

  ! Defaults for this are currently set in INITKDP.FOR

  sB_rectGrid = gtk_spin_button_new (gtk_adjustment_new(value=NRECT*1d0, &
                                                              & lower=1d0, &
                                                              & upper=300d0, &
                                                              & step_increment=1d0, &
                                                              & page_increment=1d0, &
                                                              & page_size=1d0),climb_rate=1d0, &
                                                              & digits=0_c_int)


  sB_randNum = gtk_spin_button_new (gtk_adjustment_new(value=RNUMBR*1d0, &
                                                              & lower=1d0, &
                                                              & upper=100000000d0, &
                                                              & step_increment=1d0, &
                                                              & page_increment=1d0, &
                                                              & page_size=1d0),climb_rate=1d0, &
                                                              & digits=0_c_int)

  sB_ringNum = gtk_spin_button_new (gtk_adjustment_new(value=RINGTOT*1d0, &
                                                              & lower=1d0, &
                                                              & upper=50d0, &
                                                              & step_increment=1d0, &
                                                              & page_increment=1d0, &
                                                              & page_size=1d0),climb_rate=1d0, &
                                                              & digits=0_c_int)

  spot_struct_settings%currSpotRaySetting =  SPDTYPE
  spot_struct_settings%num_rand_rays = RNUMBR
  spot_struct_settings%num_rings = RINGTOT
  spot_struct_settings%rect_grid = NRECT

  !TODO  This needs to be fixed.  For now assume default spot is last field position
  !Would like user to be able to control this
  spot_struct_settings%idxField = sysConfig%numFields

  call self%settings%addFieldSelection(c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_FIELD))

  call self%settings%addWavelengthSelection(c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_WAVELENGTH))


  call self%settings%addListBoxTextID("Spot Tracing Method", spotTrace, &
  & c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_TRACE_ALGO), &
  & spot_struct_settings%currSpotRaySetting)


 call self%addSpinBoxSetting("Rectangular Grid (nxn)", sB_rectGrid, &
 & c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_GRID))

 call self%addSpinBoxSetting("Number of Rays (Random Only)", sB_randnum, &
 & c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_RANDNUM))

 call self%addSpinBoxSetting("Number of Rings", sB_ringNum, &
 & c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_NUMRINGS))

   call spot_struct_settings%buildPlotCommand()
   call spot_struct_settings%enableSpotUISettingsByTraceType()


  call self%finalizeWindow()

end subroutine


subroutine callback_spot_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  select case (ID_SETTING)

  case (ID_SPOT_WAVELENGTH)
    spot_struct_settings%idxWavelength = INT(gtk_spin_button_get_value (widget))

  case (ID_SPOT_FIELD)
    spot_struct_settings%idxField = INT(gtk_spin_button_get_value (widget))


  case (ID_SPOT_TRACE_ALGO)
    spot_struct_settings%currSpotRaySetting = hl_zoa_combo_get_selected_list2_id(widget)
    call spot_struct_settings%enableSpotUISettingsByTraceType()

  case (ID_SPOT_RECT_GRID)
    spot_struct_settings%rect_grid = INT(gtk_spin_button_get_value (widget))

  case (ID_SPOT_RING_NUMRINGS)
    spot_struct_settings%num_rings = INT(gtk_spin_button_get_value (widget))

  case(ID_SPOT_RAND_NUMRAYS)
      spot_struct_settings%num_rand_rays = INT(gtk_spin_button_get_value (widget))

  end select

  if (spot_struct_settings%is_changed().EQ.1) call spot_struct_settings%replot()

end subroutine

 subroutine plot_spot(localcanvas)

        USE GLOBALS
        !use handlers
        use zoa_plot
        use g


     IMPLICIT NONE

     type(c_ptr)   :: localcanvas, my_cairo_context
     !type(c_ptr), value :: gdata
     type(c_ptr) ::  isurface
     !integer(c_int), value, intent(in) :: win_width, win_height
     type(zoaplot) :: xyscat1
     type(multiplot) :: mplt

     integer :: numPts, numPtsDist, numPtsFC

     REAL :: x, y

    !PRINT *, "PLOT_SPOT Started!"


    isurface = g_object_get_data(localcanvas, "backing-surface")
    PRINT *, "isurface is ", LOC(isurface)
    if (.not. c_associated(isurface)) then
       PRINT *, "error:  new plot :: Backing surface is NULL"
       return
    end if

    call mplt%initialize(localcanvas, 1,1)

    !PRINT *, "X SPOT ", REAL(DSPOTT(1,:))
    !PRINT *, "Y SPOT ", REAL(DSPOTT(2,:))

    !call filterRawSpotData(x,y)
    PRINT *, "label is"
    PRINT *, "label is ", sysConfig%lensUnits(sysConfig%currLensUnitsID)%text


    call xyscat1%initialize(c_null_ptr, REAL(pack(DSPOTT(1,:), &
    &                                   DSPOTT(1,:) /= 0 .and. DSPOTT(2,:) /=0)), &
    &                                   REAL(pack(DSPOTT(2,:), &
    &                                   DSPOTT(1,:) /= 0 .and. DSPOTT(2,:) /=0)), &
    !call xyscat1%initialize(c_null_ptr, REAL(DSPOTT(1,:)), &
    !&                                   REAL(DSPOTT(2,:)), &
    & xlabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    & ylabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    !& xlabel=' (x)'//c_null_char, ylabel='(y)'//c_null_char, &
    & title='Spot Diagram'//c_null_char)
    call xyscat1%setLineStyleCode(-1)
    !&  REAL(pack((DSPOTT(2,:)-SUM(DSPOTT(2,:)/SIZE(DSPOTT(2,:)))), &


    call mplt%set(1,1,xyscat1)
    !PRINT *, "localcanvas ", LOC(localcanvas)
    !PRINT *, "mplot area ", LOC(mplt%area)
    call mplt%draw()


end subroutine

! subroutine filterSpotData(x,y)
!   use globals
!   real, intent(inout) :: x, y
!   integer :: i
!
!   do
!
! end subroutine


end module
