module ui_spot
  use gtk
  use global_widgets
  use zoa_ui
  use iso_c_binding
  use zoa_tab


type, extends(ui_settings) :: spot_settings
   integer ast_field_dir
   integer ast_numRays
   integer changed
   integer wavelength
   logical autoplotupdate
   type(idText), allocatable :: spotRays(:)
   !integer, dimension(3), allocatable :: rayDensityMinMax
   integer currSpotRaySetting
   integer currRayDensity
   type(c_ptr) :: canvas
   character(len=140) ::astcalccmd
   character(len=140) ::distcalccmd
   character(len=140) ::fccalccmd



 contains
    procedure, public :: is_changed => spot_is_changed

    !procedure, public, pass(self) :: set_ray_fan_wavelength
    procedure, public :: replot => spot_replot


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
  integer, parameter :: ID_SPOT_RAND = 1601
  integer, parameter :: ID_SPOT_RECT = 1602
  integer, parameter :: ID_SPOT_RING = 1603
  integer, parameter :: ID_SPOT_RAYDENSITY = 1604


contains

type(spot_settings) function spot_constructor(canvas) result(self)

    type(c_ptr) :: canvas

    self%ast_field_dir = ID_AST_FIELD_Y
    self%ast_numRays = 10
    self%autoplotupdate = .TRUE.
    self%wavelength = 2 ! TODO NEED TO GET DEFAULT WAVELENGTH FROM PRESCRIPTION
    self%astcalccmd = "AST"
    self%distcalccmd = "DIST"
    self%fccalccmd = "FLDCV"

    self%canvas = canvas

    PRINT *, "Spot Constructor Called!"
    PRINT *, "Canvas is ", self%canvas

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
  class(spot_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function

subroutine spot_replot(self)
  class(spot_settings) :: self

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=5) :: ftext
       call PROCESKDP('SPD')
       PRINT *, "About to call plot_spot for replot"
       call plot_spot(self%canvas)
!

end subroutine



subroutine spot_new(self)
  use zoa_tab
  !use ROUTEMOD
  !use kdp_draw, only: plot_ast_fc_dist
  use gtk_draw_hl
  use g
  use GLOBALS
  !use handlers, only: plot_04debug
  implicit none

  type(c_ptr) :: parent_window, localcanvas, isurface
  class(spot_tab) :: self

  type(c_ptr) :: spinButton_numRays, spinButton_wavelength
  integer :: usePLPLOT = 1
  ! Added these target parameters to have only one callback function and satisfy
  ! requirement to have a target attribute for a pointer for gtk.  I could not
  ! find a more elegant solution, and this seems better than a bunch of small
  ! callback functions
  !integer, target :: TARGET_RAYFAN_WAVELENGTH  = ID_WAVELENGTH

  integer, target :: TARGET_SPOT_RAYDENSITY  = ID_SPOT_RAYDENSITY

  character(kind=c_char, len=20), dimension(2) :: vals_ast_fieldxy
  integer(c_int), dimension(2) :: refs_ast_fieldxy



  PRINT *, "Spot diagram new plot initiated!"
  !PRINT *, "DSPOTT is ", DSPOTT

  !call astfcdist_tab%initialize(notebook, "Astig FC Dist", ID_PLOTTYPE_AST)
  ! Create backing surface

  !isurface = cairo_image_surface_create(s_type, szx, szy)
  !isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
  !call g_object_set_data(astfcdist_tab%canvas, "backing-surface", isurface)

    !isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200_c_int, 500_c_int)
    !isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
    !call g_object_set_data(astfcdist_tab%canvas, "backing-surface", isurface)


  !ast_cairo_drawing_area = astfcdist_tab%canvas
  if (usePLPLOT == 1) THEN
    PRINT *, "Plotting Spot Diagram via PL PLOT!"
    self%canvas = hl_gtk_drawing_area_new(size=[700,500], &
          & has_alpha=FALSE)

          spot_struct_settings = spot_settings(self%canvas)

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




 call self%addSpinBoxSetting("Ray Density", spinButton_spotRayDensity, &
 & c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_RAYDENSITY))


  call self%finalizeWindow()
  !ast_window = astfcdist_tab%box1
  !PRINT *, "AT END OF new astig plot, canvas ptr is ", astfcdist_tab%canvas



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

  ! select case (ID_SETTING)
  !
  ! case (ID_AST_FIELDXY)
  !   call ast_settings % set_ast_field_dir(hl_zoa_combo_get_selected_list2_id(widget))
  !
  ! case (ID_RAYFAN_NUMRAYS)
  !   call ast_settings % set_ast_num_rays(INT(gtk_spin_button_get_value (widget)))
  !
  ! end select

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

    PRINT *, "PLOT_SPOT Started!"


    isurface = g_object_get_data(localcanvas, "backing-surface")
    PRINT *, "isurface is ", isurface
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


    call xyscat1%initialize(c_null_ptr, REAL(pack(DSPOTT(1,:),DSPOTT(1,:) /= 0)), &
    &                                   REAL(pack(DSPOTT(2,:), DSPOTT(2,:) /= 0)), &
    !call xyscat1%initialize(c_null_ptr, REAL(DSPOTT(1,:)), &
    !&                                   REAL(DSPOTT(2,:)), &
    & xlabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    & ylabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    !& xlabel=' (x)'//c_null_char, ylabel='(y)'//c_null_char, &
    & title='Spot Diagram'//c_null_char)
    call xyscat1%setLineStyleCode(-1)
    !&  REAL(pack((DSPOTT(2,:)-SUM(DSPOTT(2,:)/SIZE(DSPOTT(2,:)))), &


    call mplt%set(1,1,xyscat1)
    PRINT *, "localcanvas ", localcanvas
    PRINT *, "mplot area ", mplt%area
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
