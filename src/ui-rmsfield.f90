module ui_rmsfield
  use gtk
  use global_widgets
  use zoa_ui
  use iso_c_binding
  use zoa_tab


type, extends(ui_settings) :: rmsfield_settings
   integer changed
   logical autoplotupdate
   type(c_ptr) :: canvas


 contains
    procedure, public :: is_changed => rmsfield_is_changed
    procedure, public :: replot => rmsfield_replot


end type

interface rmsfield_settings
  module procedure :: rmsfield_constructor
end interface


type, extends(zoatab) :: rmsfieldtab
contains
  procedure :: newPlot => rmsfield_new
end type

  type(rmsfield_settings) :: rmsfield_struct_settings

contains

type(rmsfield_settings) function rmsfield_constructor(canvas) result(self)

    type(c_ptr) :: canvas
    self%autoplotupdate = .TRUE.
    self%canvas = canvas

end function

function rmsfield_is_changed(self) result(flag)
  class(rmsfield_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function

subroutine rmsfield_replot(self)
  class(rmsfield_settings) :: self

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=5) :: ftext
       PRINT *, "About to call plot_rmsfield for replot"
       call plot_rmsfield(self%canvas)
!
end subroutine


subroutine rmsfield_new(self)
  use zoa_tab
  use gtk_draw_hl
  use g
  use GLOBALS
  implicit none

  type(c_ptr) :: parent_window, localcanvas, isurface
  class(rmsfieldtab) :: self
  integer :: usePLPLOT = 1

  PRINT *, "rmsfield new plot initiated!"


  !ast_cairo_drawing_area = astfcdist_tab%canvas
  if (usePLPLOT == 1) THEN
    PRINT *, "Plotting rmsfield via PL PLOT!"
    self%canvas = hl_gtk_drawing_area_new(size=[700,500], &
          & has_alpha=FALSE)

          rmsfield_struct_settings = rmsfield_settings(self%canvas)

          call plot_rmsfield(self%canvas)
          call self%finalizeWindow
  else

    !call gtk_drawing_area_set_draw_func(astfcdist_tab%canvas, &
    !                & c_funloc(ROUTEDRAWING), c_loc(TARGET_PLOTTYPE_AST), c_null_funptr)
  end if


end subroutine


subroutine callback_rmsfield_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)


end subroutine

 subroutine plot_rmsfield(localcanvas)

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

     REAL, dimension(10) :: x, y

    call logger%logText('plot_rmsfield routine started')
    isurface = g_object_get_data(localcanvas, "backing-surface")
    PRINT *, "isurface is ", isurface
    if (.not. c_associated(isurface)) then
       PRINT *, "error:  new plot :: Backing surface is NULL"
       return
    end if

    call mplt%initialize(localcanvas, 1,1)

    x = [1,2,3,4,5,6,7,8,9,10]
    y = [1,2,3,4,5,6,7,8,9,10]
    !call filterRawSpotData(x,y)

    call xyscat1%initialize(c_null_ptr, x, y, &
    & xlabel=' (in)'//c_null_char, ylabel='(in)'//c_null_char, &
    & title='rmsfield Diagram'//c_null_char)
    call xyscat1%setLineStyleCode(-1)
    !&  REAL(pack((DSPOTT(2,:)-SUM(DSPOTT(2,:)/SIZE(DSPOTT(2,:)))), &

    call mplt%set(1,1,xyscat1)
    !call xyscat1%drawPlot
    call mplt%draw()

end subroutine

end module
