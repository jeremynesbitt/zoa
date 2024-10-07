
!TODO:  FInd a better home for this as it doesn't really deserive it's own file name.
module ROUTEMOD
  use iso_c_binding


  ! Need to ask zoaTabMgr what the plot command is for the tab index but to 
  ! avoid circular dependency need this interface
  interface
  function getTabPlotCommand(objIdx)
    character(len=1040) :: getTabPlotCommand
    integer :: objIdx
  end function
end interface


contains

SUBROUTINE ROUTEDRAWING(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata) bind(c)

    use zoa_ui
    !use handlers
    use type_utils
    use kdp_draw, only: DRAWOPTICALSYSTEM

    IMPLICIT NONE

    type(c_ptr), value    :: cairo_drawing_area, my_cairo_context
    type(c_ptr), value :: gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer(kind=c_int), pointer :: tabIdx

    !PRINT *, "gdata before c_f_pointer call in ROUTEDRAWING ", gdata
    call c_f_pointer(gdata, tabIdx)

    call PROCESKDP(getTabPlotCommand(tabIdx))
    call LogTermDebug("About to call DRAWOPTICALSYSTEM with cairo ptr "//int2str(INT(LOC(cairo_drawing_area),4)))
    call DRAWOPTICALSYSTEM(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata)


end SUBROUTINE

end module

