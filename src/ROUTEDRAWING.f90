
module ROUTEMOD
contains
SUBROUTINE ROUTEDRAWING(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata) bind(c)
    use iso_c_binding
    use zoa_ui
    use global_widgets
    !use mod_plotopticalsystem, only: lens_draw_replot
    !use mod_plotrayfan, only: ray_fan_replot
    use kdp_draw, only: DRAWOPTICALSYSTEM

    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: cairo_drawing_area, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer(kind=c_int), pointer :: ID_SETTING

    call c_f_pointer(gdata, ID_SETTING)

    !PRINT *, "ID_SETTING IN ROUTEDRAWING IS ", ID_SETTING
    !PRINT *, "ID_NEWPLOT_LENSDRAW IS ", ID_NEWPLOT_LENSDRAW

    select case (ID_SETTING)
    case (ID_NEWPLOT_LENSDRAW)
       PRINT *, "REROUTE TO LENS DRAW REPLOT "
       call ld_settings%lens_draw_replot()
  case (ID_NEWPLOT_RAYFAN)
      PRINT *, "REROUTE TO RAY FAN REPLOT!"
      !call rf_settings%ray_fan_replot()
  end select
      call DRAWOPTICALSYSTEM(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata)



end SUBROUTINE
end module
