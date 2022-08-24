
module ROUTEMOD
  use iso_c_binding
contains

 subroutine createCairoDrawingAreaForDraw(canvas, width, height, ID_PLOTTYPE)
     use gtk, only: gtk_drawing_area_new, gtk_drawing_area_set_content_width, &
     & gtk_drawing_area_set_content_height, gtk_drawing_area_set_draw_func
     use gtk_draw_hl
     !use kdp_interfaces, only : ROUTEDRAWING
     !use kdp_draw, only: DRAWOPTICALSYSTEM

     type(c_ptr), intent(inout) :: canvas
     integer(kind=c_int), intent(in)  :: width, height, ID_PLOTTYPE

     integer(kind=c_int), target :: TARGET_NEWPLOT
     if (ID_PLOTTYPE > 0) THEN
        canvas = gtk_drawing_area_new()
        !  canvas = hl_gtk_drawing_area_new(size=[width, height])
        !TARGET_NEWPLOT = ID_PLOTTYPE

        call gtk_drawing_area_set_content_width(canvas, width)
        call gtk_drawing_area_set_content_height(canvas, height)
      end if




 end subroutine

SUBROUTINE ROUTEDRAWING(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata) bind(c)

    use zoa_ui
    use global_widgets
    !use mod_plotopticalsystem, only: lens_draw_replot
    !use mod_plotrayfan, only: ray_fan_replot
    use kdp_draw, only: DRAWOPTICALSYSTEM

    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: cairo_drawing_area, my_cairo_context
    type(c_ptr), value :: gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer(kind=c_int), pointer :: ID_SETTING

    PRINT *, "gdata before c_f_pointer call in ROUTEDRAWING ", gdata
    call c_f_pointer(gdata, ID_SETTING)


    !PRINT *, "ID_SETTING IN ROUTEDRAWING IS ", ID_SETTING
    !PRINT *, "ID_NEWPLOT_LENSDRAW IS ", ID_NEWPLOT_LENSDRAW

    select case (ID_SETTING)
    case (ID_NEWPLOT_LENSDRAW)
       PRINT *, "REROUTE TO LENS DRAW REPLOT "
       call ld_settings%lens_draw_replot()
  case (ID_NEWPLOT_RAYFAN)
      PRINT *, "REROUTE TO RAY FAN REPLOT!"
      call rf_settings%replot()
    case (ID_PLOTTYPE_AST)
      PRINT *, "REROUTE TO Astig Replot!"
        call ast_settings%replot()
        !call plot_ast_fc_dist(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata)
        RETURN

  case default
      PRINT *, "NO ID SETTING MATCH FOUND! ID_SETTING PASSED IS ", ID_SETTING
      PRINT *, "gdata is ", gdata
  end select
      call DRAWOPTICALSYSTEM(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata)



end SUBROUTINE


subroutine debugPLPLOT(canvas)
     use handlers, only: plot_04
     implicit none
     type(c_ptr) :: canvas

     call plot_04(canvas)

end subroutine
end module

! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran GTK+ Fortran Interface library.
!
! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files COPYING3 and COPYING.RUNTIME respectively.
! If not, see <http://www.gnu.org/licenses/>.
! -----------------------------------------------------------------------------
! Contributed by James Tappin
! Some code derived from a demo program by "tadeboro" posted on the gtk forums.
! Last modifications: 2013-01-31, vmagnin 2020-06-17 (GTK 4), 2020-08-25
! -----------------------------------------------------------------------------
