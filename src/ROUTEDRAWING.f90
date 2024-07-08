
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
    use global_widgets
    !use handlers
    use mod_plotopticalsystem
    use mod_plotrayfan
    use ui_ast_fc_dist
    use ui_spot
    use kdp_draw, only: DRAWOPTICALSYSTEM

    IMPLICIT NONE

    type(c_ptr), value    :: cairo_drawing_area, my_cairo_context
    type(c_ptr), value :: gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer(kind=c_int), pointer :: ID_SETTING

    !PRINT *, "gdata before c_f_pointer call in ROUTEDRAWING ", gdata
    call c_f_pointer(gdata, ID_SETTING)


    !PRINT *, "ID_SETTING IN ROUTEDRAWING IS ", ID_SETTING
    !PRINT *, "ID_NEWPLOT_LENSDRAW IS ", ID_NEWPLOT_LENSDRAW

    ! I need to trigger replot for plotCmd plots if I want them to properly refrecsh
    ! eg
    ! get current tab
    ! if cmdBased, then run plot command
    ! may end up triggering it twice after a setting change, but at least I would solve the
    ! problem of lens draw not properly updating


    

    select case (ID_SETTING)
    case (ID_NEWPLOT_LENSDRAW)
       !call LogTermFOR("REROUTE TO LENS DRAW REPLOT ")           
       call ld_settings%replot()
  case (ID_NEWPLOT_RAYFAN)
      !PRINT *, "REROUTE TO RAY FAN REPLOT!"
      !call zoatabMgr%newPlotIfNeeded(ID_NEWPLOT_RAYFAN)
      call rf_settings%replot()
    case (ID_PLOTTYPE_AST)
        !PRINT *, "REROUTE TO Astig Replot!"
        call ast_settings%replot()
    case (ID_PLOTTYPE_SPOT)
        call spot_struct_settings%replot()


  case default
      !PRINT *, "NO ID SETTING MATCH FOUND! ID_SETTING PASSED IS ", ID_SETTING
      !PRINT *, "gdata is ", LOC(gdata)
      !Non special calls
      
      call PROCESKDP(getTabPlotCommand(ID_SETTING))
  
  end select
      call LogTermFOR("About to call Draw Optical System from Route Drawing")
      call DRAWOPTICALSYSTEM(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata)



end SUBROUTINE

end module

