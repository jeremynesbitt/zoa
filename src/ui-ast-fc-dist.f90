module ui_ast_fc_dist
  use gtk
  use global_widgets
contains

subroutine ast_fc_dist_new(astfcdist_tab)
  use zoa_tab
  use ROUTEMOD
  use kdp_draw, only: plot_ast_fc_dist
  use gtk_draw_hl
  use g
  !use handlers, only: plot_04debug
  implicit none

  type(c_ptr) :: parent_window, localcanvas, isurface
  type(zoatab) :: astfcdist_tab

  type(c_ptr) :: spinButton_numRays, spinButton_wavelength
  integer :: usePLPLOT = 1
  ! Added these target parameters to have only one callback function and satisfy
  ! requirement to have a target attribute for a pointer for gtk.  I could not
  ! find a more elegant solution, and this seems better than a bunch of small
  ! callback functions
  !integer, target :: TARGET_RAYFAN_WAVELENGTH  = ID_WAVELENGTH
  integer, target :: TARGET_AST_FIELDXY = ID_AST_FIELDXY
  integer, target :: TARGET_AST_NUMRAYS  = ID_RAYFAN_NUMRAYS
  integer, target :: TARGET_PLOTTYPE_AST  = ID_PLOTTYPE_AST

  character(kind=c_char, len=20), dimension(2) :: vals_ast_fieldxy
  integer(c_int), dimension(2) :: refs_ast_fieldxy


  vals_ast_fieldxy = [character(len=20) :: "Y Field", "X Field"]

  refs_ast_fieldxy = [ID_AST_FIELD_Y, ID_AST_FIELD_X]


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
    PRINT *, "Plotting Astig via PL PLOT!"
    astfcdist_tab%canvas = hl_gtk_drawing_area_new(size=[1200,500], &
          & has_alpha=FALSE)
    !ast_cairo_drawing_area = astfcdist_tab%canvas
          !& has_alpha=FALSE, expose_event=c_funloc(plot_ast_fc_dist), &
          !& data_expose=c_loc(TARGET_PLOTTYPE_AST))
          !ast_cairo_drawing_area = astfcdist_tab%canvas
    !call plot_04debug(astfcdist_tab%canvas)
          !call debugPLPLOT(astfcdist_tab%canvas)
          call plot_ast_fc_dist(astfcdist_tab%canvas)

    ! isurface = g_object_get_data(astfcdist_tab%canvas, "backing-surface")
    ! PRINT *, "isurface is ", isurface
    ! if (.not. c_associated(isurface)) then
    !    PRINT *, "new astig plot :: Backing surface is NULL"
    !    return
    ! end if
    !   PRINT *, "ASTIG CANVAS IS ", astfcdist_tab%canvas
     !call gtk_drawing_area_set_draw_func(astfcdist_tab%canvas, &
      !               & c_funloc(plot_ast_fc_dist), c_loc(TARGET_PLOTTYPE_AST), c_null_funptr)

!
  ! astfcdist_tab%canvas = localcanvas
  else

    !call gtk_drawing_area_set_draw_func(astfcdist_tab%canvas, &
    !                & c_funloc(ROUTEDRAWING), c_loc(TARGET_PLOTTYPE_AST), c_null_funptr)
  end if

  call astfcdist_tab%addListBoxSetting("Field Type", refs_ast_fieldxy, vals_ast_fieldxy, &
  & c_funloc(callback_ast_fc_dist_settings), c_loc(TARGET_AST_FIELDXY))


  spinButton_numRays = gtk_spin_button_new (gtk_adjustment_new(value=10d0, &
                                                              & lower=10d0, &
                                                              & upper=50d0, &
                                                              & step_increment=1d0, &
                                                              & page_increment=1d0, &
                                                              & page_size=0d0),climb_rate=1d0, &
                                                              & digits=0_c_int)



 call astfcdist_tab%addSpinBoxSetting("Number of Rays", spinButton_numRays, &
 & c_funloc(callback_ast_fc_dist_settings), c_loc(TARGET_AST_NUMRAYS))


  call astfcdist_tab%finalizeWindow()
  !ast_window = astfcdist_tab%box1
  !PRINT *, "AT END OF new astig plot, canvas ptr is ", astfcdist_tab%canvas



end subroutine


subroutine callback_ast_fc_dist_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  !PRINT *, "Integer Passed is ", ID_SETTING

  !PRINT *, "Pointer Passed is ", gdata

  select case (ID_SETTING)

  case (ID_AST_FIELDXY)
    call ast_settings % set_ast_field_dir(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_RAYFAN_NUMRAYS)
    PRINT *, "NUM RAYS IN CALLBACK!"

    call ast_settings % set_ast_num_rays(INT(gtk_spin_button_get_value (widget)))


  end select

  ! Currently autoreplotting will happen in the settings module

end subroutine


end module
