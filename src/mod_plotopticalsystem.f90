

module mod_plotopticalsystem


  use GLOBALS
  use global_widgets
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  use gtk_hl_tree
  use gtk
  use kdp_interfaces

  ! use gtk, only: gtk_button_new, gtk_window_set_child, gtk_window_destroy, &
  !      & gtk_progress_bar_new, gtk_widget_show, gtk_window_new, &
  !      & gtk_init, gtk_drawing_area_new, gtk_drawing_area_set_content_width, &
  !      & gtk_drawing_area_set_content_height, gtk_drawing_area_set_draw_func, &
  !      & gtk_window_set_mnemonics_visible, gtk_widget_queue_draw, &
  !      & gtk_expander_new_with_mnemonic, gtk_expander_set_child, &
  !      & gtk_expander_set_expanded, gtk_combo_box_text_new, gtk_combo_box_text_get_active_text, &
  !      & gtk_combo_box_text_append_text, gtk_combo_box_set_active, gtk_list_store_newv, &
  !      & gtk_list_store_append, gtk_list_store_get_type, gtk_list_store_set_value, &
  !      & gtk_combo_box_new_with_model_and_entry
  use gtk_hl_chooser
!  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending, &
!      & g_value_init
  use g

  implicit none
  type(c_ptr) :: win,bar,pbar,qbut, box
  integer(kind=c_int) :: run_status = TRUE
  integer(c_int) :: boolresult

  type(c_ptr) ::  combo_plotorientation
  type(c_ptr) ::  spinButton_azimuth, spinButton_elevation


  real :: elevation_default = 26.2
  real :: azimuth_default = 232.2

contains



  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    run_status = FALSE

    call gtk_window_destroy(ld_window)
    ld_window = c_null_ptr
  end subroutine my_destroy



subroutine spin_firstSurface_callback (widget, gdata ) bind(c)
   use iso_c_binding
   type(c_ptr), value, intent(in) :: widget, gdata
   integer surfaceIndex

   surfaceIndex = INT(gtk_spin_button_get_value (widget))

   call ld_settings % set_start_surface(surfaceIndex)

   PRINT *, "NEW FIRST SURFACE ", surfaceIndex

   if (ld_settings%changed.eq.1) THEN
      ld_settings%changed = 0
      call ld_settings%lens_draw_replot()
      !call WDRAWOPTICALSYSTEM()

   end if

end subroutine spin_firstSurface_callback

subroutine spin_endSurface_callback (widget, gdata ) bind(c)
   use iso_c_binding
   type(c_ptr), value, intent(in) :: widget, gdata
   integer surfaceIndex

   surfaceIndex = INT(gtk_spin_button_get_value (widget))

   call ld_settings % set_end_surface(surfaceIndex)

   PRINT *, "NEW LAST SURFACE SPIN CALLBACK ", surfaceIndex

   if (ld_settings%changed.eq.1) THEN
      ld_settings%changed = 0
      call ld_settings%lens_draw_replot()
      !call WDRAWOPTICALSYSTEM

   end if

end subroutine spin_endSurface_callback

subroutine combo_tmp_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy


  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  character(len=50)                        :: choice
  character(len=512)             :: my_string
  type(gtktreeiter), target :: tree_iter

  type(c_ptr)  :: model

  type(c_ptr)  :: val, cstr, ival

  type(gvalue), target :: result, iresult


  type(integer)  :: tmpresult, ivalue

  ! For HL function
  !character(len=*) :: hl_string


  PRINT *, "In callback, pointer is ", gdata
  !tree_iter = c_null_ptr
  tmpresult = gtk_combo_box_get_active_iter(widget, c_loc(tree_iter))
  !tree_iter = gtk_combo_box_get_active_iter(gdata)

  !PRINT *, "tree_iter is ", tree_iter

  model = gtk_combo_box_get_model(widget)
  val = c_loc(result)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 1_c_int, val)

  !cstr = g_value_get_string(result)

  cstr = g_value_get_string(val)
  call convert_c_string(cstr, choice)

!        cstr = g_value_get_string(val)
!          call convert_c_string(cstr, svalue)
  PRINT *, "CHOICE is ", choice

  ! Get ING
  ival = c_loc(iresult)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 0_c_int, ival)
  ivalue = g_value_get_int(ival)

  PRINT *, "Integer Index is ", ivalue

  ! Use HL function
  !call hl_gtk_list_tree_get_gvalue(val, G_TYPE_STRING, svalue=hl_string)



  !if tree_iter is not None:
  !    model = combo.get_model()
!      row_id, name = model[tree_iter][:2]
  !     print("Selected: ID=%d, name=%s" % (row_id, name))
  ! else:
  !     entry = combo.get_child()
  !     print("Entered: %s" % entry.get_text())

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(gdata), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string
  PRINT *, "Callback works!"
end subroutine combo_tmp_callback



subroutine spin_autoScale_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use hl_gtk_zoa



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  integer :: id_selected
  !character(len=50)                        :: choice
  !character(len=512)             :: my_string
  real :: scaleFactor

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(combo_plotorientation), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string
   scaleFactor = real(gtk_spin_button_get_value (widget))

   PRINT *, "Scale Factor is, ", scaleFactor

  call ld_settings % set_scaleFactor(scaleFactor)

   if (ld_settings%changed.eq.1) THEN
      ld_settings%changed = 0
      call ld_settings%lens_draw_replot()
      !call WDRAWOPTICALSYSTEM

   end if


end subroutine spin_autoScale_callback

subroutine spin_elevation_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use hl_gtk_zoa

  type(c_ptr), value, intent(in) :: widget, gdata
  real :: elevation

  elevation = real(gtk_spin_button_get_value (widget))

  call ld_settings % set_elevation(elevation)
  PRINT *, "ELEVATION IS ", ld_settings % elevation

   if (ld_settings%changed.eq.1) THEN
      ld_settings%changed = 0
      call ld_settings%lens_draw_replot()
      !call WDRAWOPTICALSYSTEM
   end if


end subroutine spin_elevation_callback

subroutine spin_azimuth_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use hl_gtk_zoa

  type(c_ptr), value, intent(in) :: widget, gdata
  real :: azimuth

  azimuth = real(gtk_spin_button_get_value (widget))

  call ld_settings % set_azimuth(azimuth)

  PRINT *, "AZIMUTH IS ", ld_settings%azimuth

   if (ld_settings%changed.eq.1) THEN
      ld_settings%changed = 0
      call ld_settings%lens_draw_replot()
      !call WDRAWOPTICALSYSTEM
   end if


end subroutine spin_azimuth_callback

subroutine combo_autoScale_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use hl_gtk_zoa



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  integer :: id_selected
  !character(len=50)                        :: choice
  !character(len=512)             :: my_string

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(combo_plotorientation), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string

  id_selected = hl_zoa_combo_get_selected_list2_id(widget)

  PRINT *, "Autoscale idx is ", id_selected

  if (id_selected.eq.ID_LENSDRAW_MANUALSCALE) THEN
     call gtk_widget_set_sensitive(gdata, TRUE)
   else
     call gtk_widget_set_sensitive(gdata, FALSE)
  end if

  call ld_settings % set_autoScale(id_selected)

  if (ld_settings%changed.eq.1) THEN
     ld_settings%changed = 0
     call ld_settings%lens_draw_replot()
     !call WDRAWOPTICALSYSTEM
  end if


end subroutine combo_autoScale_callback


subroutine combo_fieldsymmetry_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use hl_gtk_zoa



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  integer :: id_selected
  !character(len=50)                        :: choice
  !character(len=512)             :: my_string

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(combo_plotorientation), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string
  PRINT *, "Field Symmetry Callback initiated!"
  id_selected = hl_zoa_combo_get_selected_list2_id(widget)


  call ld_settings % set_field_symmetry(id_selected)

  if (ld_settings%changed.eq.1) THEN
     ld_settings%changed = 0
     call ld_settings%lens_draw_replot()
     !call WDRAWOPTICALSYSTEM
  end if


end subroutine combo_fieldsymmetry_callback


subroutine combo_plotorientation_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use hl_gtk_zoa



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  integer :: id_selected
  !character(len=50)                        :: choice
  !character(len=512)             :: my_string

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(combo_plotorientation), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string

  id_selected = hl_zoa_combo_get_selected_list2_id(widget)

  if (id_selected.eq.ID_LENSDRAW_ORTHO_PLOT_ORIENTATION) THEN
    call gtk_widget_set_sensitive(spinButton_azimuth, TRUE)
    call gtk_widget_set_sensitive(spinButton_elevation, TRUE)
  else
    call gtk_widget_set_sensitive(spinButton_azimuth, FALSE)
    call gtk_widget_set_sensitive(spinButton_elevation, FALSE)
  end if

  call ld_settings % set_plot_orientation(id_selected)

  if (ld_settings%changed.eq.1) THEN
     ld_settings%changed = 0
     call ld_settings%lens_draw_replot()
      !call WDRAWOPTICALSYSTEM
  end if


end subroutine combo_plotorientation_callback

  subroutine lens_draw_settings_dialog(box1)

    use hl_gtk_zoa

    type(c_ptr), intent(inout) :: box1

    type(c_ptr)     :: table, expander

    type(c_ptr)  :: label_plotorientation, label_fieldsymmetry

    type(c_ptr)  :: lstmp, gtype, cbox, cbox_field, cbox_scale

    integer(kind=c_int) :: lastSurface

    integer(kind=c_int), parameter :: ncols=2
    integer(kind=type_kind), dimension(ncols), target :: coltypes = &
         & [G_TYPE_INT, G_TYPE_STRING]

    type(gtktreeiter), target :: iter
    type(gvalue), target :: valt, vali
    type(c_ptr) :: val

    type(c_ptr) :: list, combo_tmp
    !type(gvalue), target :: modelv, columnv
    !type(c_ptr) :: pmodel, pcolumn, model
    !integer(kind=c_int) :: icol
    !character(len=20), dimension(4) :: vals_plotorientation

    type(c_ptr) :: label_first, label_last, spinButton_firstSurface, spinButton_lastSurface

    type(c_ptr) :: label_azimuth, label_elevation

    type(c_ptr) :: spinButton_scaleFactor

    character(kind=c_char, len=20), dimension(4) :: vals_plotorientation
    integer(c_int), dimension(4) :: refs_plotorientation

    character(kind=c_char, len=40), dimension(2) :: vals_fieldsymmetry
    integer(c_int), dimension(2) :: refs_fieldsymmetry

    character(kind=c_char, len=40), dimension(2) :: vals_scaleFactor
    integer(c_int), dimension(2) :: refs_scaleFactor



    vals_plotorientation = [character(len=20) :: "YZ - Plane Layout", "XZ - Plane Layout", &
         &"XY - Plane Layout", "Orthographic"]

    refs_plotorientation = [ID_LENSDRAW_YZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XY_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_ORTHO_PLOT_ORIENTATION]



    call hl_gtk_combo_box_list2_new(cbox, refs_plotorientation, vals_plotorientation, 1700_c_int)
    call g_signal_connect (cbox, "changed"//c_null_char, c_funloc(combo_plotorientation_callback), c_null_ptr)


    vals_fieldsymmetry =  [character(len=40) :: "Plot Upper and Lower Fields of View", &
    & "Plot Upper Fields Only"]

    refs_fieldsymmetry = [ID_LENSDRAW_PLOT_WHOLE_FIELD, ID_LENSDRAW_PLOT_HALF_FIELD]

    call hl_gtk_combo_box_list2_new(cbox_field, refs_fieldsymmetry, vals_fieldsymmetry, 1700_c_int)
    call g_signal_connect (cbox_field, "changed"//c_null_char, c_funloc(combo_fieldsymmetry_callback), c_null_ptr)

    vals_scaleFactor = [character(len=40) :: "Autoscale (Default)", "Manual Scale"]
    refs_scaleFactor = [ID_LENSDRAW_AUTOSCALE, ID_LENSDRAW_MANUALSCALE]


    spinButton_scaleFactor = gtk_spin_button_new (gtk_adjustment_new(.045*1d0,0d0,1000*1d0,1d0,1d0,0d0),2d0, 3_c_int)
    call gtk_widget_set_sensitive(spinButton_scaleFactor, FALSE)
    call g_signal_connect (spinButton_scaleFactor, "changed"//c_null_char, c_funloc(spin_autoScale_callback), c_null_ptr)

    call hl_gtk_combo_box_list2_new(cbox_scale, refs_scaleFactor, vals_scaleFactor, 1700_c_int)
    call g_signal_connect (cbox_scale, "changed"//c_null_char, c_funloc(combo_autoScale_callback), spinButton_scaleFactor)

   !gdouble value,
   !gdouble lower,
   !gdouble upper,
   !gdouble step_increment,
   !gdouble page_increment,
   !gdouble page_size);


    !Spin buttons for selecting which surfaces to plot
    call getOpticalSystemLastSurface(lastSurface)
    label_first = gtk_label_new("First Surface"//c_null_char)
    spinButton_firstSurface = gtk_spin_button_new (gtk_adjustment_new(1d0,1d0,(lastSurface-1)*1d0,1d0,1d0,0d0),2d0, 0_c_int)

    label_last = gtk_label_new("Last Surface"//c_null_char)
    spinButton_lastSurface = gtk_spin_button_new (gtk_adjustment_new(lastSurface*1d0,2d0,lastSurface*1d0, &
    & 1d0,1d0,0d0),2d0, 0_c_int)

    call g_signal_connect (spinButton_firstSurface, "value_changed"//c_null_char, c_funloc(spin_firstSurface_callback), c_null_ptr)
    call g_signal_connect (spinButton_lastSurface, "value_changed"//c_null_char, c_funloc(spin_endSurface_callback), c_null_ptr)

    !call g_signal_connect (spinButton_lastSurface, "value_changed"//c_null_char, c_funloc(spin_firstSurface_callback), c_null_ptr)



    ! Spin Buttons for 3D Layout
    label_elevation = gtk_label_new("Elevation Angle [deg]"//c_null_char)
    spinButton_elevation = gtk_spin_button_new (gtk_adjustment_new(26.2d0,0d0,180*1d0,10d0,1d0,0d0),2d0, 1_c_int)

    label_azimuth = gtk_label_new("Azimuth Angle [deg]"//c_null_char)
    spinButton_azimuth = gtk_spin_button_new (gtk_adjustment_new(232.2d0,0d0,360*1d0, &
    & 10d0,1d0,0d0),2d0, 1_c_int)

    !Default disable these
    call gtk_widget_set_sensitive(spinButton_elevation, FALSE)
    call gtk_widget_set_sensitive(spinButton_azimuth, FALSE)

    call g_signal_connect (spinButton_elevation, "value_changed"//c_null_char, c_funloc(spin_elevation_callback), c_null_ptr)
    call g_signal_connect (spinButton_azimuth, "value_changed"//c_null_char, c_funloc(spin_azimuth_callback), c_null_ptr)


    ! The combo box with predifined values of interesting Julia sets:
    label_plotorientation = gtk_label_new("Plot Orientation:"//c_null_char)
    label_fieldsymmetry   = gtk_label_new("Field Symmetry:"//c_null_char)


    !call g_signal_connect (combo_plotorientation, "changed"//c_null_char, c_funloc(combo_plotorientation_callback))

    ! A table container will contain buttons and labels:
    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)
    ! call gtk_grid_attach(table, button1, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, button2, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, button3, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
    !  call gtk_grid_attach(table, label1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, label2, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, label3, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, spinButton1, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, spinButton2, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, spinButton3, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, linkButton, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label_plotorientation, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, cbox, 0_c_int, 1_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_fieldsymmetry, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, cbox_field, 1_c_int, 1_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_first, 2_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label_last, 2_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_firstSurface, 3_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_lastSurface, 3_c_int, 2_c_int, 1_c_int, 1_c_int)

    call gtk_grid_attach(table, label_elevation, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label_azimuth, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_elevation, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_azimuth, 1_c_int, 3_c_int, 1_c_int, 1_c_int)

    call gtk_grid_attach(table, cbox_scale, 2_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_scaleFactor, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_azimuth, 1_c_int, 3_c_int, 1_c_int, 1_c_int)



    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, TRUE)

    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
    call gtk_box_append(box1, expander)

    !call gtk_box_append(box1, ld_window)
    call gtk_widget_set_vexpand (box1, FALSE)

    ! Try to fix a bug
    call ld_settings%set_elevation(elevation_default)
    call ld_settings%set_azimuth(azimuth_default)



  end subroutine lens_draw_settings_dialog


  subroutine lens_draw_new(parent_window)

    use kdp_draw, only: DRAWOPTICALSYSTEM, TESTCAIRO2, TESTCAIRO3
    USE ROUTEMOD

    type(c_ptr) :: parent_window

    type(c_ptr) :: content, junk, gfilter, tab_label
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1, location
    !type(c_ptr)     :: my_drawing_area
    !integer(c_int)  :: width, height

    type(c_ptr)     :: table, expander, box1, scrolled_tab

    integer, target :: TARGET_LENSDRAW_PLOT   = ID_NEWPLOT_LENSDRAW

    ! Create a modal dialogue
    !ld_window = gtk_window_new()
    !call gtk_window_set_modal(di, TRUE)
    !title = "Lens Draw Window"
    !if (present(title)) call gtk_window_set_title(dialog, title)
    !call gtk_window_set_title(ld_window, "Lens Draw Window"//c_null_char)
    !if (present(wsize)) then
    !   call gtk_window_set_default_size(dialog, wsize(1),&
    !        & wsize(2))
    !else

    width = 1000
    height = 700
    !     call gtk_window_set_default_size(ld_window, width, height)
    !end if

    !if (present(parent)) then
    !   call gtk_window_set_transient_for(ld_window, parent_window)
    !   call gtk_window_set_destroy_with_parent(ld_window, TRUE)
    !end if
    call getOpticalSystemLastSurface(ld_settings%end_surface)

    call lens_draw_settings_dialog(ld_window)

    ld_cairo_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(ld_cairo_drawing_area, width)
    call gtk_drawing_area_set_content_height(ld_cairo_drawing_area, height)

    !call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
    !               & c_funloc(DRAWOPTICALSYSTEM), c_null_ptr, c_null_funptr)
    !call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
    !               & c_funloc(DRAWOPTICALSYSTEM), c_null_ptr, c_null_funptr)
    call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
                   & c_funloc(ROUTEDRAWING), c_loc(TARGET_LENSDRAW_PLOT), c_null_funptr)
    !call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
    !               & c_funloc(DRAWOPTICALSYSTEM), c_loc(TARGET_LENSDRAW_PLOT), c_null_funptr)
    !call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
    !               & c_funloc(lens_draw_replot), c_null_ptr, c_null_funptr)
    !call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
    !               & c_funloc(TESTCAIRO3), c_null_ptr, c_null_funptr)

    !PRINT *, "FINISHED WITH DRAWOPTICALSYSTEM"
    !PRINT *, "LENS DATA SURFACES IS ", curr_lens_data % num_surfaces
    !IF (curr_lens_data%num_surfaces.GT.0) THEN
    !  PRINT *, "RADII ARE ", curr_lens_data%radii
    !  PRINT *, "THICKNESSES ARE ", curr_lens_data%thicknesses
    !END IF

    call gtk_box_append(ld_window, ld_cairo_drawing_area)
    !call gtk_window_set_child(ld_window, ld_cairo_drawing_area)
    !call gtk_window_set_child(ld_window, box1)
    call gtk_widget_set_vexpand (ld_window, FALSE)

    !call g_signal_connect(ld_window, "destroy"//c_null_char, c_funloc(my_destroy), c_null_ptr)
    tab_label = gtk_label_new_with_mnemonic("_Lens Draw"//c_null_char)
    scrolled_tab = gtk_scrolled_window_new()
    call gtk_scrolled_window_set_child(scrolled_tab, ld_window)
    location = gtk_notebook_append_page(notebook, scrolled_tab, tab_label)
    call gtk_notebook_set_current_page(notebook, location)

    !call gtk_window_set_mnemonics_visible (ld_window, TRUE)
    !call gtk_widget_queue_draw(my_drawing_area)
    !call gtk_widget_show(ld_window)


    PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine lens_draw_new

  subroutine lens_draw_old(parent_window)

    use kdp_draw, only: DRAWOPTICALSYSTEM, TESTCAIRO2, TESTCAIRO3
    type(c_ptr) :: parent_window

    type(c_ptr) :: content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1
    !type(c_ptr)     :: my_drawing_area
    !integer(c_int)  :: width, height

    type(c_ptr)     :: table, expander, box1

    ! Create a modal dialogue
    ld_window = gtk_window_new()
    !call gtk_window_set_modal(di, TRUE)
    !title = "Lens Draw Window"
    !if (present(title)) call gtk_window_set_title(dialog, title)
    call gtk_window_set_title(ld_window, "Lens Draw Window"//c_null_char)
    !if (present(wsize)) then
    !   call gtk_window_set_default_size(dialog, wsize(1),&
    !        & wsize(2))
    !else

    width = 1000
    height = 700
       call gtk_window_set_default_size(ld_window, width, height)
    !end if

    !if (present(parent)) then
       call gtk_window_set_transient_for(ld_window, parent_window)
       call gtk_window_set_destroy_with_parent(ld_window, TRUE)
    !end if


    call lens_draw_settings_dialog(box1)

    ld_cairo_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(ld_cairo_drawing_area, width)
    call gtk_drawing_area_set_content_height(ld_cairo_drawing_area, height)

    call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
                   & c_funloc(DRAWOPTICALSYSTEM), c_null_ptr, c_null_funptr)
    !call gtk_drawing_area_set_draw_func(ld_cairo_drawing_area, &
    !               & c_funloc(TESTCAIRO3), c_null_ptr, c_null_funptr)

    PRINT *, "FINISHED WITH DRAWOPTICALSYSTEM"
    PRINT *, "LENS DATA SURFACES IS ", curr_lens_data % num_surfaces
    IF (curr_lens_data%num_surfaces.GT.0) THEN
      PRINT *, "RADII ARE ", curr_lens_data%radii
      PRINT *, "THICKNESSES ARE ", curr_lens_data%thicknesses
    END IF

    call gtk_box_append(box1, ld_cairo_drawing_area)
    !call gtk_window_set_child(ld_window, ld_cairo_drawing_area)
    call gtk_window_set_child(ld_window, box1)
    call gtk_widget_set_vexpand (box1, FALSE)

    call g_signal_connect(ld_window, "destroy"//c_null_char, c_funloc(my_destroy), c_null_ptr)


    call gtk_window_set_mnemonics_visible (ld_window, TRUE)
    !call gtk_widget_queue_draw(my_drawing_area)
    call gtk_widget_show(ld_window)


    PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine



end module
