module ui_sys_config
  use GLOBALS
  use global_widgets
  use handlers
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  !use gtk_hl_tree
  use hl_zoa_tree_tmp
  use gtk

  use gtk_hl_chooser

  use g
  use zoa_ui
  !use mod_lens_editor_settings

  implicit none
contains

  subroutine sys_config_destroy(widget, gdata) bind(c)

    type(c_ptr), value :: widget, gdata
    type(c_ptr) :: isurface
    print *, "Exit called"
    call gtk_window_destroy(gdata)
    sys_config_window = c_null_ptr

  end subroutine

subroutine sys_config_new(parent_window)

  type(c_ptr) :: parent_window
  !type(c_ptr), value :: sys_config_window

  type(c_ptr) :: content, junk, gfilter
  integer(kind=c_int) :: icreate, idir, action, lval
  integer(kind=c_int) :: i, idx0, idx1, pageIdx
  !integer(c_int)  :: width, height

  type(c_ptr)  :: table, expander, box1, nbk, basicLabel, boxAperture, boxAsphere
  type(c_ptr)  :: lblAperture, AsphLabel

  PRINT *, "ABOUT TO FIRE UP SYS CONFIG WINDOW!"

  ! Create a modal dialogue
  sys_config_window = gtk_window_new()

  call gtk_window_set_title(sys_config_window, "Optical System Configuration"//c_null_char)

  width = 700
  height = 400
     call gtk_window_set_default_size(sys_config_window, width, height)

     call gtk_window_set_transient_for(sys_config_window, parent_window)
     call gtk_window_set_destroy_with_parent(sys_config_window, TRUE)

  !call lens_editor_basic_dialog(box1)

  !call lens_editor_asphere_dialog(boxAsphere)
  !call lens_editor_asphere_dialog(boxAsphere)


  !call lens_editor_aperture(boxAperture)

  !call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
  !location = gtk_notebook_append_page(self%notebook, scrolled_tab, self%tab_label)
  nbk = gtk_notebook_new()
  basicLabel = gtk_label_new_with_mnemonic("_Aperture"//c_null_char)
  pageIdx = gtk_notebook_append_page(nbk, box1, basicLabel)

  AsphLabel = gtk_label_new_with_mnemonic("_Fields"//c_null_char)
  pageIdx = gtk_notebook_append_page(nbk, boxAsphere, AsphLabel)


  PRINT *, "FINISHED Setting up system config ui"
  !call gtk_box_append(box1, rf_cairo_drawing_area)
  !call gtk_window_set_child(sys_config_window, rf_cairo_drawing_area)
  call gtk_window_set_child(sys_config_window, nbk)


  call g_signal_connect(sys_config_window, "destroy"//c_null_char, c_funloc(sys_config_destroy), sys_config_window)


  call gtk_window_set_mnemonics_visible (sys_config_window, TRUE)

  call gtk_widget_show(sys_config_window)


  PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine


end module
