module global_widgets
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_float
  use gtk_hl_entry
  use zoa_ui
  use mod_ray_fan_settings
  use mod_lens_draw_settings
  use kdp_data_types


  type(c_ptr) :: my_pixbuf, my_drawing_area
  type(c_ptr) :: textView, buffer, statusBar
  type(c_ptr) :: drawing_area_plot, notebook
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height, pixwidth, pixheight
  logical :: computing = .false.
  character(LEN=80) :: string
  type(c_ptr) :: app


  ! UI Parameters
  type(lens_draw_settings) :: ld_settings
  type(ray_fan_settings)   :: rf_settings
  type(lens_data)  :: curr_lens_data
  type(paraxial_ray_trace_data) :: curr_par_ray_trace

  type(c_ptr) :: rf_window = c_null_ptr
  type(c_ptr) :: rf_cairo_drawing_area !Put this here to debug issue with plotting

  type(c_ptr) :: ld_window = c_null_ptr
  type(c_ptr) :: ld_cairo_drawing_area !Put this here to debug issue with plotting

  type(c_ptr) :: lens_editor_window = c_null_ptr

  REAL :: kdp_width = 10500
  REAL :: kdp_height = 7050

  logical :: debug_messages = FALSE

end module global_widgets
