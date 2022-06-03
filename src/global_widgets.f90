module global_widgets
  use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_float
  use gtk_hl_entry
  use zoa_ui

  type(c_ptr) :: my_pixbuf, my_drawing_area, spinButton1, spinButton2, spinButton3
  type(c_ptr) :: textView, buffer, scrolled_window, statusBar, combo1
  type(c_ptr) :: drawing_area_plot
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height, pixwidth, pixheight
  logical :: computing = .false.
  character(LEN=80) :: string
  type(c_ptr) :: app

  ! UI Parameters
  type(lens_draw_settings) :: ld_settings

end module global_widgets
