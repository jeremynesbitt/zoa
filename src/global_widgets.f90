module global_widgets
  use zoa_ui
  use iso_c_binding, only: c_ptr, c_char, c_int, c_null_ptr
  use iso_fortran_env, only: real64
  use plot_geometry, only: KDP_PAGE_H, KDP_PAGE_MARGIN, KDP_DEVICE_W, KDP_DEVICE_H
  use kdp_data_types, only: sys_config, io_config, lens_data, paraxial_ray_trace_data, &
  & ray_fan_data, opd_data

  type(c_ptr) :: my_window
  type(c_ptr) :: my_pixbuf, my_drawing_area
  type(c_ptr) :: textView, buffer, statusBar
  type(c_ptr) :: drawing_area_plot, notebook
  character(kind=c_char), dimension(:), pointer :: pixel
  integer(kind=c_int) :: nch, rowstride, width, height, pixwidth, pixheight
  logical :: computing = .false.
  !character(LEN=80) :: string
  type(c_ptr) :: app
  character(len=150), pointer :: uiSettingCommands(:)
  integer :: uiSetCmdsIdx = 1
  

  ! UI Parameters
  type(sys_config) :: sysConfig
  type(io_config)  :: ioConfig
  type(lens_data)  :: curr_lens_data
  type(paraxial_ray_trace_data) :: curr_par_ray_trace
type(ray_fan_data) :: curr_ray_fan_data
  type(opd_data) :: curr_opd
  ! Band aid until I figure out how to properly handle this (refactoring KDP code)
  real(real64), allocatable :: curr_psf(:,:)
  real(real64), allocatable :: curr_mtf(:,:)


  ! Dialogs we only want one of
  type(c_ptr) :: lens_editor_window = c_null_ptr
  type(c_ptr) :: sys_config_window = c_null_ptr
  type(c_ptr) :: macro_ui_window = c_null_ptr
  type(c_ptr) :: optimizer_window = c_null_ptr
  type(c_ptr) :: preferences_window = c_null_ptr


  ! How the lens drawing's page (module plot_geometry) was last placed on its
  ! canvas: screen pixel = offset + scale * page unit, Y flipped about
  ! KDP_PAGE_H.  Set by the renderer (DRAWOPTICALSYSTEM in kdp-draw.f90) and
  ! inverted by the cursor readout (mod_vie_transform.f90).  The defaults
  ! are a 1000 x 700 canvas, until something is drawn.
  real(real64) :: vie_current_scale = 0.1d0
  real(real64) :: vie_offset_x = 0.0d0, vie_offset_y = 0.0d0

  ! The renderer's y-flip origin: KDP_PAGE_H for a page drawing, 0 for the
  ! RMS-field overlay, which draws in its own frame.
  REAL :: kdp_height = KDP_PAGE_H

  logical :: debug_messages = .FALSE.

  character(len=42), allocatable :: currVieData(:)

contains

  ! Where a page drawing goes on a w x h pixel canvas: the largest scale at
  ! which the page and its margin (KDP_DEVICE_W x KDP_DEVICE_H) fit, centred,
  ! and the pixel position (ox, oy) of the page's upper-left corner.
  pure subroutine vie_page_placement(w, h, s, ox, oy)
    integer, intent(in) :: w, h
    real(real64), intent(out) :: s, ox, oy
    s = 0.1d0
    if (w > 0 .and. h > 0) s = min(real(w, real64)/KDP_DEVICE_W, real(h, real64)/KDP_DEVICE_H)
    ox = (real(max(w, 0), real64) - s*KDP_DEVICE_W)/2.0d0 + s*KDP_PAGE_MARGIN
    oy = (real(max(h, 0), real64) - s*KDP_DEVICE_H)/2.0d0 + s*KDP_PAGE_MARGIN
  end subroutine

end module global_widgets
