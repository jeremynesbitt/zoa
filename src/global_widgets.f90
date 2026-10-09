module global_widgets
  use zoa_ui
  use iso_c_binding, only: c_ptr, c_char, c_int, c_null_ptr
  use iso_fortran_env, only: real64
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


  ! Lens-drawing device-space constants -- single source of truth, referenced by
  ! the renderer (DRAWOPTICALSYSTEM/JK_MOVETOCAIRO in kdp-draw.f90) and the
  ! cursor->world transform (mod_vie_transform.f90) so they can never drift apart.
  !   KDP_PLOT_HEIGHT : y-flip origin for the lens drawing (KDP device units)
  !   KDP_CAIRO_SCALE : KDP units -> screen pixels factor (cairo_scale)
  ! See test/KNOWN_ISSUES.md for the separate (frame) dimension set in kdp_plot_gen.
  real(real64), parameter :: KDP_PLOT_HEIGHT = 7050.0d0
  real(real64), parameter :: KDP_CAIRO_SCALE = 0.1d0
  ! The lens drawing's frame width in KDP units (with KDP_PLOT_HEIGHT).
  real(real64), parameter :: KDP_PLOT_WIDTH = 10500.0d0
  ! The scale the lens drawing was last drawn at: KDP_CAIRO_SCALE headless
  ! (fixed-size PNGs), otherwise vie_draw_scale of its widget -- the cursor
  ! readout (mod_vie_transform) inverts with the same value.
  real(real64) :: vie_current_scale = KDP_CAIRO_SCALE

  ! kdp_height doubles as the per-plot y-flip origin (lens draw uses the full
  ! height, the RMS-field overlay sets it to 0), so it stays a mutable variable.
  REAL :: kdp_width  = 10500
  REAL :: kdp_height = KDP_PLOT_HEIGHT

  logical :: debug_messages = .FALSE.

  character(len=42), allocatable :: currVieData(:)

contains

  ! The largest scale at which the whole lens-drawing frame fits w x h pixels.
  pure function vie_draw_scale(w, h) result(s)
    integer, intent(in) :: w, h
    real(real64) :: s
    s = KDP_CAIRO_SCALE
    if (w > 0 .and. h > 0) s = min(real(w, real64)/KDP_PLOT_WIDTH, real(h, real64)/KDP_PLOT_HEIGHT)
  end function

end module global_widgets
