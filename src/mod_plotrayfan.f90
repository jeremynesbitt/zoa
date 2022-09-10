

module mod_plotrayfan


  use GLOBALS
  use global_widgets
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  use gtk_hl_tree
  use gtk

  use gtk_hl_chooser

  use g
  use zoa_ui
  use mod_ray_fan_settings

  implicit none

  integer(kind=c_int) :: run_status = TRUE
  type(ray_fan_settings)   :: rf_settings

contains


  subroutine rf_my_destroy(widget, gdata) bind(c)
    ! added this dependency to allow for windows to close / reopoen
    ! However I get a bunch of GTK assertion widget failures when I reopen
    ! even though the UI works.  TODO need to find this bug
    !use mod_plotopticalsystem

    type(c_ptr), value :: widget, gdata
    type(c_ptr) :: isurface
    print *, "Exit called"


    isurface = g_object_get_data(widget, "backing-surface")
    if (c_associated(isurface)) call cairo_surface_destroy(isurface)
    !call cairo_destroy(rf_cairo_drawing_area)
    !call gtk_widget_unparent(gdata)
    !call g_object_unref(rf_cairo_drawing_area)
    call gtk_window_destroy(widget)
    !rf_cairo_drawing_area = c_null_ptr
    !call g_object_unref(rf_cairo_drawing_area)
    !call g_object_unref(gdata)
    !call g_object_unref(widget)
    !call gtk_window_destroy(widget)
    !call hl_gtk_drawing_area_cairo_destroy(rf_cairo_drawing_area)
    !
    !gdata = c_null_ptr

    !rf_window = c_null_ptr

  end subroutine rf_my_destroy



subroutine callback_ray_fan_settings (widget, gdata ) bind(c)
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

  case (ID_RAYFAN_FANTYPE)
    call rf_settings % set_ray_fan(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_RAYFAN_WFETYPE)
    call rf_settings % set_fan_wfe(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_RAYFAN_NUMRAYS)

    call rf_settings % set_num_rays(INT(gtk_spin_button_get_value (widget)))

  case (ID_RAYFAN_WAVELENGTH)
    call rf_settings % set_ray_fan_wavelength(INT(gtk_spin_button_get_value (widget)))

  case (ID_RAYFAN_MAX_PUPIL)
    call rf_settings % set_max_pupil(REAL(gtk_spin_button_get_value (widget)))

  case (ID_RAYFAN_MIN_PUPIL)
    call rf_settings % set_min_pupil(REAL(gtk_spin_button_get_value (widget)))


  end select

  ! Currently autoreplotting will happen in the settings module

end subroutine callback_ray_fan_settings


  subroutine ray_fan_new(rayfantab)
    use zoa_tab
    use ROUTEMOD
    use kdp_draw, only: DRAWOPTICALSYSTEM
    implicit none

    type(c_ptr) :: parent_window
    type(zoatab) :: rayfantab

    type(c_ptr) :: spinButton_maxPupil, spinButton_minPupil, spinButton_numRays, &
    & spinButton_wavelength
    ! Added these target parameters to have only one callback function and satisfy
    ! requirement to have a target attribute for a pointer for gtk.  I could not
    ! find a more elegant solution, and this seems better than a bunch of small
    ! callback functions
    integer, target :: TARGET_RAYFAN_WAVELENGTH  = ID_RAYFAN_WAVELENGTH
    integer, target :: TARGET_RAYFAN_NUMRAYS  = ID_RAYFAN_NUMRAYS

    integer, target :: TARGET_RAYFAN_FANTYPE  = ID_RAYFAN_FANTYPE
    integer, target :: TARGET_RAYFAN_WFETYPE  = ID_RAYFAN_WFETYPE

    integer, target :: TARGET_RAYFAN_MAX_PUPIL   = ID_RAYFAN_MAX_PUPIL
    integer, target :: TARGET_RAYFAN_MIN_PUPIL   = ID_RAYFAN_MIN_PUPIL

    integer, target :: TARGET_NEWPLOT_RAYFAN   = ID_NEWPLOT_RAYFAN


    character(kind=c_char, len=20), dimension(4) :: vals_fantype
    integer(c_int), dimension(4) :: refs_fantype

    character(kind=c_char, len=40), dimension(4) :: vals_wfetype
    integer(c_int), dimension(4) :: refs_wfetype

    rf_settings = ray_fan_settings()

    vals_fantype = [character(len=20) :: "Y - Fan", "X - Fan", &
         &"P - Fan", "N - Fan"]

    refs_fantype = [ID_RAYFAN_Y_FAN, &
                          & ID_RAYFAN_X_FAN, &
                          & ID_RAYFAN_P_FAN, &
                          & ID_RAYFAN_N_FAN]

    vals_wfetype =  [character(len=40) :: "Transverse Ray Aberration", &
                   & "Optical Path Difference (OPD)", &
                   & "Chromatic Differences", &
                   & "Longitudinal Aberrations"]

    refs_wfetype = [ID_RAYFAN_TRANSVERSE_RAY, &
                  & ID_RAYFAN_TRANSVERSE_OPD, &
                  & ID_RAYFAN_CHROMATIC, &
                  & ID_RAYFAN_LONGITUDINAL]

    !call rayfantab%initialize(notebook, "Ray Fan Output", ID_NEWPLOT_RAYFAN)



    call rayfantab%addListBoxSetting("Ray Fan", refs_fantype, vals_fantype, &
    & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_FANTYPE))

    call rayfantab%addListBoxSetting("Aberration Type", refs_wfetype, vals_wfetype, &
    & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_WFETYPE))


    spinButton_maxPupil = gtk_spin_button_new (gtk_adjustment_new(value=1d0, &
                                                                & lower=0d0, &
                                                                & upper=1d0, &
                                                                & step_increment=0.05d0, &
                                                                & page_increment=.1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=3_c_int)


    spinButton_minPupil = gtk_spin_button_new (gtk_adjustment_new(value=-1d0, &
                                                                & lower=-1d0, &
                                                                & upper=0d0, &
                                                                & step_increment=0.05d0, &
                                                                & page_increment=.1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=3_c_int)

    call rayfantab%addSpinBoxSetting("Maximum Pupil Value", spinButton_maxPupil, &
    & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_MAX_PUPIL))

    call rayfantab%addSpinBoxSetting("Minimum Pupil Value", spinButton_minPupil, &
    & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_MIN_PUPIL))


    spinButton_numRays = gtk_spin_button_new (gtk_adjustment_new(value=11d0, &
                                                                & lower=1d0, &
                                                                & upper=20d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=1d0, &
                                                                & digits=0_c_int)


    spinButton_wavelength = gtk_spin_button_new (gtk_adjustment_new(value=2d0, &
                                                                & lower=1d0, &
                                                                & upper=4d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=1d0, &
                                                                & digits=0_c_int)


   call rayfantab%addSpinBoxSetting("Number of Rays in the Fan", spinButton_numRays, &
   & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_NUMRAYS))

  call rayfantab%addSpinBoxSetting("Wavelength to Trace", spinButton_wavelength, &
  & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_WAVELENGTH))


    call rayfantab%finalizeWindow()
    !call rf_settings%replot()
    !call DRAWOPTICALSYSTEM(rayfantab%canvas, c_null_ptr, rayfantab%width, rayfantab%height, c_null_ptr)

    !rf_window = rayfantab%box1
    !rf_cairo_drawing_area = rayfantab%canvas

  end subroutine



end module
