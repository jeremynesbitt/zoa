module mod_lens_draw_tab
    use zoa_tab
    use mod_plotopticalsystem


    type, extends(zoatab) :: lensdrawtab

    contains
      procedure :: newPlot => lens_draw_new_2
    
    end type

    contains
  subroutine lens_draw_new_2(self)
    !use zoa_tab
    !use ROUTEMOD
    use kdp_draw, only: DRAWOPTICALSYSTEM
    implicit none

    !type(c_ptr) :: parent_window
    class(lensdrawtab) :: self

    type(c_ptr) :: spinButton_firstSurface, spinButton_lastSurface
    ! Added these target parameters to have only one callback function and satisfy
    ! requirement to have a target attribute for a pointer for gtk.  I could not
    ! find a more elegant solution, and this seems better than a bunch of small
    ! callback functions
    integer, target :: TARGET_LENSDRAW_FIELD_SYMMETRY  = ID_LENSDRAW_FIELD_SYMMETRY
    integer, target :: TARGET_LENSDRAW_PLOT_ORIENTATION  = ID_LENSDRAW_PLOT_ORIENTATION

    integer, target :: TARGET_LENSDRAW_SCALE  = ID_LENSDRAW_SCALE
    integer, target :: TARGET_RAYFAN_WFETYPE  = ID_RAYFAN_WFETYPE

    integer, target :: TARGET_LENS_FIRSTSURFACE = ID_LENS_FIRSTSURFACE
    integer, target :: TARGET_LENS_LASTSURFACE   = ID_LENS_LASTSURFACE

    integer, target :: TARGET_LENSDRAW_AZIMUTH   = ID_LENSDRAW_AZIMUTH

    integer, target :: TARGET_LENSDRAW_ELEVATION = ID_LENSDRAW_ELEVATION
    integer, target :: TARGET_LENSDRAW_AUTOSCALE_VALUE = ID_LENSDRAW_AUTOSCALE_VALUE
    integer, target :: TARGET_LENSDRAW_NUM_FIELD_RAYS = ID_LENSDRAW_NUM_FIELD_RAYS


    integer(kind=c_int) :: lastSurface

    character(kind=c_char, len=20), dimension(4) :: vals_plotorientation
    integer(c_int), dimension(4) :: refs_plotorientation

    character(kind=c_char, len=40), dimension(2) :: vals_fieldsymmetry
    integer(c_int), dimension(2) :: refs_fieldsymmetry

    character(kind=c_char, len=40), dimension(2) :: vals_scaleFactor
    integer(c_int), dimension(2) :: refs_scaleFactor

    ! THis is now initialized in execVIE
    !ld_settings = lens_draw_settings()



    vals_fieldsymmetry =  [character(len=40) :: "Plot Upper and Lower Fields of View", &
    & "Plot Upper Fields Only"]

    refs_fieldsymmetry = [ID_LENSDRAW_PLOT_WHOLE_FIELD, ID_LENSDRAW_PLOT_HALF_FIELD]


    PRINT *, "value is ", ld_settings%num_field_rays*1d0
    PRINT *, "lower is ", ld_settings%min_field_rays*1d0
    PRINT *, "upper is ", ld_settings%max_field_rays*1d0
    spinButton_numFieldRays = gtk_spin_button_new (gtk_adjustment_new(value=ld_settings%num_field_rays*1d0, &
                                                                & lower=ld_settings%min_field_rays*1d0, &
                                                                & upper=ld_settings%max_field_rays*1d0, &
                                                                & step_increment=2d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=0_c_int)


    vals_scaleFactor = [character(len=40) :: "Autoscale (Default)", "Manual Scale"]
    refs_scaleFactor = [ID_LENSDRAW_AUTOSCALE, ID_LENSDRAW_MANUALSCALE]



    vals_plotorientation = [character(len=20) :: "YZ - Plane Layout", "XZ - Plane Layout", &
         &"XY - Plane Layout", "Orthographic"]

    refs_plotorientation = [ID_LENSDRAW_YZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XY_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_ORTHO_PLOT_ORIENTATION]


    vals_scaleFactor = [character(len=40) :: "Autoscale (Default)", "Manual Scale"]
    refs_scaleFactor = [ID_LENSDRAW_AUTOSCALE, ID_LENSDRAW_MANUALSCALE]



    call self%addListBoxSetting("Plot Orientation:", refs_plotorientation, &
    & vals_plotorientation, c_funloc(callback_lens_draw_settings), &
    & c_loc(TARGET_LENSDRAW_PLOT_ORIENTATION))

    ! call self%addListBoxSetting("Field Symmetry:", refs_fieldsymmetry, &
    ! & vals_fieldsymmetry, c_funloc(callback_lens_draw_settings), &
    ! & c_loc(TARGET_LENSDRAW_FIELD_SYMMETRY))

    call self%addSpinBoxSetting("Num Rays Per Field", spinButton_numFieldRays, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENSDRAW_NUM_FIELD_RAYS))


    call getOpticalSystemLastSurface(lastSurface)

    spinButton_firstSurface = gtk_spin_button_new (gtk_adjustment_new(value=(ld_settings%start_surface)*1d0, &
                                                                & lower=0d0, &
                                                                & upper=(lastSurface-1)*1d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=0_c_int)


    call self%addSpinBoxSetting("First Surface", spinButton_firstSurface, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENS_FIRSTSURFACE))


    spinButton_lastSurface = gtk_spin_button_new (gtk_adjustment_new(value=(ld_settings%end_surface)*1d0, &
                                                                & lower=2d0, &
                                                                & upper=lastSurface*1d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=0_c_int)

    call self%addSpinBoxSetting("Last Surface", spinButton_lastSurface, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENS_LASTSURFACE))


    ! Spin Buttons for 3D Layout
    spinButton_elevation = gtk_spin_button_new (gtk_adjustment_new(value=26.2d0, &
                                                                & lower=0d0, &
                                                                & upper=180*1d0, &
                                                                & step_increment=10d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=1_c_int)

    call self%addSpinBoxSetting("Elevation", spinButton_elevation, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENSDRAW_ELEVATION))

    ! Spin Buttons for 3D Layout
    spinButton_azimuth = gtk_spin_button_new (gtk_adjustment_new(value=232.2d0, &
                                                                & lower=0d0, &
                                                                & upper=360*1d0, &
                                                                & step_increment=10d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=1_c_int)

    call self%addSpinBoxSetting("Azimuth", spinButton_azimuth, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENSDRAW_AZIMUTH))


    spinButton_scaleFactor = gtk_spin_button_new (gtk_adjustment_new(value=.045*1d0, &
                                                                & lower=0d0, &
                                                                & upper=1000*1d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=3_c_int)

    call gtk_widget_set_sensitive(spinButton_scaleFactor, FALSE)

    call self%addListBoxSetting("Auto or Manual Scale", refs_scaleFactor, &
    & vals_scaleFactor, c_funloc(callback_lens_draw_settings), &
    & c_loc(TARGET_LENSDRAW_SCALE))


    call self%addSpinBoxSetting("Manual Scale Factor", spinButton_scaleFactor, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENSDRAW_AUTOSCALE_VALUE))


    ! Plmumbing not working from ld_settings to use toolbar, so hard coding it here
    call self%finalizeWindow(useToolBar=.TRUE.)  

  end subroutine

end module