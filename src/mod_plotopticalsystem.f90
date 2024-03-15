

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
  use zoa_ui
  use zoa_tab
  !use kdp_interfaces

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


type, extends (ui_settings) ::  lens_draw_settings
      integer plot_orientation
      integer field_symmetry
      integer changed
      integer start_surface
      integer end_surface
      integer min_field_rays
      integer max_field_rays
      integer num_field_rays
      real    scaleFactor
      integer autoScale
      real elevation
      real azimuth
      logical autoplotupdate


contains
    procedure, public, pass(self) :: is_changed
    procedure, public, pass(self) :: set_plot_orientation
    procedure, public, pass(self) :: set_field_symmetry
    procedure, public, pass(self) :: set_start_surface
    procedure, public, pass(self) :: set_end_surface
    procedure, public, pass(self) :: set_autoScale
    procedure, public, pass(self) :: set_scaleFactor
    procedure, public, pass(self) :: set_elevation
    procedure, public, pass(self) :: set_azimuth
    procedure, public, pass(self) :: set_field_rays

    procedure :: replot => lens_draw_replot




end type lens_draw_settings


interface lens_draw_settings
    module procedure :: lens_draw_settings_constructor
end interface lens_draw_settings


type, extends(zoatab) :: lensdrawtab

contains
  procedure :: newPlot => lens_draw_new_2

end type


  type(lens_draw_settings) :: ld_settings
  type(c_ptr) :: win,bar,pbar,qbut, box
  integer(kind=c_int) :: run_status = TRUE
  integer(c_int) :: boolresult

  type(c_ptr) ::  combo_plotorientation
  type(c_ptr) ::  spinButton_azimuth, spinButton_elevation
  type(c_ptr) :: spinButton_scaleFactor, spinButton_numFieldRays

  real :: elevation_default = 26.2
  real :: azimuth_default = 232.2


contains


type(lens_draw_settings) function lens_draw_settings_constructor() result(self)

     self%plot_orientation = ID_LENSDRAW_YZ_PLOT_ORIENTATION
     self%changed= 0
     self%field_symmetry = ID_LENSDRAW_PLOT_WHOLE_FIELD
     self%start_surface = 1
     self%end_surface = 13
     self%min_field_rays = 3
     self%max_field_rays = 15
     self%num_field_rays = 5
     call getOpticalSystemLastSurface(self%end_surface)
     PRINT *, "******************** END SURFACE CONSTRUCTOR ", self%end_surface
     !WRITE(OUTLYNE, *), "END SURFACE IN LD SETTINGS CONSTRUCTOR ", self%end_surface
     !CALL SHOWIT(19)

     self%scaleFactor = .045
     self%autoScale = ID_LENSDRAW_AUTOSCALE
     self%elevation = 26.2
     self%azimuth   = 232.2
     self%autoplotupdate = .TRUE.


end function lens_draw_settings_constructor

function is_changed(self) result(flag)
  class(lens_draw_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function is_changed

subroutine set_plot_orientation(self, ID_SETTING)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%plot_orientation.ne.ID_SETTING) THEN
     self%plot_orientation = ID_SETTING
     self%changed = 1
     PRINT *, "About to call replot routine"
     PRINT *, "AUTO PLOT UPDATE IS ", self%autoplotupdate
     if (self%autoplotupdate) call self%replot()
  end if



end subroutine set_plot_orientation

subroutine set_field_symmetry(self, ID_SETTING)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%field_symmetry.ne.ID_SETTING) THEN
     self%field_symmetry = ID_SETTING
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_field_symmetry

subroutine set_start_surface(self, start_surface)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: start_surface

  if (self%start_surface.ne.start_surface) THEN

     self%start_surface = start_surface
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_start_surface

subroutine set_end_surface(self, end_surface)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: end_surface

  if (self%end_surface.ne.end_surface) THEN

     self%end_surface = end_surface
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_end_surface

subroutine set_elevation(self, elevation)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: elevation

  if (self%elevation.ne.elevation) THEN

     self%elevation = elevation
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_elevation

subroutine set_azimuth(self, azimuth)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: azimuth

  if (self%azimuth.ne.azimuth) THEN

     self%azimuth = azimuth
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_azimuth

subroutine set_autoScale(self, autoScale)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: autoScale

  if (self%autoScale.ne.autoScale) THEN

     self%autoScale = autoScale
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_autoScale

subroutine set_scaleFactor(self, scaleFactor)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: scaleFactor

  if (self%scaleFactor.ne.scaleFactor) THEN

     self%scaleFactor = scaleFactor
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if


end subroutine set_scaleFactor

subroutine set_field_rays(self, num_field_rays)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: num_field_rays

  if (self%num_field_rays.ne.num_field_rays) THEN

     self%num_field_rays = num_field_rays
     self%changed = 1
     if (self%autoplotupdate) call self%replot()
  end if

end subroutine

  subroutine update_zoa_ui_settings_and_replot(ID_SETTING)

    integer, intent(in) :: ID_SETTING

      !if (is_lens_draw_setting(ID_SETTING).EQ.1)
      !   call update_lens_draw_plot(ID_SETTING)
      !endif

  end subroutine update_zoa_ui_settings_and_replot

  subroutine is_lens_draw_setting(ID_SETTING)
      integer, intent(in) :: ID_SETTING


  end subroutine is_lens_draw_setting

subroutine lens_draw_replot(self)

  class(lens_draw_settings) :: self


  character(len=40) :: command, qual_word
  character(len=100) :: ftext
  character(len=3)   :: AJ, AK
  character(len=23) :: autoScale_text, AW2, AW3

  command = "VIECO"

  ! It is possible for the end surface to have been updated since init

  ! Original logic in LENSED.INC
  select case (self%field_symmetry)
  case (ID_LENSDRAW_PLOT_HALF_FIELD)
       ftext = 'VIESYM OFF'
       CALL PROCESKDP(ftext)
  case (ID_LENSDRAW_PLOT_WHOLE_FIELD)
       ftext = 'VIESYM ON'
       CALL PROCESKDP(ftext)
  end select

  select case (self%plot_orientation)

  case (ID_LENSDRAW_YZ_PLOT_ORIENTATION)
       qual_word = "YZ"
  case (ID_LENSDRAW_XZ_PLOT_ORIENTATION)
      qual_word = "XZ"
  case (ID_LENSDRAW_XY_PLOT_ORIENTATION)
      qual_word = "XY"
  case (ID_LENSDRAW_ORTHO_PLOT_ORIENTATION)
       qual_word = "ORTHO"
        !CALL DTOA23(ld_settings%elevation,AW2)
        !CALL DTOA23(ld_settings%azimuth,AW3)
        WRITE(AW2, *) self%elevation
        WRITE(AW3, *) self%azimuth

        ftext ='PLOT VIEW,'//AW2//','//AW3
        !PRINT *, "ORTHO TEXT IS ", ftext
        !PRINT *, "LD Settings Elevation, Azimuth is ", ld_settings%elevation, ",", ld_settings%azimuth
        CALL PROCESKDP(ftext)
  case DEFAULT
      qual_word = " "
  end select


  !INPUT='VIECO,'//','//AJ//','//AK//',1'

!      AUTOSCALE
!        CALL ITOAA(ISTARTSURF,AJ)
!        CALL ITOAA(ISTOPSURF,AK)
!        INPUT='VIECO,'//','//AJ//','//AK//',1'
!        CALL PROCES

  ! Start and End Surface
  CALL ITOAA(self%start_surface, AJ)
  CALL ITOAA(self%end_surface, AK)


  if (self%autoScale.eq.ID_LENSDRAW_MANUALSCALE) THEN
      Call DTOA23(self%scaleFactor,autoScale_text)
      WRITE(autoScale_text, *) self%scaleFactor

  else
      autoScale_text = trim("")
  end if

     
    ftext = trim(command)//" "//trim(qual_word)//","//autoScale_text//","//AJ//","//AK//",0,1"
    !call LogTermFOR(trim(ftext))

    CALL PROCESKDP(ftext)

end subroutine lens_draw_replot

subroutine callback_lens_draw_settings (widget, gdata ) bind(c)
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

  case (ID_LENSDRAW_NUM_FIELD_RAYS)
    call ld_settings % set_field_rays(INT(gtk_spin_button_get_value (widget)))

  !case (ID_LENSDRAW_FIELD_SYMMETRY)
  !  call ld_settings % set_field_symmetry(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_LENSDRAW_PLOT_ORIENTATION)
    call ld_settings % set_plot_orientation(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_LENSDRAW_SCALE)

    call ld_settings % set_autoScale(hl_zoa_combo_get_selected_list2_id(widget))

      if (hl_zoa_combo_get_selected_list2_id(widget).eq.ID_LENSDRAW_MANUALSCALE) THEN
         call gtk_widget_set_sensitive(spinButton_scaleFactor, TRUE)
       else
         call gtk_widget_set_sensitive(spinButton_scaleFactor, FALSE)
      end if

  case (ID_LENS_FIRSTSURFACE)
    call ld_settings % set_start_surface(INT(gtk_spin_button_get_value (widget)))

  case (ID_LENS_LASTSURFACE)
    call ld_settings % set_end_surface(INT(gtk_spin_button_get_value (widget)))

  case (ID_LENSDRAW_AZIMUTH)
    call ld_settings % set_azimuth(REAL(gtk_spin_button_get_value (widget)))

  case (ID_LENSDRAW_ELEVATION)
    call ld_settings % set_elevation(REAL(gtk_spin_button_get_value (widget)))


  case (ID_LENSDRAW_AUTOSCALE_VALUE)
    call ld_settings % set_scaleFactor(REAL(gtk_spin_button_get_value (widget)))


  end select

  ! Currently autoreplotting will happen in the settings module

end subroutine


  subroutine lens_draw_new_2(self)
    use zoa_tab
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


    ld_settings = lens_draw_settings()



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

    spinButton_firstSurface = gtk_spin_button_new (gtk_adjustment_new(value=1d0, &
                                                                & lower=0d0, &
                                                                & upper=(lastSurface-1)*1d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=0_c_int)


    call self%addSpinBoxSetting("First Surface", spinButton_firstSurface, &
    & c_funloc(callback_lens_draw_settings), c_loc(TARGET_LENS_FIRSTSURFACE))


    spinButton_lastSurface = gtk_spin_button_new (gtk_adjustment_new(value=lastSurface*1d0, &
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



    call self%finalizeWindow()

  end subroutine


end module
