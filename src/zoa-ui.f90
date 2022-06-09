module zoa_ui
    implicit none

integer, parameter :: ID_LENSDRAW_YZ_PLOT_ORIENTATION = 1406
integer, parameter :: ID_LENSDRAW_XZ_PLOT_ORIENTATION = 1407
integer, parameter :: ID_LENSDRAW_XY_PLOT_ORIENTATION = 1408
integer, parameter :: ID_LENSDRAW_ORTHO_PLOT_ORIENTATION = 1405

integer, parameter :: ID_LENSDRAW_PLOT_WHOLE_FIELD    = 1409
integer, parameter :: ID_LENSDRAW_PLOT_HALF_FIELD     = 1410
integer, parameter :: ID_LENSDRAW_AUTOSCALE           = 1411
integer, parameter :: ID_LENSDRAW_MANUALSCALE         = 1412




type  lens_draw_settings
      integer plot_orientation
      integer field_symmetry
      integer changed
      integer start_surface
      integer end_surface
      real    scaleFactor
      integer autoScale
      real elevation
      real azimuth


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




end type lens_draw_settings

interface lens_draw_settings
    module procedure :: lens_draw_settings_constructor
end interface lens_draw_settings

contains

type(lens_draw_settings) function lens_draw_settings_constructor() result(self)

     self%plot_orientation = ID_LENSDRAW_YZ_PLOT_ORIENTATION
     self%changed= 0
     self%field_symmetry = ID_LENSDRAW_PLOT_WHOLE_FIELD
     self%start_surface = 1
     call getOpticalSystemLastSurface(self%end_surface)
     PRINT *, "END SURFACE IS ", self%end_surface

     self%scaleFactor = .045
     self%autoScale = ID_LENSDRAW_AUTOSCALE
     self%elevation = 26.2
     self%azimuth   = 232.2


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
  end if


end subroutine set_plot_orientation

subroutine set_field_symmetry(self, ID_SETTING)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%field_symmetry.ne.ID_SETTING) THEN
     self%field_symmetry = ID_SETTING
     self%changed = 1
  end if


end subroutine set_field_symmetry

subroutine set_start_surface(self, start_surface)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: start_surface

  if (self%start_surface.ne.start_surface) THEN

     self%start_surface = start_surface
     self%changed = 1
  end if


end subroutine set_start_surface

subroutine set_end_surface(self, end_surface)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: end_surface

  if (self%end_surface.ne.end_surface) THEN

     self%end_surface = end_surface
     self%changed = 1
  end if


end subroutine set_end_surface

subroutine set_elevation(self, elevation)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: elevation

  if (self%elevation.ne.elevation) THEN

     self%elevation = elevation
     self%changed = 1
  end if


end subroutine set_elevation

subroutine set_azimuth(self, azimuth)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: azimuth

  if (self%azimuth.ne.azimuth) THEN

     self%azimuth = azimuth
     self%changed = 1
  end if


end subroutine set_azimuth

subroutine set_autoScale(self, autoScale)
  class(lens_draw_settings), intent(inout) :: self
  integer, intent(in) :: autoScale

  if (self%autoScale.ne.autoScale) THEN

     self%autoScale = autoScale
     self%changed = 1
  end if


end subroutine set_autoScale

subroutine set_scaleFactor(self, scaleFactor)
  class(lens_draw_settings), intent(inout) :: self
  real, intent(in) :: scaleFactor

  if (self%scaleFactor.ne.scaleFactor) THEN

     self%scaleFactor = scaleFactor
     self%changed = 1
  end if


end subroutine set_scaleFactor

  subroutine update_zoa_ui_settings_and_replot(ID_SETTING)

    integer, intent(in) :: ID_SETTING

      !if (is_lens_draw_setting(ID_SETTING).EQ.1)
      !   call update_lens_draw_plot(ID_SETTING)
      !endif

  end subroutine update_zoa_ui_settings_and_replot

  subroutine is_lens_draw_setting(ID_SETTING)
      integer, intent(in) :: ID_SETTING


  end subroutine is_lens_draw_setting


end module zoa_ui
