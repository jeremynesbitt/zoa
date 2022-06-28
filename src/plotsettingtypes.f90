module mod_lens_draw_settings

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


end module mod_lens_draw_settings

module mod_ray_fan_settings
 use zoa_ui

type ray_fan_settings
   integer fan_type
   integer fan_wfe
   integer changed
   real maxPupil
   real minPupil
   integer numRays
   integer wavelength

 contains
    procedure, public, pass(self) :: ray_fan_is_changed
    procedure, public, pass(self) :: set_ray_fan
    procedure, public, pass(self) :: set_fan_wfe
    procedure, public, pass(self) :: set_max_pupil
    procedure, public, pass(self) :: set_min_pupil
    procedure, public, pass(self) :: set_num_rays
    procedure, public, pass(self) :: set_ray_fan_wavelength


end type ray_fan_settings

interface ray_fan_settings
  module procedure :: ray_fan_settings_constructor
end interface ray_fan_settings

contains
type(ray_fan_settings) function ray_fan_settings_constructor() result(self)
    self%fan_type = ID_RAYFAN_Y_FAN
    self%fan_wfe  = ID_RAYFAN_TRANSVERSE_RAY
    self%maxPupil = 1.0
    self%minPupil = -1.0
    self%numRays = 11
    self%wavelength = 2 ! TODO NEED TO GET DEFAULT WAVELENGTH FROM PRESCRIPTION

end function ray_fan_settings_constructor


function ray_fan_is_changed(self) result(flag)
  class(ray_fan_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function ray_fan_is_changed

subroutine set_ray_fan(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%fan_type.ne.ID_SETTING) THEN
     self%fan_type = ID_SETTING
     self%changed = 1
  end if


end subroutine set_ray_fan

subroutine set_max_pupil(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  real, intent(in) :: ID_SETTING

  if (self%maxPupil.ne.ID_SETTING) THEN
     self%maxPupil = ID_SETTING
     self%changed = 1
  end if


end subroutine set_max_pupil

subroutine set_min_pupil(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  real, intent(in) :: ID_SETTING

  if (self%minPupil.ne.ID_SETTING) THEN
     self%minPupil = ID_SETTING
     self%changed = 1
  end if


end subroutine set_min_pupil

subroutine set_num_rays(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%numRays.ne.ID_SETTING) THEN
     self%numRays = ID_SETTING
     self%changed = 1
  end if


end subroutine set_num_rays

subroutine set_ray_fan_wavelength(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%wavelength.ne.ID_SETTING) THEN
     self%wavelength = ID_SETTING
     self%changed = 1
  end if


end subroutine set_ray_fan_wavelength

subroutine set_fan_wfe(self, ID_SETTING)
  class(ray_fan_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%fan_wfe.ne.ID_SETTING) THEN
     self%fan_wfe = ID_SETTING
     self%changed = 1
  end if


end subroutine set_fan_wfe

end module mod_ray_fan_settings
