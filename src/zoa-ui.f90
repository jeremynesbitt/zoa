module zoa_ui
   !use global_widgets

   implicit none

integer, parameter :: ID_LENSDRAW_YZ_PLOT_ORIENTATION = 1406
integer, parameter :: ID_LENSDRAW_XZ_PLOT_ORIENTATION = 1407
integer, parameter :: ID_LENSDRAW_XY_PLOT_ORIENTATION = 1408
integer, parameter :: ID_LENSDRAW_PLOT_WHOLE_FIELD    = 1409
integer, parameter :: ID_LENSDRAW_PLOT_HALF_FIELD     = 1410

type  lens_draw_settings
      integer plot_orientation
      integer field_symmetery
      integer changed

contains
    procedure, public, pass(self) :: is_changed
    procedure, public, pass(self) :: set_plot_orientation
    procedure, public, pass(self) :: set_field_symmetry

end type lens_draw_settings

interface lens_draw_settings
    module procedure :: lens_draw_settings_constructor
end interface lens_draw_settings

contains

type(lens_draw_settings) function lens_draw_settings_constructor() result(self)

     self%plot_orientation = ID_LENSDRAW_YZ_PLOT_ORIENTATION
     self%changed= 0

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

  if (self%field_symmetery.ne.ID_SETTING) THEN
     self%field_symmetery = ID_SETTING
     self%changed = 1
  end if


end subroutine set_field_symmetry

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
