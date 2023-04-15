! Desired behavior:
! for each section
! add ui elements with options
! set options based on KDP / sysconfig type call setValues(ID_SECTION))
! make subtype of ui settings that keeps track of widgets and has a function
! that can update

module ui_sys_config
  use GLOBALS
  use global_widgets
  use handlers
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  !use gtk_hl_tree
  use hl_zoa_tree_tmp
  use gtk

  use gtk_hl_chooser

  use g
  use zoa_ui
  use settings_obj
  !use mod_lens_editor_settings

  implicit none

! type, extends (ui_settings) :: sys_settings
!
!
!  contains
!     procedure, public, pass(self) :: set_sys_aperture
!     procedure, public :: replot => ray_fan_replot
!
!
! end
type, extends(zoa_settings_obj) :: ui_aperture_settings

  type(c_ptr) :: apertureType
  integer :: idxBoolXYSame

contains
  procedure, public, pass(self) :: createUI => createApertureSettingsUI
  procedure, public, pass(self) :: updateSettings => updateApertureSettingsUI

end type

type, extends(zoatab) :: sysconfigtab


contains
  procedure :: newPlot => sys_config_new_plot

end type

! Variables
  type(sysconfigtab) :: sysconfigwindow
  type(ui_aperture_settings) :: uiApertureSettings

  integer, parameter :: ID_SYSCON_APERTURE = 7040
  integer, parameter :: ID_SYSCON_FIELDTYPE = 7041
  integer, parameter :: ID_SYSCON_APERTURE_XYSAME = 7042

  type(c_ptr) :: spinButton_xAperture, spinButton_yAperture



contains

  subroutine createApertureSettingsUI(self)
    class(ui_aperture_settings) :: self
    integer, parameter :: ID_SYS_APERTURE = 7037
    integer, parameter :: ID_TST1 = 7038
    integer, parameter :: ID_TST2 = 7039
    integer, target :: TARGET_APERTURE = ID_SYSCON_APERTURE
    integer, target :: TARGET_FIELD = ID_SYSCON_FIELDTYPE
    integer, target :: TARGET_XYSAME = ID_SYSCON_APERTURE_XYSAME


    integer, target :: TARGET_X_APERTURE = 7050
    integer, target :: TARGET_Y_APERTURE = 7051


    call self%addListBoxTextID("Aperture ",  &
    & sysConfig%aperOptions, c_funloc(callback_sys_config_settings), &
    & c_loc(TARGET_APERTURE))

    ! TODO:  Find a better way of doing this (too risky if parent changes)
    self%apertureType = self%getWidget(self%numSettings)


    spinButton_xAperture = gtk_spin_button_new (gtk_adjustment_new( &
                                                     & value=sysConfig%refApertureValue(1)*1d0, &
                                                               & lower=0d0, &
                                                               & upper=10000000d0, &
                                                               & step_increment=0.05d0, &
                                                               & page_increment=.1d0, &
                                                               & page_size=0d0),climb_rate=2d0, &
                                                               & digits=3_c_int)

    call self%addSpinBox("X Aperture Value", spinButton_xAperture, &
    & c_funloc(callback_sys_config_settings), c_loc(TARGET_X_APERTURE))

    !call sysconfigwindow%addSpinBoxSetting("X Aperture Value", spinButton_xAperture, &
    !& c_funloc(callback_sys_config_settings), c_loc(TARGET_X_APERTURE))



    spinButton_yAperture = gtk_spin_button_new (gtk_adjustment_new(&
                                                     & value=sysConfig%refApertureValue(2)*1d0, &
                                                               & lower=0d0, &
                                                               & upper=10000000d0, &
                                                               & step_increment=0.05d0, &
                                                               & page_increment=.1d0, &
                                                               & page_size=0d0),climb_rate=2d0, &
                                                               & digits=3_c_int)

    call self%addSpinBox("Y Aperture Value", spinButton_yAperture, &
    & c_funloc(callback_sys_config_settings), c_loc(TARGET_Y_APERTURE))

    !call sysconfigwindow%addSpinBoxSetting("Y Aperture Value", spinButton_yAperture, &
    !& c_funloc(callback_sys_config_settings), c_loc(TARGET_Y_APERTURE))

    call self%addListBoxTextID("Field ",  &
    & sysConfig%refFieldOptions, c_funloc(callback_sys_config_settings), &
    & c_loc(TARGET_FIELD))

    call self%addCheckBox("XY Symmetric", c_funloc(callback_sys_config_settings), &
    & c_loc(TARGET_XYSAME))
    self%idxBoolXYSame = self%numSettings


  end subroutine

  subroutine updateApertureSettingsUI(self)
    use hl_gtk_zoa
    class(ui_aperture_settings) :: self

    !call hl_zoa_combo_set_selected_by_list2_id(self%apertureType, sysConfig%currApertureID)
    call hl_zoa_combo_set_selected_by_list2_id(self%getWidget(1), sysConfig%currApertureID)


  end subroutine

  subroutine sys_config_destroy(widget, gdata) bind(c)

    type(c_ptr), value :: widget, gdata
    type(c_ptr) :: isurface
    print *, "Exit called"
    call gtk_window_destroy(sys_config_window)
    sys_config_window = c_null_ptr
    !call sysconfigwindow%labels%clear
    !call sysconfigwindow%widgets%clear
    sysconfigwindow%canvas = c_null_ptr
    sysconfigwindow%box1 = c_null_ptr
    sysconfigwindow%notebook = c_null_ptr
    sysconfigwindow%tab_label = c_null_ptr





  end subroutine

subroutine buildApertureSettings(boxAperture)
  type(c_ptr), intent(inout) :: boxAperture

        ! Relevant settings from LENSED.INC

        ! CASE (IDF_B1)
        ! SYSTEM(60)=1.0D0
        ! SYSTEM(61)=1.0D0
        ! SYSTEM(18)=0.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B2)
        ! SYSTEM(60)=1.0D0
        ! SYSTEM(61)=1.0D0
        ! SYSTEM(18)=1.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B3)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(94)=-1.0D0
        ! SYSTEM(95)=-1.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B4)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(94)=-1.0D0
        ! SYSTEM(95)=-1.0D0
        ! SYSTEM(98)=0.0D0
        ! SYSTEM(99)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B5)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(98)=-1.0D0
        ! SYSTEM(99)=-1.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'
        ! CASE (IDF_B6)
        ! SYSTEM(60)=0.0D0
        ! SYSTEM(61)=0.0D0
        ! SYSTEM(98)=1.0D0
        ! SYSTEM(99)=1.0D0
        ! SYSTEM(94)=0.0D0
        ! SYSTEM(95)=0.0D0
        ! INCLUDE 'NONSURF3FRESH.INC'

      !  call sysConfig%getApertueSettings(listofNames)
      !  call setUpComboBox(listofNames, listofValues)

        !In Callback
      !  call sysConfig%setApertureByName(selectedName)


end subroutine

subroutine sys_config_new(parent_window)

  type(c_ptr) :: parent_window
  !type(c_ptr), value :: sys_config_window

  type(c_ptr) :: content, junk, gfilter
  integer(kind=c_int) :: icreate, idir, action, lval
  integer(kind=c_int) :: i, idx0, idx1, pageIdx
  !integer(c_int)  :: width, height

  type(c_ptr)  :: boxAperture, lblAperture

  type(c_ptr)  :: table, expander, box1, nbk, basicLabel, boxAsphere
  type(c_ptr)  :: AsphLabel




    !character(kind=c_char, len=20), dimension(2) :: vals_tst

    !integer(c_int), dimension(2) :: refs_tst
    character(kind=c_char, len=40), allocatable :: vals_tst(:)
    integer(c_int), allocatable :: refs_tst(:)
    integer :: nOpts, ii



  PRINT *, "ABOUT TO FIRE UP SYS CONFIG WINDOW!"

  ! Create a modal dialogue
  sys_config_window = gtk_window_new()

  call gtk_window_set_title(sys_config_window, "Optical System Configuration"//c_null_char)

  width = 300
  height = 400
     call gtk_window_set_default_size(sys_config_window, width, height)

     call gtk_window_set_transient_for(sys_config_window, parent_window)
     call gtk_window_set_destroy_with_parent(sys_config_window, TRUE)

     call uiApertureSettings%initialize(numListColumns=1, winWidth=width, name="_Aperture:")

     call uiApertureSettings%createUI()



    ! call sysconfigwindow%addListBoxSettingTextID("Field ",  &
    ! & sysConfig%refFieldOptions, c_funloc(callback_sys_config_settings), &
    ! & c_loc(TARGET_FIELD))
    expander = uiApertureSettings%build()
    call uiApertureSettings%updateSettings()

    call gtk_window_set_child(sys_config_window, expander)
  ! call sysconfigwindow%finalizeWindow()

  !lblAperture = gtk_label_new_with_mnemonic("_Aperture"//c_null_char)
  !pageIdx = gtk_notebook_append_page(nbk, boxAperture, basicLabel)

  !AsphLabel = gtk_label_new_with_mnemonic("_Fields"//c_null_char)
  !pageIdx = gtk_notebook_append_page(nbk, boxAsphere, AsphLabel)


  PRINT *, "FINISHED Setting up system config ui"
  !call gtk_box_append(box1, rf_cairo_drawing_area)
  !call gtk_window_set_child(sys_config_window, rf_cairo_drawing_area)
!  call gtk_window_set_child(sys_config_window, nbk)


  call g_signal_connect(sys_config_window, "destroy"//c_null_char, c_funloc(sys_config_destroy), sys_config_window)


  call gtk_window_set_mnemonics_visible (sys_config_window, TRUE)

  !call hl_zoa_combo_set_selected_by_list2_id()

  call gtk_widget_show(sys_config_window)


  PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine

subroutine sys_config_new_plot(self)

  class(sysconfigtab) :: self

  PRINT *, "SYS CONFIG ROUTINE CALLED VIA ZOA TAB!"
  call self%finalizeWindow()
end

subroutine callback_sys_config_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value
   real :: xAp, yAp
   integer :: xySame

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  !PRINT *, "SYS CONFIG IS ", ID_SETTING

    xySame = gtk_check_button_get_active(uiApertureSettings%getWidget(uiApertureSettings%idxBoolXYSame))
    PRINT *, "xySame is ", xySame

  select case (ID_SETTING)

  case (ID_SYSCON_APERTURE)



    int_value = hl_zoa_combo_get_selected_list2_id(widget)
    PRINT *, "Aperture Selection for ", int_value
    yAp = REAL(gtk_spin_button_get_value (spinButton_yAperture))
    xAp = REAL(gtk_spin_button_get_value (spinButton_xAperture))
    !xySame = gtk_check_button_get_active ()

    call sysConfig%updateApertureSelectionByCode(int_value, xAp, yAp, xySame)

  call gtk_spin_button_set_value(spinButton_xAperture, sysConfig%refApertureValue(1)*1d0)
  call gtk_spin_button_set_value(spinButton_yAperture, sysConfig%refApertureValue(2)*1d0)

  case (ID_SYSCON_FIELDTYPE)
    int_value = hl_zoa_combo_get_selected_list2_id(widget)
    PRINT *, "Field Type Selection for ", int_value

  case default
    PRINT *, "Nothing selected"

  end select


  !PRINT *, "Value ID is ", hl_zoa_combo_get_selected_list2_id(widget)

end subroutine
end module
