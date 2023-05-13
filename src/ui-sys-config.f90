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
  use gtk_hl_tree
  !use hl_zoa_tree_tmp
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

type :: relative_field_settings_obj
   type(c_ptr) :: label
   type(c_ptr) :: sb_relX, sb_relY
   type(c_ptr) :: colorSelection
   logical :: editable

end type

type, extends(zoa_settings_obj) :: ui_field_settings

  type(c_ptr) :: fieldType

  !integer :: idxBoolXYSame

contains
  procedure, public, pass(self) :: createUI => createFieldSettingsUI
  procedure, public, pass(self) :: updateSettings => updateFieldSettingsUI
  procedure, public, pass(self) :: createTestList
  procedure, public, pass(self) :: createFieldPointSelectionTable

end type

! interface ui_field_settings
!   module procedure :: ui_field_settings_constructor
! end interface


type, extends(zoatab) :: sysconfigtab


contains
  procedure :: newPlot => sys_config_new_plot

end type

! Variables
  type(sysconfigtab) :: sysconfigwindow
  type(ui_aperture_settings) :: uiApertureSettings
  type(ui_field_settings) :: uiFieldSettings


  integer, parameter :: ID_SYSCON_APERTURE = 7040
  integer, parameter :: ID_SYSCON_FIELDTYPE = 7041
  integer, parameter :: ID_SYSCON_APERTURE_XYSAME = 7042
  integer, parameter :: ID_SYSCON_FIELD_NUM = 7043

  type(c_ptr) :: spinButton_xAperture, spinButton_yAperture

  type(c_ptr) :: spinButton_numFields



contains

  subroutine createApertureSettingsUI(self)
    class(ui_aperture_settings) :: self
    integer, parameter :: ID_SYS_APERTURE = 7037
    integer, parameter :: ID_TST1 = 7038
    integer, parameter :: ID_TST2 = 7039
    integer, target :: TARGET_APERTURE = ID_SYSCON_APERTURE
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

    ! call self%addListBoxTextID("Field ",  &
    ! & sysConfig%refFieldOptions, c_funloc(callback_sys_config_settings), &
    ! & c_loc(TARGET_FIELD))

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

!type(ui_field_settings)  function ui_field_settings_constructor() result(self)

!end function

subroutine createFieldSettingsUI(self)
  class(ui_field_settings) :: self

  integer, target :: TARGET_FIELD = ID_SYSCON_FIELDTYPE
  integer, target :: TARGET_FIELD_NUM = ID_SYSCON_FIELD_NUM
  type(c_ptr) :: tmp

  !integer, target :: TARGET_XYSAME = ID_SYSCON_APERTURE_XYSAME


  call self%addListBoxTextID("Field Type ",  &
  & sysConfig%refFieldOptions, c_funloc(callback_sys_config_settings), &
  & c_loc(TARGET_FIELD))

  ! TODO:  Find a better way of doing this (too risky if parent changes)
  self%fieldType = self%getWidget(self%numSettings)

  spinButton_numFields =   gtk_spin_button_new (gtk_adjustment_new( &
                                                     & value=sysConfig%numFields*1d0, &
                                                               & lower=1d0, &
                                                               & upper=10d0, &
                                                               & step_increment=1d0, &
                                                               & page_increment=1d0, &
                                                               & page_size=0d0),climb_rate=2d0, &
                                                               & digits=0_c_int)

  call self%addSpinBox("Number of Fields", spinButton_numFields, &
   & c_funloc(callback_sys_config_settings), c_loc(TARGET_FIELD_NUM))

  ! Pseudocode for color selection
  ! do ii=1,maxFields
  !

  ! end do
  !tmp = self%createTestList()
  !call self%addListTable(self%createTestList())
  call self%addListTable(self%createFieldPointSelectionTable())

end subroutine

  subroutine cell_edited(renderer, path, text, gdata) bind(c)
    type(c_ptr), value :: renderer, path, text, gdata
    real :: cellData
    ! Default callback for list cell edited.
    !
    ! RENDERER: c_ptr: required: The renderer which sent the signal
    ! PATH: c_ptr: required: The path at which to insert
    ! TEXT: c_ptr: required: The text to insert
    ! GDATA: c_ptr: required: User data, Not used.
    !
    ! The column number is passed via the "column-number" gobject data value.
    ! The treeview containing the cell is passed via the "view" gobject
    ! data value.
    ! The row number is passed as a string in the PATH argument.
    ! This routine is not normally called by the application developer.
    !-

    character(len=200) :: fpath, ftext
    integer(kind=c_int) :: irow
    integer(kind=c_int), pointer :: icol
    type(c_ptr) :: pcol, locallist

    call convert_c_string(path, fpath)
    read(fpath, *) irow
    pcol = g_object_get_data(renderer, "column-number"//c_null_char)
    call c_f_pointer(pcol, icol)
    call convert_c_string(text, ftext)
    locallist = g_object_get_data(renderer, "view"//c_null_char)
    call hl_gtk_listn_set_cell(locallist, irow, icol, &
         & svalue=trim(ftext))

   ! Only want to update field points.  Ignore updates for other columns
   ! This could be handled better than checking column number I'm sure.
   PRINT *, "icol is ", icol
   if (icol < 3 ) THEN
   read(ftext,'(f200.0)') cellData

   print *, "Row selection is ", cellData
   print *, "irow is ", irow
   print *, "icol is ", icol

   sysConfig%relativeFields(icol, irow+1) = cellData
   end if

   !sysConfig%fieldColorCodes(rowSelection+1) = ivalue

  end subroutine

  subroutine ccell_changed(renderer, path, iter, gdata) bind(c)
    !use hl_gtk_zoa
    type(c_ptr), value, intent(in) :: renderer, path, gdata
    type(c_ptr), target :: iter
    type(gvalue), target :: modelv
    type(c_ptr) :: pmodel, model
    integer :: ID_SETTING, ivalue
    type(c_ptr)  :: val, cstr, ival, view
     type(gvalue), target :: result, iresult
     character(len=50)                        :: choice
     type(gtktreeiter), target :: tree_iter
    integer(c_int), dimension(:), allocatable :: selections
    integer :: nsel
    integer :: rowSelection

    ! Basic callback to report what's called
    character(len=200) :: fpath

   view = g_object_get_data(renderer, "view"//c_null_char)

    nsel = hl_gtk_listn_get_selections(C_NULL_PTR, selections, view)
    if (nsel == 0) then
       print *, "No selection"
       !return
    end if

    if (nsel == 1) PRINT *, "Selection is ", selections(1)






    ! Find the model for the combobox
    pmodel = c_loc(modelv)
    pmodel = g_value_init(pmodel, gtk_tree_model_get_type())
    call g_object_get_property(renderer, "model"//c_null_char, pmodel)
    model = g_value_get_object(pmodel)

     !valid = gtk_tree_model_iter_nth_child(store, c_loc(iter), C_NULL_PTR, row)

    val = c_loc(result)
    call gtk_tree_model_get_value(model, c_loc(iter), 1_c_int, val)


  cstr = g_value_get_string(val)
  call convert_c_string(cstr, choice)

  PRINT *, "CHOICE is ", choice

  ! Get ING
  ival = c_loc(iresult)
  call gtk_tree_model_get_value(model, c_loc(iter), 0_c_int, ival)
  ivalue = g_value_get_int(ival)

  PRINT *, "Integer Index is ", ivalue



    !ID_SETTING = hl_zoa_combo_get_selected_list2_id(renderer)
    !PRINT *, "ID_SETTING is ", ID_SETTING

    call c_f_string(path, fpath)
    print *, "Combo sent changed signal from ", trim(fpath)

    read(fpath(1:2),'(i5)') rowSelection
    print *, "Row selection is ", rowSelection

    sysConfig%fieldColorCodes(rowSelection+1) = ivalue


  end subroutine ccell_changed


function createFieldPointSelectionTable(self) result(base)
  use hl_gtk_zoa

  implicit none
  class(ui_field_settings) :: self

  type(c_ptr) :: ihlist, base, ihscrollcontain
  character(len=35) :: line
  integer(c_int) :: i, ltr
  integer(c_int), target :: fmt_col = 2
    integer, parameter :: ncols = 4, nrows=10
    integer(type_kind), dimension(ncols) :: ctypes
    character(len=20), dimension(ncols) :: titles, renderers
    integer(c_int), dimension(ncols) :: sortable, editable
    integer(c_int), dimension(ncols) :: widths
  character(kind=c_char), dimension(10) :: codes
    character(kind=c_char, len=8),dimension(9) :: valsArray
    integer(c_int), dimension(9) :: refsArray




    valsArray = [character(len=8) :: "White", "Yellow", "Magenta", "Red", "Cyan", &
    & "Green", "Blue", "Grey", "Black"]
    !valsArray(2) = "Tst2"
    refsArray = [ID_COLOR_WHITE, ID_COLOR_YELLOW, &
    & ID_COLOR_MAGENTA, ID_COLOR_RED, ID_COLOR_CYAN, &
    & ID_COLOR_GREEN, ID_COLOR_BLUE, ID_COLOR_GREY, &
    & ID_COLOR_BLACK ]






  ! Create the window:

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()
  PRINT *, "FPS:  Created Base"
  ! Now make a multi column list with multiple selections enabled
  ctypes = [ G_TYPE_INT, G_TYPE_FLOAT, G_TYPE_FLOAT, G_TYPE_STRING]
  sortable = [ FALSE, FALSE, FALSE, FALSE ]
  editable = [ FALSE, TRUE, TRUE, TRUE ]
  widths = [-1,-1,-1,-1] !Fixed width needed when adding pixbuf

  titles(1) = "Point"
  titles(2) = "X"
  titles(3) = "Y"
  titles(4) = "Color"


! Now make a multi column list with multiple selections enabled

renderers = [ hl_gtk_cell_text, hl_gtk_cell_text, hl_gtk_cell_text, &
     & hl_gtk_cell_combo] !, hl_gtk_cell_pixbuf ]

PRINT *, "FPS:  About to create ihlist"
PRINT *, "cyptes ", ctypes
PRINT *, "width ", widths
PRINT *, "editable ", editable
PRINT *, "renderers ", renderers


ihlist = hl_gtk_listn_new(types=ctypes, &
     !& changed=c_funloc(list_select),&
     & multiple=TRUE, titles=titles, width=widths, &
     & renderers=renderers, editable=editable, &
     & edited=c_funloc(cell_edited), & !toggled=c_funloc(cell_clicked), &
     !& toggled_radio=c_funloc(rcell_clicked), &
     !& edited_combo=c_funloc(ccell_edit), &
     & changed_combo=c_funloc(ccell_changed)) !, &
     !& valsArray=valsArray, refsArray=refsArray)

call hl_gtk_listn_attach_combo_box_model(ihlist, 3_c_int, valsArray, refsArray)

PRINT *, "FPS:  Created ihlist"

!call hl_gtk_listn_config_combo(ihlist, 3_c_int, &
!     & vals=['one  ', 'two  ', 'three'], &
!     & has_entry=FALSE)




! do i = 1, size(fmt_col)
!    call hl_gtk_listn_set_cell_data_func(ihlist, fmt_col(i), &
!         & func=c_funloc(display_dbl), &
!         & data=c_loc(fmt_col(i)))
! end do
! Now put <nrows> rows into it

call hl_gtk_listn_ins(ihlist, count=nrows)
do i=1,nrows
   !write(line,"('List entry number ',I0)") i

   call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 0_c_int, ivalue=i)
   call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 1_c_int, &
        & fvalue=sysConfig%relativeFields(1,i))
   call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 2_c_int, &
        & fvalue=sysConfig%relativeFields(2,i))

     call hl_gtk_listn_combo_set_by_list_id(ihlist, i-1_c_int, 3_c_int, &
          & targetValue=sysConfig%fieldColorCodes(i))

   call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 10_c_int, logvalue= i==4)
end do


  call hl_gtk_box_pack(base, ihlist)

end function

function createTestList(self) result(base)

  implicit none
  class(ui_field_settings) :: self

  type(c_ptr) :: ihlist, base, ihscrollcontain
  character(len=35) :: line
  integer(c_int) :: i, ltr
  integer(type_kind), dimension(8) :: ctypes
  character(len=20), dimension(8) :: titles
  integer(c_int), dimension(8) :: sortable, editable
  integer(c_int), target :: fmt_col = 2
  character(kind=c_char), dimension(10) :: codes

  ! Create the window:

  ! Now make a column box & put it into the window
  base = hl_gtk_box_new()

  ! Now make a multi column list with multiple selections enabled
  ctypes = [ G_TYPE_STRING, G_TYPE_INT, G_TYPE_INT, G_TYPE_FLOAT, &
       & G_TYPE_UINT64, G_TYPE_BOOLEAN, G_TYPE_CHAR, G_TYPE_UCHAR ]
  sortable = [ FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE ]
  editable = [ TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ]
  codes = [ 'A', 'X', 'B', '?', 'A', 'C', char(185), 'u', '*', char(201)]

  titles(1) = "Name"
  titles(2) = "N"
  titles(3) = "3N"
  titles(4) = "Log(n)"
  titles(5) = "N**4"
  titles(6) = "Odd?"
  titles(7) = "Code"
  titles(8) = "Ucode"

  ihlist = hl_gtk_listn_new(ihscrollcontain, types=ctypes, &
       & multiple=TRUE, height=250_c_int, swidth=600_c_int, titles=titles, &
       & sortable=sortable, editable=editable)

  ! call hl_gtk_listn_set_cell_data_func(ihlist, fmt_col, &
  !      & func=c_funloc(display_int), &
  !      & data=c_loc(fmt_col))

  ! Now put 10 rows into it
  do i=1,10
     call hl_gtk_listn_ins(ihlist)
     write(line,"('List entry number ',I0)") i
     ltr=len_trim(line)+1
     line(ltr:ltr)=c_null_char
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 0_c_int, svalue=line)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 1_c_int, ivalue=i)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 2_c_int, ivalue=3_c_int*i)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 3_c_int, &
          & fvalue=log10(real(i)))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 4_c_int, &
          & l64value=int(i,c_int64_t)**4)
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 5_c_int, &
          & ivalue=mod(i,2_c_int))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 6_c_int, svalue=codes(i))
     call hl_gtk_listn_set_cell(ihlist, i-1_c_int, 7_c_int, svalue=codes(i))
  end do


  ! It is the scrollcontainer that is placed into the box.
  call hl_gtk_box_pack(base, ihscrollcontain)

end function

  subroutine updateFieldSettingsUI(self)
    use hl_gtk_zoa
    class(ui_field_settings) :: self

    !call hl_zoa_combo_set_selected_by_list2_id(self%apertureType, sysConfig%currApertureID)
    call hl_zoa_combo_set_selected_by_list2_id(self%getWidget(1), sysConfig%currFieldID)


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
  type(c_ptr)  :: AsphLabel, expanderField




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
    !uiFieldSettings = ui_field_settings()
    call uiFieldSettings%initialize(numListColumns=1, winWidth=width, name="_Field:")
    call uiFieldSettings%createUI()
    expanderField = uiFieldSettings%build()
    call uiFieldSettings%updateSettings()



    !call gtk_window_set_child(sys_config_window, expander)
    !call gtk_window_set_child(sys_config_window, expanderField)
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 1_c_int)
    call gtk_box_append(box1, expander)
    call gtk_box_append(box1, expanderField)
    call gtk_window_set_child(sys_config_window, box1)

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

  case (ID_SYSCON_FIELD_NUM)
       call sysConfig%setNumFields(INT(gtk_spin_button_get_value (spinButton_numFields)))



  case default
    PRINT *, "Nothing selected"

  end select


  !PRINT *, "Value ID is ", hl_zoa_combo_get_selected_list2_id(widget)

end subroutine
end module
