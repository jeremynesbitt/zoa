
! This is a common module for both plots and for system configuration settings
!TODO:  Put in separate file
module settings_obj
  use gtk
  use collections
  use zoa_plot
  use plot_setting_manager, only: zoaplot_setting_manager, plot_setting

  type zoa_settings_obj

     type(c_ptr) :: expander, table, list
     integer :: numSettings = 0
     type(list) :: labels
     type(list) :: widgets
     integer, dimension(16) :: settingCodes
     integer(c_int)  :: width
     integer :: columnLayout = 2
     character(len=40) :: name

contains
   procedure, public, pass(self) :: initialize => init_settingsgen
   procedure, public, pass(self) :: addListBox
   procedure, public, pass(self) :: addListBoxTextID
   procedure, public, pass(self) :: addCheckBox
   procedure, public, pass(self) :: build
   procedure, public, pass(self) :: settingobj_get
   procedure, public, pass(self) :: getWidget
   procedure, public, pass(self) :: addLabelandWidget
   procedure, public, pass(self) :: addLabelandWidget_new

   procedure, public, pass(self) :: addSpinBox
   procedure, public, pass(self) :: addListTable



  end type

contains

subroutine init_settingsgen(self, numListColumns, winWidth, name)

   !use ROUTEMOD
   implicit none

   class(zoa_settings_obj), intent(inout) :: self
   integer, intent(in), optional :: numListColumns
   integer(c_int), optional :: winWidth
   character(len=*), optional :: name
   integer :: i

   self%numSettings = 0
   self%columnLayout = 2 ! Default

   self%list = c_null_ptr

   ! Initialize list of settings.  At present, support no more than 16 settings
   do i = 1, 16
       call self%labels%push(i)
       call self%widgets%push(i)
   end do

   if (present(numListColumns)) then
     if (numListColumns.GT.0.and.numListColumns.LT.2) Then
       self%columnLayout = numListColumns
     else
       PRINT *, "Not a valid entry for column layout (1 or 2).  ignoring..."
     end if
   end if

   if (present(winWidth)) then
     self%width = winWidth
   else
     self%width = -1
   end if

   if (present(name)) then
     self%name = name
   else
     self%name = "_The parameters:"

   end if


end subroutine

subroutine addCheckBox(self, labelText, callbackFunc, callbackData, SETTING_CODE, initial_state)
  implicit none

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), intent(in)   :: callbackData
  integer, intent(in) :: SETTING_CODE
    type(c_ptr) :: newwidget, newlabel

    integer(kind=c_int), intent(in), optional :: initial_state


    type(c_ptr) :: label_w
    logical :: markup
    logical :: is_toggle


  newlabel = gtk_label_new(labelText//c_null_char)
  newwidget = gtk_check_button_new()

    if (present(initial_state)) then
          call gtk_check_button_set_active(newwidget, initial_state)
    else
      call gtk_check_button_set_active(newwidget, 1)
    end if


  call g_signal_connect (newwidget, "toggled"//c_null_char, callbackFunc, callbackData)


    !      but = gtk_check_button_new_with_label(label)




      ! Update settings lists
      call self%addLabelandWidget_new(newlabel, newwidget, SETTING_CODE)



end subroutine


subroutine addListTable(self, inlist)
  class(zoa_settings_obj) :: self
  type(c_ptr) :: inlist

  self%list = inlist

end subroutine


subroutine addSpinBox(self, labelText, spinButton, callbackFunc, callbackData, SETTING_CODE)
  implicit none

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  integer :: SETTING_CODE
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), intent(in)   :: callbackData
  type(c_ptr), intent(in) :: spinButton

 ! Local only
  type(c_ptr) :: newlabel

  ! Create Objects
  newlabel = gtk_label_new(labelText//c_null_char)
  call g_signal_connect (spinButton, "value-changed"//c_null_char, callbackFunc, callbackData)

  ! Update settings lists
  call self%addLabelandWidget_new(newlabel, spinButton, SETTING_CODE)

end subroutine




subroutine addLabelandWidget_new(self, newlabel, newwidget, SETTING_CODE)
  implicit none
  class(zoa_settings_obj) :: self
  type(c_ptr) :: newlabel, newwidget
  integer :: SETTING_CODE

  ! Update settings
  self%numSettings = self%numSettings + 1
  call self%labels%set(self%numSettings, newlabel)
  call self%widgets%set(self%numSettings, newwidget)
  self%settingCodes(self%numSettings) = SETTING_CODE

end subroutine

subroutine addLabelandWidget(self, newlabel, newwidget)
  implicit none
  class(zoa_settings_obj) :: self
  type(c_ptr) :: newlabel, newwidget

  ! Update settings
  self%numSettings = self%numSettings + 1
  call self%labels%set(self%numSettings, newlabel)
  call self%widgets%set(self%numSettings, newwidget)

end subroutine

! Should this go in a separate type?
subroutine addListBoxTextID(self, labelText, set, callbackFunc, &
  & callbackData, defaultSetting)

 use hl_gtk_zoa
 use kdp_data_types

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), optional, intent(in)   :: callbackData
  integer, optional, intent(in) :: defaultSetting

 type(idText) :: set(:)
 character(kind=c_char, len=40), allocatable :: vals_tmp(:)
 integer(c_int), allocatable :: refs_tmp(:)
 integer :: ii, nOpts


 ! Test code for entry
 nOpts = size(set)
 !PRINT *, "nOpts is ", nOpts
 allocate(vals_tmp(nOpts))
 allocate(refs_tmp(nOpts))

 do ii=1,nOpts
   vals_tmp(ii) = set(ii)%text
   refs_tmp(ii) = set(ii)%id

 end do

 ! TODO:  This is a mess.  needs cleanup
 if (present(callbackData)) then
   if (present(defaultSetting)) then
   call addListBox(self, labelText, refs_tmp, vals_tmp, &
   & callbackFunc, callbackData, defaultSetting)
 else
     call addListBox(self, labelText, refs_tmp, vals_tmp, &
     & callbackFunc, callbackData)
   end if
  else
    if (present(defaultSetting)) then
   call addListBox(self, labelText, refs_tmp, vals_tmp, &
   & callbackFunc, defaultSetting=defaultSetting)
 else
     call addListBox(self, labelText, refs_tmp, vals_tmp, &
     & callbackFunc)
   end if
 end if

end subroutine


subroutine addListBox(self, labelText, refArray, valArray, &
  & callbackFunc, callbackData, defaultSetting)

  use hl_gtk_zoa
  implicit none

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), optional, intent(in)   :: callbackData
  integer, optional, intent(in) :: defaultSetting
   character(kind=c_char, len=*) :: valArray(:)
   integer(c_int), dimension(:) :: refArray

 ! Local only
  type(c_ptr) :: newlabel, newwidget

  ! Create Objects
  newlabel = gtk_label_new(labelText//c_null_char)
  call hl_gtk_combo_box_list2_new(newwidget, refArray, valArray)
  call g_signal_connect (newwidget, "changed"//c_null_char, callbackFunc, callbackData)

    if (present(defaultSetting)) then
      call hl_zoa_combo_set_selected_by_list2_id(newwidget, defaultSetting)

    end if

      ! DEBUG Only
    call gtk_widget_set_name(newwidget, "12345"//c_null_char)

  ! Update settings lists
  call self%addLabelandWidget(newlabel, newwidget)


end subroutine


 function settingobj_get(self,ind,flag) result(c_object)
        ! Arguments

        class(zoa_settings_obj), intent(in) :: self
        integer, intent(in) :: ind, flag
        type(c_ptr), pointer :: c_object

        ! Local Variables
        class(*), pointer :: item


        ! Process
        if (flag.eq.0) THEN

           item => self%labels%get(ind)
         else
           item => self%widgets%get(ind)
         end if

        select type (item)
        !class is (c_ptr)
        !
        class default
            c_object => item
            !nullify(c_object)
        end select

    end function

function getWidget(self, ind) result(widget)
        class(zoa_settings_obj), intent(in) :: self
        integer(kind=c_int), intent(in) :: ind
        type(c_ptr), pointer :: widget
        ! Local Variables
        class(*), pointer :: item

        PRINT *, "Index to get widget is ", ind
        item => self%widgets%get(ind)
        select type (item)
        !class is (c_ptr)
        !    widget => item
        class default
            widget => item
            !nullify(widget)
        end select
        !widget = self%settingobj_get(ind, 1)

end function

function build(self) result(expander)

  implicit none
  class(zoa_settings_obj) :: self

  type(c_ptr) :: expander, boxlist
  integer(kind=c_int) :: i
  integer :: maxRow

   self%table = gtk_grid_new ()
   call gtk_grid_set_column_homogeneous(self%table, TRUE)
   call gtk_grid_set_row_homogeneous(self%table, TRUE)

   ! The table is contained in an expander, which is contained in the vertical box:
   expander = gtk_expander_new_with_mnemonic (trim(self%name)//c_null_char)

   if (self%numSettings.EQ.0) RETURN

   maxRow = self%numSettings
   !TODO :  probably a cleaner way to code this to improve readability.
   ! If column layout = 2, then split up settings
   if (self%columnLayout.EQ.2) THEN

     if (mod(self%numSettings,2).EQ.0) THEN
         maxRow = self%numSettings/2
       else
         maxRow = (self%numSettings+1)/2
     end if
   end if

   !PRINT *, "maxRow is ", maxRow

  do i=1, maxRow
    ! Label
    call gtk_grid_attach(self%table, self%settingobj_get(i,0), 0_c_int, i-1, 1_c_int, 1_c_int)
    ! Widget
    call gtk_grid_attach(self%table, self%settingobj_get(i,1), 1_c_int, i-1, 1_c_int, 1_c_int)
  end do

  ! if column layout = 2, complete list of settings
  if (maxRow.LT.self%numSettings) THEN

  do i=maxRow+1,self%numSettings
    ! Label
    call gtk_grid_attach(self%table, self%settingobj_get(i,0), 2_c_int, i-maxRow-1, 1_c_int, 1_c_int)
    ! Widget
    call gtk_grid_attach(self%table, self%settingobj_get(i,1), 3_c_int, i-maxRow-1, 1_c_int, 1_c_int)
  end do
  end if

   ! Create Settings Box
   ! The table is contained in an expander, which is contained in the vertical box:
   expander = gtk_expander_new_with_mnemonic (trim(self%name)//c_null_char)
  if (.not. c_associated(self%list)) then
    call gtk_expander_set_child(expander, self%table)
  else
   boxlist = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int)
   call gtk_box_append(boxlist, self%table)
   call gtk_box_append(boxlist, self%list)
   call gtk_expander_set_child(expander, boxlist)
  end if

   call gtk_expander_set_expanded(expander, FALSE)

end function

end module

module zoa_tab
  use gtk
  !use global_widgets
  use gtk_hl_container
  use collections
  use settings_obj
  use global_widgets, only: uiSettingCommands, uiSetCmdsIdx
  


! pseudocode for zoatab

!Example 1 - PowSymPlot
! obj = new zoatabplot (child of zoatab?)
! obj.addPlots(2,1) % Multiplot support
! obj.Plot(1,z,y) % mimic PLPLOT here
! obj.addSetting(WAVELENGTH) % common WL setting
! obj.replotviacommand('POWSYM') % tell zoa how to replot in case data change
! eg lens edit

!Example 2 - Ray Fan plot
! obj = new zoatabplot
! obj. addKDPPLot (direct from NEUTARRAY)
! obj.addCustomSetting(fanarray, selections) % need to think  about this some more
! obj.replotviafunction(funcname) ! this already exists

! New Zoa tab
! All tabs are zoatabs in main window
! Tabs can be either plots or message (assume new types could be supported)
! Store all tabs in a list
! FOr each tab, need to be able to get drawing area and what type of plot
! it is (eg lens draw, ray fan)

! Eg when draw is called from KDP
! foreach tab in zoatab
! if isplot, get plottype.  If plottype = desired plot, replot.  else new tab
! When lens is edited
! For each tab in zoatab
! if (depends on lens design) then trigger replot

! Make new zoatabplot and zoatabmsg
! Force all gtk and plplot stuff into separate modules for possible replacement?
! zoatab
! zoatabplot
! zoatabsettings

! TODO
! Add abstract interface to replot function
! Put all settings inside zoaplotManager (extend zoatab?)
! in WDRAW Change to something like call zoaTabMgr%updatePlot(PlotID)
! This would hide the drawing pointers and the settings from the KDP side.
! Also means that zoatabData needs to get built after all of the setting modules
! So where to put zoaTabMgr?



! June 2024 notes
! if i want to abstract this such that a tab can be
! a plot or a window with data do I want to hard code all this stuff?
! Maybe some child types.
! For example:
! If user does lens draw ... call zoaTabMgr%newTab(ID_PLOT)
! If user wants to see table output of ray trace .. call zoaTabMgr%newTab(ID_DATA)
! then there are types of zoa tab that contain specific objects for those types
! The commonality is that zoaTab must have a CLI command to reproduce it             
! and that it must have some sort of ID to tell zoaTabMgr what type of plot it is

!zoatab keeps ID_PLOTTYPE (anme chance, plotCommand (needs to be > 140 chars) and psm)

type zoatab 
   type(zoa_settings_obj) :: settings
  integer(kind=c_int) :: ID_PLOTTYPE
  integer(kind=c_int), pointer :: DEBUG_PLOTTYPE
  character(len=140) :: plotCommand
  type(zoaplot_setting_manager) :: psm
  type(c_ptr) :: tab_label, notebook, expander, box1  

  contains
  procedure, public, pass(self) :: initialize => init_zoatab
  procedure, public, pass(self) :: addListBoxSetting
  procedure, public, pass(self) :: addListBoxSettingTextID
  procedure, public, pass(self) :: finalizeWindow => final_zoatab
  procedure, public, pass(self) :: addSpinButtonFromPS
  procedure, public, pass(self) :: addListBox_new
  procedure, public, pass(self) :: addEntry_runCommand 



  end type

type, extends(zoatab) ::  zoaplottab
  type(c_ptr) :: canvas
  integer(c_int)  :: width = 1*1000 !1000
  integer(c_int)  ::  height = 1*700 !700
  type(zoaplot) :: zPlot ! Should this be in a derived type instead?
  logical :: useToolbar

  contains
  procedure, public, pass(self) :: initialize => init_zoaplottab
  procedure, public, pass(self) :: finalizeWindow => final_zoaplottab
  procedure, public, pass(self) :: updateGenericMultiPlot
  procedure, public, pass(self) :: createGenericMultiPlot
  procedure, public, pass(self) :: newPlot => zoatab_newPlot

end type

! type zoatab
!      type(c_ptr) :: canvas, box1, tab_label, notebook, expander
!      integer(c_int)  :: width = 1*1000 !1000
!      integer(c_int)  ::  height = 1*700 !700
!      type(zoa_settings_obj) :: settings
!      type(zoaplot) :: zPlot ! Should this be in a derived type instead?
!      integer(kind=c_int) :: ID_PLOTTYPE
!      integer(kind=c_int), pointer :: DEBUG_PLOTTYPE
!      character(len=140) :: plotCommand
!      logical :: useToolbar
!      type(zoaplot_setting_manager) :: psm
!      ! This is not being used, but was just added for testing
!      procedure(myinterface), pointer, pass(self) :: newGenericSinglePlot


!      !Note:  I'm not super happy with having addSetting 
!  contains
!    procedure, public, pass(self) :: initialize
!    procedure, public, pass(self) :: addListBoxSetting
!    procedure, public, pass(self) :: addListBoxSettingTextID
!    procedure, public, pass(self) :: finalizeWindow
!    procedure, public, pass(self) :: addSpinButtonFromPS
!    procedure, public, pass(self) :: addListBox_new
!    procedure, public, pass(self) :: addEntry_runCommand 
!    procedure, public, pass(self) :: updateGenericMultiPlot
!    procedure, public, pass(self) :: createGenericMultiPlot
!    procedure, public, pass(self) :: newPlot => zoatab_newPlot



! end type

abstract interface
  subroutine myinterface(self)
    import :: zoatab
   class(zoatab) :: self
  end subroutine
end interface


! This is in ui-utilities
! to close a tab using zoaTabMgr
interface
  subroutine close_zoaTab
  end subroutine
end interface


interface
  function getTabPlotCommand(objIdx)
    character(len=1040) :: getTabPlotCommand
    integer :: objIdx
  end function
end interface

interface
  function updateTabPlotCommand(tabIdx, setting_code, value)
    logical :: updateTabPlotCommand
    integer :: tabIdx
    integer :: setting_code
    class(*) :: value
    !double precision :: value
  end function
  end interface

  interface
  function isSpinButtonInput(tabIdx, setting_code)
    logical:: isSpinButtonInput
    integer :: tabIdx
    integer :: setting_code
  end function
  end interface

  interface
  function getSettingUIType(tabIdx, setting_code)
    integer :: getSettingUIType
    integer :: tabIdx
    integer :: setting_code
  end function
  end interface  

contains ! for module


subroutine init_zoaplottab(self, parent_window, tabTitle, ID_PLOTTYPE, canvas)

  !use ROUTEMOD
  implicit none

  class(zoaplottab) :: self

  type(c_ptr) :: parent_window
  integer(kind=c_int) :: ID_PLOTTYPE
  type(c_ptr), optional :: canvas
  character(len=*) :: tabTitle
  type(c_ptr) :: tab_label, btn
  integer, target :: ID_TARGET


  ID_TARGET = ID_PLOTTYPE
  PRINT *, "tabTitle is ", tabTitle
  ! Set up button for exiting
  self%tab_label = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
  !self%tab_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0_c_int)
  tab_label = gtk_label_new(tabTitle//c_null_char)
  call hl_gtk_box_pack(self%tab_label, tab_label)
  !call gtk_box_append(head, tab_label)
  btn = gtk_button_new_from_icon_name ("window-close-symbolic")
  call gtk_button_set_has_frame (btn, FALSE)
  call gtk_widget_set_focus_on_click (btn, FALSE)
  call hl_gtk_box_pack (self%tab_label, btn);
  call g_signal_connect(btn, 'clicked'//c_null_char, c_funloc(close_zoaTab), c_loc(ID_TARGET))
  !self%tab_label = head
  !self%tab_label = tab_label

  !PRINT *, "Created tab label ", self%tab_label
  call gtk_widget_set_halign(self%tab_label, GTK_ALIGN_START)


  self%ID_PLOTTYPE = ID_PLOTTYPE
  self%notebook = parent_window

  self%box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);

  !call self%createCairoDrawingArea()
  if (present(canvas)) then
    call LogTermFOR("In zoa_tab initialize already have canvas?")
    self%canvas = canvas
  else
   call createCairoDrawingAreaForDraw(self%canvas, self%width, self%height, ID_PLOTTYPE)
   PRINT *, "Cairo Drawing Area is ", LOC(self%canvas)
  end if

  call self%settings%initialize()
  PRINT *, "Done with zoatab type initialization"

end subroutine

 subroutine init_zoatab(self, parent_window, tabTitle, ID_PLOTTYPE, canvas)

    !use ROUTEMOD
    implicit none

    class(zoatab) :: self
    type(c_ptr), optional :: canvas
    type(c_ptr) :: parent_window
    integer(kind=c_int) :: ID_PLOTTYPE
    character(len=*) :: tabTitle
    type(c_ptr) :: tab_label, btn
    integer, target :: ID_TARGET

    ! TODO:  MOve this to separate method to support different derived types.  Unless I can call this from children? 
    ID_TARGET = ID_PLOTTYPE
    ! Set up button for exiting
    self%tab_label = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    !self%tab_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0_c_int)
    tab_label = gtk_label_new(tabTitle//c_null_char)
    call hl_gtk_box_pack(self%tab_label, tab_label)
    !call gtk_box_append(head, tab_label)
    btn = gtk_button_new_from_icon_name ("window-close-symbolic")
    call gtk_button_set_has_frame (btn, FALSE)
    call gtk_widget_set_focus_on_click (btn, FALSE)
    call hl_gtk_box_pack (self%tab_label, btn);
    call g_signal_connect(btn, 'clicked'//c_null_char, c_funloc(close_zoaTab), c_loc(ID_TARGET))

    call gtk_widget_set_halign(self%tab_label, GTK_ALIGN_START)


    self%ID_PLOTTYPE = ID_PLOTTYPE
    self%notebook = parent_window

    self%box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
    call self%settings%initialize()    


 end subroutine

subroutine createGenericMultiPlot(self, mplt)
  use zoa_plot
  implicit none
  class(zoaplottab) :: self
  type(multiplot) :: mplt
  type(c_ptr) ::  isurface


  PRINT *, "canvas in createGenericMultiPlot is", LOC(self%canvas)
  isurface = g_object_get_data(mplt%area, "backing-surface")
  PRINT *, "multiplot isurface is ", LOC(isurface)
  if (.not. c_associated(isurface)) then
     PRINT *, "error:  new plot :: Backing surface is NULL.  Adding one"
     isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 700, 500)
     isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
     call g_object_set_data(self%canvas, "backing-surface", isurface)
     mplt%area = self%canvas

  end if  

  call mplt%draw()
  
end subroutine


subroutine updateGenericMultiPlot(self, mplt)
  class(zoaplottab) :: self
  type(multiplot) :: mplt
  if (c_associated(self%canvas)) then
      
      mplt%area = self%canvas
  else
    call LogTermFOR( "Multiplot update canvas ptr is loose")
    PRINT *, "Multiplot update canvas ptr is loose"
    PRINT *, "mplt%area is ", LOC(mplt%area)
     self%canvas = mplt%area
  end if
  call mplt%draw()
end subroutine

subroutine addEntry_runCommand(self, labelTxt, valueStr, command, SETTING_CODE)
  use gtk_hl_entry
  use gtk, only: gtk_entry_get_buffer, gtk_entry_buffer_set_text
  implicit none
  class(zoatab) :: self
  character(len=*), intent(in) :: valueStr
  character(len=*), intent(in) :: labelTxt
  character(len=*), intent(in) :: command
  integer, optional :: SETTING_CODE ! TODO:  Make this not optional once all plots are converted
  type(c_ptr) :: entryBox, newLabel

  entryBox = hl_gtk_entry_new(60_c_int, editable=TRUE, &
  & activate=c_funloc(callback_runCommandFromSpinBox_new), data=self%box1)  

  call gtk_entry_buffer_set_text(gtk_entry_get_buffer(entryBox), &
  & valueStr//c_null_char, -1)

    ! Store the commands in the name field of the widget, as we will have access to it 
    ! in the callback fcn                                                
    call gtk_widget_set_name(entryBox, trim(command)//c_null_char)

    newlabel = gtk_label_new(labelTxt//c_null_char)

    if(present(SETTING_CODE)) then 
      call self%settings%addLabelandWidget_new(newLabel, entryBox, SETTING_CODE)
    else
      call self%settings%addLabelandWidget(newLabel, entryBox)
    end if


  end subroutine


  subroutine addSpinButtonFromPS(self, ps)
    use type_utils, only: int2str
    implicit none
    class(zoatab) :: self
    type(plot_setting) :: ps
    type(c_ptr) :: spinBtn
  

    spinBtn = gtk_spin_button_new (gtk_adjustment_new(value=ps%default*1d0, &
                                                      & lower=ps%min*1d0, &
                                                      & upper=ps%max*1d0, &
                                                      & step_increment=1d0, &
                                                      & page_increment=1d0, &
                                                      & page_size=0d0), &
                                                      & climb_rate=2d0, &
                                                      & digits=3_c_int)
  
      ! Store the commands in the name field of the widget, as we will have access to it 
      ! in the callback fcn                                                
      call gtk_widget_set_name(spinBtn, trim(int2str(ps%ID))//c_null_char)
      !call gtk_widget_set_name(spinBtn, trim(command)//c_null_char)
  
      call self%settings%addSpinBox(ps%label, spinBtn, &
      & c_funloc(callback_runCommandFromSpinBox_new), self%box1, ps%ID)
  
  end subroutine

subroutine callback_runCommandFromSpinBox(widget, gdata ) bind(c)
  use command_utils, only:  removeLeadingBlanks
  use type_utils, only: int2str
  implicit none
  type(c_ptr), value, intent(in) :: widget, gdata
  integer :: intVal
 character(len=23) :: ffieldstr
 character(len=140) :: cmdOrig
 character(len=10) :: cmdToUpdate
 character(len=150) :: cmdNew
 type(c_ptr) :: cstr, buff2
 character(len=50) :: choice
 integer :: locDelim
 character(len=150), pointer :: command_base



 call c_f_pointer(gdata, command_base)

 cstr = gtk_widget_get_name(widget)
 call convert_c_string(cstr, cmdToUpdate)
 cstr= gtk_widget_get_name(gdata) 
 call convert_c_string(cstr, cmdOrig)


 !locDelim = INDEX(command_base, "--")
 !cmdToUpdate = command_base(1:locDelim-1)
 !cmdOrig = command_base(locDelim+2:len(command_base))

 !cstr = gtk_widget_get_name(gtk_widget_get_parent(gtk_widget_get_parent(widget)))


 !realData = gtk_spin_button_get_value(widget)
 !TODO:  Find a better way to tell if the widget is spin button vs entry or separate out fcns
if (cmdToUpdate(1:1).NE.'c') then
   intVal = INT(gtk_spin_button_get_value(widget))
   ffieldstr = int2str(intVal)
else
  PRINT *, "entry box!"
  buff2 = gtk_entry_get_buffer(widget)
  call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ffieldstr)
end if
    




  !write(ffieldstr, *) real(realData)

   !ffieldstr = adjustl(ffieldstr)
  !PRINT *, "command_base is ", command_base
  PRINT *, "locDelim is ", locDelim
  PRINT *, "cmdOrig is ", cmdOrig
  PRINT *, "cmdToupdate is ", cmdToUpdate
  PRINT *, "newVal is ", trim(ffieldstr)


 call updateCommand(cmdOrig, trim(cmdToUpdate), ffieldstr, cmdNew)

 PRINT *, "New Command 2 is ", cmdNew
 call gtk_Widget_set_name(gdata, trim(cmdNew)//c_null_char)
 !call gtk_widget_set_name(gtk_widget_get_parent(gtk_widget_get_parent(widget)), & 
 !& cmdNew//c_null_char)
 !PRINT *, "command_base is ", trim(command_base) //' '//trim(ffieldstr)

 CALL PROCESKDP(cmdNew)

end subroutine

function findTabParent(widget) result(tabIdx)
  use type_utils, only: str2int

 implicit none
 type(c_ptr) :: widget
 type(c_ptr) :: cstr, p1, p2
 character(len=140) :: winName
 integer :: tabIdx

! Tmp code
  p1 = gtk_widget_get_parent(widget)
  cstr= gtk_widget_get_name(p1) 
  call convert_c_string(cstr, winName)
 
  call LogTermFOR("Parent Windown name is "// trim(winName))

  p2 = gtk_widget_get_parent(p1)
  cstr= gtk_widget_get_name(p2) 
  call convert_c_string(cstr, winName)
 
  call LogTermFOR("Parent Windown name is "// trim(winName))
  tabIdx = str2int(winName)


end function

subroutine callback_runCommandFromSpinBox_new(widget, gdata ) bind(c)
  use command_utils, only:  removeLeadingBlanks
  use type_utils, only: int2str, str2int
  use zoa_ui
  use hl_gtk_zoa
  implicit none
  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double) :: realData
  integer :: intVal
 character(len=23) :: ffieldstr
 character(len=140) :: cmdOrig
 character(len=20) :: strSettingCode
 character(len=150) :: cmdNew
 type(c_ptr) :: cstr, buff2
 character(len=50) :: choice
 integer :: setting_code

 character(len=150), pointer :: command_base

 integer :: tabIdx
 logical :: result



 call c_f_pointer(gdata, command_base)

 cstr = gtk_widget_get_name(widget)
 call convert_c_string(cstr, strSettingCode)
setting_code = str2int(trim(strSettingCode))
tabIdx = findTabParent(gdata)


select case(getSettingUIType(tabIdx, setting_code))

case (UITYPE_COMBO)
  result = updateTabPlotCommand(tabIdx, setting_code, &
  & real(hl_zoa_combo_get_selected_list2_id(widget)))

case (UITYPE_SPINBUTTON)
  result = updateTabPlotCommand(tabIdx, setting_code, gtk_spin_button_get_value(widget))
case (UITYPE_ENTRY)
  buff2 = gtk_entry_get_buffer(widget)
  call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ffieldstr)  
 result = updateTabPlotCommand(tabIdx, setting_code, trim(ffieldstr))
end select


    
if (result) call PROCESKDP(getTabPlotCommand(tabIdx))

 !locDelim = INDEX(command_base, "--")
 !cmdToUpdate = command_base(1:locDelim-1)
 !cmdOrig = command_base(locDelim+2:len(command_base))

 !cstr = gtk_widget_get_name(gtk_widget_get_parent(gtk_widget_get_parent(widget)))


 !realData = gtk_spin_button_get_value(widget)
 !TODO:  Find a better way to tell if the widget is spin button vs entry or separate out fcns

! if (cmdToUpdate(1:1).NE.'c') then
!    intVal = INT(gtk_spin_button_get_value(widget))
!    ffieldstr = int2str(intVal)
! else
!   PRINT *, "entry box!"
!   buff2 = gtk_entry_get_buffer(widget)
!   call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ffieldstr)
! end if
    




  !write(ffieldstr, *) real(realData)

   !ffieldstr = adjustl(ffieldstr)
  !PRINT *, "command_base is ", command_base
  !PRINT *, "locDelim is ", locDelim
  !PRINT *, "cmdOrig is ", cmdOrig
  !PRINT *, "cmdToupdate is ", cmdToUpdate
  !PRINT *, "newVal is ", trim(ffieldstr)


 !call updateCommand(cmdOrig, trim(cmdToUpdate), ffieldstr, cmdNew)

 !PRINT *, "New Command 2 is ", cmdNew
 !call gtk_Widget_set_name(gdata, trim(cmdNew)//c_null_char)
 !call gtk_widget_set_name(gtk_widget_get_parent(gtk_widget_get_parent(widget)), & 
 !& cmdNew//c_null_char)
 !PRINT *, "command_base is ", trim(command_base) //' '//trim(ffieldstr)

! CALL PROCESKDP(cmdNew)

end subroutine

 subroutine updateCommand(cmdOrig, cmdToUpdate, newVal, cmdNew)
  use command_utils, only:  parseCommandIntoTokens
  implicit none
  character(len=*) :: cmdOrig
  character(len=*) :: cmdToUpdate
  character(len=*) :: newVal
  character(len=150), intent(inout) :: cmdNew
  character(len=80) :: tokens(40)
  character(len=1), parameter :: blank = " "
  integer :: locBlank, locCmd, i, numTokens, j
  character(len=10), dimension(5) :: specialCmds
  logical :: boolSpecialCmd


  PRINT *, "LEN of cmdToUpdate is ", LEN(cmdToUpdate)
  PRINT *, "cmdUpdate is ", cmdToUpdate


  
  locBlank= 0
  locBlank = index(cmdOrig, blank)
  PRINT *, "locBlank is ", locBlank
  if (locBlank.LT.1) then
     cmdNew = trim(cmdOrig)//blank//cmdToUpdate//trim(newVal)


     !PRINT *, "New command is supposed to be ", cmdOrig//", "//cmdToUpdate//" "//newVal
  else
     locCmd = 0
     call parseCommandIntoTokens(cmdOrig(locBlank+1:len(cmdOrig)), tokens, numTokens, blank)
     do i=1,numTokens
       locCmd = index(tokens(i), cmdToUpdate)
       if (locCmd.GT.0) then
         PRINT *, "Found command to update!"
         PRINT *, "tokens(i) is ", tokens(i)
         PRINT *, "cmdToUpdate is ", cmdToUpdate(1:len(cmdToUpdate)-1)
         tokens(i) = cmdToUpdate//trim(newVal)
         ! Build New Command, should be separate method
         cmdNew = cmdOrig(1:locBlank)
         do j=1,numTokens
           cmdNew = trim(cmdNew)//" "//trim(tokens(j))
         end do
         PRINT *, "new command from token found loop is ", cmdNew
         return
       end if
       end do
       ! If we got here it means command was not found.  Add to end of initial command
       cmdNew = trim(cmdOrig)//" "//cmdToUpdate//newVal

  end if


end subroutine

 subroutine addListBox_new(self, labelText, set, callbackFunc, callbackData, defaultSetting, winName, SETTING_CODE)

  use hl_gtk_zoa
  use kdp_data_types
   implicit none
   class(zoatab) :: self
   character(len=*), intent(in) :: labelText
   character(len=*), intent(in) :: winName
   type(c_funptr), intent(in)   :: callbackFunc
   type(c_ptr), intent(in)   :: callbackData
   integer, intent(in) :: defaultSetting
   integer, optional :: SETTING_CODE
  type(idText) :: set(:)


 ! Local only
  type(c_ptr) :: newlabel, newwidget

  ! Create Objects
  newlabel = gtk_label_new(labelText//c_null_char)
  call hl_gtk_combo_box_listid_new(newwidget, set)
  call hl_zoa_combo_set_selected_by_list2_id(newwidget, defaultSetting)
  call g_signal_connect (newwidget, "changed"//c_null_char, callbackFunc, callbackData)

  call gtk_widget_set_name(newwidget, winName//c_null_char)

  ! Update settings lists
  if(present(SETTING_CODE)) then
    call self%settings%addLabelandWidget_new(newlabel, newwidget, SETTING_CODE)
  else
    call self%settings%addLabelandWidget(newlabel, newwidget)
  end if




end subroutine

 subroutine addListBoxSettingTextID(self, labelText, set, callbackFunc, callbackData)

  use hl_gtk_zoa
  use kdp_data_types

   class(zoatab) :: self
   character(len=*), intent(in) :: labelText
   type(c_funptr), intent(in)   :: callbackFunc
   type(c_ptr), optional, intent(in)   :: callbackData

  type(idText) :: set(:)

  call self%settings%addListBoxTextID(labelText, set, callbackFunc, callbackData)


end subroutine


 subroutine addListBoxSetting(self, labelText, refArray, valArray, callbackFunc, callbackData)

   use hl_gtk_zoa
   implicit none

   class(zoatab) :: self
   character(len=*), intent(in) :: labelText
   type(c_funptr), intent(in)   :: callbackFunc
   type(c_ptr), optional, intent(in)   :: callbackData
    character(kind=c_char, len=*) :: valArray(:)
    integer(c_int), dimension(:) :: refArray

    call self%settings%addListBox(labelText, refArray, valArray, callbackFunc, callbackData)

 end subroutine


 subroutine final_zoatab(self, useToolBar)
  use g
  implicit none
  class(zoatab) :: self
  logical, optional :: useToolBar

  type(c_ptr) :: scrolled_tab, box_plotmanip, btn
   type(c_ptr) :: dcname
   character(len=80) :: dname

  integer :: location
   PRINT *, "FINALIZING WINDOW in ZOATAB"

   !call self%buildSettings()
   self%expander = self%settings%build()
   !if (self%settings%useToolbar) call self%settings%init_toolbar(self%canvas, box_plotmanip)
   call gtk_widget_set_name(self%expander, self%plotCommand)


   call gtk_box_append(self%box1, self%expander)
   call gtk_widget_set_vexpand (self%box1, FALSE)



   scrolled_tab = gtk_scrolled_window_new()
   PRINT *, "SETTING CHILD FOR SCROLLED TAB"
   call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
   location = gtk_notebook_append_page(self%notebook, scrolled_tab, self%tab_label)
   call gtk_notebook_set_current_page(self%notebook, location)


   call gtk_widget_set_name(self%box1, trim(self%plotCommand)//c_null_char)



end subroutine



 subroutine final_zoaplottab(self, useToolBar)
   use g
   use zoa_plot_manip_toolbar, only: createPlotManipulationToolbar
   implicit none
   class(zoaplottab) :: self
   logical, optional :: useToolBar

   type(c_ptr) :: scrolled_tab, box_plotmanip, btn
    type(c_ptr) :: dcname
    character(len=80) :: dname

   integer :: location
    PRINT *, "FINALIZING WINDOW in ZOATAB"

    !call self%buildSettings()
    self%expander = self%settings%build()
    !if (self%settings%useToolbar) call self%settings%init_toolbar(self%canvas, box_plotmanip)
    call gtk_widget_set_name(self%expander, self%plotCommand)
    PRINT *, "Expander is ", LOC(self%EXPANDER)
    PRINT *, "Box ptr is ", LOC(self%box1)

    if (present(useToolBar)) then
      print *, "UseToolbar here, value ", useToolBar
      if (useToolBar) then
        call LogTermFOR("Here is where I would like to init toolbar!")
       call createPlotManipulationToolbar(self%canvas, box_plotmanip) 
       call gtk_box_append(self%box1, box_plotmanip)
    end if
    end if
    

    ! We create a vertical box container:
    call gtk_box_append(self%box1, self%expander)
    call gtk_widget_set_vexpand (self%box1, FALSE)


    call gtk_box_append(self%box1, self%canvas)



    scrolled_tab = gtk_scrolled_window_new()
    PRINT *, "SETTING CHILD FOR SCROLLED TAB"
    call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
    location = gtk_notebook_append_page(self%notebook, scrolled_tab, self%tab_label)
    call gtk_notebook_set_current_page(self%notebook, location)


    call gtk_widget_set_name(self%box1, trim(self%plotCommand)//c_null_char)
    !dcname = g_type_name(gtk_widget_get_type(self%zoatab_cda))
    !dcname = g_type_name_from_instance(self%zoatab_cda)
    !call c_f_string(dcname, dname)
    !PRINT *, "WIDGET NAME IS ", dname


 end subroutine

 subroutine createCairoDrawingAreaForDraw(canvas, width, height, ID_PLOTTYPE)
     use gtk, only: gtk_drawing_area_new, gtk_drawing_area_set_content_width, &
     & gtk_drawing_area_set_content_height, gtk_drawing_area_set_draw_func
     use gtk_draw_hl
     !use kdp_interfaces, only : ROUTEDRAWING
     !use kdp_draw, only: DRAWOPTICALSYSTEM

     type(c_ptr), intent(inout) :: canvas
     integer(kind=c_int), intent(in)  :: width, height, ID_PLOTTYPE

     integer(kind=c_int), target :: TARGET_NEWPLOT
     if (ID_PLOTTYPE > 0) THEN
        canvas = gtk_drawing_area_new()
        !  canvas = hl_gtk_drawing_area_new(size=[width, height])
        !TARGET_NEWPLOT = ID_PLOTTYPE

        call gtk_drawing_area_set_content_width(canvas, width)
        call gtk_drawing_area_set_content_height(canvas, height)
      end if




 end subroutine

subroutine zoatab_newPlot(self)

  class(zoaplottab) :: self
  !type(c_ptr) :: parent_window
  PRINT *, "Dummy Function never used, only by children"

end subroutine

end module zoa_tab
