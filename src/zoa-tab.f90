
! This is a common module for both plots and for system configuration settings
module settings_obj
  use gtk
  use collections

  type zoa_settings_obj

     type(c_ptr) :: expander, table, list
     integer :: numSettings = 0
     type(list) :: labels
     type(list) :: widgets
     integer(c_int)  :: width
     integer :: columnLayout = 2
     character(len=40) :: name

contains
   procedure, public, pass(self) :: initialize => init_settingsgen
   procedure, public, pass(self) :: addListBox
   procedure, public, pass(self) :: addListBoxTextID
   procedure, public, pass(self) :: addCheckBox
   procedure, public, pass(self) :: build
   procedure, private, pass(self) :: settingobj_get
   procedure, public, pass(self) :: getWidget
   procedure, private, pass(self) :: addLabelandWidget
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

subroutine addCheckBox(self, labelText, callbackFunc, callbackData, initial_state)
  implicit none

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), optional, intent(in)   :: callbackData
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
      call self%addLabelandWidget(newlabel, newwidget)



end subroutine

subroutine addListTable(self, inlist)
  class(zoa_settings_obj) :: self
  type(c_ptr) :: inlist

  self%list = inlist

end subroutine

subroutine addSpinBox(self, labelText, spinButton, callbackFunc, callbackData)
  implicit none

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), optional, intent(in)   :: callbackData
  type(c_ptr), intent(in) :: spinButton

 ! Local only
  type(c_ptr) :: newlabel

  ! Create Objects
  newlabel = gtk_label_new(labelText//c_null_char)
  call g_signal_connect (spinButton, "changed"//c_null_char, callbackFunc, callbackData)

  ! Update settings lists
  call self%addLabelandWidget(newlabel, spinButton)

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
subroutine addListBoxTextID(self, labelText, set, callbackFunc, callbackData)

 use hl_gtk_zoa
 use kdp_data_types

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), optional, intent(in)   :: callbackData

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

 if (present(callbackData)) then
   call addListBox(self, labelText, refs_tmp, vals_tmp, &
   & callbackFunc, callbackData)
  else
   call addListBox(self, labelText, refs_tmp, vals_tmp, &
   & callbackFunc)
 end if

end subroutine


subroutine addListBox(self, labelText, refArray, valArray, callbackFunc, callbackData)

  use hl_gtk_zoa
  implicit none

  class(zoa_settings_obj) :: self
  character(len=*), intent(in) :: labelText
  type(c_funptr), intent(in)   :: callbackFunc
  type(c_ptr), optional, intent(in)   :: callbackData
   character(kind=c_char, len=*) :: valArray(:)
   integer(c_int), dimension(:) :: refArray

 ! Local only
  type(c_ptr) :: newlabel, newwidget

  ! Create Objects
  newlabel = gtk_label_new(labelText//c_null_char)
  call hl_gtk_combo_box_list2_new(newwidget, refArray, valArray)
  call g_signal_connect (newwidget, "changed"//c_null_char, callbackFunc, callbackData)

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
        class is (c_ptr)
            c_object => item
        class default
            nullify(c_object)
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
        class is (c_ptr)
            widget => item
        class default
            nullify(widget)
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

   PRINT *, "maxRow is ", maxRow

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




type zoatab
     type(c_ptr) :: canvas, box1, tab_label, notebook
     integer(c_int)  :: width = 1000
     integer(c_int)  ::  height = 700
     type(zoa_settings_obj) :: settings
     integer(kind=c_int) :: ID_PLOTTYPE
     integer(kind=c_int), pointer :: DEBUG_PLOTTYPE



 contains
   procedure, public, pass(self) :: initialize
   !procedure, private, pass(self) :: createCairoDrawingArea
   procedure, public, pass(self) :: addListBoxSetting
   procedure, public, pass(self) :: addListBoxSettingTextID
   procedure, public, pass(self) :: finalizeWindow
   procedure, public, pass(self) :: addSpinBoxSetting
   procedure, public, pass(self) :: newPlot => zoatab_newPlot



end type

interface
  subroutine close_zoaTab2
  end subroutine
end interface

contains ! for module

 subroutine initialize(self, parent_window, tabTitle, ID_PLOTTYPE)

    !use ROUTEMOD
    implicit none

    class(zoatab) :: self

    type(c_ptr) :: parent_window
    integer(kind=c_int) :: ID_PLOTTYPE
    character(len=*) :: tabTitle
    type(c_ptr) :: tab_label, scrolled_tab, head, btn
    integer :: i
    integer, target :: ID_TARGET

    ID_TARGET = ID_PLOTTYPE
    PRINT *, "tabTitle is ", tabTitle
    ! Set up button for exiting
    self%tab_label = hl_gtk_box_new(horizontal=TRUE, spacing=0_c_int)
    !self%tab_label = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0_c_int)
    tab_label = gtk_label_new(tabTitle//c_null_char)
    call hl_gtk_box_pack(self%tab_label, tab_label)
    !call gtk_box_append(head, tab_label)
    btn = gtk_button_new_from_icon_name ("gtk-close")
    call gtk_button_set_has_frame (btn, FALSE)
    call gtk_widget_set_focus_on_click (btn, FALSE)
    call hl_gtk_box_pack (self%tab_label, btn);
    call g_signal_connect(btn, 'clicked'//c_null_char, c_funloc(close_zoaTab2), c_loc(ID_TARGET))
    !self%tab_label = head
    !self%tab_label = tab_label

    PRINT *, "Created tab label ", self%tab_label
    call gtk_widget_set_halign(self%tab_label, GTK_ALIGN_START)


    self%ID_PLOTTYPE = ID_PLOTTYPE
    PRINT *, "ABOUT TO ASSIGN NOTEBOOK PTR"
    PRINT *, "NOTEBOOK PTR IS "
    self%notebook = parent_window

    self%box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);

    !call self%createCairoDrawingArea()
    call createCairoDrawingAreaForDraw(self%canvas, self%width, self%height, ID_PLOTTYPE)

    call self%settings%initialize()

 end subroutine

 subroutine addSpinBoxSetting(self, labelText, spinButton, callbackFunc, callbackData)
   implicit none

   class(zoatab) :: self
   character(len=*), intent(in) :: labelText
   type(c_funptr), intent(in)   :: callbackFunc
   type(c_ptr), optional, intent(in)   :: callbackData
   type(c_ptr), intent(in) :: spinButton

   call self%settings%addSpinBox(labelText, spinButton, callbackFunc, callbackData)

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


 subroutine finalizeWindow(self)
   use g
   implicit none
   class(zoatab) :: self

   type(c_ptr) :: scrolled_tab
    type(c_ptr) :: dcname, expander
    character(len=80) :: dname

   integer :: location
    PRINT *, "FINALIZING WINDOW in ZOATAB"

    !call self%buildSettings()
    expander = self%settings%build()
    ! We create a vertical box container:
    call gtk_box_append(self%box1, expander)
    call gtk_widget_set_vexpand (self%box1, FALSE)


    call gtk_box_append(self%box1, self%canvas)



    scrolled_tab = gtk_scrolled_window_new()
    PRINT *, "SETTING CHILD FOR SCROLLED TAB"
    call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
    location = gtk_notebook_append_page(self%notebook, scrolled_tab, self%tab_label)
    call gtk_notebook_set_current_page(self%notebook, location)


    call gtk_widget_set_name(self%box1, "TestLabel"//c_null_char)
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

  class(zoatab) :: self
  !type(c_ptr) :: parent_window
  PRINT *, "Dummy Function never used, only by children"

end subroutine


end module zoa_tab
