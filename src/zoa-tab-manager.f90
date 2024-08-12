! This module is reponsible for all of the analysis windows, whether plots or pure
! text.  
module zoa_tab_manager
  use zoa_ui
  use zoa_tab
  use gtk
  use gtk_hl_container
  use collections


! Maybe this should go away and be replaced by just tabObj
! should settings be a "child" of zoatab?    
type zoatabData
  !integer :: typeCode
  class(zoatab), allocatable :: tabObj

end type


!Purpose
! Keep track of all tabs that are open. 
! Tabs are any analysis that is done that is shown in a separate window
! Handle any requests related to tabs (whether a plot exists, close a tab, etc)
type  zoatabManager

  type(zoatabData), dimension(99) :: tabInfo
  type(c_ptr) :: buffer
  type(c_ptr) :: textView
  integer :: tabNum = 0
  type(c_ptr) :: notebook

 contains

   procedure :: addMsgTab
   procedure :: rePlotIfNeeded
   procedure :: removePlotTab
   procedure :: doesPlotExist
   procedure :: doesPlotExist_new
   procedure :: getNumberOfPlotsByCode
   procedure :: addMultiPlotTab
   procedure :: addDataTab
   procedure :: addGenericMultiPlotTab ! May be obsolete with tweak to init process
   procedure :: updateGenericMultiPlotTab
   procedure :: finalizeNewPlotTab
   procedure :: updateInputCommand
   procedure :: findTabIndex
   procedure :: closeAllTabs
   procedure :: finalize_with_psm

   !Support for KDP (now only VIE) plot
   procedure :: addKDPPlotTab
   procedure :: setKDPCallback
   procedure :: updateKDPPlotTab

   procedure :: updateUISettingsIfNeeded
   procedure :: getWidgetBySettingCode

   procedure :: getTypeCode
   procedure :: clearDataTab
   

 end type



contains

function getTypeCode(self, idx) result (TYPE_CODE)
  class(zoatabManager) :: self
  integer :: TYPE_CODE
  !TYPE_CODE = self%tabInfo(idx)%typeCode
  TYPE_CODE = self%tabInfo(idx)%tabObj%ID_PLOTTYPE
end function

! This doubles as an init routine
subroutine addMsgTab(self, notebook, winTitle)

    class(zoatabManager) :: self
    type(c_ptr) :: notebook
    character(len=*) :: winTitle

    self%notebook = notebook

    !self%textView = gtk_text_view_new ()
    !call gtk_text_view_set_editable(self%textView, FALSE)

    !self%buffer = gtk_text_view_get_buffer (self%textView)
    !call gtk_text_buffer_set_text (self%buffer, &
    !    & "ZOA Log Message Window"//C_NEW_LINE//c_null_char,&
    !    & -1_c_int)
    !scrollWin = gtk_scrolled_window_new()
    !label = gtk_label_new(winTitle//c_null_char)

    !call gtk_scrolled_window_set_child(scrollWin, self%textView)
    !tabPos = gtk_notebook_append_page (self%notebook, scrollWin, label)
    !PRINT *, "Notebook ptr is ", self%notebook


end subroutine

subroutine clearDataTab(self, objIdx)
  use gtk_hl_entry, only: hl_gtk_text_view_delete
  implicit none
  class(zoatabManager) :: self
  integer :: objIdx
  type(zoaplotdatatab) :: tmpTab
  type(c_ptr) :: buffer

  select type (tmpTab => self%tabInfo(objIdx)%tabObj)
  type is (zoaplotdatatab)
     buffer = gtk_text_view_get_buffer(tmpTab%textView)
     call hl_gtk_text_view_delete(c_null_ptr, buffer=buffer) 
     type is (zoadatatab)
     buffer = gtk_text_view_get_buffer(tmpTab%textView)
     call hl_gtk_text_view_delete(c_null_ptr, buffer=buffer)          
end select

end subroutine

function  findTabIndex(self) result(newTabIndex)
  class(zoatabManager) :: self
  integer :: i
  integer :: newTabIndex

  do i=1,self%tabNum
    if (.not.ALLOCATED(self%tabInfo(i)%tabObj)) then
        !PRINT *, "Found empty slot at ", i
        newTabIndex = i
        return
      end if
  end do

  ! If we get here then add at the end
  !PRINT *, "Increment max number of tabs"
  self%tabNum = self%tabNum + 1
  newTabIndex = self%tabNum

end function

!TODO:  Combine this with finalizewithpsm at some point
subroutine finalizeNewPlotTab(self, idx)
    class(zoatabManager) :: self
    integer :: idx

    type(c_ptr) :: currPage
    integer(kind=c_int) :: currPageIndex
    character(len=3) :: outChar
    type(zoatab) :: tmpTab

    !TODO:  Fix this - don't like how this flat is set secretly for lens draw
    select type(tmpTab => self%tabInfo(idx)%tabObj)
    type is (zoaplottab) 
    call tmpTab%finalizeWindow(tmpTab%useToolbar)
    type is (zoaplotdatatab) 
    call tmpTab%finalizeWindow(tmpTab%useToolbar)    
    type is (zoadatatab) 
    call tmpTab%finalizeWindow(tmpTab%useToolbar)    
        
  end select    
    


    ! This part is to enable close tab functionality
    currPageIndex = gtk_notebook_get_current_page(self%notebook)
    currPage = gtk_notebook_get_nth_page(self%notebook, currPageIndex)
    WRITE(outChar, '(I0.3)') idx
    call gtk_widget_set_name(currPage, outChar//c_null_char)


end subroutine

function addDataTab(self, PLOT_CODE, tabTitle) result(idx)
  class(zoatabManager) :: self
  integer, intent(in) :: PLOT_CODE
  character(len=*), intent(in) :: tabTitle
  integer :: idx

  idx = self%findTabIndex()

  allocate(zoadatatab :: self%tabInfo(idx)%tabObj)
  call self%tabInfo(idx)%tabObj%initialize(self%notebook, tabTitle, PLOT_CODE, c_null_ptr)

end function

! Added this so I could separate out initialization of the UI and creation of the multiplot object
! So I can register textViews to print data.
function addMultiPlotTab(self, PLOT_CODE, tabTitle) result(idx)
  class(zoatabManager) :: self
  integer, intent(in) :: PLOT_CODE
  character(len=*), intent(in) :: tabTitle
  integer :: idx

  idx = self%findTabIndex()

  allocate(zoaplotdatatab :: self%tabInfo(idx)%tabObj)
  call self%tabInfo(idx)%tabObj%initialize(self%notebook, tabTitle, PLOT_CODE, c_null_ptr)

end function

! I think this is where I should separate out the different tab types
! instead of allocating a zoatab, allocate a zoaplottab
! everything else could stay the same
! but then I can separate out the plot elements from zoatab type
! so I can add other types (eg a window with just text output)
function addGenericMultiPlotTab(self, PLOT_CODE, tabTitle, mplt) result(idx)
  class(zoatabManager) :: self
  integer :: PLOT_CODE
  character(len=*) :: tabTitle
  type(multiplot) :: mplt
  integer :: idx
  type(zoatab) :: tstTab

  idx = self%findTabIndex()

  allocate(zoaplotdatatab :: self%tabInfo(idx)%tabObj)
  call self%tabInfo(idx)%tabObj%initialize(self%notebook, tabTitle, PLOT_CODE, mplt%area)
  ! As far as I can tell, this is required to access a sub that is not in the parent type.  
  select type(tstTab => self%tabInfo(idx)%tabObj)
  type is (zoaplottab)
  !call self%tabInfo(idx)%tabObj%createGenericMultiPlot(mplt)
  call tstTab%createGenericMultiPlot(mplt)
  type is (zoaplotdatatab)
  !call self%tabInfo(idx)%tabObj%createGenericMultiPlot(mplt)
  call tstTab%createGenericMultiPlot(mplt)  
  ! I don't think this should be needed since it is assigned in intiialize, but maybe I added this
  ! for a reason previously?
  !tstTab%canvas = mplt%area
end select

end function

subroutine setKDPCallback(self, idx, tabIndex)
  use ROUTEMOD
  !use kdp_draw, only: DRAWOPTICALSYSTEM
  implicit none 

  class(zoatabManager) :: self
  type(zoatab) :: tmpTab
  integer, target, intent(in) :: tabIndex
  integer :: idx

  integer, pointer :: ptr

   ptr =>tabIndex

   select type (tmpTab=>self%tabInfo(idx)%tabObj)
   type is (zoaplottab)
   call gtk_drawing_area_set_draw_func(tmpTab%canvas, &
   & c_funloc(ROUTEDRAWING), c_loc(ptr), c_null_funptr) 
end select

end subroutine

! Support the KDP way of plotting
function addKDPPlotTab(self, PLOT_CODE, tabTitle) result(idx)
  use kdp_draw, only: DRAWOPTICALSYSTEM
  use type_utils, only: int2str
  
  implicit none
  class(zoatabManager) :: self
  type(zoatab) :: tmpTab
  integer :: PLOT_CODE

  character(len=*) :: tabTitle
  integer :: idx  
  !integer, target :: tabIdx


  idx = self%findTabIndex()

  !PRINT *, "idx is ", idx
  call logger%logText('New Generic Tab Starting')

  call LogTermFOR("Setting up new KDP tab for tab idx "//int2str(idx))
  allocate(zoaplottab :: self%tabInfo(idx)%tabObj)
  call self%tabInfo(idx)%tabObj%initialize(self%notebook, tabTitle, PLOT_CODE)
  !call gtk_drawing_area_set_draw_func(self%tabInfo(idx)%tabObj%canvas, &
  !& c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_LENSDRAW), c_null_funptr)  
  !ptr =tabIndices(idx)

   call self%setKDPCallback(idx, tabIndices(idx))
  !  if(idx==1) then
  ! call gtk_drawing_area_set_draw_func(self%tabInfo(idx)%tabObj%canvas, &
  ! & c_funloc(ROUTEDRAWING), c_loc(TARGET_LENSDRAW), c_null_funptr) 
  !  else
  !   call gtk_drawing_area_set_draw_func(self%tabInfo(idx)%tabObj%canvas, &
  !   & c_funloc(ROUTEDRAWING), c_loc(TARGET_TST), c_null_funptr) 
  !  end if
 
  select type (tmpTab => self%tabInfo(idx)%tabObj)
  type is (zoaplottab)
    tmpTab%useToolbar = .TRUE.
  end select


  !call gtk_widget_queue_draw(self%tabInfo(idx)%canvas)
  
end function

subroutine updateKDPPlotTab(self, idx)
  implicit none
  class(zoatabManager) :: self
  integer :: idx
  type(zoaplottab) :: tmpTab

  call self%updateUISettingsIfNeeded(idx)
  select type (tmpTab=>self%tabInfo(idx)%tabObj)
  type is (zoaplottab)
  call gtk_widget_queue_draw(tmpTab%canvas)
end select


end subroutine

! It's possible the user via CLA, or another process (eg toolbar), updated settings 
! between the time the plot was created and it is redrawn.  
! This sub is to check for any differences and resolve them if needed.
  !Key assumption here is that the master settings are always stored in the psm
subroutine updateUISettingsIfNeeded(self, tabIdx)
  implicit none
  class(zoatabManager) :: self
  type(zoaplot_setting_manager) :: psm
  integer :: tabIdx
  logical :: boolResult
  type(c_ptr) :: widget

  integer :: i

  psm = self%tabInfo(tabIdx)%tabObj%psm

  do i=1,psm%numSettings
    widget = self%getWidgetBySettingCode(tabIdx, psm%ps(i)%ID)
    if(c_associated(widget)) then
      boolResult = isUISettingDifferent(widget, psm%ps(i))
    end if

  end do
  

end subroutine

function isUISettingDifferent(widget, ps) result(boolResult)
  use hl_gtk_zoa
  use type_utils, only: int2str
  implicit none
  type(c_ptr) :: widget
  type(plot_setting) :: ps
  logical :: boolResult
  real :: uiValue
  type(c_ptr) :: buff
  character(len=1024) :: ffieldstr

  boolResult = .FALSE.


  select case (ps%uitype)

  case(UITYPE_SPINBUTTON)
    uiValue = gtk_spin_button_get_value (widget)
    if (uiValue /= ps%default) then
      boolResult = .TRUE.
        call gtk_spin_button_set_value(widget, real(ps%default,8))
    end if

  case(UITYPE_ENTRY)
    buff = gtk_entry_get_buffer(widget)
    call c_f_string_copy(gtk_entry_buffer_get_text(buff), ffieldstr)     
    if (trim(ffieldstr) /= ps%defaultStr) then 
      boolResult = .TRUE.
      call gtk_entry_buffer_set_text(buff, ps%defaultStr//c_null_char, -1)
      call gtk_entry_set_buffer(widget, buff)
    end if

  case(UITYPE_COMBO)
    if ((hl_zoa_combo_get_selected_list2_id(widget)) /= INT(ps%default)) then
      boolResult = .TRUE.
      call hl_zoa_combo_set_selected_by_list2_id(widget, INT(ps%default))
    end if

  case(UITYPE_TOOLBAR)
    !Do nothing.  THis will be drawn separately
  end select 



end function

function getWidgetBySettingCode(self, tabIdx, SETTING_CODE) result(widget)
  implicit none
  class(zoatabManager) :: self
  integer :: tabIdx
  type(c_ptr) :: widget
  integer :: SETTING_CODE
  integer :: i

  widget = c_null_ptr
  do i=1,self%tabInfo(tabIdx)%tabObj%settings%numSettings
    if (SETTING_CODE == self%tabInfo(tabIdx)%tabObj%settings%settingCodes(i)) then
        widget = self%tabInfo(tabIdx)%tabObj%settings%settingobj_get(i,1)
        return
     end if
  end do

end function

subroutine updateGenericMultiPlotTab(self, objIdx, mplt)
  implicit none
  class(zoatabManager) :: self
  type(multiplot) :: mplt
  type(zoatab) :: tmpTab
  integer :: objIdx

  select type (tmpTab =>self%tabInfo(objIdx)%tabObj )
  type is (zoaplottab)
  call tmpTab%updateGenericMultiPlot(mplt)
  type is (zoaplotdatatab)
  call tmpTab%updateGenericMultiPlot(mplt)  
  end select
end subroutine


 function doesPlotExist(self, PLOT_CODE, idxObj) result(plotFound)
   class(zoatabManager) :: self
   integer, intent(in) :: PLOT_CODE
   logical :: plotFound
   integer, intent(inout) :: idxObj
   integer :: i

    !PRINT *, "Searching for existing plot... with plot code ", PLOT_CODE
    plotFound = .FALSE.
    idxObj = -1

    DO i = 1,self%tabNum
      if(self%getTypeCode(i) == PLOT_CODE) THEN
          idxObj = i
         plotFound = .TRUE.
         tabPos = i

       end if

    END DO
    !PRINT *, "After search, plotFound is ", plotFound

 end function

 function doesPlotExist_new(self, PLOT_CODE, idxObj, plotNum) result(plotFound)
  implicit none
  class(zoatabManager) :: self
  integer, intent(in) :: PLOT_CODE
  logical :: plotFound
  integer, intent(inout) :: idxObj
  integer, intent(in) :: plotNum
  integer :: i

   !PRINT *, "Searching for existing plot... with plot code ", PLOT_CODE
   plotFound = .FALSE.
   idxObj = -1
   DO i = 1,self%tabNum
     if(self%getTypeCode(i) == PLOT_CODE) THEN
         if (self%tabInfo(i)%tabObj%psm%plotNum ==plotNum) then
         idxObj = i
        plotFound = .TRUE.
      end if
    end if

   END DO

end function

function getNumberOfPlotsByCode(self, PLOT_CODE) result(numPlots)
  implicit none
  class(zoatabManager) :: self
  integer, intent(in) :: PLOT_CODE
  integer :: numPlots
  integer :: i 

  numPlots = 0
  DO i = 1,self%tabNum
   if(self%getTypeCode(i) == PLOT_CODE) THEN
      numPlots = numPlots + 1
    end if
  END DO


end function

  subroutine rePlotIfNeeded(self)
    implicit none
    !use codeV_commands, only: cmd_loop, DRAW_LOOP

     class(zoatabManager) :: self
     integer :: i

     call LogTermFOR("About to check replot for each tab")
     ! Do nothing if there are no tabs to cycle through
      
     DO i = 1,self%tabNum
           ! Not keeping track of tabs when it is closed, so as a 
           ! hack add this.  TODO:  Fix this properly
           if (allocated(self%tabInfo(i)%tabObj)) then
              call LogTermFOR("About to call replot cmd " &
              & //trim(self%tabInfo(i)%tabObj%plotCommand))
              call PROCESKDP(self%tabInfo(i)%tabObj%plotCommand)
          end if
     END DO


  end subroutine

  subroutine removePlotTab(self, tabIndex, tabInfoIndex)

    class(zoatabManager) :: self
    integer, intent(in) :: tabIndex, tabInfoIndex


    call gtk_notebook_remove_page(self%notebook, tabIndex)
    !PRINT *, "typeCode is ", self%tabInfo(tabInfoIndex)%typeCode
    !PRINT *, "About to deallocate tabObj for index ", tabInfoIndex
    !PRINT *, "allocated test ", allocated(self%tabInfo(tabInfoIndex)%tabObj)
    if (allocated(self%tabInfo(tabInfoIndex)%tabObj)) then
       DEALLOCATE(self%tabInfo(tabInfoIndex)%tabObj)
    end if
  

  end subroutine

  subroutine updateInputCommand(self, objIdx, inputCmd)
    use global_widgets, only: uiSettingCommands, uiSetCmdsIdx
    implicit none

    class(zoatabManager) :: self
    integer, intent(in) :: objIdx
    character(len=*) :: inputCmd
    character(len=150) :: cmdOnly

    integer :: i, cmdLoc, tokLoc

    self%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

     cmdLoc = index(inputCmd, ",")
     if (cmdLoc.GT.0) cmdOnly = inputCmd(1:cmdLoc-1)

    do i=1,uiSetCmdsIdx
      cmdLoc = index(uiSettingCommands(i), trim(cmdOnly))
      if (cmdLoc.GT.0) then
         !PRINT *, "Update command ", uiSettingCommands(i)! //" to "//inputCmd
         tokLoc = index(uiSettingCommands(i), "--")

         uiSettingCommands(i) = uiSettingCommands(i)(1:tokLoc+1)//trim(inputCmd)
         !PRINT *, "New Command is ", uiSettingCommands(i)
         return
       end if
    end do


  end subroutine

  subroutine closeAllTabs(self, dialogTxt)
    use gtk_hl_dialog
    implicit none
    class(zoatabManager) :: self
    character(len=*) :: dialogTxt
    integer :: i, resp
    character(len=80), dimension(3) :: msg

    msg(1) ="You are about to open a new lens system"
    msg(2) = "This will invalidate all open plots"
    msg(3) = "Do you want to close all plots?"   

    if (self%tabNum.EQ.0) return

    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
         & "Warning"//c_null_char)
    if (resp == GTK_RESPONSE_YES) then

       
      ! This part is sketchy as if the way tabs are handled changes in the
      ! future this could break.  The logic here is to set the first tab 
      ! and delete the objects associated with that tab.  Since 
      ! objects and tabs start out at a 1:1 correspondence, doing this 
      ! tabNum times should guarantee all open plots are closed.
      ! A more robust way if this causes proglems might be to clear 
      ! all tabObjs and close all tabs independently
      do i=1,self%tabNum
        call gtk_notebook_set_current_page(self%notebook, 1)
        call self%removePlotTab(1,i) ! This is dangerous to assume both indexes are the same.  TODO:  Need to look into this.
      end do
      self%tabNum = 0
    end if


  end subroutine

subroutine finalize_with_psm(self, objIdx, psm, inputCmd)
  use iso_c_binding, only: c_null_char
  use type_utils, only: int2str
  use plot_setting_manager, only: zoaplot_setting_manager
  implicit none

  character(len=*), optional :: inputCmd
  integer :: objIdx
  integer :: i
  class(zoatabManager) :: self
  type(zoaplot_setting_manager) :: psm


  if(present(inputCmd)) self%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

  do i=1,psm%numSettings

  select case (psm%ps(i)%uitype)

  case(UITYPE_SPINBUTTON)
    call self%tabInfo(objIdx)%tabObj%addSpinButtonFromPS(psm%ps(i))
      
  !  call self%tabInfo(objIdx)%tabObj%addSpinButton_runCommand_new( & 
  !  & trim(psm%ps(i)%label), psm%ps(i)%default, psm%ps(i)%min, psm%ps(i)%max, 3_c_int, &
  !  & trim(int2str(psm%ps(i)%ID)))

  case(UITYPE_ENTRY)
  call self%tabInfo(objIdx)%tabObj%addEntry_runCommand( &
  & psm%ps(i)%label, psm%ps(i)%defaultStr, trim(int2str(psm%ps(i)%ID)), psm%ps(i)%ID)   

  case(UITYPE_COMBO)
    call self%tabInfo(objIdx)%tabObj%addListBox_new(trim(psm%ps(i)%label), &
    & psm%ps(i)%set, c_funloc(callback_runCommandFromSpinBox_new), &
    & self%tabInfo(objIdx)%tabObj%box1, &
    & defaultSetting=INT(psm%ps(i)%default), winName=trim(int2str(psm%ps(i)%ID)), SETTING_CODE=psm%ps(i)%ID)
    ! call self%tabInfo(objIdx)%tabObj%addListBoxSettingTextID_new(psm%ps(i)%label, &
    ! & psm%ps(i)%set, c_funloc(callback_combo_listID), self%tabInfo(objIdx)%tabObj%box1)
    

  end select 
  end do

  call registerPlotSettingManager(self, objIdx, psm)

end subroutine

subroutine registerPlotSettingManager(tabMgr, objIdx, psm)
  use plot_setting_manager, only: zoaplot_setting_manager
  use plot_setting_manager, only: zoaplot_setting_manager
  integer :: objIdx
  type(zoatabManager) :: tabMgr
  type(zoaplot_setting_manager) :: psm

  tabMgr%tabInfo(objIdx)%tabObj%psm = psm
  
end subroutine


end module
