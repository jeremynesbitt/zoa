! These subs are mainly to avoid circular dependencies.
! This means the design is bad, but a more elegant solution has not been found
! or invested in yet.

! This method should eventually go somewhere else IMO, but for not have it here
  subroutine close_zoaTab(widget, gdata) bind(c)
    

  ! being specific in imports here because had some compiler errors during a clean
  ! build when I just used import handlers.  Never confirmed root cause, but suspect
  ! there was some isuse where I was importing handlers but it imported zoa_tab
  ! where the interface for this method was being defined
  use iso_c_binding
  use handlers, only: zoatabMgr
  !use gtk, only:  gtk_notebook_get_nth_page, gtk_notebook_get_current_page, gtk_widget_get_name, gtk_notebook_get_type
  use gtk
  use gtk_sup, only: convert_c_string, c_f_string


  implicit none

  type(c_ptr), value :: widget, gdata
  integer(kind=c_int) :: currPageIndex
  type(c_ptr) :: currPage
   character(len=50)  :: choice
 type(c_ptr)  :: cstr
 integer :: tabInfoToDelete
 character(len=80) :: fstring
 integer :: objIdx
 type(c_ptr) :: nbook


 if (c_associated(gdata)) then
   call LogTermDebug("Pointer defind!")
   call c_f_string(gdata, fstring)
  !call c_f_pointer(cptr, fstring)
  call LogTermDebug("Check that string is "//trim(fstring))   
  else 
   call LogTermDebug("Pointer not defind!")
  end if
 call LogTermDebug("Tab key is "//trim(fstring))

 objIdx = zoatabMgr%getTabIdxByID(trim(fstring))
 call zoatabMgr%removePlotTab(zoatabMgr%tabInfo(objIdx)%tabObj%tabNum, objIdx)




! Old way - doeesn't work for detached tabs.
!   currPageIndex = gtk_notebook_get_current_page(zoatabMgr%notebook)
!   currPage = gtk_notebook_get_nth_page(zoatabMgr%notebook, currPageIndex)


!   cstr =  gtk_widget_get_name(currPage)
!   call convert_c_string(cstr, choice)


!  read(choice(1:3), '(I3)') tabInfoToDelete
!  call zoatabMgr%removePlotTab(currPageIndex, tabInfoToDelete)

end subroutine

function getTabPlotCommand(objIdx) result(outStr)
  use handlers, only: zoatabMgr
  integer :: objIdx
  character(len=1040) :: outStr
  
  outStr = zoatabMgr%tabInfo(objIdx)%tabObj%psm%generatePlotCommand()
  !outStr = zoatabMgr%tabInfo(objIdx)%tabObj%plotCommand

end function

function getTabPlotCommandValue(objIdx, SETTING_CODE) result(realVal)
  use handlers, only: zoatabMgr
  integer, intent(in) :: objIdx, SETTING_CODE
  real :: realVal

  realVal = zoatabMgr%tabInfo(objIdx)%tabObj%psm%getSettingValueByCode(SETTING_CODE)

end function

function getSettingUIType(tabIdx, setting_code) result(uitype)
  use handlers, only: zoatabMgr
  use plot_setting_manager, only: zoaplot_setting_manager, UITYPE_SPINBUTTON

  implicit none
  integer :: uitype
  integer :: tabIdx
  integer :: setting_code
  integer :: i
  type(zoaplot_setting_manager) :: psm

   
  ! Just for readability
  uitype = -1 ! Default
  psm = zoatabMgr%tabInfo(tabIdx)%tabObj%psm  
  do i=1,psm%numSettings
    if (psm%ps(i)%ID == setting_code) then
        uitype = psm%ps(i)%uitype
        return
      end if
  end do

end function

function isSpinButtonInput(tabIdx, setting_code) result(boolResult)
  use handlers, only: zoatabMgr
  use plot_setting_manager, only: zoaplot_setting_manager, UITYPE_SPINBUTTON

  implicit none
  logical :: boolResult
  integer :: tabIdx
  integer :: setting_code
  integer :: i
  type(zoaplot_setting_manager) :: psm

  boolResult = .FALSE.  
  ! Just for readability
  psm = zoatabMgr%tabInfo(tabIdx)%tabObj%psm  
  do i=1,psm%numSettings
    if (psm%ps(i)%ID == setting_code) then
      if (psm%ps(i)%uitype == UITYPE_SPINBUTTON) then
        call LogTermFOR("Found SpinButton Type")
        boolResult = .TRUE.
        return
      end if
    end if
  end do

end function

function updateTabPlotCommand(tabIdx, setting_code, value) result (boolResult)
  use handlers, only: zoatabMgr
  use plot_setting_manager, only: zoaplot_setting_manager, updateWavelengthSetting
  use type_utils, only: int2str
  
  logical :: boolResult
  integer :: tabIdx
  integer :: setting_code
  class(*) :: value
  !double precision :: value
  integer :: i

  type(zoaplot_setting_manager) :: psm

  boolResult = .FALSE.

  !call LogTermFOR("In UPdatePlotCommand")

  ! Just for readability
  psm = zoatabMgr%tabInfo(tabIdx)%tabObj%psm

  !call LogTermFOR("NumZsetings is "//int2str(psm%numSettings))
  do i=1,psm%numSettings
  !call LogTermFOR("ID is "//int2str(psm%ps(i)%ID))
  !call LogTermFOR("Code is "//int2str(setting_code))    
    if (psm%ps(i)%ID == setting_code) then
      !TODO:  Add a way to send index to update setting since we are finding it twice
      ! the way this is written
      call zoatabMgr%tabInfo(tabIdx)%tabObj%psm%updateSetting(setting_code,value)
      boolResult = .TRUE.
      return 
    end if
  end do


end function


function getNumberOfTabs() result(numTabs)
  use handlers, only: zoatabMgr
  integer :: numTabs

  numTabs = zoatabMgr%tabNum

end function

function getTabName(tabNum) result(strName)
  use handlers, only: zoatabMgr
  character(len=100) :: strName
  integer :: tabNum

  strName = zoatabMgr%getTabTitle(tabNum)
end function

function isDocked(tabNum) result(boolResult)
  use handlers, only: zoatabMgr
  logical :: boolResult
  integer :: tabNum

  boolResult = zoatabMgr%tabInfo(tabNum)%tabObj%isDocked
  !boolResult = .TRUE.

end function

function getTabParentNotebook(tabIdx) result(parent_notebook)
  use iso_c_binding
  use handlers, only: zoatabMgr
  integer, intent(in) :: tabIdx
  type(c_ptr) :: parent_notebook

  parent_notebook = zoatabMgr%tabInfo(tabIdx)%tabObj%notebook

end function


subroutine updateMenuBar()
  use handlers, only: my_window
  use zoamenubar
  
  call populatezoamenubar(my_window)
  

  !call populateWindowMenu(my_window)

end subroutine


subroutine undock_Window(act, param, gdata) bind(c)
  use iso_c_binding
  use type_utils
  use handlers
  use gtk
  use zoa_tab
  implicit none
  type(c_ptr), value, intent(in) :: act, param, gdata
  !integer(kind=c_int), pointer :: tabNum
  integer(kind=c_int) :: tabNum

  type(c_ptr) :: pagetomove, childtomove

  type(c_ptr) :: newwin, newnotebook, box2, newpage
  type(c_ptr) :: newlabel, gesture, source, dropTarget, cptr
  integer :: newtab, i
  integer :: objIdx, TAB_NUM

  type(zoatab) :: tstTab
  character(len=100), pointer :: tabName
  type(c_ptr) ::  scrolled_win, child


  

  call c_f_pointer(gdata, tabName)

  objIdx = zoatabMgr%getTabIdxByID(trim(tabName))
  TAB_NUM = zoatabMgr%tabInfo(objIdx)%tabObj%tabNum

  call LogTermDebug("In UnDock Window")
  call LogTermDebug("TabNum is "//int2str(TAB_NUM))
  call LogTermDebug("Tab Title is "//trim(tabName))

  

  child = gtk_notebook_get_nth_page(zoatabMgr%notebook, TAB_NUM)
  child = g_object_ref(child)
  call gtk_notebook_remove_page(zoatabMgr%notebook, TAB_NUM)
  newwin = gtk_window_new()

  call gtk_window_set_default_size(newwin, 1200, 700)
  call gtk_window_set_destroy_with_parent(newwin, TRUE)
  box2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int);
  newnotebook = gtk_notebook_new()
  call gtk_widget_set_vexpand (newnotebook, TRUE)
  call gtk_notebook_set_group_name(newnotebook,"0"//c_null_char)


  do i=1,zoatabMgr%tabNum
    call LogTermDebug("ABout to check "//int2str(i))
 
    if(allocated(zoatabMgr%tabInfo(i)%tabObj)) then 
      call LogTermDebug("Before checking tabNum")   
      call LogTermDebug("Tab Num is "//int2str(zoatabMgr%tabInfo(i)%tabObj%tabNum))
      if(zoatabMgr%getTabTitle(i) == tabName) then
      !if(zoatabMgr%tabInfo(i)%tabObj%tabNum == TAB_NUM) then
        call LogTermDebug("Found object in zoaTabMgr")
        call LogTermDebug("i is "//int2str(i))
        newlabel = zoatabMgr%tabInfo(i)%tabObj%tab_label
        zoatabMgr%tabInfo(i)%tabObj%tabNum = gtk_notebook_append_page(newnotebook, child, newlabel)
        zoatabMgr%tabInfo(i)%tabObj%isDocked = .FALSE. 
        ! Need this to send to the dock window callback so it knows to remove the page
        zoatabMgr%tabInfo(i)%tabObj%notebook = newnotebook
        call LogTermDebug("New nbook ptr is"//int2str(INT(LOC(newnotebook))))

      end if
    end if

  end do

  call gtk_widget_set_halign(gtk_widget_get_parent(newlabel), GTK_ALIGN_START)
  call gtk_widget_set_hexpand(gtk_widget_get_parent(gtk_widget_get_parent(newlabel)),FALSE)

  call gtk_notebook_set_tab_detachable(newnotebook, child, TRUE)
  call gtk_box_append(box2, newnotebook)

  call gtk_window_set_child(newwin, box2)  
  call gtk_window_present(newwin)
  call updateMenuBar()



  source = gtk_drag_source_new ()
  call gtk_drag_source_set_actions (source, GDK_ACTION_MOVE);
  call g_signal_connect (source, "prepare"//c_null_char, c_funloc(drag_prepare), child);
  !call g_signal_connect (source, "drag-begin"//c_null_char, c_funloc(drag_end), child);        
  call g_signal_connect (source, "drag-end"//c_null_char, c_funloc(drag_end), child)
  call gtk_widget_add_controller (newnotebook, source);

  dropTarget = gtk_drop_target_new(gtk_widget_get_type(), GDK_ACTION_MOVE)
  call g_signal_connect(dropTarget, "drop", c_funloc(on_drop), c_null_ptr)
  call g_signal_connect(dropTarget, "motion", c_funloc(on_motion), c_null_ptr)

  call gtk_widget_add_controller(newnotebook, dropTarget)
  call gtk_widget_add_controller(gtk_widget_get_parent(newnotebook), dropTarget)




  gesture = gtk_gesture_click_new ()
  call g_signal_connect (gesture, "released", c_funloc(click_done), child);
  
  call gtk_widget_add_controller (child, gesture);       

  call zoatabMgr%updateTabPositions()


end subroutine

!For docking/undocking




subroutine dock_Window(act, param, parent_notebook) bind(c)
  use gtk
  use g
  use iso_c_binding
  use gtk_sup
  use handlers, only: zoatabMgr
  use type_utils
  implicit none
  type(c_ptr), value, intent(in) :: act, param, parent_notebook
  integer(kind=c_int) :: pageNum

  type(c_ptr) :: newwin, box2, scrolled_win, child
  type(c_ptr) :: newlabel, gesture, source, dropTarget
  integer :: newtab, i, tabNum
  type(c_ptr) :: cptr
  character(len=80) :: fstring
  ! For the detached tabs I only allow one, so the current page must be the page to remove
  !call gtk_notebook_remove_page(parent_notebook, gtk_notebook_get_current_page(parent_notebook))



  ! For the detached tabs I only allow one, so the current page must be the page to remove
  pageNum = gtk_notebook_get_current_page(parent_notebook)
  child = gtk_notebook_get_nth_page(parent_notebook, pageNum)
  child = g_object_ref(child)
  call gtk_notebook_remove_page(parent_notebook, pageNum)

  cptr = g_object_get_data(child, 'tab-id'//c_null_char)
  if (c_associated(cptr)) then
    call LogTermDebug("Pointer defind!")
    call c_f_string(cptr, fstring)
   !call c_f_pointer(cptr, fstring)
   call LogTermDebug("Check that string is "//trim(fstring))   
   else 
    call LogTermDebug("Pointer not defind!")
   end if
  call LogTermDebug("Tab ID is "//trim(fstring))

  tabNum = zoatabMgr%getTabIdxByID(trim(fstring))

  newtab = gtk_notebook_append_page(zoatabMgr%notebook, child, zoatabMgr%tabInfo(tabNum)%tabObj%tab_label)
  call gtk_notebook_set_current_page(zoatabMgr%notebook, newtab)
  zoatabMgr%tabInfo(tabNum)%tabObj%isDocked = .TRUE.
  zoatabMgr%tabInfo(tabNum)%tabObj%notebook = zoatabMgr%notebook
  zoatabMgr%tabInfo(tabNum)%tabObj%tabNum = newtab
  

  call gtk_notebook_set_tab_detachable(zoatabMgr%notebook, child, TRUE)

  call gtk_window_close(gtk_widget_get_ancestor(parent_notebook, gtk_window_get_type()))  
  call updateMenuBar()



end subroutine  

! subroutine undock_Window
!   implicit none
!   type(c_ptr), value :: widget, parent_notebook
!   type(c_ptr) :: newwin, newnotebook, box2, scrolled_win
!   type(c_ptr) :: child, newlabel, gesture, source, dropTarget
!   integer :: newtab
!   !PRINT *, "Detach Event Called!"

!   !PRINT *, "widget is ", widget
!   !PRINT *, "Parent Window is ", parent_notebook

!   newwin = gtk_window_new()

!   call gtk_window_set_default_size(newwin, 1300, 700)

!   !call gtk_window_set_transient_for(newwin, my_window)
!   call gtk_window_set_destroy_with_parent(newwin, TRUE)
!   box2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int);
!   newnotebook = gtk_notebook_new()
!   call gtk_widget_set_vexpand (newnotebook, TRUE)
!   !# handler for dropping outside of current window
!   !call hl_gtk_scrolled_window_add(newwin, newnotebook)

!   call gtk_notebook_set_group_name(newnotebook,"0"//c_null_char)
!   child = gtk_notebook_get_nth_page(parent_notebook, -1_c_int)
!   !newlabel = gtk_notebook_get_tab_label(parent_notebook, widget)
!   call gtk_notebook_detach_tab(parent_notebook, child)
!   scrolled_win = gtk_scrolled_window_new()
!   call gtk_scrolled_window_set_child(scrolled_win, child)

!   !newtab = gtk_notebook_append_page(newnotebook, widget, newlabel)
!   newtab = gtk_notebook_append_page(newnotebook, scrolled_win, newlabel)
!   call gtk_notebook_set_tab_detachable(newnotebook, scrolled_win, TRUE)
!   call gtk_box_append(box2, newnotebook)

!   call gtk_window_set_child(newwin, box2)

!   !call gtk_window_set_child(my_window, newwin)


!   !call gtk_window_set_child(newwin, widget)

!   call gtk_widget_set_vexpand (box2, TRUE)


!   !call gtk_widget_show(newwin)
!   !call gtk_widget_show(parent_notebook)
!   !call pending_events()
!   call gtk_window_present(newwin)

!   source = gtk_drag_source_new ()
!   call gtk_drag_source_set_actions (source, GDK_ACTION_MOVE);
!   call g_signal_connect (source, "prepare"//c_null_char, c_funloc(drag_prepare), scrolled_win);
!   !call g_signal_connect (source, "drag-begin"//c_null_char, c_funloc(drag_end), child);        
!   call g_signal_connect (source, "drag-end"//c_null_char, c_funloc(drag_end), child)
!   call gtk_widget_add_controller (newnotebook, source);

!   dropTarget = gtk_drop_target_new(gtk_widget_get_type(), GDK_ACTION_MOVE)
!   call g_signal_connect(dropTarget, "drop", c_funloc(on_drop), c_null_ptr)
!   call g_signal_connect(dropTarget, "motion", c_funloc(on_motion), c_null_ptr)

!   call gtk_widget_add_controller(newnotebook, dropTarget)
!   call gtk_widget_add_controller(gtk_widget_get_parent(newnotebook), dropTarget)




!   gesture = gtk_gesture_click_new ()
!   call g_signal_connect (gesture, "released", c_funloc(click_done), child);
  
!   call gtk_widget_add_controller (child, gesture);       

!   PRINT *, "Modal ? ", gtk_window_get_modal(newwin)
! end subroutine

! For docking/undocking


! subroutine registerPlotSettingManager(tabMgr, objIdx, psm)
!   use zoa_tab_manager, only: zoatabManager
!   use plot_setting_manager, only: zoaplot_setting_manager
!   integer :: objIdx
!   type(zoatabManager) :: tabMgr
!   type(zoaplot_setting_manager) :: psm



  
! end subroutine


! subroutine finalize_with_psm(self, objIdx, inputCmd)
!   use plot_setting_manager
!   use handlers, only: zoaTabMgr
!   use iso_c_binding, only: c_null_char
!   use type_utils, only: int2str
!   implicit none

!   character(len=*) :: inputCmd
!   integer :: objIdx
!   integer :: i
!   class(zoaplot_setting_manager) :: self


!   zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
!   do i=1,self%numSettings

!   select case (self%ps(i)%uitype)

!   case(UITYPE_SPINBUTTON)
!   call zoaTabMgr%tabInfo(objIdx)%tabObj%addSpinButton_runCommand_new( & 
!   & trim(int2str(self%ps(i)%ID)), self%ps(i)%default, self%ps(i)%min, self%ps(i)%max, 1, &
!   & trim(self%ps(i)%prefix))
!   !"Number of Field Points", &
!   !& 10.0, 1.0, 20.0, 1, "NUMPTS"//c_null_char)
!   !call zoaTabMgr%tabInfo(objIdx)%tabObj%addSpinButton_runCommand("Test2", 1.0, 0.0, 10.0, 1, "")

!   case(UITYPE_ENTRY)
!   call zoaTabMgr%tabInfo(objIdx)%tabObj%addEntry_runCommand( &
!   & self%ps(i)%label, self%ps(i)%defaultStr, trim(self%ps(i)%prefix))   

!   end select 
!   end do

!   ! This is going to have circular dependences even with an interface?
!   call registerPlotSettingManager(zoatabMgr, objIdx, self)
! end subroutine