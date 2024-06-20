! These subs are mainly to avoid circular dependencies.
! This means the design is bad, but a more elegant solution has not been found
! or invested in yet.

! This method should eventually go somewhere else IMO, but for not have it here
subroutine close_zoaTab()
  ! being specific in imports here because had some compiler errors during a clean
  ! build when I just used import handlers.  Never confirmed root cause, but suspect
  ! there was some isuse where I was importing handlers but it imported zoa_tab
  ! where the interface for this method was being defined
  use iso_c_binding
  use handlers, only: zoatabMgr
  use gtk, only:  gtk_notebook_get_nth_page, gtk_notebook_get_current_page, gtk_widget_get_name
  use gtk_sup, only: convert_c_string


  implicit none

  integer(kind=c_int) :: currPageIndex
  type(c_ptr) :: currPage
   character(len=50)  :: choice
 type(c_ptr)  :: val, cstr
 integer :: tabInfoToDelete

  PRINT *, "Button clicked!"
  !PRINT *, "Test of accessing zoa tab manager ", zoatabMgr%tabNum
  currPageIndex = gtk_notebook_get_current_page(zoatabMgr%notebook)
  currPage = gtk_notebook_get_nth_page(zoatabMgr%notebook, currPageIndex)

  !val = c_loc(result)
  cstr =  gtk_widget_get_name(currPage)

  !cstr = g_value_get_string(val)
  call convert_c_string(cstr, choice)

 !PRINT *, "CHOICE is ", choice
 read(choice(1:3), '(I3)') tabInfoToDelete
 !PRINT *, "After int conversion, ", tabInfoToDelete

 ! Close Tab
 call zoatabMgr%removePlotTab(currPageIndex, tabInfoToDelete)

end subroutine

function getTabPlotCommand(objIdx) result(outStr)
  use handlers, only: zoatabMgr
  integer :: objIdx
  character(len=1040) :: outStr
  
  outStr = zoatabMgr%tabInfo(objIdx)%tabObj%psm%generatePlotCommand()
  !outStr = zoatabMgr%tabInfo(objIdx)%tabObj%plotCommand

end function

function updateTabPlotCommand(tabIdx, setting_code, value) result (boolResult)
  use handlers, only: zoatabMgr
  use plot_setting_manager, only: zoaplot_setting_manager, updateWavelengthSetting_new
  use type_utils, only: int2str
  
  logical :: boolResult
  integer :: tabIdx
  integer :: setting_code
  real :: value
  integer :: i
  type(zoaplot_setting_manager) :: psm

  boolResult = .FALSE.

  call LogTermFOR("In UPdatePlotCommand")

  ! Just for readability
  psm = zoatabMgr%tabInfo(tabIdx)%tabObj%psm

  call LogTermFOR("NumZsetings is "//int2str(psm%numSettings))
  do i=1,psm%numSettings
  call LogTermFOR("ID is "//int2str(psm%ps(i)%ID))
  call LogTermFOR("Code is "//int2str(setting_code))    
    if (psm%ps(i)%ID == setting_code) then
      call LogTermFOR("Found proper setting!")
      call zoatabMgr%tabInfo(tabIdx)%tabObj%psm%updateWavelengthSetting_new(INT(value))
      boolResult = .TRUE.
    end if

  end do


end function


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