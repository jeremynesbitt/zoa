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
