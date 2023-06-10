module zoa_glass_ui
      use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
      use gtk
      use gtk_hl_tree
      use gtk_hl_button
      use gtk_hl_entry
      use gtk_hl_dialog
      use global_widgets

      type(c_ptr) :: ihlist
      type(c_ptr) :: macrorun, macroedit, macrogroup, macrolist
      type(c_ptr) :: macrorename, macrocopy, macrodelete
      type(c_ptr) :: macroentry, ihwin
  contains

 recursive subroutine glass_select(list, gdata) bind(c)
    type(c_ptr), value, intent(in) :: list, gdata

    integer, pointer :: fdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    character(len=32) :: svalue

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)
       nsel = hl_gtk_list1_get_selections(C_NULL_PTR, selections, list)
       if (nsel == 0) return

       if (fdata == 0) then
          ! Find and print the selected row(s)
          print *, nsel,"Rows selected"
          print *, selections
          call hl_gtk_list1_get_cell(ihlist, selections(1), svalue)
          print *, "TXT IS ", svalue
          deallocate(selections)

          ! Get text to display in secend window
          call ioConfig%setTextBuffer(ID_TERMINAL_GLASS)
          call hl_gtk_text_view_delete(c_null_ptr, buffer=ioConfig%txtBuffer)
          CALL PROCESKDP('GLASSP '//svalue)
          call ioConfig%setTextBuffer(ID_TERMINAL_DEFAULT)


          ! Get index at wavelengths of interest
          ! GLSWV

       else    ! Delete the selected row
          !call gtk_check_button_set_active(dbut, FALSE)
          fdata = 0
          !print *, "Delete row:", selections(1)
          !call hl_gtk_list1_rem(ihlist, selections(1))
       end if
    end if

  end subroutine

  subroutine populateglasslist()

    character(len=32) :: line
    integer :: i, ltr, NUMINLIST
    CHARACTER(len=8), dimension(1024) :: CATLIST
    CHARACTER(len=50), dimension(9999) :: CATARRAY
    call hl_gtk_list1_rem(ihlist)
    ! Now put 10 rows into it
    CALL GCATLOAD(NUMINLIST, CATARRAY)
    !CALL MACARRAY_LOAD(NUMINLIST, MACARRAY)
    PRINT *, "NUMINLIST IS ", NUMINLIST
    do i=1,NUMINLIST-1

       line = CATARRAY(i)
       call hl_gtk_list1_ins(ihlist, line//c_null_char)

    end do


  end subroutine

  subroutine glassui_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Exit called"
    call gtk_window_destroy(gdata)
  end subroutine

    subroutine glassmanagerUI(act, param, win) bind(c)

      implicit none
      type(c_ptr), value, intent(in) :: act, param, win
      type(c_ptr) :: base, ihscrollcontain, jbox, jbox2, abut, qbut
      type(c_ptr) :: macroentrylabel, textView, buffer

      integer, target :: iappend=0, idel=0
      integer :: ltr


      ! Create the window:
      ihwin = gtk_window_new()
      call gtk_window_set_title(ihwin, "Glass Manager"//c_null_char)

      call gtk_window_set_default_size(ihwin, 300_c_int, 600_c_int)

      ! Now make a column box & put it into the window
      base = hl_gtk_box_new()
      call gtk_window_set_child(ihwin, base)

    ! Now make a single column list with multiple selections enabled

    ihlist = hl_gtk_list1_new(ihscrollcontain, changed=c_funloc(glass_select),&
         & data=c_loc(idel), multiple=FALSE, height=400_c_int, &
         & title="List of Available Glasses"//c_null_char)

    call populateglasslist()

    textView = gtk_text_view_new ()
    call gtk_text_view_set_editable(textView, FALSE)
        !
    buffer = gtk_text_view_get_buffer (textView)
    call ioConfig%registerTextBuffer(buffer, ID_TERMINAL_GLASS)

    ! It is the scrollcontainer that is placed into the box.
    call hl_gtk_box_pack(base, ihscrollcontain)
    call gtk_box_append(base, textView)
    ! macrorun = gtk_check_button_new_with_label("Run"//c_null_char)
    ! macroedit = gtk_check_button_new_with_label("Edit"//c_null_char)
    ! macrolist = gtk_check_button_new_with_label("List"//c_null_char)
    ! macrorename = gtk_check_button_new_with_label("Rename"//c_null_char)
    ! macrocopy = gtk_check_button_new_with_label("Copy"//c_null_char)
    ! macrodelete = gtk_check_button_new_with_label("Delete"//c_null_char)

    ! macrogroup = gtk_window_group_new()
    ! call gtk_check_button_set_group(macrorun, macroedit)
    ! call gtk_check_button_set_group(macrolist, macrorun)
    ! call gtk_check_button_set_group(macrorename, macrorun)
    ! call gtk_check_button_set_group(macrocopy, macrorun)
    ! call gtk_check_button_set_group(macrodelete, macrorun)
    !
    ! call gtk_check_button_set_active(macrorun, 1_c_int)
    !
    !
    ! !call gtk_check_button_set_group(macroedit, macrogroup)
    !
    ! call hl_gtk_box_pack(base, macrorun)
    ! call hl_gtk_box_pack(base, macroedit)
    ! call hl_gtk_box_pack(base, macrolist)
    ! call hl_gtk_box_pack(base, macrorename)
    ! call hl_gtk_box_pack(base, macrocopy)
    ! call hl_gtk_box_pack(base, macrodelete)

     !macroentrylabel = gtk_label_new("Target Name for Macro rename or copy"//c_null_char)

    ! macroentry = hl_gtk_entry_new(editable=TRUE)
     !call hl_gtk_box_pack(base, macroentrylabel)
     !call hl_gtk_box_pack(base, macroentry)






    ! Make row box put it in the column box and put an editable
    ! 1-line text widget and a button in it
    !jbox = hl_gtk_box_new(horizontal=TRUE)
    !call hl_gtk_box_pack(base, jbox)

    ! abut = hl_gtk_button_new("Run Macro"//c_null_char, clicked=c_funloc(macrorun_click),&
    !      & data=c_loc(iappend))
    ! call hl_gtk_box_pack(jbox, abut)


    ! Also a quit button
    ! qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(macroui_destroy), &
    ! & data=ihwin)
    ! call hl_gtk_box_pack(base,qbut)

    ! realize the window
    call gtk_widget_show(ihwin)

    end subroutine

end module
