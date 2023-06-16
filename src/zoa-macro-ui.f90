module zoa_macro_ui
      use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
      use gtk
      use gtk_hl_tree
      use gtk_hl_button
      use gtk_hl_entry
      use gtk_hl_dialog
      use GLOBALS
      use global_widgets

      type(c_ptr) :: ihlist
      type(c_ptr) :: macrorun, macroedit, macrogroup, macrolist
      type(c_ptr) :: macrorename, macrocopy, macrodelete
      type(c_ptr) :: macroentry, ihwin
  contains

 recursive subroutine macrolist_select(list, gdata) bind(c)
    type(c_ptr), value, intent(in) :: list, gdata

    integer, pointer :: fdata
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    type(c_ptr) :: buffer
    character(len=8) :: svalue

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
          call ioConfig%setTextView(ID_TERMINAL_MACRO)
          buffer = gtk_text_view_get_buffer(ioConfig%textView)
          call hl_gtk_text_view_delete(c_null_ptr, buffer=buffer)
          CALL PROCESKDP('MFL '//svalue)
          call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
          deallocate(selections)

       else    ! Delete the selected row
          !call gtk_check_button_set_active(dbut, FALSE)
          fdata = 0
          !print *, "Delete row:", selections(1)
          !call hl_gtk_list1_rem(ihlist, selections(1))
       end if
    end if

  end subroutine

  subroutine macrorun_click(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    integer, pointer :: fdata
    type(c_ptr) :: buffer
    integer(c_int16_t) :: ntext
    character(kind=c_char, len=100) :: ftext
    integer(kind=c_int) :: nsel
    integer(kind=c_int), dimension(:), allocatable :: selections
    character(len=8) :: svalue
    character(len=100) :: entryText

    if (c_associated(gdata)) then
       call c_f_pointer(gdata, fdata)

       nsel = hl_gtk_list1_get_selections(ihlist, selections)
       if (nsel == 0) return

       if (fdata == 0) then
              PRINT *, "RUN SELECTED!"
              ! Find and print the selected row(s)
              print *, nsel,"Rows selected"
              print *, selections
              call hl_gtk_list1_get_cell(ihlist, selections(1), svalue)
              print *, "TXT IS ", svalue
              deallocate(selections)

          if (gtk_check_button_get_active(macrorun).EQ.TRUE) THEN
              CALL PROCESKDP(svalue)
              !call PROCESKDP('MREFRESH')
          else if (gtk_check_button_get_active(macrolist).EQ.TRUE) THEN
               CALL PROCESKDP('MFL '//svalue)

          else if (gtk_check_button_get_active(macrodelete).EQ.TRUE) THEN
                call getmacroeditboxText(entryText)
                CALL PROCESKDP('MDEL '//svalue)
                call populatemacrolist()

          else if (gtk_check_button_get_active(macrorename).EQ.TRUE) THEN
                call getmacroeditboxText(entryText)
                CALL PROCESKDP('MRENAME '//svalue//' '//entryText(1:8))
                call populatemacrolist()
                !CALL MACARRAY_LOAD(NUMINLIST)
          else if (gtk_check_button_get_active(macroedit).EQ.TRUE) THEN
                !call getmacroeditboxText(entryText)
                CALL PROCESKDP('MEDIT '//svalue)
                print *, "About to call alert dialog"
                call macroedit_alert()
                print *, "Dialog Closed!"
                call PROCESKDP('MREFRESH')
                call populatemacrolist()
              !call PROCESKDP('MREFRESH')
          else if (gtk_check_button_get_active(macrocopy).EQ.TRUE) THEN
                call getmacroeditboxText(entryText)
                CALL PROCESKDP('MCOPY '//svalue//' '//entryText(1:8))
                call populatemacrolist()
        end if

       else    ! Delete the selected row
          !call gtk_check_button_set_active(dbut, FALSE)
          fdata = 0
          !print *, "Delete row:", selections(1)
          !call hl_gtk_list1_rem(ihlist, selections(1))
       end if
    end if


  end subroutine
  subroutine getmacroeditboxText(ftext)

    type(c_ptr) :: buffer
    character(len=100), intent(inout) :: ftext
    buffer = gtk_entry_get_buffer(macroentry)
    call c_f_string_copy(gtk_entry_buffer_get_text(buffer), ftext)
    print *, "Entered name as:",trim(ftext)

  end subroutine

  subroutine macroedit_alert()

    integer(kind=c_int) :: resp
    character(len=40), dimension(5) :: msg

    msg(1) = "To continue macro editing"
    msg(2) = "open MAC_EDIT.DAT in your favorite "
    msg(3) = "ASCII text editor.  When finished"
    msg(4) = "click okay to continue"
    msg(5) = "So ZOA can update the macro list"

    resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_OK, &
         & "ALERT"//c_null_char, &
         & type=GTK_MESSAGE_WARNING, parent=ihwin)
    print *, "hl_dialog.f90 resp=", resp
  end subroutine

  subroutine populatemacrolist()

    character(len=8) :: line
    integer :: i, ltr, NUMINLIST
    CHARACTER(len=8), dimension(1024) :: MACARRAY

    call hl_gtk_list1_rem(ihlist)
    call logger%logText("About to call MACARRAY_LOAD")
    ! Now put 10 rows into it
    CALL MACARRAY_LOAD(NUMINLIST, MACARRAY)
    PRINT *, "NUMINLIST IS ", NUMINLIST
    do i=1,NUMINLIST-1
       !write(line,"('List entry number ',I0)") i
       !ltr=len_trim(line)+1
       line = MACARRAY(i)
       !line(ltr:ltr)=c_null_char
       print *, line
       call hl_gtk_list1_ins(ihlist, line//c_null_char)

    end do


  end subroutine

  subroutine macroui_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Exit called"
    call gtk_window_destroy(gdata)
  end subroutine

  subroutine zoa_macrosaveUI(act, param, win) bind(c)
      implicit none
      type(c_ptr), value, intent(in) :: act, param, win
      !PRINT *, "ABOUT TO CALL MACSAVE!"
      call PROCESKDP('MACSAVE')
  end subroutine

  subroutine zoa_macrorestoreUI(act, param, win) bind(c)
      implicit none
      type(c_ptr), value, intent(in) :: act, param, win

      call PROCESKDP('MACREST')
  end subroutine

  subroutine zoa_macromanualUI(act, param, win) bind(c)

      implicit none
      type(c_ptr), value, intent(in) :: act, param, win

      call gtk_show_uri(win, 'file://Macprmpt.pdf', 0_c_int)

  end subroutine

    subroutine zoa_macrooperationsUI(act, param, win) bind(c)

      implicit none
      type(c_ptr), value, intent(in) :: act, param, win
      type(c_ptr) :: base, ihscrollcontain, jbox, jbox2, abut, qbut
      type(c_ptr) :: macroentrylabel, textView, buffer, pane, rightPane
      type(c_ptr) :: leftPane, boxWin, rightPaneLabel, boxRight
      integer, target :: iappend=0, idel=0
      integer :: ltr



      print *, "Macro Operations Selected!"

      ! Create the window:
      ihwin = gtk_window_new()

      call gtk_window_set_title(ihwin, "Macro Operations"//c_null_char)

      call gtk_window_set_default_size(ihwin, 600_c_int, 600_c_int)

      ! Now make a column box & put it into the window

      leftPane = gtk_scrolled_window_new()
      base = hl_gtk_box_new()
      boxWin = hl_gtk_box_new(GTK_ORIENTATION_VERTICAL, 5_c_int)
      call gtk_window_set_child(leftPane, base)
      !call gtk_window_set_child(leftPane, base)
      call gtk_window_set_child(ihwin, boxWin)

    ! Now make a single column list with multiple selections enabled

    ihlist = hl_gtk_list1_new(ihscrollcontain, changed=c_funloc(macrolist_select),&
         & data=c_loc(idel), multiple=FALSE, height=400_c_int, &
         & title="List of Available Macros"//c_null_char)

    call populatemacrolist()
    ! It is the scrollcontainer that is placed into the box.
    call hl_gtk_box_pack(base, ihscrollcontain)

    macrorun = gtk_check_button_new_with_label("Run"//c_null_char)
    macroedit = gtk_check_button_new_with_label("Edit"//c_null_char)
    macrolist = gtk_check_button_new_with_label("List"//c_null_char)
    macrorename = gtk_check_button_new_with_label("Rename"//c_null_char)
    macrocopy = gtk_check_button_new_with_label("Copy"//c_null_char)
    macrodelete = gtk_check_button_new_with_label("Delete"//c_null_char)

    macrogroup = gtk_window_group_new()
    call gtk_check_button_set_group(macrorun, macroedit)
    call gtk_check_button_set_group(macrolist, macrorun)
    call gtk_check_button_set_group(macrorename, macrorun)
    call gtk_check_button_set_group(macrocopy, macrorun)
    call gtk_check_button_set_group(macrodelete, macrorun)

    call gtk_check_button_set_active(macrorun, 1_c_int)


    !call gtk_check_button_set_group(macroedit, macrogroup)

    call hl_gtk_box_pack(base, macrorun)
    call hl_gtk_box_pack(base, macroedit)
    call hl_gtk_box_pack(base, macrolist)
    call hl_gtk_box_pack(base, macrorename)
    call hl_gtk_box_pack(base, macrocopy)
    call hl_gtk_box_pack(base, macrodelete)

     macroentrylabel = gtk_label_new("Target Name for Macro rename or copy"//c_null_char)

     macroentry = hl_gtk_entry_new(editable=TRUE)
     call hl_gtk_box_pack(base, macroentrylabel)
     call hl_gtk_box_pack(base, macroentry)


     textView = gtk_text_view_new ()
     call gtk_text_view_set_editable(textView, FALSE)
     boxRight = hl_gtk_box_new()


     rightPane = gtk_scrolled_window_new()
     rightPaneLabel = gtk_label_new("Macro Contents"//c_null_char)

     call gtk_box_append(boxRight, rightPaneLabel)
     call gtk_box_append(boxRight, textView)


     !call gtk_scrolled_window_set_child(rightPane, textView)
     call gtk_scrolled_window_set_child(rightPane, boxRight)
     call gtk_widget_set_size_request(rightPane, 200_c_int, -1_c_int)
    !
     buffer = gtk_text_view_get_buffer (textView)
     call ioConfig%registerTextView(textView, ID_TERMINAL_MACRO)

! It is the scrollcontainer that is placed into the box.
    !call hl_gtk_box_pack(rightPane, textView)
    call gtk_window_set_child(leftPane, base)


    !call gtk_window_set_child(ihwin, pane)
    !call gtk_box_append(ihwin,pane)






    ! Make row box put it in the column box and put an editable
    ! 1-line text widget and a button in it
    jbox = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jbox)

    ! newline = hl_gtk_entry_new(len=35_c_int, editable=TRUE, &
    !      & activate=c_funloc(text_cr), data=c_loc(iappend), &
    !      & tooltip="Enter some text followed by <CR>"//c_new_line//&
    !      &"then click 'Append' to add it to the list"//c_null_char)
    ! call hl_gtk_box_pack(jbox, newline)
    abut = hl_gtk_button_new("Run Macro"//c_null_char, clicked=c_funloc(macrorun_click),&
         & data=c_loc(iappend))
    call hl_gtk_box_pack(jbox, abut)

    ! Make a row box and put it in the main box
    ! jbox2 = hl_gtk_box_new(horizontal=TRUE)
    ! call hl_gtk_box_pack(base, jbox2)
    ! ! Make a checkbox button and put it in the row box
    ! dbut = hl_gtk_check_button_new("Delete line"//c_null_char,&
    !      & toggled=c_funloc(del_toggle), initial_state=FALSE, &
    !      & data=c_loc(idel), &
    !      & tooltip="Set this then click on a line to delete it"//c_null_char)
    ! call hl_gtk_box_pack(jbox2, dbut)
    !
    ! ! And a delete all button.
    ! dabut = hl_gtk_button_new("Clear"//c_null_char, clicked=c_funloc(delete_all))
    ! call hl_gtk_box_pack(jbox2, dabut)
    !
    ! ! And a swap rows button
    ! swbut = hl_gtk_button_new("Swap rows"//c_null_char, clicked=c_funloc(swap_rows))
    ! call hl_gtk_box_pack(jbox2, swbut)

    ! Also a quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(macroui_destroy), &
    & data=ihwin)
    call hl_gtk_box_pack(base,qbut)

    ! realize the window
    PRINT *, "Create Paned Window"
    pane = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)
    call gtk_paned_set_start_child(pane, base)
    call gtk_paned_set_end_child(pane, rightPane)

    call gtk_box_append(boxWin,pane)

    call gtk_scrolled_window_set_min_content_width(rightPane, 200_c_int)

    call gtk_widget_show(ihwin)

    end subroutine

end module
