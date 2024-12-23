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
      type(c_ptr) :: macroentry, macroTextView
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
              !PRINT *, "RUN SELECTED!"
              ! Find and print the selected row(s)
              !print *, nsel,"Rows selected"
              !print *, selections
              call hl_gtk_list1_get_cell(ihlist, selections(1), svalue)
              !print *, "TXT IS ", svalue
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
                call macroedit_savetofile(gtk_text_view_get_buffer(macroTextView))
                !print *, "About to call alert dialog"
                !call macroedit_alert()
                !print *, "Dialog Closed!"  
                !CALL MREFRESH 
                call PROCESKDP('IN FILE MAC_EDIT.DAT')
                !call PROCESKDP('MREFRESH')
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

  subroutine macroedit_savetofile(buffer)
    use zoa_file_handler, only: delete_file
    use kdp_utils, only: OUTKDP

    implicit none
    type(c_ptr) :: buffer
    type(gtktextiter), target :: iterStart, iterEnd
    integer :: numLines, i, boolRet, stat
    character(len=10000) :: bufferChar
    character(len=1024) :: lineTxt

    numLines = gtk_text_buffer_get_line_count(buffer)
    ! Use existing KDP code to edit.  Dump the contents of the
    ! buffer into MAC_EDIT.DAT.  Then when MREFRESH is called
    ! it will read this file and update the appropriate macro
    if (numLines > 1) then
      !call clear_file(trim(basePath)//'MAC_EDIT.DAT')
      call delete_file(trim(basePath)//'MAC_EDIT.DAT')
      call PROCESKDP('OUTPUT FILE MAC_EDIT.DAT')
    call gtk_text_buffer_get_start_iter(buffer, c_loc(iterStart))
    do i=1,numLines
      boolRet = gtk_text_buffer_get_iter_at_line(buffer, c_loc(iterEnd), i)
      call c_f_string_copy(gtk_text_buffer_get_text(buffer, &
      & c_loc(iterStart),c_loc(iterEnd), FALSE), lineTxt)
      if (i>1) call OUTKDP(trim(lineTxt),0)
      !PRINT *, "lineTxt is ", trim(lineTxt)
      iterStart = iterEnd
    end do
      CALL CLOSE_FILE(31,1)
      call PROCESKDP('OUTPUT TP')
    end if

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
         & type=GTK_MESSAGE_WARNING, parent=macro_ui_window)
    print *, "hl_dialog.f90 resp=", resp
  end subroutine

  subroutine populatemacrolist()
    use zoa_file_handler
     
    character(len=24) :: line
    integer :: i, ltr, NUMINLIST
    CHARACTER(len=1024), dimension(1024) :: MACARRAY
    !! This is a FORD test!

    call hl_gtk_list1_rem(ihlist)
    call logger%logText("About to call MACARRAY_LOAD")
    ! Now put 10 rows into it
    call getListofFilesInDirectory(trim(getMacroDir()), '.zoa', MACARRAY, NUMINLIST)
    !CALL MACARRAY_LOAD(NUMINLIST, MACARRAY)
    PRINT *, "NUMINLIST IS ", NUMINLIST
    do i=1,NUMINLIST
       !write(line,"('List entry number ',I0)") i
       !ltr=len_trim(line)+1
       line = MACARRAY(i)
       !line(ltr:ltr)=c_null_char
       print *, line
       call hl_gtk_list1_ins(ihlist, trim(line)//c_null_char)

    end do


  end subroutine

  subroutine macroui_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    macro_ui_window = c_null_ptr

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

    subroutine zoa_macrooperationsUI(win) bind(c)
      use hl_gtk_zoa, only: hl_zoa_text_view_new
      implicit none
      type(c_ptr), value, intent(in) ::  win
      type(c_ptr) :: base, ihscrollcontain, jbox, jbox2, abut, qbut
      type(c_ptr) :: macroentrylabel, buffer, pane, rightPane
      type(c_ptr) :: leftPane, boxWin, rightPaneLabel, boxRight
      integer, target :: iappend=0, idel=0
      integer :: ltr



      ! Create the window:
      macro_ui_window = gtk_window_new()

      call gtk_window_set_title(macro_ui_window, "Macro Operations"//c_null_char)

      call gtk_window_set_default_size(macro_ui_window, 600_c_int, 600_c_int)

      ! Now make a column box & put it into the window

      leftPane = gtk_scrolled_window_new()
      base = hl_gtk_box_new()
      boxWin = hl_gtk_box_new(GTK_ORIENTATION_VERTICAL, 5_c_int)
      call gtk_window_set_child(leftPane, base)
      !call gtk_window_set_child(leftPane, base)
      call gtk_window_set_child(macro_ui_window, boxWin)

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



     macroTextView = hl_zoa_text_view_new()
     call gtk_text_view_set_editable(macroTextView, TRUE)
     boxRight = hl_gtk_box_new()


     rightPane = gtk_scrolled_window_new()
     rightPaneLabel = gtk_label_new("Macro Contents"//c_null_char)

     call gtk_box_append(boxRight, rightPaneLabel)
     call gtk_box_append(boxRight, macroTextView)


     call gtk_scrolled_window_set_child(rightPane, boxRight)
     call gtk_widget_set_size_request(rightPane, 200_c_int, -1_c_int)
    !
     buffer = gtk_text_view_get_buffer (macroTextView)
     PRINT *, "Buffer is ", LOC(buffer)
     call ioConfig%registerTextView(macroTextView, ID_TERMINAL_MACRO)

    call gtk_window_set_child(leftPane, base)

    jbox = hl_gtk_box_new(horizontal=TRUE)
    call hl_gtk_box_pack(base, jbox)

    abut = hl_gtk_button_new("Execute Command"//c_null_char, &
         & clicked=c_funloc(macrorun_click),&
         & data=c_loc(iappend))
    call hl_gtk_box_pack(jbox, abut)

    ! Also a quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(macroui_destroy), &
    & data=macro_ui_window)
    call hl_gtk_box_pack(base,qbut)

    ! realize the window
    PRINT *, "Create Paned Window"
    pane = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)
    call gtk_paned_set_start_child(pane, base)
    call gtk_paned_set_end_child(pane, rightPane)

    call gtk_box_append(boxWin,pane)

    call gtk_scrolled_window_set_min_content_width(rightPane, 200_c_int)


    call gtk_widget_show(macro_ui_window)

    end subroutine




end module
