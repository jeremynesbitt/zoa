module ui_lens_library
      use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
      use gtk
      use gtk_hl_tree
      use gtk_hl_button
      use gtk_hl_entry
      use gtk_hl_dialog
      use GLOBALS
      use global_widgets

      type(c_ptr) :: ihlist
      type(c_ptr) :: btn_load, btn_del
      type(c_ptr) :: macrorename, macrocopy, macrodelete
      type(c_ptr) :: macroentry, ihwin
  contains

 recursive subroutine lenslibrarylist_select(list, gdata) bind(c)
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
          call ioConfig%setTextView(ID_TERMINAL_LENSLIB)
          buffer = gtk_text_view_get_buffer(ioConfig%textView)
          call hl_gtk_text_view_delete(c_null_ptr, buffer=buffer)
          !CALL PROCESKDP('LIB P ' //svalue)
          CALL PROCESKDP('LIB P '//svalue(1:3))
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

  subroutine lenslib_addNew(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

     call PROCESKDP('LIB PUT')
     call populatelenslibrarylist()

  end subroutine

  subroutine lenslib_exec(widget, gdata) bind(c)
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
              print *, nsel,"Rows selected"
              print *, selections
              call hl_gtk_list1_get_cell(ihlist, selections(1), svalue)
              print *, "TXT IS ", svalue
              deallocate(selections)

          if (gtk_check_button_get_active(btn_load).EQ.TRUE) THEN
              PRINT *, "Load Library Selected!"
              CALL PROCESKDP('LIB GET , '//svalue(1:3))
              !CALL PROCESKDP(svalue)
              !call PROCESKDP('MREFRESH')
          else if (gtk_check_button_get_active(btn_del).EQ.TRUE) THEN
               PRINT *, "Del Library Selected!"
               CALL PROCESKDP('LIB DEL , '//svalue(1:3))
               call populatelenslibrarylist()
               !CALL PROCESKDP('MFL '//svalue)


        end if

       else    ! Delete the selected row
          !call gtk_check_button_set_active(dbut, FALSE)
          fdata = 0
          !print *, "Delete row:", selections(1)
          !call hl_gtk_list1_rem(ihlist, selections(1))
       end if
    end if


  end subroutine


subroutine populatelenslibrarylist()

  character(len=32) :: line
  integer :: i, ltr, NUMINLIST
  CHARACTER(len=8), dimension(1024) :: CATLIST
  CHARACTER(len=80), dimension(999) :: LIBARRAY
  call hl_gtk_list1_rem(ihlist)
  ! Now put 10 rows into it
  !CALL LIBLOAD()
  CALL LIBLOAD(NUMINLIST, LIBARRAY)
  !  PRINT *, LIBARRAY
  !CALL GCATLOAD(NUMINLIST, CATARRAY)
  !CALL MACARRAY_LOAD(NUMINLIST, MACARRAY)
  PRINT *, "NUMINLIST IS ", NUMINLIST
  do i=1,NUMINLIST-1

     line = LIBARRAY(i)
     call hl_gtk_list1_ins(ihlist, line//c_null_char)

  end do


end subroutine

  subroutine lenslibraryui_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata

    print *, "Exit called"
    call gtk_window_destroy(gdata)
  end subroutine


    subroutine create_lenslibraryUI(act, param, win) bind(c)
      use hl_gtk_zoa, only: hl_zoa_text_view_new

      implicit none
      type(c_ptr), value, intent(in) :: act, param, win
      type(c_ptr) :: base, ihscrollcontain, abut, qbut, btn_addToLib
      type(c_ptr) :: otherlabel, textView, buffer, pane, rightPane
      type(c_ptr) :: leftPane, boxWin, rightPaneLabel, boxRight
      integer, target :: iappend=0, idel=0
      integer :: ltr


      ! Create the window:
      ihwin = gtk_window_new()

      call gtk_window_set_title(ihwin, "Lens Library"//c_null_char)

      call gtk_window_set_default_size(ihwin, 600_c_int, 600_c_int)

      ! Now make a column box & put it into the window

      leftPane = gtk_scrolled_window_new()
      base = hl_gtk_box_new()
      boxWin = hl_gtk_box_new(GTK_ORIENTATION_VERTICAL, 5_c_int)
      call gtk_window_set_child(leftPane, base)
      !call gtk_window_set_child(leftPane, base)
      call gtk_window_set_child(ihwin, boxWin)

    ! Now make a single column list with multiple selections enabled

    ihlist = hl_gtk_list1_new(ihscrollcontain, changed=c_funloc(lenslibrarylist_select),&
         & data=c_loc(idel), multiple=FALSE, height=400_c_int, &
         & title="List of Lens Libraries"//c_null_char)

    call populatelenslibrarylist()
    ! It is the scrollcontainer that is placed into the box.
    PRINT *, "Okay here?"
    call hl_gtk_box_pack(base, ihscrollcontain)
    PRINT *, "Okay here?"
    btn_load = gtk_check_button_new_with_label("Load"//c_null_char)
    btn_del = gtk_check_button_new_with_label("Delete"//c_null_char)

    call gtk_check_button_set_group(btn_load, btn_del)
    call gtk_check_button_set_active(btn_load, 1_c_int)


    call hl_gtk_box_pack(base, btn_load)
    call hl_gtk_box_pack(base, btn_del)


     textView = hl_zoa_text_view_new()
     call gtk_text_view_set_editable(textView, FALSE)
     boxRight = hl_gtk_box_new()


     rightPane = gtk_scrolled_window_new()
     rightPaneLabel = gtk_label_new("Selected Lens Library Contents"//c_null_char)

     call gtk_box_append(boxRight, rightPaneLabel)
     call gtk_box_append(boxRight, textView)


     !call gtk_scrolled_window_set_child(rightPane, textView)
     call gtk_scrolled_window_set_child(rightPane, boxRight)
     call gtk_widget_set_size_request(rightPane, 200_c_int, -1_c_int)
    !
     buffer = gtk_text_view_get_buffer (textView)
     call ioConfig%registerTextView(textView, ID_TERMINAL_LENSLIB)

! It is the scrollcontainer that is placed into the box.
    !call hl_gtk_box_pack(rightPane, textView)
    call gtk_window_set_child(leftPane, base)

    abut = hl_gtk_button_new("Execute"//c_null_char, clicked=c_funloc(lenslib_exec),&
         & data=c_loc(iappend))
    call hl_gtk_box_pack(base, abut)

     otherlabel = gtk_label_new("Other commands"//c_null_char)
     call hl_gtk_box_pack(base, otherlabel)


    btn_addToLib = hl_gtk_button_new("Add current lens to library"//c_null_char, clicked=c_funloc(lenslib_addNew))
    call hl_gtk_box_pack(base, btn_addToLib)

    ! Also a quit button
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(lenslibraryui_destroy), &
    & data=ihwin)
    call hl_gtk_box_pack(base,qbut)

    !PRINT *, "Create Paned Window"
    pane = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)
    call gtk_paned_set_start_child(pane, base)
    call gtk_paned_set_end_child(pane, rightPane)

    call gtk_box_append(boxWin,pane)

    call gtk_scrolled_window_set_min_content_width(rightPane, 200_c_int)

    call gtk_widget_show(ihwin)

    end subroutine

end module
