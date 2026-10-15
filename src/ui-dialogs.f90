! Save, open dialogs
module ui_dialogs
    use iso_c_binding, only: c_ptr, c_char

    implicit none

    interface
      ! GTK >= 4.4; not in the gtk-fortran 4.4.1 bindings.  Unlike
      ! gtk_file_filter_add_pattern, a suffix matches case-insensitively, so
      ! "zoa" lists both .zoa and .ZOA files.
      subroutine gtk_file_filter_add_suffix(filter, suffix) bind(c)
        import :: c_ptr, c_char
        type(c_ptr), value :: filter
        character(kind=c_char), dimension(*) :: suffix
      end subroutine
    end interface

    contains

    ! A file chooser for Zoa's open/save dialogs, built on gtk-fortran's
    ! hl_gtk_file_chooser_new (gtk-hl-chooser.f90, GPL v3 with the GCC
    ! Runtime Library Exception, Copyright (C) 2011 The gtk-fortran team),
    ! whose run loop below follows hl_gtk_file_chooser_show.  Two changes
    ! from the library dialog:
    !  - filters match the extension case-insensitively ("*.zoa" lists
    !    .zoa and .ZOA; the library's patterns are case-sensitive);
    !  - in save mode the accept button reads "Save" (the library always
    !    labels it "Open").
    ! patterns(i) are "*.ext" filters named names(i); an All Files filter
    ! is added last.
    function zoa_file_chooser_show(files, cdir, saveMode, title, initialDir, &
                                   patterns, names, parent) result(isel)
        use iso_c_binding
        use gtk_hl_chooser, only: hl_gtk_file_chooser_new, hl_gtk_chooser_info, &
                                  dialog_gmainloop
        use gtk, only: gtk_file_filter_new, gtk_file_filter_set_name, &
                       gtk_file_filter_add_pattern, gtk_file_chooser_add_filter, &
                       gtk_dialog_get_widget_for_response, gtk_button_set_label, &
                       gtk_widget_show, gtk_window_close, GTK_RESPONSE_APPLY, TRUE, FALSE
        use g, only: g_main_loop_new, g_main_loop_run, g_list_model_get_n_items, &
                     g_list_model_get_item, g_file_get_path, g_object_unref
        use gtk_sup, only: convert_c_string
        character(len=*), dimension(:), allocatable, intent(out) :: files
        character(len=*), intent(out) :: cdir
        logical, intent(in) :: saveMode
        character(len=*), intent(in) :: title, initialDir
        character(len=*), dimension(:), intent(in) :: patterns, names
        type(c_ptr), intent(in) :: parent
        integer(c_int) :: isel

        type(c_ptr) :: dialog, gfilter, btn, g_file
        type(hl_gtk_chooser_info) :: info
        integer(c_int) :: create, i, nsel
        character(len=64) :: ext

        create = FALSE
        if (saveMode) create = TRUE
        dialog = hl_gtk_file_chooser_new(info, create=create, &
             & title=trim(title)//c_null_char, initial_dir=trim(initialDir)//c_null_char, &
             & parent=parent, wsize=(/ 600_c_int, 400_c_int /))

        do i = 1, size(patterns)
            gfilter = gtk_file_filter_new()
            ext = adjustl(patterns(i))
            if (ext(1:2) == '*.') then
                call gtk_file_filter_add_suffix(gfilter, trim(ext(3:))//c_null_char)
            else
                call gtk_file_filter_add_pattern(gfilter, trim(ext)//c_null_char)
            end if
            call gtk_file_filter_set_name(gfilter, trim(names(i))//c_null_char)
            call gtk_file_chooser_add_filter(info%chooser, gfilter)
        end do
        gfilter = gtk_file_filter_new()
        call gtk_file_filter_add_pattern(gfilter, "*"//c_null_char)
        call gtk_file_filter_set_name(gfilter, "All Files"//c_null_char)
        call gtk_file_chooser_add_filter(info%chooser, gfilter)

        if (saveMode) then
            btn = gtk_dialog_get_widget_for_response(dialog, GTK_RESPONSE_APPLY)
            if (c_associated(btn)) call gtk_button_set_label(btn, "_Save"//c_null_char)
        end if

        call gtk_widget_show(dialog)
        ! hl_gtk_chooser_resp_cb (connected by hl_gtk_file_chooser_new) fills
        ! info and quits this loop.
        dialog_gmainloop = g_main_loop_new(c_null_ptr, FALSE)
        call g_main_loop_run(dialog_gmainloop)

        isel = info%iselect
        cdir = ''
        if (info%iselect == TRUE) then
            nsel = g_list_model_get_n_items(info%chooser_sel_list)
            allocate(files(nsel))
            do i = 1, nsel
                g_file = g_list_model_get_item(info%chooser_sel_list, i-1_c_int)
                call convert_c_string(g_file_get_path(g_file), files(i))
                call g_object_unref(g_file)
            end do
            call g_object_unref(info%chooser_sel_list)
            call convert_c_string(g_file_get_path(info%chooser_curdir), cdir)
        end if

        call gtk_window_close(dialog)
    end function

    function ui_open_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
        use iso_c_binding
        use gtk_hl_chooser

        type(c_ptr), value, intent(in) :: parent_window
        character(len=*), intent(inout) :: filename
        character(len=*), intent(inout) :: cdir
        character(len=*), intent(in) :: startFileDir
        character(len=*), intent(in) :: iptFilter
        character(len=*), intent(in) :: iptFilterName
      
        logical :: fileSelected
      
        integer(kind=c_int) :: isel
        character(len=120), dimension(:), allocatable :: chfile
        character(len=30), dimension(2) :: filters
        character(len=30), dimension(2) :: filtnames
        character(len=200) :: inln
        integer :: ios
        integer :: idxs
      
        filters(1) = iptFilter
        filters(2) = "*.txt"
        filtnames(1) = iptFilterName
        filtnames(2) = ".txt File"
        ! filters(1) = "*.seq"
        ! filters(2) = "*.txt"
        ! filtnames(1) = "CodeV .seq File"
        ! filtnames(2) = ".txt File"  
        fileSelected = .TRUE.
      
        isel = zoa_file_chooser_show(chfile, cdir, .false., "Select input file", &
             & startFileDir, filters, filtnames, parent_window)
        print *, "isel = hl_gtk_file_chooser_show=", isel
        if (isel == FALSE) then
          fileSelected = .FALSE.
          return
        else   ! No selection made
      
          filename = chfile(1)
          deallocate(chfile)
        end if
      
      end function    

      function ui_new_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
        use iso_c_binding
        use gtk_hl_chooser
        implicit none
        type(c_ptr), value, intent(in) :: parent_window
        character(len=*), intent(inout) :: filename
        character(len=*), intent(inout) :: cdir
        character(len=*), intent(in) :: startFileDir
        character(len=*), intent(in) :: iptFilter
        character(len=*), intent(in) :: iptFilterName
      
        logical :: fileSelected
      
        integer(kind=c_int) :: isel
        character(len=120), dimension(:), allocatable :: chfile
        character(len=30), dimension(2) :: filters
        character(len=30), dimension(2) :: filtnames
        character(len=200) :: inln
        integer :: ios
        integer :: idxs
      
        filters(1) = iptFilter
        filters(2) = "*.txt"
        filtnames(1) = iptFilterName
        filtnames(2) = ".txt File"
        ! filters(1) = "*.seq"
        ! filters(2) = "*.txt"
        ! filtnames(1) = "CodeV .seq File"
        ! filtnames(2) = ".txt File"  
        fileSelected = .TRUE.
      
        isel = zoa_file_chooser_show(chfile, cdir, .true., "Save file", &
             & startFileDir, filters, filtnames, parent_window)
        print *, "isel = hl_gtk_file_chooser_show=", isel
        if (isel == FALSE) then
          fileSelected = .FALSE.
          return
        else   ! No selection made
      
          filename = chfile(1)
          deallocate(chfile)
        end if
      
      end function

end module