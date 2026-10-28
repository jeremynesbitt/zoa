! Save, open dialogs
module ui_dialogs
    use iso_c_binding, only: c_ptr, c_char, c_int, c_null_ptr

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

    ! State of the one dialog open at a time, filled by file_dialog_done.
    type(c_ptr), save :: dlg_loop = c_null_ptr
    logical, save :: dlg_save = .false., dlg_ok = .false.
    character(len=1024), save :: dlg_path = '', dlg_dir = ''

    private :: dlg_loop, dlg_save, dlg_ok, dlg_path, dlg_dir

    contains

    ! Zoa's open/save dialog: a GtkFileDialog (GTK >= 4.10), which is the
    ! platform's own dialog where GTK has one -- NSOpenPanel/NSSavePanel on
    ! macOS, the system dialog on Windows, the portal on Linux.  The dialog is
    ! asynchronous; this waits for it in a nested main loop, so callers block
    ! as they did with the old GtkFileChooserDialog.
    ! patterns(i) are "*.ext" filters (matched case-insensitively) named
    ! names(i); an All Files filter is added last.  On success file is the
    ! chosen path and cdir its folder.
    logical function zoa_file_chooser_show(file, cdir, saveMode, title, initialDir, &
                                           patterns, names, parent) result(ok)
        use iso_c_binding
        use gtk, only: gtk_file_dialog_new, gtk_file_dialog_set_title, &
                       gtk_file_dialog_set_modal, gtk_file_dialog_set_initial_folder, &
                       gtk_file_dialog_set_filters, gtk_file_dialog_set_default_filter, &
                       gtk_file_dialog_open, gtk_file_dialog_save, &
                       gtk_file_filter_new, gtk_file_filter_set_name, &
                       gtk_file_filter_add_pattern, gtk_file_filter_get_type, TRUE, FALSE
        use g, only: g_main_loop_new, g_main_loop_run, g_main_loop_unref, &
                     g_list_store_new, g_list_store_append, g_file_new_for_path, &
                     g_object_unref
        character(len=*), intent(out) :: file, cdir
        logical, intent(in) :: saveMode
        character(len=*), intent(in) :: title, initialDir
        character(len=*), dimension(:), intent(in) :: patterns, names
        type(c_ptr), intent(in) :: parent

        type(c_ptr) :: dialog, store, gfilter, folder
        integer :: i
        character(len=64) :: ext

        dialog = gtk_file_dialog_new()
        call gtk_file_dialog_set_title(dialog, trim(title)//c_null_char)
        call gtk_file_dialog_set_modal(dialog, TRUE)
        if (len_trim(initialDir) > 0) then
            folder = g_file_new_for_path(trim(initialDir)//c_null_char)
            call gtk_file_dialog_set_initial_folder(dialog, folder)
            call g_object_unref(folder)
        end if

        store = g_list_store_new(gtk_file_filter_get_type())
        do i = 1, size(patterns)
            gfilter = gtk_file_filter_new()
            ext = adjustl(patterns(i))
            if (ext(1:2) == '*.') then
                call gtk_file_filter_add_suffix(gfilter, trim(ext(3:))//c_null_char)
            else
                call gtk_file_filter_add_pattern(gfilter, trim(ext)//c_null_char)
            end if
            call gtk_file_filter_set_name(gfilter, trim(names(i))//c_null_char)
            call g_list_store_append(store, gfilter)
            if (i == 1) call gtk_file_dialog_set_default_filter(dialog, gfilter)
            call g_object_unref(gfilter)
        end do
        gfilter = gtk_file_filter_new()
        call gtk_file_filter_add_pattern(gfilter, "*"//c_null_char)
        call gtk_file_filter_set_name(gfilter, "All Files"//c_null_char)
        call g_list_store_append(store, gfilter)
        call g_object_unref(gfilter)
        call gtk_file_dialog_set_filters(dialog, store)
        call g_object_unref(store)

        dlg_save = saveMode
        dlg_ok = .false.; dlg_path = ''; dlg_dir = ''
        dlg_loop = g_main_loop_new(c_null_ptr, FALSE)
        if (saveMode) then
            call gtk_file_dialog_save(dialog, parent, c_null_ptr, &
                                      c_funloc(file_dialog_done), c_null_ptr)
        else
            call gtk_file_dialog_open(dialog, parent, c_null_ptr, &
                                      c_funloc(file_dialog_done), c_null_ptr)
        end if
        call g_main_loop_run(dlg_loop)       ! until file_dialog_done
        call g_main_loop_unref(dlg_loop)
        dlg_loop = c_null_ptr
        call g_object_unref(dialog)

        ok = dlg_ok
        file = dlg_path
        cdir = dlg_dir
    end function

    ! GAsyncReadyCallback of the open/save: the chosen file (none when the
    ! user cancels), then end the wait in zoa_file_chooser_show.
    subroutine file_dialog_done(source, result, user_data) bind(c)
        use iso_c_binding
        use gtk, only: gtk_file_dialog_open_finish, gtk_file_dialog_save_finish
        use g, only: g_main_loop_quit, g_file_get_path, g_file_get_parent, &
                     g_object_unref, g_free
        use gtk_sup, only: convert_c_string
        type(c_ptr), value :: source, result, user_data
        type(c_ptr) :: gfile, parentDir, cpath

        if (dlg_save) then
            gfile = gtk_file_dialog_save_finish(source, result, c_null_ptr)
        else
            gfile = gtk_file_dialog_open_finish(source, result, c_null_ptr)
        end if
        if (c_associated(gfile)) then
            cpath = g_file_get_path(gfile)
            if (c_associated(cpath)) then
                call convert_c_string(cpath, dlg_path)
                call g_free(cpath)
                dlg_ok = .true.
            end if
            parentDir = g_file_get_parent(gfile)
            if (c_associated(parentDir)) then
                cpath = g_file_get_path(parentDir)
                if (c_associated(cpath)) then
                    call convert_c_string(cpath, dlg_dir)
                    call g_free(cpath)
                end if
                call g_object_unref(parentDir)
            end if
            call g_object_unref(gfile)
        end if
        if (c_associated(dlg_loop)) call g_main_loop_quit(dlg_loop)
    end subroutine

    function ui_open_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
        type(c_ptr), value, intent(in) :: parent_window
        character(len=*), intent(inout) :: filename
        character(len=*), intent(inout) :: cdir
        character(len=*), intent(in) :: startFileDir
        character(len=*), intent(in) :: iptFilter
        character(len=*), intent(in) :: iptFilterName
        logical :: fileSelected

        character(len=1024) :: path, dir
        character(len=30), dimension(2) :: filters
        character(len=30), dimension(2) :: filtnames

        filters(1) = iptFilter
        filters(2) = "*.txt"
        filtnames(1) = iptFilterName
        filtnames(2) = ".txt File"

        fileSelected = zoa_file_chooser_show(path, dir, .false., "Select input file", &
             & startFileDir, filters, filtnames, parent_window)
        if (fileSelected) then
            filename = path
            cdir = dir
        end if
    end function

    function ui_new_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
        type(c_ptr), value, intent(in) :: parent_window
        character(len=*), intent(inout) :: filename
        character(len=*), intent(inout) :: cdir
        character(len=*), intent(in) :: startFileDir
        character(len=*), intent(in) :: iptFilter
        character(len=*), intent(in) :: iptFilterName
        logical :: fileSelected

        character(len=1024) :: path, dir
        character(len=30), dimension(2) :: filters
        character(len=30), dimension(2) :: filtnames

        filters(1) = iptFilter
        filters(2) = "*.txt"
        filtnames(1) = iptFilterName
        filtnames(2) = ".txt File"

        fileSelected = zoa_file_chooser_show(path, dir, .true., "Save file", &
             & startFileDir, filters, filtnames, parent_window)
        if (fileSelected) then
            filename = path
            cdir = dir
        end if
    end function

end module
