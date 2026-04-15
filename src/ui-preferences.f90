module ui_preferences
  use iso_c_binding
  use gtk
  use g
  use gtk_hl_container
  use gtk_hl_button
  use gtk_hl_entry
  use global_widgets, only: preferences_window, my_window
  use zoa_file_handler, only: getMacroDir, getTempDirectory, getSaveDirectory, &
                               getGlassCatalogDir, setMacroDir, setTempDir,    &
                               setProjectDir, setGlassCatalogDir,              &
                               doesDirectoryExist, savePreferences

  implicit none
  private
  public :: preferences_new

  ! Entry widgets — module-level so callbacks can read them
  type(c_ptr) :: entry_glass, entry_temp, entry_macros, entry_project
  type(c_ptr) :: label_error

contains

  ! -----------------------------------------------------------------------
  ! Helper: read a Fortran string from a GTK entry widget
  ! -----------------------------------------------------------------------
  subroutine get_entry_text(entry_widget, fstr)
    use gtk_sup, only: c_f_string_copy
    type(c_ptr), intent(in)       :: entry_widget
    character(len=*), intent(out) :: fstr
    type(c_ptr) :: buff
    buff = gtk_entry_get_buffer(entry_widget)
    call c_f_string_copy(gtk_entry_buffer_get_text(buff), fstr)
  end subroutine

  ! -----------------------------------------------------------------------
  ! Validate and apply the four directory entries.
  ! Returns .TRUE. if all paths exist (or are empty → keep current).
  ! On failure, sets the error label text.
  ! -----------------------------------------------------------------------
  function apply_preferences() result(ok)
    logical :: ok
    character(len=1024) :: glass, tmp, macros, proj
    character(len=512)  :: errmsg

    call get_entry_text(entry_glass,   glass)
    call get_entry_text(entry_temp,    tmp)
    call get_entry_text(entry_macros,  macros)
    call get_entry_text(entry_project, proj)

    errmsg = ''

    if (len_trim(glass)  > 0 .and. .not. doesDirectoryExist(trim(glass)))  &
        errmsg = trim(errmsg)//'  Glass Catalogs'
    if (len_trim(tmp)    > 0 .and. .not. doesDirectoryExist(trim(tmp)))    &
        errmsg = trim(errmsg)//'  Temp Dir'
    if (len_trim(macros) > 0 .and. .not. doesDirectoryExist(trim(macros))) &
        errmsg = trim(errmsg)//'  Macros'
    if (len_trim(proj)   > 0 .and. .not. doesDirectoryExist(trim(proj)))   &
        errmsg = trim(errmsg)//'  Project Dir'

    if (len_trim(errmsg) > 0) then
      call gtk_label_set_text(label_error, &
           'Directory not found:'//trim(errmsg)//c_null_char)
      ok = .FALSE.
      return
    end if

    ! All paths valid — apply
    call gtk_label_set_text(label_error, ''//c_null_char)
    if (len_trim(glass)  > 0) call setGlassCatalogDir(trim(glass))
    if (len_trim(tmp)    > 0) call setTempDir(trim(tmp))
    if (len_trim(macros) > 0) call setMacroDir(trim(macros))
    if (len_trim(proj)   > 0) call setProjectDir(trim(proj))
    call savePreferences()
    ok = .TRUE.
  end function

  ! -----------------------------------------------------------------------
  ! Button callbacks
  ! -----------------------------------------------------------------------

  subroutine callback_prefs_ok(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    logical :: ok
    ok = apply_preferences()
    if (ok) call gtk_window_destroy(preferences_window)
  end subroutine

  subroutine callback_prefs_apply(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    logical :: ok
    ok = apply_preferences()
  end subroutine

  subroutine callback_prefs_cancel(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    call gtk_window_destroy(preferences_window)
  end subroutine

  subroutine callback_prefs_destroy(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    preferences_window = c_null_ptr
  end subroutine

  ! -----------------------------------------------------------------------
  ! Create and show the Preferences window
  ! -----------------------------------------------------------------------
  subroutine preferences_new(parent_win)
    type(c_ptr), intent(in) :: parent_win

    type(c_ptr) :: outer_box, grid, sep, btn_box
    type(c_ptr) :: btn_ok, btn_apply, btn_cancel
    type(c_ptr) :: lbl
    integer(c_int), parameter :: LABEL_COL = 0, ENTRY_COL = 1
    integer(c_int), parameter :: ROW_GLASS = 0, ROW_TEMP = 1, ROW_MACROS = 2, ROW_PROJ = 3
    integer(c_int), parameter :: GRID_COL_SPAN = 1, GRID_ROW_SPAN = 1
    integer(c_int), parameter :: WIN_WIDTH = 550, WIN_HEIGHT = 220

    ! Create window
    preferences_window = gtk_window_new()
    call gtk_window_set_title(preferences_window, 'Preferences'//c_null_char)
    call gtk_window_set_default_size(preferences_window, WIN_WIDTH, WIN_HEIGHT)
    call gtk_window_set_transient_for(preferences_window, parent_win)
    call gtk_window_set_destroy_with_parent(preferences_window, TRUE)
    call gtk_window_set_resizable(preferences_window, FALSE)

    ! Outer vertical box (margin = 12)
    outer_box = gtk_box_new(GTK_ORIENTATION_VERTICAL, 6_c_int)
    call gtk_widget_set_margin_start(outer_box,  12_c_int)
    call gtk_widget_set_margin_end(outer_box,    12_c_int)
    call gtk_widget_set_margin_top(outer_box,    12_c_int)
    call gtk_widget_set_margin_bottom(outer_box, 12_c_int)

    ! Grid: 2 columns (label | entry), 4 rows
    grid = gtk_grid_new()
    call gtk_grid_set_column_spacing(grid, 12_c_int)
    call gtk_grid_set_row_spacing(grid,    6_c_int)
    call gtk_widget_set_hexpand(grid, TRUE)

    ! Row 0 — Glass Catalogs
    lbl = gtk_label_new('Glass Catalogs:'//c_null_char)
    call gtk_label_set_xalign(lbl, 1.0_c_float)
    call gtk_grid_attach(grid, lbl, LABEL_COL, ROW_GLASS, GRID_COL_SPAN, GRID_ROW_SPAN)

    entry_glass = hl_gtk_entry_new(editable=TRUE, value=trim(getGlassCatalogDir()))
    call gtk_widget_set_hexpand(entry_glass, TRUE)
    call gtk_grid_attach(grid, entry_glass, ENTRY_COL, ROW_GLASS, GRID_COL_SPAN, GRID_ROW_SPAN)

    ! Row 1 — Temp Dir
    lbl = gtk_label_new('Temp Dir:'//c_null_char)
    call gtk_label_set_xalign(lbl, 1.0_c_float)
    call gtk_grid_attach(grid, lbl, LABEL_COL, ROW_TEMP, GRID_COL_SPAN, GRID_ROW_SPAN)

    entry_temp = hl_gtk_entry_new(editable=TRUE, value=trim(getTempDirectory()))
    call gtk_widget_set_hexpand(entry_temp, TRUE)
    call gtk_grid_attach(grid, entry_temp, ENTRY_COL, ROW_TEMP, GRID_COL_SPAN, GRID_ROW_SPAN)

    ! Row 2 — Macros
    lbl = gtk_label_new('Macros:'//c_null_char)
    call gtk_label_set_xalign(lbl, 1.0_c_float)
    call gtk_grid_attach(grid, lbl, LABEL_COL, ROW_MACROS, GRID_COL_SPAN, GRID_ROW_SPAN)

    entry_macros = hl_gtk_entry_new(editable=TRUE, value=trim(getMacroDir()))
    call gtk_widget_set_hexpand(entry_macros, TRUE)
    call gtk_grid_attach(grid, entry_macros, ENTRY_COL, ROW_MACROS, GRID_COL_SPAN, GRID_ROW_SPAN)

    ! Row 3 — Project Dir
    lbl = gtk_label_new('Project Dir:'//c_null_char)
    call gtk_label_set_xalign(lbl, 1.0_c_float)
    call gtk_grid_attach(grid, lbl, LABEL_COL, ROW_PROJ, GRID_COL_SPAN, GRID_ROW_SPAN)

    entry_project = hl_gtk_entry_new(editable=TRUE, value=trim(getSaveDirectory()))
    call gtk_widget_set_hexpand(entry_project, TRUE)
    call gtk_grid_attach(grid, entry_project, ENTRY_COL, ROW_PROJ, GRID_COL_SPAN, GRID_ROW_SPAN)

    call gtk_box_append(outer_box, grid)

    ! Error label (hidden until validation fails)
    label_error = gtk_label_new(''//c_null_char)
    call gtk_label_set_xalign(label_error, 0.0_c_float)
    call gtk_widget_add_css_class(label_error, 'error'//c_null_char)
    call gtk_box_append(outer_box, label_error)

    ! Separator
    sep = gtk_separator_new(GTK_ORIENTATION_HORIZONTAL)
    call gtk_box_append(outer_box, sep)

    ! Button row: Cancel | (spacer) | Apply | OK
    btn_box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 6_c_int)

    btn_cancel = gtk_button_new_with_label('Cancel'//c_null_char)
    call g_signal_connect(btn_cancel, 'clicked'//c_null_char, &
                          c_funloc(callback_prefs_cancel), c_null_ptr)
    call gtk_box_append(btn_box, btn_cancel)

    ! Spacer to push Apply/OK to the right
    lbl = gtk_label_new(''//c_null_char)
    call gtk_widget_set_hexpand(lbl, TRUE)
    call gtk_box_append(btn_box, lbl)

    btn_apply = gtk_button_new_with_label('Apply'//c_null_char)
    call g_signal_connect(btn_apply, 'clicked'//c_null_char, &
                          c_funloc(callback_prefs_apply), c_null_ptr)
    call gtk_box_append(btn_box, btn_apply)

    btn_ok = gtk_button_new_with_label('OK'//c_null_char)
    call g_signal_connect(btn_ok, 'clicked'//c_null_char, &
                          c_funloc(callback_prefs_ok), c_null_ptr)
    call gtk_box_append(btn_box, btn_ok)

    call gtk_box_append(outer_box, btn_box)

    call gtk_window_set_child(preferences_window, outer_box)

    ! Clear preferences_window pointer on destroy
    call g_signal_connect(preferences_window, 'destroy'//c_null_char, &
                          c_funloc(callback_prefs_destroy), c_null_ptr)

    call gtk_widget_show(preferences_window)

  end subroutine preferences_new

end module ui_preferences
