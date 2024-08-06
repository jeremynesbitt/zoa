
module zoamenubar

  use global_widgets
  use GLOBALS

  use gtk, only: gtk_application_window_new, gtk_window_destroy, &
  & g_signal_connect, g_signal_connect_swapped, &
  & gtk_window_set_child, gtk_expander_set_child, gtk_box_append, &
  & gtk_scrolled_window_set_child, gtk_drawing_area_new, &
  & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  & gtk_drawing_area_set_draw_func, &
  & gtk_widget_queue_draw, gtk_widget_show, &
  & gtk_window_set_default_size, gtk_window_set_title, &
  & TRUE, FALSE, GDK_COLORSPACE_RGB, &
  & gtk_grid_new, gtk_grid_attach, gtk_button_new_with_label,&
  & gtk_box_new, gtk_spin_button_new,&
  & gtk_adjustment_new, gtk_spin_button_get_value, gtk_label_new, &
  & gtk_expander_new_with_mnemonic, gtk_expander_set_expanded, &
  & gtk_toggle_button_new_with_label, gtk_toggle_button_get_active, gtk_notebook_new,&
  & gtk_notebook_append_page, gtk_text_view_new, gtk_text_view_get_buffer, &
  & gtk_text_buffer_set_text, gtk_scrolled_window_new, &
  & gtk_text_buffer_get_end_iter, &
  & gtk_text_buffer_insert_at_cursor, gtk_statusbar_new, &
  & gtk_statusbar_push, gtk_statusbar_pop, gtk_statusbar_get_context_id, &
  & gtk_button_new_with_mnemonic, gtk_link_button_new_with_label, &
  & gtk_toggle_button_new_with_mnemonic, gtk_label_new_with_mnemonic, &
  & gtk_window_set_mnemonics_visible, gtk_combo_box_text_new, &
  & gtk_combo_box_text_append_text, gtk_combo_box_text_get_active_text, &
  & gtk_combo_box_text_insert_text, gtk_spin_button_set_value, gtk_spin_button_update,&
  & GTK_ORIENTATION_VERTICAL, gtk_grid_set_column_homogeneous, &
  & gtk_grid_set_row_homogeneous, gtk_statusbar_remove_all, &
  & gtk_widget_set_vexpand, gtk_entry_get_text_length, &
  & gtk_entry_get_buffer, gtk_entry_buffer_get_text, &
  & gtk_text_tag_new, gtk_entry_buffer_set_text, &
  & gtk_text_tag_table_new, gtk_text_buffer_insert_markup, &
  & gtk_text_buffer_apply_tag, gtk_text_view_scroll_to_mark, &
  & gtk_widget_get_display, gtk_css_provider_new, &
  & gtk_toggle_button_get_active, gtk_statusbar_get_context_id, &
  & gtk_style_context_add_provider_for_display, gtk_css_provider_load_from_data, &
  & gtk_application_window_set_show_menubar, gtk_window_maximize, gtk_window_unmaximize, &
  & gtk_application_set_menubar, gtk_widget_set_name, gtk_window_present


  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending, &
  & g_object_set_property, g_value_set_interned_string, g_variant_new_boolean, &
  & g_menu_new, g_menu_item_new, g_action_map_add_action, g_menu_append_item, &
  & g_object_unref, g_menu_append_section, g_menu_append_submenu, &
  & g_simple_action_new_stateful, g_variant_type_new, g_simple_action_new, &
  & g_variant_get_boolean, g_variant_get_string, g_variant_new_string, &
  & g_action_change_state, g_application_quit, g_simple_action_set_state, &
  & g_menu_item_set_attribute_value, g_variant_new_string


  !use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr, c_null_char, C_NEW_LINE
  use, intrinsic :: iso_fortran_env, only: int64
  ! Pl Plot based inputs
  use, intrinsic :: iso_c_binding
  use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width

  use plplot, PI => PL_PI
  use plplot_extra
  use gtk_draw_hl
  use gtk_hl_container
  use gtk_hl_button

  use gtk_hl_chooser


    implicit NONE
    !addNewMenuItemThatExecutesCommand(topLevelMenu, menuitemText, menuItemEvenName, arrayOfCommands)


! type zoamenubar
!
!   !integer, allocatable :: surface(:)
!   type(c_ptr) :: app, win
!
! end type zoamenubar
!
! interface zoamenubar
!     module procedure :: zoamenubar_constructor
! end interface zoamenubar


contains

  subroutine populatezoamenubar(win)

    use zoa_macro_ui
    use zoa_glass_ui
    use ui_lens_library


    type(c_ptr), intent(in) :: win

    type(c_ptr) :: act_quit
    type(c_ptr) :: menubar, menu, section1, section2, section3, &
      & menu_item_quit

    type(c_ptr) :: menu_lens, section1_lens
    type(c_ptr) :: menu_paraxial, act_firstorder, menu_item_firstorder
    type(c_ptr) :: menu_imagEval, menu_wavefront

    type(c_ptr) :: menu_macro
    type(c_ptr) :: menu_edit

    type(c_ptr) :: act_editlensrad, menu_item_editlensrad

    type(c_ptr) :: act_glassDisplay, menu_item_glass
    type(c_ptr) :: act_lenslib, menu_item_lenslib
 

    type(c_ptr) :: menu_import


    character(len=100), target :: pltAst = "ASTFCDIST; GO"
    character(len=100), target :: pltSpd = "SPO; GO"
    character(len=100), target :: pltRMS = "PLORMSFLD; GO"
    character(len=100), target :: pltSPR = "SPR"

    character(len=100), target :: syscon = "SYSCON"
    character(len=100), target :: drawCmd = "VIE; GO"
    character(len=100), target :: seidelCmd = "PLTTHO; GO"
    character(len=100), target :: macroCmd = "MACROUI"
    character(len=100), target :: zernFldCmd = "ZERN_TST; GO"
    character(len=100), target :: opdPltCmd = "PMA; GO"    
    character(len=100), target :: newLensCmd = "LEN NEW" 
    character(len=100), target :: fanCmd = "RIM;GO"    



    act_quit = g_simple_action_new ("quit"//c_null_char, c_null_ptr)


    menubar = g_menu_new ()
    menu = g_menu_new ()
    menu_edit = g_menu_new()
    menu_import = g_menu_new()

    menu_lens = g_menu_new()
    menu_macro = g_menu_new()
    menu_imagEval = g_menu_new()
    menu_wavefront = g_menu_new()

    call g_menu_append_submenu (menubar, "File"//c_null_char, menu)
    call addCommandMenuItem(menu, "New Lens", &
    & "NewLens", newLensCmd, win)    
    call addFuncMenuItem(menu, "Open .zoa File", "OpenZoa", c_funloc(open_zoa), win)

    call g_menu_append_submenu (menu, "Import"//c_null_char, menu_import)
    call g_menu_append_submenu (menubar, "Edit"//c_null_char, menu_edit)
    call g_menu_append_submenu (menubar, "Macro"//c_null_char, menu_macro)

    !act_macrooperations = g_simple_action_new("MacroOperations"//c_null_char, c_null_ptr)
    !call g_action_map_add_action (win, act_macrooperations)
    !call g_signal_connect (act_macrooperations, "activate"//c_null_char, c_funloc(zoa_macrooperationsUI), win)

    !menu_item_macrooperations = g_menu_item_new ("Macro Operations"//c_null_char, "win.MacroOperations"//c_null_char)



    call addCommandMenuItem(menu_macro, "Macro Operations", &
    & "MacroOperations", macroCmd, win)

    !call g_menu_append_item (menu_macro, menu_item_macrooperations)

    ! Save Macro
    !act_macrosave = g_simple_action_new("MacroSave"//c_null_char, c_null_ptr)
    !call g_action_map_add_action (win, act_macrosave)
    !call g_signal_connect (act_macrosave, "activate"//c_null_char, c_funloc(zoa_macrosaveUI), win)
    !menu_item_macrosave = g_menu_item_new ("Save Macro Directory"//c_null_char, "win.MacroSave"//c_null_char)
    !call g_menu_append_item (menu_macro, menu_item_macrosave)



    !Pseudocode for new type
    !addNewMenuItemThatExecutesCommand(topLevelMenu, menuitemText, menuItemEvenName, arrayOfCommands)


    ! Restore Macro
    !act_macrorestore = g_simple_action_new("MacroRestore"//c_null_char, c_null_ptr)
    !call g_action_map_add_action (win, act_macrorestore)
    !call g_signal_connect (act_macrorestore, "activate"//c_null_char, c_funloc(zoa_macrorestoreUI), win)
    !menu_item_macrorestore = g_menu_item_new ("Restore Macro Directory"//c_null_char, "win.MacroRestore"//c_null_char)
    !call g_menu_append_item (menu_macro, menu_item_macrorestore)

    ! Macro Manual
    !act_macromanual = g_simple_action_new("MacroManual"//c_null_char, c_null_ptr)
    !call g_action_map_add_action (win, act_macromanual)
    !call g_signal_connect (act_macromanual, "activate"//c_null_char, c_funloc(zoa_macromanualUI), win)
    !menu_item_macromanual = g_menu_item_new ("Open Macro Manual"//c_null_char, "win.MacroManual"//c_null_char)
    !call g_menu_append_item (menu_macro, menu_item_macromanual)

    !call addCommandMenuItem(menu_macro, "TestText", "TestEvent", tstTarget, win)
    !call addCommandMenuItem(menu_macro, "TestSecond", "TestSecondEvent", tstTarget2, win)

    !call tstMenuType%addCommandMenuItem(menu_macro, "TestText", "TestEvent", "TestCommand", win)
    call addCommandMenuItem(menu_imagEval, "Plot Astigmatism, Distortion, and Field Curvature", &
    & "PltAst", pltAst, win)

    call addCommandMenuItem(menu_imagEval, "Spot Diagram", &
    & "PltSPD", pltSpd, win)

    call addCommandMenuItem(menu_imagEval, "RMS Wavefront Error vs Field", &
    & "RMSFIELD", pltRms, win)

    call addCommandMenuItem(menu_imagEval, "Spot Size vs Field", &
    & "PltSPR", pltSPR, win)

    ! Lens Menu
    call g_menu_append_submenu (menubar, "Lens"//c_null_char, menu_lens)

    !Edit Lens
    call addCommandMenuItem(menu_edit, "System Configuration", &
    & "SYSCON", syscon, win)
    !act_sysconfig   = g_simple_action_new("EditSysConfig"//c_null_char, c_null_ptr)
    !call g_action_map_add_action (win, act_sysconfig)
    !call g_signal_connect (act_sysconfig, "activate"//c_null_char, c_funloc(editSysConfigUI), win)
    !menu_item_editsysconfig = g_menu_item_new ("System Configuration"//c_null_char, "win.EditSysConfig"//c_null_char)
    !call g_menu_append_item (menu_lens, menu_item_editsysconfig)

    act_editlensrad = g_simple_action_new("EditLensRad"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_editlensrad)
    call g_signal_connect (act_editlensrad, "activate"//c_null_char, c_funloc(zoa_editLensRadUI), win)
    menu_item_editlensrad = g_menu_item_new ("Edit Lens (Radius Mode)"//c_null_char, "win.EditLensRad"//c_null_char)
    call g_menu_append_item (menu_lens, menu_item_editlensrad)

    ! Lens Library
    act_lenslib = g_simple_action_new("LensLib"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_lenslib)
    call g_signal_connect (act_lenslib, "activate"//c_null_char, c_funloc(create_lenslibraryUI), win)
    menu_item_lenslib = g_menu_item_new ("Lens Library"//c_null_char, "win.LensLib"//c_null_char)
    call g_menu_append_item (menu_lens, menu_item_lenslib)


    ! Lens Sub Menus
    !call addFuncMenuItem(menu_lens, "Tst Draw Lens", "TstDrawLens", c_funloc(drawLens_activated), win)
    call addCommandMenuItem(menu_lens, "Draw Lens", &
    & "DrawLens", drawCmd, win)

    act_glassDisplay = g_simple_action_new("GlassDisplay"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_glassdisplay)
    call g_signal_connect (act_glassDisplay, "activate"//c_null_char, c_funloc (glassmanagerUI), win)
    menu_item_glass = g_menu_item_new ("Display Glass"//c_null_char, "win.GlassDisplay"//c_null_char)
    call g_menu_append_item (menu_lens, menu_item_glass)


    menu_paraxial = g_menu_new()

    call g_menu_append_submenu (menubar, "Paxaxial"//c_null_char, menu_paraxial)
    act_firstorder = g_simple_action_new("ParaFirstOrder"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_firstorder)

    call g_signal_connect (act_firstorder, "activate"//c_null_char, c_funloc(paraxfirstorder_activated), win)

    menu_item_firstorder = g_menu_item_new ("First Order Parameters"//c_null_char, "win.ParaFirstOrder"//c_null_char)
    call g_menu_append_item (menu_paraxial, menu_item_firstorder)

    call addCommandMenuItem(menu_paraxial, "Seidel Aberrations", &
    & "Seidel", seidelCmd, win)

    call g_menu_append_submenu (menubar, "Wavefront Analysis"//c_null_char, menu_wavefront)

    call addCommandMenuItem(menu_wavefront, "Zernike Coefficients vs Field", &
    & "ZernikeVsField", zernFldCmd, win)

    call addCommandMenuItem(menu_wavefront, "Optical Path Difference Plot", &
    & "OPDPlot", opdPltCmd, win)

    call addCommandMenuItem(menu_wavefront, "Transverse Aberration Ray Fans", &
    & "FANPlot", fanCmd, win)    

    call g_menu_append_submenu (menubar, "Image Evaluation"//c_null_char, menu_imagEval)


    section1 = g_menu_new ()
    section2 = g_menu_new ()
    section3 = g_menu_new ()
    section1_lens = g_menu_new()
    menu_item_quit = g_menu_item_new ("Quit"//c_null_char, "app.quit"//c_null_char)

    !call g_menu_item_set_attribute_value(menu_item_quit, "accel"//c_null_char, &
    !& g_variant_new_string("<Meta>q"//c_null_char))

    call g_signal_connect (act_quit, "activate"//c_null_char, c_funloc (quit_activated), app)


    call g_action_map_add_action (app, act_quit)
    call g_menu_append_item (section3, menu_item_quit)

    !call g_menu_append_item (section1_lens, menu_item_drawLens)

    call g_object_unref (menu_item_quit)

    call g_menu_append_section (menu, c_null_char, section1)
    call g_menu_append_section (menu, c_null_char, section3)

    !call g_menu_append_section (menu_lens, "Draw Lens"//c_null_char, section1_lens)

    !call addFuncMenuItem(menu_import, "CodeV .seq File", &
    !& "CV2PRG", c_funloc(ui_open_file), win)

    call addFuncMenuItem(menu_import, "CodeV .seq file", "ImpCodeV", c_funloc(import_codeV), win)
    call addFuncMenuItem(menu_import, "Zemax .zmx file", "ImpZemax", c_funloc(import_zemax), win)


    !call addCommandMenuItem(menu_import, "CodeV .seq File", &
    !& "CV2PRG", cv2prg, win)

    !PRINT *, "APP In Activate is ", app

    !call gtk_application_set_accels_for_action(app, &
    !& "app.quit"//c_null_char, "<Meta>q"//c_null_char//c_null_char)


    call gtk_application_set_menubar (app, menubar)


    call gtk_application_window_set_show_menubar (win, TRUE)

    !g_listenv


  end subroutine

  subroutine paraxfirstorder_activated(act, avalue, data) bind(c)
      type(c_ptr), value, intent(in) :: act, avalue, data

      character(len=100) :: ftext

      ftext = "OCDY"
      CALL PROCESKDP(ftext)


  end subroutine

  subroutine editSysConfigUI(act, avalue, win) bind(c)

     type(c_ptr), value, intent(in) :: act, avalue, win
     call PROCESKDP('SYSCON')

  end subroutine

  subroutine zoa_editLensRadUI(act, avalue, win) bind(c)
    type(c_ptr), value, intent(in) :: act, avalue, win

    call PROCESKDP('EDIT')

  end subroutine

  subroutine import_zemax(act, avalue, win) bind(c)
    use zoa_file_handler, only: getFileNameFromPath, getZemaxDir, getCodeVDir, setCodeVDir
    type(c_ptr), value, intent(in) :: act, avalue, win
    character(len=500) :: fileName
    character(len=500) :: cdir
    character(len=500) :: existingCodeVDir
    logical :: fileSelected


    fileSelected = ui_open_file(win, fileName, cdir, trim(getZemaxDir()), "*.zmx", "Zemax .zmx File")

    if (fileSelected) then

     PRINT *, "fileName is ", trim(fileName)
     PRINT *, "fileDirectory is is ", trim(cdir)

     ! Set CodeV Dir
     existingCodeVDir = getCodeVDir()
     call setCodeVDir(trim(cdir))
     CALL PROCESKDP('ZMX2PRG '//trim(getFileNameFromPath(trim(fileName))))
     call setCodeVDir(trim(existingCodeVDir))
    end if
    ! Restore CodeVDir

  end subroutine

  subroutine import_codev(act, avalue, win) bind(c)
    use zoa_file_handler, only: getFileNameFromPath, getCodeVDir, setCodeVDir
    type(c_ptr), value, intent(in) :: act, avalue, win
    character(len=500) :: fileName
    character(len=500) :: cdir
    character(len=500) :: existingCodeVDir
    logical :: fileSelected


    fileSelected = ui_open_file(win, fileName, cdir, trim(getCodeVDir()), "*.seq", "CodeV .seq File")

    if (fileSelected) then

     PRINT *, "fileName is ", trim(fileName)
     PRINT *, "fileDirectory is is ", trim(cdir)

     ! Set CodeV Dir
     existingCodeVDir = getCodeVDir()
     call setCodeVDir(trim(cdir))
     CALL PROCESKDP('CV2PRG '//trim(getFileNameFromPath(trim(fileName))))
     call setCodeVDir(trim(existingCodeVDir))
    end if
    ! Restore CodeVDir

  end subroutine

  subroutine open_zoa(act, avalue, win) bind(c)
    use zoa_file_handler, only: getFileNameFromPath, getFileSep, getProjectDir
    type(c_ptr), value, intent(in) :: act, avalue, win
    character(len=500) :: fileName
    character(len=500) :: cdir
    logical :: fileSelected

    integer :: n, ios
    character(len=256) :: line


    fileSelected = ui_open_file(win, fileName, cdir, trim(getProjectDir()), "*.zoa", "Zoa FIle")

    if (fileSelected) then

     PRINT *, "fileName is ", trim(fileName)
     PRINT *, "fileDirectory is is ", trim(cdir)


     ! Open File and Proces (TODO move this somewhere else)
     open(unit=99, file=trim(fileName), iostat=ios)
     if ( ios /= 0 ) stop "Error opening file "
 
     n = 0
 
     do
         read(99, '(A)', iostat=ios) line
         if (ios /= 0) then 
           call LogtermFOR("End of file?")
           return
         else
           PRINT *, "LINE IS "
           call LogTermFOR("LINE IS "//trim(line))
           call PROCESKDP(trim(line))
         end if
         n = n + 1
     end do      
     
    end if


  end subroutine  


  subroutine quit_activated (act, param, win) bind(c)
    type(c_ptr), value, intent(in) :: act, param, win

    print *, "QUIT!"
    call g_application_quit (app)
  end subroutine

  !addNewMenuItemThatExecutesCommand(topLevelMenu, menuitemText, menuItemEvenName, arrayOfCommands)
  subroutine genericMenuCommandCallback(act, param, gdata) bind(c)
    use gtk_sup

    type(c_ptr), value, intent(in) :: act, param, gdata
    character(len=80) :: fstring

    call C_F_string_ptr(gdata, fstring)

    CALL PROCESKDP(fstring)

  end subroutine

  subroutine addFuncMenuItem(topLevelMenu, menuItemText, menuItemEventName, funcPointer, win)
    integer :: numCommands
    type(c_ptr) :: topLevelMenu, win
    character(len=*) :: menuItemText, menuItemEventName
    type(c_funptr) :: funcPointer

    character(len=100), pointer :: ptr
    type(c_ptr) ::menuAction, menuItem



    menuAction = g_simple_action_new(menuItemEventName//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, menuAction)
    call g_signal_connect (menuAction, "activate"//c_null_char, funcPointer, c_null_ptr)
    !PRINT *, "menuItemEventName is ", menuItemEventName
    menuItem = g_menu_item_new (menuItemText//c_null_char, "win."//menuItemEventName//c_null_char)
    call g_menu_append_item (topLevelMenu, menuItem)

  end subroutine

  subroutine addCommandMenuItem(topLevelMenu, menuItemText, menuItemEventName, singleCommand, win)
    use g

    integer :: numCommands
    type(c_ptr) :: topLevelMenu, win
    character(len=*) :: menuItemText, menuItemEventName
    character(len=*), target, intent(in) :: singleCommand
    character(len=len(singleCommand)), pointer :: ptr
    type(c_ptr) ::menuAction, menuItem

    ptr =>singleCommand


    menuAction = g_simple_action_new(menuItemEventName//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, menuAction)
    call g_signal_connect (menuAction, "activate"//c_null_char, c_funloc(genericMenuCommandCallback), c_loc(ptr))
    !PRINT *, "menuItemEventName is ", menuItemEventName
    menuItem = g_menu_item_new (menuItemText//c_null_char, "win."//menuItemEventName//c_null_char)
    call g_menu_append_item (topLevelMenu, menuItem)

  end subroutine

  subroutine open_file(filename)

    character(len=120), intent(inout) :: filename
    integer(kind=c_int) :: isel
    character(len=120), dimension(:), allocatable :: chfile
    character(len=30), dimension(2) :: filters
    character(len=30), dimension(2) :: filtnames
    character(len=200) :: inln
    integer :: ios
    integer :: idxs

    filters(1) = "*.seq"
    filters(2) = "*.txt"
    filtnames(1) = "CodeV Sequence Files"
    filtnames(2) = "Plain Text"

    isel = hl_gtk_file_chooser_show(chfile, create=FALSE,&
         & title="Select input file"//c_null_char, filter=filters, &
         & filter_name=filtnames, wsize=(/ 600_c_int, 400_c_int /), &
         & edit_filters=TRUE, &
         & parent=app, all=TRUE)
    print *, "isel = hl_gtk_file_chooser_show=", isel
    if (isel == FALSE) return   ! No selection made

    filename = chfile(1)
    deallocate(chfile)

    !open(37, file=filename, action='read')
    !call hl_gtk_text_view_delete(tedit)
    !do
    !   read(37,"(A)",iostat=ios) inln
    !   if (ios /= 0) exit
    !   call hl_gtk_text_view_insert(tedit, (/ trim(inln)//c_new_line /))
    !end do
    !close(37)
    !idxs = index(filename, '/', .true.)+1
    !call gtk_window_set_title(window, trim(filename(idxs:))//c_null_char)

    ! We manually reset the changed flag as the text box signal handler sets it.

    !file_is_changed = .FALSE.

  end subroutine open_file


function ui_open_file(parent_window, filename, cdir, startFileDir, iptFilter, iptFilterName) result(fileSelected)
  use iso_c_binding
  use gtk_hl_chooser
  use zoa_file_handler, only : getCodeVDir
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

  isel = hl_gtk_file_chooser_show(chfile, cdir=cdir, create=FALSE,&
       & title="Select input file"//c_null_char, filter=filters, &
       & filter_name=filtnames, wsize=(/ 600_c_int, 400_c_int /), &
       & edit_filters=TRUE, initial_dir=startFileDir, &
       & parent=parent_window, all=TRUE)
  print *, "isel = hl_gtk_file_chooser_show=", isel
  if (isel == FALSE) then
    fileSelected = .FALSE.
    return
  else   ! No selection made

    filename = chfile(1)
    deallocate(chfile)
  end if

end function


end module zoamenubar
