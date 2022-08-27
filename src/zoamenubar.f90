
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
  & g_action_change_state, g_application_quit, g_simple_action_set_state


  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr, c_null_char, C_NEW_LINE
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

    type zoamenucommand
      type(c_ptr) :: menuItem, menuAction
      !character(len=*) :: menuItemText
      !character(len=*) :: menuItemEventName
      !character(len=*) :: arrayOfCommands(:)


    contains
      procedure, public :: addCommandMenuItem

    end type

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

    type(c_ptr), intent(in) :: win

    type(c_ptr) :: act_fullscreen, act_color, act_quit, display, provider
    type(c_ptr) :: menubar, menu, section1, section2, section3, &
      & menu_item_red, menu_item_green, &
      & menu_item_blue, menu_item_quit, menu_item_fullscreen, lb

    type(c_ptr) :: menu_lens, menu_item_drawLens, section1_lens, act_drawLens
    type(c_ptr) :: menu_paraxial, act_firstorder, menu_item_firstorder

    type(c_ptr) :: menu_macro, menu_item_macrooperations, act_macrooperations
    type(c_ptr) :: act_macrosave, menu_item_macrosave, menu_item_macrorestore, act_macrorestore
    type(c_ptr) :: act_macromanual, menu_item_macromanual

    type(c_ptr) :: act_editlensrad, menu_item_editlensrad

    type(zoamenucommand) :: tstMenuType
    character(len=100), target :: tstTarget = "TestCommandTarget"
    character(len=100), target :: tstTarget2 = "TestMemoryLoss"

    character(len=100), pointer :: tstPtr

    ! Menu Bar funcionality
    act_fullscreen = g_simple_action_new_stateful ("fullscreen"//c_null_char, &
                                  & c_null_ptr, g_variant_new_boolean (FALSE))

    act_color = g_simple_action_new_stateful ("color"//c_null_char, &
                                  & g_variant_type_new("s"//c_null_char), &
                                  & g_variant_new_string ("red"//c_null_char))

    act_quit = g_simple_action_new ("quit"//c_null_char, c_null_ptr)


    menubar = g_menu_new ()
    menu = g_menu_new ()

    menu_lens = g_menu_new()
    menu_macro = g_menu_new()

    call g_menu_append_submenu (menubar, "ZOA"//c_null_char, menu)

    call g_menu_append_submenu (menubar, "Macro"//c_null_char, menu_macro)

    act_macrooperations = g_simple_action_new("MacroOperations"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_macrooperations)
    call g_signal_connect (act_macrooperations, "activate"//c_null_char, c_funloc(zoa_macrooperationsUI), win)

    menu_item_macrooperations = g_menu_item_new ("Macro Operations"//c_null_char, "win.MacroOperations"//c_null_char)

    call g_menu_append_item (menu_macro, menu_item_macrooperations)

    ! Save Macro
    act_macrosave = g_simple_action_new("MacroSave"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_macrosave)
    call g_signal_connect (act_macrosave, "activate"//c_null_char, c_funloc(zoa_macrosaveUI), win)
    menu_item_macrosave = g_menu_item_new ("Save Macro Directory"//c_null_char, "win.MacroSave"//c_null_char)
    call g_menu_append_item (menu_macro, menu_item_macrosave)

    !Pseudocode for new type
    !addNewMenuItemThatExecutesCommand(topLevelMenu, menuitemText, menuItemEvenName, arrayOfCommands)


    ! Restore Macro
    act_macrorestore = g_simple_action_new("MacroRestore"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_macrorestore)
    call g_signal_connect (act_macrorestore, "activate"//c_null_char, c_funloc(zoa_macrorestoreUI), win)
    menu_item_macrorestore = g_menu_item_new ("Restore Macro Directory"//c_null_char, "win.MacroRestore"//c_null_char)
    call g_menu_append_item (menu_macro, menu_item_macrorestore)

    ! Macro Manual
    act_macromanual = g_simple_action_new("MacroManual"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_macromanual)
    call g_signal_connect (act_macromanual, "activate"//c_null_char, c_funloc(zoa_macromanualUI), win)
    menu_item_macromanual = g_menu_item_new ("Open Macro Manual"//c_null_char, "win.MacroManual"//c_null_char)
    call g_menu_append_item (menu_macro, menu_item_macromanual)

    ! Test new type
    !tstPtr => tstTarget
    !call tstMenuType%addCommandMenuItem(menu_macro, "TestText", "TestEvent", tstPtr, win)
    call tstMenuType%addCommandMenuItem(menu_macro, "TestText", "TestEvent", tstTarget, win)
    call tstMenuType%addCommandMenuItem(menu_macro, "TestSecond", "TestSecondEvent", tstTarget2, win)

    !call tstMenuType%addCommandMenuItem(menu_macro, "TestText", "TestEvent", "TestCommand", win)


    ! Lens Menu
    call g_menu_append_submenu (menubar, "Lens"//c_null_char, menu_lens)

    !Edit Lens
    act_editlensrad = g_simple_action_new("EditLensRad"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_editlensrad)
    call g_signal_connect (act_editlensrad, "activate"//c_null_char, c_funloc(zoa_editLensRadUI), win)
    menu_item_editlensrad = g_menu_item_new ("Edit Lens (Radius Mode)"//c_null_char, "win.EditLensRad"//c_null_char)
    call g_menu_append_item (menu_lens, menu_item_editlensrad)


    ! Lens Sub Menus

    act_drawLens = g_simple_action_new("DrawLens"//c_null_char, c_null_ptr)

    call g_action_map_add_action (win, act_drawLens)

    call g_signal_connect (act_drawLens, "activate"//c_null_char, c_funloc(drawLens_activated), win)

    menu_item_drawLens = g_menu_item_new ("Draw Lens"//c_null_char, "win.DrawLens"//c_null_char)

    call g_menu_append_item (menu_lens, menu_item_drawLens)




    menu_paraxial = g_menu_new()

    call g_menu_append_submenu (menubar, "Paxaxial"//c_null_char, menu_paraxial)
    act_firstorder = g_simple_action_new("ParaFirstOrder"//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, act_firstorder)

    call g_signal_connect (act_firstorder, "activate"//c_null_char, c_funloc(paraxfirstorder_activated), win)

    menu_item_firstorder = g_menu_item_new ("First Order Parameters"//c_null_char, "win.ParaFirstOrder"//c_null_char)

    call g_menu_append_item (menu_paraxial, menu_item_firstorder)


    section1 = g_menu_new ()
    section2 = g_menu_new ()
    section3 = g_menu_new ()
    section1_lens = g_menu_new()
    menu_item_fullscreen = g_menu_item_new ("Full Screen"//c_null_char, "win.fullscreen"//c_null_char)
    menu_item_red = g_menu_item_new ("Red"//c_null_char, "win.color::red"//c_null_char)
    menu_item_green = g_menu_item_new ("Green"//c_null_char, "win.color::green"//c_null_char)
    menu_item_blue = g_menu_item_new ("Blue"//c_null_char, "win.color::blue"//c_null_char)
    menu_item_quit = g_menu_item_new ("Quit"//c_null_char, "app.quit"//c_null_char)


    call g_signal_connect (act_fullscreen, "change-state"//c_null_char, c_funloc(fullscreen_changed), win)
    call g_signal_connect (act_color, "activate"//c_null_char, c_funloc (color_activated), win)
    call g_signal_connect (act_quit, "activate"//c_null_char, c_funloc (quit_activated), app)


    call g_action_map_add_action (win, act_fullscreen)
    call g_action_map_add_action (win, act_color)
    call g_action_map_add_action (app, act_quit)


    call g_menu_append_item (section1, menu_item_fullscreen)
    call g_menu_append_item (section2, menu_item_red)
    call g_menu_append_item (section2, menu_item_green)
    call g_menu_append_item (section2, menu_item_blue)
    call g_menu_append_item (section3, menu_item_quit)

    !call g_menu_append_item (section1_lens, menu_item_drawLens)

    call g_object_unref (menu_item_red)
    call g_object_unref (menu_item_green)
    call g_object_unref (menu_item_blue)
    call g_object_unref (menu_item_fullscreen)
    call g_object_unref (menu_item_quit)

    call g_object_unref (menu_item_drawLens)

    call g_menu_append_section (menu, c_null_char, section1)
    call g_menu_append_section (menu, "Color"//c_null_char, section2)
    call g_menu_append_section (menu, c_null_char, section3)

    !call g_menu_append_section (menu_lens, "Draw Lens"//c_null_char, section1_lens)


    !PRINT *, "APP In Activate is ", app
    call gtk_application_set_menubar (app, menubar)
    call gtk_application_window_set_show_menubar (win, TRUE)

    provider = gtk_css_provider_new ()
    display = gtk_widget_get_display (win)
    call gtk_css_provider_load_from_data (provider, "label#lb {background-color: red;}"//c_null_char, -1_c_size_t)
    call gtk_style_context_add_provider_for_display (display, provider, 0)



  end subroutine

  subroutine populatezoamenubar_old(win)

    type(c_ptr), intent(in) :: win

    type(c_ptr) :: act_fullscreen, act_color, act_quit, display, provider
    type(c_ptr) :: menubar, menu, section1, section2, section3, &
      & menu_item_red, menu_item_green, &
      & menu_item_blue, menu_item_quit, menu_item_fullscreen, lb

    type(c_ptr) :: menu_lens, menu_item_drawLens, section1_lens, act_drawLens
    type(c_ptr) :: menu_paraxial


    ! Menu Bar funcionality
    act_fullscreen = g_simple_action_new_stateful ("fullscreen"//c_null_char, &
                                  & c_null_ptr, g_variant_new_boolean (FALSE))

    act_color = g_simple_action_new_stateful ("color"//c_null_char, &
                                  & g_variant_type_new("s"//c_null_char), &
                                  & g_variant_new_string ("red"//c_null_char))

    act_quit = g_simple_action_new ("quit"//c_null_char, c_null_ptr)

    act_drawLens = g_simple_action_new("DrawLens"//c_null_char, c_null_ptr)

    menubar = g_menu_new ()
    menu = g_menu_new ()
    menu_lens = g_menu_new()
    menu_paraxial = g_menu_new()

    section1 = g_menu_new ()
    section2 = g_menu_new ()
    section3 = g_menu_new ()
    section1_lens = g_menu_new()
    menu_item_fullscreen = g_menu_item_new ("Full Screen"//c_null_char, "win.fullscreen"//c_null_char)
    menu_item_red = g_menu_item_new ("Red"//c_null_char, "win.color::red"//c_null_char)
    menu_item_green = g_menu_item_new ("Green"//c_null_char, "win.color::green"//c_null_char)
    menu_item_blue = g_menu_item_new ("Blue"//c_null_char, "win.color::blue"//c_null_char)
    menu_item_quit = g_menu_item_new ("Quit"//c_null_char, "app.quit"//c_null_char)

    menu_item_drawLens = g_menu_item_new ("Draw Lens"//c_null_char, "win.DrawLens"//c_null_char)

    call g_signal_connect (act_fullscreen, "change-state"//c_null_char, c_funloc(fullscreen_changed), win)
    call g_signal_connect (act_color, "activate"//c_null_char, c_funloc (color_activated), win)
    call g_signal_connect (act_quit, "activate"//c_null_char, c_funloc (quit_activated), app)

    call g_signal_connect (act_drawLens, "activate"//c_null_char, c_funloc(drawLens_activated), win)

    call g_action_map_add_action (win, act_fullscreen)
    call g_action_map_add_action (win, act_color)
    call g_action_map_add_action (app, act_quit)

    call g_action_map_add_action (win, act_drawLens)

    call g_menu_append_item (section1, menu_item_fullscreen)
    call g_menu_append_item (section2, menu_item_red)
    call g_menu_append_item (section2, menu_item_green)
    call g_menu_append_item (section2, menu_item_blue)
    call g_menu_append_item (section3, menu_item_quit)

    call g_menu_append_item (section1_lens, menu_item_drawLens)

    call g_object_unref (menu_item_red)
    call g_object_unref (menu_item_green)
    call g_object_unref (menu_item_blue)
    call g_object_unref (menu_item_fullscreen)
    call g_object_unref (menu_item_quit)

    call g_object_unref (menu_item_drawLens)

    call g_menu_append_section (menu, c_null_char, section1)
    call g_menu_append_section (menu, "Color"//c_null_char, section2)
    call g_menu_append_section (menu, c_null_char, section3)
    call g_menu_append_submenu (menubar, "ZOA"//c_null_char, menu)

    call g_menu_append_section (menu_lens, "Draw Lens"//c_null_char, section1_lens)
    call g_menu_append_submenu (menubar, "Lens"//c_null_char, menu_lens)


    !PRINT *, "APP In Activate is ", app
    call gtk_application_set_menubar (app, menubar)
    call gtk_application_window_set_show_menubar (win, TRUE)

    provider = gtk_css_provider_new ()
    display = gtk_widget_get_display (win)
    call gtk_css_provider_load_from_data (provider, "label#lb {background-color: red;}"//c_null_char, -1_c_size_t)
    call gtk_style_context_add_provider_for_display (display, provider, 0)



  end subroutine
  !*************************************************
  ! The three callback functions of the menu items:
  !*************************************************
  subroutine paraxfirstorder_activated(act, avalue, data) bind(c)
      type(c_ptr), value, intent(in) :: act, avalue, data

      character(len=100) :: ftext

      ftext = "OCDY"
      CALL PROCESKDP(ftext)


  end subroutine

  subroutine zoa_editLensRadUI(act, avalue, win) bind(c)
    type(c_ptr), value, intent(in) :: act, avalue, win

    call PROCESKDP('EDIT')

  end subroutine

  subroutine drawLens_activated (act, avalue, win) bind(c)
    type(c_ptr), value, intent(in) :: act, avalue, win


    character(len=100) :: ftext
     !PRINT *, "Callback working!"

     !ipick = hl_gtk_file_chooser_show(new_files, &
     !       & create=FALSE, multiple=TRUE, filter=["image/*"], &
     !       & parent=my_window, all=TRUE)

     !ftext = 'LIB GET 1 '
     ftext = 'CV2PRG DoubleGauss.seq'
     CALL PROCESKDP(ftext)
     ftext = 'COLORSET RAYS 2'
     CALL PROCESKDP(ftext)
     ftext = 'VIECO'
     CALL PROCESKDP(ftext)

  end subroutine drawLens_activated


  subroutine fullscreen_changed (act, avalue, win) bind(c)
    !use handlers

    type(c_ptr), value, intent(in) :: act, avalue, win
    logical :: state

    ! This is the correct way to convert a boolean integer to logical.
    state = transfer(g_variant_get_boolean (avalue), state)

    if (state) then
      call gtk_window_maximize (win)
    else
      call gtk_window_unmaximize (win)
    end if
    call g_simple_action_set_state (act, avalue)
  end subroutine

  subroutine color_activated (act, param, win) bind(c)
    type(c_ptr), value, intent(in) :: act, param, win
    type(c_ptr) :: param_string
    character(kind=c_char, len=40):: color, color_str

    ! Get the C string from param
    param_string = g_variant_get_string (param, c_null_ptr)

    ! Convert it to a Fortran string
    call convert_c_string(param_string, color_str)
    print *, "The color will be: "//trim(color_str)//"!"

    color = "label#lb {background-color: "//trim(color_str)//"; }"//c_null_char

    print *, "Let's change the color!"
    !call gtk_css_provider_load_from_data (provider, color, -1_int64)
    !call g_action_change_state (act, param)
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
    !call convert_c_string_scalar(gdata, fstring)

    !character, pointer :: fstring(:)
    !call c_f_pointer(gdata, fstring, [5])

    print *, "Test!"
    print *, "fstring is ", fstring

  end subroutine

  subroutine addCommandMenuItem(self, topLevelMenu, menuItemText, menuItemEventName, singleCommand, win)
    use g

    class(zoamenucommand) :: self
    integer :: numCommands
    type(c_ptr) :: topLevelMenu, win
    character(len=*) :: menuItemText, menuItemEventName
    character(len=*), target, intent(in) :: singleCommand
    character(len=80), pointer :: ptr
    ! Working
  !  character(len=*), pointer :: singleCommand


    !character(len=*), intent(in) :: singleCommand

    !character(len=80), target :: stringOutput
    !character(len=80), pointer :: ptr

    !stringOutput = trim(singleCommand)
    ptr =>singleCommand

    !PRINT *, "String Output is ", stringOutput


    !type(c_ptr),  target :: arrayOutput

    !act_macrorestore = g_simple_action_new("MacroRestore"//c_null_char, c_null_ptr)
    !call g_action_map_add_action (win, act_macrorestore)
    !call g_signal_connect (act_macrorestore, "activate"//c_null_char, c_funloc(zoa_macrorestoreUI), win)
    !menu_item_macrorestore = g_menu_item_new ("Restore Macro Directory"//c_null_char, "win.MacroRestore"//c_null_char)
    !call g_menu_append_item (menu_macro, menu_item_macrorestore)

    !self%menuItem = g_menu_new()
    !call g_menu_append_submenu (topLevelMenu, menuItemText//c_null_char, self%menuItem)


    !!
    self%menuAction = g_simple_action_new(menuItemEventName//c_null_char, c_null_ptr)
    call g_action_map_add_action (win, self%menuAction)
    !Working
    !call g_signal_connect (self%menuAction, "activate"//c_null_char, c_funloc(genericMenuCommandCallback), c_loc(singleCommand))
    call g_signal_connect (self%menuAction, "activate"//c_null_char, c_funloc(genericMenuCommandCallback), c_loc(ptr))
    PRINT *, "menuItemEventName is ", menuItemEventName
    self%menuItem = g_menu_item_new (menuItemText//c_null_char, "win."//menuItemEventName//c_null_char)
    call g_menu_append_item (topLevelMenu, self%menuItem)

  end subroutine

end module zoamenubar
