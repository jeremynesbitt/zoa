module handlers
  use global_widgets
  use GLOBALS
  !use lens_analysis
  use gtk

  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending, &
  & g_object_set_property, g_value_set_interned_string, g_variant_new_boolean, &
  & g_menu_new, g_menu_item_new, g_action_map_add_action, g_menu_append_item, &
  & g_object_unref, g_menu_append_section, g_menu_append_submenu, &
  & g_simple_action_new_stateful, g_variant_type_new, g_simple_action_new, &
  & g_variant_get_boolean, g_variant_get_string, g_variant_new_string, &
  & g_action_change_state, g_application_quit, g_simple_action_set_state, &
  & g_signal_override_class_handler, g_app_info_launch_default_for_uri, &
  & g_app_info_launch, g_app_info_get_default_for_type, g_file_new_for_uri


  use, intrinsic :: iso_c_binding
  use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width

  use plplot, PI => PL_PI
  use plplot_extra
  use gtk_draw_hl
  use gtk_hl_container
  use gtk_hl_button
  use zoa_ui

  use gtk_hl_chooser
  use zoa_tab
  use zoa_tab_manager

  implicit none
  type(c_ptr)    :: my_window, entry,  provider
  ! For updating UI during long computations
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  ! Command history vars
  integer, parameter :: cmdHistorySize = 500
  character(len=256), dimension(cmdHistorySize) ::  command_history
  integer :: command_index = 1
  integer :: command_search = 1
  integer :: terminalOutput = ID_TERMINAL_DEFAULT


  type(zoatabManager) :: zoatabMgr



contains

  subroutine dispHelpScreen(widget, event, gdata) bind(c)
    use zoa_file_handler, only: openHelpFile
    use type_utils, only: int2str
    !use type_utils, only: int2str
    !use zoa_file_handler, only: getFileSep
    !use hl_gtk_zoa, only : hl_zoa_text_view_new
   
    type(c_ptr), value, intent(in) :: widget, event, gdata
    !type(c_ptr) :: helpwin, linkbutton, helpbox, helpbuff, helpview
    !type(c_ptr) :: short1, shortControl
    !character(len=1024) :: manPath
    
    ! Temp vars
    ! type(c_ptr) :: ptr_macbundledir
    ! character(len=1024) :: str_bundle_dir
    ! character(len=1024) :: helpfilePath
    ! integer(kind=c_int) :: browserResult

    
    call openHelpFile()



    ! helpwin = gtk_window_new()
    ! call gtk_window_set_default_size(helpwin, 300,200)
    ! helpbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int);
    ! !helpview = gtk_text_view_new ();
    ! helpview = hl_zoa_text_view_new ();

    ! helpbuff = gtk_text_view_get_buffer (helpview);

    ! call gtk_text_buffer_set_text(helpbuff, "Zoa has no integrated help system." &
    ! & //c_new_line//"The KDP-2 manual is the best resource to learn "&
    ! & //c_new_line //"commands.  In addition, the macro examples are a " &
    ! & //c_new_line// "good way to understand how the program works." &
    ! & //c_new_line//"Online link below.  local copy should have been installed here:" &
    ! & //c_new_line//trim(basePath)//"Manuals"//getFileSep() &
    ! & //c_new_line,-1)


    ! linkButton = gtk_link_button_new( &
    ! &"https://github.com/jnez137/zoa/blob/main/Library/Manuals/Manual.pdf" &
    ! & //c_null_char)
    ! call g_signal_connect(linkButton, 'activate-link', c_funloc(open_url), linkButton)

    ! call LogTermFOR("Testing123!")
    ! ptr_macbundledir = get_macos_bundle_dir()
    ! print *, "Ptr Loc is ", LOC(ptr_macbundledir)
    ! call convert_c_string(ptr_macbundledir, str_bundle_dir)
    ! helpfilePath = 'file:'//getFileSep()//getFileSep()//getFileSep()// &
    ! & trim(str_bundle_dir)//getFileSep()//'Resources'//getFileSep()// &
    ! & 'help'//getFileSep()//'html'//getFileSep()//'index.html'
    
    ! call LogTermFOR("Loc is "//trim(helpfilePath))   
    ! browserResult =  browser_open_url(trim(helpfilePath))
    ! call LogTermFOR("Browser Result is "//int2str(browserResult))


    ! call gtk_box_append(helpbox, helpview)
    ! call gtk_box_append(helpbox, linkbutton)

    ! call gtk_window_set_child(helpwin, helpbox)
    ! call gtk_window_present(helpwin)


  end subroutine
  ! Our callback function before destroying the window:
  subroutine destroy_signal(widget, event, gdata) bind(c)
    use zoa_file_handler, only: saveCommandHistoryToFile
    type(c_ptr), value, intent(in) :: widget, event, gdata

    print *, "Your destroy_signal() function has been invoked !"
    ! Some functions and subroutines need to know that it's finished:
    run_status = FALSE
    call saveCommandHistoryToFile(command_history, command_index)
    call PROCESKDP('EXIT')
    call gtk_window_destroy(my_window)
  end subroutine destroy_signal

  ! This function is needed to update the GUI during long computations.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  recursive subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
      !PRINT *, "BOOLRESULT IS ", boolresult
    end do

  end subroutine pending_events

  subroutine updateTerminalLog(ftext, txtColor)
      USE GLOBALS
      use global_widgets

      IMPLICIT NONE

      character(len=*), intent(in) :: ftext
      character(len=*), intent(in)  :: txtColor

      type(gtktextiter), target :: endIter
      logical :: scrollResult
      type(c_ptr) ::  scroll_win, vAdj
      type(c_ptr) ::  txtBuffer
      character(len=280) :: markup
      integer(kind=c_int) :: iotst



      ! This routine is to update the terminal log, and is
      ! abstracted in case the method (font color, bold) needs to be changed

      ! The way implemented is to use the pango / markup interface
      ! See for some examples
      ! https://basic-converter.proboards.com/thread/314/pango-markup-text-examples


      txtBuffer = gtk_text_view_get_buffer(ioConfig%textView)
      call gtk_text_buffer_get_end_iter(txtBuffer, c_loc(endIter))

    if (ftext.ne."  ") THEN

      markup ="<span foreground='"//trim(txtColor)// &
      & "'>"//trim(ftext)//"</span>"//C_NEW_LINE//c_null_char//c_null_char
      ! Added this because for some strings, gtk would throw a parsing error
      ! Could not fiure out the pattern but found that if I don't let gtk calc the
      ! size of the buffer it seems to work every time
      inquire(iolength=iotst) trim(markup)
       call gtk_text_buffer_insert_markup(txtBuffer, c_loc(endIter), trim(markup), iotst)      
     
    END IF

      ! After some trial and error, here is how to make sure after the user enter a
      ! command that it shows the output at the bottom of the screen.

      ! Make sure the window has been updated from the insert command above
      ! note: this is necessary based on testing
      call pending_events ()

      if (TEST_MODE) call dumpToFile(trim(ftext))

      ! Get scroll window and v adjustment
      scroll_win = gtk_widget_get_parent(ioConfig%textView)
      vAdj = gtk_scrolled_window_get_vadjustment(scroll_win)

      ! Set value to be upper limit - page size
      call gtk_adjustment_set_value(vAdj, gtk_adjustment_get_upper(vAdj) - &
      & gtk_adjustment_get_page_size(vAdj))
      
      !call pending_events()
      !PRINT *, "End of updateterminallog"

  end subroutine

  subroutine name_enter(widget, data) bind(c)


         USE GLOBALS
         use global_widgets
        IMPLICIT NONE

    type(c_ptr), value :: widget, data
    type(c_ptr) :: buff2, tag, tagTable, gBool
    type(c_ptr) :: page, enter, iterPtr, gColor
    type(gtktextiter), target :: iter, startIter, endIter
    character(len=100) :: ftext
    character(len=20)  :: txtColor

    if (c_associated(data)) then
       entry = data
    else
       entry = widget
    end if

    buff2 = gtk_entry_get_buffer(entry)
    call c_f_string_copy(gtk_entry_buffer_get_text(buff2), ftext)
    !print *, "Entered name as:",trim(ftext)


    
    ! Log results
    txtColor = "blue"
    call updateTerminalLog(ftext, txtColor)

    ! Update command history for a simple way for the user to get previous commands
    if (txtColor.eq."blue") then
      if (command_index.LT.cmdHistorySize+1) THEN

       command_history(command_index) = ftext
       command_index = command_index + 1
       command_search = command_index
     else ! array full
       command_history(1:cmdHistorySize-1) = command_history(2:cmdHistorySize)
       command_history(cmdHistorySize) = ftext

     END IF
   END IF

    ! Clear Input
    call gtk_entry_buffer_set_text(buff2, c_null_char,-1_c_int)

    !call pending_events()

    ! Finally actually process the command
    CALL PROCESKDP(ftext)



  end subroutine name_enter

  subroutine load_css()
    use gdk, only: gdk_display_get_default
    type(c_ptr) :: provider

    provider = gtk_css_provider_new()
    call gtk_css_provider_load_from_resource(provider, "/zoa/optical-analysis/zoa.css"//c_null_char)

    
    call gtk_style_context_add_provider_for_display(gdk_display_get_default(), provider, 600_c_int)

  end subroutine

  subroutine activate(app2, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_f_pointer, c_null_funptr
    use zoa_file_handler
    use hl_gtk_zoa, only : hl_zoa_text_view_new, createNullTerminatedCString
    use GLOBALS
    use zoamenubar
    use hl_gtk_zoa
    use zoa_status_bar
    use gdk

    implicit none
    type(c_ptr), value, intent(in)  :: gdata, app2
    ! Pointers toward our GTK widgets:
    type(c_ptr)    :: scroll_win_detach, pane
    type(c_ptr)    :: table, button2, button3, box1, scroll_ptr
    type(c_ptr)    :: entry2, keyevent, controller_k
    type(c_ptr)    :: toggle1, expander, notebookLabel1, notebookLabel2
    type(c_ptr)    :: linkButton, iterPtr, iterGUI, notebookLabel3
    integer(c_int) :: message_id, firstTab, secondTab, thirdTab
    type(c_ptr) :: act_fullscreen, act_color, act_quit, display
    type(c_ptr) :: menubar, menu, section1, section2, section3, &
      & menu_item_red, menu_item_green, &
      & menu_item_blue, menu_item_quit, menu_item_fullscreen
    logical :: tstResult
  type(c_ptr) :: quit_action, theme



    ! Create the window:
    my_window = gtk_application_window_new(app)

    call load_css()

  !/* quit */
    quit_action = g_simple_action_new("quit"//c_null_char, c_null_ptr);
    call g_signal_connect(quit_action, "activate"//c_null_char, &
		 &  c_funloc(destroy_signal), app)
    call g_action_map_add_action(app,quit_action)

    !call gtk_application_set_accels_for_action(app, "app.quit"//c_null_char, c_ptr_array)
    if (ID_SYSTEM.EQ.ID_OS_MAC) then
    call gtk_application_set_accels_for_action(app, "app.quit"//c_null_char, &
    & createNullTerminatedCString("<meta>q"//c_null_char))
  elseif (ID_SYSTEM == ID_OS_WINDOWS ) then
    call gtk_application_set_accels_for_action(app, "app.quit"//c_null_char, &
    & createNullTerminatedCString("<ctrl>q"//c_null_char))
  end if



    call g_signal_connect(my_window, "destroy"//c_null_char, &
                        & c_funloc(destroy_signal))

    call gtk_window_set_title(my_window, "Zoa Optical Analysis"//c_null_char)

    width  = 1000
    height = 700
    call gtk_window_set_default_size(my_window, width, height)

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    ! The four buttons:

    button2 = gtk_button_new_with_mnemonic ("_Help"//c_null_char)
    call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(dispHelpScreen))
    button3 = gtk_button_new_with_mnemonic ("_Exit"//c_null_char)
    call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(destroy_signal))

    ! A clickable URL link:
    !linkButton = gtk_link_button_new_with_label ( &
    !                      &"http://www.ecalculations.com"//c_null_char,&
    !                      &"More on KDP2"//c_null_char)

    ! A table container will contain buttons and labels:
    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)

    call gtk_grid_attach(table, button2, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, button3, 3_c_int, 0_c_int, 1_c_int, 1_c_int)



    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5_c_int)
    call gtk_box_append(box1, table)
    call gtk_box_append(box1, createMenuIconBar(my_window))


    PRINT *, "Create Notebook Object"
    notebook = gtk_notebook_new ()
    call gtk_widget_set_vexpand (notebook, TRUE)


    ! Theorecticzlly needed for detaching (detaching not working at present).
    call gtk_notebook_set_group_name(notebook,"0"//c_null_char)


   
     textView = hl_zoa_text_view_new()
     call gtk_text_view_set_editable(textView, FALSE)

     ! Set a monospaced font so table output looks reasonable
     call gtk_text_view_set_monospace(textView, 1_c_int) 
    !
     buffer = gtk_text_view_get_buffer (textView)
     ! This is done for some of the ui options, so we can funnel the output of a KDP
     ! command to a different textView when convienent.
     call ioConfig%registerTextView(textView, ID_TERMINAL_DEFAULT)
     call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

     scroll_win_detach = gtk_scrolled_window_new()
     notebookLabel2 = gtk_label_new_with_mnemonic("_Messages"//c_null_char)

    ! TODO:  Width of the two windows should be abstracted somewhere
     call gtk_scrolled_window_set_child(scroll_win_detach, textView)
     call gtk_widget_set_size_request(scroll_win_detach, 200_c_int, -1_c_int)
     call gtk_widget_set_size_request(notebook, 500_c_int, -1_c_int)


    call zoatabMgr%addMsgTab(notebook, "Messages")
    !zoatabMgr%buffer = buffer

    notebookLabel3 = gtk_label_new_with_mnemonic("_Intro"//c_null_char)
    scroll_ptr = gtk_scrolled_window_new()

    call populateSplashWindow(scroll_ptr)


    thirdTab  = gtk_notebook_append_page (notebook, scroll_ptr, notebookLabel3)

    ! Having an issue where every time I try to detach a tab the main window
    ! freezes, so disabling this for now.  Believe it is related to this bug:
    !call gtk_notebook_set_tab_detachable(notebook, scroll_ptr, TRUE)

    !call g_signal_connect(notebook, 'create-window'//c_null_char, c_funloc(detachTabTst), c_null_ptr)


    pane = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)
    call gtk_paned_set_start_child(pane, scroll_win_detach)
    call gtk_paned_set_end_child(pane, notebook)



    call gtk_scrolled_window_set_min_content_width(scroll_win_detach, 200_c_int)


    call gtk_box_append(box1,pane)
    !call gtk_scrolled_window_set_min_content_width(notebook, 700_c_int)

    call gtk_widget_set_hexpand(notebook, TRUE)

    call gtk_widget_set_vexpand (box1, TRUE)

    ! CMD Entry TextBox

    entry = hl_gtk_entry_new(60_c_int, editable=TRUE, tooltip = &
         & "Enter text here for command interpreter"//c_null_char, &
         & activate=c_funloc(name_enter))
    
    ! This does not work and I am not sure why.       
    !call addMacOSClipboardShortcuts(entry)
         
    call readCommandHistoryFromFile(command_history, command_index)
    command_search = command_index
    PRINT *, "command_index is ", command_index
    PRINT *, "command history at command index is ", command_history(command_index)
    IF(command_index.GT.1) PRINT *, "prior command is ",command_history(command_index-1)

    !tmp code to test key pressed event
    controller_k = gtk_event_controller_key_new()



    call gtk_widget_add_controller(entry, controller_k)
    call g_signal_connect(controller_k, "key-pressed"//c_null_char, &
               & c_funloc(key_event_h), c_null_ptr)

    !call gtk_widget_set_focusable(entry2, TRUE)
    !call gtk_widget_set_can_focus(entry2, FALSE)
    !call gtk_widget_set_focus_on_click(entry2, FALSE)
    !call gtk_widget_grab_focus(entry2)

    call gtk_box_append(box1, entry)
    call gtk_box_append(box1, createStatusBar())
    !all gtk_box_append(box1, entry2)
    call gtk_widget_set_vexpand (box1, TRUE)


    call gtk_window_set_interactive_debugging(TRUE)
    call populatezoamenubar(my_window)


    ! Let's finalize the GUI:
    call gtk_window_set_child(my_window, box1)
    call gtk_window_set_mnemonics_visible (my_window, TRUE)
    call gtk_widget_show(my_window)

    call gtk_window_present (my_window)

    ! INIT KDP
    CALL INITKDP
    call refreshLensDataStruct()

    PRINT *, "DONE WITH INITKDP!"

    theme = gtk_icon_theme_get_for_display(gdk_display_get_default())
    call gtk_icon_theme_add_resource_path(theme, "/zoa/optical-analysisicons"//c_null_char)


  end subroutine activate

subroutine populateSplashWindow(splashWin)
  use GLOBALS, only: zoaVersion
  implicit none
  type(c_ptr), intent(inout) :: splashWin
  type(c_ptr) :: view, splashBuff, box3, linkbutton
  integer :: uriResult

    type(gtktextiter), target :: endIter
    integer :: i

    box3 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int);
    view = gtk_text_view_new ();
    call gtk_text_view_set_justification(view, GTK_JUSTIFY_CENTER)

    splashBuff = gtk_text_view_get_buffer (view);

    call gtk_text_buffer_get_end_iter(splashBuff, c_loc(endIter))


    call gtk_text_buffer_get_end_iter(splashBuff, c_loc(endIter))

    call gtk_text_buffer_insert_markup(splashBuff, c_loc(endIter), &
       & c_new_line//'<span foreground="blue" size="x-large"> &
       &  Zoa Optical Analysis</span>'//C_NEW_LINE// &
       &  c_new_line//'<span foreground="blue" size="large"> &
       &  Version '//zoaVersion// ' (beta)</span>' &
       &  //c_null_char, -1_c_int)
    PRINT *, "Post Markup"

    call gtk_text_buffer_get_end_iter(splashBuff, c_loc(endIter))

    !call gtk_text_buffer_set_text(splashBuff, "Zoa Optical Analysis" &
    !& //c_new_line//c_new_line//c_new_line//"https://github.com/jnez137/zoa" &
    !& //c_new_line//c_null_char,-1)

    !call gtk_about_dialog_set_website(dialog, &
    !              & "https://github.com/vmagnin/gtk-fortran/wiki"//c_null_char)
    !call gtk_text_buffer_get_end_iter(splashBuff, c_loc(endIter))

    !call gtk_text_buffer_insert_at_cursor(splashBuff, c_new_line//"Hello, this is some text",-1)

    ! A clickable URL link:
    linkButton = gtk_link_button_new( &
                          &"https://github.com/jnez137/zoa"//c_null_char)
    call g_signal_connect(linkButton, 'activate-link', c_funloc(open_url), linkButton)
    !uriResult = g_app_info_launch_default_for_uri("https://github.com/jnez137/zoa"//c_null_char, c_null_ptr, c_null_ptr)
    call gtk_box_append(box3, view)
    call gtk_box_append(box3, linkbutton)

    !call gtk_scrolled_window_set_child(splashWin, view)
    call gtk_scrolled_window_set_child(splashWin, box3)




end subroutine

  subroutine detachTabTst(parent_notebook, widget) bind(c)
        implicit none
        type(c_ptr), value :: widget, parent_notebook
        type(c_ptr) :: newwin, newnotebook, box2, scrolled_win
        type(c_ptr) :: child, newlabel
        integer :: newtab
        !PRINT *, "Detach Event Called!"

        !PRINT *, "widget is ", widget
        !PRINT *, "Parent Window is ", parent_notebook

        newwin = gtk_window_new()

        call gtk_window_set_default_size(newwin, 1300, 700)

        !call gtk_window_set_transient_for(newwin, my_window)
        call gtk_window_set_destroy_with_parent(newwin, TRUE)
        box2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int);
        newnotebook = gtk_notebook_new()
        call gtk_widget_set_vexpand (newnotebook, TRUE)
        !# handler for dropping outside of current window
        !call hl_gtk_scrolled_window_add(newwin, newnotebook)

        call gtk_notebook_set_group_name(newnotebook,"1"//c_null_char)
        child = gtk_notebook_get_nth_page(parent_notebook, -1_c_int)
        !newlabel = gtk_notebook_get_tab_label(parent_notebook, widget)
        call gtk_notebook_detach_tab(parent_notebook, child)
        scrolled_win = gtk_scrolled_window_new()
        call gtk_scrolled_window_set_child(scrolled_win, child)

        !newtab = gtk_notebook_append_page(newnotebook, widget, newlabel)
        newtab = gtk_notebook_append_page(newnotebook, scrolled_win, newlabel)
        call gtk_notebook_set_tab_detachable(newnotebook, scrolled_win, TRUE)
        call gtk_box_append(box2, newnotebook)

        call gtk_window_set_child(newwin, box2)

        !call gtk_window_set_child(my_window, newwin)


        !call gtk_window_set_child(newwin, widget)

        call gtk_widget_set_vexpand (box2, TRUE)


        !call gtk_widget_show(newwin)
        !call gtk_widget_show(parent_notebook)
        !call pending_events()
        call gtk_window_present(newwin)

        PRINT *, "Modal ? ", gtk_window_get_modal(newwin)

        !call gtk_notebook_set_current_page(parent_notebook, 0_c_int)
        !call gtk_notebook_set_tab_pos(parent_notebook, 0_c_int)
        !call g_object_unref(widget)
        !call gtk_window_set_transient_for(my_window)
        !call pending_events()

        !window = Gtk.Window()
        !new_notebook = Gtk.Notebook()
        !window.add(new_notebook)
        ! new_notebook.set_group_name('0') # very important for DND
        ! new_notebook.connect('page-removed', self.notebook_page_removed, window)
        ! window.connect('destroy', self.sub_window_destroyed, new_notebook, notebook)
        ! window.set_transient_for(self.window)
        ! window.set_destroy_with_parent(True)
        ! window.set_size_request(400, 400)
        ! window.move(x, y)
        ! window.show_all()
        ! return new_notebook

  end subroutine


! Adapted from https://stackoverflow.com/questions/56377755/given-a-gtk-notebook-how-does-one-drag-and-drop-a-page-to-a-new-window

  subroutine detachTab(parent_notebook, widget) bind(c)
        type(c_ptr), value :: widget, parent_notebook
        type(c_ptr) :: newwin, newnotebook, newlabel, box2, scrolled_win
        integer :: newtab
        !PRINT *, "Detach Event Called!"

        !PRINT *, "widget is ", widget
        !PRINT *, "Parent Window is ", parent_notebook

        newwin = gtk_window_new()

        call gtk_window_set_default_size(newwin, 1300, 700)

        !call gtk_window_set_transient_for(newwin, my_window)
        call gtk_window_set_destroy_with_parent(newwin, TRUE)
        box2 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0_c_int);
        newnotebook = gtk_notebook_new()
        call gtk_widget_set_vexpand (newnotebook, TRUE)
        !# handler for dropping outside of current window
        !call hl_gtk_scrolled_window_add(newwin, newnotebook)

        call gtk_notebook_set_group_name(newnotebook,"1"//c_null_char)
        newlabel = gtk_notebook_get_tab_label(parent_notebook, widget)
        call gtk_notebook_detach_tab(parent_notebook, widget)
        scrolled_win = gtk_scrolled_window_new()
        call gtk_scrolled_window_set_child(scrolled_win, widget)

        !newtab = gtk_notebook_append_page(newnotebook, widget, newlabel)
        newtab = gtk_notebook_append_page(newnotebook, scrolled_win, newlabel)
        call gtk_notebook_set_tab_detachable(newnotebook, scrolled_win, TRUE)
        call gtk_box_append(box2, newnotebook)

        call gtk_window_set_child(newwin, box2)

        !call gtk_window_set_child(my_window, newwin)


        !call gtk_window_set_child(newwin, widget)

        call gtk_widget_set_vexpand (box2, TRUE)


        !call gtk_widget_show(newwin)
        !call gtk_widget_show(parent_notebook)
        !call pending_events()
        call gtk_window_present(newwin)

        PRINT *, "Modal ? ", gtk_window_get_modal(newwin)

        !call gtk_notebook_set_current_page(parent_notebook, 0_c_int)
        !call gtk_notebook_set_tab_pos(parent_notebook, 0_c_int)
        !call g_object_unref(widget)
        !call gtk_window_set_transient_for(my_window)
        !call pending_events()

        !window = Gtk.Window()
        !new_notebook = Gtk.Notebook()
        !window.add(new_notebook)
        ! new_notebook.set_group_name('0') # very important for DND
        ! new_notebook.connect('page-removed', self.notebook_page_removed, window)
        ! window.connect('destroy', self.sub_window_destroyed, new_notebook, notebook)
        ! window.set_transient_for(self.window)
        ! window.set_destroy_with_parent(True)
        ! window.set_size_request(400, 400)
        ! window.move(x, y)
        ! window.show_all()
        ! return new_notebook

  end subroutine

  function open_url(widget, linkButton) result(boolGood) bind(c)
    type(c_ptr) :: widget
    type(c_ptr), value, intent(in) :: linkButton
    integer :: boolGood
    character(len=1024) :: strURI

    call convert_c_string(gtk_link_button_get_uri(linkButton), strURI)
    !call execute_command_line("open "//trim(strURI))
    !call execute_command_line("open http://www.google.com")

    boolGood = g_app_info_launch_default_for_uri(trim(strURI)//c_null_char, c_null_ptr, c_null_ptr)

  end function

  function open_pdf(widget, linkButton) result(boolGood) bind(c)
    type(c_ptr) :: widget
    type(c_ptr), value, intent(in) :: linkButton
    integer :: boolGood
    character(len=1024) :: strURI

    call convert_c_string(gtk_link_button_get_uri(linkButton), strURI)

    boolGood= g_app_info_launch(g_app_info_get_default_for_type(".pdf"//c_null_char,FALSE), &
    & g_file_new_for_uri(strURI), c_null_ptr, c_null_ptr)

    !boolGood = g_app_info_launch_default_for_uri(trim(strURI)//c_null_char, c_null_ptr, c_null_ptr)

  end function

  function key_event_h(controller, keyval, keycode, state, gdata) result(ret) bind(c)

    use gdk, only: gdk_device_get_name, gdk_keyval_from_name, gdk_keyval_name
  use gtk, only: gtk_window_set_child, gtk_window_destroy, &
       & gtk_widget_queue_draw, gtk_widget_show, gtk_init, TRUE, FALSE, &
       & GDK_CONTROL_MASK, &
       & CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL, &
       & gtk_event_controller_get_current_event_device, &
       & gtk_gesture_single_get_current_button
    type(c_ptr), value, intent(in) :: controller, gdata
    type(c_ptr) :: buff
    integer(c_int), value, intent(in) :: keyval, keycode, state
    logical(c_bool) :: ret
    character(len=20) :: keyname
    integer(kind=c_int) :: key_up, key_down
    type(gtktextiter), target :: iter
    integer(kind=c_int) :: result

    call convert_c_string(gdk_keyval_name(keyval), keyname)
    !print *, "Keyval: ",keyval," Name: ", trim(keyname), "      Keycode: ", &
    !         & keycode, " Modifier: ", state

    key_up = gdk_keyval_from_name("Up"//c_null_char)
    key_down = gdk_keyval_from_name("Down"//c_null_char)

    if (keyval == key_up) then
      !PRINT *, "Capturing Up!"
      if (command_search.GT.1) THEN
        command_search = command_search - 1
        call updateTerminalFromCommandHistory(entry, command_search)
      end if
    end if

    if (keyval == key_down) then
      !PRINT *, "Capturing Down!"
      if (command_search.LT.command_index) THEN
          command_search = command_search + 1
          call updateTerminalFromCommandHistory(entry, command_search)
      end if
    end if

    ret = .true.
  end function key_event_h

  subroutine updateTerminalFromCommandHistory(entry, idx)

    implicit none
    type(c_ptr) :: entry, buffer
    integer :: idx

        buffer = gtk_entry_get_buffer(entry)
        if(idx.GT.cmdHistorySize.OR.idx.LT.1) THEN
          call gtk_entry_buffer_set_text(buffer, ' '//c_null_char, -1)
        else
          
          call gtk_entry_buffer_set_text(buffer, command_history(idx)//c_null_char, -1)
          call gtk_editable_set_position(entry, len(trim(command_history(idx))))
        end if

  end subroutine

  subroutine refreshStatusBar()
    use mod_lens_data_manager
    use zoa_status_bar
    use type_utils, only: real2str

    ! Box 1 - effective focal length
    call updateStatusBar(1, "EFL = "//real2str(ldm%getEFL(),4))

    ! Box 2 - dimensions
    call updateStatusBar(2, "DIM = "//sysConfig%getDimensions())

    ! Box 3 - Track Length
    call updateStatusBar(3, "Total Track: "//trim(real2str(ldm%getTrackLength(),4)))


  end subroutine


end module handlers
