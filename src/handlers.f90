module handlers
  use global_widgets
  use GLOBALS
  use lens_analysis
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
  & gtk_style_context_add_provider_for_display, &
  & gtk_css_provider_load_from_data, &
  & gtk_application_window_set_show_menubar, gtk_window_maximize, &
  & gtk_window_unmaximize, &
  & gtk_application_set_menubar, gtk_widget_set_name, gtk_window_present


  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending, &
  & g_object_set_property, g_value_set_interned_string, g_variant_new_boolean, &
  & g_menu_new, g_menu_item_new, g_action_map_add_action, g_menu_append_item, &
  & g_object_unref, g_menu_append_section, g_menu_append_submenu, &
  & g_simple_action_new_stateful, g_variant_type_new, g_simple_action_new, &
  & g_variant_get_boolean, g_variant_get_string, g_variant_new_string, &
  & g_action_change_state, g_application_quit, g_simple_action_set_state

  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr, &
  & c_null_char, C_NEW_LINE
  use, intrinsic :: iso_fortran_env, only: int64
  ! Pl Plot based inputs
  use, intrinsic :: iso_c_binding
  use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width

  use plplot, PI => PL_PI
  use plplot_extra
  use gtk_draw_hl
  use plplot_extra
  use gtk_hl_container
  use gtk_hl_button

  use gtk_hl_chooser

  implicit none
  type(c_ptr)    :: my_window, entry, abut, ibut, provider
  ! run_status is TRUE until the user closes the top window:
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult

contains
  subroutine tst_plottingincairo
  !use mod_plotopticalsystem


  character(len=256), dimension(:), allocatable :: new_files, tmp

  integer(kind=c_int) :: ipick, i
 character(len=100) :: ftext
  PRINT *, "Before Mod Call, ", my_window

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
  !CALL WDRAWOPTICALSYSTEM
  !CALL RUN_WDRAWOPTICALSYSTEM

  end subroutine tst_plottingincairo



  subroutine plot_04(area)
    type(c_ptr), intent(in) :: area

    type(c_ptr)  :: cc, cs
    integer :: i
    character(len=25) :: geometry

    ! needed for use as functions instead of subroutines
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
         & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170/), &
         & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170/)

    !  Process command-line arguments

    ! 4/10/22 have to comment out this to get it to compile.  Not sure why
    ! Go back and try to manually compile examples to debug it
    print *, "PL PARSE FULL = ", PL_PARSE_FULL
    !plparseopts_rc = plparseopts(PL_PARSE_FULL)

    !if (plparseopts_rc .ne. 0) stop "plparseopts error"


    ! Get a cairo context from the drawing area.
    !do i=1,2
    i = 1
       cc = hl_gtk_drawing_area_cairo_new(area)
       cs = cairo_get_target(cc)
    !end do

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc .ne. 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") cairo_image_surface_get_width(cs), &
         & cairo_image_surface_get_height(cs)
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc .ne. 0) stop "plsetopt error"

    ! Initialize
    call plinit

    ! Tell the "extcairo" driver where the context is located. This must be
    ! done AFTER the plstar or plinit call.
    print *, "ABOUT TO pl_cmd"
    call pl_cmd(PLESC_DEVINIT, cc)

    call plfont(2)   ! Roman font
    call plot1(0)

    ! Now continue the plot on the other surface
    !call pl_cmd(PLESC_DEVINIT, cc(2))

    !call plot1(1)

    call plend

    call hl_gtk_drawing_area_cairo_destroy(cc)
    !call hl_gtk_drawing_area_cairo_destroy(cc(2))

  end subroutine plot_04

  subroutine plot1(type)
    use plplot, PI => PL_PI
    implicit none
    integer, parameter :: MAX_NLEGEND = 2

    real(kind=plflt) :: freql(0:100),ampl(0:100),phase(0:100), freq, f0
    integer          :: i, type
    integer          :: nlegend

    real(kind=plflt) :: legend_width, legend_height
    integer          :: opt_array(MAX_NLEGEND), text_colors(MAX_NLEGEND), &
    & line_colors(MAX_NLEGEND), line_styles(MAX_NLEGEND), &
    & symbol_colors(MAX_NLEGEND), symbol_numbers(MAX_NLEGEND)

    ! For plplot 5.9.9 or lower the next declarations should be integers
    ! For 5.9.10 or higher they should be reals
!    real(kind=plflt) :: symbol_scales(MAX_NLEGEND), box_scales(0)
!    integer :: line_widths(MAX_NLEGEND), box_line_widths(0)
    real(kind=plflt) :: symbol_scales(MAX_NLEGEND), box_scales(MAX_NLEGEND)

!    real(kind=plflt) :: line_widths(MAX_NLEGEND), box_line_widths(0)
!    integer          :: box_colors(0), box_patterns(0)

    real(kind=plflt) :: line_widths(MAX_NLEGEND), box_line_widths(MAX_NLEGEND)
    integer          :: box_colors(MAX_NLEGEND), box_patterns(MAX_NLEGEND)

    character(len=20):: text(MAX_NLEGEND)
    character(len=1) :: symbols(MAX_NLEGEND)

    call pladv(0)
    !      Set up data for log plot.
    f0 = 1._plflt
    do i=0,100
       freql(i)= -2.0_plflt + dble (i)/20.0_plflt
       freq=10.0_plflt**freql(i)
       ampl(i)=20.0_plflt*log10(1.0_plflt/sqrt(1.0_plflt+(freq/f0)**2))
       phase(i)=-(180.0_plflt/PI)*atan(freq/f0)
    enddo
    call plvpor(0.15_plflt, 0.85_plflt, 0.1_plflt, 0.9_plflt)
    call plwind(-2.0_plflt, 3.0_plflt, -80.0_plflt, 0.0_plflt)
    call plcol0(1)
    !      Try different axis and labelling styles.
    if (type.eq.0) then
       call plbox('bclnst', 0.0_plflt, 0, 'bnstv', 0.0_plflt, 0)
    elseif (type.eq.1) then
       call plbox('bcfghlnst', 0.0_plflt, 0, 'bcghnstv', 0.0_plflt, 0)
    else
       stop 'plot1: invalid type'
    endif
    !      Plot ampl vs freq.
    call plcol0(2)
    call plline(freql,ampl)
    call plcol0(2)
    call plptex(1.6_plflt, -30.0_plflt, 1.0_plflt, -20.0_plflt, 0.5_plflt, &
         '-20 dB/decade')
    !      Put labels on.
    call plcol0(1)
    call plmtex('b', 3.2_plflt, 0.5_plflt, 0.5_plflt, 'Frequency')
    call plmtex('t', 2.0_plflt, 0.5_plflt, 0.5_plflt, &
         'Single Pole Low-Pass Filter')
    call plcol0(2)
    call plmtex('l', 5.0_plflt, 0.5_plflt, 0.5_plflt, 'Amplitude (dB)')
    nlegend = 1
    !      For the gridless case, put phase vs freq on same plot.
    if (type.eq.0) then
       call plcol0(1)
       call plwind(-2.0_plflt, 3.0_plflt, -100.0_plflt, 0.0_plflt)
       call plbox(' ', 0.0_plflt, 0, 'cmstv', 30.0_plflt, 3)
       call plcol0(3)
       call plline(freql, phase)
       call plstring(freql, phase, '*')
       call plcol0(3)
       call plmtex('r', 5.0_plflt, 0.5_plflt, 0.5_plflt, &
            'Phase shift (degrees)')
       nlegend = 2
    endif

    !     Draw a legend
    !     First legend entry.
    opt_array(1)   = PL_LEGEND_LINE
    text_colors(1) = 2
    text(1)        = 'Amplitude'
    line_colors(1) = 2
    line_styles(1) = 1
    ! For plplot 5.9.9 or lower comment out the real assignment,
    ! for 5.9.10 or higher, comment out the integer assignment.
!    line_widths(1) = 1
    line_widths(1) = 1.0_plflt
    !     note from the above opt_array the first symbol (and box) indices
    !     do not have to be specified

    !     Second legend entry.
    opt_array(2)      = PL_LEGEND_LINE + PL_LEGEND_SYMBOL
    text_colors(2)    = 3
    text(2)           = 'Phase shift'
    line_colors(2)    = 3
    line_styles(2)    = 1
    ! For plplot 5.9.9 or lower comment out the real assignment,
    ! for 5.9.10 or higher, comment out the integer assignment.
!    line_widths(2)    = 1
    line_widths(2)    = 1.0_plflt
    symbol_colors(2)  = 3
    symbol_scales(2)  = 1.0
    symbol_numbers(2) = 4
    symbols(2)        = '*'

    !     from the above opt_arrays we can completely ignore everything
    !     to do with boxes. (Hence the size 0 for the associated arrays)
    !     (note: use the argument nlegend explicitly)

    call plscol0a( 15, 32, 32, 32, 0.70_plflt )
    call pllegend( legend_width, legend_height, &
         PL_LEGEND_BOUNDING_BOX, 0, &
         0.0_plflt, 0.0_plflt, 0.1_plflt, 15, &
         1, 1, 0, 0, &
         opt_array, &
         1.0_plflt, 1.0_plflt, 2.0_plflt, &
         1.0_plflt, &           !
         text_colors, text, &
         box_colors, box_patterns, box_scales, box_line_widths, &
         line_colors, line_styles, line_widths, &
         symbol_colors, symbol_scales, symbol_numbers, symbols )
  end subroutine plot1



  ! Our callback function before destroying the window:
  recursive subroutine destroy_signal(widget, event, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, event, gdata

    print *, "Your destroy_signal() function has been invoked !"
    ! Some functions and subroutines need to know that it's finished:
    run_status = FALSE
    call gtk_window_destroy(my_window)
  end subroutine destroy_signal

  ! This function is needed to update the GUI during long computations.
  ! https://developer.gnome.org/glib/stable/glib-The-Main-Event-Loop.html
  recursive subroutine pending_events ()
    do while(IAND(g_main_context_pending(c_null_ptr), run_status) /= FALSE)
      ! FALSE for non-blocking:
      boolresult = g_main_context_iteration(c_null_ptr, FALSE)
    end do

  end subroutine pending_events

  ! "It is called whenever GTK needs to draw the contents of the drawing area
  ! to the screen."
  ! https://developer.gnome.org/gtk4/stable/GtkDrawingArea.html#gtk-drawing-area-set-draw-func
  subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)
    use cairo, only: cairo_paint
    use gdk, only: gdk_cairo_set_source_pixbuf
    use global_widgets, only: my_pixbuf

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: width, height

    ! We redraw the pixbuf:
    call gdk_cairo_set_source_pixbuf(my_cairo_context, my_pixbuf, 0d0, 0d0)
    call cairo_paint(my_cairo_context)
  end subroutine my_draw_function

  ! GtkButton signal emitted by the "Compute" button
  ! In this example, this function is declared recursive because it can be
  ! called a second time from the Julia_set() subroutine:
  recursive subroutine firstbutton (widget, gdata ) bind(c)
    use global_widgets

    integer(c_int)                 :: message_id
    type(c_ptr), value, intent(in) :: widget, gdata
    complex(kind(1d0))             :: c
    integer                        :: iterations


  end subroutine firstbutton

  ! GtkComboBox signal emitted when the user selects predifined c values of
  ! interesting Julia sets in the combo box:
  subroutine firstCombo (widget, gdata ) bind(c)
    use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
    use gtk_sup, only: c_f_string_copy
    use global_widgets

    type(c_ptr), value, intent(in) :: widget, gdata
    real(c_double)                 :: x, y
    integer                        :: choice
    character(len=512)             :: my_string

    ! Get the value selected by the user:
    call c_f_string_copy( gtk_combo_box_text_get_active_text(combo1), my_string)
    read(my_string, *) choice

    select case (choice)
    case(1)
      x = +0d0
      y = +1d0
    case(2)
      x = -1d0
      y = +0d0
    case(3)
      x = -0.8d0
      y = +0.2d0
    case(4)
      x = +0.39d0
      y = +0.60d0
    case(5)
      x = -0.2d0
      y = +0.8d0
    case(6)
      x = -0.8d0
      y = +0.4d0
    case(7)
      x = +0.39d0
      y = +0.00d0
    end select

    ! Update the spin buttons real(c) and imag(c):
    call gtk_spin_button_set_value (spinButton1, x)
    call gtk_spin_button_set_value (spinButton2, y)
  end subroutine firstCombo

  ! GtkButton signal emitted by the button "Save as PNG":
  subroutine secondbutton (widget, gdata) bind(c)
    use gtk_os_dependent, only: gdk_pixbuf_savev
    use global_widgets

    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int)                 :: cstatus, message_id


  end subroutine secondbutton

  ! GtkToggleButton signal emitted when the "Pause" button is clicked.
  ! This function is declared recursive because it will be pressed a first time
  ! to pause and a second time to end pause (from the "do while" loop):
  recursive subroutine firstToggle (widget, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_long
    use global_widgets

    type(c_ptr), value, intent(in) :: widget, gdata
    integer(c_int)                 :: message_id

    ! The button is pressed:
    if (gtk_toggle_button_get_active(widget) == TRUE) then
      ! Print messages:
      call gtk_text_buffer_insert_at_cursor (buffer, &
              & "In pause"//C_NEW_LINE//c_null_char, -1_c_int)
      message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(&
                                      &statusBar, "ZOA"//c_null_char), &
                                      &"In pause..."//c_null_char)
      ! The Pause loop must handle events in order to avoid the blocking of
      ! the GUI:
      do while (gtk_toggle_button_get_active(widget) == TRUE)
        call pending_events
        if (run_status == FALSE) exit ! Exit if we had a destroy signal
        call g_usleep(50000_c_long)   ! In microseconds
      end do
    else  ! The button is raised:
      call gtk_text_buffer_insert_at_cursor (buffer, &
                            & "Not in pause"//C_NEW_LINE//c_null_char, -1_c_int)
      ! Remove the "In pause..." message from the status bar:
      call gtk_statusbar_pop (statusBar, gtk_statusbar_get_context_id(statusBar,&
                            & "ZOA"//c_null_char))
    end if
  end subroutine firstToggle

  subroutine updateTerminalLog(ftext, txtColor)
      USE GLOBALS
      use global_widgets

      IMPLICIT NONE

      ! This routine is to update the terminal log, and is
      ! abstracted in case the method (font color, bold) needs to be changed

      ! The way implemented is to use the pango / markup interface
      ! See for some examples
      ! https://basic-converter.proboards.com/thread/314/pango-markup-text-examples


      character(len=100) :: ftext
      character(len=20)  :: txtColor

      !type(c_ptr) :: buff2, tag, tagTable
      !type(c_ptr) :: page, enter, iterPtr, gColor
      type(gtktextiter), target :: iter, startIter, endIter


      call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))


      call gtk_text_buffer_insert_markup(buffer, c_loc(endIter), &
      & "<span foreground='"//trim(txtColor)//"'>"//ftext//"</span>"//C_NEW_LINE &
      & //c_null_char, -1_c_int)

      !PRINT *, trim(txtColor)
      !PRINT *, "blue"

      !call gtk_text_buffer_insert_markup(buffer, c_loc(endIter), &
      !& "<span foreground='blue'>"//">> "//ftext//"</span>"//C_NEW_LINE &
      !& //c_null_char, -1_c_int)

      ! create a new property tag and change the color
      !tag = gtk_text_tag_new("blue_fg")
      !call g_value_set_interned_string(gColor, "blue"//c_null_char)
      !call g_object_set_property(tag, "foreground"//c_null_char, gColor)
      !call gtk_text_buffer_get_start_iter(buffer, c_loc(startIter))

      !Insert text
      !call gtk_text_buffer_insert_at_cursor(buffer, &
      ! & ">> "//ftext//C_NEW_LINE//c_null_char, -1_c_int)

      ! Get gtkIter and update text with nbew property tag
      ! call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))
      !call gtk_text_buffer_apply_tag(buffer, tag, c_loc(startIter), c_loc(endIter))

      !When I ran this, nothing changed.  Suspect there is some issue
      !with start and end Iter, but did not spend enough time debugging

  end


  subroutine name_enter(widget, data) bind(c)


         USE GLOBALS
         use global_widgets
        IMPLICIT NONE

    type(c_ptr), value :: widget, data
    type(c_ptr) :: buff2, tag, tagTable, gBool
    type(c_ptr) :: page, enter, iterPtr, gColor, buffInsert
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
    print *, "Entered name as:",trim(ftext)

    ! Log results
    txtColor = "blue"
    call updateTerminalLog(ftext, txtColor)

    ! Clear Input
    call gtk_entry_buffer_set_text(buff2, c_null_char,-1_c_int)

    call pending_events()

    ! Finally actually process the command
    CALL PROCESKDP(ftext)

    ! Make sure we are at the bottom of the scroll window
    buffInsert = gtk_text_buffer_get_insert(buffer)
    !gBool = g_variant_new_boolean(True)
    call gtk_text_view_scroll_to_mark(textView, buffInsert, 0.0_c_double, &
    &  True, 0.5_c_double, 0.5_c_double)



  end subroutine name_enter

  subroutine entry_text(widget, gdata) bind(c)



    type(c_ptr), value, intent(in) :: widget, gdata



    integer(kind=c_int16_t) :: ntext


    ntext = gtk_entry_get_text_length(widget)
    if (ntext > 0) then
       call gtk_widget_set_sensitive(abut, TRUE)
       call gtk_widget_set_sensitive(ibut, TRUE)
    else
       call gtk_widget_set_sensitive(abut, FALSE)
       call gtk_widget_set_sensitive(ibut, FALSE)
    end if
  end subroutine entry_text




  ! Callback function for the signal "activate" emitted by g_application_run().
  ! We use a subroutine because it should return void.
  ! The GUI is defined here.
  subroutine activate(app2, gdata) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_f_pointer, c_null_funptr
    use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
    !use global_widgets
    use GLOBALS
    use zoamenubar
    implicit none
    type(c_ptr), value, intent(in)  :: gdata, app2
    ! Pointers toward our GTK widgets:
    type(c_ptr)    :: table, button1, button2, button3, box1
    type(c_ptr)    :: label1, label2, label3, label4
    type(c_ptr)    :: toggle1, expander, notebook, notebookLabel1, notebookLabel2
    type(c_ptr)    :: linkButton, iterPtr, iterGUI, notebookLabel3
    integer(c_int) :: message_id, firstTab, secondTab, thirdTab
    type(c_ptr) :: act_fullscreen, act_color, act_quit, display
    type(c_ptr) :: menubar, menu, section1, section2, section3, &
      & menu_item_red, menu_item_green, &
      & menu_item_blue, menu_item_quit, menu_item_fullscreen, lb



    ! Create the window:
    my_window = gtk_application_window_new(app)
    !call g_signal_connect(my_window, "destroy"//c_null_char, &
    !                    & c_funloc(destroy_signal))
   ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(my_window, "ZOA Optical Analysis"//c_null_char)
    ! Properties of the main window :
    width  = 700
    height = 700
    call gtk_window_set_default_size(my_window, width, height)


    lb = gtk_label_new (""//c_null_char)
    call gtk_widget_set_name (lb, "lb"//c_null_char)  ! the name is used by CSS Selector.
    call gtk_window_set_child (my_window, lb)

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    ! The four buttons:
    button1 = gtk_button_new_with_mnemonic ("_Compute"//c_null_char)
    call g_signal_connect (button1, "clicked"//c_null_char, c_funloc(firstbutton))
    button2 = gtk_button_new_with_mnemonic ("_Save as PNG"//c_null_char)
    call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(secondbutton))
    button3 = gtk_button_new_with_mnemonic ("_Exit"//c_null_char)
    call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(destroy_signal))
    toggle1 = gtk_toggle_button_new_with_mnemonic ("_TstFunc"//c_null_char)
    !call g_signal_connect (toggle1, "clicked"//c_null_char, c_funloc(plotLensData))
    call g_signal_connect (toggle1, "clicked"//c_null_char, c_funloc(tst_plottingincairo))

    ! The spin buttons to set the parameters:
    label1 = gtk_label_new("real(c)"//c_null_char)
    spinButton1 = gtk_spin_button_new (gtk_adjustment_new(-0.835d0,-2d0,+2d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)
    label2 = gtk_label_new("imag(c) "//c_null_char)
    spinButton2 = gtk_spin_button_new (gtk_adjustment_new(-0.2321d0, -2d0,+2d0,0.05d0,0.5d0,0d0),0.05d0, 7_c_int)
    label3 = gtk_label_new("iterations"//c_null_char)
    spinButton3 = gtk_spin_button_new (gtk_adjustment_new(100000d0,1d0,+1000000d0,10d0,100d0,0d0),10d0, 0_c_int)

    ! The combo box with predifined values of interesting Julia sets:
    label4 = gtk_label_new("Predefined values:"//c_null_char)
    combo1 = gtk_combo_box_text_new()
    call gtk_combo_box_text_append_text(combo1, "1"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "2"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "3"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "4"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "5"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "6"//c_null_char)
    call gtk_combo_box_text_append_text(combo1, "7"//c_null_char)
    call g_signal_connect (combo1, "changed"//c_null_char, c_funloc(firstCombo))

    ! A clickable URL link:
    linkButton = gtk_link_button_new_with_label ( &
                          &"http://www.ecalculations.com"//c_null_char,&
                          &"More on KDP2"//c_null_char)

    ! A table container will contain buttons and labels:
    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)
    call gtk_grid_attach(table, button1, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, button2, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, button3, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label2, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label3, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton1, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton2, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton3, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, linkButton, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label4, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, combo1, 2_c_int, 1_c_int, 1_c_int,1_c_int)
    call gtk_grid_attach(table, toggle1, 2_c_int, 3_c_int, 1_c_int,1_c_int)

    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, TRUE)

    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
    call gtk_box_append(box1, expander)

    ! We need a widget where to draw our pixbuf.
    ! The drawing area is contained in the vertical box:
    pixwidth  = 500
    pixheight = 500
    my_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(my_drawing_area, pixwidth)
    call gtk_drawing_area_set_content_height(my_drawing_area, pixheight)
    call gtk_drawing_area_set_draw_func(my_drawing_area, &
                     & c_funloc(my_draw_function), c_null_ptr, c_null_funptr)

    ! We define a notebook with two tabs "Graphics" and "Messages":
    notebook = gtk_notebook_new ()
    call gtk_widget_set_vexpand (notebook, TRUE)
    notebookLabel1 = gtk_label_new_with_mnemonic("_Graphics"//c_null_char)
    firstTab = gtk_notebook_append_page (notebook, my_drawing_area, notebookLabel1)

    textView = gtk_text_view_new ()
    buffer = gtk_text_view_get_buffer (textView)
    call gtk_text_buffer_set_text (buffer, &
        & "You can copy this text and even edit it !"//C_NEW_LINE//c_null_char,&
        & -1_c_int)
    scrolled_window = gtk_scrolled_window_new()
    notebookLabel2 = gtk_label_new_with_mnemonic("_Messages"//c_null_char)
    notebookLabel3 = gtk_label_new_with_mnemonic("_XYPlot"//c_null_char)

    call gtk_scrolled_window_set_child(scrolled_window, textView)
    secondTab = gtk_notebook_append_page (notebook, scrolled_window, notebookLabel2)

    !  Need unique item for third window
    drawing_area_plot = hl_gtk_drawing_area_new(size=[800,500], &
         & has_alpha=FALSE)
    thirdTab  = gtk_notebook_append_page (notebook, drawing_area_plot, notebookLabel3)

    call gtk_box_append(box1, notebook)
    call gtk_widget_set_vexpand (box1, TRUE)

    call plot_04(drawing_area_plot)
    !call x12f(drawing_area_plot)

    ! The window status bar can be used to print messages:
    statusBar = gtk_statusbar_new ()
    message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
                & "ZOA"//c_null_char), "Waiting..."//c_null_char)
    call gtk_box_append(box1, statusBar)

    ! CMD Entry TextBox
    entry = hl_gtk_entry_new(60_c_int, editable=TRUE, tooltip = &
         & "Enter text here for command interpreter"//c_null_char, &
         & activate=c_funloc(name_enter))
    call gtk_box_append(box1, entry)



    ! We create a "pixbuffer" to store the pixels of the image:
    my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, pixwidth, pixheight)
    nch = gdk_pixbuf_get_n_channels(my_pixbuf)
    rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, (/pixwidth*pixheight*nch/))
    ! We use char() for "pixel" because we need unsigned integers.
    ! This pixbuffer has no Alpha channel (15% faster), only RGB.
    pixel = char(0)
    !******************************************************************

    call populatezoamenubar(app, my_window)


    ! Let's finalize the GUI:
    call gtk_window_set_child(my_window, box1)
    call gtk_window_set_mnemonics_visible (my_window, TRUE)
    call gtk_widget_show(my_window)

    call gtk_window_present (my_window)

    ! If you don't show it, nothing will appear on screen...
    !call gtk_widget_show(my_window)
    !call gtk_text_buffer_get_end_iter(buffer, iterPtr)
    !PRINT *, "ITER PTR IS ", iterPtr
    !call c_f_pointer(iterPtr, iterGUI)
    !PRINT *, "FPTR IS ", iterGUI

    ! INIT KDP
    CALL INITKDP



  end subroutine activate




subroutine convertRGBtoPixBuf(animage)

    use RCImageBasic

    use, intrinsic :: iso_c_binding
    use global_widgets
    use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new, &
                      & gdk_pixbuf_scale_simple

    implicit none
    type (rgbImage) animage, newimage
    type (rgb) rgbPixel
    type(c_ptr) :: my_tmppixbuf
    character(kind=c_char), dimension(:), pointer :: tmppixel
    integer    :: i, j, k, p, itermax
    double precision :: x, y, xmin, xmax, ymin, ymax ! coordinates in the complex plane
    complex(kind(1d0)) :: c, z
    double precision :: scx, scy       ! scales
    integer(4) :: red, green, blue     ! rgb color
    integer(kind=c_int) :: tmppixwidth, tmppixheight, tmpnch
    double precision :: t0, t1

    !alloc_img(animage, 7400, 5652)
    tmppixheight = 7400
    tmppixwidth  = 5652
    call rescalergbImage(animage, newimage, 7400, 5652, 500, 500)

    ! dummy
     ! We create a "pixbuffer" to store the pixels of the image:
    my_tmppixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, tmppixheight, tmppixwidth)
    tmpnch = gdk_pixbuf_get_n_channels(my_tmppixbuf)
    !rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
    call c_f_pointer(gdk_pixbuf_get_pixels(my_tmppixbuf), tmppixel, (/tmppixwidth*tmppixheight*tmpnch/))

    tmppixel = char(0)

    computing = .true.
    call cpu_time(t0)

    !scx = ((1) / pixwidth)   ! x scale
    !scy = ((1) / pixheight)  ! y scale

    !scx = tmppixwidth  / pixwidth
    !scy = tmppixheight / pixheight
    scx = 1
    scy = 1
    xmin = 0
    ymin = 0
    ! We compute the colour of each pixel (i,j):
    do i=0, pixwidth-1
      ! We provoke a draw event only once in a while to improve performances:
      !if (mod(i,10) == 0) then
      !  call gtk_widget_queue_draw(my_drawing_area)
      !end if

      x = xmin + scx * i
      do j=0, pixheight-1
        y = ymin + scy * j


          ! Colour palette:
         ! red   = int(min(255, k*2),  KIND=1)
         ! green = int(min(255, k*5),  KIND=1)
         ! blue  = int(min(255, k*10), KIND=1)


        ! We write in the pixbuffer:
        p = i * nch + j * rowstride + 1
        !PRINT *, i, ",", j

        !call get_pixel(animage, int(x), int(y), rgbPixel)
        call get_pixel(newimage, int(x), int(y), rgbPixel)
        !PRINT *, rgbPixel
        call get_color(rgbPixel, red, green, blue)
        !if (red.NE.255) PRINT *, red
        !if (green.NE.255) PRINT *, green
        !if (blue.NE.255) PRINT *, blue

        pixel(p)   = achar(red)
        pixel(p+1) = achar(green)
        pixel(p+2) = achar(blue)

        !pixel(p)   = achar(animage%red(i,j))
        !pixel(p+1) = achar(animage%green(i,j))
        !pixel(p+2) = achar(animage%blue(i,j))


        ! This subroutine processes GTK events that occurs during the computation:
        call pending_events()
        if (run_status == FALSE) return ! Exit subroutine if we had a delete event.
      end do
    end do

    ! scale image
    ! pixbuf = pixbuf.scale_simple(width, height, gtk.gdk.INTERP_BILINEAR)
    !my_pixbuf = gdk_pixbuf_scale_simple(my_tmppixbuf, pixwidth, pixheight, 2_c_int)

    ! Final update of the display:
    call gtk_widget_queue_draw(my_drawing_area)

    computing = .false.

    call cpu_time(t1)
    write(string, '("System time = ",F8.3, " s")') t1-t0
    call gtk_text_buffer_insert_at_cursor (buffer, &
                                      & string//C_NEW_LINE//c_null_char, -1_c_int)
    PRINT *, "DONE WITH GRAOUT"
end subroutine convertRGBtoPixBuf





end module handlers
