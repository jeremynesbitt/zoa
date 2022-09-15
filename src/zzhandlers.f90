module handlers
  use global_widgets
  use GLOBALS
  !use lens_analysis
  use gtk
  ! , only: gtk_application_window_new, gtk_window_destroy, &
  ! & g_signal_connect, g_signal_connect_swapped, &
  ! & gtk_window_set_child, gtk_expander_set_child, gtk_box_append, &
  ! & gtk_scrolled_window_set_child, gtk_drawing_area_new, &
  ! & gtk_drawing_area_set_content_width, gtk_drawing_area_set_content_height, &
  ! & gtk_drawing_area_set_draw_func, &
  ! & gtk_widget_queue_draw, gtk_widget_show, &
  ! & gtk_window_set_default_size, gtk_window_set_title, &
  ! & TRUE, FALSE, GDK_COLORSPACE_RGB, &
  ! & gtk_grid_new, gtk_grid_attach, gtk_button_new_with_label,&
  ! & gtk_box_new, gtk_spin_button_new,&
  ! & gtk_adjustment_new, gtk_spin_button_get_value, gtk_label_new, &
  ! & gtk_expander_new_with_mnemonic, gtk_expander_set_expanded, &
  ! & gtk_toggle_button_new_with_label, gtk_toggle_button_get_active, gtk_notebook_new,&
  ! & gtk_notebook_append_page, gtk_text_view_new, gtk_text_view_get_buffer, &
  ! & gtk_text_buffer_set_text, gtk_scrolled_window_new, &
  ! & gtk_text_buffer_get_end_iter, &
  ! & gtk_text_buffer_insert_at_cursor, gtk_statusbar_new, &
  ! & gtk_statusbar_push, gtk_statusbar_pop, gtk_statusbar_get_context_id, &
  ! & gtk_button_new_with_mnemonic, gtk_link_button_new_with_label, &
  ! & gtk_toggle_button_new_with_mnemonic, gtk_label_new_with_mnemonic, &
  ! & gtk_window_set_mnemonics_visible, gtk_combo_box_text_new, &
  ! & gtk_combo_box_text_append_text, gtk_combo_box_text_get_active_text, &
  ! & gtk_combo_box_text_insert_text, gtk_spin_button_set_value, gtk_spin_button_update,&
  ! & GTK_ORIENTATION_VERTICAL, gtk_grid_set_column_homogeneous, &
  ! & gtk_grid_set_row_homogeneous, gtk_statusbar_remove_all, &
  ! & gtk_widget_set_vexpand, gtk_entry_get_text_length, &
  ! & gtk_entry_get_buffer, gtk_entry_buffer_get_text, &
  ! & gtk_text_tag_new, gtk_entry_buffer_set_text, &
  ! & gtk_text_tag_table_new, gtk_text_buffer_insert_markup, &
  ! & gtk_text_buffer_apply_tag, gtk_text_view_scroll_to_mark, &
  ! & gtk_widget_get_display, gtk_css_provider_new, &
  ! & gtk_toggle_button_get_active, gtk_statusbar_get_context_id, &
  ! & gtk_style_context_add_provider_for_display, &
  ! & gtk_css_provider_load_from_data, &
  ! & gtk_application_window_set_show_menubar, gtk_window_maximize, &
  ! & gtk_window_unmaximize, &
  ! & gtk_application_set_menubar, gtk_widget_set_name, gtk_window_present, &
  ! & gtk_combo_box_text_new_with_entry, gtk_combo_box_set_active, &
  ! & gtk_entry_set_activates_default, gtk_widget_set_focus_on_click, &
  ! & gtk_widget_set_can_focus, gtk_notebook_set_group_name, &
  ! & gtk_notebook_get_tab_label, gtk_notebook_detach_tab, &
  ! & gtk_window_get_modal, gtk_window_set_interactive_debugging, &
  ! & gtk_notebook_set_current_page


  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending, &
  & g_object_set_property, g_value_set_interned_string, g_variant_new_boolean, &
  & g_menu_new, g_menu_item_new, g_action_map_add_action, g_menu_append_item, &
  & g_object_unref, g_menu_append_section, g_menu_append_submenu, &
  & g_simple_action_new_stateful, g_variant_type_new, g_simple_action_new, &
  & g_variant_get_boolean, g_variant_get_string, g_variant_new_string, &
  & g_action_change_state, g_application_quit, g_simple_action_set_state, &
  & g_signal_override_class_handler

  use, intrinsic :: iso_c_binding, only: c_int, c_ptr, c_null_ptr, &
  & c_null_char, C_NEW_LINE

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
  use zoa_tab
  use zoa_tab_manager

  implicit none
  type(c_ptr)    :: my_window, entry, abut, ibut, provider
  ! run_status is TRUE until the user closes the top window:
  integer(c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  character(len=256), dimension(100) ::  command_history
  integer :: command_index = 1
  integer :: command_search = 1
  type(zoatabManager) :: zoatabMgr


contains

  subroutine proto_symfunc

    use global_widgets
    !use handlers
    use zoa_plot
  !  use mod_plotopticalsystem
  !use mod_plotopticalsystem




  integer(kind=c_int) :: ipick, i, endSurface, ii
 character(len=100) :: ftext, strTitle
 CHARACTER(LEN=*), PARAMETER  :: FMT1 = "(I5, F10.3, F10.3)"
 CHARACTER(LEN=*), PARAMETER  :: FMTHDR = "(A12, A5, A5)"
 character(len=100) :: logText
 real, ALLOCATABLE :: w(:), symcalc(:)
 real :: w_sum, s_sum, aplanatic, imageNA
 integer :: totalSurfaces
 integer, ALLOCATABLE ::  surfaceno(:)
  type(barchart) :: plotter, bar1, bar2
  type(multiplot) :: mplt

  !PRINT *, "Before Mod Call, ", my_window

  !ipick = hl_gtk_file_chooser_show(new_files, &
  !       & create=FALSE, multiple=TRUE, filter=["image/*"], &
  !       & parent=my_window, all=TRUE)

  !ftext = 'LIB GET 1 '
  !ftext = 'CV2PRG DoubleGauss.seq'
  ftext = 'CV2PRG LithoBraat.seq'

  CALL PROCESKDP(ftext)

  ftext = 'RTG ALL'
  CALL PROCESKDP(ftext)


  ftext = 'COLORSET RAYS 6'
  CALL PROCESKDP(ftext)
  !call getOpticalSystemLastSurface(endSurface)
  !call ld_settings%set_end_surface(endSurface)
  ftext = 'VIECO'
  CALL PROCESKDP(ftext)

  ftext = 'PXTY ALL'
  CALL PROCESKDP(ftext)

  ftext = 'OCDY'
  CALL PROCESKDP(ftext)


PRINT *, "Magnification is ", curr_par_ray_trace%t_mag



  ! Compute lens weight and symmetry
  allocate(w(curr_lens_data % num_surfaces-2))
  allocate(symcalc(curr_lens_data % num_surfaces-2))
  allocate(surfaceno(curr_lens_data % num_surfaces-2))

  PRINT *, "SIZE OF w is ", size(w)
  PRINT *, "SIZE of no_surfaces is ", size(surfaceno)

  WRITE (logText, FMTHDR), "Surface", "w_j", "s_j"
  call updateTerminalLog(logText, "black")


  do ii = 2, curr_lens_data % num_surfaces - 1
     w(ii-1) = -1/(1-curr_par_ray_trace%t_mag)
     w(ii-1) = w(ii-1) * (curr_lens_data % surf_index(ii) - curr_lens_data % surf_index(ii-1)) &
     & * curr_par_ray_trace % marginal_ray_height(ii) * curr_lens_data % curvatures(ii) &
     & / curr_lens_data % surf_index(curr_lens_data % num_surfaces) &
     & / curr_par_ray_trace % marginal_ray_angle(curr_lens_data % num_surfaces)
     w_sum = w_sum + w(ii-1)*w(ii-1)

     !PRINT *, "Check Ref Stop ", curr_lens_data % ref_stop
    !PRINT *, "SURF INDEX ", curr_lens_data % surf_index(ii)
    !PRINT *, "AOI is ", curr_par_ray_trace % chief_ray_aoi(ii)
    !allocate(symcalc(curr_lens_data % num_surfaces-2))

      totalSurfaces = curr_lens_data % num_surfaces
      aplanatic = curr_par_ray_trace % marginal_ray_angle(ii) / curr_lens_data % surf_index(ii)
      aplanatic = aplanatic - curr_par_ray_trace % marginal_ray_angle(ii-1) / curr_lens_data % surf_index(ii-1)
      imageNA = curr_lens_data%surf_index(totalSurfaces) * curr_par_ray_trace%marginal_ray_angle(totalSurfaces)

      !PRINT *, "IMAGE NA is ", imageNA
      !PRINT *, "APLANATIC IS ", aplanatic
      !PRINT *, "Marginal Ray Angle is ", curr_par_ray_trace % marginal_ray_angle(ii)
      !PRINT *, "Surface Index is ", curr_lens_data % surf_index(ii)
      symcalc(ii-1) = 1/(1-curr_par_ray_trace%t_mag)
      ! symcalc(ii-1) = symcalc(ii-1) * aplanatic * curr_lens_data % surf_index(ii) * curr_par_ray_trace % chief_ray_aoi(ii) &
      ! & / ((curr_lens_data % surf_index(curr_lens_data%ref_stop)* curr_par_ray_trace % chief_ray_aoi(curr_lens_data%ref_stop)) &
      ! & * imageNA)

      symcalc(ii-1) = symcalc(ii-1) * curr_lens_data % surf_index(ii-1) * curr_par_ray_trace % chief_ray_aoi(ii)

      !PRINT *, "FIrst Term is ", symcalc(ii-1)

      symcalc(ii-1) = symcalc(ii-1) /curr_lens_data%surf_index(curr_lens_data%ref_stop)
      symcalc(ii-1) = symcalc(ii-1) /curr_par_ray_trace%chief_ray_aoi(curr_lens_data%ref_stop)
      symcalc(ii-1) = symcalc(ii-1)*aplanatic/imageNA


      s_sum = s_sum + symcalc(ii-1)*symcalc(ii-1)

      surfaceno(ii-1) = ii-1

      WRITE(logText, FMT1), surfaceno(ii-1), w(ii-1), symcalc(ii-1)
      call updateTerminalLog(logText, "black")


  end do

  w_sum = SQRT(w_sum/(curr_lens_data % num_surfaces-2))
  s_sum = SQRT(s_sum/(curr_lens_data % num_surfaces-2))

  !PRINT *, " w is ", w
  WRITE(logText, *), " w_sum is ", w_sum
  call updateTerminalLog(logText, "black")

  !PRINT *, " s is ", symcalc
  WRITE(logText, *), " s_sum is ", s_sum
  call updateTerminalLog(logText, "black")

    !call barchart2(x,y)
    ! print *, "Calling Plotter"
     !plotter = barchart(drawing_area_plot,surfaceno,abs(w))
     !call plotter % initialize(drawing_area_plot,surfaceno,abs(w))

     ! call plotter % setxlabel
     ! call plotter % setMajorGridLines(TRUE)

     !call plotter % drawPlot()
    ! print *, "Done calling plotter"

  !PRINT *, "Paraxial Data tst ", curr_par_ray_trace%marginal_ray_height

! Multiplot
  WRITE(strTitle, "(A15, F10.3)"), "Power:  w = ", w_sum
  call mplt%initialize(drawing_area_plot, 2,1)
  call bar1%initialize(c_null_ptr, real(surfaceno),abs(w), &
  & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
  & title=trim(strTitle)//c_null_char)
  !PRINT *, "Bar chart color code is ", bar1%dataColorCode
  WRITE(strTitle, "(A15, F10.3)"), "Symmetry:  s = ", s_sum
  call bar2%initialize(c_null_ptr, real(surfaceno),abs(symcalc), &
  & xlabel='Surface No'//c_null_char, ylabel='s'//c_null_char, &
  & title=trim(strTitle)//c_null_char)
  call bar2%setDataColorCode(PL_PLOT_BLUE)
  call mplt%set(1,1,bar1)
  call mplt%set(2,1,bar2)
  call mplt%draw()

  !CALL WDRAWOPTICALSYSTEM
  !CALL RUN_WDRAWOPTICALSYSTEM

end subroutine proto_symfunc

  subroutine tst_plottingincairo
  !use mod_plotopticalsystem



  integer(kind=c_int) :: ipick, i, endSurface
 character(len=100) :: ftext
  PRINT *, "Before Mod Call, ", my_window


  !ftext = 'LIB GET 1 '
  ftext = 'CV2PRG DoubleGauss.seq'
  CALL PROCESKDP(ftext)
  ftext = 'COLORSET RAYS 2'
  CALL PROCESKDP(ftext)
  !call getOpticalSystemLastSurface(endSurface)
  !call ld_settings%set_end_surface(endSurface)
  ftext = 'VIECO'
  CALL PROCESKDP(ftext)
  !CALL WDRAWOPTICALSYSTEM
  !CALL RUN_WDRAWOPTICALSYSTEM

  end subroutine tst_plottingincairo



  subroutine plot_04(area)
    type(c_ptr), intent(inout) :: area

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

  subroutine plot_04debug(area)
    type(c_ptr), intent(inout) :: area

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

    !call hl_gtk_drawing_area_cairo_destroy(cc)
    !call hl_gtk_drawing_area_cairo_destroy(cc(2))

  end subroutine plot_04debug


  subroutine plot1(type)
    use plplot, PI => PL_PI
    implicit none
    integer, parameter :: MAX_NLEGEND = 2

    real(kind=plflt) :: freql(0:100),ampl(0:100),phase(0:100), freq, f0
    integer          :: i, type
    integer          :: nlegend
    real(kind=plflt) :: Xtst(0:50), Ytst(0:50)

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
    !call PROCESKDP('VIECO')
    !call PROCESKDP('AST')
    !call getAstigCalcResult(Xtst(0:10), Ytst(0:10))
    !call plline(Xtst,Ytst)

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
      !PRINT *, "BOOLRESULT IS ", boolresult
    end do

  end subroutine pending_events


  subroutine updateTerminalLog2(ftext, txtColor)
      USE GLOBALS
      use global_widgets

      IMPLICIT NONE

      ! This routine is to update the terminal log, and is
      ! abstracted in case the method (font color, bold) needs to be changed

      ! The way implemented is to use the pango / markup interface
      ! See for some examples
      ! https://basic-converter.proboards.com/thread/314/pango-markup-text-examples


      character(len=*), intent(in) :: ftext
      character(len=*), intent(in)  :: txtColor

      !type(c_ptr) :: buff2, tag, tagTable
      !type(c_ptr) :: page, enter, iterPtr, gColor
      type(gtktextiter), target :: iter, startIter, endIter
      logical :: scrollResult
      type(c_ptr) ::  buffInsert

      !PRINT *, "TERMINAL LOG COLOR ARGUMENT IS ", txtColor

      call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))

      !PRINT *, "ABOUT TO CALL MARKUP "
      !TODO Sometimes an empty ftext is sent to this function and GTK throws a
      !warnting.  Need to figure out how to detect empty string (this is not working)
    if (ftext.ne."  ") THEN
      call gtk_text_buffer_insert_markup(buffer, c_loc(endIter), &
      & "<span foreground='"//trim(txtColor)//"'>"//ftext//"</span>"//C_NEW_LINE &
      & //c_null_char, -1_c_int)
    END IF

      !PRINT *, "MARKUP TEXT IS ", "<span foreground='"//trim(txtColor)//"'>"//trim(ftext)//"</span>"//C_NEW_LINE &
      !& //c_null_char
      !PRINT *, "LEN of ftext is ", len(trim(ftext))

      !call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))

      !scrollResult = gtk_text_view_scroll_to_iter(textView, c_loc(endIter), 0.5_c_double, &
      !&  True, 0.0_c_double, 1.0_c_double)
      ! Make sure we are at the bottom of the scroll window


      ! Way that wasn't working, originally in name_enter
      ! Make sure we are at the bottom of the scroll window
      buffInsert = gtk_text_buffer_get_insert(buffer)
     !gBool = g_variant_new_boolean(True)
     PRINT *, "textView ptr is ", textView
      call gtk_text_view_scroll_to_mark(textView, buffInsert, 0.0_c_double, &
      &  True, 0.0_c_double, 1.0_c_double)

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

      ! Update command history for a simple way for the user to get previous commands
      if (txtColor.eq."blue") then
        if (command_index.LT.99) THEN

         command_history(command_index) = ftext
         command_index = command_index + 1
         command_search = command_index
       END IF
     END IF

      call pending_events()

  end


  subroutine updateTerminalLog(ftext, txtColor)
      USE GLOBALS
      use global_widgets

      IMPLICIT NONE

      character(len=*), intent(in) :: ftext
      character(len=*), intent(in)  :: txtColor

      type(gtktextiter), target :: iter, startIter, endIter
      logical :: scrollResult
      type(c_ptr) ::  buffInsert



      ! This routine is to update the terminal log, and is
      ! abstracted in case the method (font color, bold) needs to be changed

      ! The way implemented is to use the pango / markup interface
      ! See for some examples
      ! https://basic-converter.proboards.com/thread/314/pango-markup-text-examples




      !PRINT *, "TERMINAL LOG COLOR ARGUMENT IS ", txtColor

      call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))

      !PRINT *, "ABOUT TO CALL MARKUP "
      !TODO Sometimes an empty ftext is sent to this function and GTK throws a
      !warnting.  Need to figure out how to detect empty string (this is not working)
    if (ftext.ne."  ") THEN
      call gtk_text_buffer_insert_markup(buffer, c_loc(endIter), &
      & "<span foreground='"//trim(txtColor)//"'>"//ftext//"</span>"//C_NEW_LINE &
      & //c_null_char, -1_c_int)
    END IF

      !PRINT *, "MARKUP TEXT IS ", "<span foreground='"//trim(txtColor)//"'>"//trim(ftext)//"</span>"//C_NEW_LINE &
      !& //c_null_char
      !PRINT *, "LEN of ftext is ", len(trim(ftext))

      !call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))

      !scrollResult = gtk_text_view_scroll_to_iter(textView, c_loc(endIter), 0.5_c_double, &
      !&  True, 0.0_c_double, 1.0_c_double)
      ! Make sure we are at the bottom of the scroll window


      ! Way that wasn't working, originally in name_enter
      ! Make sure we are at the bottom of the scroll window
      buffInsert = gtk_text_buffer_get_insert(buffer)
     !gBool = g_variant_new_boolean(True)
      call gtk_text_view_scroll_to_mark(textView, buffInsert, 0.0_c_double, &
      &  True, 0.0_c_double, 1.0_c_double)

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


      !call zoatabMgr%updateMsgBuffer(ftext, txtColor)

      ! Update command history for a simple way for the user to get previous commands
      if (txtColor.eq."blue") then
        if (command_index.LT.99) THEN

         command_history(command_index) = ftext
         command_index = command_index + 1
         command_search = command_index
       END IF
     END IF

      call pending_events()

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
    print *, "Entered name as:",trim(ftext)

    ! Log results
    txtColor = "blue"
    call updateTerminalLog(ftext, txtColor)

    ! Clear Input
    call gtk_entry_buffer_set_text(buff2, c_null_char,-1_c_int)

    !call pending_events()

    ! Finally actually process the command
    CALL PROCESKDP(ftext)



  end subroutine name_enter


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
    use zoa_tab

    implicit none
    type(c_ptr), value, intent(in)  :: gdata, app2
    ! Pointers toward our GTK widgets:
    type(c_ptr)    :: scroll_win_detach, win_msg, pane
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




    ! Create the window:
    my_window = gtk_application_window_new(app)
    !call g_signal_connect(my_window, "destroy"//c_null_char, &
    !                    & c_funloc(destroy_signal))
   ! Don't forget that C strings must end with a null char:
    call gtk_window_set_title(my_window, "ZOA Optical Analysis"//c_null_char)
    ! Properties of the main window :
    width  = 1000
    height = 700
    call gtk_window_set_default_size(my_window, width, height)

    !******************************************************************
    ! Adding widgets in the window:
    !******************************************************************
    ! The four buttons:

    button2 = gtk_button_new_with_mnemonic ("Test _PLPLOT"//c_null_char)
    !call g_signal_connect (button2, "clicked"//c_null_char, c_funloc(plotLensData))
    button3 = gtk_button_new_with_mnemonic ("_Exit"//c_null_char)
    call g_signal_connect (button3, "clicked"//c_null_char, c_funloc(destroy_signal))
    toggle1 = gtk_button_new_with_mnemonic ("_TstFunc"//c_null_char)
    !call g_signal_connect (toggle1, "clicked"//c_null_char, c_funloc(plotLensData))
    !call g_signal_connect (toggle1, "clicked"//c_null_char, c_funloc(tst_plottingincairo))
    call g_signal_connect (toggle1, "clicked"//c_null_char, c_funloc(proto_symfunc))



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
    !call gtk_grid_attach(table, linkButton, 3_c_int, 0_c_int, 1_c_int, 1_c_int)

    call gtk_grid_attach(table, toggle1, 2_c_int, 0_c_int, 1_c_int,1_c_int)

    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, TRUE)



    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 5_c_int)
    call gtk_box_append(box1, expander)


    ! This object is the container for all the plot and message tabs attach
    ! here
    notebook = gtk_notebook_new ()
    call gtk_widget_set_vexpand (notebook, TRUE)


   !call zoaTabMgr%initialize(notebook)


    ! Theorecticzlly needed for detaching.
    call gtk_notebook_set_group_name(notebook,"0"//c_null_char)

    !Pseudocode
    !  msgtab = zoatabmsg%initialize(title)
    ! tabList(1) = msgTab


    !buffer = zoaTabMgr%buffer

     textView = gtk_text_view_new ()
     call gtk_text_view_set_editable(textView, FALSE)
    !
     buffer = gtk_text_view_get_buffer (textView)
    ! call gtk_text_buffer_set_text (buffer, &
    !     & "ZOA Log Message Window"//C_NEW_LINE//c_null_char,&
    !     & -1_c_int)
     scroll_win_detach = gtk_scrolled_window_new()
     notebookLabel2 = gtk_label_new_with_mnemonic("_Messages"//c_null_char)
    !
     call gtk_scrolled_window_set_child(scroll_win_detach, textView)
    ! secondTab = gtk_notebook_append_page (notebook, scroll_win_detach, notebookLabel2)


    call zoatabMgr%addMsgTab(notebook, "Messages")
    !zoatabMgr%buffer = buffer

    notebookLabel3 = gtk_label_new_with_mnemonic("_XYPlot"//c_null_char)
    !  Need unique item for third window
    scroll_ptr = gtk_scrolled_window_new()


    drawing_area_plot = hl_gtk_drawing_area_new(size=[1200,500], &
         & has_alpha=FALSE, key_press_event=c_funloc(key_event_h))
    call gtk_scrolled_window_set_child(scroll_ptr, drawing_area_plot)
    thirdTab  = gtk_notebook_append_page (notebook, scroll_ptr, notebookLabel3)

    ! Having an issue where every time I try to detach a tab the main window
    ! freezes, so disabling this for now.  Hopefully this can get resolved
    !call gtk_notebook_set_tab_detachable(notebook, scroll_ptr, TRUE)

    !call g_signal_connect(notebook, 'create-window'//c_null_char, c_funloc(detachTab), notebook)
    call g_signal_connect(notebook, 'create-window'//c_null_char, c_funloc(detachTabTst), c_null_ptr)

    call g_signal_connect(notebook, 'switch-page'//c_null_char, c_funloc(setActivePlot), c_null_ptr)


    !print *, "Notebook ptr is ", notebook
    !print *, "Detachable Tab is ", scroll_ptr
    win_msg = gtk_window_new()
    call gtk_window_set_default_size(win_msg, 200_c_int, 500_c_int)

    pane = gtk_paned_new(GTK_ORIENTATION_HORIZONTAL)
    call gtk_paned_set_start_child(pane, scroll_win_detach)
    call gtk_paned_set_end_child(pane, notebook)


    call gtk_scrolled_window_set_min_content_width(scroll_win_detach, 200_c_int)

    call gtk_box_append(box1,pane)
    call gtk_scrolled_window_set_min_content_width(notebook, 700_c_int)

    call gtk_widget_set_hexpand(notebook, TRUE)

    call gtk_widget_set_vexpand (box1, TRUE)
    !call plot_04(drawing_area_plot)

    !call x12f(drawing_area_plot)

    ! The window status bar can be used to print messages:
    !statusBar = gtk_statusbar_new ()
    !message_id = gtk_statusbar_push (statusBar, gtk_statusbar_get_context_id(statusBar, &
    !            & "ZOA"//c_null_char), "Waiting..."//c_null_char)
    !call gtk_box_append(box1, statusBar)

    ! CMD Entry TextBox

    entry = hl_gtk_entry_new(60_c_int, editable=TRUE, tooltip = &
         & "Enter text here for command interpreter"//c_null_char, &
         & activate=c_funloc(name_enter))
    !entry2 = gtk_combo_box_text_new_with_entry()
    !call gtk_combo_box_set_active(entry2, 1_c_int)
    !call gtk_entry_set_activates_default(entry2, TRUE)
    !call gtk_combo_box_text_insert_text(entry2, 0_c_int, ".."//c_null_char)
  !  call g_signal_connect(entry2, "activate"//c_null_char, c_funloc(callback_editbox), c_null_ptr)
    !keyevent = gtk_event_controller_key_new()
    !call g_signal_connect(keyevent, "key-pressed"//c_null_char, c_funloc(callback_editbox), entry2)

    !call g_signal_connect(entry, "key-pressed"//c_null_char, c_funloc(callback_editbox), c_null_ptr)
    !tmp code to test key pressed event
    controller_k = gtk_event_controller_key_new()


    !call gtk_widget_add_controller(my_drawing_area, controller_k)
    !call gtk_widget_set_focusable(my_drawing_area, TRUE)

    !tstResult = gtk_event_controller_key_forward(controller_k, entry2)

    call gtk_widget_add_controller(entry, controller_k)
    call g_signal_connect(controller_k, "key-pressed"//c_null_char, &
               & c_funloc(key_event_h), c_null_ptr)

    !call gtk_widget_set_focusable(entry2, TRUE)
    !call gtk_widget_set_can_focus(entry2, FALSE)
    !call gtk_widget_set_focus_on_click(entry2, FALSE)
    !call gtk_widget_grab_focus(entry2)

    call gtk_box_append(box1, entry)
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



  end subroutine activate

 subroutine setActivePlot(parent_notebook, selected_page, page_index, gdata) bind(c)
        type(c_ptr), value :: selected_page, parent_notebook, page_index, gdata
        type(c_ptr) :: newwin, newnotebook, newlabel, box2, scrolled_win
        type(c_ptr) :: stringPtr
        integer :: newtab
        character(len=20) :: winTitle

        PRINT *, "TAB SWITCH EVENT DETECTED! "
        PRINT *, "page_index is ", page_index
        stringPtr = gtk_notebook_get_tab_label_text(parent_notebook, selected_page)

        PRINT *, "stringPtr is ", stringPtr
        call convert_c_string(stringPtr, winTitle)
        PRINT *, "Window Title is ", trim(winTitle)

        active_plot = ID_NEWPLOT_RAYFAN



 end subroutine

  subroutine detachTabTst(parent_notebook, widget) bind(c)
        type(c_ptr), value :: widget, parent_notebook
        type(c_ptr) :: newwin, newnotebook, newlabel, box2, scrolled_win
        type(c_ptr) :: child
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

    call convert_c_string(gdk_keyval_name(keyval), keyname)
    print *, "Keyval: ",keyval," Name: ", trim(keyname), "      Keycode: ", &
             & keycode, " Modifier: ", state

    key_up = gdk_keyval_from_name("Up"//c_null_char)
    key_down = gdk_keyval_from_name("Down"//c_null_char)

    if (keyval == key_up) then
      PRINT *, "Capturing Up!"
      if (command_search.GT.1) THEN
        command_search = command_search - 1
        PRINT *, "CMD IS ", command_history(command_search)
        buff = gtk_entry_get_buffer(entry)
        call gtk_entry_buffer_set_text(buff, command_history(command_search), -1)
      end if

      !call gtk_window_destroy(my_window)
    end if

    if (keyval == key_down) then
      PRINT *, "Capturing Down!"
      if (command_search.LT.command_index) THEN
        command_search = command_search + 1
        PRINT *, "CMD IS ", command_history(command_search)
        buff = gtk_entry_get_buffer(entry)
        call gtk_entry_buffer_set_text(buff, command_history(command_search), -1)
      end if

      !call gtk_window_destroy(my_window)
    end if

    ret = .true.
  end function key_event_h


end module handlers
