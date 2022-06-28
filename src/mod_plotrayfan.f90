

module mod_plotrayfan


  use GLOBALS
  use global_widgets
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  use gtk_hl_tree
  use gtk

  use gtk_hl_chooser

  use g
  use zoa_ui
  use mod_ray_fan_settings

  implicit none

  integer(kind=c_int) :: run_status = TRUE

  REAL :: kdp_width = 10500
  REAL :: kdp_height = 7050

contains

  subroutine ray_fan_plot_check_status
        use handlers

        if (.not. c_associated(rf_window))  THEN
            PRINT *, "Call New Ray Fan Window"
            call ray_fan_new(my_window)
        else
           PRINT *, "Redraw Ray Fan "
           call gtk_widget_queue_draw(rf_cairo_drawing_area)
        end if
        PRINT *, "READY TO PLOT!"
  end subroutine ray_fan_plot_check_status

  subroutine rf_my_destroy(widget, gdata) bind(c)
    ! added this dependency to allow for windows to close / reopoen
    ! However I get a bunch of GTK assertion widget failures when I reopen
    ! even though the UI works.  TODO need to find this bug
    use mod_plotopticalsystem

    type(c_ptr), value :: widget, gdata
    type(c_ptr) :: isurface
    print *, "Exit called"


    isurface = g_object_get_data(widget, "backing-surface")
    if (c_associated(isurface)) call cairo_surface_destroy(isurface)
    !call cairo_destroy(rf_cairo_drawing_area)
    !call gtk_widget_unparent(gdata)
    !call g_object_unref(rf_cairo_drawing_area)
    call gtk_window_destroy(widget)
    !rf_cairo_drawing_area = c_null_ptr
    !call g_object_unref(rf_cairo_drawing_area)
    !call g_object_unref(gdata)
    !call g_object_unref(widget)
    !call gtk_window_destroy(widget)
    !call hl_gtk_drawing_area_cairo_destroy(rf_cairo_drawing_area)
    !
    !gdata = c_null_ptr

    rf_window = c_null_ptr

  end subroutine rf_my_destroy



subroutine callback_ray_fan_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  PRINT *, "Integer Passed is ", ID_SETTING

  PRINT *, "Pointer Passed is ", gdata

  select case (ID_SETTING)

  case (ID_RAYFAN_FANTYPE)
    call rf_settings % set_ray_fan(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_RAYFAN_WFETYPE)
    call rf_settings % set_fan_wfe(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_RAYFAN_NUMRAYS)

    call rf_settings % set_num_rays(INT(gtk_spin_button_get_value (widget)))

  case (ID_RAYFAN_WAVELENGTH)
    call rf_settings % set_ray_fan_wavelength(INT(gtk_spin_button_get_value (widget)))

  case (ID_RAYFAN_MAX_PUPIL)
    call rf_settings % set_max_pupil(REAL(gtk_spin_button_get_value (widget)))

  case (ID_RAYFAN_MIN_PUPIL)
    call rf_settings % set_min_pupil(REAL(gtk_spin_button_get_value (widget)))


  end select

   if (rf_settings%changed.eq.1) THEN
      rf_settings%changed = 0
      call ray_fan_replot()

   end if

end subroutine callback_ray_fan_settings

  subroutine ray_fan_settings_dialog(box1)

    use hl_gtk_zoa

    type(c_ptr), intent(inout) :: box1

    type(c_ptr)     :: table, expander

    type(c_ptr)  :: label_fantype, label_wfetype

    type(c_ptr)  :: lstmp, gtype, cbox, cbox_wfetype, cbox_scale


    type(c_ptr)  :: label_maxPupil, label_minPupil, label_numRays, label_wavelength
    type(c_ptr)  :: spinButton_maxPupil, spinButton_minPupil
    type(c_ptr)  :: spinButton_numRays, spinButton_wavelength

    type(c_ptr)  :: spinButton_fanScale, label_fanScale


    integer(kind=c_int) :: lastSurface


    ! Added these target parameters to have only one callback function and satisfy
    ! requirement to have a target attribute for a pointer for gtk.  I could not
    ! find a more elegant solution, and this seems better than a bunch of small
    ! callback functions
    integer, target :: TARGET_RAYFAN_WAVELENGTH  = ID_RAYFAN_WAVELENGTH
    integer, target :: TARGET_RAYFAN_NUMRAYS  = ID_RAYFAN_NUMRAYS

    integer, target :: TARGET_RAYFAN_FANTYPE  = ID_RAYFAN_FANTYPE
    integer, target :: TARGET_RAYFAN_WFETYPE  = ID_RAYFAN_WFETYPE


    integer, target :: TARGET_RAYFAN_MAX_PUPIL   = ID_RAYFAN_MAX_PUPIL
    integer, target :: TARGET_RAYFAN_MIN_PUPIL   = ID_RAYFAN_MIN_PUPIL

    type(c_ptr) :: output_ptr

    integer(kind=c_int), parameter :: ncols=2
    integer(kind=type_kind), dimension(ncols), target :: coltypes = &
         & [G_TYPE_INT, G_TYPE_STRING]

    type(gtktreeiter), target :: iter
    type(gvalue), target :: valt, vali
    type(c_ptr) :: val

    type(c_ptr) :: list, combo_tmp
    !type(gvalue), target :: modelv, columnv
    !type(c_ptr) :: pmodel, pcolumn, model
    !integer(kind=c_int) :: icol
    !character(len=20), dimension(4) :: vals_plotorientation

    type(c_ptr) :: label_first, label_last, spinButton_firstSurface, spinButton_lastSurface

    type(c_ptr) :: label_azimuth, label_elevation

    type(c_ptr) :: spinButton_scaleFactor

    character(kind=c_char, len=20), dimension(4) :: vals_fantype
    integer(c_int), dimension(4) :: refs_fantype

    character(kind=c_char, len=40), dimension(4) :: vals_wfetype
    integer(c_int), dimension(4) :: refs_wfetype

    character(kind=c_char, len=40), dimension(2) :: vals_scaleFactor
    integer(c_int), dimension(2) :: refs_scaleFactor



    vals_fantype = [character(len=20) :: "Y - Fan", "X - Fan", &
         &"P - Fan", "N - Fan"]

    refs_fantype = [ID_RAYFAN_Y_FAN, &
                          & ID_RAYFAN_X_FAN, &
                          & ID_RAYFAN_P_FAN, &
                          & ID_RAYFAN_N_FAN]


    call hl_gtk_combo_box_list2_new(cbox, refs_fantype, vals_fantype, 1700_c_int)
    !call g_signal_connect (cbox, "changed"//c_null_char, c_funloc(combo_fantype_callback), c_null_ptr)
    call g_signal_connect (cbox, "changed"//c_null_char, c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_FANTYPE))


    vals_wfetype =  [character(len=40) :: "Transverse Ray Aberration", &
                   & "Optical Path Difference (OPD)", &
                   & "Chromatic Differences", &
                   & "Longitudinal Aberrations"]

    refs_wfetype = [ID_RAYFAN_TRANSVERSE_RAY, &
                  & ID_RAYFAN_TRANSVERSE_OPD, &
                  & ID_RAYFAN_CHROMATIC, &
                  & ID_RAYFAN_LONGITUDINAL]

    call hl_gtk_combo_box_list2_new(cbox_wfetype, refs_wfetype, vals_wfetype, 1700_c_int)
    !call g_signal_connect (cbox_wfetype, "changed"//c_null_char, c_funloc(combo_wfetype_callback), c_null_ptr)
    call g_signal_connect (cbox_wfetype, "changed"//c_null_char, c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_WFETYPE))


    label_maxPupil = gtk_label_new("Maximum Pupil Value"//c_null_char)
    spinButton_maxPupil = gtk_spin_button_new (gtk_adjustment_new(value=1d0, &
                                                                & lower=0d0, &
                                                                & upper=1d0, &
                                                                & step_increment=0.05d0, &
                                                                & page_increment=.1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=3_c_int)

    label_minPupil = gtk_label_new("Minimum Pupil Value"//c_null_char)
    spinButton_minPupil = gtk_spin_button_new (gtk_adjustment_new(value=-1d0, &
                                                                & lower=-1d0, &
                                                                & upper=0d0, &
                                                                & step_increment=0.05d0, &
                                                                & page_increment=.1d0, &
                                                                & page_size=0d0),climb_rate=2d0, &
                                                                & digits=3_c_int)

    label_numRays = gtk_label_new("Number of Rays in the Fan"//c_null_char)
    spinButton_numRays = gtk_spin_button_new (gtk_adjustment_new(value=11d0, &
                                                                & lower=1d0, &
                                                                & upper=20d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=1d0, &
                                                                & digits=0_c_int)

    label_wavelength = gtk_label_new("Wavelength to Trace"//c_null_char)
    spinButton_wavelength = gtk_spin_button_new (gtk_adjustment_new(value=2d0, &
                                                                & lower=1d0, &
                                                                & upper=4d0, &
                                                                & step_increment=1d0, &
                                                                & page_increment=1d0, &
                                                                & page_size=0d0),climb_rate=1d0, &
                                                                & digits=0_c_int)


    label_fanScale = gtk_label_new("Fan Scale (Optional)"//c_null_char)


    call g_signal_connect (spinButton_maxPupil, "changed"//c_null_char, &
    & c_funloc(callback_ray_fan_settings),c_loc(TARGET_RAYFAN_MAX_PUPIL))

    call g_signal_connect (spinButton_minPupil, "changed"//c_null_char, &
    & c_funloc(callback_ray_fan_settings),c_loc(TARGET_RAYFAN_MIN_PUPIL))



    call g_signal_connect (spinButton_wavelength, "changed"//c_null_char, &
    & c_funloc(callback_ray_fan_settings),c_loc(TARGET_RAYFAN_WAVELENGTH))


    !call g_signal_connect (spinButton_wavelength, "changed"//c_null_char, &
    !& c_funloc(callback_ray_fan_settings),c_loc(INT(ID_RAYFAN_WAVELENGTH)))


    call g_signal_connect (spinButton_numRays, "changed"//c_null_char, &
    & c_funloc(callback_ray_fan_settings), c_loc(TARGET_RAYFAN_NUMRAYS))

    !vals_scaleFactor = [character(len=40) :: "Autoscale (Default)", "Manual Scale"]
    !refs_scaleFactor = [ID_LENSDRAW_AUTOSCALE, ID_LENSDRAW_MANUALSCALE]


    !spinButton_scaleFactor = gtk_spin_button_new (gtk_adjustment_new(.045*1d0,0d0,1000*1d0,1d0,1d0,0d0),2d0, 3_c_int)
    !call gtk_widget_set_sensitive(spinButton_scaleFactor, FALSE)
    !call g_signal_connect (spinButton_scaleFactor, "changed"//c_null_char, c_funloc(spin_autoScale_callback), c_null_ptr)

    !call hl_gtk_combo_box_list2_new(cbox_scale, refs_scaleFactor, vals_scaleFactor, 1700_c_int)
    !call g_signal_connect (cbox_scale, "changed"//c_null_char, c_funloc(combo_autoScale_callback), spinButton_scaleFactor)

   !gdouble value,
   !gdouble lower,
   !gdouble upper,
   !gdouble step_increment,
   !gdouble page_increment,
   !gdouble page_size);


    !Spin buttons for selecting which surfaces to plot

    !label_first = gtk_label_new("First Surface"//c_null_char)
    !spinButton_firstSurface = gtk_spin_button_new (gtk_adjustment_new(1d0,1d0,(lastSurface-1)*1d0,1d0,1d0,0d0),2d0, 0_c_int)

    !label_last = gtk_label_new("Last Surface"//c_null_char)
    !spinButton_lastSurface = gtk_spin_button_new (gtk_adjustment_new(lastSurface*1d0,2d0,lastSurface*1d0, &
    !& 1d0,1d0,0d0),2d0, 0_c_int)

    !call g_signal_connect (spinButton_firstSurface, "value_changed"//c_null_char, c_funloc(spin_firstSurface_callback), c_null_ptr)
    !call g_signal_connect (spinButton_lastSurface, "value_changed"//c_null_char, c_funloc(spin_endSurface_callback), c_null_ptr)

    !call g_signal_connect (spinButton_lastSurface, "value_changed"//c_null_char, c_funloc(spin_firstSurface_callback), c_null_ptr)



    ! Spin Buttons for 3D Layout
    !label_elevation = gtk_label_new("Elevation Angle [deg]"//c_null_char)
    !spinButton_elevation = gtk_spin_button_new (gtk_adjustment_new(26.2d0,0d0,180*1d0,10d0,1d0,0d0),2d0, 1_c_int)

    !label_azimuth = gtk_label_new("Azimuth Angle [deg]"//c_null_char)
    !spinButton_azimuth = gtk_spin_button_new (gtk_adjustment_new(232.2d0,0d0,360*1d0, &
    !& 10d0,1d0,0d0),2d0, 1_c_int)

    !Default disable these
    !call gtk_widget_set_sensitive(spinButton_elevation, FALSE)
    !call gtk_widget_set_sensitive(spinButton_azimuth, FALSE)

    !call g_signal_connect (spinButton_elevation, "value_changed"//c_null_char, c_funloc(spin_elevation_callback), c_null_ptr)
    !call g_signal_connect (spinButton_azimuth, "value_changed"//c_null_char, c_funloc(spin_azimuth_callback), c_null_ptr)


    ! The combo box with predifined values of interesting Julia sets:
    label_fantype = gtk_label_new("Ray Fan:"//c_null_char)
    label_wfetype   = gtk_label_new("Aberration:"//c_null_char)


    !call g_signal_connect (combo_plotorientation, "changed"//c_null_char, c_funloc(combo_plotorientation_callback))

    ! A table container will contain buttons and labels:
    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)
    ! call gtk_grid_attach(table, button1, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, button2, 1_c_int, 3_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, button3, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
    !  call gtk_grid_attach(table, label1, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, label2, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, label3, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, spinButton1, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, spinButton2, 1_c_int, 1_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, spinButton3, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    ! call gtk_grid_attach(table, linkButton, 3_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, label_fantype, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, cbox, 1_c_int, 0_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_wfetype, 2_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, cbox_wfetype, 3_c_int, 0_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_maxPupil, 0_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_maxPupil, 1_c_int, 1_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_minPupil, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_minPupil, 1_c_int, 2_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_numRays, 2_c_int, 1_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_numRays, 3_c_int, 1_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_wavelength, 2_c_int, 2_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, spinButton_wavelength, 3_c_int, 2_c_int, 1_c_int,1_c_int)




    !call gtk_grid_attach(table, label_first, 2_c_int, 1_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, label_last, 2_c_int, 2_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_firstSurface, 3_c_int, 1_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_lastSurface, 3_c_int, 2_c_int, 1_c_int, 1_c_int)

    !call gtk_grid_attach(table, label_elevation, 0_c_int, 2_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, label_azimuth, 0_c_int, 3_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_elevation, 1_c_int, 2_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_azimuth, 1_c_int, 3_c_int, 1_c_int, 1_c_int)

    !call gtk_grid_attach(table, cbox_scale, 2_c_int, 3_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_scaleFactor, 3_c_int, 3_c_int, 1_c_int, 1_c_int)
    !call gtk_grid_attach(table, spinButton_azimuth, 1_c_int, 3_c_int, 1_c_int, 1_c_int)



    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, TRUE)

    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
    call gtk_box_append(box1, expander)

    !call gtk_box_append(box1, rf_window)
    call gtk_widget_set_vexpand (box1, FALSE)




  end subroutine ray_fan_settings_dialog


  subroutine ray_fan_new(parent_window)
    use mod_plotopticalsystem, only: DRAWOPTICALSYSTEM

    type(c_ptr) :: parent_window
    !type(c_ptr), value :: rf_window

    type(c_ptr) :: content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1
    !type(c_ptr)     :: my_drawing_area
    !integer(c_int)  :: width, height

    type(c_ptr)     :: table, expander, box1

    ! Create a modal dialogue
    rf_window = gtk_window_new()
    !call gtk_window_set_modal(di, TRUE)
    !title = "Lens Draw Window"
    !if (present(title)) call gtk_window_set_title(dialog, title)
    call gtk_window_set_title(rf_window, "Ray Fan Window"//c_null_char)
    !if (present(wsize)) then
    !   call gtk_window_set_default_size(dialog, wsize(1),&
    !        & wsize(2))
    !else

    width = 1000
    height = 700
       call gtk_window_set_default_size(rf_window, width, height)
    !end if

    !if (present(parent)) then
       call gtk_window_set_transient_for(rf_window, parent_window)
       call gtk_window_set_destroy_with_parent(rf_window, TRUE)
    !end if


    call ray_fan_settings_dialog(box1)

    rf_cairo_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(rf_cairo_drawing_area, width)
    call gtk_drawing_area_set_content_height(rf_cairo_drawing_area, height)

    call gtk_drawing_area_set_draw_func(rf_cairo_drawing_area, &
                   & c_funloc(DRAWOPTICALSYSTEM), c_null_ptr, c_null_funptr)
    !call gtk_drawing_area_set_draw_func(rf_cairo_drawing_area, &
    !               & c_funloc(TESTCAIRO2), c_null_ptr, c_null_funptr)

    PRINT *, "FINISHED WITH DRAWOPTICALSYSTEM"
    call gtk_box_append(box1, rf_cairo_drawing_area)
    !call gtk_window_set_child(rf_window, rf_cairo_drawing_area)
    call gtk_window_set_child(rf_window, box1)
    call gtk_widget_set_vexpand (box1, FALSE)

    call g_signal_connect(rf_window, "destroy"//c_null_char, c_funloc(rf_my_destroy), rf_window)


    call gtk_window_set_mnemonics_visible (rf_window, TRUE)
    !call gtk_widget_queue_draw(my_drawing_area)
    call gtk_widget_show(rf_window)


    PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine ray_fan_new

subroutine ray_fan_replot

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=100) :: ftext

!
!       SIMPLE FANS
!
!
        !IF(MESSAGE%WIN.EQ.IDD_FAN1) THEN
        !CALL WDIALOGSELECT(IDD_FAN1)
        !SELECT CASE(MESSAGE%VALUE1)
        !CASE (IDOK)
!       MAX PUPIL HT
        !CALL WDIALOGGETDOUBLE(IDF_MIN,DW1)

!       MIN PUPIL HT
        !CALL WDIALOGGETDOUBLE(IDF_MAX,DW2)

!       PLOT FAN SCALE
        !CALL WDIALOGGETDOUBLE(IDF_SSI,SSI)
        !CALL DTOA23(SSI,ASSI)

!       NUMBER OF RAYS IN FAN
        !CALL WDIALOGGETINTEGER(IDF_NUM,INUM)

!       FAN WAVELENGTH
        !CALL WDIALOGGETINTEGER(IDF_WAV,IWAV)
!
!       FAN PRIMARY TYPE
!       1=YFAN
!       2=XFAN
!       3=PFAN
!       4=NFAN
        !CALL WDIALOGGETRADIOBUTTON(IDF_FT1,ISET)
!
!       FAN SECONDARY TYPE
!       1=TRANSVERSE
!       2=OPD
!       3=CD
!       4=LA
        !CALL WDIALOGGETRADIOBUTTON(IDF_Q1,JSET)
!
!       PLOTTING ON OR OFF
!       0=NO PLOT
!       1=PLOT
        !CALL WDIALOGGETCHECKBOX(IDF_PLOT,ISTATE)
!
!       PLOTTING AUTOSCALE FACTOR
!       1=AUTOSCALE
!       0=NO AUTOSCALE
        !CALL WDIALOGGETCHECKBOX(IDF_AUTOSSI,KSTATE)
        !IF(KSTATE.EQ.1.AND.SSI.EQ.0.0D0) THEN
        !KSTATE=1
        !CALL WDIALOGPUTCHECKBOX(IDF_AUTOSSI,KSTATE)
        !                END IF
!
!       TRACE THE CORRECT CHIEF RAY
        !CALL CHIEFTRACE
!
        select case (rf_settings%fan_type)
          case (ID_RAYFAN_Y_FAN)
                PART1='YFAN '
          case (ID_RAYFAN_X_FAN)
                PART1='XFAN '
          case (ID_RAYFAN_P_FAN)
                PART1='PFAN '
          case (ID_RAYFAN_N_FAN)
                PART1='NFAN '
          case DEFAULT
               PART1='YFAN '
        end select
!
        select case (rf_settings%fan_wfe)
        case (ID_RAYFAN_TRANSVERSE_RAY)
        PART2='     '
      case (ID_RAYFAN_TRANSVERSE_OPD)
        PART2='OPD  '
      case (ID_RAYFAN_CHROMATIC)
        PART2='CD   '
      case (ID_RAYFAN_LONGITUDINAL)
        PART2='LA   '
      case DEFAULT
        PART2 = '    '
      end select
!
        !CALL DTOA23(DW1,AW1)
        !CALL DTOA23(DW2,AW2)
        CALL ITOAA(rf_settings%numRays, A6)
        CALL ITOAA(rf_settings%wavelength, AJ)

        PRINT *, "Max Pupil ", rf_settings%maxPupil
        PRINT *, "Min Pupil ", rf_settings%minPupil
        PRINT *, "NumRays ", rf_settings%numRays
        PRINT *, "Wavelength ", rf_settings%wavelength

        WRITE(AW2, *) rf_settings%maxPupil
        WRITE(AW1, *) rf_settings%minPupil
        !WRITE(A6, *)  rf_settings%numRays
        !WRITE(AJ, *)  rf_settings%wavelength
        !CALL ITOAA(IWAV,AJ)
        !CALL ITOA6(INUM,A6)
!
        ftext=PART1//TRIM(PART2)//','//AW1//','//AW2//','//AJ//','//A6
        PRINT *, ftext
        CALL PROCESKDP(ftext)
        !                IF(ISTATE.EQ.1) THEN
!       PLOTTING
        !                IF(KSTATE.EQ.0) THEN
!       USE SCALE
        !INPUT='DRAWFAN,'//ASSI//',1'
        !CALL PROCES
        !                ELSE
!       AUTO SCALE
        ftext='DRAWFAN,,1'
        CALL PROCESKDP(ftext)
        ftext = 'DRAW'
        CALL PROCESKDP(ftext)
        !                END IF
        !CALL GRAPHOUTPUT
        !                END IF

        !END SELECT
        !                        END IF
!

end subroutine ray_fan_replot

function wrap_integer_parameter(ID_SETTING) result(output_ptr)
    use iso_c_binding
    integer, intent(in) :: ID_SETTING
    type(c_ptr) :: output_ptr
    integer, target, save :: target_integer = -1
    integer, pointer :: tst_pointer

    PRINT *, "Integer to wrap is ", ID_SETTING

    target_integer = ID_SETTING
    output_ptr = c_loc(target_integer)

    call c_f_pointer(output_ptr, tst_pointer)

    PRINT *, "Integer Passed is ", tst_pointer

    PRINT *, "PTR IS, ", output_ptr

end function wrap_integer_parameter

end module
