module kdp_interfaces

contains

subroutine POWSYM


   use global_widgets
   use handlers, only : updateTerminalLog
    use zoa_plot
    use mod_plotopticalsystem


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

   PRINT *, "New Command POWSYM in kdp_interfaces!"


  !PRINT *, "Before Mod Call, ", my_window

  !ipick = hl_gtk_file_chooser_show(new_files, &
  !       & create=FALSE, multiple=TRUE, filter=["image/*"], &
  !       & parent=my_window, all=TRUE)

  !ftext = 'LIB GET 1 '
  !ftext = 'CV2PRG DoubleGauss.seq'
  ! ftext = 'CV2PRG LithoBraat.seq'
  !
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'RTG ALL'
  ! CALL PROCESKDP(ftext)
  !
  !
  ! ftext = 'COLORSET RAYS 6'
  ! CALL PROCESKDP(ftext)
  ! call getOpticalSystemLastSurface(endSurface)
  ! call ld_settings%set_end_surface(endSurface)
  ! ftext = 'VIECO'
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'PXTY ALL'
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'OCDY'
  ! CALL PROCESKDP(ftext)


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

end subroutine POWSYM

subroutine EDITOR
  use lens_editor
  use global_widgets
  use handlers

    if (.not. c_associated(lens_editor_window))  THEN
       PRINT *, "Call New Lens Editor Window"
       call lens_editor_new(my_window)
    else
      PRINT *, "Do nothing..lens editor exists. "

    end if



end subroutine EDITOR

SUBROUTINE RUN_WDRAWOPTICALSYSTEM
!     THIS IS THE DRIVER ROUTINE FOR SENDING GRAPHICS TO
!     A GRAPHIC WINDOW
      !USE WINTERACTER
      IMPLICIT NONE
      LOGICAL EXISD
      !INCLUDE 'DATMAI.INC'
      !CALL WDRAWOPTICALSYSTEM
      RETURN
END SUBROUTINE RUN_WDRAWOPTICALSYSTEM



SUBROUTINE WDRAWOPTICALSYSTEM

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       use global_widgets
       use handlers
       use zoa_ui
       use kdp_draw
       use mod_plotrayfan, only: ray_fan_new !ray_fan_plot_check_status !
       use mod_plotopticalsystem
       use mod_lens_draw_settings
       !USE WINTERACTER
       IMPLICIT NONE





       LOGICAL FIRST
       INTEGER NCOL256,ID,IDRAW1,ISKEY,INFO,IWX,IWY,IX,IY, NEUTTOTAL

    !type(c_ptr), value, intent(in) :: widget
    character(len=256), dimension(:), allocatable :: new_files, tmp
    logical, pointer :: idelete
    integer(kind=c_int) :: ipick, i




       !PRINT *, "After Mod Call, ", my_window
       !PRINT *, "NEUTARRAY is ", NEUTARRAY

       FIRST=.TRUE.
       ISKEY=-999
       READ(NEUTARRAY(1),1000) NEUTTOTAL
 1000  FORMAT(I9,32X)
       PRINT *, "NEUTTOTAL IS ", NEUTTOTAL
       IF(NEUTTOTAL.EQ.0) GO TO 10
!     INITIALIZE SCREEN
!     IDRAW1 IS THE WINDOW HANDLE
!     DRW IS THE STRUCTURE WHICH PASSES CHILD WINDOW CHARACTERISTICS
!     TO THE WINDOW
      !CALL WindowOpenChild(DRW,IDRAW1)
      ! Define black and white, and set the color to black

      !CALL IGRPALETTERGB(223,0,0,0)
      !CALL IGRPALETTERGB(0,255,255,255)
      !CALL IGrColourN(223)
    PRINT *, "Active Plot is ", active_plot


    select case (active_plot)
    case (ID_NEWPLOT_LENSDRAW)

    ! Only support one window at present.
    if (.not. c_associated(ld_window))  THEN
        call lens_draw_new(my_window)
    else
       call gtk_widget_queue_draw(ld_cairo_drawing_area)
    end if

  case (ID_NEWPLOT_RAYFAN)

    ! Only support one window at present.
    PRINT *, "RAY FAN Plotting Initiated!"
    !call ray_fan_plot_check_status()
    if (.not. c_associated(rf_window))  THEN
       PRINT *, "Call New Ray Fan Window"
       call ray_fan_new(my_window)
    else
      PRINT *, "Redraw Ray Fan "
      call gtk_widget_queue_draw(rf_cairo_drawing_area)
    end if
    PRINT *, "READY TO PLOT!"
  end select

    !call DRAWDEBUG
      !CALL DRAWOPTICALSYSTEM(1,FIRST,ISKEY)


      !CALL WindowCloseChild(IDRAW1)
      !CALL WindowSelect(1)
 10   CONTINUE
                        RETURN
                        END


 !      SUBROUTINE MY_COLTYP_ALPHA(COLPAS, alpha)
 !      USE GLOBALS
 !      IMPLICIT NONE
 !      CHARACTER STRINGER*1,NEUTLINE*42
 !      INTEGER COLPAS, NEUTTOTAL, MAXNEUTRAL
 !      ! alpha here is 100x * transparency to keep it an integer
 !      INTEGER, optional :: alpha
 !      INTEGER I1,I2,I3,I4,I5,I6,I7,I8
 !      INTEGER II1,II2,II3,II4,II5,II6,II7,II8
 !
 !      COMMON NEUTTOTAL, MAXNEUTRAL, NEUTARRAY
 !
 !      !INCLUDE 'DATHGR.INC'
 !      !INCLUDE 'DATMAI.INC'
 !
 !      if (.not.present(alpha)) alpha = 0
 !
 !
 !      I1=COLPAS
 !      I2=alpha
 !      I3=0
 !      I4=0
 !      I5=0
 !      I6=0
 !      I7=0
 !      I8=0
 !      IF(I1.GT.99999) I1=99999
 !      IF(I2.GT.99999) I2=99999
 !      IF(I3.GT.99999) I3=99999
 !      IF(I4.GT.99999) I4=99999
 !      IF(I5.GT.99999) I5=99999
 !      IF(I6.GT.99999) I6=99999
 !      IF(I7.GT.99999) I7=99999
 !      IF(I8.GT.99999) I8=99999
 !      IF(I1.LT.-9999) I1=-9999
 !      IF(I2.LT.-9999) I2=-9999
 !      IF(I3.LT.-9999) I3=-9999
 !      IF(I4.LT.-9999) I4=-9999
 !      IF(I5.LT.-9999) I5=-9999
 !      IF(I6.LT.-9999) I6=-9999
 !      IF(I7.LT.-9999) I7=-9999
 !      IF(I8.LT.-9999) I8=-9999
 !      STRINGER='E'
 !      NEUTTOTAL=NEUTTOTAL+1
 !      PRINT 1000, STRINGER, I1,I2,I3,I4,I5,I6,I7,I8
 !      WRITE(NEUTLINE,1000) STRINGER, I1,I2,I3,I4,I5,I6,I7,I8
 !      IF(NEUTTOTAL+1.GE.MAXNEUTRAL/2) CALL RESIZE_NEUT
 !      NEUTARRAY(NEUTTOTAL+1)=NEUTLINE
 !
 ! 1000 FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)
 !                        RETURN
 !                        END

end module kdp_interfaces
