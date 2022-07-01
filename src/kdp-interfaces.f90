module kdp_interfaces

contains

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
