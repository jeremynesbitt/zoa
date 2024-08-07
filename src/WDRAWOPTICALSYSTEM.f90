SUBROUTINE WDRAWOPTICALSYSTEM

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       use global_widgets
       use handlers
       use zoa_ui

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
    ! TEMP if statement for debugging
!     if (active_plot.NE.ID_PLOTTYPE_LENSDRAW) then
!     call zoatabMgr%newPlotIfNeeded(active_plot)
!     end if

10 CONTINUE

                        END
