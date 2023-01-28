module kdp_draw
   use cairo
   use gtk
   use global_widgets
contains


SUBROUTINE TESTCAIRO(widget, my_cairo_context, win_width, win_height, gdata) bind(c)

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       !use handlers

       !USE WINTERACTER
    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer                           :: cstatus
    integer                           :: t
    !real(8), parameter                :: pi = 3.14159265358979323846d0




    !   INTEGER NCOL256,ID,IDRAW1,ISKEY,INFO,IWX,IWY,IX,IY, NEUTTOTAL

    !type(c_ptr), value, intent(in) :: widget
    character(len=256), dimension(:), allocatable :: new_files, tmp
    logical, pointer :: idelete
    integer(kind=c_int) :: ipick
    !type(c_ptr) :: ld_window

    INTEGER  NEUTTOTAL
    !PRINT *, "Trying to draw system!"

    ! Bezier curve:
    call cairo_set_source_rgb(my_cairo_context, 0.9d0, 0.8d0, 0.8d0)
    call cairo_set_line_width(my_cairo_context, 4d0)
    call cairo_move_to(my_cairo_context, 0d0, 0d0)
    call cairo_curve_to(my_cairo_context, 600d0, 50d0, 115d0, 545d0, &
                      & win_width*1d0, win_height*1d0)
    call cairo_stroke(my_cairo_context)

    ! Lines:
    call cairo_set_source_rgb(my_cairo_context, 0d0, 0.5d0, 0.5d0)
    call cairo_set_line_width(my_cairo_context, 2d0)
    do t = 0, int(height), +20
      call cairo_move_to(my_cairo_context, 0d0, t*1d0)
      call cairo_line_to(my_cairo_context, t*1d0, win_height*1d0)
      call cairo_stroke(my_cairo_context)
    end do


END SUBROUTINE TESTCAIRO



SUBROUTINE TESTCAIRO2(widget, my_cairo_context, win_width, win_height, gdata) bind(c)

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       !use handlers

       !USE WINTERACTER
    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer                           :: cstatus
    integer                           :: t
    !real(8), parameter                :: pi = 3.14159265358979323846d0




    !   INTEGER NCOL256,ID,IDRAW1,ISKEY,INFO,IWX,IWY,IX,IY, NEUTTOTAL

    !type(c_ptr), value, intent(in) :: widget
    character(len=256), dimension(:), allocatable :: new_files, tmp
    logical, pointer :: idelete
    integer(kind=c_int) :: ipick
    !type(c_ptr) :: ld_window

    INTEGER  NEUTTOTAL
    !PRINT *, "Trying to draw system!"


    !Try to draw a few lines with different colors


    call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.0d0)


    call cairo_set_line_width(my_cairo_context, 4d0)
    call cairo_move_to(my_cairo_context, (0.5*win_height)*1d0, (0.5*win_width)*1d0)
    call cairo_line_to(my_cairo_context, (0.5*win_height)*1d0, (0.5*win_width+200)*1d0)
    call cairo_stroke(my_cairo_context)

    call cairo_set_source_rgb(my_cairo_context, 1.0d0, 1.0d0, 0.0d0)
    call cairo_set_line_width(my_cairo_context, 4d0)
    call cairo_move_to(my_cairo_context, (0.5*win_height)*1d0, (0.5*win_width+200)*1d0)

    call cairo_line_to(my_cairo_context, (0.5*win_height+200)*1d0, (0.5*win_width+200)*1d0)
    !call cairo_move_to(my_cairo_context, 0d0, 0d0)
    !call cairo_curve_to(my_cairo_context, 600d0, 50d0, 115d0, 545d0, &
    !                  & win_width*1d0, win_height*1d0)
    call cairo_stroke(my_cairo_context)

    ! Lines:
!    call cairo_set_source_rgb(my_cairo_context, 0d0, 0.5d0, 0.5d0)
!    call cairo_set_line_width(my_cairo_context, 2d0)
!    do t = 0, int(height), +20
!      call cairo_move_to(my_cairo_context, 0d0, t*1d0)
!      call cairo_line_to(my_cairo_context, t*1d0, win_height*1d0)
!      call cairo_stroke(my_cairo_context)
!    end do


END SUBROUTINE TESTCAIRO2

SUBROUTINE TESTCAIRO3(widget, my_cairo_context, win_width, win_height, gdata) bind(c)

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       !use handlers

       !USE WINTERACTER
    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer                           :: cstatus
    integer                           :: t

    call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.0d0)

    call cairo_rectangle (my_cairo_context, (win_width/4)*1d0, (win_height/2)*1d0, 200d0, 200d0);
    call cairo_fill (my_cairo_context);

    call cairo_set_source_rgb(my_cairo_context, 1.0d0, 0.0d0, 0.0d0)
    call cairo_rectangle (my_cairo_context, (win_width/4)*1d0, (win_height/2-300)*1d0, 200d0, 200d0);


    ! Manual way (just to check lower level commands)

    call cairo_move_to (my_cairo_context, (win_width/4)*3d0, (win_height/2)*1d0)
    call cairo_rel_line_to (my_cairo_context, 200d0, 0d0)
    call cairo_rel_line_to (my_cairo_context, 0d0, 200d0);
    call cairo_rel_line_to (my_cairo_context, -200d0, 0d0);
    call cairo_close_path (my_cairo_context);
    call cairo_set_source_rgb(my_cairo_context, 1.0d0, 0.0d0, 0.0d0)

    call cairo_fill(my_cairo_context)
    ! Test drawing outline
    call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.0d0)

    call cairo_move_to (my_cairo_context, (win_width/4)*3d0, (win_height/2)*1d0)
    call cairo_rel_line_to (my_cairo_context, 200d0, 0d0)
    call cairo_rel_line_to (my_cairo_context, 0d0, 200d0);
    call cairo_rel_line_to (my_cairo_context, -200d0, 0d0);
    call cairo_stroke(my_cairo_context)
    ! call cairo_close_path (my_cairo_context);

    ! Manual way 2 (just to check lower level commands)

    call cairo_move_to (my_cairo_context, (win_width/4)*3d0, (win_height/2-300)*1d0)
    call cairo_line_to (my_cairo_context, (win_width/4)*3d0+200d0, (win_height/2-300)*1d0)
    call cairo_line_to (my_cairo_context, (win_width/4)*3d0+200d0, (win_height/2-300)*1d0+200d0)
    call cairo_line_to (my_cairo_context, (win_width/4)*3d0, (win_height/2-300)*1d0+200d0);
    !call cairo_stroke(my_cairo_context)
    call cairo_close_path (my_cairo_context);
    call cairo_set_source_rgba(my_cairo_context, 0.0d0, 0.0d0, 0.0d0, 0.5d0)
    call cairo_fill(my_cairo_context)

END SUBROUTINE TESTCAIRO3

SUBROUTINE PRINTUSERTODEVICE(my_cairo_context,X,Y)
  type(c_ptr), value, intent(in)    :: my_cairo_context
  real(kind=c_double), intent(in), value :: X,Y

  REAL(kind=c_double), target :: Xtgt, Ytgt
  !REAL(kind=c_double), target :: Ytgt
  type(c_ptr) :: Xptr, Yptr
  real(kind=c_double), pointer :: Xtmp, Ytmp


      Xtgt = X
      Ytgt = Y
      Xptr = c_loc(Xtgt)
      Yptr = c_loc(Ytgt)

      call cairo_user_to_device(my_cairo_context, Xptr, Yptr)
      call c_f_pointer(Xptr, Xtmp)
      call c_f_pointer(Yptr, Ytmp)
      !PRINT *, "Cairo Line to Device X = ", Xtmp, " Y = ", Ytmp

END SUBROUTINE

SUBROUTINE IGRLINETO(my_cairo_context, cairo_drawing_area, X,Y)
  REAL, intent(IN) :: X,Y
  REAL(kind=c_double), target :: Xtgt = 1000.0
  REAL(kind=c_double), target :: Ytgt = 1000.0
  type(c_ptr) :: Xptr, Yptr
  real(kind=c_double), pointer :: Xtmp, Ytmp
  type(c_ptr), value, intent(in)    :: my_cairo_context, cairo_drawing_area
      call cairo_line_to(my_cairo_context, X*1d0, Y*1d0)

      !PRINT *, "Cairo Line to X = ", X, " Y = ", Y

      CALL PRINTUSERTODEVICE(my_cairo_context, X*1d0, Y*1d0)


      !PRINT *, "IGRLINETO being called!"
      !call cairo_move_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)


      !call gtk_window_set_child(ld_window, my_drawing_area)
      !call gtk_window_set_mnemonics_visible (ld_window, TRUE)
      !call gtk_widget_queue_draw(cairo_drawing_area)

END SUBROUTINE IGRLINETO



SUBROUTINE DRAWOPTICALSYSTEM(cairo_drawing_area, my_cairo_context, win_width, win_height, gdata) bind(c)

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       !use handlers

       !USE WINTERACTER
    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: cairo_drawing_area, my_cairo_context, gdata
    integer(c_int), value, intent(in) :: win_width, win_height
    integer                           :: cstatus
    integer                           :: t, DEBUG
    !real(8), parameter                :: pi = 3.14159265358979323846d0
    real(c_double) :: current_cairo_x, current_cairo_y
    REAL :: scaleFactor, fontScaleFactor




    !   INTEGER NCOL256,ID,IDRAW1,ISKEY,INFO,IWX,IWY,IX,IY, NEUTTOTAL

    !type(c_ptr), value, intent(in) :: widget
    character(len=256), dimension(:), allocatable :: new_files, tmp
    logical, pointer :: idelete
    integer(kind=c_int) :: ipick
    type(c_ptr) :: cairo_scaleFactor

    INTEGER  NEUTTOTAL
    !PRINT *, "Trying to draw system!"





      LOGICAL FIRST
      INTEGER CON_ARRAY,NX,NY,ZSTEP,ALLOERR
      REAL OPDPEAK,OPDPIT
      DIMENSION CON_ARRAY(:,:)
      ALLOCATABLE :: CON_ARRAY
      CHARACTER AB*80,STRINGER*1,C1A*20,C1B*20,C1C*20,C1D*20
      CHARACTER C1*80
      CHARACTER CC1*1
      CHARACTER CC2*2
      CHARACTER CC3*3
      CHARACTER CC4*4
      CHARACTER CC5*5
      CHARACTER CC6*6
      CHARACTER CC7*7
      CHARACTER CC8*8
      CHARACTER CC9*9
      CHARACTER CC10*10
      CHARACTER CC11*11
      CHARACTER CC12*12
      CHARACTER CC13*13
      CHARACTER CC14*14
      CHARACTER CC15*15
      CHARACTER CC16*16
      CHARACTER CC17*17
      CHARACTER CC18*18
      CHARACTER CC19*19
      CHARACTER CC20*20
      CHARACTER CC21*21
      CHARACTER CC22*22
      CHARACTER CC23*23
      CHARACTER CC24*24
      CHARACTER CC25*25
      CHARACTER CC26*26
      CHARACTER CC27*27
      CHARACTER CC28*28
      CHARACTER CC29*29
      CHARACTER CC30*30
      CHARACTER CC31*31
      CHARACTER CC32*32
      CHARACTER CC33*33
      CHARACTER CC34*34
      CHARACTER CC35*35
      CHARACTER CC36*36
      CHARACTER CC37*37
      CHARACTER CC38*38
      CHARACTER CC39*39
      CHARACTER CC40*40
      INTEGER I,I1,I2,I3,I4,I5,I6,I7,I8,JIMLEN
      INTEGER ISKEY,II1,II2,II3,II4,II5,II6,II7,II8 &
      ,COLPASS,COLTRUE,CHARSTYPE,ITYPER
      INTEGER J,II,JJ
      REAL CL1,CL2,CL3,CL4
      COMMON/OLDCLIP/CL1,CL2,CL3,CL4
      REAL IA,IB,RRR1,RRR2
      COMMON/TEXTCOM/CHARSTYPE
      COMMON/COLCOM/COLPASS,COLTRUE
      REAL JJ_X,JJ_Y
      COMMON/ASPECTER/JJ_X,JJ_Y

      REAL*8 sf

    integer(kind=c_int), pointer :: ID_SETTING

    call c_f_pointer(gdata, ID_SETTING)
    select case (ID_SETTING)
    case (ID_NEWPLOT_LENSDRAW)

        PRINT *, "DRAWOS CALLED FROM LENSDRAW"

  case (ID_NEWPLOT_RAYFAN)
      PRINT *, "DRAWOS CALLED FROM RAYFAN"

  end select


 !     INCLUDE 'DATMAI.INC'
 !     INCLUDE 'DATHGR.INC'
!     INITIALIZE CHARACTER ASPECT RATIO
      JJ_X=1.0
      JJ_Y=1.0

     scaleFactor = 1! 0.1


     DEBUG = 0
     IF (DEBUG.EQ.1) CALL PRINTNEUTARRAY

      PRINT *, "STARTING TO DRAW LENS"
                       J=1
      READ(NEUTARRAY(1),1000) NEUTTOTAL
 1000 FORMAT(I9,32X)
 300                   J=J+1
      IF(J.GE.NEUTTOTAL+1) GO TO 999
      ! DEBUG ONLY!  REMOVE THIS second exit when done

      !IF(J.GE.(17*NEUTTOTAL/32)) DEBUG=1
      !IF(J.GE.(4590)) DEBUG=1
      !IF(J.GE.(4700)) GO TO 999
      !IF(J.GE.(9*NEUTTOTAL/16)) GO TO 999

      !IF(J.GE.(9*NEUTTOTAL/16)) DEBUG=1
      !IF(J.GE.(4590)) DEBUG=1
      !IF(J.GE.(4700)) GO TO 999
      !IF(J.GE.(10*NEUTTOTAL/16)) GO TO 999

      READ(NEUTARRAY(J),2000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8
      !PRINT *, "STRINGER = ", STRINGER
      !IF (DEBUG.EQ.1) PRINT *, "NEUT STRINGER ", J, " ", STRINGER
      !IF (STRINGER.NE.'I') PRINT *, "STRINGER = ", STRINGER
 2000 FORMAT(A1,I5,I5,I5,I5,I5,I5,I5,I5)

      II1=(I1)
      II2=(I2)
      II3=(I3)
      II4=(I4)
      II5=(I5)
      II6=(I6)
      II7=(I7)
      II8=(I8)
      IF (DEBUG.EQ.1) WRITE(*,2000) STRINGER,I1,I2,I3,I4,I5,I6,I7,I8
!
!     "PLOT NEW" STRINGER = A
!
      IF(STRINGER.EQ.'A') THEN
!     INITIALIZE THE DRAW SCREEN
      !CALL NEWDRAWSCREEN
!
!     DEFINE DARK GREY AS BLACK
!     THIS LETS COLOR 0 BE RE-DEFINED AS ANY BACKGROUND COLOR DESIRED
      !CALL IGRPALETTERGB(240,0,0,0)
      call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.0d0)

      !call cairo_scale(my_cairo_context, 0.1d0, 0.1d0)
      CALL getVIECOScaleFactor(sf)
      !PRINT *, "SCFAY IN DRAW OPTICAL SYSTEM IS ", sf
    select case (ID_SETTING)
    case (ID_PLOTTYPE_RMSFIELD)
       call cairo_scale(my_cairo_context, .8*1d0, .8*1d0)
       kdp_height = 0
       fontScaleFactor = 1



  case default
      call cairo_scale(my_cairo_context, 0.1d0, 0.1d0)
      kdp_height = 7050.0
      fontScaleFactor = 20


  end select

      call cairo_set_line_width(my_cairo_context, fontScaleFactor*1d0)

      !call cairo_scale(my_cairo_context, 2.22/sf*1d0, 2.22/sf*1d0)
      !call cairo_scale(my_cairo_context, sf/222*1d0, sf/222*1d0)

      !call cairo_translate(my_cairo_context, 0*1d0, 5*height*1d0)
      call cairo_set_font_size(my_cairo_context, 5*fontScaleFactor*1d0)
      !call cairo_stroke(my_cairo_context)

!
      !DEFINE COLOR 208 AS WHITE
!     THIS LETS COLOR 0 BE RE-DEFINED AS ANY BACKGROUND COLOR DESIRED
      !CALL IGRPALETTERGB(208,255,255,255)
!     NOW READ ANOTHER COMMAND
               GO TO 300
               END IF
!
!     "PLOT COLTYP"
!
      IF(STRINGER.EQ.'E') THEN
!     SETTING THE FOREGROUND COLOR
      !  IF (II1.NE.COLPASS) THEN
      !   PRINT *, "COLPASS CHANGED NEWVAL = ", II1, " ", J
      !    COLTRUE = COLPASS
      ! END IF

! C               COLORS AND THEIR INTEGER CODES
! C
! C               0 = WHITE
! C               1 = LIGHT YELLOW
! C               2 = LIGHT MAGENTA
! C               3 = LIGHT RED
! C               4 = LIGHT CYAN
! C               5 = LIGHT GREEN
! C               6 = LIGHT BLUE
! C               7 = DARK GREY
! C               8 = LIGHT GREY
! C               9 = DARK YELLOW
! C              10 = DARK MAGENTA
! C              11 = DARK RED
! C              12 = DARK CYAN
! C              13 = DARK GREEN
! C              14 = DARK BLUE
! C              15 = BLACK
! C

      COLPASS=II1

      IF(COLPASS.EQ.0)  call cairo_set_source_rgb(my_cairo_context, 1.0d0, 1.0d0, 1.0d0)
!      IF(COLPASS.EQ.1)  call cairo_set_source_rgb(my_cairo_context, 1.0d0, 1.0d0, 0.0d0)
      IF(COLPASS.EQ.1)  call cairo_set_source_rgba(my_cairo_context, 0.75d0, 0.75d0, 0.0d0, (1-REAL(II2)/100.0)*1d0)

!      IF(COLPASS.EQ.1)  PRINT *, "COL PASS = 1!!"
      IF(COLPASS.EQ.2)  call cairo_set_source_rgba(my_cairo_context, 1.0d0, 0.0d0, 1.0d0, (1-REAL(II2)/100.0)*1d0)
      !IF(COLPASS.EQ.2)  call cairo_set_source_rgba(my_cairo_context, .51d0, .51d0, 0.0d0, (1-REAL(II2)/100.0)*1d0)

      !IF(COLPASS.EQ.2)  call cairo_set_source_rgb(my_cairo_context, 1.0d0, 0.0d0, 1.0d0)
      IF(COLPASS.EQ.2.AND.II2.GT.0)  PRINT *, "Alpha is ", (1-REAL(II2)/100.0)*1d0
      IF(COLPASS.EQ.3)  call cairo_set_source_rgb(my_cairo_context, .75d0, 0.0d0, 0.0d0)
      IF(COLPASS.EQ.4)  call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.75d0, 0.75d0)
      IF(COLPASS.EQ.5)  call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.75d0, 0.0d0)
      IF(COLPASS.EQ.6)  call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.75d0)
      IF(COLPASS.EQ.7)  call cairo_set_source_rgb(my_cairo_context, 0.25d0, 0.25d0, 0.25d0)
      IF(COLPASS.EQ.8)  call cairo_set_source_rgb(my_cairo_context, 0.51d0, 0.51d0, 0.51d0)
      IF(COLPASS.EQ.9)  call cairo_set_source_rgb(my_cairo_context, 0.51d0, 0.51d0, 0.0d0)
      IF(COLPASS.EQ.10) COLTRUE=192
      IF(COLPASS.EQ.11) COLTRUE=32
      IF(COLPASS.EQ.12) call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.513d0, 0.513d0)
      IF(COLPASS.EQ.13) COLTRUE=96
      IF(COLPASS.EQ.14) call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.513d0)
      IF(COLPASS.EQ.15) call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.0d0)

      call cairo_set_line_width(my_cairo_context, fontScaleFactor*1d0)
      !call cairo_stroke(my_cairo_context)
      !CALL IGrColourN(COLTRUE)
               GO TO 300
               END IF
!
!
!     "PLOT SETCHARACTERASPECT" = F
!
      IF(STRINGER.EQ.'F') THEN
!     SETTING THE CHARACTER ASPECT
      J=J+1
 3000 FORMAT(E15.7,E15.7,11X)
      READ(NEUTARRAY(J),3000) RRR1,RRR2
      JJ_X=RRR1
      JJ_Y=RRR2
!     NOW READ ANOTHER COMMAND
               GO TO 300
               END IF
!
!     "PLOT SETPAL" = G
!
      IF(STRINGER.EQ.'G') THEN
!     SETTING THE BACKGROUND COLOR TO A NEW VALUE
!     FORCES A SCREEN CLEAR AND A PLOT NEW EFFECT
      !IF(II1.EQ.0)  CALL IGrPaletteRGB(0,255,255,255)
      !IF(II1.EQ.1)  CALL IGrPaletteRGB(0,255,255,000)
      !IF(II1.EQ.2)  CALL IGrPaletteRGB(0,255,000,255)
      !IF(II1.EQ.3)  CALL IGrPaletteRGB(0,255,000,000)
      !IF(II1.EQ.4)  CALL IGrPaletteRGB(0,000,255,255)
      !IF(II1.EQ.5)  CALL IGrPaletteRGB(0,000,255,000)
      !IF(II1.EQ.6)  CALL IGrPaletteRGB(0,000,000,255)
      !IF(II1.EQ.7)  CALL IGrPaletteRGB(0,000,000,000)
      !IF(II1.EQ.8)  CALL IGrPaletteRGB(0,191,191,191)
      !IF(II1.EQ.9)  CALL IGrPaletteRGB(0,191,191,000)
      !IF(II1.EQ.10) CALL IGrPaletteRGB(0,191,000,191)
      !IF(II1.EQ.11) CALL IGrPaletteRGB(0,191,000,000)
      !IF(II1.EQ.12) CALL IGrPaletteRGB(0,000,191,191)
      !IF(II1.EQ.13) CALL IGrPaletteRGB(0,000,191,000)
      !IF(II1.EQ.14) CALL IGrPaletteRGB(0,000,000,191)
      !IF(II1.EQ.15) CALL IGrPaletteRGB(0,000,000,000)
      !PRINT *, "G II1 = ", II1
!     DEFINE DARK GREY AS BLACK
      !CALL IGRPALETTERGB(240,0,0,0)
      !CALL IGrViewport(-510.0,-510.0,10510.0,7510.0)
      !CALL IGrAreaClear
!     NOW READ ANOTHER COMMAND
               GO TO 300
               END IF
!
!     "PLOT MARKER" = C
!
      IF(STRINGER.EQ.'C') THEN
!     PLOTTING A SYMBOL
      !CALL IGrMarker(REAL(II3),REAL(II4),II1)
      !CALL IGrCharSize(1.0,1.0)
!     NOW READ ANOTHER COMMAND
               GO TO 300
               END IF
!
!     "PLOT SETFONT" = H
!
      IF(STRINGER.EQ.'H') THEN
!     CHANGING FONTS
      !CHARSTYPE=II1
!     NOW READ ANOTHER COMMAND
               GO TO 300
               END IF
!
!     "PLOT JUSTIFYSTRING" = D
!

      IF(STRINGER.EQ.'D') THEN

!     PLOTTING A STRING
                   J=J+1
 1003 FORMAT(A20,22X)
      READ(NEUTARRAY(J),1003)C1A
                   J=J+1
      READ(NEUTARRAY(J),1003)C1B
                   J=J+1
      READ(NEUTARRAY(J),1003)C1C
                   J=J+1
      READ(NEUTARRAY(J),1003)C1D
      AB='                    '
      C1=AB//AB//AB//AB
      !PRINT *, "STRINGER D ", C1A, " ", C1B, " ", C1C, " ", C1D
      !PRINT *, "OTHER VALS ", II1, " ", II2, " ", II3, " ", II4, " ", II5, " ", II6

 !     call cairo_move_to(my_cairo_context, (REAL(II1))*.1d0, &
 !     & (REAL(II2))*.1d0)

!      call cairo_show_text (my_cairo_context, trim(C1A//C1B//C1C//C1D)//c_null_char)


      IF(II6.LE.20) THEN
      C1(1:II6)=C1A(1:II6)
          END IF
      IF(II6.GT.20.AND.II6.LE.40) THEN
      C1(1:II6)=C1A(1:20)//C1B(1:II6-20)
          END IF
      IF(II6.GT.40.AND.II6.LE.60) THEN
      C1(1:II6)=C1A(1:20)//C1B(1:20)//C1C(1:II6-40)
          END IF
      IF(II6.GT.60.AND.II6.LE.80) THEN
      C1(1:II6)=C1A(1:20)//C1B(1:20)//C1C(1:20)//C1D(1:II6-60)
          END IF
     IF(CHARSTYPE.EQ.1) PRINT *, "SHOULD BE CALLING SETSTANDARD"
      IF(CHARSTYPE.EQ.2) PRINT *, "SHOULD BE CALLING SETSYMBOL"
!     IF(CHARSTYPE.EQ.1) CALL SETSTANDARD
!      IF(CHARSTYPE.EQ.2) CALL SETSYMBOL
      IA=0.0
      IB=35.0

      !call cairo_move_to(my_cairo_context, scaleFactor*(REAL(II1)+IA)*1d0, &
      !& scaleFactor*(REAL(II2)+IB)*1d0)
      call cairo_move_to(my_cairo_context, scaleFactor*(REAL(II1)+IA)*1d0, &
      & scaleFactor*(kdp_height-REAL(II2)+IB)*1d0)
      IF (II3.NE.0) PRINT *, "SHOULD BE CALLING IGrCharRotate"
!      CALL IGrCharRotate(REAL(II3))
!      CALL
!     1IGrCharSize((REAL(II4)*0.392*JJ_X),(REAL(II4)*0.363*JJ_Y))
!      CALL IGrCharRotate(REAL(II3))
!      IF(II3.EQ.0)
!     1CALL
!     2IGrCharSize((REAL(II4)*0.392*JJ_X),(REAL(II4)*0.30*JJ_Y))
!      IF(II3.EQ.90)
!     1CALL
!     2IGrCharSize((REAL(II4)*0.2*JJ_X),(REAL(II4)*0.363*JJ_Y))
      IF(II5.EQ.2) THEN
                   DO I=1,80
        IF(C1(1:1).EQ.' ') THEN
               C1(1:80)=C1(2:80)//' '
                   ELSE
               GO TO 88
                   END IF
                   END DO
 88                CONTINUE
                   JIMLEN=0
                   DO I=80,1,-1
        IF(C1(I:I).NE.' ') THEN
                   JIMLEN=I
                   GO TO 89
                   END IF
                   END DO
 89                CONTINUE
      IF(JIMLEN.EQ.1)   CC1=C1(1:1)
      IF(JIMLEN.EQ.2)   CC2=C1(1:2)
      IF(JIMLEN.EQ.3)   CC3=C1(1:3)
      IF(JIMLEN.EQ.4)   CC4=C1(1:4)
      IF(JIMLEN.EQ.5)   CC5=C1(1:5)
      IF(JIMLEN.EQ.6)   CC6=C1(1:6)
      IF(JIMLEN.EQ.7)   CC7=C1(1:7)
      IF(JIMLEN.EQ.8)   CC8=C1(1:8)
      IF(JIMLEN.EQ.9)   CC9=C1(1:9)
      IF(JIMLEN.EQ.10) CC10=C1(1:10)
      IF(JIMLEN.EQ.11) CC11=C1(1:11)
      IF(JIMLEN.EQ.12) CC12=C1(1:12)
      IF(JIMLEN.EQ.13) CC13=C1(1:13)
      IF(JIMLEN.EQ.14) CC14=C1(1:14)
      IF(JIMLEN.EQ.15) CC15=C1(1:15)
      IF(JIMLEN.EQ.16) CC16=C1(1:16)
      IF(JIMLEN.EQ.17) CC17=C1(1:17)
      IF(JIMLEN.EQ.18) CC18=C1(1:18)
      IF(JIMLEN.EQ.19) CC19=C1(1:19)
      IF(JIMLEN.EQ.20) CC20=C1(1:20)
      IF(JIMLEN.EQ.21) CC21=C1(1:21)
      IF(JIMLEN.EQ.22) CC22=C1(1:22)
      IF(JIMLEN.EQ.23) CC23=C1(1:23)
      IF(JIMLEN.EQ.24) CC24=C1(1:24)
      IF(JIMLEN.EQ.25) CC25=C1(1:25)
      IF(JIMLEN.EQ.26) CC26=C1(1:26)
      IF(JIMLEN.EQ.27) CC27=C1(1:27)
      IF(JIMLEN.EQ.28) CC28=C1(1:28)
      IF(JIMLEN.EQ.29) CC29=C1(1:29)
      IF(JIMLEN.EQ.30) CC30=C1(1:30)
      IF(JIMLEN.EQ.31) CC31=C1(1:31)
      IF(JIMLEN.EQ.32) CC32=C1(1:32)
      IF(JIMLEN.EQ.33) CC33=C1(1:33)
      IF(JIMLEN.EQ.34) CC34=C1(1:34)
      IF(JIMLEN.EQ.35) CC35=C1(1:35)
      IF(JIMLEN.EQ.36) CC36=C1(1:36)
      IF(JIMLEN.EQ.37) CC37=C1(1:37)
      IF(JIMLEN.EQ.38) CC38=C1(1:38)
      IF(JIMLEN.EQ.39) CC39=C1(1:39)
      IF(JIMLEN.EQ.40) CC40=C1(1:40)
                   END IF
      IF(II5.EQ.2) THEN
      !CALL IGrCharJustify('center')

      IF(JIMLEN.EQ.1) THEN
         call cairo_show_text (my_cairo_context, trim(C1A//C1B//C1C//C1D)//c_null_char)
      END IF
                       ELSE
      !CALL IGrCharJustify('left')
      call cairo_show_text (my_cairo_context, trim(C1A//C1B//C1C//C1D)//c_null_char)
      !call cairo_stroke(my_cairo_context)
      !CALL IGrCharOut(REAL(II1)+IA,REAL(II2)+IB,C1)
                       END IF


!      CALL IGrCHARSIZE(1.0,1.0)
!     NOW READ ANOTHER COMMAND
               GO TO 300
               END IF  ! STRINGER D PLOT TEXT

      IF(STRINGER.EQ.'I') THEN
      !IF (DEBUG.EQ.1.AND.II3.NE.0) PRINT *, " ", II1, " ", II2, " ", II3, " ", II4, " ", II5, " ", II6, " ", II7, " ", II8
      CALL JK_MoveToCAIRO(STRINGER, scaleFactor*REAL(II1),scaleFactor*REAL(II2),II3,II4, &
      REAL(II5), REAL(II6), REAL(II7), REAL(II8), my_cairo_context, cairo_drawing_area)

               GO TO 300
               END IF
!
!     "PLOT PLOTTOC" = J
!
      IF(STRINGER.EQ.'J') THEN
        ! PRINT *, "STRINGER = J!"
      !CALL JK_MoveToC(REAL(II1),REAL(II2),II3,II4,II5,II6,II7,II8)
      !PRINT *, "J II1 II2 are ", II1, ",", II2
      CALL JK_MoveToCAIRO('I', scaleFactor*REAL(II1),scaleFactor*REAL(II2),II3,II4, &
      REAL(II5), REAL(II6), REAL(II7), REAL(II8), my_cairo_context, cairo_drawing_area)

               GO TO 300
               END IF
!
!     CAPFN_OPD CONTOUR PLOTTING
!
!     JN Deleted STRINGER = K
      IF(STRINGER.EQ.'K') GO TO 300

      IF(STRINGER.EQ.'L') THEN
        PRINT *, "Stringer L Called!"
        !call cairo_move_to(my_cairo_context, REAL(II1)*1d0, (kdp_height-REAL(II2))*1d0)
        call cairo_close_path(my_cairo_context)

        call cairo_fill(my_cairo_context)

!
      END IF

!     "PLOT END" = B
      IF(J.EQ.NEUTTOTAL+1) THEN
      READ(NEUTARRAY(J),2000) STRINGER, &
      & I1,I2,I3,I4,I5,I6,I7,I8
      END IF
      IF(J.EQ.NEUTTOTAL+1.OR.STRINGER(1:1).NE.'B') THEN
      STRINGER='B'
      I1=0
      I2=0
      I3=0
      I4=0
      I5=0
      I6=0
      I7=0
      I8=0
      END IF
      IF(J.EQ.NEUTTOTAL+1.AND.STRINGER(1:1).EQ.'B') THEN
      !PRINT *, "DEBUG"
!      CALL IGrHardCopy('S')

               END IF

      GO TO 300
 999           CONTINUE
      PRINT *, "EXITING DRAWING ROUTINE!"


      RETURN

END SUBROUTINE DRAWOPTICALSYSTEM

SUBROUTINE JK_MOVETOCAIRO(STRINGER, MY_IX,MY_IY,MY_IPEN,MY_LINESTYLE,II5,II6,II7,II8, my_cairo_context, cairo_drawing_area)
      !USE WINTERACTER
      use iso_c_binding, only : c_f_pointer
      IMPLICIT NONE
      type(c_ptr), value, intent(in)    ::  my_cairo_context
      type(c_ptr), value, intent(in)    ::  cairo_drawing_area

      character*1, intent(in)  :: STRINGER
      REAL, optional :: II5, II6, II7, II8
      REAL INKER,MY_IX,MY_IY
      INTEGER MY_IPEN,MY_LINESTYLE, &
      MY_OLDIPEN
      REAL OX,OY,MY_OLDX,MY_OLDY,DELTADIST
      COMMON/MYLINESTUFF/OX,OY,MY_OLDX,MY_OLDY,MY_OLDIPEN
      REAL MY_DISTANCE,OLDDIST
      integer :: X, Y, XINC, YINC
      REAL REMAIN,OREMAIN,MYMOD
      REAL R5,R6,R7,R8
      integer :: units_long, unit_center
      COMMON/HOWFAR/MY_DISTANCE,OLDDIST,REMAIN,OREMAIN
      REAL SLOPE
      real*8, target :: dashes = 1.0
      type(c_ptr) :: dashes_ptr
      INKER=2.0
      R5=II5
      R6=II6
      R7=II7
      R8=II8

      !PRINT *, "R5, R6 = ", R5, " ", R6
      !PRINT *, "R7, R8 = ", R7, " ", R8


      IF(MY_IPEN.EQ.0) THEN
!     USER HAS THE PEN UP
!     WE ARE JUST MOVING SOME PLACE SO RE-SET MY_DISTANCE TO ZERO
         MY_DISTANCE=0
!     NOW MAKE THE MOVE
      !CALL IGRMOVETO(MY_IX,MY_IY)
      !PRINT *, "MOVE TO PEN UP!"
      !PRINT *, "MY_IX = ", MY_IX, " MY_IY = ", MY_IY
      ! Need to draw the current path before moving the pen, hence the call here to
      ! cairo_stroke
         !PRINT *, "Cairo Stroke call"
         call cairo_stroke(my_cairo_context)
         call cairo_move_to(my_cairo_context, MY_IX*1d0, (kdp_height-MY_IY)*1d0)
      !call cairo_stroke_preserve(my_cairo_context)
      !UPDATE OLD VALUES
         MY_OLDX=MY_IX
         MY_OLDY=MY_IY
         OX=MY_OLDX
         OY=MY_OLDY
         MY_OLDIPEN=MY_IPEN
         REMAIN=0.0
         OREMAIN=0.0
         MY_DISTANCE=0.0
         OLDDIST=0.0
                       RETURN
                       END IF
!
      IF(MY_IPEN.EQ.1) THEN
!     USER HAS THE PEN DOWN
        IF(MY_LINESTYLE.EQ.0) THEN
  !     SIMPLE SOLID LINE (TYPE 0)
        IF (STRINGER.EQ.'I') THEN
          MY_OLDX=MY_IX
          MY_OLDY=MY_IY
          OY=MY_OLDX
          OY=MY_OLDY
          MY_DISTANCE=0.0
          OLDDIST=0.0
          OLDDIST=0
          REMAIN=0.0
          OREMAIN=0.0
          !PRINT *, "WRITE LINE!"
          !PRINT *, "R6 is ", R6, "kdp_height is ", kdp_height
          call IGRLINETO(my_cairo_context, cairo_drawing_area, MY_IX, REAL(kdp_height-MY_IY))
          !call cairo_line_to(my_cairo_context, MY_IX*1d0, (kdp_height-MY_IY)*1d0)
          !call cairo_stroke(my_cairo_context)
          !call cairo_move_to(my_cairo_context, MY_IX*1d0, (kdp_height-MY_IY)*1d0)
          !call gtk_window_set_child(ld_window, my_drawing_area)
          !call gtk_window_set_mnemonics_visible (ld_window, TRUE)
          !call gtk_widget_queue_draw(cairo_drawing_area)
          !call gtk_widget_show(ld_window)
          !PRINT *, "SHOULD SEE GRAPHICS NOW!"
          !call DEBUGPROMPT
          !CALL IGRLINETO(MY_IX,MY_IY)
        ELSE IF (STRINGER.EQ.'J') THEN
        !  PRINT *, "IN STRINGER J Loop"

    !     SOLID LINE
    !     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
    !
    !     NOW DRAW THE LINE
          IF((MY_IX-MY_OLDX).NE.0.0) THEN
          SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                       IF(MY_IX.GE.MY_OLDX) XINC=INT(INKER)
                       IF(MY_IX.LT.MY_OLDX) XINC=-INT(INKER)
         !PRINT *, "MY_OLDX ", MY_OLDX
         !PRINT *, "MY_IX ", MY_IX
         !PRINT *, "OX, OX = ", OX, " ", OY

          DO X=INT(MY_OLDX),INT(MY_IX),XINC
                         !PRINT *, "X = ", X
          Y=((SLOPE*(X-OX))+OY)
    !    DRAW LINE
          IF(X.GE.R5.AND.X.LE.R6.AND.Y.GE.R7.AND.Y.LE.R8) THEN

          call IGRLINETO(my_cairo_context, cairo_drawing_area, MY_IX, REAL(kdp_height-MY_IY))
          !call cairo_line_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)

                       ELSE

           PRINT *, "J Move To "
           call cairo_stroke(my_cairo_context)
           call cairo_move_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)

                       END IF
          !call gtk_widget_queue_draw(cairo_drawing_area)
          OX=X
          OY=Y
                           END DO
    !     UPDATE OLD VALUES
          ! PRINT *, "Update Old Values"
          MY_OLDX=MY_IX
          MY_OLDY=MY_IY
          OX=MY_OLDX
          OY=MY_OLDY
          MY_OLDIPEN=MY_IPEN
                           ELSE
    !     LINE IS VERTICAL
                       IF(MY_IY.GE.MY_OLDY) YINC=INT(INKER)
                       IF(MY_IY.LT.MY_OLDY) YINC=-INT(INKER)
                       DO Y=INT(MY_OLDY),INT(MY_IY),YINC
          X=MY_IX
    !     DRAW LINE
          IF(X.GE.R5.AND.X.LE.R6.AND.Y.GE.R7.AND.Y.LE.R8) THEN
            !PRINT *, " X Y ", X, " ", Y
            PRINT *, "Drawing J Vertical Line"
          call IGRLINETO(my_cairo_context, cairo_drawing_area, MY_IX, REAL(kdp_height-MY_IY))
          !call cairo_line_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)
          call cairo_stroke(my_cairo_context)
          call cairo_move_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)
          call gtk_widget_queue_draw(cairo_drawing_area)
    !      CALL IGRLINETO(X,Y)

                       ELSE
    !      CALL IGRMOVETO(X,Y)
           call cairo_move_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)
                       END IF

          OX=X
          OY=Y
                       END DO
                     END IF ! Stringer J logic

        END IF
                       RETURN
                       ELSE
                       END IF
!*****************************************************************************
      IF(MY_LINESTYLE.GT.0) THEN
         !PRINT *, "LINESTYLE = ", MY_LINESTYLE
         select case (MY_LINESTYLE)
         case (1)
  !     DOTTED LINE, 10 UNITS LONG ON 40 UNIT CENTERS
           units_long = 10
           unit_center = 40
        case(2)
             units_long = 40
             unit_center = 100
!     DASHED LINE, 40 UNITS LONG ON 100 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
         end select


!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INT(INKER)
                   IF(MY_IX.LT.MY_OLDX) XINC=-INT(INKER)
                   DO X=INT(MY_OLDX),INT(MY_IX),XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= (SQRT(((OX-X)**2) +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/unit_center)*unit_center*1d0
      REMAIN=(MY_DISTANCE-MYMOD)
      IF(OREMAIN.LT.units_long.AND.REMAIN.LT.units_long) THEN
!     DRAW LINE
      IF(STRINGER.EQ.'I') THEN
        call cairo_set_dash(my_cairo_context, c_loc(dashes), 1, 0d0)
        CALL IGRLINETO(my_cairo_context, cairo_drawing_area, REAL(X),REAL(kdp_height-Y))
        !call cairo_set_dash(my_cairo_context, c_loc(dashes), 0, 0d0)
      END IF

      IF (STRINGER.EQ.'J'.AND.X.GE.R5.AND.X.LE.R6.AND.Y.GE.R7.AND.Y.LE.R8) THEN
            CALL IGRLINETO(my_cairo_context, cairo_drawing_area, REAL(X),REAL(kdp_height-Y))
      ELSE

      call cairo_move_to(my_cairo_context, X*1d0, (kdp_height-Y)*1d0)
      END IF

      OX=X
      OY=Y
                   ELSE
!     MOVE
      !CALL IGRMOVETO(X,Y)
      OX=X
      OY=Y
                   END IF
                       END DO
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INT(INKER)
                   IF(MY_IY.LT.MY_OLDY) YINC=-INT(INKER)
                   DO Y=INT(MY_OLDY),INT(MY_IY),YINC
      X=MY_IX
      DELTADIST= (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/unit_center)*unit_center*1d0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.units_long.AND.REMAIN.LT.units_long) THEN
!     DRAW LINE
      !CALL IGRLINETO(X,Y)
      OX=X
      OY=Y
                   ELSE
!     MOVE
      !CALL IGRMOVETO(X,Y)
      OX=X
      OY=Y
                   END IF
                   END DO
                       END IF
                       ELSE
!     NOT LINE TYPE 1
                       END IF
!*****************************************************************************


!*****************************************************************************
!     UPDATE OLD VALUES
      !PRINT *, "Update OLD VALUES"
      MY_OLDX=MY_IX
      MY_OLDY=MY_IY
      OX=MY_OLDX
      OY=MY_OLDY
      MY_OLDIPEN=MY_IPEN
                       ELSE
!     IPEN NOT 1
                       END IF
                        RETURN
                        END

end module kdp_draw
