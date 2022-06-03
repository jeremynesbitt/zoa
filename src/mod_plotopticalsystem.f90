
module mod_plotopticalsystem


  use GLOBALS
  use global_widgets
  use cairo
!  use gth_hl
  use gtk_hl_container
  use gtk_hl_progress
  use gtk_hl_button
  use gtk_hl_tree
  use gtk
  ! use gtk, only: gtk_button_new, gtk_window_set_child, gtk_window_destroy, &
  !      & gtk_progress_bar_new, gtk_widget_show, gtk_window_new, &
  !      & gtk_init, gtk_drawing_area_new, gtk_drawing_area_set_content_width, &
  !      & gtk_drawing_area_set_content_height, gtk_drawing_area_set_draw_func, &
  !      & gtk_window_set_mnemonics_visible, gtk_widget_queue_draw, &
  !      & gtk_expander_new_with_mnemonic, gtk_expander_set_child, &
  !      & gtk_expander_set_expanded, gtk_combo_box_text_new, gtk_combo_box_text_get_active_text, &
  !      & gtk_combo_box_text_append_text, gtk_combo_box_set_active, gtk_list_store_newv, &
  !      & gtk_list_store_append, gtk_list_store_get_type, gtk_list_store_set_value, &
  !      & gtk_combo_box_new_with_model_and_entry
  use gtk_hl_chooser
!  use g, only: g_usleep, g_main_context_iteration, g_main_context_pending, &
!      & g_value_init
  use g

  implicit none
  type(c_ptr) :: win,bar,pbar,qbut, box
  integer(kind=c_int) :: run_status = TRUE
  integer(c_int) :: boolresult
  type(c_ptr) :: ld_window = c_null_ptr
  type(c_ptr) :: cairo_drawing_area !Put this here to debug issue with plotting

  type(c_ptr) ::  combo_plotorientation




contains
  subroutine my_destroy(widget, gdata) bind(c)
    type(c_ptr), value :: widget, gdata
    print *, "Exit called"
    run_status = FALSE

    call gtk_window_destroy(ld_window)
    ld_window = c_null_ptr
  end subroutine my_destroy


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


SUBROUTINE TESTCAIRO(widget, my_cairo_context, win_width, win_height, gdata) bind(c)

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       use handlers

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
       use handlers

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


SUBROUTINE DRAWOPTICALSYSTEM(widget, my_cairo_context, win_width, win_height, gdata) bind(c)

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       use handlers
       !USE WINTERACTER
    IMPLICIT NONE

    type(c_ptr), value, intent(in)    :: widget, my_cairo_context, gdata
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
    type(c_ptr) :: ld_window, cairo_scaleFactor

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
 !     INCLUDE 'DATMAI.INC'
 !     INCLUDE 'DATHGR.INC'
!     INITIALIZE CHARACTER ASPECT RATIO
      JJ_X=1.0
      JJ_Y=1.0

     scaleFactor = 1! 0.1
     fontScaleFactor = 20
     DEBUG = 0
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
      call cairo_set_line_width(my_cairo_context, fontScaleFactor*1d0)

      call cairo_scale(my_cairo_context, 0.1d0, 0.1d0)
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
       IF (II1.NE.COLPASS) THEN
         PRINT *, "COLPASS CHANGED NEWVAL = ", II1, " ", J
         !COLTRUE = COLPASS
      END IF

      COLPASS=II1

      IF(COLPASS.EQ.0)  call cairo_set_source_rgb(my_cairo_context, 1.0d0, 1.0d0, 1.0d0)
!      IF(COLPASS.EQ.1)  call cairo_set_source_rgb(my_cairo_context, 1.0d0, 1.0d0, 0.0d0)
      IF(COLPASS.EQ.1)  call cairo_set_source_rgb(my_cairo_context, 0.0d0, 0.0d0, 0.0d0)

!      IF(COLPASS.EQ.1)  PRINT *, "COL PASS = 1!!"
      IF(COLPASS.EQ.2)  call cairo_set_source_rgb(my_cairo_context, 1.0d0, 0.0d0, 1.0d0)
      IF(COLPASS.EQ.3)  COLTRUE=16
      IF(COLPASS.EQ.4)  COLTRUE=112
      IF(COLPASS.EQ.5)  COLTRUE=80
      IF(COLPASS.EQ.6)  COLTRUE=144
      IF(COLPASS.EQ.7)  COLTRUE=240
      IF(COLPASS.EQ.8)  COLTRUE=224
      IF(COLPASS.EQ.9)  COLTRUE=64
      IF(COLPASS.EQ.10) COLTRUE=192
      IF(COLPASS.EQ.11) COLTRUE=32
      IF(COLPASS.EQ.12) COLTRUE=128
      IF(COLPASS.EQ.13) COLTRUE=96
      IF(COLPASS.EQ.14) COLTRUE=160
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
      PRINT *, "G II1 = ", II1
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
      PRINT *, "STRINGER D ", C1A, " ", C1B, " ", C1C, " ", C1D
      PRINT *, "OTHER VALS ", II1, " ", II2, " ", II3, " ", II4, " ", II5, " ", II6

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

      call cairo_move_to(my_cairo_context, scaleFactor*(REAL(II1)+IA)*1d0, &
      & scaleFactor*(REAL(II2)+IB)*1d0)
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
      IF (DEBUG.EQ.1) PRINT *, " ", II1, " ", II2, " ", II3, " ", II4, " ", II5, " ", II6, " ", II7, " ", II8
      CALL JK_MoveToCAIRO(scaleFactor*REAL(II1),scaleFactor*REAL(II2),II3,II4, my_cairo_context)

               GO TO 300
               END IF
!
!     "PLOT PLOTTOC" = J
!
      IF(STRINGER.EQ.'J') THEN

      !CALL JK_MoveToC(REAL(II1),REAL(II2),II3,II4,II5,II6,II7,II8)
               GO TO 300
               END IF
!
!     CAPFN_OPD CONTOUR PLOTTING
!
!     JN Deleted STRINGER = K
      IF(STRINGER.EQ.'K') GO TO 300
!

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
      PRINT *, "DEBUG"
!      CALL IGrHardCopy('S')

               END IF

      GO TO 300
 999           CONTINUE
      PRINT *, "EXITING ROUTINE!"


      RETURN

END SUBROUTINE DRAWOPTICALSYSTEM



SUBROUTINE JK_MOVETOCAIRO(MY_IX,MY_IY,MY_IPEN,MY_LINESTYLE, my_cairo_context)
      !USE WINTERACTER
      IMPLICIT NONE
      type(c_ptr), value, intent(in)    ::  my_cairo_context
      REAL INKER,MY_IX,MY_IY,XINC,YINC
      INTEGER MY_IPEN,MY_LINESTYLE, &
      MY_OLDIPEN
      REAL OX,OY,MY_OLDX,MY_OLDY,DELTADIST
      COMMON/MYLINESTUFF/OX,OY,MY_OLDX,MY_OLDY,MY_OLDIPEN
      REAL MY_DISTANCE,X,Y,OLDDIST
      REAL REMAIN,OREMAIN,MYMOD
      COMMON/HOWFAR/MY_DISTANCE,OLDDIST,REMAIN,OREMAIN
      REAL SLOPE
      INKER=2.0
!
      IF(MY_IPEN.EQ.0) THEN
!     USER HAS THE PEN UP
!     WE ARE JUST MOVING SOME PLACE SO RE-SET MY_DISTANCE TO ZERO
      MY_DISTANCE=0
!     NOW MAKE THE MOVE
      !CALL IGRMOVETO(MY_IX,MY_IY)
      !PRINT *, "MOVE TO PEN UP!"
      call cairo_move_to(my_cairo_context, MY_IX*1d0, MY_IY*1d0)
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
      call cairo_line_to(my_cairo_context, MY_IX*1d0, MY_IY*1d0)
      call cairo_stroke(my_cairo_context)
      call cairo_move_to(my_cairo_context, MY_IX*1d0, MY_IY*1d0)
      !call gtk_window_set_child(ld_window, my_drawing_area)
      !call gtk_window_set_mnemonics_visible (ld_window, TRUE)
      call gtk_widget_queue_draw(cairo_drawing_area)
      !call gtk_widget_show(ld_window)
      !PRINT *, "SHOULD SEE GRAPHICS NOW!"
      !call DEBUGPROMPT
      !CALL IGRLINETO(MY_IX,MY_IY)
                       RETURN
                       ELSE
                       END IF
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.1) THEN
!     DOTTED LINE, 10 UNITS LONG ON 40 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/40)*40.0
      REMAIN=(MY_DISTANCE-MYMOD)
      IF(OREMAIN.LT.10.0.AND.REMAIN.LT.10.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/40)*40.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.10.0.AND.REMAIN.LT.10.0) THEN
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
      IF(MY_LINESTYLE.EQ.2) THEN
!     DASHED LINE, 40 UNITS LONG ON 100 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/100)*100.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.40.0.AND.REMAIN.LT.40.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/100)*100.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.40.0.AND.REMAIN.LT.40.0) THEN
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
!     NOT LINE TYPE 2
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.3) THEN
!     60 UNIT DASH,60 UNIT SPACE, 20 UNIT DASH, 60 UNIT
!     SPACE ON 200 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/200)*200.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.60.0.AND.REMAIN.LT.60.0.OR. &
      OREMAIN.GT.120.0.AND.REMAIN.GT.120.0.AND. &
      OREMAIN.LT.140.0.AND.REMAIN.LT.140.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/200)*200.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.60.0.AND.REMAIN.LT.60.0.OR. &
      OREMAIN.GT.120.0.AND.REMAIN.GT.120.0.AND. &
      OREMAIN.LT.140.0.AND.REMAIN.LT.140.0) THEN
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
!     NOT LINE TYPE 3
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.4) THEN
!     60 UNIT DASH,60 UNIT SPACE, 20 UNIT DASH, 60 UNIT SPACE, 20 UNIT DASH
!     60 UNIT SPACE ON 280 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/280)*280.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.60.0.AND.REMAIN.LT.60.0.OR. &
      OREMAIN.GT.120.0.AND.REMAIN.GT.120.0.AND. &
      OREMAIN.LT.140.0.AND.REMAIN.LT.140.0.OR. &
      OREMAIN.GT.200.0.AND.REMAIN.GT.200.0.AND. &
      OREMAIN.LT.220.0.AND.REMAIN.LT.220.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/280)*280.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.60.0.AND.REMAIN.LT.60.0.OR. &
      OREMAIN.GT.120.0.AND.REMAIN.GT.120.0.AND. &
      OREMAIN.LT.140.0.AND.REMAIN.LT.140.0.OR. &
      OREMAIN.GT.200.0.AND.REMAIN.GT.200.0.AND. &
      OREMAIN.LT.220.0.AND.REMAIN.LT.220.0) THEN
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
!     NOT LINE TYPE 4
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.5) THEN
!     80 UNIT DASH,60 UNIT SPACE, 20 UNIT DASH, 60 UNIT SPACE, 20 UNIT DASH
!     60 UNIT SPACE A 20 UNIT DASH AND A 60 UNIT SPACE ON 380 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/380)*380.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.80.0.AND.REMAIN.LT.80.0.OR. &
      OREMAIN.GT.140.0.AND.REMAIN.GT.140.0.AND. &
      OREMAIN.LT.160.0.AND.REMAIN.LT.160.0.OR. &
      OREMAIN.GT.220.0.AND.REMAIN.GT.220.0.AND. &
      OREMAIN.LT.240.0.AND.REMAIN.LT.240.0.OR. &
      OREMAIN.GT.300.0.AND.REMAIN.GT.300.0.AND. &
      OREMAIN.LT.320.0.AND.REMAIN.LT.320.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/380)*380.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.80.0.AND.REMAIN.LT.80.0.OR. &
      OREMAIN.GT.140.0.AND.REMAIN.GT.140.0.AND. &
      OREMAIN.LT.160.0.AND.REMAIN.LT.160.0.OR. &
      OREMAIN.GT.220.0.AND.REMAIN.GT.220.0.AND. &
      OREMAIN.LT.240.0.AND.REMAIN.LT.240.0.OR. &
      OREMAIN.GT.300.0.AND.REMAIN.GT.300.0.AND. &
      OREMAIN.LT.320.0.AND.REMAIN.LT.320.0) THEN
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
!     NOT LINE TYPE 5
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.6) THEN
!     80 UNIT DASH,60 UNIT SPACE, 80 UNIT DASH, 60 UNIT SPACE, 20 UNIT DASH
!     60 UNIT SPACE A 20 UNIT DASH AND A 60 UNIT SPACE ON 440 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/440)*440.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.80.0.AND.REMAIN.LT.80.0.OR. &
      OREMAIN.GT.140.0.AND.REMAIN.GT.140.0.AND. &
      OREMAIN.LT.220.0.AND.REMAIN.LT.220.0.OR. &
      OREMAIN.GT.280.0.AND.REMAIN.GT.280.0.AND. &
      OREMAIN.LT.300.0.AND.REMAIN.LT.300.0.OR. &
      OREMAIN.GT.360.0.AND.REMAIN.GT.360.0.AND. &
      OREMAIN.LT.380.0.AND.REMAIN.LT.380.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/440)*440.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.80.0.AND.REMAIN.LT.80.0.OR. &
      OREMAIN.GT.140.0.AND.REMAIN.GT.140.0.AND. &
      OREMAIN.LT.220.0.AND.REMAIN.LT.220.0.OR. &
      OREMAIN.GT.280.0.AND.REMAIN.GT.280.0.AND. &
      OREMAIN.LT.300.0.AND.REMAIN.LT.300.0.OR. &
      OREMAIN.GT.360.0.AND.REMAIN.GT.360.0.AND. &
      OREMAIN.LT.380.0.AND.REMAIN.LT.380.0) THEN
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
!     NOT LINE TYPE 6
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.7) THEN
!     80 UNIT DASH,60 UNIT SPACE, 80 UNIT DASH, 60 UNIT SPACE, 80 UNIT DASH
!     60 UNIT SPACE A 20 UNIT DASH AND A 60 UNIT SPACE ON 500 UNIT CENTERS
!     WE ARE DRAWING TO SOME PLACE SO UPDATE MY_DISTANCE
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      SQRT(((OX-X)**2) &
      +((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/500)*500.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.80.0.AND.REMAIN.LT.80.0.OR. &
      OREMAIN.GT.140.0.AND.REMAIN.GT.140.0.AND. &
      OREMAIN.LT.220.0.AND.REMAIN.LT.220.0.OR. &
      OREMAIN.GT.280.0.AND.REMAIN.GT.280.0.AND. &
      OREMAIN.LT.360.0.AND.REMAIN.LT.360.0.OR. &
      OREMAIN.GT.420.0.AND.REMAIN.GT.420.0.AND. &
      OREMAIN.LT.440.0.AND.REMAIN.LT.440.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/500)*500.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.80.0.AND.REMAIN.LT.80.0.OR. &
      OREMAIN.GT.140.0.AND.REMAIN.GT.140.0.AND. &
      OREMAIN.LT.220.0.AND.REMAIN.LT.220.0.OR. &
      OREMAIN.GT.280.0.AND.REMAIN.GT.280.0.AND. &
      OREMAIN.LT.360.0.AND.REMAIN.LT.360.0.OR. &
      OREMAIN.GT.420.0.AND.REMAIN.GT.420.0.AND. &
      OREMAIN.LT.440.0.AND.REMAIN.LT.440.0) THEN
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
!     NOT LINE TYPE 7
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.8) THEN
!     200 UNIT DASH ON 280 UNIT CENTERS
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(REAL(MY_IY)-REAL(MY_OLDY))/(REAL(MY_IX)-REAL(MY_OLDX))
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/280)*280.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.200.0.AND.REMAIN.LT.200.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/280)*280.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.200.0.AND.REMAIN.LT.200.0) THEN
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
!     NOT LINE TYPE 8
                       END IF
!*****************************************************************************
!*****************************************************************************
      IF(MY_LINESTYLE.EQ.9) THEN
!     200 UNIT DASH 60 UNIT SPACE, 60 UNIT DASH 60 UNIT SPACE
!     ON 380 CENTERS
!
!     NOW DRAW THE LINE
      IF((MY_IX-MY_OLDX).NE.0.0) THEN
      SLOPE=(MY_IY-MY_OLDY)/(MY_IX-MY_OLDX)
                   IF(MY_IX.GE.MY_OLDX) XINC=INKER
                   IF(MY_IX.LT.MY_OLDX) XINC=-INKER
                   DO X=MY_OLDX,MY_IX,XINC
      Y=((SLOPE*(X-OX))+OY)
      DELTADIST= &
      (SQRT(((OX-X)**2) &
      +((OY-Y)**2)))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/380)*380.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.200.0.AND.REMAIN.LT.200.0.OR. &
      OREMAIN.GT.260.0.AND.REMAIN.GT.260.0.AND. &
      OREMAIN.LT.320.0.AND.REMAIN.LT.320.0) THEN
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
                       ELSE
!     LINE IS VERTICAL
                   IF(MY_IY.GE.MY_OLDY) YINC=INKER
                   IF(MY_IY.LT.MY_OLDY) YINC=-INKER
                   DO Y=MY_OLDY,MY_IY,YINC
      X=MY_IX
      DELTADIST= &
       (SQRT((OY-Y)**2))
      OLDDIST=MY_DISTANCE
      MY_DISTANCE=MY_DISTANCE+DELTADIST
      OREMAIN=REMAIN
      MYMOD=REAL(INT(MY_DISTANCE)/380)*380.0
      REMAIN=MY_DISTANCE-MYMOD
      IF(OREMAIN.LT.200.0.AND.REMAIN.LT.200.0.OR. &
      OREMAIN.GT.260.0.AND.REMAIN.GT.260.0.AND. &
      OREMAIN.LT.320.0.AND.REMAIN.LT.320.0) THEN
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
!     NOT LINE TYPE 9
                       END IF
!*****************************************************************************
!     UPDATE OLD VALUES
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

SUBROUTINE WDRAWOPTICALSYSTEM

  !subroutine my_draw_function(widget, my_cairo_context, width, height, gdata) bind(c)

       USE GLOBALS
       use handlers
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



    if (.not. c_associated(ld_window))  THEN
        call lens_draw_new(my_window)
    else
       call gtk_widget_queue_draw(cairo_drawing_area)
    end if

    !call DRAWDEBUG
      !CALL DRAWOPTICALSYSTEM(1,FIRST,ISKEY)


      !CALL WindowCloseChild(IDRAW1)
      !CALL WindowSelect(1)
 10   CONTINUE
                        RETURN
                        END

subroutine combo_tmp_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy


  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  character(len=50)                        :: choice
  character(len=512)             :: my_string
  type(gtktreeiter), target :: tree_iter

  type(c_ptr)  :: model

  type(c_ptr)  :: val, cstr, ival

  type(gvalue), target :: result, iresult


  type(integer)  :: tmpresult, ivalue

  ! For HL function
  !character(len=*) :: hl_string


  PRINT *, "In callback, pointer is ", gdata
  !tree_iter = c_null_ptr
  tmpresult = gtk_combo_box_get_active_iter(widget, c_loc(tree_iter))
  !tree_iter = gtk_combo_box_get_active_iter(gdata)

  !PRINT *, "tree_iter is ", tree_iter

  model = gtk_combo_box_get_model(widget)
  val = c_loc(result)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 1_c_int, val)

  !cstr = g_value_get_string(result)

  cstr = g_value_get_string(val)
  call convert_c_string(cstr, choice)

!        cstr = g_value_get_string(val)
!          call convert_c_string(cstr, svalue)
  PRINT *, "CHOICE is ", choice

  ! Get ING
  ival = c_loc(iresult)
  call gtk_tree_model_get_value(model, c_loc(tree_iter), 0_c_int, ival)
  ivalue = g_value_get_int(ival)

  PRINT *, "Integer Index is ", ivalue

  ! Use HL function
  !call hl_gtk_list_tree_get_gvalue(val, G_TYPE_STRING, svalue=hl_string)



  !if tree_iter is not None:
  !    model = combo.get_model()
!      row_id, name = model[tree_iter][:2]
  !     print("Selected: ID=%d, name=%s" % (row_id, name))
  ! else:
  !     entry = combo.get_child()
  !     print("Entered: %s" % entry.get_text())

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(gdata), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string
  PRINT *, "Callback works!"
end subroutine combo_tmp_callback


subroutine combo_fieldsymmetry_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use hl_gtk_zoa



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  integer :: id_selected
  !character(len=50)                        :: choice
  !character(len=512)             :: my_string

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(combo_plotorientation), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string

  id_selected = hl_zoa_combo_get_selected_list2_id(widget)


  call ld_settings % set_field_symmetry(id_selected)

  if (ld_settings%changed.eq.1) THEN
     ld_settings%changed = 0
     call lens_draw_replot()

  end if


end subroutine combo_fieldsymmetry_callback


subroutine combo_plotorientation_callback (widget, gdata ) bind(c)
  use, intrinsic :: iso_c_binding, only: c_double, c_f_pointer
  use gtk_sup, only: c_f_string_copy
  use hl_gtk_zoa



  type(c_ptr), value, intent(in) :: widget, gdata
  real(c_double)                 :: x, y
  integer :: id_selected
  !character(len=50)                        :: choice
  !character(len=512)             :: my_string

  ! Get the value selected by the user:
  !call c_f_string_copy( gtk_combo_box_text_get_active_text(combo_plotorientation), my_string)
  !read(my_string, *) choice

  !PRINT *, "CHOICE = ", my_string

  id_selected = hl_zoa_combo_get_selected_list2_id(widget)


  call ld_settings % set_plot_orientation(id_selected)

  if (ld_settings%changed.eq.1) THEN
     ld_settings%changed = 0
     call lens_draw_replot()

  end if


end subroutine combo_plotorientation_callback

  subroutine lens_draw_settings_dialog(box1)

    use hl_gtk_zoa

    type(c_ptr), intent(inout) :: box1

    type(c_ptr)     :: table, expander

    type(c_ptr)  :: label_plotorientation, label_fieldsymmetry

    type(c_ptr)  :: lstmp, gtype, cbox, cbox_field



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
    character(kind=c_char, len=20), dimension(4) :: vals_plotorientation
    integer(c_int), dimension(4) :: refs_plotorientation

    character(kind=c_char, len=40), dimension(2) :: vals_fieldsymmetry
    integer(c_int), dimension(2) :: refs_fieldsymmetry

    vals_plotorientation = [character(len=20) :: "YZ - Plane Layout", "XZ - Plane Layout", &
         &"XY - Plane Layout", "Orthographic"]

    refs_plotorientation = [ID_LENSDRAW_YZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XY_PLOT_ORIENTATION, 1407]

    call hl_gtk_combo_box_list2_new(cbox, refs_plotorientation, vals_plotorientation, 1700_c_int)
    call g_signal_connect (cbox, "changed"//c_null_char, c_funloc(combo_plotorientation_callback), c_null_ptr)


    vals_fieldsymmetry =  [character(len=40) :: "Plot Upper and Lower Fields of View", &
    & "Plot Upper Fields Only"]

    refs_fieldsymmetry = [ID_LENSDRAW_PLOT_WHOLE_FIELD, ID_LENSDRAW_PLOT_HALF_FIELD]

    call hl_gtk_combo_box_list2_new(cbox_field, refs_fieldsymmetry, vals_fieldsymmetry, 1700_c_int)
    call g_signal_connect (cbox_field, "changed"//c_null_char, c_funloc(combo_fieldsymmetry_callback), c_null_ptr)


    lstmp = gtk_list_store_newv(ncols, c_loc(coltypes))


    ! Initialize the GValues

    val = c_loc(vali)
    val = g_value_init(val, G_TYPE_INT)
    val = c_loc(valt)
    val = g_value_init(val, G_TYPE_STRING)

    ! Create the list store
    !store = gtk_list_store_newv(NUM_COLS, c_loc(ctypes))

    ! Append row 1 and add data
    call gtk_list_store_append(lstmp, c_loc(iter))
    call g_value_set_int(c_loc(vali), 51_c_int)
    call gtk_list_store_set_value(lstmp, c_loc(iter), 0_c_int, c_loc(vali))
    call g_value_set_static_string(c_loc(valt), "Heinz El-Mann"//c_null_char)
    call gtk_list_store_set_value(lstmp, c_loc(iter), 1_c_int, &
         & c_loc(valt))


   ! Append row 1 and add data
   call gtk_list_store_append(lstmp, c_loc(iter))
   call g_value_set_int(c_loc(vali), 101_c_int)
   call gtk_list_store_set_value(lstmp, c_loc(iter), 0_c_int, c_loc(vali))
   call g_value_set_static_string(c_loc(valt), "Shaquille O-NEAL"//c_null_char)
   call gtk_list_store_set_value(lstmp, c_loc(iter), 1_c_int, &
        & c_loc(valt))

  combo_tmp = gtk_combo_box_new_with_model_and_entry(lstmp)
  call g_signal_connect (combo_tmp, "changed"//c_null_char, c_funloc(combo_tmp_callback), c_null_ptr)
  call gtk_combo_box_set_entry_text_column(combo_tmp, 1_c_int)
  call gtk_combo_box_set_active(combo_tmp, 0_c_int)

  PRINT *, "Before callback, pointer is ", combo_tmp



    !list = hl_gtk_listn_new(renderers="combo")

    !gtype = gtk_list_store_get_type()
    !lstmp = gtk_list_store_newv(1_c_int, gtype)

    ! The combo box with predifined values of interesting Julia sets:
    label_plotorientation = gtk_label_new("Plot Orientation:"//c_null_char)
    label_fieldsymmetry   = gtk_label_new("Field Symmetry:"//c_null_char)


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
    call gtk_grid_attach(table, label_plotorientation, 0_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, cbox, 0_c_int, 1_c_int, 1_c_int,1_c_int)

    call gtk_grid_attach(table, label_fieldsymmetry, 1_c_int, 0_c_int, 1_c_int, 1_c_int)
    call gtk_grid_attach(table, cbox_field, 1_c_int, 1_c_int, 1_c_int,1_c_int)


    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, TRUE)

    ! We create a vertical box container:
    box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);
    call gtk_box_append(box1, expander)

    !call gtk_box_append(box1, ld_window)
    call gtk_widget_set_vexpand (box1, FALSE)



  end subroutine lens_draw_settings_dialog


  subroutine lens_draw_new(parent_window)


    type(c_ptr) :: parent_window

    type(c_ptr) :: content, junk, gfilter
    integer(kind=c_int) :: icreate, idir, action, lval
    integer(kind=c_int) :: i, idx0, idx1
    type(c_ptr)     :: my_drawing_area
    integer(c_int)  :: width, height

    type(c_ptr)     :: table, expander, box1

    ! Create a modal dialogue
    ld_window = gtk_window_new()
    !call gtk_window_set_modal(di, TRUE)
    !title = "Lens Draw Window"
    !if (present(title)) call gtk_window_set_title(dialog, title)
    call gtk_window_set_title(ld_window, "Lens Draw Window"//c_null_char)
    !if (present(wsize)) then
    !   call gtk_window_set_default_size(dialog, wsize(1),&
    !        & wsize(2))
    !else

    width = 1000
    height = 700
       call gtk_window_set_default_size(ld_window, width, height)
    !end if

    !if (present(parent)) then
       call gtk_window_set_transient_for(ld_window, parent_window)
       call gtk_window_set_destroy_with_parent(ld_window, TRUE)
    !end if


    call lens_draw_settings_dialog(box1)

    cairo_drawing_area = gtk_drawing_area_new()
    call gtk_drawing_area_set_content_width(cairo_drawing_area, width)
    call gtk_drawing_area_set_content_height(cairo_drawing_area, height)
    call gtk_drawing_area_set_draw_func(cairo_drawing_area, &
                   & c_funloc(DRAWOPTICALSYSTEM), c_null_ptr, c_null_funptr)
    !call gtk_drawing_area_set_draw_func(cairo_drawing_area, &
    !               & c_funloc(TESTCAIRO2), c_null_ptr, c_null_funptr)

    PRINT *, "FINISHED WITH DRAWOPTICALSYSTEM"
    call gtk_box_append(box1, cairo_drawing_area)
    !call gtk_window_set_child(ld_window, cairo_drawing_area)
    call gtk_window_set_child(ld_window, box1)
    call gtk_widget_set_vexpand (box1, FALSE)

    call g_signal_connect(ld_window, "destroy"//c_null_char, c_funloc(my_destroy), c_null_ptr)


    call gtk_window_set_mnemonics_visible (ld_window, TRUE)
    !call gtk_widget_queue_draw(my_drawing_area)
    call gtk_widget_show(ld_window)


    PRINT *, "SHOULD SEE GRAPHICS NOW!"
end subroutine lens_draw_new


subroutine lens_draw_replot()


  character(len=40) :: command, qual_word
  character(len=100) :: ftext

  command = "VIECO"


  ! Original logic in LENSED.INC
  select case (ld_settings%field_symmetery)
  case (ID_LENSDRAW_PLOT_HALF_FIELD)
       ftext = 'VIESYM OFF'
       CALL PROCESKDP(ftext)
  case (ID_LENSDRAW_PLOT_WHOLE_FIELD)
       ftext = 'VIESYM ON'
       CALL PROCESKDP(ftext)
  end select

  select case (ld_settings%plot_orientation)

  case (ID_LENSDRAW_YZ_PLOT_ORIENTATION)
       qual_word = "YZ"
  case (ID_LENSDRAW_XZ_PLOT_ORIENTATION)
      qual_word = "XZ"
  case (ID_LENSDRAW_XY_PLOT_ORIENTATION)
      qual_word = "XY"
  case DEFAULT
      qual_word = " "
  end select

    ftext = trim(command)//" "//trim(qual_word)

    PRINT *, "Command is ", ftext
    CALL PROCESKDP(ftext)

end subroutine lens_draw_replot

end module
