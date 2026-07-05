!       SEVENTH FILE OF PLOT/CAD ROUTINES

! SUB ROT4.FOR
SUBROUTINE ROT4(JJ,CLPDAT,M1,M2,M3,M4,M5)
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
!     AND PLOT VIEW FOR THE PLOT COBS COMMAND
!
!     IF SEVERAL SURFACES ARE ASKED FOR, ROTATION POINTS ARE
!     CALCULATED USING GLOBAL VERTEX DATA. FOR ONE SURFACE,
!     THE ROTATION POINT IS SET AT THE CENTER ON THE X AND Y
!     SURFACE COBS.
!
   real(real64) X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4 &
   &,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ,XA,XB,YA,YB,ZA,ZB
!
   INTEGER JJ,I,M1,M2,M3,M4,M5
!
!
   REAL CLPDAT
   DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
!
   STASUR=INT(W1)
   STPSUR=INT(W2)
   IF(STASUR.NE.STPSUR) THEN
      IF(.NOT.ROTSET) THEN
         DO I=STASUR,STPSUR
            X=VERTEX(1,I)
            Y=VERTEX(2,I)
            Z=VERTEX(3,I)
            IF(I.EQ.STASUR) THEN
               XMINIX=X
               XMAXIX=X
               YMINIY=Y
               YMAXIY=Y
               ZMINIZ=Z
               ZMAXIZ=Z
            ELSE
            END IF
            IF(X.LE.XMINIX) XMINIX=X
            IF(X.GT.XMAXIX) XMAXIX=X
            IF(Y.LE.YMINIY) YMINIY=Y
            IF(Y.GT.YMAXIY) YMAXIY=Y
            IF(Z.LE.ZMINIZ) ZMINIZ=Z
            IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
         END DO
         XROT=(XMAXIX+XMINIX)/2.0D0
         YROT=(YMAXIY+YMINIY)/2.0D0
         ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
         ROTSET=.TRUE.
      ELSE
!     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
      END IF
   ELSE
!     STASUR SAME AS STPSUR, KEY OFF COBS END POINTS
      IF(.NOT.ROTSET) THEN
         I=STASUR
!
         X1=CLPDAT(0,1,I,JJ)
         Y1=CLPDAT(0,2,I,JJ)
         Z1=CLPDAT(0,3,I,JJ)
         X2=CLPDAT(180,1,I,JJ)
         Y2=CLPDAT(180,2,I,JJ)
         Z2=CLPDAT(180,3,I,JJ)
         XA=(X2+X1)/2.0D0
         YA=(Y2+Y1)/2.0D0
         ZA=(Z2+Z1)/2.0D0
         X3=(CLPDAT(22,1,I,JJ)+CLPDAT(90,1,I,JJ))/2.0D0
         Y3=(CLPDAT(22,2,I,JJ)+CLPDAT(90,2,I,JJ))/2.0D0
         Z3=(CLPDAT(22,3,I,JJ)+CLPDAT(90,3,I,JJ))/2.0D0
         X4=(CLPDAT(62,1,I,JJ)+CLPDAT(270,1,I,JJ))/2.0D0
         Y4=(CLPDAT(62,2,I,JJ)+CLPDAT(270,2,I,JJ))/2.0D0
         Z4=(CLPDAT(62,3,I,JJ)+CLPDAT(270,3,I,JJ))/2.0D0
         XB=(X4+X3)/2.0D0
         YB=(Y4+Y3)/2.0D0
         ZB=(Z4+Z3)/2.0D0
         XROT=(XB+XA)/2.0D0
         YROT=(YB+YA)/2.0D0
         ZROT=(ZB+ZA)/2.0D0
!
         ROTSET=.TRUE.
      ELSE
!     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
      END IF
   END IF
   RETURN
END
! SUB ROT3.FOR
SUBROUTINE ROT3(JJ,CLPDAT,M1,M2,M3,M4,M5)
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
!     AND PLOT VIEW FOR THE PLOT CLAP COMMAND
!
!     IF SEVERAL SURFACES ARE ASKED FOR, ROTATION POINTS ARE
!     CALCULATED USING GLOBAL VERTEX DATA. FOR ONE SURFACE,
!     THE ROTATION POINT IS SET AT THE CENTER ON THE X AND Y
!     SURFACE CLAPS.
!
   real(real64) X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4 &
   &,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ,XA,XB,YA,YB,ZA,ZB
!
   INTEGER I,JJ,M1,M2,M3,M4,M5
!
   REAL CLPDAT
   DIMENSION CLPDAT(M1:M2,M3,M1:M4,M5)
!
!
   STASUR=INT(W1)
   STPSUR=INT(W2)
   IF(STASUR.NE.STPSUR) THEN
      IF(.NOT.ROTSET) THEN
         DO I=STASUR,STPSUR
            X=VERTEX(1,I)
            Y=VERTEX(2,I)
            Z=VERTEX(3,I)
            IF(I.EQ.STASUR) THEN
               XMINIX=X
               XMAXIX=X
               YMINIY=Y
               YMAXIY=Y
               ZMINIZ=Z
               ZMAXIZ=Z
            ELSE
            END IF
            IF(X.LE.XMINIX) XMINIX=X
            IF(X.GT.XMAXIX) XMAXIX=X
            IF(Y.LE.YMINIY) YMINIY=Y
            IF(Y.GT.YMAXIY) YMAXIY=Y
            IF(Z.LE.ZMINIZ) ZMINIZ=Z
            IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
         END DO
         XROT=(XMAXIX+XMINIX)/2.0D0
         YROT=(YMAXIY+YMINIY)/2.0D0
         ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
         ROTSET=.TRUE.
      ELSE
!     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
      END IF
   ELSE
!     STASUR SAME AS STPSUR, KEY OFF CLAP END POINTS
      IF(.NOT.ROTSET) THEN
         I=STASUR
!
         X1=CLPDAT(0,1,I,JJ)
         Y1=CLPDAT(0,2,I,JJ)
         Z1=CLPDAT(0,3,I,JJ)
         X2=CLPDAT(180,1,I,JJ)
         Y2=CLPDAT(180,2,I,JJ)
         Z2=CLPDAT(180,3,I,JJ)
         XA=(X2+X1)/2.0D0
         YA=(Y2+Y1)/2.0D0
         ZA=(Z2+Z1)/2.0D0
         X3=(CLPDAT(22,1,I,JJ)+CLPDAT(90,1,I,JJ))/2.0D0
         Y3=(CLPDAT(22,2,I,JJ)+CLPDAT(90,2,I,JJ))/2.0D0
         Z3=(CLPDAT(22,3,I,JJ)+CLPDAT(90,3,I,JJ))/2.0D0
         X4=(CLPDAT(62,1,I,JJ)+CLPDAT(270,1,I,JJ))/2.0D0
         Y4=(CLPDAT(62,2,I,JJ)+CLPDAT(270,2,I,JJ))/2.0D0
         Z4=(CLPDAT(62,3,I,JJ)+CLPDAT(270,3,I,JJ))/2.0D0
         XB=(X4+X3)/2.0D0
         YB=(Y4+Y3)/2.0D0
         ZB=(Z4+Z3)/2.0D0
         XROT=(XB+XA)/2.0D0
         YROT=(YB+YA)/2.0D0
         ZROT=(ZB+ZA)/2.0D0
!
         ROTSET=.TRUE.
      ELSE
!     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
      END IF
   END IF
   RETURN
END
! SUB RIMS.FOR
SUBROUTINE RIMS
!
   use DATLEN
   use mod_system, only: sys_wl_ref
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS PROGRAM CONTROLS THE "FANS" COMMAND
!
   real(real64) F1X,F1Y,F2X,F2Y,F3X,F3Y
!
   COMMON/FANFOB/F1X,F1Y,F2X,F2Y,F3X,F3Y
!
   CHARACTER RIMWQ*8
!
   real(real64) RIMW1
!
   INTEGER DFLAG
!
   LOGICAL FANEXT,RIM
!
   COMMON/RINSHT/RIM
!
   COMMON/FANEXI/FANEXT
!
!
   FANEXT=.FALSE.
   RIM=.TRUE.
   SAVE_KDP(1)=SAVEINPT(1)
!
   CALL PLTRST
!
!     W1 IS FOR SSI
!
!     QUALIFIER WORDS:
!         YFAN
!         XFAN
!         NFAN
!         PFAN
!         XYFAN
!         YXFAN
!         YOPD
!         XOPD
!         NOPD
!         POPD
!         XYOPD
!         YLA
!         XLA
!         NLA
!         PLA
!         XYLA
!         YXLA
!         YCD
!         XCD
!         NCD
!         PCD
!         XYCD
!         YXCD
!
   IF(SQ.EQ.0) THEN
      SQ=1
      WQ='XYFAN   '
   END IF
   IF(WQ.NE.'YFAN'.AND.WQ.NE.'XFAN'.AND.WQ.NE.'YOPD'.AND.&
   &WQ.NE.'XOPD'.AND.WQ.NE.'XCD'.AND.WQ.NE.'YCD'.AND.&
   &WQ.NE.'XLA'.AND.WQ.NE.'YLA'.AND.&
   &WQ.NE.'NFAN'.AND.WQ.NE.'PFAN'.AND.WQ.NE.'NOPD'.AND.&
   &WQ.NE.'POPD'.AND.WQ.NE.'NCD'.AND.WQ.NE.'PCD'.AND.&
   &WQ.NE.'NLA'.AND.WQ.NE.'PLA'.AND.&
   &WQ.NE.'XYFAN'.AND.WQ.NE.'YXFAN'.AND.&
   &WQ.NE.'XYOPD'.AND.&
   &WQ.NE.'XYCD'.AND.&
   &WQ.NE.'YXCD'.AND.&
   &WQ.NE.'XYLA'.AND.WQ.NE.'YXLA') THEN
      OUTLYNE= 'FOR "FANS"'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE ONLY VALID QUALIFIER WORDS ARE:'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL(&
      & '"XFAN"  - FOR X-RAY,  "YFAN"  - FOR Y-RAY  (FANS)'//'\n'//&
      & '"XYFAN" - FOR XY-RAY, "YXFAN" - FOR YX-RAY (FANS)'//'\n'//&
      & '"NFAN"  - FOR N-RAY,  "PFAN"  - FOR P-RAY  (FANS)'//'\n'//&
      & '"XOPD"  - FOR X-OPD,  "YOPD"  - FOR Y-OPD  (FANS)'//'\n'//&
      & '"XYOPD" - FOR XY-OPD                       (FANS)'//'\n'//&
      & '"NOPD"  - FOR N-OPD,  "POPD"  - FOR P-OPD  (FANS)'//'\n'//&
      & '"XCD"   - FOR X-CD,   "YCD"   - FOR Y-CD   (FANS)'//'\n'//&
      & '"XYCD"  - FOR XY-CD,  "YXCD"  - FOR YX-CD  (FANS)'//'\n'//&
      & '"NCD"   - FOR N-CD,   "PCD"   - FOR P-CD   (FANS)'//'\n'//&
      & '"XLA"   - FOR X-LA,   "YLA"   - FOR Y-LA   (FANS)'//'\n'//&
      & '"XYLA"  - FOR XY-LA,  "YXLA"  - FOR YX-LA  (FANS)'//'\n'//&
      & '"NLA"   - FOR N-LA,   "PLA"   - FOR P-LA   (FANS)'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(is_command_query()) THEN
      OUTLYNE= '"FANS" PERFOMS AUTOMATED FAN PLOTTING'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FANS" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.0.AND.W1.LE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE "SSI" VALUE MUST BE GREATER THAN 0.0'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!     DO A PLOT NEW
   FANEXT=.FALSE.
   RIM=.TRUE.
   SAVE_KDP(1)=SAVEINPT(1)
!
   CALL PLTRST
!
!
!
   FANEXT=.FALSE.
   RIM=.TRUE.
   DEVTYP=1
   LOOKY=0.0D0
   LOOKX=-1.0D0
   LOOKZ=0.0D0
   OUTLYNE='RAY FAN DATA BEING GENERATED'
   CALL SHOWIT(1)
   OUTLYNE='PLEASE WAIT...'
   CALL SHOWIT(1)
   CALL PLTDEV
   IF(DF1.EQ.1) SSI=0.0D0
   IF(DF1.EQ.1) SSIFLG=.TRUE.
   IF(DF1.EQ.0) SSI=W1/2.0D0
   IF(DF1.EQ.0) SSIFLG=.FALSE.
   IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
   IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
!     SSI DONE
   RIMWQ=WQ
   RIMW1=W1
   RIM=.TRUE.
   GRASET=.TRUE.
!
!     SET UP 3 FANS
!
   FANNUM=3
   YFOB1=F1Y
   YFOB2=F2Y
   YFOB3=F3Y
   XFOB1=F1X
   XFOB2=F2X
   XFOB3=F3X
!
!     THREE FANS SET UP
!
!     NOW SET REFERENCE WAVELENGTH
!
   REFWV=INT(sys_wl_ref())
   REST_KDP(1)=RESTINPT(1)
!
!     SET TYPES
   SAVE_KDP(1)=SAVEINPT(1)
   IF(RIMWQ.EQ.'XFAN') INPUT='PLTXFAN'
   IF(RIMWQ.EQ.'YFAN') INPUT='PLTYFAN'
   IF(RIMWQ.EQ.'NFAN') INPUT='PLTNFAN'
   IF(RIMWQ.EQ.'PFAN') INPUT='PLTPFAN'
   IF(RIMWQ.EQ.'XYFAN') INPUT='PLTXYFAN'
   IF(RIMWQ.EQ.'YXFAN') INPUT='PLTYXFAN'
   IF(RIMWQ.EQ.'XOPD') INPUT='PLTXFAN OPD'
   IF(RIMWQ.EQ.'YOPD') INPUT='PLTYFAN OPD'
   IF(RIMWQ.EQ.'NOPD') INPUT='PLTNFAN OPD'
   IF(RIMWQ.EQ.'POPD') INPUT='PLTPFAN OPD'
   IF(RIMWQ.EQ.'XYOPD') INPUT='PLTXYFAN OPD'
   IF(RIMWQ.EQ.'XCD') INPUT='PLTXFAN CD'
   IF(RIMWQ.EQ.'YCD') INPUT='PLTYFAN CD'
   IF(RIMWQ.EQ.'NCD') INPUT='PLTNFAN CD'
   IF(RIMWQ.EQ.'PCD') INPUT='PLTPFAN CD'
   IF(RIMWQ.EQ.'XYCD') INPUT='PLTXYFAN CD'
   IF(RIMWQ.EQ.'YXCD') INPUT='PLTYXFAN CD'
   IF(RIMWQ.EQ.'XLA') INPUT='PLTXFAN LA'
   IF(RIMWQ.EQ.'YLA') INPUT='PLTYFAN LA'
   IF(RIMWQ.EQ.'NLA') INPUT='PLTNFAN LA'
   IF(RIMWQ.EQ.'PLA') INPUT='PLTPFAN LA'
   IF(RIMWQ.EQ.'XYLA') INPUT='PLTXYFAN LA'
   IF(RIMWQ.EQ.'YXLA') INPUT='PLTYXFAN LA'
   CALL PROCES
   REST_KDP(1)=RESTINPT(1)
!
!     DO A PLOTFANS GO
   FANEXT=.FALSE.
   RIM=.TRUE.
   MSG=.FALSE.
      CALL KDP_EXEC('PLOTFANS GO')
   MSG=.TRUE.
!
!     DO A DRAW
!
   IF(DFLAG.EQ.0) THEN
            CALL KDP_EXEC('DRAW')
   END IF

   RETURN
END
! SUB CHSIZE.FOR
SUBROUTINE CHSIZE
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DOES THE PLOT CHNOTE, PLOT CHSYM AND
!       AND PLOT CHLAB COMMANDS AT THE CMD LEVEL
!
!
   IF(WQ.EQ.'CHNOTE') THEN
!       CHECK SYNTAX
      IF(SST.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHNOTE" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHNOTE" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,100)NTSIZ
         CALL SHOWIT(1)
         WRITE(OUTLYNE,200)NTANG
         CALL SHOWIT(1)
         RETURN
      END IF
!       ASSIGN VALUES
      IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHNOTE" REQUIRES SOME EXPLICIT NUMERIC'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'WORD #1 OR #2 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DF1.EQ.1) W1=1.0D0
      IF(DF2.EQ.1) W2=0.0D0
      IF(INT(W1).LT.1.OR.INT(W1).GT.9) THEN
         OUTLYNE=&
         &'NOTE SIZE VALUES CAN ONLY BE 1,2,3,4,5,6,7,8 AND 9'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DABS(W2).GT.360.0D0) THEN
         OUTLYNE=&
         &'NOTE ANGLES CAN NOT BE GREATER THAN 360.0 DEGREES'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      NTSIZ=INT(W1)
      NTANG=INT(W2)
   ELSE
!       NOT PLOT CHNOTE
   END IF
!
!
   IF(WQ.EQ.'CHSYM') THEN
!       CHECK SYNTAX
      IF(SST.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHSYM" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHSYM" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,300)SYMSIZ
         CALL SHOWIT(1)
         WRITE(OUTLYNE,400)SYMANG
         CALL SHOWIT(1)
         RETURN
      END IF
!       ASSIGN VALUES
      IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHSYM" REQUIRES SOME EXPLICIT NUMERIC'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'WORD #1 OR #2 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DF1.EQ.1) W1=1.0D0
      IF(DF2.EQ.1) W2=0.0D0
      IF(INT(W1).LT.1.OR.INT(W1).GT.9) THEN
         OUTLYNE=&
         &'SYMBOL SIZE VALUES CAN ONLY BE 1,2,3,4,5,6,7,8 AND 9'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DABS(W2).GT.360.0D0) THEN
         OUTLYNE=&
         &'SYMBOL ANGLES CAN NOT BE GREATER THAN 360.0 DEGREES'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      SYMSIZ=INT(W1)
      SYMANG=INT(W2)
   END IF
!
   IF(WQ.EQ.'CHLAB') THEN
!       CHECK SYNTAX
      IF(SST.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHLAB" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHLAB" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,500)LABSIZ
         CALL SHOWIT(1)
         WRITE(OUTLYNE,600)LABANG
         CALL SHOWIT(1)
         RETURN
      END IF
!       ASSIGN VALUES
      IF(DF1.EQ.1.AND.DF2.EQ.1) THEN
         OUTLYNE=&
         &'"PLOT CHLAB" REQUIRES SOME EXPLICIT NUMERIC'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'WORD #1 OR #2 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DF1.EQ.1) W1=1.0D0
      IF(DF2.EQ.1) W2=0.0D0
      IF(INT(W1).LT.1.OR.INT(W1).GT.9) THEN
         OUTLYNE=&
         &'LABEL SIZE VALUES CAN ONLY BE 1,2,3,4,5,6,7,8 AND 9'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(DABS(W2).GT.360.0D0) THEN
         OUTLYNE=&
         &'LABEL ANGLES CAN NOT BE GREATER THAN 360.0 DEGREES'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      LABSIZ=INT(W1)
      LABANG=INT(W2)
   END IF
!
100 FORMAT('CURRENT NOTE SIZE  IS = ',I1)
200 FORMAT('CURRENT NOTE ANGLE IS = ',I4)
300 FORMAT('CURRENT SYMBOL SIZE  IS = ',I1)
400 FORMAT('CURRENT SYMBOL ANGLE IS = ',I4)
500 FORMAT('CURRENT LABEL SIZE  IS = ',I1)
600 FORMAT('CURRENT LABEL ANGLE IS = ',I4)
   RETURN
END
! SUB CAO1.FOR
!
!     THIS ROUTINE IS CALLED BY PLTCLP
!
SUBROUTINE CAO1(X,Y,ANGLE,I,AN2,JJ,XID,YID,IIRUN,CLPTYPE,ZDELZ &
&,MULTX,MULTY,GAMGAM)
!
!     THIS SUBROUTINE CALCULATES THE X AND Y CLAP LIMIT VALUES
!     FOR SURFACE III WHICH LIE ON A LINE WHICH EXTENDS FROM THE
!     SURFACE VERTEX OUT TO THE CLAP EDGE. "ANGLE" IS MEASURED
!     COUNTER-CLOCKWISE FROM THE X AXIS TO THE Y AXIS OF THE LOCAL
!     SURFACE VERTEX COORDINATE SYSTEM. 0 DEGREES POINTS IN THE +X
!     DIRECTION. "ANGLE" IS PASSED TO THIS ROUTINE IN RADIAN MEASURE.
!     JJ IS ALWAYS 1 EXCEPT IF THE INNER CIRCULAR BOUNDRY OF A
!     TYPE 18 GRAZING INCIDENCE SURFACE MUST BE DRAWN.
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER JJ,CAFLG,I,IIRUN,J,CLPTYPE
!
   LOGICAL DOIT,ISAIR,ISAIR2,POSDIR,OLDIN,NEWIN,ISITIN,ISITINI
!
   EXTERNAL ISAIR,ISAIR2,ISITIN,ISITINI
!
   real(real64) X,Y,GAM,THETA1,THETA2,THETA3,THETA4,XID,YID,GAMGAM &
   &,A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4,DA,DB,ZDELZ &
   &,YCOR1,YCOR2,YCOR3,YCOR4,A1TEST,A2TEST,QTEST,Q,JK_AA,JK_BB &
   &,XA,YA,XB,YB,M,RADIUS,CBX,CBY,SGNB,CEE,ZEE,KAPPA,RHO2,RHO &
   &,JK_CC,CLPLCX(0:499),CLPLCY(0:499),XOLDX,YOLDY,MULTX,MULTY
!
   COMMON/CLPLOC/CLPLCX,CLPLCY
!
!
   ZDELZ=0.0D0
!
!       SET FLAG CAFLG
   CAFLG=INT(ALENS(9,I))
   GAM=0.0D0
!
!       CAFLG HAS BEEN INITIALIZED
!
!       CAFLG=0 NO CLAP, USE PARAXIAL DATA
   IF(ALENS(34,I).NE.18.0D0) THEN
!     NOT TYPE 18
      IF(CAFLG.EQ.0) THEN
!
!     COORDINATES FOR THE SPECIFIED ANGLE ARE TAKEN FROM THE YZ
!     PLANE PARAXIAL DATA. CLAP TILT OR DECENTRATION APPLIES
         RAD=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
         X=(DCOS(ANGLE)*RAD)+CLPLCX(I)
         Y=(DSIN(ANGLE)*RAD)+CLPLCY(I)
         XID=(DCOS(ANGLE)*RAD)+CLPLCX(I)
         YID=(DSIN(ANGLE)*RAD)+CLPLCY(I)
      ELSE
!       CAFLG NOT 0
      END IF
!
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1) THEN
         IF(IIRUN.EQ.1) RAD=DABS(ALENS(10,I))
         IF(IIRUN.EQ.2) RAD=DABS(ALENS(11,I))
         IF(CLPTYPE.EQ.1) THEN
            X=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
            Y=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
            XID=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
            YID=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
         ELSE
            X=(DCOS(ANGLE)*DABS(ALENS(11,I)))+ALENS(13,I)+MULTX
            Y=(DSIN(ANGLE)*DABS(ALENS(11,I)))+ALENS(12,I)+MULTY
            XID=(DCOS(ANGLE)*DABS(ALENS(11,I)))+ALENS(13,I)+MULTX
            YID=(DSIN(ANGLE)*DABS(ALENS(11,I)))+ALENS(12,I)+MULTY
         END IF
      ELSE
!       CAFLG NOT 1
      END IF
!       CAFLG=5, CLAP POLY
      IF(CAFLG.EQ.5) THEN
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(ANGLE)
            Y=(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(ANGLE)
            NEWIN=ISITIN(X,Y,I)
            X=X+ALENS(13,I)+MULTX
            Y=Y+ALENS(12,I)+MULTY
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               XID=X
               YID=Y
               GO TO 10
            ELSE
            END IF
         END DO
      ELSE
!     CAFLG NOT 5
      END IF
!       CAFLG=6, CLAP IPOLY
      IF(CAFLG.EQ.6) THEN
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=(DBLE(J)*0.01D0*ALENS(14,I))*DCOS(ANGLE)
            Y=(DBLE(J)*0.01D0*ALENS(14,I))*DSIN(ANGLE)
            NEWIN=ISITINI(X,Y,I)
            X=X+ALENS(13,I)+MULTX
            Y=Y+ALENS(12,I)+MULTY
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               XID=X
               YID=Y
               GO TO 10
            ELSE
            END IF
         END DO
      ELSE
!     CAFLG NOT 6
      END IF
10    CONTINUE
!       CAFLG=2, CLAP RECT
      IF(CAFLG.EQ.2) THEN
!     CORNER 1 HAS COORDINATES
         XCOR1=+DABS(ALENS(11,I))
         YCOR1=+DABS(ALENS(10,I))
         IF(DABS(YCOR1).LE.1.0D-15.AND.DABS(XCOR1).LE.1.0D-15) THEN
            THETA1=0.0D0
         ELSE
            THETA1=DATAN2(YCOR1,XCOR1)
         END IF
         IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
!     CORNER 2 HAS COORDINATES
         XCOR2=-DABS(ALENS(11,I))
         YCOR2=+DABS(ALENS(10,I))
         IF(DABS(YCOR2).LE.1.0D-15.AND.DABS(XCOR2).LE.1.0D-15) THEN
            THETA2=0.0D0
         ELSE
            THETA2=DATAN2(YCOR2,XCOR2)
         END IF
         IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
!     CORNER 3 HAS COORDINATES
         XCOR3=-DABS(ALENS(11,I))
         YCOR3=-DABS(ALENS(10,I))
         IF(DABS(YCOR3).LE.1.0D-15.AND.DABS(XCOR3).LE.1.0D-15) THEN
            THETA3=0.0D0
         ELSE
            THETA3=DATAN2(YCOR3,XCOR3)
         END IF
         IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
!     CORNER 4 HAS COORDINATES
         XCOR4=+DABS(ALENS(11,I))
         YCOR4=-DABS(ALENS(10,I))
         IF(DABS(YCOR4).LE.1.0D-15.AND.DABS(XCOR4).LE.1.0D-15) THEN
            THETA4=0.0D0
         ELSE
            THETA4=DATAN2(YCOR4,XCOR4)
         END IF
         IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
!
         IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
            Y=DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
            X=-DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=-DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
            Y=-DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=-DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF

         IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
            X=XCOR1
            Y=YCOR1
            XID=XCOR1
            YID=YCOR1
         ELSE
!     POINT NOT AT CORNER1
         END IF
         IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
            X=XCOR2
            Y=YCOR2
            XID=XCOR2
            YID=YCOR2
         ELSE
!     POINT NOT AT CORNER2
         END IF
         IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
            X=XCOR3
            Y=YCOR3
            XID=XCOR3
            YID=YCOR3
         ELSE
!     POINT NOT AT CORNER3
         END IF
         IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
            X=XCOR4
            Y=YCOR4
            XID=XCOR4
            YID=YCOR4
         ELSE
!     POINT NOT AT CORNER4
         END IF
!
!     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
         GAM=(PII/180.0D0)*(ALENS(15,I)+GAMGAM)
         XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
         YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
         X=XR+ALENS(13,I)+MULTX
         Y=YR+ALENS(12,I)+MULTY
         XID=XR+ALENS(13,I)+MULTX
         YID=YR+ALENS(12,I)+MULTY
!
      ELSE
!     CAFLG NOT 2
      END IF
!
!       CAFLG=3, ELLIPTICAL CLAP
      IF(CAFLG.EQ.3) THEN
!       X-SEMI-MAJOR AXIS IS ALENS(11,I)
!       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
         A=(ALENS(11,I))
         B=(ALENS(10,I))
         RAD=((A**2)*(B**2))/&
         &(((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
         RAD=DSQRT(RAD)
         X=RAD*DCOS(ANGLE)
         Y=RAD*DSIN(ANGLE)
         XID=RAD*DCOS(ANGLE)
         YID=RAD*DSIN(ANGLE)
!
!     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
         GAM=(PII/180.0D0)*(ALENS(15,I)+GAMGAM)
         XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
         YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
         X=XR+ALENS(13,I)+MULTX
         Y=YR+ALENS(12,I)+MULTY
         XID=XR+ALENS(13,I)+MULTX
         YID=YR+ALENS(12,I)+MULTY
!
      ELSE
!     CAFLG NOT 3
      END IF
!       CAFLG=4, CLAP RCTK
      IF(CAFLG.EQ.4) THEN
!     CORNER 1 HAS COORDINATES
         XCOR1=+DABS(ALENS(11,I))
         YCOR1=+DABS(ALENS(10,I))
         IF(DABS(YCOR1).LE.1.0D-15.AND.DABS(XCOR1).LE.1.0D-15) THEN
            THETA1=0.0D0
         ELSE
            THETA1=DATAN2(YCOR1,XCOR1)
         END IF
         IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
!     CORNER 2 HAS COORDINATES
         XCOR2=-DABS(ALENS(11,I))
         YCOR2=+DABS(ALENS(10,I))
         IF(DABS(YCOR2).LE.1.0D-15.AND.DABS(XCOR2).LE.1.0D-15) THEN
            THETA2=0.0D0
         ELSE
            THETA2=DATAN2(YCOR2,XCOR2)
         END IF
         IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
!     CORNER 3 HAS COORDINATES
         XCOR3=-DABS(ALENS(11,I))
         YCOR3=-DABS(ALENS(10,I))
         IF(DABS(YCOR3).LE.1.0D-15.AND.DABS(XCOR3).LE.1.0D-15) THEN
            THETA3=0.0D0
         ELSE
            THETA3=DATAN2(YCOR3,XCOR3)
         END IF
         IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
!     CORNER 4 HAS COORDINATES
         XCOR4=+DABS(ALENS(11,I))
         YCOR4=-DABS(ALENS(10,I))
         IF(DABS(YCOR4).LE.1.0D-15.AND.DABS(XCOR4).LE.1.0D-15) THEN
            THETA4=0.0D0
         ELSE
            THETA4=DATAN2(YCOR4,XCOR4)
         END IF
         IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
!
         IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
            Y=DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
            X=-DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=-DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
            Y=-DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=-DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
!
!     HERE, THE RACETRACK HAS BEEN DRAWN AS A RECTANGLE. NOW
!     DOES THIS POINT NEED TO BE MODIFIED DUE TO A RACETRACK
!     CORNER. THIS MODIFICATION IS DONE FOR:
!
!     ANGLES FROM 0 TO 90
!     THEN FROM 90 TO 180
!     THEN FROM 180 TO 270
!     THEN FROM 270 TO 360
         RADIUS=DABS(ALENS(14,I))
!
!     THESE ARE FOUR AREAS OF ADJUSTMENT
!
!     AREA 1
!
         IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.(PII/2.0D0)) THEN
            IF(DABS(YCOR1-RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR1).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2((YCOR1-RADIUS),XCOR1)
            END IF
            IF(DABS(YCOR1).LE.1.0D-15.AND.&
            &DABS(XCOR1-RADIUS).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2(YCOR1,(XCOR1-RADIUS))
            END IF
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR1-RADIUS
               CBY=YCOR1-RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     AREA 2
!
         IF(ANGLE.GE.(PII/2.0D0).AND.ANGLE.LE.PII) THEN
            IF(DABS(YCOR2).LE.1.0D-15.AND.&
            &DABS(XCOR2+RADIUS).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2(YCOR2,(XCOR2+RADIUS))
            END IF
            IF(DABS(YCOR2-RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR2).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2((YCOR2-RADIUS),XCOR2)
            END IF
            IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR2+RADIUS
               CBY=YCOR2-RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     AREA 3
!
         IF(ANGLE.GE.PII.AND.ANGLE.LE.((3.0D0*PII)/2.0D0)) THEN
            IF(DABS(YCOR3+RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR3).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2((YCOR3+RADIUS),XCOR3)
            END IF
            IF(DABS(YCOR3).LE.1.0D-15.AND.&
            &DABS(XCOR3+RADIUS).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2(YCOR3,(XCOR3+RADIUS))
            END IF
            IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR3+RADIUS
               CBY=YCOR3+RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     AREA 4
!
         IF(ANGLE.GE.((3.0D0*PII)/2.0D0)) THEN
            IF(DABS(YCOR4).LE.1.0D-15.AND.&
            &DABS(XCOR4-RADIUS).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2(YCOR4,(XCOR4-RADIUS))
            END IF
            IF(DABS(YCOR4+RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR4).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2((YCOR4+RADIUS),XCOR4)
            END IF
            IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(A2TEST.LE.PII)   A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR4-RADIUS
               CBY=YCOR4+RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
         GAM=(PII/180.0D0)*(ALENS(15,I)+GAMGAM)
         XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
         YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
         X=XR+ALENS(13,I)+MULTX
         Y=YR+ALENS(12,I)+MULTY
         XID=XR+ALENS(13,I)+MULTX
         YID=YR+ALENS(12,I)+MULTY
!
      ELSE
!     CAFLG NOT 4
      END IF
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1) THEN
!
         IF(IIRUN.EQ.1) RAD=DABS(ALENS(10,I))
         IF(IIRUN.EQ.2) RAD=DABS(ALENS(11,I))
!
!     CLAP DEC MUST BE 0
         IF((ALENS(12,I)+MULTX).EQ.0.0D0.AND.&
         &(ALENS(13,I)+MULTY).EQ.0.0D0) THEN
            IF(ALENS(34,I).NE.18.0D0) THEN
!
!     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
!     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
!
!     IF SURFACE I IS NOT AIR AND I+1 IS AIR
!     THEN IF CV>0 THERE MAY BE A FLAT TO DO
!
               DOIT=.FALSE.
               DOIT=ISAIR(I,POSDIR)
               IF(DOIT) THEN
                  IF(ALENS(1,I).GT.0.0D0.AND.POSDIR.OR.&
                  &ALENS(1,I).LT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                     RAD=DABS(ALENS(11,I))
                     IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                     IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                  END IF
               END IF
!
!     IF SURFACE I IS AIR AIR AND I+1 IS NOT AIR
!     THEN IF CV<0 THERE MAY BE A FLAT TO DO
!
               DOIT=.FALSE.
               DOIT=ISAIR2(I,POSDIR)
               IF(DOIT) THEN
                  IF(ALENS(1,I).LT.0.0D0.AND.POSDIR.OR.&
                  &ALENS(1,I).GT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                     RAD=DABS(ALENS(11,I))
                     IF(ALENS(1,I).GT.0.0D0) ZDELZ=-DABS(ALENS(14,I))
                     IF(ALENS(1,I).LT.0.0D0) ZDELZ=DABS(ALENS(14,I))
                  END IF
               END IF
            END IF
            XID=(DCOS(ANGLE)*RAD)
            YID=(DSIN(ANGLE)*RAD)
         ELSE
!     DEC CLAP, NO FLAT CALC
         END IF
      ELSE
!       CAFLG NOT 1
      END IF
   ELSE
!     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
!     CALCULATE THE RADIUS OF THE CLAP
!     CEE IS THE CURVATURE CV (ALENS(1,I))
      CEE=ALENS(1,I)
!     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
      KAPPA=ALENS(2,I)
!
!     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
!     FIRST THE FORE OR FRONT POSITION
      IF(JJ.EQ.1) ZEE=FTFL01(1,I)
      IF(JJ.EQ.2) ZEE=FTFL01(2,I)
      RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
      IF(RHO2.LT.0.0D0) THEN
         IF(JJ.EQ.1)&
         &OUTLYNE=&
         &'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         IF(JJ.EQ.2)&
         &OUTLYNE=&
         &'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='NO CLAP COULD BE DRAWN'
         CALL SHOWIT(1)
         RETURN
      END IF
      RHO=DSQRT(RHO2)
      X=(DCOS(ANGLE)*RHO)
      Y=(DSIN(ANGLE)*RHO)
      XID=(DCOS(ANGLE)*RHO)
      YID=(DSIN(ANGLE)*RHO)
   END IF
   RETURN
END
! SUB CAO3.FOR
!
!     THIS ROUTINE IS CALLED BY PLTFOOT
!
SUBROUTINE CAO3(X,Y,ANGLE,I,AN2,JJ,XID,YID,MULTX,MULTY)
!
!     THIS SUBROUTINE CALCULATES THE X AND Y CLAP LIMIT VALUES
!     FOR SURFACE III WHICH LIE ON A LINE WHICH EXTENDS FROM THE
!     SURFACE VERTEX OUT TO THE CLAP EDGE. "ANGLE" IS MEASURED
!     COUNTER-CLOCKWISE FROM THE X AXIS TO THE Y AXIS OF THE LOCAL
!     SURFACE VERTEX COORDINATE SYSTEM. 0 DEGREES POINTS IN THE +X
!     DIRECTION. "ANGLE" IS PASSED TO THIS ROUTINE IN RADIAN MEASURE.
!     JJ IS ALWAYS 1 EXCEPT IF THE INNER CIRCULAR BOUNDRY OF A
!     TYPE 18 GRAZING INCIDENCE SURFACE MUST BE DRAWN.
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER JJ,CAFLG,I,J
!
   LOGICAL DOIT,ISAIR,ISAIR2,POSDIR,OLDIN,NEWIN,ISITIN,ISITINI
!
   EXTERNAL ISAIR,ISAIR2,ISITIN,ISITINI
!
   real(real64) X,Y,GAM,THETA1,THETA2,THETA3,THETA4,XID,YID &
   &,A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4,DA,DB &
   &,YCOR1,YCOR2,YCOR3,YCOR4,A1TEST,A2TEST,QTEST,Q,JK_AA,JK_BB &
   &,XA,YA,XB,YB,M,RADIUS,CBX,CBY,SGNB,CEE,ZEE,KAPPA,RHO2,RHO &
   &,JK_CC,CLPLCX(0:499),CLPLCY(0:499),XOLDX,YOLDY,MULTX,MULTY
!
   COMMON/CLPLOC/CLPLCX,CLPLCY
!
!
!       SET FLAG CAFLG
   CAFLG=INT(ALENS(9,I))
   GAM=0.0D0
!
!       CAFLG HAS BEEN INITIALIZED
!
!       CAFLG=0 NO CLAP, USE PARAXIAL DATA
   IF(ALENS(34,I).NE.18.0D0) THEN
!     NOT TYPE 18
      IF(CAFLG.EQ.0.OR.ALENS(127,I).NE.0.0D0) THEN
!
!     COORDINATES FOR THE SPECIFIED ANGLE ARE TAKEN FROM THE YZ
!     PLANE PARAXIAL DATA. CLAP TILT OR DECENTRATION APPLIES
         RAD=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
         X=(DCOS(ANGLE)*RAD)+CLPLCX(I)
         Y=(DSIN(ANGLE)*RAD)+CLPLCY(I)
         XID=(DCOS(ANGLE)*RAD)+CLPLCX(I)
         YID=(DSIN(ANGLE)*RAD)+CLPLCY(I)
      ELSE
!       CAFLG NOT 0
      END IF
!
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1.AND.ALENS(127,I).EQ.0.0D0) THEN
         IF(ALENS(10,I).LE.ALENS(11,I))  RAD=DABS(ALENS(10,I))
         IF(ALENS(10,I).GT.ALENS(11,I))  RAD=DABS(ALENS(11,I))
         X=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
         Y=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
         XID=(DCOS(ANGLE)*RAD)+ALENS(13,I)+MULTX
         YID=(DSIN(ANGLE)*RAD)+ALENS(12,I)+MULTY
      ELSE
!       CAFLG NOT 1
      END IF
!       CAFLG=5, CLAP POLY
      IF(CAFLG.EQ.5.AND.ALENS(127,I).EQ.0.0D0) THEN
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=(DBLE(J)*0.01D0*ALENS(10,I))*DCOS(ANGLE)
            Y=(DBLE(J)*0.01D0*ALENS(10,I))*DSIN(ANGLE)
            NEWIN=ISITIN(X,Y,I)
            X=X+ALENS(13,I)+MULTX
            Y=Y+ALENS(12,I)+MULTY
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               XID=X
               YID=Y
               GO TO 10
            ELSE
            END IF
         END DO
      ELSE
!     CAFLG NOT 5
      END IF
!       CAFLG=6, CLAP IPOLY
      IF(CAFLG.EQ.6) THEN
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=(DBLE(J)*0.01D0*ALENS(14,I))*DCOS(ANGLE)
            Y=(DBLE(J)*0.01D0*ALENS(14,I))*DSIN(ANGLE)
            NEWIN=ISITINI(X,Y,I)
            X=X+ALENS(13,I)
            Y=Y+ALENS(12,I)
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               XID=X
               YID=Y
               GO TO 10
            END IF
         END DO
      ELSE
!     CAFLG NOT 6
      END IF
10    CONTINUE
!       CAFLG=2, CLAP RECT
      IF(CAFLG.EQ.2.AND.ALENS(127,I).EQ.0.0D0) THEN
!     CORNER 1 HAS COORDINATES
         XCOR1=+DABS(ALENS(11,I))
         YCOR1=+DABS(ALENS(10,I))
         IF(DABS(YCOR1).LE.1.0D-15.AND.&
         &DABS(XCOR1).LE.1.0D-15) THEN
            THETA1=0.0D0
         ELSE
            THETA1=DATAN2(YCOR1,XCOR1)
         END IF
         IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
!     CORNER 2 HAS COORDINATES
         XCOR2=-DABS(ALENS(11,I))
         YCOR2=+DABS(ALENS(10,I))
         IF(DABS(YCOR2).LE.1.0D-15.AND.&
         &DABS(XCOR2).LE.1.0D-15) THEN
            THETA2=0.0D0
         ELSE
            THETA2=DATAN2(YCOR2,XCOR2)
         END IF
         IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
!     CORNER 3 HAS COORDINATES
         XCOR3=-DABS(ALENS(11,I))
         YCOR3=-DABS(ALENS(10,I))
         IF(DABS(YCOR3).LE.1.0D-15.AND.&
         &DABS(XCOR3).LE.1.0D-15) THEN
            THETA3=0.0D0
         ELSE
            THETA3=DATAN2(YCOR3,XCOR3)
         END IF
         IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
!     CORNER 4 HAS COORDINATES
         XCOR4=+DABS(ALENS(11,I))
         YCOR4=-DABS(ALENS(10,I))
         IF(DABS(YCOR4).LE.1.0D-15.AND.&
         &DABS(XCOR4).LE.1.0D-15) THEN
            THETA4=0.0D0
         ELSE
            THETA4=DATAN2(YCOR4,XCOR4)
         END IF
         IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
!
         IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
            Y=DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
            X=-DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=-DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
            Y=-DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=-DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF

         IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
            X=XCOR1
            Y=YCOR1
            XID=XCOR1
            YID=YCOR1
         ELSE
!     POINT NOT AT CORNER1
         END IF
         IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
            X=XCOR2
            Y=YCOR2
            XID=XCOR2
            YID=YCOR2
         ELSE
!     POINT NOT AT CORNER2
         END IF
         IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
            X=XCOR3
            Y=YCOR3
            XID=XCOR3
            YID=YCOR3
         ELSE
!     POINT NOT AT CORNER3
         END IF
         IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
            X=XCOR4
            Y=YCOR4
            XID=XCOR4
            YID=YCOR4
         ELSE
!     POINT NOT AT CORNER4
         END IF
!
!     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
         GAM=(PII/180.0D0)*ALENS(15,I)
         XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
         YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
         X=XR+ALENS(13,I)+MULTX
         Y=YR+ALENS(12,I)+MULTY
         XID=XR+ALENS(13,I)+MULTX
         YID=YR+ALENS(12,I)+MULTY
!
      ELSE
!     CAFLG NOT 2
      END IF
!
!       CAFLG=3, ELLIPTICAL CLAP
      IF(CAFLG.EQ.3.AND.ALENS(127,I).EQ.0.0D0) THEN
!       X-SEMI-MAJOR AXIS IS ALENS(11,I)
!       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
         A=(ALENS(11,I))
         B=(ALENS(10,I))
         RAD=((A**2)*(B**2))/&
         &(((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
         RAD=DSQRT(RAD)
         X=RAD*DCOS(ANGLE)
         Y=RAD*DSIN(ANGLE)
         XID=RAD*DCOS(ANGLE)
         YID=RAD*DSIN(ANGLE)
!
!     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
         GAM=(PII/180.0D0)*ALENS(15,I)
         XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
         YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
         X=XR+ALENS(13,I)+MULTX
         Y=YR+ALENS(12,I)+MULTY
         XID=XR+ALENS(13,I)+MULTX
         YID=YR+ALENS(12,I)+MULTY
!
      ELSE
!     CAFLG NOT 3
      END IF
!       CAFLG=4, CLAP RCTK
      IF(CAFLG.EQ.4.AND.ALENS(127,I).EQ.0.0D0) THEN
!     CORNER 1 HAS COORDINATES
         XCOR1=+DABS(ALENS(11,I))
         YCOR1=+DABS(ALENS(10,I))
         IF(DABS(YCOR1).LE.1.0D-15.AND.&
         &DABS(XCOR1).LE.1.0D-15) THEN
            THETA1=0.0D0
         ELSE
            THETA1=DATAN2(YCOR1,XCOR1)
         END IF
         IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
!     CORNER 2 HAS COORDINATES
         XCOR2=-DABS(ALENS(11,I))
         YCOR2=+DABS(ALENS(10,I))
         IF(DABS(YCOR2).LE.1.0D-15.AND.&
         &DABS(XCOR2).LE.1.0D-15) THEN
            THETA2=0.0D0
         ELSE
            THETA2=DATAN2(YCOR2,XCOR2)
         END IF
         IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
!     CORNER 3 HAS COORDINATES
         XCOR3=-DABS(ALENS(11,I))
         YCOR3=-DABS(ALENS(10,I))
         IF(DABS(YCOR3).LE.1.0D-15.AND.&
         &DABS(XCOR3).LE.1.0D-15) THEN
            THETA3=0.0D0
         ELSE
            THETA3=DATAN2(YCOR3,XCOR3)
         END IF
         IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
!     CORNER 4 HAS COORDINATES
         XCOR4=+DABS(ALENS(11,I))
         YCOR4=-DABS(ALENS(10,I))
         IF(DABS(YCOR4).LE.1.0D-15.AND.&
         &DABS(XCOR4).LE.1.0D-15) THEN
            THETA4=0.0D0
         ELSE
            THETA4=DATAN2(YCOR4,XCOR4)
         END IF
         IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
!
         IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
            Y=DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
            X=-DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=-DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
            Y=-DABS(ALENS(10,I))
            X=Y*DTAN((PII/2.0D0)-ANGLE)
            YID=-DABS(ALENS(10,I))
            XID=Y*DTAN((PII/2.0D0)-ANGLE)
         ELSE
         END IF
         IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
            X=DABS(ALENS(11,I))
            Y=X*DTAN(ANGLE)
            XID=DABS(ALENS(11,I))
            YID=X*DTAN(ANGLE)
         ELSE
         END IF
!
!     HERE, THE RACETRACK HAS BEEN DRAWN AS A RECTANGLE. NOW
!     DOES THIS POINT NEED TO BE MODIFIED DUE TO A RACETRACK
!     CORNER. THIS MODIFICATION IS DONE FOR:
!
!     ANGLES FROM 0 TO 90
!     THEN FROM 90 TO 180
!     THEN FROM 180 TO 270
!     THEN FROM 270 TO 360
         RADIUS=DABS(ALENS(14,I))
!
!     THESE ARE FOUR AREAS OF ADJUSTMENT
!
!     AREA 1
!
         IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.(PII/2.0D0)) THEN
            IF(DABS(YCOR1-RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR1).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2((YCOR1-RADIUS),XCOR1)
            END IF
            IF(DABS(YCOR1).LE.1.0D-15.AND.&
            &DABS(XCOR1-RADIUS).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2(YCOR1,(XCOR1-RADIUS))
            END IF
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR1-RADIUS
               CBY=YCOR1-RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     AREA 2
!
         IF(ANGLE.GE.(PII/2.0D0).AND.ANGLE.LE.PII) THEN
            IF(DABS(YCOR2).LE.1.0D-15.AND.&
            &DABS(XCOR2+RADIUS).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2(YCOR2,(XCOR2+RADIUS))
            END IF
            IF(DABS(YCOR2-RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR2).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2((YCOR2-RADIUS),XCOR2)
            END IF
            IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR2+RADIUS
               CBY=YCOR2-RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     AREA 3
!
         IF(ANGLE.GE.PII.AND.ANGLE.LE.((3.0D0*PII)/2.0D0)) THEN
            IF(DABS(YCOR3+RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR3).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2((YCOR3+RADIUS),XCOR3)
            END IF
            IF(DABS(YCOR3).LE.1.0D-15.AND.&
            &DABS(XCOR3+RADIUS).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2(YCOR3,(XCOR3+RADIUS))
            END IF
            IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR3+RADIUS
               CBY=YCOR3+RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     AREA 4
!
         IF(ANGLE.GE.((3.0D0*PII)/2.0D0)) THEN
            IF(DABS(YCOR4).LE.1.0D-15.AND.&
            &DABS(XCOR4-RADIUS).LE.1.0D-15) THEN
               A1TEST=0.0D0
            ELSE
               A1TEST=DATAN2(YCOR4,(XCOR4-RADIUS))
            END IF
            IF(DABS(YCOR4+RADIUS).LE.1.0D-15.AND.&
            &DABS(XCOR4).LE.1.0D-15) THEN
               A2TEST=0.0D0
            ELSE
               A2TEST=DATAN2((YCOR4+RADIUS),XCOR4)
            END IF
            IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
            IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
            IF(A2TEST.LE.PII)   A2TEST=A2TEST+(TWOPII)
            IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
               CBX=XCOR4-RADIUS
               CBY=YCOR4+RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
               M=DTAN(ANGLE)
               JK_AA=1.0D0+(M**2)
               JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
               JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
               QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
               IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
                  IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                     X=-JK_CC/JK_BB
                     Y=M*X
                     XID=-JK_CC/JK_BB
                     YID=M*X
                  ELSE
!     MORE THAN ONE SOLUTION EXISTS
                     SGNB=JK_BB/DABS(JK_BB)
                     Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                     XA=Q/JK_AA
                     XB=JK_CC/Q
                     YA=M*XA
                     YB=M*XB
                     DA=(XA**2)+(YA**2)
                     DB=(XB**2)+(YB**2)
                     IF(DA.GE.DB) THEN
                        X=XA
                        Y=YA
                        XID=XA
                        YID=YA
                     ELSE
                        X=XB
                        Y=YB
                        XID=XB
                        YID=YB
                     END IF
                  END IF
               ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
               END IF
            ELSE
!     NO CALCULATION NEEDED
            END IF
         ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
         END IF
!
!     APPLY THE CLAP TILT ANGLE AND THE CLAP DECENTRATIONS
         GAM=(PII/180.0D0)*ALENS(15,I)
         XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
         YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
         X=XR+ALENS(13,I)+MULTX
         Y=YR+ALENS(12,I)+MULTY
         XID=XR+ALENS(13,I)+MULTX
         YID=YR+ALENS(12,I)+MULTY
!
      ELSE
!     CAFLG NOT 4
      END IF
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1.AND.ALENS(127,I).EQ.0.0D0) THEN
!
         IF(ALENS(10,I).LE.ALENS(11,I))  RAD=DABS(ALENS(10,I))
         IF(ALENS(10,I).GT.ALENS(11,I))  RAD=DABS(ALENS(11,I))
!
!     CLAP DEC MUST BE 0
         IF((ALENS(12,I)+MULTX).EQ.0.0D0.AND.&
         &(ALENS(13,I)+MULTY).EQ.0.0D0) THEN
            IF(ALENS(34,I).NE.18.0D0) THEN
!
!     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
!     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
!
!     IF SURFACE I IS NOT AIR AND I+1 IS AIR
!     THEN IF CV>0 THERE MAY BE A FLAT TO DO
!
               DOIT=.FALSE.
               DOIT=ISAIR(I,POSDIR)
               IF(DOIT) THEN
                  IF(ALENS(1,I).GT.0.0D0.AND.POSDIR.OR.&
                  &ALENS(1,I).LT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                     RAD=DABS(ALENS(11,I))
                  END IF
               END IF
!
!     IF SURFACE I IS AIR AIR AND I+1 IS NOT AIR
!     THEN IF CV<0 THERE MAY BE A FLAT TO DO
!
               DOIT=.FALSE.
               DOIT=ISAIR2(I,POSDIR)
               IF(DOIT) THEN
                  IF(ALENS(1,I).LT.0.0D0.AND.POSDIR.OR.&
                  &ALENS(1,I).GT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                     RAD=DABS(ALENS(11,I))
                  END IF
               END IF
            END IF
         ELSE
!     DEC CLAP, NO FLAT CALC
         END IF
         XID=(DCOS(ANGLE)*RAD)
         YID=(DSIN(ANGLE)*RAD)
      ELSE
!       CAFLG NOT 1
      END IF
   ELSE
!     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
!     CALCULATE THE RADIUS OF THE CLAP
!     CEE IS THE CURVATURE CV (ALENS(1,I))
      CEE=ALENS(1,I)
!     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
      KAPPA=ALENS(2,I)
!
!     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
!     FIRST THE FORE OR FRONT POSITION
      IF(JJ.EQ.1) ZEE=FTFL01(1,I)
      IF(JJ.EQ.2) ZEE=FTFL01(2,I)
      RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
      IF(RHO2.LT.0.0D0) THEN
         IF(JJ.EQ.1)&
         &OUTLYNE=&
         &'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         IF(JJ.EQ.2)&
         &OUTLYNE=&
         &'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='NO CLAP COULD BE DRAWN'
         CALL SHOWIT(1)
         RETURN
      END IF
      RHO=DSQRT(RHO2)
      X=(DCOS(ANGLE)*RHO)
      Y=(DSIN(ANGLE)*RHO)
      XID=(DCOS(ANGLE)*RHO)
      YID=(DSIN(ANGLE)*RHO)
   END IF
   RETURN
END
! SUB CAO2.FOR
!
!     THIS ROUTINE IS CALLED BY PLTCOB
!
SUBROUTINE CAO2(X,Y,ANGLE,I,AN2,MDX,MDY,GAMGAM)
!
!     THIS SUBROUTINE CALCULATES THE X AND Y COBS LIMIT VALUES
!     FOR SURFACE III WHICH LIE ON A LINE WHICH EXTENDS FROM THE
!     SURFACE VERTEX OUT TO THE COBS EDGE. "ANGLE" IS MEASURED
!     COUNTER-CLOCKWISE FROM THE X AXIS TO THE Y AXIS OF THE LOCAL
!     SURFACE VERTEX COORDINATE SYSTEM. 0 DEGREES POINTS IN THE +X
!     DIRECTION. "ANGLE" IS PASSED TO THIS ROUTINE IN RADIAN MEASURE.
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER COFLG,I,J
!
   LOGICAL OLDIN,NEWIN,ISITIN2,ISITIN2I
!
   EXTERNAL ISITIN2,ISITIN2I
!
   real(real64) X,Y,GAM,THETA1,THETA2,THETA3,THETA4,MDX,MDY,GAMGAM &
   &,A,B,RAD,XR,YR,ANGLE,AN2,XCOR1,XCOR2,XCOR3,XCOR4,XOLDX,YOLDY &
   &,YCOR1,YCOR2,YCOR3,YCOR4,JK_AA,JK_BB,RADIUS,CBX,CBY &
   &,JK_CC,QTEST,Q,SGNB,XA,XB,YA,YB,A1TEST,A2TEST,M,DA,DB &
   &,XID,YID
!
!
!       SET FLAG COFLG
   COFLG=INT(ALENS(16,I))
   GAM=0.0D0
!
!       COFLG HAS BEEN INITIALIZED
!
!       COFLG=0 NO COBS, NOTHING TO DRAW, JUST RETURN
   IF(COFLG.EQ.0) THEN
      RETURN
   ELSE
!       COFLG NOT 0
   END IF
!
!       COFLG=1 CIRCULAR COBS
   IF(COFLG.EQ.1) THEN
      RAD=DABS(ALENS(17,I))
      X=(DCOS(ANGLE)*RAD)+ALENS(20,I)+MDX
      Y=(DSIN(ANGLE)*RAD)+ALENS(19,I)+MDY
   ELSE
!       COFLG NOT 1
   END IF
!       C0FLG=5, CLAP POLY
   IF(COFLG.EQ.5) THEN
      OLDIN=.TRUE.
      NEWIN=.TRUE.
      XOLDX=0.0D0
      YOLDY=0.0D0
      X=0.0D0
      Y=0.0D0
      DO J=1,99999

         XOLDX=X
         YOLDY=Y
         X=(DBLE(J)*0.005D0*ALENS(10,I))*DCOS(ANGLE)
         Y=(DBLE(J)*0.005D0*ALENS(10,I))*DSIN(ANGLE)
         NEWIN=ISITIN2(X,Y,I)
         X=X+ALENS(20,I)+MDX
         Y=Y+ALENS(19,I)+MDY
         IF(OLDIN.AND..NOT.NEWIN) THEN
            X=(X+XOLDX)/2.0D0
            Y=(Y+YOLDY)/2.0D0
            GO TO 10
         ELSE
         END IF
      END DO
   ELSE
!     COFLG NOT 5
   END IF
!       C0FLG=6, COBS IPOLY
   IF(COFLG.EQ.6) THEN
      OLDIN=.TRUE.
      NEWIN=.TRUE.
      XOLDX=0.0D0
      YOLDY=0.0D0
      X=0.0D0
      Y=0.0D0
      DO J=1,99999

         XOLDX=X
         YOLDY=Y
         X=(DBLE(J)*0.005D0*ALENS(14,I))*DCOS(ANGLE)
         Y=(DBLE(J)*0.005D0*ALENS(14,I))*DSIN(ANGLE)
         NEWIN=ISITIN2I(X,Y,I)
         X=X+ALENS(20,I)+MDX
         Y=Y+ALENS(19,I)+MDY
         IF(OLDIN.AND..NOT.NEWIN) THEN
            X=(X+XOLDX)/2.0D0
            Y=(Y+YOLDY)/2.0D0
            GO TO 10
         ELSE
         END IF
      END DO
   ELSE
!     COFLG NOT 6
   END IF
10 CONTINUE
!       COFLG=2, COBS RECT
   IF(COFLG.EQ.2) THEN
!     CORNER 1 HAS COORDINATES
      XCOR1=+DABS(ALENS(18,I))
      YCOR1=+DABS(ALENS(17,I))
      IF(DABS(YCOR1).LE.1.0D-15.AND.&
      &DABS(XCOR1).LE.1.0D-15) THEN
         THETA1=0.0D0
      ELSE
         THETA1=DATAN2(YCOR1,XCOR1)
      END IF
      IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
!     CORNER 2 HAS COORDINATES
      XCOR2=-DABS(ALENS(18,I))
      YCOR2=+DABS(ALENS(17,I))
      IF(DABS(YCOR2).LE.1.0D-15.AND.&
      &DABS(XCOR2).LE.1.0D-15) THEN
         THETA2=0.0D0
      ELSE
         THETA2=DATAN2(YCOR2,XCOR2)
      END IF
      IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
!     CORNER 3 HAS COORDINATES
      XCOR3=-DABS(ALENS(18,I))
      YCOR3=-DABS(ALENS(17,I))
      IF(DABS(YCOR3).LE.1.0D-15.AND.&
      &DABS(XCOR3).LE.1.0D-15) THEN
         THETA3=0.0D0
      ELSE
         THETA3=DATAN2(YCOR3,XCOR3)
      END IF
      IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
!     CORNER 4 HAS COORDINATES
      XCOR4=+DABS(ALENS(18,I))
      YCOR4=-DABS(ALENS(17,I))
      IF(DABS(YCOR4).LE.1.0D-15.AND.&
      &DABS(XCOR4).LE.1.0D-15) THEN
         THETA4=0.0D0
      ELSE
         THETA4=DATAN2(YCOR4,XCOR4)
      END IF
      IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
!
      IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
         X=DABS(ALENS(18,I))
         Y=X*DTAN(ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
         Y=DABS(ALENS(17,I))
         X=Y*DTAN((PII/2.0D0)-ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
         X=-DABS(ALENS(18,I))
         Y=X*DTAN(ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
         Y=-DABS(ALENS(17,I))
         X=Y*DTAN((PII/2.0D0)-ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
         X=DABS(ALENS(18,I))
         Y=X*DTAN(ANGLE)
      ELSE
      END IF
      IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
         X=XCOR1
         Y=YCOR1
      ELSE
!     POINT NOT AT CORNER1
      END IF
      IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
         X=XCOR2
         Y=YCOR2
      ELSE
!     POINT NOT AT CORNER2
      END IF
      IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
         X=XCOR3
         Y=YCOR3
      ELSE
!     POINT NOT AT CORNER3
      END IF
      IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
         X=XCOR4
         Y=YCOR4
      ELSE
!     POINT NOT AT CORNER4
      END IF
!
!     APPLY THE COBS TILT ANGLE AND THE COBS DECENTRATIONS
      GAM=(PII/180.0D0)*(ALENS(22,I)+GAMGAM)
      XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
      YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
      X=XR+ALENS(20,I)+MDX
      Y=YR+ALENS(19,I)+MDY
!
   ELSE
!     COFLG NOT 2
   END IF
!
!       COFLG=3, ELLIPTICAL COBS
   IF(COFLG.EQ.3) THEN
!       X-SEMI-MAJOR AXIS IS ALENS(11,I)
!       Y-SEMI-MAJOR AXIS IS ALENS(10,I)
      A=(ALENS(18,I))
      B=(ALENS(17,I))
      RAD=((A**2)*(B**2))/&
      &(((B**2)*((DCOS(ANGLE))**2))+((A**2)*((DSIN(ANGLE)**2))))
      RAD=DSQRT(RAD)
      X=RAD*DCOS(ANGLE)
      Y=RAD*DSIN(ANGLE)
!
!     APPLY THE COBS TILT ANGLE AND THE COBS DECENTRATIONS
      GAM=(PII/180.0D0)*(ALENS(22,I)+GAMGAM)
      XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
      YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
      X=XR+ALENS(20,I)+MDX
      Y=YR+ALENS(19,I)+MDY
!
   ELSE
!     COFLG NOT 3
   END IF
!       COFLG=4, COBS RCTK
   IF(COFLG.EQ.4) THEN
!     CORNER 1 HAS COORDINATES
      XCOR1=+DABS(ALENS(18,I))
      YCOR1=+DABS(ALENS(17,I))
      IF(DABS(YCOR1).LE.1.0D-15.AND.&
      &DABS(XCOR1).LE.1.0D-15) THEN
         THETA1=0.0D0
      ELSE
         THETA1=DATAN2(YCOR1,XCOR1)
      END IF
      IF(THETA1.LT.0.0D0) THETA1=THETA1+(TWOPII)
!     CORNER 2 HAS COORDINATES
      XCOR2=-DABS(ALENS(18,I))
      YCOR2=+DABS(ALENS(17,I))
      IF(DABS(YCOR2).LE.1.0D-15.AND.&
      &DABS(XCOR2).LE.1.0D-15) THEN
         THETA2=0.0D0
      ELSE
         THETA2=DATAN2(YCOR2,XCOR2)
      END IF
      IF(THETA2.LT.0.0D0) THETA2=THETA2+(TWOPII)
!     CORNER 3 HAS COORDINATES
      XCOR3=-DABS(ALENS(18,I))
      YCOR3=-DABS(ALENS(17,I))
      IF(DABS(YCOR3).LE.1.0D-15.AND.&
      &DABS(XCOR3).LE.1.0D-15) THEN
         THETA3=0.0D0
      ELSE
         THETA3=DATAN2(YCOR3,XCOR3)
      END IF
      IF(THETA3.LT.0.0D0) THETA3=THETA3+(TWOPII)
!     CORNER 4 HAS COORDINATES
      XCOR4=+DABS(ALENS(18,I))
      YCOR4=-DABS(ALENS(17,I))
      IF(DABS(YCOR4).LE.1.0D-15.AND.&
      &DABS(XCOR4).LE.1.0D-15) THEN
         THETA4=0.0D0
      ELSE
         THETA4=DATAN2(YCOR4,XCOR4)
      END IF
      IF(THETA4.LT.0.0D0) THETA4=THETA4+(TWOPII)
!
      IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.THETA1) THEN
         X=DABS(ALENS(18,I))
         Y=X*DTAN(ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA1.AND.ANGLE.LE.THETA2) THEN
         Y=DABS(ALENS(17,I))
         X=Y*DTAN((PII/2.0D0)-ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA2.AND.ANGLE.LE.THETA3) THEN
         X=-DABS(ALENS(18,I))
         Y=X*DTAN(ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA3.AND.ANGLE.LE.THETA4) THEN
         Y=-DABS(ALENS(17,I))
         X=Y*DTAN((PII/2.0D0)-ANGLE)
      ELSE
      END IF
      IF(ANGLE.GT.THETA4.AND.ANGLE.LE.(TWOPII)) THEN
         X=DABS(ALENS(18,I))
         Y=X*DTAN(ANGLE)
      ELSE
      END IF
      IF(ANGLE.LE.THETA1.AND.AN2.GE.THETA1) THEN
         X=XCOR1
         Y=YCOR1
      ELSE
!     POINT NOT AT CORNER1
      END IF
      IF(ANGLE.LE.THETA2.AND.AN2.GE.THETA2) THEN
         X=XCOR2
         Y=YCOR2
      ELSE
!     POINT NOT AT CORNER2
      END IF
      IF(ANGLE.LE.THETA3.AND.AN2.GE.THETA3) THEN
         X=XCOR3
         Y=YCOR3
      ELSE
!     POINT NOT AT CORNER3
      END IF
      IF(ANGLE.LE.THETA4.AND.AN2.GE.THETA4) THEN
         X=XCOR4
         Y=YCOR4
      ELSE
!     POINT NOT AT CORNER4
      END IF
!
!     HERE, THE RACETRACK HAS BEEN DRAWN AS A RECTANGLE. NOW
!     DOES THIS POINT NEED TO BE MODIFIED DUE TO A RACETRACK
!     CORNER. THIS MODIFICATION IS DONE FOR:
!
!     ANGLES FROM 0 TO 90
!     THEN FROM 90 TO 180
!     THEN FROM 180 TO 270
!     THEN FROM 270 TO 360
      RADIUS=DABS(ALENS(21,I))
!
!     THESE ARE FOUR AREAS OF ADJUSTMENT
!
!     AREA 1
!
      IF(ANGLE.GE.0.0D0.AND.ANGLE.LE.(PII/2.0D0)) THEN
         IF(DABS(YCOR1-RADIUS).LE.1.0D-15.AND.&
         &DABS(XCOR1).LE.1.0D-15) THEN
            A1TEST=0.0D0
         ELSE
            A1TEST=DATAN2((YCOR1-RADIUS),XCOR1)
         END IF
         IF(DABS(YCOR1).LE.1.0D-15.AND.&
         &DABS(XCOR1-RADIUS).LE.1.0D-15) THEN
            A2TEST=0.0D0
         ELSE
            A2TEST=DATAN2(YCOR1,(XCOR1-RADIUS))
         END IF
         IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
         IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
            CBX=XCOR1-RADIUS
            CBY=YCOR1-RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
            M=DTAN(ANGLE)
            JK_AA=1.0D0+(M**2)
            JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
            JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
            QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
            IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
               IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                  X=-JK_CC/JK_BB
                  Y=M*X
               ELSE
!     MORE THAN ONE SOLUTION EXISTS
                  SGNB=JK_BB/DABS(JK_BB)
                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                  XA=Q/JK_AA
                  XB=JK_CC/Q
                  YA=M*XA
                  YB=M*XB
                  DA=(XA**2)+(YA**2)
                  DB=(XB**2)+(YB**2)
                  IF(DA.GE.DB) THEN
                     X=XA
                     Y=YA
                  ELSE
                     X=XB
                     Y=YB
                  END IF
               END IF
            ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
            END IF
         ELSE
!     NO CALCULATION NEEDED
         END IF
      ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
      END IF
!
!     AREA 2
!
      IF(ANGLE.GE.(PII/2.0D0).AND.ANGLE.LE.PII) THEN
         IF(DABS(YCOR2).LE.1.0D-15.AND.&
         &DABS(XCOR2+RADIUS).LE.1.0D-15) THEN
            A1TEST=0.0D0
         ELSE
            A1TEST=DATAN2(YCOR2,(XCOR2+RADIUS))
         END IF
         IF(DABS(YCOR2-RADIUS).LE.1.0D-15.AND.&
         &DABS(XCOR2).LE.1.0D-15) THEN
            A2TEST=0.0D0
         ELSE
            A2TEST=DATAN2((YCOR2-RADIUS),XCOR2)
         END IF
         IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
         IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
         IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
            CBX=XCOR2+RADIUS
            CBY=YCOR2-RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
            M=DTAN(ANGLE)
            JK_AA=1.0D0+(M**2)
            JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
            JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
            QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
            IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
               IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                  X=-JK_CC/JK_BB
                  Y=M*X
               ELSE
!     MORE THAN ONE SOLUTION EXISTS
                  SGNB=JK_BB/DABS(JK_BB)
                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                  XA=Q/JK_AA
                  XB=JK_CC/Q
                  YA=M*XA
                  YB=M*XB
                  DA=(XA**2)+(YA**2)
                  DB=(XB**2)+(YB**2)
                  IF(DA.GE.DB) THEN
                     X=XA
                     Y=YA
                  ELSE
                     X=XB
                     Y=YB
                  END IF
               END IF
            ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
            END IF
         ELSE
!     NO CALCULATION NEEDED
         END IF
      ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
      END IF
!
!     AREA 3
!
      IF(ANGLE.GE.PII.AND.ANGLE.LE.((3.0D0*PII)/2.0D0)) THEN
         IF(DABS(YCOR3+RADIUS).LE.1.0D-15.AND.&
         &DABS(XCOR3).LE.1.0D-15) THEN
            A1TEST=0.0D0
         ELSE
            A1TEST=DATAN2((YCOR3+RADIUS),XCOR3)
         END IF
         IF(DABS(YCOR3).LE.1.0D-15.AND.&
         &DABS(XCOR3+RADIUS).LE.1.0D-15) THEN
            A2TEST=0.0D0
         ELSE
            A2TEST=DATAN2(YCOR3,(XCOR3+RADIUS))
         END IF
         IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
         IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
         IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
            CBX=XCOR3+RADIUS
            CBY=YCOR3+RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
            M=DTAN(ANGLE)
            JK_AA=1.0D0+(M**2)
            JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
            JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
            QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
            IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
               IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                  X=-JK_CC/JK_BB
                  Y=M*X
               ELSE
!     MORE THAN ONE SOLUTION EXISTS
                  SGNB=JK_BB/DABS(JK_BB)
                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                  XA=Q/JK_AA
                  XB=JK_CC/Q
                  YA=M*XA
                  YB=M*XB
                  DA=(XA**2)+(YA**2)
                  DB=(XB**2)+(YB**2)
                  IF(DA.GE.DB) THEN
                     X=XA
                     Y=YA
                  ELSE
                     X=XB
                     Y=YB
                  END IF
               END IF
            ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
            END IF
         ELSE
!     NO CALCULATION NEEDED
         END IF
      ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
      END IF
!
!     AREA 4
!
      IF(ANGLE.GE.((3.0D0*PII)/2.0D0)) THEN
         IF(DABS(YCOR4).LE.1.0D-15.AND.&
         &DABS(XCOR4-RADIUS).LE.1.0D-15) THEN
            A1TEST=0.0D0
         ELSE
            A1TEST=DATAN2(YCOR4,(XCOR4-RADIUS))
         END IF
         IF(DABS(YCOR4+RADIUS).LE.1.0D-15.AND.&
         &DABS(XCOR4).LE.1.0D-15) THEN
            A2TEST=0.0D0
         ELSE
            A2TEST=DATAN2((YCOR4+RADIUS),XCOR4)
         END IF
         IF(A1TEST.LE.0.0D0) A1TEST=A1TEST+(TWOPII)
         IF(A2TEST.LE.0.0D0) A2TEST=A2TEST+(TWOPII)
         IF(A2TEST.LE.PII)   A2TEST=A2TEST+(TWOPII)
         IF(ANGLE.GE.A1TEST.AND.ANGLE.LE.A2TEST) THEN
!     CALCULATE A NEW SET OF X AND Y VALUES
!     CENTER OF CIRCLE WITH RESPECT TO CENTER OF THE CLAP
            CBX=XCOR4-RADIUS
            CBY=YCOR4+RADIUS
!     SLOPE OF LINE FROM CLAP CENTER
            M=DTAN(ANGLE)
            JK_AA=1.0D0+(M**2)
            JK_BB=-(2.0D0*CBX)-(2.0D0*CBY*M)
            JK_CC=(CBX**2)+(CBY**2)-(RADIUS**2)
            QTEST=(JK_BB**2)-(4.0D0*JK_AA*JK_CC)
            IF(QTEST.GE.0.0D0) THEN
!     SOLUTION EXISTS, ADJUSTMENT IS NECESSARY
               IF(JK_AA.EQ.0.0D0) THEN
!     ONLY ONE SOLUTION EXISTS
                  X=-JK_CC/JK_BB
                  Y=M*X
               ELSE
!     MORE THAN ONE SOLUTION EXISTS
                  SGNB=JK_BB/DABS(JK_BB)
                  Q=-0.5*(JK_BB+(SGNB*DSQRT(QTEST)))
                  XA=Q/JK_AA
                  XB=JK_CC/Q
                  YA=M*XA
                  YB=M*XB
                  DA=(XA**2)+(YA**2)
                  DB=(XB**2)+(YB**2)
                  IF(DA.GE.DB) THEN
                     X=XA
                     Y=YA
                  ELSE
                     X=XB
                     Y=YB
                  END IF
               END IF
            ELSE
!     NO SOLUTION(S) EXISTS, NO ADJUSTMENT IS NECESSARY
            END IF
         ELSE
!     NO CALCULATION NEEDED
         END IF
      ELSE
!     NO CORNER VALUES NEEDED TO BE CALCULATED
      END IF
!
!     APPLY THE COBS TILT ANGLE AND THE COBS DECENTRATIONS
      GAM=(PII/180.0D0)*(ALENS(22,I)+GAMGAM)
      XR=(X*DCOS(GAM))-(Y*DSIN(GAM))
      YR=(Y*DCOS(GAM))+(X*DSIN(GAM))
      X=XR+ALENS(20,I)+MDX
      Y=YR+ALENS(19,I)+MDY
!
   ELSE
!     COFLG NOT 4
   END IF
   RETURN
END
! SUB CAOJK.FOR
!
SUBROUTINE CAOJK(YMIN,XMIN,YMAX,XMAX,&
&YMINO,XMINO,YMAXO,XMAXO,CAFLG,&
&COFLG,I,&
&YMIN2,XMIN2,YMAX2,XMAX2,THETA,ZDELZ)
!
!     THIS ROUTINE GETS THE SAGS OF LIMITING POINTS AROUND A SURFACE
!     CLAP OR COBS AND RECOGNIZES THE EXISTENCE OF FLATS ON CONCAVE
!     SURFACES.
!
   use DATLEN
   use DATMAI
   use mod_lens_data_manager, only: ldm
   use global_widgets, only: sysConfig
   use mod_surface, only: surf_clap_type, surf_clap_dim, surf_clap_tilt, &
                          surf_special_type, surf_curvature
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER CAFLG,COFLG,I,J
!
   LOGICAL ISITIN,DOIT,ISAIR,ISAIR2,POSDIR,OLDIN,NEWIN,ISITINI
!
   EXTERNAL ISAIR,ISAIR2,ISITIN,ISITINI
!
   real(real64) X,Y,XMIN,YMIN,XMAX,YMAX,XOLDX,YOLDY,ZDELZ &
   &,XMINO,YMINO,XMAXO,YMAXO,XTOPO,GAM,RHO2,RHO &
   &,CLPLCX(0:499),CLPLCY(0:499),KAPPA,CEE,ZEE,PEEPEE,&
   &YMIN2,XMIN2,YMAX2,XMAX2,THETA,THETA2,APX,APY,CHX,CHY
!
   COMMON/CLPLOC/CLPLCX,CLPLCY
   real(real64) :: dF   ! system default edge-aperture scale factor
!
!
   ZDELZ=0.0D0
   dF = sysConfig%defaultEdgeScaleFactor
!
   THETA2=THETA+PII
!
!       SET FLAGS CAFLG AND COFLG
   CAFLG=surf_clap_type(I)
   COFLG=INT(ALENS(16,I))
   GAM=0.0D0
   IF(surf_special_type(I) /= 18) THEN
!     NOT TYPE 18 SPECIAL SURFACE
!
!       CAFLG AND COFLG HAVE BEEN SET
!
!       CAFLG=0 NO CLAP, USE PARAXIAL DATA
!
      IF(CAFLG.EQ.0) THEN
!
!     COORDINATES OF THE END POINTS FOR PROF.
!     With no clear aperture, the surface half-extent is estimated from paraxial
!     data: |marginal ray height| + |chief ray height|.  The chief term blows up
!     for a surface at an infinite conjugate (object/image at infinity, where
!     chief height = field angle x ~infinite distance); drop it when it is
!     non-physically large AND utterly dominates the marginal, so such a surface
!     does not draw enormously and wreck the plot autoscale.
         APX=DABS(PXTRAX(1,I))
         APY=DABS(PXTRAY(1,I))
         CHX=DABS(PXTRAX(5,I))
         CHY=DABS(PXTRAY(5,I))
         IF(CHX.GT.1.0D10.AND.CHX.GT.1.0D6*APX) CHX=0.0D0
         IF(CHY.GT.1.0D10.AND.CHY.GT.1.0D6*APY) CHY=0.0D0
         APX=APX+CHX
         APY=APY+CHY
!     Prefer the ray-traced footprint (display-only auto clear aperture, filled by
!     check_clear_apertures) when it is larger, so the drawn surface always covers
!     the rays that hit it -- the paraxial estimate underestimates fast/aberrated
!     surfaces (e.g. a Cassegrain secondary with no defined clear aperture).
         APX=MAX(APX, DABS(ldm%getSurfAutoSemiX(I)))
         APY=MAX(APY, DABS(ldm%getSurfAutoSemiY(I)))
!     Edge (physical) aperture: an explicit CIR EDG value overrides the extent;
!     otherwise the system default edge factor scales the auto/paraxial extent.
         IF(ldm%getEdgeSemiAperture(I).GT.0.0D0) THEN
            APX=ldm%getEdgeSemiAperture(I)
            APY=ldm%getEdgeSemiAperture(I)
         ELSE
            APX=APX*dF
            APY=APY*dF
         END IF
         XMAX=(APX*DCOS(THETA))+CLPLCX(I)
         YMAX=(APY*DSIN(THETA))+CLPLCY(I)
         XMIN=(APX*DCOS(THETA2))+CLPLCX(I)
         YMIN=(APY*DSIN(THETA2))+CLPLCY(I)
!
         XMIN2=XMIN
         YMIN2=YMIN
         XMAX2=XMAX
         YMAX2=YMAX
      ELSE
!       CAFLG NOT 0
      END IF
!
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1) THEN
!
         PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,1,dF))
!
!       RIGHT POINT
         XMAX=((PEEPEE)*DCOS(THETA))+surf_clap_dim(I,4)
         YMAX=((PEEPEE)*DSIN(THETA))+surf_clap_dim(I,3)
         XMIN=((PEEPEE)*DCOS(THETA2))+surf_clap_dim(I,4)
         YMIN=((PEEPEE)*DSIN(THETA2))+surf_clap_dim(I,3)
         XMAX2=XMAX
         YMAX2=YMAX
         XMIN2=XMIN
         YMIN2=YMIN
      ELSE
!       CAFLG NOT 1
      END IF
!
!       CAFLG=2,3,OR 4
      IF(CAFLG.EQ.2.OR.CAFLG.EQ.3.OR.CAFLG.EQ.4) THEN
!
         GAM=(PII/180.0D0)*surf_clap_tilt(I)
         XMAX=(ldm%getClearApertureForLensDraw(I,2,dF))*DCOS(THETA)
         YMAX=(ldm%getClearApertureForLensDraw(I,1,dF))*DSIN(THETA)
         X=(XMAX*DCOS(GAM))-(YMAX*DSIN(GAM))
         Y=(YMAX*DCOS(GAM))+(XMAX*DSIN(GAM))
         XMAX=X+surf_clap_dim(I,4)
         YMAX=Y+surf_clap_dim(I,3)
         XMAX2=X+surf_clap_dim(I,4)
         YMAX2=Y+surf_clap_dim(I,3)
         XMIN=(ldm%getClearApertureForLensDraw(I,2,dF))*DCOS(THETA2)
         YMIN=(ldm%getClearApertureForLensDraw(I,1,dF))*DSIN(THETA2)
         X=(XMIN*DCOS(GAM))-(YMIN*DSIN(GAM))
         Y=(YMIN*DCOS(GAM))+(XMIN*DSIN(GAM))
         XMIN=X+surf_clap_dim(I,4)
         YMIN=Y+surf_clap_dim(I,3)
         XMIN2=X+surf_clap_dim(I,4)
         YMIN2=Y+surf_clap_dim(I,3)
      ELSE
!     CAFLG NOT 2,3 OR 4
      END IF
!       CAFLG=5
      IF(CAFLG.EQ.5) THEN
!   THETA IS THE ANGLE OF THE PROFILE LINE IN THE
!   LOCAL COORDINATE SYSTEM OF THE SURFACE
!   THE COORDINATES ON THIS LINE ARE
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DCOS(THETA)
            Y=(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DSIN(THETA)
            NEWIN=ISITIN(X,Y,I)
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               X=X+surf_clap_dim(I,4)
               Y=Y+surf_clap_dim(I,3)
               XMAX=X
               YMAX=Y
               XMAX2=X
               YMAX2=Y
               GO TO 10
            ELSE
            END IF
         END DO
10       CONTINUE
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=-(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DCOS(THETA)
            Y=-(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DSIN(THETA)
            NEWIN=ISITIN(X,Y,I)
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               X=X+surf_clap_dim(I,4)
               Y=Y+surf_clap_dim(I,3)
               XMIN=X
               YMIN=Y
               XMIN2=X
               YMIN2=Y
               GO TO 20
            ELSE
            END IF
         END DO
      ELSE
!     CAFLG NOT 5
      END IF
!       CAFLG=6
      IF(CAFLG.EQ.6) THEN
!   THETA IS THE ANGLE OF THE PROFILE LINE IN THE
!   LOCAL COORDINATE SYSTEM OF THE SURFACE
!   THE COORDINATES ON THIS LINE ARE
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DCOS(THETA)
            Y=(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DSIN(THETA)
            NEWIN=ISITIN(X,Y,I)
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               X=X+surf_clap_dim(I,4)
               Y=Y+surf_clap_dim(I,3)
               XMAX=X
               YMAX=Y
               XMAX2=X
               YMAX2=Y
               GO TO 11
            ELSE
            END IF
         END DO
11       CONTINUE
         OLDIN=.TRUE.
         NEWIN=.TRUE.
         XOLDX=0.0D0
         YOLDY=0.0D0
         X=0.0D0
         Y=0.0D0
         DO J=1,99999

            XOLDX=X
            YOLDY=Y
            X=-(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DCOS(THETA)
            Y=-(DBLE(J)*0.01D0*ldm%getClearApertureForLensDraw(I,1,dF))*DSIN(THETA)
            NEWIN=ISITIN(X,Y,I)
            IF(OLDIN.AND..NOT.NEWIN) THEN
               X=(X+XOLDX)/2.0D0
               Y=(Y+YOLDY)/2.0D0
               X=X+surf_clap_dim(I,4)
               Y=Y+surf_clap_dim(I,3)
               XMIN=X
               YMIN=Y
               XMIN2=X
               YMIN2=Y
               GO TO 20
            ELSE
            END IF
         END DO
      ELSE
!     CAFLG NOT 5
      END IF
20    CONTINUE
!       CAFLG=1 CIRCULAR CLAP
      IF(surf_clap_dim(I,4).EQ.0.0D0.AND.surf_clap_dim(I,3).EQ.0.0D0) THEN
         IF(CAFLG.EQ.1) THEN
!
            PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,1,dF))
!
!     CLAP DEC MUST BE 0
!
!     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
!     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
!
!     IF SURFACE I IS NOT AIR AND I+1 IS AIR
!     THEN IF CV>0 THERE MAY BE A FLAT TO DO
!     AS LONG AS SPECIAL SURFACE 18 NOT PRESENT
!
!
            DOIT=.FALSE.
!     CHECK FOR A FLAT, ELSE DON'T
!
            DOIT=ISAIR(I,POSDIR)
            IF(DOIT) THEN
               IF(surf_curvature(I).GT.0.0D0.AND.POSDIR.OR.&
               &surf_curvature(I).LT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                  PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,2,dF))
                  IF(surf_curvature(I).GT.0.0D0) ZDELZ=-DABS(surf_clap_dim(I,5))
                  IF(surf_curvature(I).LT.0.0D0) ZDELZ=DABS(surf_clap_dim(I,5))
               END IF
            END IF
!
!     IF SURFACE I IS NOT AIR AND I+1 IS AIR
!     THEN IF CV<0 THERE MAY BE A FLAT TO DO
            DOIT=.FALSE.
            DOIT=ISAIR2(I,POSDIR)
            IF(DOIT) THEN
               IF(surf_curvature(I).LT.0.0D0.AND.POSDIR.OR.&
               &surf_curvature(I).GT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                  PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,2,dF))
                  IF(surf_curvature(I).GT.0.0D0) ZDELZ=-DABS(surf_clap_dim(I,5))
                  IF(surf_curvature(I).LT.0.0D0) ZDELZ=DABS(surf_clap_dim(I,5))
               END IF
            END IF
!
            XMAX2=(PEEPEE)*DCOS(THETA)
            YMAX2=(PEEPEE)*DSIN(THETA)
            XMIN2=(PEEPEE)*DCOS(THETA2)
            YMIN2=(PEEPEE)*DSIN(THETA2)
         ELSE
!       CAFLG NOT 1
         END IF
      ELSE
!     DEC CLAP, NO FLAT CALC
         XMAX2=XMAX
         YMAX2=YMAX
         XMIN2=XMIN
         YMIN2=YMIN
      END IF
!       COFLG=0 NO COBS
      IF(COFLG.EQ.0) THEN
         XMINO=0.0D0
         YMINO=0.0D0
         XMAXO=0.0D0
         YMAXO=0.0D0
         RETURN
      ELSE
!     MUST BE SOME COBS, CONTINUE
      END IF
!       COFLG=1 CIRCULAR COBS
      IF(COFLG.EQ.1) THEN
!       RIGHT POINT
         XMAXO=((ALENS(17,I))*DCOS(THETA))
         YMAXO=((ALENS(17,I))*DSIN(THETA))
         XMINO=((ALENS(17,I))*DCOS(THETA2))
         YMINO=((ALENS(17,I))*DSIN(THETA2))
         XMAXO=XMAXO+ALENS(20,I)
         XMINO=XMINO+ALENS(20,I)
         YMAXO=YMAXO+ALENS(19,I)
         YMINO=YMINO+ALENS(19,I)
      ELSE
!       COFLG NOT 1
      END IF
!
!       COFLG=2,3,OR 4
      IF(COFLG.EQ.2.OR.COFLG.EQ.3.OR.COFLG.EQ.4) THEN
!
         GAM=(PII/180.0D0)*ALENS(22,I)
         XMAXO=(ALENS(18,I))*DCOS(THETA)
         YMAXO=(ALENS(17,I))*DSIN(THETA)
         X=(XMAXO*DCOS(GAM))-(YMAXO*DSIN(GAM))
         Y=(YMAXO*DCOS(GAM))+(XMAXO*DSIN(GAM))
         XMAXO=X+ALENS(20,I)
         YMAXO=Y+ALENS(19,I)
         XMINO=(ALENS(18,I))*DCOS(THETA2)
         YMINO=(ALENS(17,I))*DSIN(THETA2)
         X=(XMINO*DCOS(GAM))-(YMINO*DSIN(GAM))
         Y=(YMINO*DCOS(GAM))+(XMINO*DSIN(GAM))
         XMINO=X+ALENS(20,I)
         YMINO=Y+ALENS(19,I)
      ELSE
!     COFLG NOT 2,3 OR 4
      END IF
!     NOW FOR SOME SURFACE TYPES, THERE IS A PREDICTABLE
!     EXTENT BEYOND WHICH THE SAG OF THE SURFACE WILL NOT
!     BE A REAL NUMBER. IF THE LIMITS CALCULATED ABOVE EXCEED
!     THESE LIMITS, THEN ADJUST THE LIMITS BEFORE PROCEEDING.
   ELSE
!     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
!     CEE IS THE CURVATURE CV (ALENS(1,I))
      CEE=ALENS(1,I)
!     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
      KAPPA=ALENS(2,I)
!
!     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
!     FIRST THE FORE OR FRONT POSITION
      ZEE=FTFL01(1,I)
      RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
      IF(RHO2.LT.0.0D0) THEN
         OUTLYNE=&
         &'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='NO PROFILE COULD BE DRAWN'
         CALL SHOWIT(1)
         XMIN=0.0D0
         YMIN=0.0D0
         XMAX=0.0D0
         YMAX=0.0D0
         XMINO=0.0D0
         YMINO=0.0D0
         XMAXO=0.0D0
         YMAXO=0.0D0
         RETURN
      END IF
      RHO=DSQRT(RHO2)
      XMAX=RHO*DCOS(THETA)
      YMAX=RHO*DSIN(THETA)
      XMIN=RHO*DCOS(THETA2)
      YMIN=RHO*DSIN(THETA2)
!     THEN THE AFT OR BACK POSITION
      ZEE=FTFL01(2,I)
      RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
      IF(RHO2.LT.0.0D0) THEN
         OUTLYNE=&
         &'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='NO PROFILE COULD BE DRAWN'
         CALL SHOWIT(1)
         XMIN=0.0D0
         YMIN=0.0D0
         XMAX=0.0D0
         YMAX=0.0D0
         XMINO=0.0D0
         YMINO=0.0D0
         XMAXO=0.0D0
         YMAXO=0.0D0
         RETURN
      END IF
      RHO=DSQRT(RHO2)
      XMAXO=RHO*DCOS(THETA)
      YMAXO=RHO*DSIN(THETA)
      XMINO=RHO*DCOS(THETA2)
      YMINO=RHO*DSIN(THETA2)
   END IF
   RETURN
END
! SUB ROT1.FOR
SUBROUTINE ROT1
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
!     AND PLOT VIEW FOR THE PLOT RAY, EDGEY AND EDGEX COMMANDS
!
   real(real64) X,Y,Z &
   &,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ
!
   INTEGER I
!
!
   STASUR=INT(W1)
   STPSUR=INT(W2)
   IF(.NOT.ROTSET) THEN
      DO I=STASUR,STPSUR
         X=VERTEX(1,I)
         Y=VERTEX(2,I)
         Z=VERTEX(3,I)
         IF(I.EQ.STASUR) THEN
            XMINIX=X
            XMAXIX=X
            YMINIY=Y
            YMAXIY=Y
            ZMINIZ=Z
            ZMAXIZ=Z
         ELSE
         END IF
         IF(X.LE.XMINIX) XMINIX=X
         IF(X.GT.XMAXIX) XMAXIX=X
         IF(Y.LE.YMINIY) YMINIY=Y
         IF(Y.GT.YMAXIY) YMAXIY=Y
         IF(Z.LE.ZMINIZ) ZMINIZ=Z
         IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
      END DO
      XROT=(XMAXIX+XMINIX)/2.0D0
      YROT=(YMAXIY+YMINIY)/2.0D0
      ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
      ROTSET=.TRUE.
   ELSE
   END IF
   RETURN
END
! SUB ROT2.FOR
SUBROUTINE ROT2(PRO,M1,M2,M3,M4)
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DETERMINES THE CENTER OF ROTATION FOR PLOT LOOK
!     AND PLOT VIEW FOR THE PLOT PROFX/PROFY COMMANDS
!
!     IF SEVERAL SURFACES ARE ASKED FOR, ROTATION POINTS ARE
!     CALCULATED USING GLOBAL VERTEX DATA. FOR ONE SURFACE,
!     THE ROTATION POINT IS SET AT THE CENTER ON THE X AND Y
!     SURFACE PROFILES.
!
   real(real64) X,Y,Z,X1,Y1,Z1,X2,Y2,Z2 &
   &,XMAXIX,XMINIX,YMAXIY,YMINIY,ZMAXIZ,ZMINIZ,XA,YA,ZA,PRO
!
   INTEGER I,M1,M2,M3,M4
!
   DIMENSION PRO(M1,M2,M3:M4)
!
!
   STASUR=INT(W1)
   STPSUR=INT(W2)
   IF(STASUR.NE.STPSUR) THEN
      IF(.NOT.ROTSET) THEN
         DO I=STASUR,STPSUR
            X=VERTEX(1,I)
            Y=VERTEX(2,I)
            Z=VERTEX(3,I)
            IF(I.EQ.STASUR) THEN
               XMINIX=X
               XMAXIX=X
               YMINIY=Y
               YMAXIY=Y
               ZMINIZ=Z
               ZMAXIZ=Z
            ELSE
            END IF
            IF(X.LE.XMINIX) XMINIX=X
            IF(X.GT.XMAXIX) XMAXIX=X
            IF(Y.LE.YMINIY) YMINIY=Y
            IF(Y.GT.YMAXIY) YMAXIY=Y
            IF(Z.LE.ZMINIZ) ZMINIZ=Z
            IF(Z.GT.ZMAXIZ) ZMAXIZ=Z
         END DO
         XROT=(XMAXIX+XMINIX)/2.0D0
         YROT=(YMAXIY+YMINIY)/2.0D0
         ZROT=(ZMAXIZ+ZMINIZ)/2.0D0
         ROTSET=.TRUE.
      ELSE
!     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
      END IF
   ELSE
!     STASUR SAME AS STPSUR, KEY OFF PROFILE END POINTS
      IF(.NOT.ROTSET) THEN
         I=STASUR
!
         X1=PRO(1,1,I)
         Y1=PRO(1,2,I)
         Z1=PRO(1,3,I)
         X2=PRO(90,1,I)
         Y2=PRO(90,2,I)
         Z2=PRO(90,3,I)
         XA=(X2+X1)/2.0D0
         YA=(Y2+Y1)/2.0D0
         ZA=(Z2+Z1)/2.0D0
         XROT=XA
         YROT=YA
         ZROT=ZA
         ROTSET=.TRUE.
      ELSE
!     ROTSET IS TRUE AND XROT,YROT AND ZROT HAVE ALREADY BEEN DONE
      END IF
   END IF
   RETURN
END
FUNCTION ISAIR(I,POSDIR)
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   LOGICAL A1,A2,ISAIR,POSDIR
   INTEGER I
   A1=.TRUE.
!     A1 TRUE MEANS AIR
   IF(DABS(ALENS(46,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(47,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(48,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(49,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(50,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(71,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(72,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(73,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(74,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(75,I-1)).GT.1.0D0) A1=.FALSE.
   A2=.TRUE.
!     A2 TRUE MEANS AIR
   IF(DABS(ALENS(46,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(47,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(48,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(49,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(50,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(71,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(72,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(73,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(74,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(75,I)).GT.1.0D0) A2=.FALSE.
   ISAIR=.FALSE.
   IF(.NOT.A1.AND.A2) ISAIR=.TRUE.
!
   POSDIR=.TRUE.
   IF((ALENS(46,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(47,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(48,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(49,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(50,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(71,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(72,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(73,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(74,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(75,I)).LT.0.0D0) POSDIR=.FALSE.
   RETURN
END
FUNCTION ISAIR2(I,POSDIR)
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   LOGICAL A1,A2,ISAIR2,POSDIR
   INTEGER I
   A1=.TRUE.
   IF(DABS(ALENS(46,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(47,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(48,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(49,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(50,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(71,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(72,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(73,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(74,I-1)).GT.1.0D0) A1=.FALSE.
   IF(DABS(ALENS(75,I-1)).GT.1.0D0) A1=.FALSE.
   A2=.TRUE.
   IF(DABS(ALENS(46,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(47,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(48,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(49,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(50,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(71,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(72,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(73,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(74,I)).GT.1.0D0) A2=.FALSE.
   IF(DABS(ALENS(75,I)).GT.1.0D0) A2=.FALSE.
   ISAIR2=.FALSE.
   IF(A1.AND..NOT.A2) ISAIR2=.TRUE.
!
   POSDIR=.TRUE.
   IF((ALENS(46,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(47,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(48,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(49,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(50,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(71,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(72,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(73,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(74,I)).LT.0.0D0) POSDIR=.FALSE.
   IF((ALENS(75,I)).LT.0.0D0) POSDIR=.FALSE.
   RETURN
END
! SUB CAO.FOR
!
SUBROUTINE CAO(YLFT,XLFT,YRHT,XRHT,XTOP,YTOP,XBOT,YBOT,&
&YLFTO,XLFTO,YRHTO,XRHTO,XTOPO,YTOPO,XBOTO,YBOTO,CAFLG,&
&COFLG,I,&
&YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ)
!
!     THIS ROUTINE GETS THE SAGS OF LIMITING POINTS AROUND A SURFACE
!     CLAP OR COBS AND RECOGNIZES THE EXISTENCE OF FLATS ON CONCAVE
!     SURFACES.
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   use mod_lens_data_manager, only: ldm
   use global_widgets, only: sysConfig
   use mod_surface, only: surf_clap_type, surf_clap_dim, surf_clap_tilt, &
                          surf_special_type, surf_curvature
   IMPLICIT NONE
!
   INTEGER CAFLG,COFLG,I,CAFLG2
!
   LOGICAL DOIT,ISAIR,ISAIR2,POSDIR
!
   EXTERNAL ISAIR,ISAIR2
!
   real(real64) X,Y,XLFT,YLFT,XRHT,YRHT,XTOP,YTOP,XBOT,YBOT &
   &,XLFTO,YLFTO,XRHTO,YRHTO,XTOPO,YTOPO,XBOTO,YBOTO,GAM,RHO2,RHO &
   &,CLPLCX(0:499),CLPLCY(0:499),KAPPA,CEE,ZEE,PEEPEE,&
   &YLFT2,XLFT2,YRHT2,XRHT2,XTOP2,YTOP2,XBOT2,YBOT2,ZDELZ
!
   COMMON/CLPLOC/CLPLCX,CLPLCY
   real(real64) :: dF   ! system default edge-aperture scale factor
!
   ZDELZ=0.0D0
   dF = sysConfig%defaultEdgeScaleFactor
!
!       SET FLAGS CAFLG AND COFLG
   CAFLG=surf_clap_type(I)
   COFLG=INT(ALENS(16,I))
   GAM=0.0D0
   IF(surf_special_type(I) /= 18) THEN
!     NOT TYPE 18 SPECIAL SURFACE
!
!       CAFLG AND COFLG HAVE BEEN SET
!
!       CAFLG=0 NO CLAP, USE PARAXIAL DATA
!
      IF(CAFLG.EQ.0) THEN
!     Auto (paraxial) aperture with no explicit clear aperture.  Apply the edge
!     (physical) size: an explicit CIR EDG value overrides the extent; otherwise
!     the system default edge factor scales the paraxial extent.
         X=(DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I)))
         Y=(DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I)))
         IF(ldm%getEdgeSemiAperture(I).GT.0.0D0) THEN
            X=ldm%getEdgeSemiAperture(I)
            Y=ldm%getEdgeSemiAperture(I)
         ELSE
            X=X*dF
            Y=Y*dF
         END IF
!     COORDINATES OF THE END POINTS FOR PROFX
         XRHT=( X)+CLPLCX(I)
         YRHT=0.0D0+CLPLCY(I)
         XLFT=(-X)+CLPLCX(I)
         YLFT=0.0D0+CLPLCY(I)
!     COORDINATES OF THE END POINTS FOR PROFY
         XTOP=0.0D0+CLPLCX(I)
         YTOP=( Y)+CLPLCY(I)
         XBOT=0.0D0+CLPLCX(I)
         YBOT=(-Y)+CLPLCY(I)
         XTOP2=XTOP
         YTOP2=YTOP
         XBOT2=XBOT
         YBOT2=YBOT
         XRHT2=XRHT
         YRHT2=YRHT
         XLFT2=XLFT
         YLFT2=YLFT
      ELSE
!       CAFLG NOT 0
      END IF
!
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1) THEN
!
         PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,1,dF))
!
!       RIGHT POINT
         XRHT=(PEEPEE)+surf_clap_dim(I,4)
         YRHT=0.0D0+surf_clap_dim(I,3)
!       LEFT POINT
         XLFT=(-PEEPEE)+surf_clap_dim(I,4)
         YLFT=0.0D0+surf_clap_dim(I,3)
!       TOP POINT
         XTOP=0.0D0+surf_clap_dim(I,4)
         YTOP=(PEEPEE)+surf_clap_dim(I,3)
!       BOTTOM BOINT
         XBOT=0.0D0+surf_clap_dim(I,4)
         YBOT=(-PEEPEE)+surf_clap_dim(I,3)
      ELSE
!       CAFLG NOT 1
      END IF
!
!       CAFLG=2
      IF(CAFLG.EQ.2) THEN
!
         GAM=(PII/180.0D0)*surf_clap_tilt(I)
!       RIGHT POINT
         XRHT=( ldm%getClearApertureForLensDraw(I,2,dF))
         YRHT=0.0D0
         X=(XRHT*DCOS(GAM))-(YRHT*DSIN(GAM))
         Y=(YRHT*DCOS(GAM))+(XRHT*DSIN(GAM))
         XRHT=X+surf_clap_dim(I,4)
         YRHT=Y+surf_clap_dim(I,3)
         XRHT2=X+surf_clap_dim(I,4)
         YRHT2=Y+surf_clap_dim(I,3)
!       LEFT POINT
         XLFT=(-ldm%getClearApertureForLensDraw(I,2,dF))
         YLFT=0.0D0
         X=(XLFT*DCOS(GAM))-(YLFT*DSIN(GAM))
         Y=(YLFT*DCOS(GAM))+(XLFT*DSIN(GAM))
         XLFT=X+surf_clap_dim(I,4)
         YLFT=Y+surf_clap_dim(I,3)
         XLFT2=X+surf_clap_dim(I,4)
         YLFT2=Y+surf_clap_dim(I,3)
!       TOP POINT
         XTOP=0.0D0
         YTOP=( ldm%getClearApertureForLensDraw(I,1,dF))
         X=(XTOP*DCOS(GAM))-(YTOP*DSIN(GAM))
         Y=(YTOP*DCOS(GAM))+(XTOP*DSIN(GAM))
         XTOP=X+surf_clap_dim(I,4)
         YTOP=Y+surf_clap_dim(I,3)
         XTOP2=X+surf_clap_dim(I,4)
         YTOP2=Y+surf_clap_dim(I,3)
!       BOTTOM POINT
         XBOT=0.0D0
         YBOT=(-ldm%getClearApertureForLensDraw(I,1,dF))
         X=(XBOT*DCOS(GAM))-(YBOT*DSIN(GAM))
         Y=(YBOT*DCOS(GAM))+(XBOT*DSIN(GAM))
         XBOT=X+surf_clap_dim(I,4)
         YBOT=Y+surf_clap_dim(I,3)
         XBOT2=X+surf_clap_dim(I,4)
         YBOT2=Y+surf_clap_dim(I,3)
      ELSE
!     CAFLG NOT 2
      END IF
!       CAFLG=3,OR 4
      IF(CAFLG.EQ.3.OR.CAFLG.EQ.4) THEN
!
         GAM=(PII/180.0D0)*surf_clap_tilt(I)
!       RIGHT POINT
         XRHT=( ldm%getClearApertureForLensDraw(I,2,dF))
         YRHT=0.0D0
         X=(XRHT*DCOS(GAM))-(YRHT*DSIN(GAM))
         Y=(YRHT*DCOS(GAM))+(XRHT*DSIN(GAM))
         XRHT=X+surf_clap_dim(I,4)
         YRHT=Y+surf_clap_dim(I,3)
         XRHT2=X+surf_clap_dim(I,4)
         YRHT2=Y+surf_clap_dim(I,3)
!       LEFT POINT
         XLFT=(-ldm%getClearApertureForLensDraw(I,2,dF))
         YLFT=0.0D0
         X=(XLFT*DCOS(GAM))-(YLFT*DSIN(GAM))
         Y=(YLFT*DCOS(GAM))+(XLFT*DSIN(GAM))
         XLFT=X+surf_clap_dim(I,4)
         YLFT=Y+surf_clap_dim(I,3)
         XLFT2=X+surf_clap_dim(I,4)
         YLFT2=Y+surf_clap_dim(I,3)
!       TOP POINT
         XTOP=0.0D0
         YTOP=( ldm%getClearApertureForLensDraw(I,1,dF))
         X=(XTOP*DCOS(GAM))-(YTOP*DSIN(GAM))
         Y=(YTOP*DCOS(GAM))+(XTOP*DSIN(GAM))
         XTOP=X+surf_clap_dim(I,4)
         YTOP=Y+surf_clap_dim(I,3)
         XTOP2=X+surf_clap_dim(I,4)
         YTOP2=Y+surf_clap_dim(I,3)
!       BOTTOM POINT
         XBOT=0.0D0
         YBOT=(-ldm%getClearApertureForLensDraw(I,1,dF))
         X=(XBOT*DCOS(GAM))-(YBOT*DSIN(GAM))
         Y=(YBOT*DCOS(GAM))+(XBOT*DSIN(GAM))
         XBOT=X+surf_clap_dim(I,4)
         YBOT=Y+surf_clap_dim(I,3)
         XBOT2=X+surf_clap_dim(I,4)
         YBOT2=Y+surf_clap_dim(I,3)
      ELSE
!     CAFLG NOT 2,3 OR 4
      END IF
!       CAFLG=1 CIRCULAR CLAP
      IF(CAFLG.EQ.1) THEN
!
         PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,1,dF))
!
!     CLAP DEC MUST BE 0
         IF(surf_clap_dim(I,3).EQ.0.0D0.AND.surf_clap_dim(I,4).EQ.0.0D0) THEN
!
!     WE HAVE A CIRCULAR CLAP. IS THE SURFACE ONE WHICH COULD HAVE A
!     FLAT ON IT. ONLY IF SURFACE IS CONCAVE TO AIR.
!
!     IF SURFACE I IS NOT AIR AND I+1 IS AIR
!     THEN IF CV>0 THERE MAY BE A FLAT TO DO
!     AS LONG AS SPECIAL SURFACE 18 NOT PRESENT
!
!
            DOIT=.FALSE.
            DOIT=ISAIR(I,POSDIR)
            IF(DOIT) THEN
               IF(surf_curvature(I).GT.0.0D0.AND.POSDIR.OR.&
               &surf_curvature(I).LT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                  PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,2,dF))
                  IF(surf_curvature(I).GT.0.0D0) ZDELZ=-DABS(surf_clap_dim(I,5))
                  IF(surf_curvature(I).LT.0.0D0) ZDELZ=DABS(surf_clap_dim(I,5))
               END IF
            END IF
!
!     IF SURFACE I IS NOT AIR AND I+1 IS AIR
!     THEN IF CV<0 THERE MAY BE A FLAT TO DO
            DOIT=.FALSE.
            DOIT=ISAIR2(I,POSDIR)
            IF(DOIT) THEN
               IF(surf_curvature(I).LT.0.0D0.AND.POSDIR.OR.&
               &surf_curvature(I).GT.0.0D0.AND..NOT.POSDIR) THEN
!     ADJUST POINT
                  PEEPEE=DABS(ldm%getClearApertureForLensDraw(I,2,dF))
                  IF(surf_curvature(I).GT.0.0D0) ZDELZ=-DABS(surf_clap_dim(I,5))
                  IF(surf_curvature(I).LT.0.0D0) ZDELZ=DABS(surf_clap_dim(I,5))
               END IF
            END IF
         ELSE
!     DEC CLAP, NO FLAT CALC
         END IF
!
!       RIGHT POINT
         XRHT2=(PEEPEE)
         YRHT2=0.0D0
!       LEFT POINT
         XLFT2=(-PEEPEE)
         YLFT2=0.0D0
!       TOP POINT
         XTOP2=0.0D0
         YTOP2=(PEEPEE)
!       BOTTOM BOINT
         XBOT2=0.0D0
         YBOT2=(-PEEPEE)
      ELSE
!       CAFLG NOT 1
      END IF
!       COFLG=0 NO COBS
      IF(COFLG.EQ.0) THEN
         XLFTO=0.0D0
         YLFTO=0.0D0
         XRHTO=0.0D0
         YRHTO=0.0D0
         XBOTO=0.0D0
         YBOTO=0.0D0
         XTOPO=0.0D0
         YTOPO=0.0D0
         RETURN
      ELSE
!     MUST BE SOME COBS, CONTINUE
      END IF
!       COFLG=1 CIRCULAR COBS
      IF(COFLG.EQ.1) THEN
!       RIGHT POINT
         XRHTO=( ALENS(17,I))+ALENS(20,I)
         YRHTO=0.0D0
!       LEFT POINT
         XLFTO=(-ALENS(17,I))+ALENS(20,I)
         YLFTO=0.0D0
!       TOP POINT
         XTOPO=0.0D0
         YTOPO=( ALENS(17,I))+ALENS(19,I)
!       BOTTOM BOINT
         XBOTO=0.0D0
         YBOTO=(-ALENS(17,I))+ALENS(19,I)
      ELSE
!       COFLG NOT 1
      END IF
!
!       COFLG=2,3,OR 4
      IF(COFLG.EQ.2.OR.COFLG.EQ.3.OR.COFLG.EQ.4) THEN
!
         GAM=(PII/180.0D0)*ALENS(22,I)
!       RIGHT POINT
         XRHTO=( ALENS(18,I))
         YRHTO=0.0D0
         X=(XRHTO*DCOS(GAM))-(YRHTO*DSIN(GAM))
         Y=(YRHTO*DCOS(GAM))+(XRHTO*DSIN(GAM))
         XRHTO=X+ALENS(20,I)
         YRHTO=Y+ALENS(19,I)
!       LEFT POINT
         XLFTO=(-ALENS(18,I))
         YLFTO=0.0D0
         X=(XLFTO*DCOS(GAM))-(YLFTO*DSIN(GAM))
         Y=(YLFTO*DCOS(GAM))+(XLFTO*DSIN(GAM))
         XLFTO=X+ALENS(20,I)
         YLFTO=Y+ALENS(19,I)
!       TOP POINT
         XTOPO=0.0D0
         YTOPO=( ALENS(17,I))
         X=(XTOPO*DCOS(GAM))-(YTOPO*DSIN(GAM))
         Y=(YTOPO*DCOS(GAM))+(XTOPO*DSIN(GAM))
         XTOPO=X+ALENS(20,I)
         YTOPO=Y+ALENS(19,I)
!       BOTTOM POINT
         XBOTO=0.0D0
         YBOTO=(-ALENS(17,I))
         X=(XBOTO*DCOS(GAM))-(YBOTO*DSIN(GAM))
         Y=(YBOTO*DCOS(GAM))+(XBOTO*DSIN(GAM))
         XBOTO=X+ALENS(20,I)
         YBOTO=Y+ALENS(19,I)
      ELSE
!     COFLG NOT 2,3 OR 4
      END IF
!     NOW FOR SOME SURFACE TYPES, THERE IS A PREDICTABLE
!     EXTENT BEYOND WHICH THE SAG OF THE SURFACE WILL NOT
!     BE A REAL NUMBER. IF THE LIMITS CALCULATED ABOVE EXCEED
!     THESE LIMITS, THEN ADJUST THE LIMITS BEFORE PROCEEDING.
   ELSE
!     TYPE 18 SPECIAL SURFACE AND IT IS "ON"
!     CEE IS THE CURVATURE CV (ALENS(1,I))
      CEE=ALENS(1,I)
!     KAPPA IS THE CONIC CONSTANT CC (ALENS(2,I))
      KAPPA=ALENS(2,I)
!
!     ZEE IS THE Z POSITION FOR WHICH A RHO IS DESIRED
!     FIRST THE FORE OR FRONT POSITION
      ZEE=FTFL01(1,I)
      RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
      IF(RHO2.LT.0.0D0) THEN
         OUTLYNE=&
         &'UN-REALISTIC FRONT Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='NO PROFILE COULD BE DRAWN'
         CALL SHOWIT(1)
         XLFT=0.0D0
         YLFT=0.0D0
         XRHT=0.0D0
         YRHT=0.0D0
         XTOP=0.0D0
         YTOP=0.0D0
         XBOT=0.0D0
         YBOT=0.0D0
         XLFTO=0.0D0
         YLFTO=0.0D0
         XRHTO=0.0D0
         YRHTO=0.0D0
         XTOPO=0.0D0
         YTOPO=0.0D0
         XBOTO=0.0D0
         YBOTO=0.0D0
         RETURN
      END IF
      RHO=DSQRT(RHO2)
      XLFT=-RHO
      YLFT=0.0D0
      XRHT=RHO
      YRHT=0.0D0
      XTOP=0.0D0
      YTOP=RHO
      XBOT=0.0D0
      YBOT=-RHO
!     THEN THE AFT OR BACK POSITION
      ZEE=FTFL01(2,I)
      RHO2=(((2.0D0*ZEE)-(CEE*(KAPPA+1.0D0)*(ZEE**2)))/CEE)
      IF(RHO2.LT.0.0D0) THEN
         OUTLYNE=&
         &'UN-REALISTIC REAR Z-POSITION IN TYPE 18 SPECIAL SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='NO PROFILE COULD BE DRAWN'
         CALL SHOWIT(1)
         XLFT=0.0D0
         YLFT=0.0D0
         XRHT=0.0D0
         YRHT=0.0D0
         XTOP=0.0D0
         YTOP=0.0D0
         XBOT=0.0D0
         YBOT=0.0D0
         XLFTO=0.0D0
         YLFTO=0.0D0
         XRHTO=0.0D0
         YRHTO=0.0D0
         XTOPO=0.0D0
         YTOPO=0.0D0
         XBOTO=0.0D0
         YBOTO=0.0D0
         RETURN
      END IF
      RHO=DSQRT(RHO2)
      XLFTO=-RHO
      YLFTO=0.0D0
      XRHTO=RHO
      YRHTO=0.0D0
      XTOPO=0.0D0
      YTOPO=RHO
      XBOTO=0.0D0
      YBOTO=-RHO
   END IF
   RETURN
END
! SUB CAO5.FOR
!
SUBROUTINE CAO5(X1,X2,X3,X4,Y1,Y2,Y3,Y4,I)
!
!     DOES EDGES WHEN I AND I+1 HAVE RECTANGULAR CLAPS
!
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I
!
   real(real64) X,Y,X1,X2,X3,X4,Y1,Y2,Y3,Y4 &
   &,GAM

!
!
!       SET FLAGS CAFLG AND COFLG. CAFLG2 TO SUPPORT RECT CLAPS ON I AND I+1
   GAM=0.0D0
!
   GAM=(PII/180.0D0)*ALENS(15,I)
!       X1,Y1
   X1=ALENS(11,I)
   Y1=ALENS(10,I)
   X=(X1*DCOS(GAM))-(Y1*DSIN(GAM))
   Y=(Y1*DCOS(GAM))+(X1*DSIN(GAM))
   X1=X+ALENS(13,I)
   Y1=Y+ALENS(12,I)
!       X2,Y2
   X2=-ALENS(11,I)
   Y2=ALENS(10,I)
   X=(X2*DCOS(GAM))-(Y2*DSIN(GAM))
   Y=(Y2*DCOS(GAM))+(X2*DSIN(GAM))
   X2=X+ALENS(13,I)
   Y2=Y+ALENS(12,I)
!       X3,Y3
   X3=-ALENS(11,I)
   Y3=-ALENS(10,I)
   X=(X3*DCOS(GAM))-(Y3*DSIN(GAM))
   Y=(Y3*DCOS(GAM))+(X3*DSIN(GAM))
   X3=X+ALENS(13,I)
   Y3=Y+ALENS(12,I)
!       X4,Y4
   X4=ALENS(11,I)
   Y4=-ALENS(10,I)
   X=(X4*DCOS(GAM))-(Y4*DSIN(GAM))
   Y=(Y4*DCOS(GAM))+(X4*DSIN(GAM))
   X4=X+ALENS(13,I)
   Y4=Y+ALENS(12,I)
   RETURN
END
! SUB PLTCLP.FOR
SUBROUTINE PLTCLP(CLPTYPE,SURFACEI,SFI,MDX,MDY,GAMGAM)
   USE GLOBALS
!
   use DATLEN
   use mod_system, only: sys_last_surf
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DOES THE PLOT CLAP COMMAND
!
   real(real64) X,Y,Z,XN,YN,ZN,ROT1X,ROT1Z,ROT2Y,MDX,MDY,GAMGAM &
   &,ROT2Z,AX,AY,AZ,AALF,APHI,YMAXI,YMINI,XMAXI,XMINI &
   &,XNEW,YNEW,LKG,VIEPH,VIEAL,Z1,ANGLE,AN2,ZDELZ &
   &,X00,Y00,Z0,LX0,LY0,LZ0,ZCORR,SFI &
   &,X1,Y1,MX0,MY0,MZ0,NX0,NY0,NZ0,XID,YID
!
   INTEGER KKK,JJSTOP,J,IK,III,NO,CLRR,JJ,ALLOERR,IIRUN,CLPTYPE
!
   INTEGER M1,M2,M3,M4,M5,IX,IY,I,II,IPST,K,L,POINT(1:2,1:3)
!
   INTEGER SURFACEI
!
   INTEGER COLPAS
!
   LOGICAL SECPLT(0:499),NOPLOT
!
!
   REAL CLPDAT
   DIMENSION CLPDAT(:,:,:,:)
   ALLOCATABLE :: CLPDAT
!
!     LOOK.VIEW TRANSFORMS

   ROT1X(AX,AZ,APHI)=((AX*DCOS(APHI))-(AZ*DSIN(APHI)))
   ROT1Z(AX,AZ,APHI)=((AX*DSIN(APHI))+(AZ*DCOS(APHI)))
!
   ROT2Z(AZ,AY,AALF)=((AZ*DCOS(AALF))+(AY*DSIN(AALF)))
   ROT2Y(AZ,AY,AALF)=((-AZ*DSIN(AALF))+(AY*DCOS(AALF)))
!
   II=SURFACEI
   NOPLOT=.FALSE.
   IF(DF5.EQ.0) NOPLOT=.TRUE.
   PRINT *, "PLTCAP Routine Starting..."
!
   DO KKK=1,2
!         KKK IS USED TO PERFORM MIRROR BACKING PLOTS WHEN KKK=2
      M1=0
      M2=360
      M3=3
      M4=INT(sys_last_surf())
      M5=2
      DEALLOCATE(CLPDAT,STAT=ALLOERR)
      ALLOCATE(CLPDAT(M1:M2,M3,M1:M4,M5),STAT=ALLOERR)
!
      VIEPH=(PII/180.0D0)*VIEPHI
      VIEAL=(PII/180.0D0)*VIEALF
!
!     MAKE SURE YOU HAVE VERTEX DATA WITHOUT OFFSETS AND SET TO
!     THE FIRST SURFACE WITHOUT AN INFINITE THICKNESS
      GLSURF=-99
      DO I=NEWIMG,0,-1
         IF(DABS(ALENS(3,I)).LE.1.0D10) GLSURF=I
      END DO
      IF(GLSURF.EQ.-99) THEN
         GLOBE=.FALSE.
         CALL REPORT_ERROR_AND_FAIL(&
         & 'ALL SURFACES WERE OF INFINITE THICKNESS'//'\n'//&
         & 'NO OPTICAL SYSTEM PLOT COULD BE MADE', 1)
         DEALLOCATE(CLPDAT,STAT=ALLOERR)
         RETURN
      END IF
      GLOBE=.TRUE.
      OFFX=0.0D0
      OFFY=0.0D0
      OFFZ=0.0D0
      OFFA=0.0D0
      OFFB=0.0D0
      OFFC=0.0D0
      CALL GLVERT
      GLOBE=.FALSE.
!
      DO IIRUN=1,2
         CLPDAT(0:360,1:3,0:M4,1:2)=0.0
         X=0.0D0
         Y=0.0D0
         XID=0.0D0
         YID=0.0D0
!
!       ALL INPUT IS OK, KEEP GOING
!     THE ARRAY CONTAINING SURFACE CLAP DATA IS:
!     CLPDAT(0:360,1:3,0:MAXSUR,1:2)
!
!     THE FIRST DIMENSION IS FOR THE DATA POINT NUMBER
!     THE SECOND DIMENSION IS FOR THE X,Y AND Z COORDINATES OR THE POINT
!     THE THIRD IS THE SURFACE NUMBER
!
!     WE NEED TO LOAD THE ARRAY BEFORE PLOTTING
!
!     THE PROCEDURE IS:
!
!     CYCLE THROUGH ALL THE SURFACES
!
         IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
         IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
         DO JJ=1,JJSTOP
!
!     1. WE WILL CLOCK AROUND THE CLEAR APERTURE FROM THE LOCAL +X
!     TOWARD THE LOCAL +Y AXIS,
!     4.0 DEGREE INCREMENTS AS MEASURED
!     BY AN OBSERVER AT THE SURFACE VERTEX, IN THE LOCAL COORDINATE
!     SYSTEM OF THE SURFACE, WITH THE OBSERVER FACING THE -Z AXIS
!     DIRECTION
!
            DO J=0,360
               ANGLE=(DBLE(J)*PII)/180.0D0
               AN2=(DBLE(J+1)*PII)/180.0D0
!     NOW ALONG THIS ANGLED LINE, WHAT ARE THE X AND Y COORDINATES
!     OF THE CLEAR APERTURE
               III=II
!
               CALL CAO1(X,Y,ANGLE,III,AN2,JJ,XID,YID,IIRUN,CLPTYPE,ZDELZ &
               &,MDX,MDY,GAMGAM)
!
!     THE RETURNED X AND Y ARE WHERE THE SAG IS TO BE CALCULATED
!
!     2. USE APPROPRIATE CALLS TO THE SAGPLT.FOR ROUTINE
!               TO CALCULATE THE SAG AND MAKE
!               CERTAIN THE SIGN IS CORRECT FOR A LOCAL Z COORDINATE
!
!
!     CALLS TO SAGPLT GO HERE
               III=II
               XID=XID*SFI
               YID=YID*SFI
               CALL SAGPLT(III,XID,YID,Z,NO)
               IF(CLPTYPE.EQ.1) Z=Z+ZDELZ
!
!     ASSIGN ARRAY VALUES BASED ON J VALUE
               CLPDAT(J,1,II,JJ)=X*SFI
               CLPDAT(J,2,II,JJ)=Y*SFI
!
               SECPLT(II)=.FALSE.
               IF(KKK.EQ.2) THEN
                  IF(ALENS(110,II).NE.0.0D0) THEN
                     ZCORR=DABS(ALENS(110,II))
                     IF(ALENS(46,II).LT.0.0D0) Z=Z+ZCORR
                     IF(ALENS(46,II).GT.0.0D0) Z=Z-ZCORR
                  END IF
                  SECPLT(II)=.TRUE.
               ELSE
                  SECPLT(II)=.FALSE.
               END IF
               CLPDAT(J,3,II,JJ)=Z
!
!               CYCLE THROUGH THE NEXT DATA PAIR
            END DO
!               CYCLE THROUGH THE NEXT JJ
         END DO
!
!     3. THE ARRAYS NOW HAVE LOCAL X,Y AND Z VALUES STORED IN THEM
!     CONVERT THE LOCAL X ANY Y CLAPS TO GLOBAL NUMBERS
!     GLOBAL VERTEX DATA IS
         IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
         IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
         DO JJ=1,JJSTOP
            DO I=0,360
               X00=VERTEX(1,II)
               Y00=VERTEX(2,II)
               Z0=VERTEX(3,II)
               LX0=VERTEX(4,II)
               MX0=VERTEX(5,II)
               NX0=VERTEX(6,II)
               LY0=VERTEX(7,II)
               MY0=VERTEX(8,II)
               NY0=VERTEX(9,II)
               LZ0=VERTEX(10,II)
               MZ0=VERTEX(11,II)
               NZ0=VERTEX(12,II)
               X=CLPDAT(I,1,II,JJ)
               Y=CLPDAT(I,2,II,JJ)
               Z=CLPDAT(I,3,II,JJ)
!
               X1=X00+((LX0*(X))+(LY0*(Y))&
               &+(LZ0*(Z)))
               Y1=Y00+((MX0*(X))+(MY0*(Y))&
               &+(MZ0*(Z)))
               Z1=Z0+((NX0*(X))+(NY0*(Y))&
               &+(NZ0*(Z)))
               CLPDAT(I,1,II,JJ)=X1
               CLPDAT(I,2,II,JJ)=Y1
               CLPDAT(I,3,II,JJ)=Z1
            END DO
         END DO
!
!     4. NOW DETERMINE THE BEST PLACE FOR THE ROTATION POINT FOR
!               PLOT LOOK/VIEW
!
!     ROT 3 IS LIKE ROT2 BUT KEYS OFF CLAP DATA NOT PROF DATA
         CALL ROT3(JJ,CLPDAT,M1,M2,M3,M4,M5)
!
!     5.  CONVERT THE GLOBAL X AND Y CLAP VALUES
!               USING THE LOOK/VIEW VALUES
         IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
         IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
         DO JJ=1,JJSTOP
            DO I=0,360
               X=CLPDAT(I,1,II,JJ)
               Y=CLPDAT(I,2,II,JJ)
               Z=CLPDAT(I,3,II,JJ)
               X=X-XROT
               Y=Y-YROT
               Z=Z-ZROT
               XN=ROT1X(X,Z,VIEPH)
               YN=Y
               ZN=ROT1Z(X,Z,VIEPH)
               X=XN
               Y=YN
               Z=ZN
!
               ZN=ROT2Z(Z,Y,VIEAL)
               YN=ROT2Y(Z,Y,VIEAL)
               XN=X
               CLPDAT(I,1,II,JJ)=XN
               CLPDAT(I,2,II,JJ)=YN
               CLPDAT(I,3,II,JJ)=ZN
            END DO
         END DO
!
!     THE ARRAYS NOW HAVE GLOBAL SURFACE CLAP DATA IN THEM
!
!     6.IF NEEDED, DETERMINE SCALE FACTORS AND PLOT RANGE
!
!     PLTSC3 IS LIKE PLTSC2 BUT KEYS OFF CLAP DATA NOT PROF DATA
!
         CALL PLTSC3(XMINI,XMAXI,YMINI,YMAXI,JJ,CLPDAT,M1,M2,M3,M4,M5)
!
!     7.CYCLE THROUGH THE THE ARRAYS, APPLY SCALE FACTORS
!
!     RIGHT NOW,COORDINATES ARE IN WORLD COORDINATES
!     CONVERT THEM TO DEVICE INDEPENDENT GRAPHICS COORDINATES IN
!     TWO STEPS.
!
!     THE WORLD X PLOTS TO THE PLOTTER X
!     THE WORLD Y PLOTS TO THE PLOTTER Y
!
!     STEP 1: CONVERT USING AN APPROPRIATE SCALE FACTOR
!               CALCULATING AN APPROPRIATE FACTOR IF NECESSARY
!
         IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
         IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
         DO JJ=1,JJSTOP
            DO I=0,360
               CLPDAT(I,1,II,JJ)=(CLPDAT(I,1,II,JJ)/SCFAX)*1000.0D0
               CLPDAT(I,2,II,JJ)=(CLPDAT(I,2,II,JJ)/SCFAY)*1000.0D0
            END DO
         END DO
!
!     8. APPLY THE XSHIFT AND YSHIFT VALUES
         DO I=0,360
            IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
            IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
            DO JJ=1,JJSTOP
               IF(LORIENT) CALL ORSHIFT
               CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+DBLE(PXSHFT)
               CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)+3500.0D0+DBLE(PYSHFT)
            END DO
         END DO
!
!     9. SET THE PLOT JUSTIFICATION IF NEEDED
!     IF THERE ARE X-OFFSETS (JUSTIFICATION) TO APPLY, DO THEM HERE.
!
!     NOW
         IF(RCL.EQ.1.OR.RCL.EQ.-1) THEN
            JUSOFF=500.0D0-((XMINI/SCFAX)*1000.0D0)
            RCL=-1
         ELSE
         END IF
         IF(RCL.EQ.2.OR.RCL.EQ.-2) THEN
            RCL=-2
            JUSOFF=5000.0D0
         ELSE
         END IF
         IF(RCL.EQ.3.OR.RCL.EQ.-3) THEN
            JUSOFF=9500.0D0-((XMAXI/SCFAX)*1000.0D0)
            RCL=-3
         ELSE
         END IF
!
         DO I=0,360
            IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
            IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
            DO JJ=1,JJSTOP
               CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+JUSOFF
            END DO
         END DO
!     9. PLOT GAMMA
!     IF THERE IS A NON-ZERO GAMMA SPECIFIED IN PLOT LOOK OR PLOT VIEW THEN
!     ROTATE IN GAMMA ON THE SCREEN ABOUT THE CENTER OF THE SCREEN
!     WHICH HAS COORDINATES X=5000.0D0,Y=3500.0D0
!
!     FIRST SHIFT THE COORDINATE ORIGIN TO THE CENTER OF THE DISPLAY
!
         DO I=0,360
            IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
            IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
            DO JJ=1,JJSTOP
               CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)-5000.0D0
               CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)-3500.0D0
            END DO
         END DO
!     THE SCREEN COORDINATE IN real(real64) IN THE SHIFTED COORDINATE
!     FRAME IS NOW X AND Y

         IF(DBLE(PGAMMA).NE.0.0D0) THEN
            LKG=(PII/180.0D0)*DBLE(PGAMMA)

            DO I=0,360
               IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
               IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
               DO JJ=1,JJSTOP
                  X=CLPDAT(I,1,II,JJ)
                  Y=CLPDAT(I,2,II,JJ)
                  XNEW=((X*DCOS(LKG))-(Y*DSIN(LKG)))
                  YNEW=((X*DSIN(LKG))+(Y*DCOS(LKG)))
                  CLPDAT(I,1,II,JJ)=XNEW
                  CLPDAT(I,2,II,JJ)=YNEW
               END DO
            END DO
         END IF
!     THE ROTATION IS DONE, NOW SHIFT THE ORIGIN BACK TO THE BOTTOM
!     LEFT HAND CORNER
         DO I=0,360
            IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
            IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
            DO JJ=1,JJSTOP
               CLPDAT(I,1,II,JJ)=CLPDAT(I,1,II,JJ)+5000.0D0
               CLPDAT(I,2,II,JJ)=CLPDAT(I,2,II,JJ)+3500.0D0
            END DO
         END DO
!
         IF(ALENS(34,II).EQ.18.0D0) JJSTOP=2
         IF(ALENS(34,II).NE.18.0D0) JJSTOP=1
         DO JJ=1,JJSTOP
!     NOW DRAW THE CLAP AT SURFACE I
!     WITH THE PEN UP, GO TO THE STARTING PLOT POSITION
            IF(.NOT.PLEXIS) PLEXIS=.TRUE.
            DO J=0,360
!     PUT INSTRUCTIONS IN P1ARAY TO DROP PEN AND DRAW
               IF(J.EQ.0) IPST=0
               IF(J.NE.0) IPST=1
               IF(CLPDAT(J,1,II,JJ).GT.1.0D6) CLPDAT(J,1,II,JJ)=1.0D6
               IF(CLPDAT(J,2,II,JJ).GT.1.0D6) CLPDAT(J,2,II,JJ)=1.0D6
               IF(CLPDAT(J,1,II,JJ).LT.-1.0D6) CLPDAT(J,1,II,JJ)=-1.0D6
               IF(CLPDAT(J,2,II,JJ).LT.-1.0D6) CLPDAT(J,2,II,JJ)=-1.0D6
               IX=INT(CLPDAT(J,1,II,JJ))
               IY=INT(CLPDAT(J,2,II,JJ))
               P1ARAY(J,1,1)=IX
               P1ARAY(J,2,1)=IY
               P1ARAY(J,3,1)=IPST
               IF(.NOT.PLEXIS) PLEXIS=.TRUE.
            END DO
!     FINISHED WITH THAT CLAP, LIFT PEN
            IPST=0
!     NOW ISSUE THE PLOTTING COMMANDS STORED IN THE P1ARAY ARRAY
!
!     LINE TYPE SETTING
            COLPAS=COLCLP
            IF(SURFACEI.EQ.HILITE_SURF) COLPAS=HILITE_COLOR
            CALL MY_COLTYP(COLPAS)
            OLLNTP=LNTYPE
            LNTYPE=0
!     DASH, SOLID OR INVISIBLE
            CLRR=0
            IF(DUMMMY(II).AND.ALENS(9,II).EQ.0.0D0) CLRR=-1
            IF(DUMMMY(II).AND.ALENS(9,II).NE.0.0D0) THEN
               IF(DASHH) LNTYPE=2
            ELSE
!     LEAVE LINE ALONE
            END IF
            IF(CLRR.NE.-1) THEN
               FIXUP=.FALSE.
!
               DO IK=0,360
                  IF(IK.EQ.0) P1ARAY(IK,3,1)=0
                  IF(IK.GT.0) THEN
                     IF(P1ARAY(IK-1,1,1).LE.0.OR.P1ARAY(IK-1,2,1).LE.0) THEN
                        P1ARAY(IK,3,1)=0
                     END IF
                     IF(P1ARAY(IK-1,1,1).GE.10000.OR.P1ARAY(IK-1,2,1).GE.7000) THEN
                        P1ARAY(IK,3,1)=0
                     END IF
                     IF(P1ARAY(IK,1,1).LE.0.OR.P1ARAY(IK,2,1).LE.0) THEN
                        P1ARAY(IK,3,1)=0
                     END IF
                     IF(P1ARAY(IK,1,1).GE.10000.OR.P1ARAY(IK,2,1).GE.7000) THEN
                        P1ARAY(IK,3,1)=0
                     END IF
                  END IF
                  IF(KKK.EQ.1.AND.IK.EQ.0) THEN
                     POINT(1,1)=P1ARAY(IK,1,1)
                     POINT(1,2)=P1ARAY(IK,2,1)
                     POINT(1,3)=P1ARAY(IK,3,1)
                  END IF
                  IF(KKK.EQ.1.AND.IK.EQ.360) THEN
                     POINT(2,1)=P1ARAY(IK,1,1)
                     POINT(2,2)=P1ARAY(IK,2,1)
                     POINT(2,3)=P1ARAY(IK,3,1)
                  END IF
                  IF(KKK.EQ.2.AND.SECPLT(II)) THEN
                     IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,II).NE.0.0D0)&
                     &CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                  END IF
                  IF(KKK.EQ.1) THEN
                     IF(.NOT.NOPLOT.OR.NOPLOT.AND.ALENS(9,II).NE.0.0D0)&
                     &CALL PENMV1(P1ARAY(IK,1,1),P1ARAY(IK,2,1),P1ARAY(IK,3,1))
                  END IF
               END DO
            ELSE
               CLRR=0
            END IF
            LNTYPE=OLLNTP
         END DO

!     THIS LAST END DO ID FOR THE IIRUN LOOP
      END DO
!     DO THE NEXT KKK LOOP FOR MIRROR BACKS
   END DO
!
!     HERE WE DO THE PLOT LI AND PLOT AXIS DRAWING
   IF(.NOT.VIGFLG.AND.PLTVIG) THEN
      CALL VIGSHO
      VIGFLG=.TRUE.
   ELSE
   END IF
   DEALLOCATE(CLPDAT,STAT=ALLOERR)
   LNTYPE=0
   RETURN
END
FUNCTION ISITIN(X,Y,I)
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) GAM,ANGLE,XR,YR,X,Y,XRD,YRD
   LOGICAL INS,INSID1,ISITIN
   EXTERNAL INSID1
   INTEGER III,II,I
   ANGLE=0.0D0
   DO III=1,INT(ALENS(11,I))
      XT(III)=ALENS(10,I)*DCOS(ANGLE+(PII/2.0D0))
      YT(III)=ALENS(10,I)*DSIN(ANGLE+(PII/2.0D0))
      ANGLE=ANGLE+((TWOPII)/ALENS(11,I))
   END DO
   XRD=X
   YRD=Y
!
   GAM=(PII/180.0D0)*ALENS(15,I)
   XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
   YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
!
!        XR=XR-(ALENS(13,I)+MULTX)
!        YR=YR-(ALENS(12,I)+MULTY)
!
!       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
   X0=XR
   Y0=YR
   NP=INT(ALENS(11,I))
   INS=INSID1()
   IF(INS) THEN
      ISITIN=.TRUE.
   ELSE
      ISITIN=.FALSE.
   END IF
   RETURN
END
FUNCTION ISITIN2(X,Y,I)
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) GAM,ANGLE,XR,YR,X,Y,XRD,YRD
   LOGICAL INS,INSID2,ISITIN2
   EXTERNAL INSID2
   INTEGER III,II,I
   ANGLE=0.0D0
   DO III=1,INT(ALENS(18,I))
      XT(III)=ALENS(17,I)*DCOS(ANGLE+(PII/2.0D0))
      YT(III)=ALENS(17,I)*DSIN(ANGLE+(PII/2.0D0))
      ANGLE=ANGLE+((TWOPII)/ALENS(18,I))
   END DO
   XRD=X
   YRD=Y
!
   GAM=(PII/180.0D0)*ALENS(22,I)
   XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
   YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
!
   XR=XR-ALENS(20,I)
   YR=YR-ALENS(19,I)
!
!       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
   X0=XR
   Y0=YR
   NP=INT(ALENS(18,I))
   INS=INSID2()
   IF(INS) THEN
      ISITIN2=.TRUE.
   ELSE
      ISITIN2=.FALSE.
   END IF
   RETURN
END
FUNCTION ISITINI(X,Y,I)
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) GAM,ANGLE,XR,YR,X,Y,XRD,YRD
   LOGICAL INS,INSID1,ISITINI
   EXTERNAL INSID1
   INTEGER III,II,I
   DO III=1,INT(ALENS(11,I))
      XT(III)=IPOLYX(III,I,1)
      YT(III)=IPOLYY(III,I,1)
   END DO
   XRD=X
   YRD=Y
!
   GAM=(PII/180.0D0)*ALENS(15,I)
   XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
   YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
!
!
!       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
   X0=XR
   Y0=YR
   NP=INT(ALENS(11,I))
   INS=INSID1()
   IF(INS) THEN
      ISITINI=.TRUE.
   ELSE
      ISITINI=.FALSE.
   END IF
   RETURN
END
FUNCTION ISITIN2I(X,Y,I)
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) GAM,ANGLE,XR,YR,X,Y,XRD,YRD
   LOGICAL INS,INSID2,ISITIN2I
   EXTERNAL INSID2
   INTEGER III,II,I
   DO III=1,INT(ALENS(18,I))
      XT(III)=IPOLYX(III,I,3)
      YT(III)=IPOLYY(III,I,3)
   END DO
   XRD=X
   YRD=Y
!
   GAM=(PII/180.0D0)*ALENS(22,I)
   XR=(XRD*DCOS(GAM))+(YRD*DSIN(GAM))
   YR=(YRD*DCOS(GAM))-(XRD*DSIN(GAM))
!
   XR=XR-ALENS(20,I)
   YR=YR-ALENS(19,I)
!
!       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
   X0=XR
   Y0=YR
   NP=INT(ALENS(18,I))
   INS=INSID2()
   IF(INS) THEN
      ISITIN2I=.TRUE.
   ELSE
      ISITIN2I=.FALSE.
   END IF
   RETURN
END
