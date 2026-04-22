!       SEVENTH SET OF OPTIMIZATION ROUTINES

! SUB TOPOUT.FOR
SUBROUTINE TOPOUT
!
   use DATCFG
   use DATSUB
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I,CFGCHK,II,TAGER
!
   LOGICAL DESYES,ALLER
!
   LOGICAL YES
!
   CHARACTER CNAM*3,FNAMER*6,DASHER*3
!
!
!
   ALLER=.TRUE.
   TAGER=0
!
   DASHER='---'
!
   FUNNAM(0)='FUNC00'
   FUNNAM(1)='FUNC01'
   FUNNAM(2)='FUNC02'
   FUNNAM(3)='FUNC03'
   FUNNAM(4)='FUNC04'
   FUNNAM(5)='FUNC05'
   FUNNAM(6)='FUNC06'
   FUNNAM(7)='FUNC07'
   FUNNAM(8)='FUNC08'
   FUNNAM(9)='FUNC09'
   FUNNAM(10)='FUNC10'
!
!       THIS IS SUBROUTINE TOPOUT. THIS IS THE SUBROUTINE WHICH DOES OPERAND
!       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE MERIT AND
!       UPDATE MERIT
   IF(WC.EQ.'TOPS') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"TOPS" TAKES NO QUALIFIER OR STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TOPS') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TOPS" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.0) THEN
      TAGER=INT(W1)
      ALLER=.FALSE.
   END IF
   IF(DF1.EQ.1) THEN
      ALLER=.TRUE.
   END IF
!     NOW TAGER AND ALLER ARE PROBERLY SET
!
   IF(DF1.EQ.0) THEN
!     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
!     AND PERFORM THE OPERATION
      IF(W1.LE.0.0D0.OR.W1.GE.MAXTOP) THEN
         WRITE(OUTLYNE,*)'TOPER NUMBER BEYOND DEFINED BOUNDS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
!     W1 IS A VALID NUMBER, PROCEED
      END IF
      IF(.NOT.ISTOP(INT(W1))) THEN
         WRITE(OUTLYNE,*)'TOPER NUMBER NOT CURRENTLY DEFINED'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
!     W1 IS A VALID NUMBER, PROCEED
      END IF
!
!     HERE DO THE OUTPUT FOR OPERAND NUMBER INT(W1)
!       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
      YES=.FALSE.
      DO I=1,MAXTOP
         IF(ISTOP(I)) YES=.TRUE.
      END DO
      IF(.NOT.YES) TOPCNT=0
      IF(YES) TOPCNT=1
      IF(TOPCNT.EQ.0) THEN
         WRITE(OUTLYNE,*)'NO TOPER DATA EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(.NOT.ISTOP(INT(W1))) THEN
         WRITE(OUTLYNE,*)'TOPER DATA NOT DEFINED FOR TOPER #',INT(W1)
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'NO ACTION TAKEN'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WC.EQ.'TOPS') THEN
!     DO TOPS HEADER PRINTING
!     PRINT CRIT DATA
100      FORMAT(&
         &'CURRENT TOLERANCE OPERAND DATA FOR TOPER #',I2)
         WRITE(OUTLYNE,100) INT(W1)
         CALL SHOWIT(0)
         II=INT(W1)+MAXFOCRIT
!
101      FORMAT(&
         &'TOPER # ',2X,'TOPER NAME ',6X,&
         &' NW2 ',8X,' NW3 ',8X,' NW4 ',8X,' NW5')
401      FORMAT(&
         &27X,&
         &' (I) ',8X,' (J) ',8X,' (K) ')
         WRITE(OUTLYNE,101)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
!
!     NO DEFAULT
102      FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,G11.4,2X,G11.4)
!
!     JUST K DEFAULT
103      FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,&
         &3X,'-----',5X,G11.4)
!
!     J AND K DEFAULT
104      FORMAT(I3,11X,A8,2X,G11.4,2X,3X,&
         &'-----',3X,2X,3X,'-----',5X,G11.4)
!
!     I,J AND K DEFAULT
105      FORMAT(I3,11X,A8,2X,3X,'-----',3X,2X,&
         &3X,'-----',3X,2X,3X,'-----',5X,G11.4)

!     JUST J DEFAULT
106      FORMAT(I3,11X,A8,2X,G11.4,2X,3X,&
         &'-----',3X,2X,G11.4,2X,G11.4)

!     JUST I DEFAULT
107      FORMAT(I3,11X,A8,2X,3X,'-----',3X,&
         &2X,G11.4,2X,G11.4,2X,G11.4)

!     I AND K DEFAULT
108      FORMAT(I3,11X,A8,2X,G11.4,2X,3X,&
         &'-----',3X,2X,3X,'-----',5X,G11.4)

!     I AND J DEFAULT
109      FORMAT(I3,11X,A8,2X,3X,'-----',3X,&
         &2X,G11.4,2X,3X,'-----',5X,G11.4)
!
!     NO DEFAULTS
         IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
         &.EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
            WRITE(OUTLYNE,102) INT(W1),OPNAM(II),&
            &OPERND(II,8),OPERND(II,9),&
            &OPERND(II,10),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!
!     K DEFAULT
         IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
         &.EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
            WRITE(OUTLYNE,103) INT(W1),OPNAM(II),&
            &OPERND(II,8),OPERND(II,9),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!
!     J AND K DEFAULTS
         IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
         &.EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
            WRITE(OUTLYNE,104) INT(W1),OPNAM(II),&
            &OPERND(II,8),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!
!     I J K DEFAULTS
         IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
         &.EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
            WRITE(OUTLYNE,105) INT(W1),OPNAM(II),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!
!     J DEFAULT
         IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
         &.EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
            WRITE(OUTLYNE,106) INT(W1),OPNAM(II),&
            &OPERND(II,8),&
            &OPERND(II,10),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!
!     I DEFAULT
         IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
         &.EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
            WRITE(OUTLYNE,107) INT(W1),OPNAM(II),&
            &OPERND(II,9),&
            &OPERND(II,10),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!     I AND K DEFAULT
         IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
         &.EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
            WRITE(OUTLYNE,108) INT(W1),OPNAM(II),&
            &OPERND(II,9),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
!     I AND J DEFAULT
         IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
         &.EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
            WRITE(OUTLYNE,109) INT(W1),OPNAM(II),&
            &OPERND(II,10),OPERND(II,20)
            CALL SHOWIT(0)
         END IF
         IF(OPERDESC(II)(1:8).NE.'        ')&
         &WRITE(OUTLYNE,1701) OPNAM(II),OPERDESC(II)(1:69)
         CALL SHOWIT(0)
1701     FORMAT(A8,'::',A69)
         RETURN
      END IF
   ELSE
!     DF1=1, NO NUMERIC INPUT, DO ALL OUTPUT
!     HERE DO THE MERIT OUTPUT FOR ALL OPERANDS
!       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
      YES=.FALSE.
      DO I=1,MAXTOP
         IF(ISTOP(I)) YES=.TRUE.
      END DO
      IF(.NOT.YES) TOPCNT=0
      IF(YES) TOPCNT=1
      IF(TOPCNT.EQ.0) THEN
         WRITE(OUTLYNE,*)'NO TOPER DATA EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WC.EQ.'TOPS') THEN
         WRITE(OUTLYNE,400)
         CALL SHOWIT(0)
400      FORMAT(&
         &'CURRENT TOPER DATA')
         WRITE(OUTLYNE,101)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
!     DO MERIT HEADER PRINTING
         DO I=1,MAXTOP
            II=I+MAXFOCRIT
            IF(ISTOP(I)) THEN
!     WRITE DATA
!
!     NO DEFAULTS
               IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
               &.EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
                  WRITE(OUTLYNE,102) I,OPNAM(II),&
                  &OPERND(II,8),OPERND(II,9),&
                  &OPERND(II,10),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!
!     K DEFAULT
               IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
               &.EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
                  WRITE(OUTLYNE,103) I,OPNAM(II),&
                  &OPERND(II,8),OPERND(II,9),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!
!     J AND K DEFAULTS
               IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
               &.EQ.1.AND.INT(OPERND(II,18)).EQ.0) THEN
                  WRITE(OUTLYNE,104) I,OPNAM(II),&
                  &OPERND(II,8),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!
!     I J K DEFAULTS
               IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
               &.EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
                  WRITE(OUTLYNE,105) I,OPNAM(II),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!
!     J DEFAULT
               IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
               &.EQ.0.AND.INT(OPERND(II,18)).EQ.0) THEN
                  WRITE(OUTLYNE,106) I,OPNAM(II),&
                  &OPERND(II,8),&
                  &OPERND(II,10),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!
!     I DEFAULT
               IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
               &.EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
                  WRITE(OUTLYNE,107) I,OPNAM(II),&
                  &OPERND(II,9),&
                  &OPERND(II,10),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!     I AND K DEFAULT
               IF(INT(OPERND(II,11)).EQ.0.AND.INT(OPERND(II,12))&
               &.EQ.1.AND.INT(OPERND(II,18)).EQ.1) THEN
                  WRITE(OUTLYNE,108) I,OPNAM(II),&
                  &OPERND(II,9),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
!     I AND J DEFAULT
               IF(INT(OPERND(II,11)).EQ.1.AND.INT(OPERND(II,12))&
               &.EQ.0.AND.INT(OPERND(II,18)).EQ.1) THEN
                  WRITE(OUTLYNE,109) I,OPNAM(II),&
                  &OPERND(II,10),OPERND(II,20)
                  CALL SHOWIT(0)
               END IF
            END IF
         END DO
         DESYES=.FALSE.
         DO I=1,MAXTOP
            II=I+MAXFOCRIT
            IF(OPERDESC(II)(1:8).NE.'        ') THEN
               DESYES=.TRUE.
            END IF
         END DO
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1702)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
1702     FORMAT('CURRENT TOERANCE OPERAND DESCRIPTIONS')
1703     FORMAT(1X)
         DO I=1,MAXTOP
            II=I+MAXFOCRIT
            IF(OPERDESC(II)(1:8).NE.'        ')&
            &WRITE(OUTLYNE,1701) OPNAM(II),OPERDESC(II)(1:69)
            IF(OPERDESC(II)(1:8).NE.'        ')&
            &CALL SHOWIT(0)
         END DO
         RETURN
      END IF
   END IF
   RETURN
END
! SUB CRITOUT.FOR
SUBROUTINE CRITOUT
!
   use DATCFG
   use DATSUB
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I,CFGCHK,TAGER
!
   LOGICAL DESYES,ALLER
!
   LOGICAL YES
!
   CHARACTER CNAM*3,FNAMER*5,DASHER*3
!
!
!
   ALLER=.TRUE.
   TAGER=0
!
   DASHER='---'
!
   FUNNAM(0)='FUNC00'
   FUNNAM(1)='FUNC01'
   FUNNAM(2)='FUNC02'
   FUNNAM(3)='FUNC03'
   FUNNAM(4)='FUNC04'
   FUNNAM(5)='FUNC05'
   FUNNAM(6)='FUNC06'
   FUNNAM(7)='FUNC07'
   FUNNAM(8)='FUNC08'
   FUNNAM(9)='FUNC09'
   FUNNAM(10)='FUNC10'
!
!       THIS IS SUBROUTINE CRITOUT. THIS IS THE SUBROUTINE WHICH DOES OPERAND
!       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE MERIT AND
!       UPDATE MERIT
   IF(WC.EQ.'CRITS') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"CRITS" TAKES NO QUALIFIER OR STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'CRITS') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"CRITS" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.0) THEN
      TAGER=INT(W1)
      ALLER=.FALSE.
   END IF
   IF(DF1.EQ.1) THEN
      ALLER=.TRUE.
   END IF
!     NOW TAGER AND ALLER ARE PROBERLY SET
!
   IF(DF1.EQ.0) THEN
!     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
!     AND PERFORM THE OPERATION
      IF(W1.LE.0.0D0.OR.W1.GT.MAXFOCRIT) THEN
         WRITE(OUTLYNE,*)'FOCRIT NUMBER BEYOND DEFINED BOUNDS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
!     W1 IS A VALID NUMBER, PROCEED
      END IF
      IF(.NOT.ISCRIT(INT(W1))) THEN
         WRITE(OUTLYNE,*)'FOCRIT NUMBER NOT CURRENTLY DEFINED'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
!     W1 IS A VALID NUMBER, PROCEED
      END IF
!
!     HERE DO THE OUTPUT FOR OPERAND NUMBER INT(W1)
!       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
      YES=.FALSE.
      DO I=1,MAXFOCRIT
         IF(ISCRIT(I)) YES=.TRUE.
      END DO
      IF(.NOT.YES) FCCNT=0
      IF(YES) FCCNT=1
      IF(FCCNT.EQ.0) THEN
         WRITE(OUTLYNE,*)'NO FOCRIT DATA EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(.NOT.ISCRIT(INT(W1))) THEN
         WRITE(OUTLYNE,*)'FOCRIT DATA NOT DEFINED FOR FOCRIT #',INT(W1)
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'NO ACTION TAKEN'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WC.EQ.'CRITS') THEN
!     DO CRITS HEADER PRINTING
!     PRINT CRIT DATA
100      FORMAT(&
         &'CURRENT FOCRIT DATA FOR FOCRIT #',I2)
         WRITE(OUTLYNE,100) INT(W1)
         CALL SHOWIT(0)
!
101      FORMAT(&
         &'FOCRIT #',2X,'FOCRIT NAME',6X,&
         &' NW2 ',8X,' NW3 ',8X,' NW4 ')
401      FORMAT(&
         &27X,&
         &' (I) ',8X,' (J) ',8X,' (K) ')
         WRITE(OUTLYNE,101)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
!
!     NO DEFAULT
102      FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,G11.4)
!
!     JUST K DEFAULT
103      FORMAT(I3,11X,A8,2X,G11.4,2X,G11.4,2X,&
         &3X,'-----')
!
!     J AND K DEFAULT
104      FORMAT(I3,11X,A8,2X,G11.4,2X,3X,&
         &'-----',3X,2X,3X,'-----')
!
!     I,J AND K DEFAULT
105      FORMAT(I3,11X,A8,2X,3X,'-----',3X,2X,&
         &3X,'-----',3X,2X,3X,'-----')

!     JUST J DEFAULT
106      FORMAT(I3,11X,A8,2X,G11.4,2X,3X,&
         &'-----',3X,2X,G11.4)

!     JUST I DEFAULT
107      FORMAT(I3,11X,A8,2X,3X,'-----',3X,&
         &2X,G11.4,2X,G11.4)

!     I AND K DEFAULT
108      FORMAT(I3,11X,A8,2X,G11.4,2X,3X,&
         &'-----',3X,2X,3X,'-----')

!     I AND J DEFAULT
109      FORMAT(I3,11X,A8,2X,3X,'-----',3X,&
         &2X,G11.4,2X,3X,'-----')
!
!     NO DEFAULTS
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,102) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),8),OPERND(INT(W1),9),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
!
!     K DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,103) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),8),OPERND(INT(W1),9)
            CALL SHOWIT(0)
         END IF
!
!     J AND K DEFAULTS
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,104) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),8)
            CALL SHOWIT(0)
         END IF
!
!     I J K DEFAULTS
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,105) INT(W1),OPNAM(INT(W1))
            CALL SHOWIT(0)
         END IF
!
!     J DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,106) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),8),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
!
!     I DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,107) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),9),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
!     I AND K DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,108) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),9)
            CALL SHOWIT(0)
         END IF
!     I AND J DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,109) INT(W1),OPNAM(INT(W1)),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
         IF(OPERDESC(INT(W1))(1:8).NE.'        ')&
         &WRITE(OUTLYNE,1701) OPNAM(INT(W1)),OPERDESC(INT(W1))(1:69)
         IF(OPERDESC(INT(W1))(1:8).NE.'        ')&
         &CALL SHOWIT(1)
1701     FORMAT(A8,'::',A69)
         RETURN
      END IF
   ELSE
!     DF1=1, NO NUMERIC INPUT, DO ALL OUTPUT
!     HERE DO THE MERIT OUTPUT FOR ALL OPERANDS
!       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
      YES=.FALSE.
      DO I=1,MAXFOCRIT
         IF(ISCRIT(I)) YES=.TRUE.
      END DO
      IF(.NOT.YES) FCCNT=0
      IF(YES) FCCNT=1
      IF(FCCNT.EQ.0) THEN
         WRITE(OUTLYNE,*)'NO FOCRIT DATA EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WC.EQ.'CRITS') THEN
         WRITE(OUTLYNE,400)
         CALL SHOWIT(0)
400      FORMAT(&
         &'CURRENT FOCRIT DATA')
         WRITE(OUTLYNE,101)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
!     DO MERIT HEADER PRINTING
         DO I=1,MAXFOCRIT
            IF(ISCRIT(I)) THEN
!     WRITE DATA
!
!     NO DEFAULTS
               IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
               &.EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
                  WRITE(OUTLYNE,102) I,OPNAM(I),&
                  &OPERND(I,8),OPERND(I,9),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
!
!     K DEFAULT
               IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
               &.EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
                  WRITE(OUTLYNE,103) I,OPNAM(I),&
                  &OPERND(I,8),OPERND(I,9)
                  CALL SHOWIT(0)
               END IF
!
!     J AND K DEFAULTS
               IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
               &.EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
                  WRITE(OUTLYNE,104) I,OPNAM(I),&
                  &OPERND(I,8)
                  CALL SHOWIT(0)
               END IF
!
!     I J K DEFAULTS
               IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
               &.EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
                  WRITE(OUTLYNE,105) I,OPNAM(I)
                  CALL SHOWIT(0)
               END IF
!
!     J DEFAULT
               IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
               &.EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
                  WRITE(OUTLYNE,106) I,OPNAM(I),&
                  &OPERND(I,8),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
!
!     I DEFAULT
               IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
               &.EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
                  WRITE(OUTLYNE,107) I,OPNAM(I),&
                  &OPERND(I,9),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
!     I AND K DEFAULT
               IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
               &.EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
                  WRITE(OUTLYNE,108) I,OPNAM(I),&
                  &OPERND(I,9)
                  CALL SHOWIT(0)
               END IF
!     I AND J DEFAULT
               IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
               &.EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
                  WRITE(OUTLYNE,109) I,OPNAM(I),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
            END IF
         END DO
         DESYES=.FALSE.
         DO I=1,MAXFOCRIT
            IF(OPERDESC(I)(1:8).NE.'        ') THEN
               DESYES=.TRUE.
            END IF
         END DO
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1702)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
1702     FORMAT('CURRENT FOCRIT OPERAND DESCRIPTIONS')
1703     FORMAT(1X)
         DO I=1,MAXTOP
            IF(OPERDESC(I)(1:8).NE.'        ')&
            &WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
            IF(OPERDESC(I)(1:8).NE.'        ')&
            &CALL SHOWIT(0)
         END DO
         RETURN
      END IF
   END IF
   RETURN
END
! SUB MAROUT.FOR
SUBROUTINE MAROUT
!
   use DATCFG
   use DATSUB
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER J,I,CFGCHK,TAGER
!
   LOGICAL DESYES,ALLER,CFGER
!
   CHARACTER CNAM*3,FNAMER*5,DASHER*3
!
!
!
   ALLER=.TRUE.
   CFGER=.FALSE.
   TAGER=0
!
   DASHER='---'
!
   FUNNAM(0)='FUNC00'
   FUNNAM(1)='FUNC01'
   FUNNAM(2)='FUNC02'
   FUNNAM(3)='FUNC03'
   FUNNAM(4)='FUNC04'
   FUNNAM(5)='FUNC05'
   FUNNAM(6)='FUNC06'
   FUNNAM(7)='FUNC07'
   FUNNAM(8)='FUNC08'
   FUNNAM(9)='FUNC09'
   FUNNAM(10)='FUNC10'
!
!       THIS IS SUBROUTINE MAROUT. THIS IS THE SUBROUTINE WHICH DOES OPERAND
!       OUTPUT COMMANDS AT THE CMD LEVEL AND FROM INSIDE MERIT AND
!       UPDATE MERIT
   IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"',WC(1:2),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'CFG'.AND.DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"',WC(1:3),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'CFG'.AND.DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO NUMERIC WORD 2, 3, 4 OR 5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WQ.EQ.'CFG') THEN
      TAGER=INT(W1)
      ALLER=.FALSE.
      CFGER=.TRUE.
   END IF
   IF(WQ.NE.'CFG'.AND.DF1.EQ.0) THEN
      TAGER=INT(W1)
      ALLER=.FALSE.
      CFGER=.FALSE.
   END IF
   IF(WQ.NE.'CFG'.AND.DF1.EQ.1) THEN
      ALLER=.TRUE.
      CFGER=.FALSE.
   END IF
   IF(CFGER) THEN
!     CHECK FOR TAGER GREATER THAN MAXCFG
      IF(TAGER.GT.MAXCFG) THEN
!     CFG NOT EXISTANT
         WRITE(OUTLYNE,*)&
         &'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     PROCEED
      CFGCHK=0
      DO I=0,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
      END DO
!     CHECK FOR OPERANDS ACTIVE IN CFG TAGER

      IF(CFGCHK.EQ.0) THEN
!     CFG OP DATA NOT EXISTANT
         WRITE(OUTLYNE,*)&
         &'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!     NOW TAGER, ALLER AND CFGER ARE PROBERLY SET
!
   IF(DF1.EQ.0.AND.WQ.NE.'CFG') THEN
!     CHECK FOR A VALID NUMERIC WORD #1 INPUT VALUE
!     AND PERFORM THE OPERATION
      IF(W1.LE.0.0D0.OR.W1.GT.OPCNT) THEN
         WRITE(OUTLYNE,*)'OPERAND NUMBER BEYOND DEFINED BOUNDS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     HERE DO THE OUTPUT FOR OPERAND NUMBER INT(W1)
!       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
      IF(OPCNT.EQ.0) THEN
         WRITE(OUTLYNE,*)'NO MERIT DATA EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
!     DO MRA/OPA HEADER PRINTING
!     PRINT MRA/OPA DATA
         WRITE(OUTLYNE,200)INT(W1)
         CALL SHOWIT(0)
200      FORMAT(&
         &'CURRENT AUXILLIARY MERIT DATA (MRA/OPA) FOR OPERAND #',I3)
201      FORMAT(&
         &'OP #',2X,'OP NAME',6X,'TARGET',9X,'WEIGHT',6X,&
         &'FUNC.',2X,'REG #',2X,'MODE',2X,'CFG')
301      FORMAT(53X,'(NW3)')
         WRITE(OUTLYNE,201)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,301)
         CALL SHOWIT(0)
202      FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,I3,4X,A3,3X,I3)
802      FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,I3,4X,A3,3X,A3)
1202     FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,'---',4X,A3,3X,I3)
1802     FORMAT(I3,3X,A8,2X,G13.6,2X,G13.6,2X,A6,1X,'---',4X,A3,3X,A3)
         IF(OPERND(INT(W1),13).EQ.1.0D0) CNAM ='COR'
         IF(OPERND(INT(W1),13).EQ.0.0D0) CNAM ='BYP'
         IF(OPERND(INT(W1),13).EQ.-2.0D0) CNAM ='GTE'
         IF(OPERND(INT(W1),13).EQ.2.0D0) CNAM ='LTE'
         IF(OPERND(INT(W1),13).EQ.10.0D0) CNAM='HLD'
         IF(FUNNAM(INT(OPERND(INT(W1),1))).EQ.'FUNC00') THEN
            FNAMER='------'
         ELSE
            FNAMER=FUNNAM(INT(OPERND(INT(W1),1)))
         END IF
         IF(INT(OPERND(INT(W1),16)).NE.0) THEN
            IF(INT(OPERND(INT(W1),18)).EQ.0)&
            &WRITE(OUTLYNE,202) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),FNAMER,&
            &INT(OPERND(INT(W1),8)),CNAM,INT(OPERND(INT(W1),16))
            IF(INT(OPERND(INT(W1),18)).EQ.0) CALL SHOWIT(0)
            IF(INT(OPERND(INT(W1),18)).EQ.1)&
            &WRITE(OUTLYNE,1202) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),FNAMER,&
            &CNAM,INT(OPERND(INT(W1),16))
            IF(INT(OPERND(INT(W1),18)).EQ.1) CALL SHOWIT(0)
         ELSE
            IF(INT(OPERND(INT(W1),18)).EQ.0)&
            &WRITE(OUTLYNE,802) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),FNAMER,&
            &INT(OPERND(INT(W1),8)),CNAM,DASHER
            IF(INT(OPERND(INT(W1),18)).EQ.0) CALL SHOWIT(0)
            IF(INT(OPERND(INT(W1),18)).EQ.1)&
            &WRITE(OUTLYNE,1802) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),FNAMER,&
            &CNAM,DASHER
            IF(INT(OPERND(INT(W1),18)).EQ.1) CALL SHOWIT(0)
         END IF
!     WRITE DATA ITEMS
         IF(OPERDESC(INT(W1))(1:8).NE.'        ')&
         &WRITE(OUTLYNE,1701) OPNAM(INT(W1)),OPERDESC(INT(W1))(1:69)
         IF(OPERDESC(INT(W1))(1:8).NE.'        ')&
         &CALL SHOWIT(0)
1701     FORMAT(A8,'::',A69)
         RETURN
      END IF
      IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
!     DO MR/OP HEADER PRINTING
!     PRINT MR.OP DATA
100      FORMAT(&
         &'CURRENT MERIT FUNCTION DATA (MR/OP) FOR OPERAND #',I2)
         WRITE(OUTLYNE,100) INT(W1)
         CALL SHOWIT(0)
!
101      FORMAT(&
         &'OP #',2X,'OP NAME',6X,'TARGET',7X,'WEIGHT',7X,&
         &' NW3 ',8X,' NW4 ',8X,' NW5 ')
401      FORMAT(&
         &45X,&
         &' (I) ',8X,' (J) ',8X,' (K) ')
         WRITE(OUTLYNE,101)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
!
!     NO DEFAULT
102      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5)
!
!     JUST K DEFAULT
103      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,2X,&
         &3X,'-----')
!
!     J AND K DEFAULT
104      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,1X,3X,&
         &'-----',3X,2X,3X,'-----')
!
!     I,J AND K DEFAULT
105      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,3X,'-----',3X,2X,&
         &3X,'-----',3X,2X,3X,'-----')

!     JUST J DEFAULT
106      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,2X,3X,&
         &'-----',3X,1X,G12.5)

!     JUST I DEFAULT
107      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,3X,'-----',3X,&
         &1X,G12.5,1X,G12.5)

!     I AND K DEFAULT
108      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,1X,G12.5,2X,3X,&
         &'-----',3X,2X,3X,'-----')

!     I AND J DEFAULT
109      FORMAT(I3,3X,A8,1X,G12.5,1X,G12.5,2X,3X,'-----',3X,&
         &1X,G12.4,2X,3X,'-----')
!
!     NO DEFAULTS
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,102) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),8),OPERND(INT(W1),9),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
!
!     K DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,103) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),8),OPERND(INT(W1),9)
            CALL SHOWIT(0)
         END IF
!
!     J AND K DEFAULTS
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,104) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),8)
            CALL SHOWIT(0)
         END IF
!
!     I J K DEFAULTS
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,105) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7)
            CALL SHOWIT(0)
         END IF
!
!     J DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.0) THEN
            WRITE(OUTLYNE,106) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),8),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
!
!     I DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,107) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),9),&
            &OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
!     I AND K DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.0.AND.INT(OPERND(INT(W1),12))&
         &.EQ.1.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,108) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),9)
            CALL SHOWIT(0)
         END IF
!     I AND J DEFAULT
         IF(INT(OPERND(INT(W1),11)).EQ.1.AND.INT(OPERND(INT(W1),12))&
         &.EQ.0.AND.INT(OPERND(INT(W1),18)).EQ.1) THEN
            WRITE(OUTLYNE,109) INT(W1),OPNAM(INT(W1)),OPERND(INT(W1),2),&
            &OPERND(INT(W1),7),OPERND(INT(W1),10)
            CALL SHOWIT(0)
         END IF
         IF(OPERDESC(INT(W1))(1:8).NE.'        ')&
         &WRITE(OUTLYNE,1701) OPNAM(INT(W1)),OPERDESC(INT(W1))(1:69)
         IF(OPERDESC(INT(W1))(1:8).NE.'        ')&
         &CALL SHOWIT(0)
         RETURN
      END IF
   ELSE
!     DF1=1, NO NUMERIC INPUT, DO ALL OUTPUT
!     HERE DO THE MERIT OUTPUT FOR ALL OPERANDS
!       CHECK IF OPERAND DATA EXISTS. IF NOT PRINT MESSAGE
      IF(OPCNT.EQ.0) THEN
         WRITE(OUTLYNE,*)'NO MERIT DATA EXISTS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WC.EQ.'MRA'.OR.WC.EQ.'OPA') THEN
!     DO MRA.OPA HEADER PRINTING
         WRITE(OUTLYNE,300)
         CALL SHOWIT(0)
         IF(CFGER) WRITE(OUTLYNE,3011) TAGER
         IF(CFGER) CALL SHOWIT(0)
300      FORMAT(&
         &'CURRENT AUXILLIARY MERIT DATA (MRA/OPA)')
3011     FORMAT('ACTIVE IN CONFIGURATION # ',I2)
         WRITE(OUTLYNE,201)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,301)
         CALL SHOWIT(0)
         DO I=1,OPCNT
!     WRITE DATA ITEMS
            IF(OPERND(I,13).EQ.1.0D0) CNAM ='COR'
            IF(OPERND(I,13).EQ.0.0D0) CNAM ='BYP'
            IF(OPERND(I,13).EQ.-2.0D0) CNAM ='GTE'
            IF(OPERND(I,13).EQ.2.0D0) CNAM ='LTE'
            IF(OPERND(I,13).EQ.10.0D0) CNAM ='HLD'
            IF(FUNNAM(INT(OPERND(I,1))).EQ.'FUNC00') THEN
               FNAMER='-----'
            ELSE
               FNAMER=FUNNAM(INT(OPERND(I,1)))
            END IF
            IF(INT(OPERND(I,16)).NE.0) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  IF(INT(OPERND(I,18)).EQ.0)&
                  &WRITE(OUTLYNE,202) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),FNAMER,&
                  &INT(OPERND(I,8)),CNAM,INT(OPERND(I,16))
                  IF(INT(OPERND(I,18)).EQ.0) CALL SHOWIT(0)
                  IF(INT(OPERND(I,18)).EQ.1)&
                  &WRITE(OUTLYNE,1202) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),FNAMER,&
                  &CNAM,INT(OPERND(I,16))
                  IF(INT(OPERND(I,18)).EQ.1) CALL SHOWIT(0)
               END IF
            ELSE
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  IF(INT(OPERND(I,18)).EQ.0)&
                  &WRITE(OUTLYNE,802) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),FNAMER,&
                  &INT(OPERND(I,8)),CNAM,DASHER
                  IF(INT(OPERND(I,18)).EQ.0) CALL SHOWIT(0)
                  IF(INT(OPERND(I,18)).EQ.1)&
                  &WRITE(OUTLYNE,1802) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),FNAMER,&
                  &CNAM,DASHER
                  IF(INT(OPERND(I,18)).EQ.1) CALL SHOWIT(0)
               END IF
            END IF
         END DO
         DESYES=.FALSE.
         DO I=1,OPCNT
            IF(OPERDESC(I)(1:8).NE.'        ') THEN
               DESYES=.TRUE.
            END IF
         END DO
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1702)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
1702     FORMAT('CURRENT OPERAND DESCRIPTIONS')
1703     FORMAT(1X)
         DO I=1,OPCNT
            IF(OPERDESC(I)(1:8).NE.'        ')&
            &WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
            IF(OPERDESC(I)(1:8).NE.'        ')&
            &CALL SHOWIT(0)
         END DO
         RETURN
      END IF
      IF(WC.EQ.'MR'.OR.WC.EQ.'OP') THEN
         WRITE(OUTLYNE,400)
         CALL SHOWIT(0)
         IF(CFGER) WRITE(OUTLYNE,3011) TAGER
         IF(CFGER) CALL SHOWIT(0)
400      FORMAT(&
         &'CURRENT MERIT FUNCTION DATA (MR/OP)')
         WRITE(OUTLYNE,101)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,401)
         CALL SHOWIT(0)
!     DO MERIT HEADER PRINTING
         DO I=1,OPCNT
!     WRITE DATA
!
!     NO DEFAULTS
            IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
            &.EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,102) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,8),OPERND(I,9),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
            END IF
!
!     K DEFAULT
            IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
            &.EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,103) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,8),OPERND(I,9)
                  CALL SHOWIT(0)
               END IF
            END IF
!
!     J AND K DEFAULTS
            IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
            &.EQ.1.AND.INT(OPERND(I,18)).EQ.0) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,104) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,8)
                  CALL SHOWIT(0)
               END IF
            END IF
!
!     I J K DEFAULTS
            IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
            &.EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
               WRITE(OUTLYNE,105) I,OPNAM(I),OPERND(I,2),&
               &OPERND(I,7)
               CALL SHOWIT(0)
            END IF
!
!     J DEFAULT
            IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
            &.EQ.0.AND.INT(OPERND(I,18)).EQ.0) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,106) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,8),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
            END IF
!
!     I DEFAULT
            IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
            &.EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,107) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,9),&
                  &OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
            END IF
!     I AND K DEFAULT
            IF(INT(OPERND(I,11)).EQ.0.AND.INT(OPERND(I,12))&
            &.EQ.1.AND.INT(OPERND(I,18)).EQ.1) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,108) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,9)
                  CALL SHOWIT(0)
               END IF
            END IF
!     I AND J DEFAULT
            IF(INT(OPERND(I,11)).EQ.1.AND.INT(OPERND(I,12))&
            &.EQ.0.AND.INT(OPERND(I,18)).EQ.1) THEN
               IF(ALLER.OR.CFGER.AND.INT(OPERND(I,16)).EQ.TAGER) THEN
                  WRITE(OUTLYNE,109) I,OPNAM(I),OPERND(I,2),&
                  &OPERND(I,7),OPERND(I,10)
                  CALL SHOWIT(0)
               END IF
            END IF

         END DO
         DESYES=.FALSE.
         DO I=1,OPCNT
            IF(OPERDESC(I)(1:8).NE.'        ') THEN
               DESYES=.TRUE.
            END IF
         END DO
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1702)
         IF(DESYES) CALL SHOWIT(0)
         IF(DESYES) WRITE(OUTLYNE,1703)
         IF(DESYES) CALL SHOWIT(0)
         DO I=1,OPCNT
            IF(OPERDESC(I)(1:8).NE.'        ') THEN
               WRITE(OUTLYNE,1701) OPNAM(I),OPERDESC(I)(1:69)
               CALL SHOWIT(0)
            END IF
         END DO
         RETURN
      END IF
   END IF
   RETURN
END
