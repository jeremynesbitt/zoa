!       FIRST FILE OF PIKUP ROUTINES

! SUB PIKXYD.FOR
SUBROUTINE PIKXYD
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKXYD. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE X AND Y-DECENTRATION (XD) OR (YD) OR (ZD)
!       PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER CT
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE MAY NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SURF.EQ.0) THEN
      OUTLYNE='OBJECT SURFACE MAY NOT PIKUP A DECENTER'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(WQ.EQ.'YD') CT=13
   IF(WQ.EQ.'XD') CT=14
   IF(WQ.EQ.'ZD') CT=33
   IF(WQ.EQ.'GDX') CT=37
   IF(WQ.EQ.'GDY') CT=38
   IF(WQ.EQ.'GDZ') CT=39
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0 THE PIKUP EXISTS
!       IF IT IS 0.0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0
!       IF DF4=1 THEN W4=0.0
!       IF DF5=1 THEN W5=0.0
   IF(DF4.EQ.1) W4=0.0
   IF(DF5.EQ.1) W5=0.0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!               INCR. PIKUP INDICATOR TO 1.0
!               I.E. ALENS(32,SURF)
   ALENS(32,SURF)=ALENS(32,SURF)+1.0
   PIKUP(1,SURF,CT)=1.0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
!
!
!       NOW SET THE DECENTER FLAG CORRECTLY
!       IF ALENS(29,I) IS 1.0 FOR I = INT(W1)
!       THEN ALENS(29,SURF) = 1.0 ELSE SET TO 0.0
   IF(WQ(1:1).NE.'G') THEN
      IF(ALENS(29,INT(W1)).EQ.1.0) THEN
         ALENS(29,SURF)=1.0
      ELSE
         ALENS(29,SURF)=0.0
      END IF
   END IF
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
   IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.&
   &3.0D0) THEN
      ALENS(25,SURF)=1.0D0
      IF(ALENS(25,SURF).EQ.2.0D0) WRITE(OUTLYNE,*)&
      &'"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      IF(ALENS(25,SURF).EQ.3.0D0) WRITE(OUTLYNE,*)&
      &'"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      CALL SHOWIT(1)
   END IF
!
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB PIKPXYD.FOR
SUBROUTINE PIKPXYD
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKPXYD. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE (PIVX) OR (PIVY) OR (PIVZ)
!       PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER CT
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE MAY NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SURF.EQ.0) THEN
      OUTLYNE='OBJECT SURFACE MAY NOT PIKUP A DECENTER'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(WQ.EQ.'PIVX') CT=34
   IF(WQ.EQ.'PIVY') CT=35
   IF(WQ.EQ.'PIVZ') CT=36
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0 THE PIKUP EXISTS
!       IF IT IS 0.0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0
!       IF DF4=1 THEN W4=0.0
!       IF DF5=1 THEN W5=0.0
   IF(DF4.EQ.1) W4=0.0
   IF(DF5.EQ.1) W5=0.0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!               INCR. PIKUP INDICATOR TO 1.0
!               I.E. ALENS(32,SURF)
   ALENS(32,SURF)=ALENS(32,SURF)+1.0
   PIKUP(1,SURF,CT)=1.0
   PIKUP(2,SURF,CT)=W1
   PIKUP(3,SURF,CT)=W2
   PIKUP(4,SURF,CT)=W3
   PIKUP(5,SURF,CT)=W4
   PIKUP(6,SURF,CT)=W5
!
!
!       NOW SET THE PIVOT DECENTER FLAG CORRECTLY
!       IF ALENS(59,I) IS 1.0 FOR I = INT(W1)
!       THEN ALENS(59,SURF) = 1.0 ELSE SET TO 0.0
!
   IF(ALENS(59,INT(W1)).EQ.1.0) THEN
      ALENS(59,SURF)=1.0
   ELSE
      ALENS(59,SURF)=0.0
   END IF
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
   IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.&
   &3.0D0) THEN
      ALENS(25,SURF)=1.0D0
      IF(ALENS(25,SURF).EQ.2.0D0) WRITE(OUTLYNE,*)&
      &'"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      IF(ALENS(25,SURF).EQ.3.0D0) WRITE(OUTLYNE,*)&
      &'"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
      CALL SHOWIT(1)
   END IF
!
!       THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB PIKTH.FOR
SUBROUTINE PIKTH
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKTH. THIS IS THE SUBROUTINE WHICH
!       HANDLES THICKNESS PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER IM1,IM2
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!       CHECK FOR ASTOP EN OR EX OR ENEX INCONSISTENCIES.
!       IF FOUND, DISALLOW THE PIKUP
!
!               IS THERE AN ASTOP?
   IF(SYSTEM(26).NE.-99.0D0) THEN
!               THERE IS AN ASTOP TO CHECK
      IF(SYSTEM(27).EQ.1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
!       THERE IS AN EN ADJUSTMENT IN EFFECT
         IF(SURF.EQ.0.0D0.OR.SURF.EQ.1.0D0) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            OUTLYNE='PIKUP WILL CONFLICT WITH ASTOP (EN)'
            CALL SHOWIT(1)
            OUTLYNE='PIKUP TH ASSIGNMENT DISALLOWED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SYSTEM(27).EQ.-1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
!       THERE IS AN EX ADJUSTMENT IN EFFECT
         IM2=(INT(SYSTEM(20))-2.0D0)
         IM1=(INT(SYSTEM(20))-1.0D0)
         IF(SURF.EQ.IM1.OR.SURF.EQ.IM2) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            OUTLYNE='PIKUP WILL CONFLICT WITH ASTOP (EX)'
            CALL SHOWIT(1)
            OUTLYNE='PIKUP TH ASSIGNMENT DISALLOWED'
            CALL SHOWIT(1)
         END IF
      END IF
   ELSE
!       NO ASTOP,PROCEED
   END IF
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!       SINCE THIS IS A THICKNESS PIKUP THE THIRD DIMENSION WILL
!       BE 3
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP THE OBJECT THICKNESS
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0D0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0D0
!       IF DF4=1 THEN W4=0.0D0
!       IF DF5=1 THEN W5=0.0D0
   IF(DF4.EQ.1) W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!               INCREMENT PIKUP COUNTER BY 1.0D0
!               I.E. ALENS(32,SURF)
   IF(PIKUP(1,SURF,32).EQ.0.0D0)&
   &ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
   PIKUP(1,SURF,32)=0.0D0
   PIKUP(1,SURF,3)=1.0D0
   PIKUP(2,SURF,3)=W1
   PIKUP(3,SURF,3)=W2
   PIKUP(4,SURF,3)=W3
   PIKUP(5,SURF,3)=W4
   PIKUP(6,SURF,3)=W5
!       HANDEL SOLVE DELETIONS IF THEY EXIST
!
   IF(SOLVE(6,SURF).GT.0.0D0) THEN
      SOLVE(6,SURF)=0.0D0
      SOLVE(7,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'SURFACE',SURF,' :YZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   END IF
   IF(SOLVE(4,SURF).GT.0.0D0) THEN
      SOLVE(4,SURF)=0.0D0
      SOLVE(3,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'SURFACE',SURF,' :XZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   END IF
!
   RETURN
END
! SUB PIKTHOAL.FOR
SUBROUTINE PIKTHOAL
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKTHOAL. THIS IS THE SUBROUTINE WHICH
!       HANDLES THOAL PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER IM1,IM2
!
!
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!       CHECK FOR ASTOP EN OR EX OR ENEX INCONSISTENCIES.
!       IF FOUND, DISALLOW THE PIKUP
!
!               IS THERE AN ASTOP?
   IF(SYSTEM(26).NE.-99.0D0) THEN
!               THERE IS AN ASTOP TO CHECK
      IF(SYSTEM(27).EQ.1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
!       THERE IS AN EN ADJUSTMENT IN EFFECT
         IF(SURF.EQ.0.0D0.OR.SURF.EQ.1.0D0) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            OUTLYNE='PIKUP WILL CONFLICT WITH ASTOP (EN)'
            CALL SHOWIT(1)
            OUTLYNE='PIKUP TH ASSIGNMENT DISALLOWED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SYSTEM(27).EQ.-1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
!       THERE IS AN EX ADJUSTMENT IN EFFECT
         IM2=(INT(SYSTEM(20))-2.0D0)
         IM1=(INT(SYSTEM(20))-1.0D0)
         IF(SURF.EQ.IM1.OR.SURF.EQ.IM2) THEN
!       YOU ARE TRYING TO PUT A PIKUP WERE YOU CAN'T
            OUTLYNE='PIKUP WILL CONFLICT WITH ASTOP (EX)'
            CALL SHOWIT(1)
            OUTLYNE='PIKUP TH ASSIGNMENT DISALLOWED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!
   IF(SURF.GE.INT(W1).AND.SURF.LE.INT(W2)) THEN
      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!       SINCE THIS IS A THICKNESS PIKUP THE THIRD DIMENSION WILL
!       BE 32
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA
   IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
      OUTLYNE='"PIKUP THOAL" REQUIRES EXPLICIT SURFACE NUMBER'
      CALL SHOWIT(1)
      OUTLYNE='ENTRIES FOR NUMERIC WORDS #1 AND #2'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF

!       IF DF3=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF3.EQ.1) W3=1.0D0
!       IF DF4=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF4.EQ.1) W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20).OR.W2.LT.0.0D0 &
   &.OR.W2.GT.SYSTEM(20).OR.W1.GT.W2) THEN
      OUTLYNE='SOURCE SURFACE NUMBERS BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='"PIKUP THOAL" NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!               INCREMENT PIKUP COUNTER BY 1.0D0
!               I.E. ALENS(32,SURF)
   IF(PIKUP(1,SURF,3).EQ.0.0D0)&
   &ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
   PIKUP(1,SURF,3)=0.0D0
   PIKUP(1,SURF,32)=1.0D0
   PIKUP(2,SURF,32)=W1
   PIKUP(3,SURF,32)=W2
   PIKUP(4,SURF,32)=W3
   PIKUP(5,SURF,32)=W4
   PIKUP(6,SURF,32)=W5
!       HANDEL SOLVE DELETIONS IF THEY EXIST
!
   IF(SOLVE(6,SURF).GT.0.0D0) THEN
      SOLVE(6,SURF)=0.0D0
      SOLVE(7,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'SURFACE',SURF,' :YZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   END IF
   IF(SOLVE(4,SURF).GT.0.0D0) THEN
      SOLVE(4,SURF)=0.0D0
      SOLVE(3,SURF)=0.0D0
      WRITE(OUTLYNE,*)&
      &'SURFACE',SURF,' :XZ PLANE THICKNESS SOLVE DELETED'
      CALL SHOWIT(1)
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   END IF
!
   RETURN
END
! SUB PIKRES.FOR
SUBROUTINE PIKRES
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKRES. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE PIKUP RESOLUTIONS FOR A LENS SYSTEM SURFACE
!       DURING EOS FROM LENS INPUT OR LENS UPDATE
!
   CHARACTER TTP*11
!
   INTEGER II,JJ,KK,IT,ITHJK,PKD,I,K,J,PIKCNT,COMI
!
   COMMON/PIKCOM/COMI
!
   REAL*8 DUMMY,DUMMY2,AALL1,AALL2,&
   &AALL3,AALL4,TOTTH
!
!
   I=COMI
!
!       ARE THERE PIKUPS ON THIS SURFACE ?
!
   IF(ALENS(32,I).GT.0.0D0) THEN
!       YES, RESOLVE THEM
!       HOW MANY ARE THERE ? THERE ARE ALENS(32,I) OF THEM
!       SET PIKUP COUNTER PIKCNT
      PIKCNT=INT(ALENS(32,I))
      DO 10 K=PIKCNT,0,-1
         DO 20 J=1,PSIZ
            IF(PIKUP(1,I,J).GT.0.0D0) THEN
!       FOUND A PIKUP TO RESOLVE
!**************************************************************
               IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIKUP FROM CURRENT LENS
                  IF(ALENS(1,I).EQ.0.0D0) THEN
                     AALL1=0.0D0
                  ELSE
                     AALL1=1.0D0/ALENS(1,I)
                  END IF
                  IF(ALENS(1,INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL2=0.0D0
                  ELSE
                     AALL2=1.0D0/(ALENS(1,INT(PIKUP(2,I,J))))
                  END IF
                  IF(ALENS(24,I).EQ.0.0D0) THEN
                     AALL3=0.0D0
                  ELSE
                     AALL3=1.0D0/ALENS(24,I)
                  END IF
                  IF(ALENS(24,INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL4=0.0D0
                  ELSE
                     AALL4=1.0D0/(ALENS(24,INT(PIKUP(2,I,J))))
                  END IF
               ELSE
!     PIKUP FROM MAIN LENS
                  IF(ALENP(1,I).EQ.0.0D0) THEN
                     AALL1=0.0D0
                  ELSE
                     AALL1=1.0D0/ALENP(1,I)
                  END IF
                  IF(ALENP(1,INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL2=0.0D0
                  ELSE
                     AALL2=1.0D0/(ALENP(1,INT(PIKUP(2,I,J))))
                  END IF
                  IF(ALENP(24,I).EQ.0.0D0) THEN
                     AALL3=0.0D0
                  ELSE
                     AALL3=1.0D0/ALENP(24,I)
                  END IF
                  IF(ALENP(24,INT(PIKUP(2,I,J))).EQ.0.0D0) THEN
                     AALL4=0.0D0
                  ELSE
                     AALL4=1.0D0/(ALENP(24,INT(PIKUP(2,I,J))))
                  END IF
               END IF
! *************************************************************
               IF(J.EQ.1) THEN
!       PIKUP RD IS FOUND
!       HANDLE SPECIAL PIKUP OPTION
                  IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                     PIKUP(4,I,J)=(AALL1)-(PIKUP(3,I,J)*&
                     &(AALL2))
                     PIKUP(5,I,J)=0.0D0
                  ELSE
!       NO SPECIAL PIKUP OPTION
                  END IF
                  DUMMY=((((AALL2)*&
                  &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  IF(DUMMY.EQ.0.0D0) THEN
                     ALENS(1,I)=0.0D0
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
                  ELSE
                     ALENS(1,I)=(1.0D0/DUMMY)
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,SURF)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
                  END IF
               ELSE
!       J NOT 1, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.2) THEN
!       PIKUP CV IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(1,I)-(PIKUP(3,I,J)*&
                        &(ALENS(1,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(1,I)=(((ALENS(1,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIKUP FROM MAIN LENS
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(1,I)-(PIKUP(3,I,J)*&
                        &(ALENP(1,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(1,I)=(((ALENP(1,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 2, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.3) THEN
!       PIKUP TH IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIKUP FROM CURRENT LENS
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(3,I)-(PIKUP(3,I,J)*&
                        &(ALENS(3,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(3,I)=(((ALENS(3,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIKUP FROM MAIN LENS
!     PIKUP FROM CURRENT LENS
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(3,I)-(PIKUP(3,I,J)*&
                        &(ALENP(3,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(3,I)=(((ALENP(3,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 3, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.32) THEN
!       PIKUP TH IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIKUP FROM CURRENT LENS
!       NO SPECIAL PIKUP OPTION FOR THOAL
                     TOTTH=0.0D0
                     DO ITHJK=INT(PIKUP(2,I,J)),INT(PIKUP(3,I,J))-1
                        TOTTH=TOTTH+ALENS(3,ITHJK)
                     END DO
                     ALENS(3,I)=(((TOTTH*&
                     &(PIKUP(4,I,J)))+PIKUP(5,I,J)))
                  ELSE
!     PIKUP FROM MAIN LENS
!     PIKUP FROM CURRENT LENS
                     TOTTH=0.0D0
                     DO ITHJK=INT(PIKUP(2,I,J)),INT(PIKUP(3,I,J))-1
                        TOTTH=TOTTH+ALENP(3,ITHJK)
                     END DO
                     ALENS(3,I)=(((TOTTH*&
                     &(PIKUP(4,I,J)))+PIKUP(5,I,J)))
                  END IF
               ELSE
!       J NOT 32, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.4) THEN
!       PIKUP CC IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(2,I)-(PIKUP(3,I,J)*&
                        &(ALENS(2,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(2,I)=(((ALENS(2,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(2,I)-(PIKUP(3,I,J)*&
                        &(ALENP(2,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(2,I)=(((ALENP(2,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 4, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.5) THEN
!       PIKUP AD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(4,I)-(PIKUP(3,I,J)*&
                        &(ALENS(4,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(4,I)=(((ALENS(4,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(4,I)-(PIKUP(3,I,J)*&
                        &(ALENP(4,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(4,I)=(((ALENP(4,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 5, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.6) THEN
!       PIKUP AE IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(5,I)-(PIKUP(3,I,J)*&
                        &(ALENS(5,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(5,I)=(((ALENS(5,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(5,I)-(PIKUP(3,I,J)*&
                        &(ALENP(5,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(5,I)=(((ALENP(5,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 6, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.7) THEN
!       PIKUP AF IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(6,I)-(PIKUP(3,I,J)*&
                        &(ALENS(6,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(6,I)=(((ALENS(6,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(6,I)-(PIKUP(3,I,J)*&
                        &(ALENP(6,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(6,I)=(((ALENP(6,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 7, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.8) THEN
!       PIKUP AG IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(7,I)-(PIKUP(3,I,J)*&
                        &(ALENS(7,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(7,I)=(((ALENS(7,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(7,I)-(PIKUP(3,I,J)*&
                        &(ALENP(7,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(7,I)=(((ALENP(7,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 8, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.27) THEN
!       PIKUP AH IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(81,I)-(PIKUP(3,I,J)*&
                        &(ALENS(81,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(81,I)=(((ALENS(81,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(81,I)-(PIKUP(3,I,J)*&
                        &(ALENP(81,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(81,I)=(((ALENP(81,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 27, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.28) THEN
!       PIKUP AI IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(82,I)-(PIKUP(3,I,J)*&
                        &(ALENS(82,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(82,I)=(((ALENS(82,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(82,I)-(PIKUP(3,I,J)*&
                        &(ALENP(82,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(82,I)=(((ALENP(82,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 28, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.29) THEN
!       PIKUP AJ IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(83,I)-(PIKUP(3,I,J)*&
                        &(ALENS(83,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(83,I)=(((ALENS(83,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(83,I)-(PIKUP(3,I,J)*&
                        &(ALENP(83,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(83,I)=(((ALENP(83,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 29, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.30) THEN
!       PIKUP AK IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(84,I)-(PIKUP(3,I,J)*&
                        &(ALENS(84,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(84,I)=(((ALENS(84,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(84,I)-(PIKUP(3,I,J)*&
                        &(ALENP(84,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(84,I)=(((ALENP(84,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 30, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.31) THEN
!       PIKUP AL IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(85,I)-(PIKUP(3,I,J)*&
                        &(ALENS(85,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(85,I)=(((ALENS(85,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(85,I)-(PIKUP(3,I,J)*&
                        &(ALENP(85,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(85,I)=(((ALENP(85,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 31, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.9) THEN
!       PIKUP CVTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(24,I)-(PIKUP(3,I,J)*&
                        &(ALENS(24,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENS(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENS(23,INT(PIKUP(2,I,J)))
                     ALENS(24,I)=(((ALENS(24,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(24,I)-(PIKUP(3,I,J)*&
                        &(ALENP(24,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENP(23,INT(PIKUP(2,I,J)))
                     ALENS(24,I)=(((ALENP(24,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 9, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.10) THEN
!       PIKUP RDTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(AALL3)-(PIKUP(3,I,J)*&
                        &(AALL4))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENS(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENS(23,INT(PIKUP(2,I,J)))
                     DUMMY2=((((AALL4)*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     IF(DUMMY.EQ.0.0D0) THEN
                        ALENS(24,I)=0.0D0
                     ELSE
                        ALENS(24,I)=(1.0D0/DUMMY2)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(AALL3)-(PIKUP(3,I,J)*&
                        &(AALL4))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENP(23,INT(PIKUP(2,I,J)))
                     DUMMY2=((((AALL4)*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     IF(DUMMY.EQ.0.0D0) THEN
                        ALENS(24,I)=0.0D0
                     ELSE
                        ALENS(24,I)=(1.0D0/DUMMY2)
                     END IF
                  END IF
               ELSE
!       J NOT 10, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.11) THEN
!       PIKUP PRO IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP PRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     ALENS(1,I)=ALENS(1,INT(PIKUP(2,I,J)))
                     ALENS(2,I)=ALENS(2,INT(PIKUP(2,I,J)))
                     ALENS(4,I)=ALENS(4,INT(PIKUP(2,I,J)))
                     ALENS(5,I)=ALENS(5,INT(PIKUP(2,I,J)))
                     ALENS(6,I)=ALENS(6,INT(PIKUP(2,I,J)))
                     ALENS(7,I)=ALENS(7,INT(PIKUP(2,I,J)))
                     ALENS(8,I)=ALENS(8,INT(PIKUP(2,I,J)))
                     ALENS(81,I)=ALENS(81,INT(PIKUP(2,I,J)))
                     ALENS(82,I)=ALENS(82,INT(PIKUP(2,I,J)))
                     ALENS(83,I)=ALENS(83,INT(PIKUP(2,I,J)))
                     ALENS(84,I)=ALENS(84,INT(PIKUP(2,I,J)))
                     ALENS(85,I)=ALENS(85,INT(PIKUP(2,I,J)))
                     ALENS(96,I)=ALENS(96,INT(PIKUP(2,I,J)))
                     ALENS(97,I)=ALENS(97,INT(PIKUP(2,I,J)))
                     ALENS(98,I)=ALENS(98,INT(PIKUP(2,I,J)))
                     ALENS(99,I)=ALENS(99,INT(PIKUP(2,I,J)))
                     ALENS(100,I)=ALENS(100,INT(PIKUP(2,I,J)))
                     ALENS(101,I)=ALENS(101,INT(PIKUP(2,I,J)))
                     ALENS(126,I)=ALENS(126,INT(PIKUP(2,I,J)))
                     ALENS(138,I)=ALENS(138,INT(PIKUP(2,I,J)))
                     ALENS(139,I)=ALENS(139,INT(PIKUP(2,I,J)))
                     ALENS(140,I)=ALENS(140,INT(PIKUP(2,I,J)))
                     ALENS(141,I)=ALENS(141,INT(PIKUP(2,I,J)))
                     ALENS(142,I)=ALENS(142,INT(PIKUP(2,I,J)))
                     IF(ALENS(34,INT(PIKUP(2,I,J))).NE.0.0D0) THEN
                        ALENS(34,I)=ALENS(34,INT(PIKUP(2,I,J)))
                        FTFL01(1:96,I)=FTFL01(1:96,INT(PIKUP(2,I,J)))
                     END IF

!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENS(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENS(23,INT(PIKUP(2,I,J)))
                     ALENS(24,I)=ALENS(24,INT(PIKUP(2,I,J)))
                     ALENS(34,I)=ALENS(34,INT(PIKUP(2,I,J)))
                     ALENS(36,I)=ALENS(36,INT(PIKUP(2,I,J)))
                     ALENS(37,I)=ALENS(37,INT(PIKUP(2,I,J)))
                     ALENS(38,I)=ALENS(38,INT(PIKUP(2,I,J)))
                     ALENS(39,I)=ALENS(39,INT(PIKUP(2,I,J)))
                     ALENS(40,I)=ALENS(40,INT(PIKUP(2,I,J)))
                     ALENS(41,I)=ALENS(41,INT(PIKUP(2,I,J)))
                     ALENS(43,I)=ALENS(43,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP PRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     ALENS(1,I)=ALENP(1,INT(PIKUP(2,I,J)))
                     ALENS(2,I)=ALENP(2,INT(PIKUP(2,I,J)))
                     ALENS(4,I)=ALENP(4,INT(PIKUP(2,I,J)))
                     ALENS(5,I)=ALENP(5,INT(PIKUP(2,I,J)))
                     ALENS(6,I)=ALENP(6,INT(PIKUP(2,I,J)))
                     ALENS(7,I)=ALENP(7,INT(PIKUP(2,I,J)))
                     ALENS(8,I)=ALENP(8,INT(PIKUP(2,I,J)))
                     ALENS(81,I)=ALENP(81,INT(PIKUP(2,I,J)))
                     ALENS(82,I)=ALENP(82,INT(PIKUP(2,I,J)))
                     ALENS(83,I)=ALENP(83,INT(PIKUP(2,I,J)))
                     ALENS(84,I)=ALENP(84,INT(PIKUP(2,I,J)))
                     ALENS(85,I)=ALENP(85,INT(PIKUP(2,I,J)))
                     ALENS(96,I)=ALENS(96,INT(PIKUP(2,I,J)))
                     ALENS(97,I)=ALENS(97,INT(PIKUP(2,I,J)))
                     ALENS(98,I)=ALENS(98,INT(PIKUP(2,I,J)))
                     ALENS(99,I)=ALENS(99,INT(PIKUP(2,I,J)))
                     ALENS(100,I)=ALENS(100,INT(PIKUP(2,I,J)))
                     ALENS(101,I)=ALENS(101,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENP(23,INT(PIKUP(2,I,J)))
                     ALENS(24,I)=ALENP(24,INT(PIKUP(2,I,J)))
                     ALENS(34,I)=ALENP(34,INT(PIKUP(2,I,J)))
                     ALENS(36,I)=ALENP(36,INT(PIKUP(2,I,J)))
                     ALENS(37,I)=ALENP(37,INT(PIKUP(2,I,J)))
                     ALENS(38,I)=ALENP(38,INT(PIKUP(2,I,J)))
                     ALENS(39,I)=ALENP(39,INT(PIKUP(2,I,J)))
                     ALENS(40,I)=ALENP(40,INT(PIKUP(2,I,J)))
                     ALENS(41,I)=ALENP(41,INT(PIKUP(2,I,J)))
                     ALENS(43,I)=ALENP(43,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 11, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.12) THEN
!       PIKUP NPRO IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP NPRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     ALENS(1,I)=-ALENS(1,INT(PIKUP(2,I,J)))
                     ALENS(2,I)=ALENS(2,INT(PIKUP(2,I,J)))
                     ALENS(4,I)=-ALENS(4,INT(PIKUP(2,I,J)))
                     ALENS(5,I)=-ALENS(5,INT(PIKUP(2,I,J)))
                     ALENS(6,I)=-ALENS(6,INT(PIKUP(2,I,J)))
                     ALENS(7,I)=-ALENS(7,INT(PIKUP(2,I,J)))
                     ALENS(8,I)=ALENS(8,INT(PIKUP(2,I,J)))
                     ALENS(81,I)=-ALENS(81,INT(PIKUP(2,I,J)))
                     ALENS(82,I)=-ALENS(82,INT(PIKUP(2,I,J)))
                     ALENS(83,I)=-ALENS(83,INT(PIKUP(2,I,J)))
                     ALENS(84,I)=-ALENS(84,INT(PIKUP(2,I,J)))
                     ALENS(85,I)=-ALENS(85,INT(PIKUP(2,I,J)))
                     ALENS(96,I)=ALENS(96,INT(PIKUP(2,I,J)))
                     ALENS(97,I)=ALENS(97,INT(PIKUP(2,I,J)))
                     ALENS(98,I)=ALENS(98,INT(PIKUP(2,I,J)))
                     ALENS(99,I)=ALENS(99,INT(PIKUP(2,I,J)))
                     ALENS(100,I)=ALENS(100,INT(PIKUP(2,I,J)))
                     ALENS(101,I)=ALENS(101,INT(PIKUP(2,I,J)))
                     ALENS(126,I)=ALENS(126,INT(PIKUP(2,I,J)))
                     ALENS(138,I)=ALENS(138,INT(PIKUP(2,I,J)))
                     ALENS(139,I)=-ALENS(139,INT(PIKUP(2,I,J)))
                     ALENS(140,I)=-ALENS(140,INT(PIKUP(2,I,J)))
                     ALENS(141,I)=-ALENS(141,INT(PIKUP(2,I,J)))
                     ALENS(142,I)=-ALENS(142,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENS(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENS(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENS(23,INT(PIKUP(2,I,J)))
                     ALENS(24,I)=-ALENS(24,INT(PIKUP(2,I,J)))
                     ALENS(34,I)=ALENS(34,INT(PIKUP(2,I,J)))
                     ALENS(36,I)=ALENS(36,INT(PIKUP(2,I,J)))
                     ALENS(37,I)=-ALENS(37,INT(PIKUP(2,I,J)))
                     ALENS(38,I)=-ALENS(38,INT(PIKUP(2,I,J)))
                     ALENS(39,I)=-ALENS(39,INT(PIKUP(2,I,J)))
                     ALENS(40,I)=-ALENS(40,INT(PIKUP(2,I,J)))
                     ALENS(41,I)=ALENS(41,INT(PIKUP(2,I,J)))
                     ALENS(43,I)=ALENS(43,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP NPRO HAS NO SPECIAL PIKUP OPTION
!       PIKUP EVERY SURFACE PROILE DATA ITEM FROM THE
!       SOURCE SURFACE TO THE TARGET SURFACE
                     ALENS(1,I)=-ALENP(1,INT(PIKUP(2,I,J)))
                     ALENS(2,I)=ALENP(2,INT(PIKUP(2,I,J)))
                     ALENS(4,I)=-ALENP(4,INT(PIKUP(2,I,J)))
                     ALENS(5,I)=-ALENP(5,INT(PIKUP(2,I,J)))
                     ALENS(6,I)=-ALENP(6,INT(PIKUP(2,I,J)))
                     ALENS(7,I)=-ALENP(7,INT(PIKUP(2,I,J)))
                     ALENS(8,I)=ALENP(8,INT(PIKUP(2,I,J)))
                     ALENS(81,I)=-ALENP(81,INT(PIKUP(2,I,J)))
                     ALENS(82,I)=-ALENP(82,INT(PIKUP(2,I,J)))
                     ALENS(83,I)=-ALENP(83,INT(PIKUP(2,I,J)))
                     ALENS(84,I)=-ALENP(84,INT(PIKUP(2,I,J)))
                     ALENS(85,I)=-ALENP(85,INT(PIKUP(2,I,J)))
                     ALENS(96,I)=ALENS(96,INT(PIKUP(2,I,J)))
                     ALENS(97,I)=ALENS(97,INT(PIKUP(2,I,J)))
                     ALENS(98,I)=ALENS(98,INT(PIKUP(2,I,J)))
                     ALENS(99,I)=ALENS(99,INT(PIKUP(2,I,J)))
                     ALENS(100,I)=ALENS(100,INT(PIKUP(2,I,J)))
                     ALENS(101,I)=ALENS(101,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).EQ.0.0D0.AND.ALENS(2,I).NE.0.0D0) THEN
                        ALENS(2,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
!
!
!       IF THE SURFACE BEING PICKED UP FROM IS A TORIC AND
!       IF IT IS NOT THE SAME TORIC TYPE AS THE PICKING SURFACE
!       THEN PRINT A MESSAGE TO THE EFFECT THAT THE TORIC TYPE
!       OF THE PICKING SURFACE IS BEING CHANGED TO THE TORIC TYPE
!       OF THE SURFACE BEING PICKED UP FROM.
                     PKD=INT(PIKUP(2,I,J))
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.1.0D0) TTP='AN  Y-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.2.0D0) TTP='A   X-TORIC'
                     IF(ALENP(23,INT(PIKUP(2,I,J))).EQ.0.0D0) TTP='A NON-TORIC'
                     IF(&
                     &ALENS(23,I).NE.ALENP(23,INT(PIKUP(2,I,J)))) THEN
                        WRITE(OUTLYNE,100) I
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,200) TTP,PKD
                        CALL SHOWIT(1)
                     END IF
!
                     ALENS(23,I)=ALENP(23,INT(PIKUP(2,I,J)))
                     ALENS(24,I)=-ALENP(24,INT(PIKUP(2,I,J)))
                     ALENS(34,I)=ALENP(34,INT(PIKUP(2,I,J)))
                     ALENS(36,I)=ALENP(36,INT(PIKUP(2,I,J)))
                     ALENS(37,I)=-ALENP(37,INT(PIKUP(2,I,J)))
                     ALENS(38,I)=-ALENP(38,INT(PIKUP(2,I,J)))
                     ALENS(39,I)=-ALENP(39,INT(PIKUP(2,I,J)))
                     ALENS(40,I)=-ALENP(40,INT(PIKUP(2,I,J)))
                     ALENS(41,I)=ALENP(41,INT(PIKUP(2,I,J)))
                     ALENS(43,I)=ALENP(43,INT(PIKUP(2,I,J)))
!
                     IF(ALENS(1,I).NE.0.0D0.AND.ALENS(43,I).NE.0.0D0) THEN
                        ALENS(43,I)=0.0D0
                        OUTLYNE='WARNING:'
                        CALL SHOWIT(1)
                        WRITE(OUTLYNE,*)'FOR SURFACE ',I
                        CALL SHOWIT(1)
                        OUTLYNE=&
                        &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                        CALL SHOWIT(1)
                     END IF
                  END IF
!
               ELSE
!       J NOT 12, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.13) THEN
!       PIKUP YD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(115,I)-(PIKUP(3,I,J)*&
                        &(ALENS(115,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0d0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(29,I)=(ALENS(29,INT(PIKUP(2,I,J))))
                     ALENS(30,I)=(((ALENS(30,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(115,I)=(((ALENS(115,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(30,I)-(PIKUP(3,I,J)*&
                        &(ALENP(30,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(29,I)=(ALENP(29,INT(PIKUP(2,I,J))))
                     ALENS(30,I)=(((ALENP(30,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(115,I)=(((ALENP(115,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 13, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.33) THEN
!       PIKUP ZD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(116,I)-(PIKUP(3,I,J)*&
                        &(ALENS(116,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0d0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(29,I)=(ALENS(29,INT(PIKUP(2,I,J))))
                     ALENS(69,I)=(((ALENS(69,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(116,I)=(((ALENS(116,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(69,I)-(PIKUP(3,I,J)*&
                        &(ALENP(69,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(29,I)=(ALENP(29,INT(PIKUP(2,I,J))))
                     ALENS(69,I)=(((ALENP(69,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(116,I)=(((ALENP(116,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 33, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.34) THEN
!       PIKUP PIVX IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(78,I)-(PIKUP(3,I,J)*&
                        &(ALENS(78,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(59,I)=(ALENS(59,INT(PIKUP(2,I,J))))
                     ALENS(78,I)=(((ALENS(78,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(78,I)-(PIKUP(3,I,J)*&
                        &(ALENP(78,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(59,I)=(ALENP(59,INT(PIKUP(2,I,J))))
                     ALENS(78,I)=(((ALENP(78,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 34, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.35) THEN
!       PIKUP PIVY IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(79,I)-(PIKUP(3,I,J)*&
                        &(ALENS(79,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(59,I)=(ALENS(59,INT(PIKUP(2,I,J))))
                     ALENS(79,I)=(((ALENS(79,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(79,I)-(PIKUP(3,I,J)*&
                        &(ALENP(79,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(59,I)=(ALENP(59,INT(PIKUP(2,I,J))))
                     ALENS(79,I)=(((ALENP(79,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 35, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.36) THEN
!       PIKUP PIVZ IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(80,I)-(PIKUP(3,I,J)*&
                        &(ALENS(80,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(59,I)=(ALENS(59,INT(PIKUP(2,I,J))))
                     ALENS(80,I)=(((ALENS(80,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(80,I)-(PIKUP(3,I,J)*&
                        &(ALENP(80,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(59,I)=(ALENP(59,INT(PIKUP(2,I,J))))
                     ALENS(80,I)=(((ALENP(80,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 36, CONTINUE PROCESSING
               END IF
!**************************************************************
               IF(J.EQ.14) THEN
!       PIKUP XD IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(114,I)-(PIKUP(3,I,J)*&
                        &(ALENS(114,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(29,I)=(ALENS(29,INT(PIKUP(2,I,J))))
                     ALENS(31,I)=(((ALENS(31,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(114,I)=(((ALENS(114,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(31,I)-(PIKUP(3,I,J)*&
                        &(ALENP(31,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(29,I)=(ALENP(29,INT(PIKUP(2,I,J))))
                     ALENS(31,I)=(((ALENP(31,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(114,I)=(((ALENP(114,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 14, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.37) THEN
!       PIKUP GDX IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(90,I)-(PIKUP(3,I,J)*&
                        &(ALENS(90,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(90,I)=(((ALENS(90,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(90,I)-(PIKUP(3,I,J)*&
                        &(ALENP(90,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(90,I)=(((ALENP(90,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 37, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.38) THEN
!       PIKUP GDY IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(91,I)-(PIKUP(3,I,J)*&
                        &(ALENS(91,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(91,I)=(((ALENS(91,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(91,I)-(PIKUP(3,I,J)*&
                        &(ALENP(91,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(91,I)=(((ALENP(91,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 38, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.39) THEN
!       PIKUP GDZ IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(92,I)-(PIKUP(3,I,J)*&
                        &(ALENS(92,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(92,I)=(((ALENS(92,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(92,I)-(PIKUP(3,I,J)*&
                        &(ALENP(92,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(92,I)=(((ALENP(92,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 39, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.40) THEN
!       PIKUP GALPHA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(93,I)-(PIKUP(3,I,J)*&
                        &(ALENS(93,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(93,I)=(((ALENS(93,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(93,I)-(PIKUP(3,I,J)*&
                        &(ALENP(93,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(93,I)=(((ALENP(93,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 40, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.41) THEN
!       PIKUP GBETA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(94,I)-(PIKUP(3,I,J)*&
                        &(ALENS(94,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(94,I)=(((ALENS(94,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(94,I)-(PIKUP(3,I,J)*&
                        &(ALENP(94,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(94,I)=(((ALENP(94,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 41, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
               IF(J.EQ.42) THEN
!       PIKUP GGAMMA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(95,I)-(PIKUP(3,I,J)*&
                        &(ALENS(95,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(95,I)=(((ALENS(95,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(95,I)-(PIKUP(3,I,J)*&
                        &(ALENP(95,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(95,I)=(((ALENP(95,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 42, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.15) THEN
!       PIKUP ALPHA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(26,I)-(PIKUP(3,I,J)*&
                        &(ALENS(26,INT(PIKUP(2,I,J)))))
                        PIKUP(4,I,J)=ALENS(118,I)-(PIKUP(3,I,J)*&
                        &(ALENS(118,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(118,I)=(((ALENS(118,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(26,I)=(((ALENS(26,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(26,I)-(PIKUP(3,I,J)*&
                        &(ALENP(26,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(26,I)=(((ALENP(26,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(118,I)=(((ALENP(118,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 15, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.16) THEN
!       PIKUP BETA IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(ALENS(27,I)-(PIKUP(3,I,J)*&
                        &(ALENS(27,INT(PIKUP(2,I,J))))))
                        PIKUP(4,I,J)=(ALENS(119,I)-(PIKUP(3,I,J)*&
                        &(ALENS(119,INT(PIKUP(2,I,J))))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(119,I)=(((ALENS(119,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(27,I)=(((ALENS(27,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=(ALENP(27,I)-(PIKUP(3,I,J)*&
                        &(ALENP(27,INT(PIKUP(2,I,J))))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(27,I)=(((ALENP(27,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(119,I)=(((ALENP(119,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 16, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.17) THEN
!       PIKUP GAMMA IS FOUND
!       HANDLE SPECIAL PIKUP OPTION
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(120,I)-(PIKUP(3,I,J)*&
                        &(ALENS(120,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(28,I)=(((ALENS(28,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(120,I)=(((ALENS(120,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(28,I)-(PIKUP(3,I,J)*&
                        &(ALENP(28,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(28,I)=(((ALENP(28,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                     ALENS(120,I)=(((ALENP(120,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 14, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.18) THEN
!       PIKUP CLAP IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP CLAP HAS NO SPECIAL PIKUP OPTION
                     ALENS(10,I)=(ALENS(10,INT(PIKUP(2,I,J))))
                     ALENS(11,I)=(ALENS(11,INT(PIKUP(2,I,J))))
                     ALENS(12,I)=(ALENS(12,INT(PIKUP(2,I,J))))
                     ALENS(13,I)=(ALENS(13,INT(PIKUP(2,I,J))))
                     ALENS(14,I)=(ALENS(14,INT(PIKUP(2,I,J))))
                     ALENS(15,I)=(ALENS(15,INT(PIKUP(2,I,J))))
                     ALENS(52,I)=(ALENS(52,INT(PIKUP(2,I,J))))
                     ALENS(53,I)=(ALENS(53,INT(PIKUP(2,I,J))))
                     ALENS(54,I)=(ALENS(54,INT(PIKUP(2,I,J))))
                     ALENS(55,I)=(ALENS(55,INT(PIKUP(2,I,J))))
                     ALENS(56,I)=(ALENS(56,INT(PIKUP(2,I,J))))
                     ALENS(57,I)=(ALENS(57,INT(PIKUP(2,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP CLAP HAS NO SPECIAL PIKUP OPTION
                     ALENS(10,I)=(ALENP(10,INT(PIKUP(2,I,J))))
                     ALENS(11,I)=(ALENP(11,INT(PIKUP(2,I,J))))
                     ALENS(12,I)=(ALENP(12,INT(PIKUP(2,I,J))))
                     ALENS(13,I)=(ALENP(13,INT(PIKUP(2,I,J))))
                     ALENS(14,I)=(ALENP(14,INT(PIKUP(2,I,J))))
                     ALENS(15,I)=(ALENP(15,INT(PIKUP(2,I,J))))
                     ALENS(52,I)=(ALENP(52,INT(PIKUP(2,I,J))))
                     ALENS(53,I)=(ALENP(53,INT(PIKUP(2,I,J))))
                     ALENS(54,I)=(ALENP(54,INT(PIKUP(2,I,J))))
                     ALENS(55,I)=(ALENP(55,INT(PIKUP(2,I,J))))
                     ALENS(56,I)=(ALENP(56,INT(PIKUP(2,I,J))))
                     ALENS(57,I)=(ALENP(57,INT(PIKUP(2,I,J))))
                  END IF
               ELSE
!       J NOT 18, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.19) THEN
!       PIKUP COBS IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP COBS HAS NO SPECIAL PIKUP OPTION
                     ALENS(17,I)=(ALENS(17,INT(PIKUP(2,I,J))))
                     ALENS(18,I)=(ALENS(18,INT(PIKUP(2,I,J))))
                     ALENS(19,I)=(ALENS(19,INT(PIKUP(2,I,J))))
                     ALENS(20,I)=(ALENS(20,INT(PIKUP(2,I,J))))
                     ALENS(21,I)=(ALENS(21,INT(PIKUP(2,I,J))))
                     ALENS(22,I)=(ALENS(22,INT(PIKUP(2,I,J))))
                     ALENS(62,I)=(ALENS(62,INT(PIKUP(2,I,J))))
                     ALENS(63,I)=(ALENS(63,INT(PIKUP(2,I,J))))
                     ALENS(64,I)=(ALENS(64,INT(PIKUP(2,I,J))))
                     ALENS(65,I)=(ALENS(65,INT(PIKUP(2,I,J))))
                     ALENS(66,I)=(ALENS(66,INT(PIKUP(2,I,J))))
                     ALENS(67,I)=(ALENS(67,INT(PIKUP(2,I,J))))
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP COBS HAS NO SPECIAL PIKUP OPTION
                     ALENS(17,I)=(ALENP(17,INT(PIKUP(2,I,J))))
                     ALENS(18,I)=(ALENP(18,INT(PIKUP(2,I,J))))
                     ALENS(19,I)=(ALENP(19,INT(PIKUP(2,I,J))))
                     ALENS(20,I)=(ALENP(20,INT(PIKUP(2,I,J))))
                     ALENS(21,I)=(ALENP(21,INT(PIKUP(2,I,J))))
                     ALENS(22,I)=(ALENP(22,INT(PIKUP(2,I,J))))
                     ALENS(62,I)=(ALENP(62,INT(PIKUP(2,I,J))))
                     ALENS(63,I)=(ALENP(63,INT(PIKUP(2,I,J))))
                     ALENS(64,I)=(ALENP(64,INT(PIKUP(2,I,J))))
                     ALENS(65,I)=(ALENP(65,INT(PIKUP(2,I,J))))
                     ALENS(66,I)=(ALENP(66,INT(PIKUP(2,I,J))))
                     ALENS(67,I)=(ALENP(67,INT(PIKUP(2,I,J))))
                  END IF
               ELSE
!       J NOT 19, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.20) THEN
!       PIKUP GLASS IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       PIKUP GLASS HAS NO SPECIAL PIKUP OPTION
                     GLANAM(I,1)=GLANAM(INT(PIKUP(2,I,J)),1)
                     GLANAM(I,2)=GLANAM(INT(PIKUP(2,I,J)),2)
                     ALENS(46,I)=(ALENS(46,INT(PIKUP(2,I,J))))
                     ALENS(47,I)=(ALENS(47,INT(PIKUP(2,I,J))))
                     ALENS(48,I)=(ALENS(48,INT(PIKUP(2,I,J))))
                     ALENS(49,I)=(ALENS(49,INT(PIKUP(2,I,J))))
                     ALENS(50,I)=(ALENS(50,INT(PIKUP(2,I,J))))
                     ALENS(71,I)=(ALENS(71,INT(PIKUP(2,I,J))))
                     ALENS(72,I)=(ALENS(72,INT(PIKUP(2,I,J))))
                     ALENS(73,I)=(ALENS(73,INT(PIKUP(2,I,J))))
                     ALENS(74,I)=(ALENS(74,INT(PIKUP(2,I,J))))
                     ALENS(75,I)=(ALENS(75,INT(PIKUP(2,I,J))))
                     ALENS(86,I)=(ALENS(86,INT(PIKUP(2,I,J))))
                     ALENS(87,I)=(ALENS(87,INT(PIKUP(2,I,J))))
                     ALENS(89,I)=(ALENS(89,INT(PIKUP(2,I,J))))
                     IF(I.EQ.0.AND.ALENS(46,I).LT.0.0D0.AND.&
                     &GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.&
                     &GLANAM(I,2).NE.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
                        ALENS(46,I)=-ALENS(46,I)
                        ALENS(47,I)=-ALENS(47,I)
                        ALENS(48,I)=-ALENS(48,I)
                        ALENS(49,I)=-ALENS(49,I)
                        ALENS(50,I)=-ALENS(50,I)
                        ALENS(71,I)=-ALENS(71,I)
                        ALENS(72,I)=-ALENS(72,I)
                        ALENS(73,I)=-ALENS(73,I)
                        ALENS(74,I)=-ALENS(74,I)
                        ALENS(75,I)=-ALENS(75,I)
                     END IF
                     IF(I.EQ.0.AND.ALENS(46,I).GT.0.0D0.AND.&
                     &GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.OR.&
                     &I.EQ.0.AND.ALENS(46,I).GT.0.0D0.AND.&
                     &GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE ALWAYS POSITIVE
                        ALENS(46,I)=-ALENS(46,I)
                        ALENS(47,I)=-ALENS(47,I)
                        ALENS(48,I)=-ALENS(48,I)
                        ALENS(49,I)=-ALENS(49,I)
                        ALENS(50,I)=-ALENS(50,I)
                        ALENS(71,I)=-ALENS(71,I)
                        ALENS(72,I)=-ALENS(72,I)
                        ALENS(73,I)=-ALENS(73,I)
                        ALENS(74,I)=-ALENS(74,I)
                        ALENS(75,I)=-ALENS(75,I)
                     END IF
                     IF(I.GT.0.AND.ALENS(46,I-1).LT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS NEG INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE NEG, MAKE POS
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFL'.AND.ALENS(46,I).LT.0.0D0.OR.&
                        &GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFLTIRO'.AND.ALENS(46,I).LT.0.0D0) THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
!     IF CURRENT SURFACE IS NOT REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.&
                        &GLANAM(I,2).NE.'REFL'.AND.ALENS(46,I).GT.0.0D0.AND.&
                        &GLANAM(I,2).NE.'REFLTIRO') THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
                     END IF
                     IF(I.GT.0.AND.ALENS(46,I-1).GT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS POS INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFL'.AND.ALENS(46,I).GT.0.0D0.OR.&
                        &GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFLTIRO'.AND.ALENS(46,I).GT.0.0D0) THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.&
                        &GLANAM(I,2).NE.'REFL'.AND.ALENS(46,I).LT.0.0D0.AND.&
                        &GLANAM(I,2).NE.'REFLTIRO') THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
                     END IF
                  ELSE
!     PIK FROM MAIN CFG
!       PIKUP GLASS HAS NO SPECIAL PIKUP OPTION
                     GLANAM(I,1)=GLANMP(INT(PIKUP(2,I,J)),1)
                     GLANAM(I,2)=GLANMP(INT(PIKUP(2,I,J)),2)
                     ALENS(46,I)=(ALENP(46,INT(PIKUP(2,I,J))))
                     ALENS(47,I)=(ALENP(47,INT(PIKUP(2,I,J))))
                     ALENS(48,I)=(ALENP(48,INT(PIKUP(2,I,J))))
                     ALENS(49,I)=(ALENP(49,INT(PIKUP(2,I,J))))
                     ALENS(50,I)=(ALENP(50,INT(PIKUP(2,I,J))))
                     ALENS(71,I)=(ALENP(71,INT(PIKUP(2,I,J))))
                     ALENS(72,I)=(ALENP(72,INT(PIKUP(2,I,J))))
                     ALENS(73,I)=(ALENP(73,INT(PIKUP(2,I,J))))
                     ALENS(74,I)=(ALENP(74,INT(PIKUP(2,I,J))))
                     ALENS(75,I)=(ALENP(75,INT(PIKUP(2,I,J))))
                     ALENS(86,I)=(ALENP(86,INT(PIKUP(2,I,J))))
                     ALENS(87,I)=(ALENP(87,INT(PIKUP(2,I,J))))
                     ALENS(89,I)=(ALENP(89,INT(PIKUP(2,I,J))))
                     IF(I.EQ.0.AND.ALENS(46,I).LT.0.0D0.AND.&
                     &GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFL'.AND.&
                     &GLANAM(I,2).NE.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE ALWAYS POSITIVE
                        ALENS(46,I)=-ALENS(46,I)
                        ALENS(47,I)=-ALENS(47,I)
                        ALENS(48,I)=-ALENS(48,I)
                        ALENS(49,I)=-ALENS(49,I)
                        ALENS(50,I)=-ALENS(50,I)
                        ALENS(71,I)=-ALENS(71,I)
                        ALENS(72,I)=-ALENS(72,I)
                        ALENS(73,I)=-ALENS(73,I)
                        ALENS(74,I)=-ALENS(74,I)
                        ALENS(75,I)=-ALENS(75,I)
                     END IF
                     IF(I.EQ.0.AND.ALENS(46,I).GT.0.0D0.AND.&
                     &GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFL'.OR.&
                     &I.EQ.0.AND.ALENS(46,I).GT.0.0D0.AND.&
                     &GLANAM(I,1)(1:5).EQ.'     '.AND.GLANAM(I,2).EQ.'REFLTIRO') THEN
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE POSITIVE UNLESS GLASS
!     IS REFL
!     AT OBJECT SURFACE, REFRACTIVE INDECIS ARE ALWAYS POSITIVE
                        ALENS(46,I)=-ALENS(46,I)
                        ALENS(47,I)=-ALENS(47,I)
                        ALENS(48,I)=-ALENS(48,I)
                        ALENS(49,I)=-ALENS(49,I)
                        ALENS(50,I)=-ALENS(50,I)
                        ALENS(71,I)=-ALENS(71,I)
                        ALENS(72,I)=-ALENS(72,I)
                        ALENS(73,I)=-ALENS(73,I)
                        ALENS(74,I)=-ALENS(74,I)
                        ALENS(75,I)=-ALENS(75,I)
                     END IF
                     IF(I.GT.0.AND.ALENS(46,I-1).LT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS NEG INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE NEG, MAKE POS
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFL'.AND.ALENS(46,I).LT.0.0D0.OR.&
                        &GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFLTIRO'.AND.ALENS(46,I).LT.0.0D0) THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
!     IF CURRENT SURFACE IS NOT REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.&
                        &GLANAM(I,2).NE.'REFL'.AND.ALENS(46,I).GT.0.0D0.AND.&
                        &GLANAM(I,2).NE.'REFLTIRO') THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
                     END IF
                     IF(I.GT.0.AND.ALENS(46,I-1).GT.0.0D0) THEN
!     NOT AT OBJECT AND PREVIOUS SURFACE HAS POS INDEX
!     IF CURRENT SURFACE IS REFL, AND INDECIS ARE POS, MAKE NEG
                        IF(GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFL'.AND.ALENS(46,I).GT.0.0D0.OR.&
                        &GLANAM(I,1)(1:5).EQ.'     '.AND.&
                        &GLANAM(I,2).EQ.'REFLTIRO'.AND.ALENS(46,I).GT.0.0D0) THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
!     IF CURRENT SURFACE IS NOT REFL, AND INDECIS ARE NEG, MAKE POS
                        IF(GLANAM(I,1)(1:5).NE.'     '.AND.GLANAM(I,2).NE.'REFLTIRO'&
                        &.AND.GLANAM(I,2).NE.'REFL'.AND.ALENS(46,I).LT.0.0D0) THEN
                           ALENS(46,I)=-ALENS(46,I)
                           ALENS(47,I)=-ALENS(47,I)
                           ALENS(48,I)=-ALENS(48,I)
                           ALENS(49,I)=-ALENS(49,I)
                           ALENS(50,I)=-ALENS(50,I)
                           ALENS(71,I)=-ALENS(71,I)
                           ALENS(72,I)=-ALENS(72,I)
                           ALENS(73,I)=-ALENS(73,I)
                           ALENS(74,I)=-ALENS(74,I)
                           ALENS(75,I)=-ALENS(75,I)
                        END IF
                     END IF
                  END IF
               ELSE
!       J NOT 20, CONTINUE PROCESSING
               END IF
! DO A SPECIAL RESOLUTION OF INDEX VALUES TO COVER PICKING UP A REFLECTOR

!********************************************************************
!     THIS WAS MOVED HERE AFTER PIKUPS TO MAKE REFL WORK WITH
!     PIKUP GLASS  FIXED ON 6/14/96 FOUND BY HANS PEW OF MOXTEC
!
               DO 201 KK=0,INT(SYSTEM(20))
                  ALENS(46,KK)=DABS(ALENS(46,KK))
                  ALENS(47,KK)=DABS(ALENS(47,KK))
                  ALENS(48,KK)=DABS(ALENS(48,KK))
                  ALENS(49,KK)=DABS(ALENS(49,KK))
                  ALENS(50,KK)=DABS(ALENS(50,KK))
                  ALENS(71,KK)=DABS(ALENS(71,KK))
                  ALENS(72,KK)=DABS(ALENS(72,KK))
                  ALENS(73,KK)=DABS(ALENS(73,KK))
                  ALENS(74,KK)=DABS(ALENS(74,KK))
                  ALENS(75,KK)=DABS(ALENS(75,KK))
201            CONTINUE
!
               F21=0
               DO 101 II=1,INT(SYSTEM(20))
                  IF(GLANAM(II,2).EQ.'REFL'.OR.GLANAM(II,2).EQ.'REFLTIRO') THEN
                     IF(F21.EQ.1.OR.F21.EQ.0) THEN
                        F21=-1
                        ALENS(46,II)=ALENS(46,(II-1))
                        ALENS(47,II)=ALENS(47,(II-1))
                        ALENS(48,II)=ALENS(48,(II-1))
                        ALENS(49,II)=ALENS(49,(II-1))
                        ALENS(50,II)=ALENS(50,(II-1))
                        ALENS(71,II)=ALENS(71,(II-1))
                        ALENS(72,II)=ALENS(72,(II-1))
                        ALENS(73,II)=ALENS(73,(II-1))
                        ALENS(74,II)=ALENS(74,(II-1))
                        ALENS(75,II)=ALENS(75,(II-1))
                     ELSE
                        IF(F21.EQ.-1)THEN
                           F21=1
                           ALENS(46,II)=ALENS(46,(II-1))
                           ALENS(47,II)=ALENS(47,(II-1))
                           ALENS(48,II)=ALENS(48,(II-1))
                           ALENS(49,II)=ALENS(49,(II-1))
                           ALENS(50,II)=ALENS(50,(II-1))
                           ALENS(71,II)=ALENS(71,(II-1))
                           ALENS(72,II)=ALENS(72,(II-1))
                           ALENS(73,II)=ALENS(73,(II-1))
                           ALENS(74,II)=ALENS(74,(II-1))
                           ALENS(75,II)=ALENS(75,(II-1))
                        END IF
                     END IF
                  END IF
                  IF(F21.EQ.0) GO TO 101
                  IF(F21.EQ.-1) THEN
                     IF(ALENS(46,II).GT.0.0D0)  ALENS(46,II)=-ALENS(46,II)
                     IF(ALENS(47,II).GT.0.0D0)  ALENS(47,II)=-ALENS(47,II)
                     IF(ALENS(48,II).GT.0.0D0)  ALENS(48,II)=-ALENS(48,II)
                     IF(ALENS(49,II).GT.0.0D0)  ALENS(49,II)=-ALENS(49,II)
                     IF(ALENS(50,II).GT.0.0D0)  ALENS(50,II)=-ALENS(50,II)
                     IF(ALENS(71,II).GT.0.0D0)  ALENS(71,II)=-ALENS(71,II)
                     IF(ALENS(72,II).GT.0.0D0)  ALENS(72,II)=-ALENS(72,II)
                     IF(ALENS(73,II).GT.0.0D0)  ALENS(73,II)=-ALENS(73,II)
                     IF(ALENS(74,II).GT.0.0D0)  ALENS(74,II)=-ALENS(74,II)
                     IF(ALENS(75,II).GT.0.0D0)  ALENS(75,II)=-ALENS(75,II)
                  END IF
                  IF(F21.EQ.1) THEN
                     IF(ALENS(46,II).LT.0.0D0)ALENS(46,II)=DABS(ALENS(46,II))
                     IF(ALENS(47,II).LT.0.0D0)ALENS(47,II)=DABS(ALENS(47,II))
                     IF(ALENS(48,II).LT.0.0D0)ALENS(48,II)=DABS(ALENS(48,II))
                     IF(ALENS(49,II).LT.0.0D0)ALENS(49,II)=DABS(ALENS(49,II))
                     IF(ALENS(50,II).LT.0.0D0)ALENS(50,II)=DABS(ALENS(50,II))
                     IF(ALENS(71,II).LT.0.0D0)ALENS(71,II)=DABS(ALENS(71,II))
                     IF(ALENS(72,II).LT.0.0D0)ALENS(72,II)=DABS(ALENS(72,II))
                     IF(ALENS(73,II).LT.0.0D0)ALENS(73,II)=DABS(ALENS(73,II))
                     IF(ALENS(74,II).LT.0.0D0)ALENS(74,II)=DABS(ALENS(74,II))
                     IF(ALENS(75,II).LT.0.0D0)ALENS(75,II)=DABS(ALENS(75,II))
                  END IF
101            CONTINUE
!
!********************************************************************
! *************************************************************
               IF(J.EQ.21) THEN
!       PIKUP CCTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(41,I)-(PIKUP(3,I,J)*&
                        &(ALENS(41,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(41,I)=(((ALENS(41,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(41,I)-(PIKUP(3,I,J)*&
                        &(ALENP(41,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(41,I)=(((ALENP(41,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 21, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.22) THEN
!       PIKUP ADTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(37,I)-(PIKUP(3,I,J)*&
                        &(ALENS(37,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENS(36,INT(PIKUP(2,I,J)))
                     ALENS(37,I)=(((ALENS(37,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(37,I)-(PIKUP(3,I,J)*&
                        &(ALENP(37,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENP(36,INT(PIKUP(2,I,J)))
                     ALENS(37,I)=(((ALENP(37,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 22, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.23) THEN
!       PIKUP AETOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(38,I)-(PIKUP(3,I,J)*&
                        &(ALENS(38,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENS(36,INT(PIKUP(2,I,J)))
                     ALENS(38,I)=(((ALENS(38,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(38,I)-(PIKUP(3,I,J)*&
                        &(ALENP(38,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENP(36,INT(PIKUP(2,I,J)))
                     ALENS(38,I)=(((ALENP(38,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 23, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.24) THEN
!       PIKUP AFTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(39,I)-(PIKUP(3,I,J)*&
                        &(ALENS(39,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENS(36,INT(PIKUP(2,I,J)))
                     ALENS(39,I)=(((ALENS(39,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(39,I)-(PIKUP(3,I,J)*&
                        &(ALENP(39,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENP(36,INT(PIKUP(2,I,J)))
                     ALENS(39,I)=(((ALENP(39,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 24, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.25) THEN
!       PIKUP AGTOR IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENS(40,I)-(PIKUP(3,I,J)*&
                        &(ALENS(40,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENS(36,INT(PIKUP(2,I,J)))
                     ALENS(40,I)=(((ALENS(40,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
!       HANDLE SPECIAL PIKUP OPTION
                     IF(PIKUP(5,I,J).EQ.1.0D0) THEN
!       FOUND SPECIAL PIKUP OPTION
                        PIKUP(4,I,J)=ALENP(40,I)-(PIKUP(3,I,J)*&
                        &(ALENP(40,INT(PIKUP(2,I,J)))))
                        PIKUP(5,I,J)=0.0D0
                     ELSE
!       NO SPECIAL PIKUP OPTION
                     END IF
                     ALENS(36,I)=ALENP(36,INT(PIKUP(2,I,J)))
                     ALENS(40,I)=(((ALENP(40,INT(PIKUP(2,I,J)))*&
                     &(PIKUP(3,I,J)))+PIKUP(4,I,J)))
                  END IF
               ELSE
!       J NOT 25, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.43) THEN
!       PIKUP GRT IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
                     ALENS(96,I)=ALENS(96,INT(PIKUP(2,I,J)))
                     ALENS(97,I)=ALENS(97,INT(PIKUP(2,I,J)))
                     ALENS(98,I)=ALENS(98,INT(PIKUP(2,I,J)))
                     ALENS(99,I)=ALENS(99,INT(PIKUP(2,I,J)))
                     ALENS(100,I)=ALENS(100,INT(PIKUP(2,I,J)))
                     ALENS(101,I)=ALENS(101,INT(PIKUP(2,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
                     ALENS(96,I)=ALENP(96,INT(PIKUP(2,I,J)))
                     ALENS(97,I)=ALENP(97,INT(PIKUP(2,I,J)))
                     ALENS(98,I)=ALENP(98,INT(PIKUP(2,I,J)))
                     ALENS(99,I)=ALENP(99,INT(PIKUP(2,I,J)))
                     ALENS(100,I)=ALENP(100,INT(PIKUP(2,I,J)))
                     ALENS(101,I)=ALENP(101,INT(PIKUP(2,I,J)))
                  END IF
               ELSE
!       J NOT 43, CONTINUE PROCESSING
               END IF
!**************************************************************
! *************************************************************
               IF(J.EQ.44) THEN
!       PIKUP COATING IS FOUND
                  IF(PIKUP(6,I,J).EQ.0.0D0.OR.F12.EQ.1) THEN
!     PIK FROM CURRENT CFG
                     ALENS(112,I)=ALENS(112,INT(PIKUP(2,I,J)))
                  ELSE
!     PIK FROM MAIN CFG
                     ALENS(112,I)=ALENP(112,INT(PIKUP(2,I,J)))
                  END IF
               ELSE
!       J NOT 44, CONTINUE PROCESSING
               END IF
!**************************************************************
!**************************************************************
            ELSE
!       CHECK THE NEXT J VALUE
            END IF
20       CONTINUE
!       RAN THROUGH ALL THE TYPES
10    CONTINUE
!       RESOLVED ALL THE PIKUPS
   ELSE
!       NO PIKUPS,JUST RETURN
   END IF
!
   RETURN
100 FORMAT(&
   &'THE TORIC TYPE OF SURFACE ',I3,' IS BEING CHANGED')
200 FORMAT(&
   &'FROM ',A11,' TO MATCH THAT OF SURFACE ',I3)
END
! SUB PIKPRO.FOR
SUBROUTINE PIKPRO
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKPRO. THIS IS THE SUBROUTINE WHICH
!       HANDLES (PRO) AND (NPRO)PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!       PRO AND NPRO PIKUP:
!               CURVATURE (CV)
!               CONIC CONSTANT (CC)
!       ASPHERIC DEFINITION AND TERMS (AC TO AG)
!       TORIC DEFINITION AND VALUES (CVTOR) AND TYPE
!       TORIC CONIC AND TORIC ASPHERIC
!       TERMS (CCTOR AND ADTOR TO AGTOR)
!       AND SPECIAL SURFACE TYPE DEFINED WITH SPSRF
!       IT DESTROYS WHATEVER WAS ON THE TARGET SURFACE
!       AND PRINTS A SIMPLE MESSAGE THAT THE SURFACE NOW
!       IS EITHER IDENTICAL TO OR THE NEGATIVE OF THE
!       SOURCE SURFACE. CONIC TERMS ARE THE
!       SAME UNDER PRO AND NPRO, BUT ASPHERIC
!       TERMS AND CURVATURES HAVE A SIGN REVERSAL
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!       FOR (PRO)
!       THE THIRD DIMENSION WILL BE 11
!       FOR (NPRO)
!       THE THIRD DIMENSION WILL BE 12
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
      OUTLYNE=&
      &'"PIKUP (PRO OR NPRO)" ONLY TAKE NUMERIC WORD #1 AND #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(W1.LT.0.0D0) W1=SURF+W1
!
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       IF SOLVES EXIST ON THE SOURCE SURFACE THEN
!       PRO AND NPRO WILL ONLY BE VALID IF THE TARGET
!       SURFACE HAS A HIGHER SURFACE NUMBER (COMES AFTER)
!       THE SOURCE SURFACE. ALL CURVATURE SOLVES
!       ON THE TARGET SURFACE WILL BE DESTROYED AND SOLVES
!       WILL NO BE ASSIGNED TO THE TARGET SURFACE BY PRO
!       OR NPRO.
!
   IF(SOLVE(8,SURF).GT.0.0D0) THEN
      SOLVE(8,SURF)=0.0D0
      SOLVE(9,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,&
      &' : ALL YZ PLANE CURVATURE SOLVES DELETED'
      CALL SHOWIT(1)
   END IF
   IF(SOLVE(2,SURF).GT.0.0D0) THEN
      SOLVE(2,SURF)=0.0D0
      SOLVE(1,SURF)=0.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,&
      &' : ALL XZ PLANE CURVATURE SOLVES DELETED'
      CALL SHOWIT(1)
   ELSE
!       RESOLVE ALENS(33,SURF)
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   END IF
!       DUMP OLD CV  AND CV OR RD PIKUPS
   IF(PIKUP(1,SURF,1).GT.0.0D0) THEN
      PIKUP(1:6,SURF,1)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (RD) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,2).GT.0.0D0) THEN
      PIKUP(1:6,SURF,2)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CV) DELETED'
      CALL SHOWIT(1)
   END IF
   ALENS(1,SURF)=0.0D0
   ALENS(2,SURF)=0.0D0
!       CONIC CONSTANT REMOVAL
   IF(PIKUP(1,SURF,4).GT.0.0D0) THEN
      PIKUP(1:6,SURF,4)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CC) DELETED'
      CALL SHOWIT(1)
   END IF
   ALENS(2,SURF)=0.0D0
!       ASPHERIC DEFINITION,TERM VALUES AND PIKUPS
   IF(PIKUP(1,SURF,26).GT.0.0D0) THEN
      PIKUP(1:6,SURF,26)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AC) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,5).GT.0.0D0) THEN
      PIKUP(1:6,SURF,5)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AD) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,6).GT.0.0D0) THEN
      PIKUP(1:6,SURF,6)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AE) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,7).GT.0.0D0) THEN
      PIKUP(1:6,SURF,7)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AF) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,8).GT.0.0D0) THEN
      PIKUP(1:6,SURF,8)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AG) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,27).GT.0.0D0) THEN
      PIKUP(1:6,SURF,27)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AH) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,28).GT.0.0D0) THEN
      PIKUP(1:6,SURF,28)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AI) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,29).GT.0.0D0) THEN
      PIKUP(1:6,SURF,29)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AJ) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,30).GT.0.0D0) THEN
      PIKUP(1:6,SURF,30)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AK) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,31).GT.0.0D0) THEN
      PIKUP(1:6,SURF,31)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AL) DELETED'
      CALL SHOWIT(1)
   END IF
   ALENS(5:8,SURF)=0.0D0
   ALENS(81:85,SURF)=0.0D0
!       TORIC DEFINITION,CONICS,ASPH AND CURVES
   IF(PIKUP(1,SURF,9).GT.0.0D0) THEN
      PIKUP(1:6,SURF,9)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (RDTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,10).GT.0.0D0) THEN
      PIKUP(1:6,SURF,10)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CVTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,21).GT.0.0D0) THEN
      PIKUP(1:6,SURF,21)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CCTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,22).GT.0.0D0) THEN
      PIKUP(1:6,SURF,22)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ADTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,23).GT.0.0D0) THEN
      PIKUP(1:6,SURF,23)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AETOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,24).GT.0.0D0) THEN
      PIKUP(1:6,SURF,24)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AFTOR) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,25).GT.0.0D0) THEN
      PIKUP(1:6,SURF,25)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (AGTOR) DELETED'
      CALL SHOWIT(1)
   END IF
!       TORICS
   ALENS(23:24,SURF)=0.0D0
!       SPSRF
   ALENS(34,SURF)=0.0D0
!       ASPHT
   ALENS(36:40,SURF)=0.0D0
!       CCTOR
   ALENS(41,SURF)=0.0D0
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1:6,SURF,11)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   ALENS(1:2,SURF)=0.0D0
   ALENS(4:8,SURF)=0.0D0
   ALENS(81:85,SURF)=0.0D0
   ALENS(23:24,SURF)=0.0D0
   ALENS(34,SURF)=0.0D0
   ALENS(36:41,SURF)=0.0D0
   ALENS(43,SURF)=0.0D0
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1:6,SURF,12)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
   ALENS(1:2,SURF)=0.0D0
   ALENS(4:8,SURF)=0.0D0
   ALENS(81:85,SURF)=0.0D0
   ALENS(23:24,SURF)=0.0D0
   ALENS(34,SURF)=0.0D0
   ALENS(36:41,SURF)=0.0D0
   ALENS(43,SURF)=0.0D0
!               INCREMENT THEPIKUP COUNTER BY 1.0D0
!               I.E. ALENS(32,SURF)
   ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
   IF(WQ.EQ.'PRO') THEN
      PIKUP(1,SURF,11)=1.0D0
      PIKUP(2,SURF,11)=W1
      PIKUP(3,SURF,11)=W2
      PIKUP(4,SURF,11)=W3
      PIKUP(5,SURF,11)=W4
      PIKUP(6,SURF,11)=W5
   ELSE
   END IF
   IF(WQ.EQ.'NPRO') THEN
      PIKUP(1,SURF,12)=1.0D0
      PIKUP(2,SURF,12)=W1
      PIKUP(3,SURF,12)=W2
      PIKUP(4,SURF,12)=W3
      PIKUP(5,SURF,12)=W4
      PIKUP(6,SURF,12)=W5
   ELSE
   END IF
!
!       NOW ALL OLD SOLVES AND CURVATURE DATA IS GONE
   IF(WQ.EQ.'PRO') THEN
   ELSE
   END IF
   IF(WQ.EQ.'NPRO') THEN
   ELSE
   END IF
   RETURN
END
! SUB PIKCOAT.FOR
SUBROUTINE PIKCOAT
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!       THIS IS SUBROUTINE PIKCOAT. THIS IS THE SUBROUTINE WHICH
!       HANDLES SURFACE COATING PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       ONLY NUMERIC WORD 1 AND 5 ARE VALID FOR A GLASS COATING
!
   IF(DF2.EQ.0.OR.DF3.EQ.0.OR.&
   &DF4.EQ.0) THEN
      OUTLYNE=&
      &'"PIKUP COATING" ONLY USES NUMERIC WORD #1 AND #5 DATA'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!       SINCE THIS IS A COATING PIKUP,
!       THE THIRD DIMENSION WILL BE 44
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!               INCREMENT THE PIKUP COUNTER BY 1.0D0
!               I.E. ALENS(32,SURF)
   ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
   PIKUP(1,SURF,44)=1.0D0
   PIKUP(2,SURF,44)=W1
   PIKUP(3,SURF,44)=W2
   PIKUP(4,SURF,44)=W3
   PIKUP(5,SURF,44)=W4
   PIKUP(6,SURF,44)=W5
   F22=1
!       THERE ARE NO SOLVES FOR GLASS TYPE
!       SO THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
   RETURN
END
! SUB PIKGLS.FOR
SUBROUTINE PIKGLS
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!       THIS IS SUBROUTINE PIKGLS. THIS IS THE SUBROUTINE WHICH
!       HANDLES MATERIAL AND INDEX PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!       PIKUP GLASS IS A SURFACE TERMINAL COMMAND IN THE LENS INPUT
!       ROUTINE F5=1. IT CAUSES THE SURFACE COUNTING VARIABLE
!       (SURF) AND SYSTEM(20) TO BE INCREMENTED BY 1 AND 1.0D0
!       AND CAUSES THE NEXT SURFACE DATA TO BE STORED FOR THE NEXT
!       SURFACE. AT F6=1 (UPDATE ROUTINE) THIS INCREMENTING DOES
!       NO OCCUR. THE LAST SURFACE FOR WHICH DATA MAY BE ENTERED
!       IS SURFACE MAXSUR. SURF AND SYSTEM(20) ARE NOT INCREMENTED
!       PASSED MAXSUR SO IF AN ATTEMPT IS MADE TO INPUT PASSED
!       SURFACE MAXSUR IN THE LENS INPUT MODE, THE DATA FOR SURFACE
!       MAXSUR WILL BE OVERWRITTEN.
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       ONLY NUMERIC WORD 1 AND 5 ARE VALID FOR A GLASS PIKUP
!
   IF(DF2.EQ.0.OR.DF3.EQ.0.OR.&
   &DF4.EQ.0) THEN
      OUTLYNE=&
      &'"PIKUP GLASS" ONLY USES NUMERIC WORD #1 AND #5 DATA'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!       SINCE THIS IS A GLASSPIKUP,
!       THE THIRD DIMENSION WILL BE 20
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!               INCREMENT THE PIKUP COUNTER BY 1.0D0
!               I.E. ALENS(32,SURF)
   ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
   PIKUP(1,SURF,20)=1.0D0
   PIKUP(2,SURF,20)=W1
   PIKUP(3,SURF,20)=W2
   PIKUP(4,SURF,20)=W3
   PIKUP(5,SURF,20)=W4
   PIKUP(6,SURF,20)=W5
   F22=1
   IF(F5.EQ.1.AND.SURF.LT.MAXSUR) THEN
      SURF=SURF+1
      ALENS(1:LSIZ,SURF)=0.0D0
      SYSTEM(20)=DBLE(SURF)
   ELSE
!       DON'T INCREMENT SURF AND SYSTEM(20)
   END IF
!       THERE ARE NO SOLVES FOR GLASS TYPE
!       SO THERE ARE NO SOLVE DELETIONS TO HANDEL.
!
!
   RETURN
END
! SUB PIKGRT.FOR
SUBROUTINE PIKGRT
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
   INTEGER I
!
!       THIS IS SUBROUTINE PIKGRT. THIS IS THE SUBROUTINE WHICH
!       HANDLES GRT PIKUPS AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
!
   IF(SURF.EQ.INT(W1)) THEN
      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       ONLY NUMERIC WORD 1 AND 5 ARE VALID FOR A GRT PIKUP
!
   IF(DF2.EQ.0.OR.DF3.EQ.0.OR.&
   &DF4.EQ.0) THEN
      OUTLYNE=&
      &'"PIKUP GRT" ONLY USES NUMERIC WORD #1 AND #5 DATA'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!       SINCE THIS IS A GRT PIKUP,
!       THE THIRD DIMENSION WILL BE 43
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
   W2=0.0D0
   W3=0.0D0
   W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!               INCREMENT THE PIKUP COUNTER BY 1.0D0
!               I.E. ALENS(32,SURF)
   ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
   PIKUP(1,SURF,43)=1.0D0
   PIKUP(2,SURF,43)=W1
   PIKUP(3,SURF,43)=0.0D0
   PIKUP(4,SURF,43)=0.0D0
   PIKUP(5,SURF,43)=0.0D0
   PIKUP(6,SURF,43)=W5
!
   RETURN
END
! SUB PIKCVT.FOR
SUBROUTINE PIKCVT
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIKCVT. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE TORIC CURVATURE (CVTOR) AND TORIC RADIUS (RDTOR)
!       PIKUP AT THE LENS INPUT
!       AND UPDATE LENS LEVEL.
!
   INTEGER FSURF
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA,SOLVE AND
!       PIKUP ITEMS.
!
!       TEST TO SEE IF THE SURFACE FROM WHICH WE ARE PIKING UP
!       DATA IS THE SAME AS THE SURFACE TO WHICH THE PIKUP
!       WILL BE ASSINGED. OPERATION NOT LEGAL
!
   IF(SURF.EQ.INT(W1)) THEN

      OUTLYNE='A SURFACE CAN NOT PIKUP ITS OWN VALUES'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!       THE PIKUP EXISTENCE FLAG IS PIKUP(1,I,J) FOR EVERY
!       PIKUP ON EVERY SURFACE. IF IT IS 1.0D0 THE PIKUP EXISTS
!       IF IT IS 0.0D0 THE PIKUP DOES NOT EXIST
!
!       STORE NUMERIC INPUT AND HANDEL DEFAULT DATA

!       IF DF1=1 THIS MEANS PIKING UP FROM THE OBJECT SURFACE
!       ON TO THE CURRENT SURFACE
   IF(DF1.EQ.1) W1=0.0D0
!       IF DF2=1 THIS MEANS MULTIPLIER IS UNITY
   IF(DF2.EQ.1) W2=1.0D0
!       IF DF3=1 TIS MEANS ADDITIVE CONSTANT ZERO
   IF(DF3.EQ.1) W3=0.0D0
!       IF DF4=1 THEN W4=0.0D0
!       IF DF5=1 THEN W5=0.0D0
   IF(DF4.EQ.1) W4=0.0D0
   IF(DF5.EQ.1) W5=0.0D0
   IF(W1.LT.0.0D0) W1=SURF+W1
   IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN

      OUTLYNE='SOURCE SURFACE NUMBER BEYOND LEGAL RANGE'
      CALL SHOWIT(1)
      OUTLYNE='PIKUP NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       IF THE SURFACE BEING PIKED UP FROM IS NOT A TORIC
!       THEN THE PIKUP IS NOT ALLOWED. CHECK STATUS OF
!       ALENS(23, ) FOR THAT SURFACE.
!
   FSURF=INT(W1)
   IF(ALENS(23,FSURF).EQ.0.0D0) THEN
      OUTLYNE=&
      &'SURFACE TO BE PIKED UP FROM IS NOT DEFINED AS TORIC'
      CALL SHOWIT(1)
      OUTLYNE='TORIC PIKUP IS NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       IF PIKING SURFACE IS NOT A TORIC, THE CVTOR/RDTOR PIKUP IS
!       NOT ALLOWED.
   IF(ALENS(23,SURF).NE.1.0D0.AND.&
   &ALENS(23,SURF).NE.2.0D0) THEN
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :NOT DEFINED AS TORIC'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'('//WQ(1:5)//') PIKUP NOT ALLOWED ON NON-TORIC SURFACE'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!               INCR. PIKUP INDICATOR BY 1.0D0
!               I.E. ALENS(32,SURF)
   IF(WQ.EQ.'CVTOR') THEN
      IF(PIKUP(1,SURF,10).EQ.0.0D0)&
      &ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
      PIKUP(1,SURF,10)=0.0D0
      PIKUP(1,SURF,9)=1.0D0
      PIKUP(2,SURF,9)=W1
      PIKUP(3,SURF,9)=W2
      PIKUP(4,SURF,9)=W3
      PIKUP(5,SURF,9)=W4
      PIKUP(6,SURF,9)=W5
   END IF
   IF(WQ.EQ.'RDTOR') THEN
      IF(PIKUP(1,SURF,9).EQ.0.0D0)&
      &ALENS(32,SURF)=ALENS(32,SURF)+1.0D0
      PIKUP(1,SURF,9)=0.0D0
      PIKUP(1,SURF,10)=1.0D0
      PIKUP(2,SURF,10)=W1
      PIKUP(3,SURF,10)=W2
      PIKUP(4,SURF,10)=W3
      PIKUP(5,SURF,10)=W4
      PIKUP(6,SURF,10)=W5
   END IF
!       DUMP PIKUP PRO AND NPRO IF FOUND
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1:6,SURF,11)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1:6,SURF,12)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0

      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
!
!       IF PIKING SURFACE IS NOT A TORIC, IT WILL BE REDEFINED AS ONE
!       DURING THE RECONCILLIATION AT (EOS) WHEN ALL PIKUPS ARE
!       RECONCILLED.
!
!       TORIC INDICATOR SET CORRECTLY NOW
!
!       THERE MAY BE SOLVES TO DELETE. SINCE THIS
!       SURFACE IS NOW A TORIC (Y OR X) WITH PIKED UP DATA
!       THEN IF IT HAD A CURVATURE SOLVE ON IT, THAT CURVATURE
!       SOLVE MAY NEED TO BE REMOVED. THERE ARE DIFFERENT
!       COMBINATIONS OF TORIC TYPES, PIKUPS AND SOLVE TO
!       CONSIDER.
!
!       FIRST, IF THE TORIC TYPE ON SURFACE SURF IS A
!               Y-TORIC. FOR A Y-TORIC
!       PIKING UP THR CVTOR EFFECTS THE TORIC CURVATURE OF
!       REVOLUTION IN THE XZ PLANE SINCE THE REVOLUTION
!       AXIS IS IN THE Y DIRECTION. THUS THIS PIKUP
!       WOULD ONLY DELETE A CURVATURE SOLVE IN THE XZ PLANE
!
!       SECOND, IF THE TORIC TYPE ON SURFACE SURF IS AN
!               X-TORIC. FOR AN X-TORIC
!       PIKING UP THE CVTOR EFFECTS THE TORIC CURVATURE OF
!       REVOLUTION IN THE YZ PLANE SINCE THE REVOLUTION
!       AXIS IS IN THE X DIRECTION. THUS THIS PIKUP
!       WOULD ONLY DELETE A CURVATURE SOLVE IN THE YZ PLANE
!
   IF(ALENS(23,SURF).EQ.1.0D0) THEN
!       SURF IS Y-TORIC, REMOVE ANY XZ SOLVES
      IF(SOLVE(2,SURF).GT.0.0D0) THEN
         SOLVE(2,SURF)=0.0D0
         SOLVE(1,SURF)=0.0D0
         WRITE(OUTLYNE,*)&
         &'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVES DELETED'
         CALL SHOWIT(1)
      END IF
   END IF
   IF(ALENS(23,SURF).EQ.2.0D0) THEN
!       SURF IS X-TORIC, REMOVE ANY YZ SOLVES
      IF(SOLVE(8,SURF).GT.0.0D0) THEN
         SOLVE(8,SURF)=0.0D0
         SOLVE(9,SURF)=0.0D0
         WRITE(OUTLYNE,*)&
         &'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVES DELETE'
         CALL SHOWIT(1)
      END IF
   END IF
!       RESOLVE ALENS(33,SURF)
   ALENS(33,SURF)=0.0D0
   IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
   IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
   IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
   IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
!
!       SOLVE CONFLICTS RESOLVED
!
   RETURN
END
