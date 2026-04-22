!       SIXTH FILE FOR LENS DATABASE MANAGER FILES

! SUB SLABEL.FOR
SUBROUTINE SLABEL
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SLABEL WHICH IMPLEMENTS THE LBL/LABEL
!       COMMAND
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
   IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
      LBL(SURF)(1:80)=WS(1:80)
      IF(WS.NE.CNULL) ALENS(44,SURF)=1.0D0
      IF(WS.EQ.CNULL) ALENS(44,SURF)=0.0D0
   ELSE
!       NOT LABEL OR LBL
   END IF
   RETURN
END
! SUB SFNO.FOR
SUBROUTINE SFNO
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SFNO WHICH IMPLEMENTS THE FNO(X OR Y) COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE FNO(X OR Y) COMMAND AT
!       THE CMD LEVEL
!
!
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
         OUTLYNE='AT THE CMD LEVEL, "FNOY" AND "FNOX"'
         CALL SHOWIT(1)
         OUTLYNE='TAKE NO EXPLICIT INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      IF(STI.EQ.1.OR.STI.EQ.0) THEN
         IF(WC.EQ.'FNOY') THEN
            IF(SYSTEM(67).NE.1.0D0.AND.SYSTEM(67).NE.3.0D0) THEN
               OUTLYNE='NOTE:'
               CALL SHOWIT(1)
               OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
               CALL SHOWIT(1)
               SYSTEM(68)=1.0D0/((2.0D0*SYSTEM(12))/ALENS(3,0))
               SYSTEM(83)=0.0D0
               SYSTEM(84)=0.0D0
            ELSE
            END IF
            WRITE(OUTLYNE,2000) SYSTEM(68)
            CALL SHOWIT(0)
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            IF(SYSTEM(67).NE.2.0D0.AND.SYSTEM(67).NE.3.0D0) THEN
               OUTLYNE='NOTE:'
               CALL SHOWIT(1)
               OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
               CALL SHOWIT(1)
               SYSTEM(69)=1.0D0/((2.0D0*SYSTEM(13))/ALENS(3,0))
               SYSTEM(83)=0.0D0
               SYSTEM(84)=0.0D0
            END IF
            WRITE(OUTLYNE,3000) SYSTEM(69)
            CALL SHOWIT(0)
            RETURN
         END IF
      END IF
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(STI.EQ.1) THEN
         OUTLYNE=&
         &'QUERRY OBJECT F-NUMBER VALUES FROM THE CMD LEVEL WITH THE'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"FNOY" OR "FNOX" COMMANDS'
         CALL SHOWIT(1)
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            OUTLYNE='"FNOY" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            OUTLYNE='"FNOX" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1 &
      &.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            OUTLYNE='"FNOY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            OUTLYNE='"FNOX" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.1.AND.F5.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            OUTLYNE='"FNOY" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            OUTLYNE='"FNOX" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(F6.EQ.1) THEN
         IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
!
            IF(WC.EQ.'FNOY') THEN
               OUTLYNE='INVALID QUALIFIER WORD USED WITH "FNOY"'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'FNOX') THEN
               OUTLYNE='INVALID QUALIFIER WORD USED WITH "FNOX"'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
      END IF
      IF(DF1.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            OUTLYNE='"FNOY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            OUTLYNE='"FNOX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      IF(S1.EQ.0) THEN
         IF(WC.EQ.'FNOY') THEN
            WRITE(OUTLYNE,2000) SYSTEM(68)
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(WC.EQ.'FNOX') THEN
            WRITE(OUTLYNE,3000) SYSTEM(69)
            CALL SHOWIT(0)
            IF(SYSTEM(49).EQ.0.0D0) SYSTEM(49)=1.0D0
            IF(SYSTEM(49).EQ.2.0D0) SYSTEM(49)=3.0D0
         ELSE
         END IF
         RETURN
      ELSE
         IF(W1.EQ.0.0D0) THEN
            IF(WC.EQ.'FNOY') THEN
               OUTLYNE='"FNOY" MAY NOT BE SET TO ZERO'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'FNOX') THEN
               OUTLYNE='"FNOX" MAY NOT BE SET TO ZERO'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'FNOY') THEN
               IF(SYSTEM(67).NE.3.0D0.AND.SYSTEM(67).NE.1.0D0) THEN
                  IF(SYSTEM(67).EQ.0.0D0) SYSTEM(67)=1.0D0
                  IF(SYSTEM(67).EQ.2.0D0) SYSTEM(67)=3.0D0
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
            END IF
            IF(WC.EQ.'FNOX') THEN
               IF(SYSTEM(67).NE.3.0D0.AND.SYSTEM(67).NE.2.0D0) THEN
                  IF(SYSTEM(67).EQ.0.0D0) SYSTEM(67)=2.0D0
                  IF(SYSTEM(67).EQ.1.0D0) SYSTEM(67)=3.0D0
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
            END IF
            SYSTEM(64)=0.0D0
            IF(WC.EQ.'FNOY') SYSTEM(68)=DABS(W1)
            IF(WC.EQ.'FNOX') SYSTEM(69)=DABS(W1)
         END IF
         IF(WQ.EQ.'DELT') THEN
            SYSTEM(64)=0.0D0
            IF(WC.EQ.'FNOY') SYSTEM(68)=SYSTEM(68)+(W1)
            IF(WC.EQ.'FNOX') SYSTEM(69)=SYSTEM(69)+(W1)
         END IF
         IF(WQ.EQ.'CENT') THEN
            SYSTEM(64)=0.0D0
            IF(WC.EQ.'FNOY') SYSTEM(68)=SYSTEM(68)+(W1*0.0D0*SYSTEM(68))
            IF(WC.EQ.'FNOX') SYSTEM(69)=SYSTEM(69)+(W1*0.0D0*SYSTEM(69))
         END IF
         RETURN
      END IF
   END IF
2000 FORMAT('FNOY=',1X,D23.15)
3000 FORMAT('FNOX=',1X,D23.15)
   RETURN
END
! SUB SFNB.FOR
SUBROUTINE SFNB
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SFNB WHICH IMPLEMENTS THE FNBY AND FNBX
!       COMMANDS AT THE CMD LEVEL. (ALSO FNBY HLD AND FNBX HLD)
!
   REAL*8 FN,ABSSYS
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(STI.EQ.1) THEN
      OUTLYNE=&
      &'QUERRY F-NUMBER VALUES FROM THE CMD LEVEL WITH THE'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'"SHO" OR "GET" AND "WRITE" COMMANDS'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
      IF(WC.EQ.'FNBY') THEN
         OUTLYNE=&
         &'"FNBY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FNBX') THEN
         OUTLYNE=&
         &'"FNBX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
!
      IF(WC.EQ.'FNBY') THEN
         OUTLYNE=&
         &'"FNBY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FNBX') THEN
         OUTLYNE=&
         &'"FNBX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FNBX'.AND.SYSTEM(64).NE.0.0D0) THEN
         OUTLYNE=&
         &'"FNBX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "NAOX"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FNBY'.AND.SYSTEM(64).NE.0.0D0) THEN
         OUTLYNE=&
         &'"FNBY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "NAOY"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FNBX'.AND.SYSTEM(67).NE.0.0D0) THEN
         OUTLYNE=&
         &'"FNBX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "FNOX"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FNBY'.AND.SYSTEM(67).NE.0.0D0) THEN
         OUTLYNE=&
         &'"FNBY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "FNOY"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
   END IF
   IF(SYSTEM(83).NE.0.0D0.OR.SYSTEM(84).NE.0.0D0) THEN
      OUTLYNE='SAY OR SAX IS CURRENTLY FLOATING'
      CALL SHOWIT(1)
      OUTLYNE='"FNBY/FNBX" ADJUSTMENT NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.&
   &DF4.EQ.1.AND.DF5.EQ.1) THEN
!       FNBY OR FNBX TYPED IN WITH NO INPUT,PRINTS OUT CURRENT VALUES
      IF(WC.EQ.'FNBY'.AND.SYSTEM(44).NE.1.0D0.AND.&
      &SYSTEM(44).NE.-1.0D0) THEN
         IF(DABS(PXTRAY(2,(INT(SYSTEM(20))))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,100)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAY(2,(INT(SYSTEM(20))))))
         END IF
         WRITE(OUTLYNE,200)FN
         CALL SHOWIT(0)
         RETURN
      END IF
      ABSSYS=DABS(SYSTEM(44))
      IF(WC.EQ.'FNBY'.AND.ABSSYS.EQ.1.0D0) THEN
         IF(DABS(PXTRAY(2,(INT(SYSTEM(20))))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,100)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAY(2,(INT(SYSTEM(20))))))
         END IF
         IF(SYSTEM(44).GT.0.0D0) THEN
            WRITE(OUTLYNE,200) FN
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(44).LT.0.0D0) THEN
            WRITE(OUTLYNE,300) SYSTEM(46)
            CALL SHOWIT(0)
         END IF
         RETURN
      END IF
      IF(WC.EQ.'FNBX'.AND.SYSTEM(45).NE.1.0D0.AND.&
      &SYSTEM(45).NE.-1.0D0) THEN
         IF(DABS(PXTRAX(2,(INT(SYSTEM(20))))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,101)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAX(2,(INT(SYSTEM(20))))))
         END IF
         WRITE(OUTLYNE,201) FN
         CALL SHOWIT(0)
         RETURN
      END IF
      ABSSYS=DABS(SYSTEM(45))
      IF(WC.EQ.'FNBX'.AND.ABSSYS.EQ.1.0D0) THEN
         IF(DABS(PXTRAX(2,(INT(SYSTEM(20))))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,101)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAX(2,(INT(SYSTEM(20))))))
         END IF
         IF(SYSTEM(45).GT.0.0D0) THEN
            WRITE(OUTLYNE,201) FN
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(45).LT.0.0D0) THEN
            WRITE(OUTLYNE,301) SYSTEM(47)
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
      END IF
!
   ELSE
!       NUMERIC INPUT AND QUALIFIER INPUT NOT DEFAULT
      IF(WQ.NE.'DELK'.AND.WQ.NE.'HLD'.AND.SQ.NE.0) THEN
         OUTLYNE='INVALID QUALIFIER INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'DELK') THEN
         IF(WC.EQ.'FNBY') THEN
            IF(W1.EQ.0.0D0) THEN
               OUTLYNE=&
               &'F/0.0 IS MEANINGLESS, ADJUSTMENT NOT PERFORMED'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!       CHECK LENS MODE, MUST BE FOCAL OR UFOCAL
            IF(SYSTEM(30).EQ.3.0.OR.SYSTEM(30).EQ.4.0) THEN
               IF(WC.EQ.'FNBY') THEN
                  OUTLYNE='"FNBY" REQUIRES THE FOCAL OR UFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(SYSTEM(30).EQ.3.0) THEN
                     OUTLYNE='CURRENT MODE IS AFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(SYSTEM(30).EQ.4.0) THEN
                     OUTLYNE='CURRENT MODE IS UAFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  OUTLYNE='"FNBY" ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
               IF(WC.EQ.'FNBX') THEN
                  OUTLYNE='"FNBX" REQUIRES THE FOCAL OR UFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(SYSTEM(30).EQ.3.0) THEN
                     OUTLYNE='CURRENT MODE IS AFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(SYSTEM(30).EQ.4.0) THEN
                     OUTLYNE='CURRENT MODE IS UAFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  OUTLYNE='"FNBX" ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
            ELSE
            END IF
!
            IF(WQ.NE.'HLD') SYSTEM(44)=1.0D0
            IF(WQ.EQ.'HLD') SYSTEM(44)=-1.0D0
            SYSTEM(46)=W1
!       CALL THE SUBROUTINE FYADJ TO PERFORM THE F NUMBER ADJUSTMENT
!       DO A PARAXIAL TRACE TO GET STARTING VALUES
            ITYPEP=1
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            ITYPEP=1
            CALL FADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            RETURN
         ELSE
!       WC MUST BE 'FNBX'
            IF(W1.EQ.0.0D0) THEN
               OUTLYNE=&
               &'F/0.0 IS MEANINGLESS, ADJUSTMENT NOT PERFORMED'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WQ.NE.'HLD') SYSTEM(45)=1.0D0
            IF(WQ.EQ.'HLD') SYSTEM(45)=-1.0D0
            SYSTEM(47)=W1
!       CALL SUBROUTINE FXADJ TO ADJUST THE F NUMBER IN THE X PLANE
!       DO A PARAXIAL TRACE TO GET STARTING VALUES.
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            ITYPEP=2
            CALL FADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
         END IF
         RETURN
      ELSE
!       WQ IS DEL
         IF(WC.EQ.'FNBY') THEN
            ABSSYS=DABS(SYSTEM(44))
            IF(ABSSYS.EQ.1.0D0) THEN
               OUTLYNE='"FNBY" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               SYSTEM(44)=0.0D0
               SYSTEM(46)=0.0D0
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO FNBY ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
!       WC WAS FNBX
            ABSSYS=DABS(SYSTEM(45))
            IF(ABSSYS.EQ.1.0D0) THEN
               OUTLYNE='"FNBX" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               SYSTEM(45)=0.0D0
               SYSTEM(47)=0.0D0
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO FNBX ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!
100 FORMAT('F-NUMBER (YZ-PLANE) IS INFINITE')
101 FORMAT('F-NUMBER (YX-PLANE) IS INFINITE')
200 FORMAT('F-NUMBER (YZ-PLANE) =',G12.5)
201 FORMAT('F-NUMBER (XZ-PLANE) =',G12.5)
300 FORMAT('F-NUMBER HOLD (YZ-PLANE) =',G12.5)
301 FORMAT('F-NUMBER HOLD (XZ-PLANE) =',G12.5)
!
   RETURN
END
! SUB PIVAXIS.FOR
SUBROUTINE PIVAXIS
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIVAXIS WHICH IMPLEMENTS THE
!       "PIVAXIS"
!       COMMAND AT THE CMD LEVEL.
!
   INTEGER AMODE
!
   IF(STI.EQ.1.OR.SQ.EQ.0) THEN
      IF(ALENS(113,SURF).EQ.0.0D0)&
      &OUTLYNE='"PIVAXIS" IS SET TO "VERTEX"'
      IF(ALENS(113,SURF).EQ.1.0D0)&
      &OUTLYNE='"PIVAXIS" IS SET TO "NORMAL"'
      CALL SHOWIT(0)
      RETURN
   END IF
!
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      OUTLYNE='"PIVAXIS" TAKES NO STRING OR NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1) THEN
      IF(WQ.NE.'VERTEX'.AND.WQ.NE.'NORMAL') THEN
         OUTLYNE='INVALID QUALIFIER INPUT USED WITH "PIVAXIS"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!
   IF(WQ.EQ.'VERTEX')THEN
      ALENS(113,SURF)=0.0D0
   END IF
   IF(WQ.EQ.'NORMAL')THEN
      ALENS(113,SURF)=1.0D0
   END IF
   RETURN
END
! SUB SER.FOR
SUBROUTINE SER
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SER WHICH IMPLEMENTS THE ERY AND ERX
!       COMMANDS AT THE CMD LEVEL.
!
   CHARACTER UNIT*5
!
   REAL*8 ABSSYS,EP
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
   IF(SYSTEM(6).EQ.1.0) UNIT='IN   '
   IF(SYSTEM(6).EQ.2.0) UNIT='CM   '
   IF(SYSTEM(6).EQ.3.0) UNIT='MM'
   IF(SYSTEM(6).EQ.4.0) UNIT='M    '
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(STI.EQ.1) THEN
      OUTLYNE=&
      &'QUERRY EXIT PUPIL VALUES FROM THE CMD LEVEL WITH THE'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'"SHO" OR "GET" AND "WRITE" COMMANDS'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
!
      IF(WC.EQ.'ERY') THEN
         OUTLYNE=&
         &'"ERY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'ERX') THEN
         OUTLYNE=&
         &'"ERX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!
   IF(DF1.EQ.1) THEN
!
      IF(WC.EQ.'ERY') THEN
         OUTLYNE=&
         &'"ERY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'ERX') THEN
         OUTLYNE=&
         &'"ERX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'ERX'.AND.SYSTEM(64).NE.0.0D0) THEN
      OUTLYNE=&
      &'"ERX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "NAOX"'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'ERY'.AND.SYSTEM(64).NE.0.0D0) THEN
      OUTLYNE=&
      &'"ERY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "NAOY"'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'ERX'.AND.SYSTEM(67).NE.0.0D0) THEN
      OUTLYNE=&
      &'"ERX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "FNOX"'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'ERY'.AND.SYSTEM(67).NE.0.0D0) THEN
      OUTLYNE=&
      &'"ERY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "FNOY"'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SYSTEM(83).NE.0.0D0.OR.SYSTEM(84).NE.0.0D0) THEN
      OUTLYNE='SAY OR SAX IS CURRENTLY FLOATING'
      CALL SHOWIT(1)
      OUTLYNE='"ERY/ERX" ADJUSTMENT NOT ALLOWED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.&
   &DF4.EQ.1.AND.DF5.EQ.1) THEN
!       ERY OR ERX TYPED IN WITH NO INPUT,PRINTS OUT CURRENT VALUES
      IF(WC.EQ.'ERY'.AND.SYSTEM(44).NE.2.0.AND.&
      &SYSTEM(44).NE.-2.0) THEN
!       CALCULATE EXIT PUPIL RADIUS
         EP=PXTRAY(1,(INT(SYSTEM(20))))-&
         &(PXTRAY(2,(INT(SYSTEM(20))))*(PXTRAY(5,(INT(SYSTEM(20))))/&
         &PXTRAY(6,(INT(SYSTEM(20))))))
         WRITE(OUTLYNE,200) EP,UNIT
         CALL SHOWIT(0)
         RETURN
      ELSE
      END IF
      ABSSYS=DABS(SYSTEM(44))
      IF(WC.EQ.'ERY'.AND.ABSSYS.EQ.2.0) THEN
         EP=PXTRAY(1,(INT(SYSTEM(20))))-&
         &(PXTRAY(2,(INT(SYSTEM(20))))*(PXTRAY(5,(INT(SYSTEM(20))))/&
         &PXTRAY(6,(INT(SYSTEM(20))))))
         IF(SYSTEM(44).GT.0.0D0) THEN
            WRITE(OUTLYNE,200) EP,UNIT
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(44).LT.0.0D0) THEN
            WRITE(OUTLYNE,300) SYSTEM(46),UNIT
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
      END IF
      IF(WC.EQ.'ERX'.AND.SYSTEM(45).NE.2.0.AND.&
      &SYSTEM(45).NE.-2.0) THEN
         EP=PXTRAX(1,(INT(SYSTEM(20))))-&
         &(PXTRAX(2,(INT(SYSTEM(20))))*(PXTRAX(5,(INT(SYSTEM(20))))/&
         &PXTRAX(6,(INT(SYSTEM(20))))))
         WRITE(OUTLYNE,201) EP,UNIT
         CALL SHOWIT(0)
         RETURN
      END IF
      ABSSYS=DABS(SYSTEM(45))
      IF(WC.EQ.'ERX'.AND.ABSSYS.EQ.2.0) THEN
         EP=PXTRAX(1,(INT(SYSTEM(20))))-&
         &(PXTRAX(2,(INT(SYSTEM(20))))*(PXTRAX(5,(INT(SYSTEM(20))))&
         &/PXTRAX(6,(INT(SYSTEM(20))))))
         IF(SYSTEM(45).GT.0.0D0) THEN
            WRITE(OUTLYNE,201) EP,UNIT
            CALL SHOWIT(0)
         END IF
         IF(SYSTEM(45).LT.0.0D0) THEN
            WRITE(OUTLYNE,301) SYSTEM(47),UNIT
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
      END IF
!
   ELSE
!       NUMERIC INPUT AND QUALIFIER INPUT NOT DEFAULT
      IF(WQ.NE.'DELK'.AND.WQ.NE.'HLD'.AND.SQ.NE.0) THEN
         OUTLYNE='INVALID QUALIFIER INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'DELK') THEN
         IF(WC.EQ.'ERY') THEN
            IF(W1.EQ.0.0D0) THEN
               OUTLYNE='ZERO DIAMETER EXIT PUPIL RADIUS IS MEANINGLESS'
               CALL SHOWIT(1)
               OUTLYNE='EXIT PUPIL ADJUSTMENT NOT PERFORMED'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
            IF(SYSTEM(30).EQ.1.0.OR.SYSTEM(30).EQ.2.0) THEN
               IF(WC.EQ.'ERY') THEN
                  OUTLYNE='"ERY" REQUIRES THE AFOCAL OR UAFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(SYSTEM(30).EQ.1.0) THEN
                     CALL SHOWIT(1)
                     OUTLYNE='CURRENT MODE IS FOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(SYSTEM(30).EQ.2.0) THEN
                     OUTLYNE='CURRENT MODE IS UFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  OUTLYNE='"ERY" ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
               END IF
               IF(WC.EQ.'ERX') THEN
                  OUTLYNE='"ERX" REQUIRES THE AFOCAL OR UAFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(SYSTEM(30).EQ.1.0) THEN
                     OUTLYNE='CURRENT MODE IS FOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(SYSTEM(30).EQ.2.0) THEN
                     OUTLYNE='CURRENT MODE IS UFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  OUTLYNE='"ERX" ADJUSTMENT NOT PERFORMED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
               END IF
            ELSE
            END IF
            IF(WQ.NE.'HLD') SYSTEM(44)=2.0
            IF(WQ.EQ.'HLD') SYSTEM(44)=-2.0
            SYSTEM(46)=W1
!       PERFORM ERY ADJUSTMENT BUT
!       FIRST DO A PARAXIAL TRACE FOR STARTING VALUES
            ITYPEP=1
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            CALL ERADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            RETURN
         ELSE
!       WC MUST BE 'ERX'
            IF(W1.EQ.0.0D0) THEN
               OUTLYNE='ZERO DIAMETER EXIT PUPIL RADIUS IS MEANINGLESS'
               CALL SHOWIT(1)
               OUTLYNE='ADJUSTMENT NOT PERFORMED'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
            IF(WQ.NE.'HLD') SYSTEM(45)=2.0
            IF(WQ.EQ.'HLD') SYSTEM(45)=-2.0
            SYSTEM(47)=W1
!               PERFORM ERX ADJUSTMENT
!       FIRST DO A PARAXIAL TRACE FOR STARTING VALUES
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            CALL ERADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(SYSTEM(63).EQ.1.0D0) THEN
               IF(SYSTEM(64).EQ.0.0D0.AND.SYSTEM(67).EQ.0.0D0) THEN
                  OUTLYNE='WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'
                  CALL SHOWIT(1)
                  OUTLYNE='TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'
                  CALL SHOWIT(1)
                  OUTLYNE='VALUES'
                  CALL SHOWIT(1)
                  OUTLYNE='PARAXIAL TRACE STOPPED'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  IF(SYSTEM(64).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                  END IF
                  IF(SYSTEM(64).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(64).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)*SYSTEM(65)
                     SYSTEM(13)=ALENS(3,0)*SYSTEM(66)
                  END IF
                  IF(SYSTEM(67).EQ.1.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                  END IF
                  IF(SYSTEM(67).EQ.2.0D0) THEN
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
                  IF(SYSTEM(67).EQ.3.0D0) THEN
                     SYSTEM(12)=ALENS(3,0)/(2.0D0*SYSTEM(68))
                     SYSTEM(13)=ALENS(3,0)/(2.0D0*SYSTEM(69))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
         END IF
         RETURN
      ELSE
!       WQ IS DEL
         IF(WC.EQ.'ERY') THEN
            ABSSYS=DABS(SYSTEM(44))
            IF(ABSSYS.EQ.2.0) THEN
               OUTLYNE='"ERY" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               SYSTEM(44)=0.0
               SYSTEM(46)=0.0
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO ERY ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
!       WC WAS ERX
            ABSSYS=DABS(SYSTEM(45))
            IF(ABSSYS.EQ.2.0) THEN
               OUTLYNE='"ERX" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               SYSTEM(45)=0.0
               SYSTEM(47)=0.0
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO ERX ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!
200 FORMAT('EXIT PUPIL RADIUS (YZ-PLANE) =',G12.5,1X,A5)
201 FORMAT('EXIT PUPIL RADIUS (XZ-PLANE) =',G12.5,1X,A5)
300 FORMAT('EXIT PUPIL RADIUS HOLD (YZ-PLANE) =',G12.5,1X,A5)
301 FORMAT('EXIT PIPIL RADIUS HOLD (XZ-PLANE) =',G12.5,1X,A5)
!
   RETURN
END
! SUB SDEFG.FOR
SUBROUTINE SDEFG
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SDEFG WHICH IMPLEMENTS THE AC,AD,
!       AE,AF,AG,ADTOR,AETOR,AFTOR AND AGTOR
!       COMMANDS AT THE UPDATE LENS LEVEL.
!
   INTEGER CT,PIKCNT,I
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
   &.OR.S5.EQ.1) THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'&
      &.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')&
      &OUTLYNE=&
      &'AC,AD,AE,AF AND AG TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'&
      &.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')&
      &OUTLYNE=&
      &'AH,AI,AJ,AK AND AL TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
      &.OR.WC.EQ.'AGTOR')&
      &OUTLYNE=&
      &'ADTOR, AETOR, AFTOR AND AGTOR '//&
      &'TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SST.EQ.1) THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'&
      &.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')&
      &OUTLYNE=&
      &'AC,AD,AE,AF AND AG TAKE NO STRING INPUT'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'&
      &.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')&
      &OUTLYNE=&
      &'AH,AI,AJ,AK AND AL TAKE NO STRING INPUT'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
      &.OR.WC.EQ.'AGTOR')&
      &OUTLYNE=&
      &'ADTOR, AETOR, AFTOR AND AGTOR '//&
      &'TAKE NO STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'&
      &.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')&
      &OUTLYNE=&
      &'AC,AD,AE,AF AND AG TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'&
      &.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')&
      &OUTLYNE=&
      &'AH,AI,AJ,AK AND AL TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
      &.OR.WC.EQ.'AGTOR')&
      &OUTLYNE=&
      &'ADTOR, AETOR, AFTOR AND AGTOR '//&
      &'TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'&
         &.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')&
         &OUTLYNE=&
         &'INVALID QUALIFIER WORD USED WITH AC,AD,AE,AF OR AG'
         IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'&
         &.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')&
         &OUTLYNE=&
         &'INVALID QUALIFIER WORD USED WITH AH,AI,AJ,AK OR AL'
         IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
         &.OR.WC.EQ.'AGTOR') THEN
            OUTLYNE=&
            &'INVALID QUALIFIER WORD USED WITH'
            OUTLYNE=&
            &'ADTOR, AETOR, AFTOR OR AGTOR'
         END IF
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1)THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'&
      &.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')&
      &OUTLYNE=&
      &'AC,AD,AE,AF OR AG REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'&
      &.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')&
      &OUTLYNE=&
      &'AH,AI,AJ,AK OR AL REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
      &.OR.WC.EQ.'AGTOR') THEN
         OUTLYNE=&
         &'ADTOR,AETOR,AFTOR OR AGTOR REQUIRES EXPLICIT'
         OUTLYNE='NUMERIC WORD #1 INPUT'
      END IF
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!               WE ARE AT LENS UPDATE LEVEL
   IF(WC.EQ.'AC') CT=26
   IF(WC.EQ.'AD') CT=5
   IF(WC.EQ.'AE') CT=6
   IF(WC.EQ.'AF') CT=7
   IF(WC.EQ.'AG') CT=8
   IF(WC.EQ.'AH') CT=27
   IF(WC.EQ.'AI') CT=28
   IF(WC.EQ.'AJ') CT=29
   IF(WC.EQ.'AK') CT=30
   IF(WC.EQ.'AL') CT=31
   IF(WC.EQ.'ADTOR') CT=22
   IF(WC.EQ.'AETOR') CT=23
   IF(WC.EQ.'AFTOR') CT=24
   IF(WC.EQ.'AGTOR') CT=25
!
   IF(WC.EQ.'AD'.OR.WC.EQ.'AF'.OR.WC.EQ.'AE'.OR.&
   &WC.EQ.'AG'.OR.WC.EQ.'AC'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'&
   &.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
      IF(ALENS(8,SURF).NE.1.0D0) THEN
!       SURFACE NOT ASPHERIC, SET IT AS SUCH.
         IF(WC.EQ.'AC') THEN
            IF(ALENS(1,SURF).NE.0.0D0) THEN
               OUTLYNE='WARNING:'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
               CALL SHOWIT(1)
               OUTLYNE=&
               &'THE "AC" TERM WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
               CALL SHOWIT(1)
            END IF
            W5=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=1
            DF5=0
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AD') THEN
            W1=W1
            DF1=0
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AE') THEN
            W2=W1
            DF1=1
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            W1=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AF') THEN
            W3=W1
            DF1=1
            DF2=1
            DF3=0
            DF4=1
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AG') THEN
            W4=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=0
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AH') THEN
            W1=W1
            DF1=0
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AI') THEN
            W2=W1
            DF1=1
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            W1=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AJ') THEN
            W3=W1
            DF1=1
            DF2=1
            DF3=0
            DF4=1
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AK') THEN
            W4=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=0
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AL') THEN
            W5=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=1
            DF5=0
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AC'.OR.WC.EQ.'AD'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.&
         &WC.EQ.'AE')&
         &CALL SASPH
         IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.&
         &WC.EQ.'AL')&
         &CALL SASPH
         RETURN
      ELSE
      END IF
      IF(ALENS(8,SURF).EQ.1.0D0) THEN
         IF(WC.EQ.'AC') THEN
            IF(SQ.EQ.0) ALENS(43,SURF)=W1
            IF(WQ.EQ.'DELT') ALENS(43,SURF)=ALENS(43,SURF)+W1
            IF(WQ.EQ.'CENT')&
            &ALENS(43,SURF)=ALENS(43,SURF)+(W1*0.01D0*ALENS(43,SURF))
            IF(ALENS(1,SURF).NE.0.0D0) THEN
               OUTLYNE='WARNING:'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
               CALL SHOWIT(1)
               OUTLYNE=&
               &'THE "AC" TERM WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'AD') ALENS(4,SURF)=W1
            IF(WC.EQ.'AE') ALENS(5,SURF)=W1
            IF(WC.EQ.'AF') ALENS(6,SURF)=W1
            IF(WC.EQ.'AG') ALENS(7,SURF)=W1
            IF(WC.EQ.'AH') ALENS(81,SURF)=W1
            IF(WC.EQ.'AI') ALENS(82,SURF)=W1
            IF(WC.EQ.'AJ') ALENS(83,SURF)=W1
            IF(WC.EQ.'AK') ALENS(84,SURF)=W1
            IF(WC.EQ.'AL') ALENS(85,SURF)=W1
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'AD') ALENS(4,SURF)=ALENS(4,SURF)+W1
            IF(WC.EQ.'AE') ALENS(5,SURF)=ALENS(5,SURF)+W1
            IF(WC.EQ.'AF') ALENS(6,SURF)=ALENS(6,SURF)+W1
            IF(WC.EQ.'AG') ALENS(7,SURF)=ALENS(7,SURF)+W1
            IF(WC.EQ.'AH') ALENS(81,SURF)=ALENS(81,SURF)+W1
            IF(WC.EQ.'AI') ALENS(82,SURF)=ALENS(82,SURF)+W1
            IF(WC.EQ.'AJ') ALENS(83,SURF)=ALENS(83,SURF)+W1
            IF(WC.EQ.'AK') ALENS(84,SURF)=ALENS(84,SURF)+W1
            IF(WC.EQ.'AL') ALENS(85,SURF)=ALENS(85,SURF)+W1
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'AD')&
            &ALENS(4,SURF)=ALENS(4,SURF)+(W1*0.01D0*ALENS(4,SURF))
            IF(WC.EQ.'AE')&
            &ALENS(5,SURF)=ALENS(5,SURF)+(W1*0.01D0*ALENS(5,SURF))
            IF(WC.EQ.'AF')&
            &ALENS(6,SURF)=ALENS(6,SURF)+(W1*0.01D0*ALENS(6,SURF))
            IF(WC.EQ.'AG')&
            &ALENS(7,SURF)=ALENS(7,SURF)+(W1*0.01D0*ALENS(7,SURF))
            IF(WC.EQ.'AH')&
            &ALENS(81,SURF)=ALENS(81,SURF)+(W1*0.01D0*ALENS(81,SURF))
            IF(WC.EQ.'AI')&
            &ALENS(82,SURF)=ALENS(82,SURF)+(W1*0.01D0*ALENS(82,SURF))
            IF(WC.EQ.'AJ')&
            &ALENS(83,SURF)=ALENS(83,SURF)+(W1*0.01D0*ALENS(83,SURF))
            IF(WC.EQ.'AK')&
            &ALENS(84,SURF)=ALENS(84,SURF)+(W1*0.01D0*ALENS(84,SURF))
            IF(WC.EQ.'AL')&
            &ALENS(85,SURF)=ALENS(85,SURF)+(W1*0.01D0*ALENS(85,SURF))
         END IF
      ELSE
      END IF
   ELSE
!       NOT AC,AD,AE,AF,AG,AH,AI,AJ,AK OR AL
   END IF
   IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
   &.OR.WC.EQ.'AGTOR') THEN
      IF(ALENS(23,SURF).EQ.0.0D0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' NOT A TORIC'
         CALL SHOWIT(1)
         OUTLYNE='RE-DEFINE SURFACE TYPE AS X OR Y-TORIC AND THEN'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(ALENS(36,SURF).NE.1.0D0) THEN
         IF(WC.EQ.'ADTOR') THEN
            W1=W1
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            DF1=0
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AETOR') THEN
            W2=W1
            W1=0.0D0
            W3=0.0D0
            W4=0.0D0
            DF1=1
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AFTOR') THEN
            W3=W1
            W1=0.0D0
            W2=0.0D0
            W4=0.0D0
            DF1=1
            DF2=1
            DF3=0
            DF4=1
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AGTOR') THEN
            W4=W1
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            DF1=1
            DF2=1
            DF3=1
            DF4=0
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         CALL STASPH
      ELSE
      END IF
      IF(ALENS(36,SURF).EQ.1.0D0) THEN
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'ADTOR') ALENS(37,SURF)=W1
            IF(WC.EQ.'AETOR') ALENS(38,SURF)=W1
            IF(WC.EQ.'AFTOR') ALENS(39,SURF)=W1
            IF(WC.EQ.'AGTOR') ALENS(40,SURF)=W1
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'ADTOR') ALENS(37,SURF)=ALENS(37,SURF)+W1
            IF(WC.EQ.'AETOR') ALENS(38,SURF)=ALENS(38,SURF)+W1
            IF(WC.EQ.'AFTOR') ALENS(39,SURF)=ALENS(39,SURF)+W1
            IF(WC.EQ.'AGTOR') ALENS(40,SURF)=ALENS(40,SURF)+W1
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'ADTOR')&
            &ALENS(37,SURF)=ALENS(37,SURF)+(W1*0.01D0*ALENS(37,SURF))
            IF(WC.EQ.'AETOR')&
            &ALENS(38,SURF)=ALENS(38,SURF)+(W1*0.01D0*ALENS(38,SURF))
            IF(WC.EQ.'AFTOR')&
            &ALENS(39,SURF)=ALENS(39,SURF)+(W1*0.01D0*ALENS(39,SURF))
            IF(WC.EQ.'AGTOR')&
            &ALENS(40,SURF)=ALENS(40,SURF)+(W1*0.01D0*ALENS(40,SURF))
         END IF
      ELSE
      END IF
   ELSE
!       NOT ADTOR,AETOR,AFTOR OR AGTOR
   END IF
!
!       IF THERE ARE PIKUPS FOR AD,AE,AF,AG,AH,AI,AJ,AK OR AL
!       THEN DELETE THEM.
!
!       CHECK FOR ANY PIKUPS
!
!       CHECK FOR PIKUP
   IF(PIKUP(1,SURF,CT).EQ.1.0D0)THEN
!       DELETE PIKUP
      PIKUP(1:6,SURF,CT)=0.0D0
      ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'&
      &.OR.WC.EQ.'AG'.OR.WC.EQ.'AC'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'&
      &.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')&
      &WRITE(OUTLYNE,*)&
      &'SURFACE',SURF,' : PIKUP (',WC(1:2),') DELETED'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'&
      &.OR.WC.EQ.'AGTOR')&
      &WRITE(OUTLYNE,*)&
      &'SURFACE',SURF,' : PIKUP (',WC(1:5),') DELETED'
      CALL SHOWIT(1)
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
!       NOW SEE IF THERE ARE AND REMAINING PIKUPS. IF NOT, SET
!       ALENS(32,SURF) TO ZERO
   END IF
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
   RETURN
END
! SUB SPIVOT.FOR
SUBROUTINE SPIVOT
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIVOT WHICH IMPLEMENTS THE PIVOT
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!       THE DEC COMMAND IS:
!
!               PIVOT X Y Z
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE='"PIVOT" TAKES NO QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='"PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
      OUTLYNE='"PIVOT" REQUIRES SOME EXPLICIT NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               PROCEED
!
!       CHECK FOR PIKUP PIVX IF FOUND REMOVE
!
      IF(PIKUP(1,SURF,34).EQ.1.0D0) THEN
!       DELETE PIKUP PIVX
         PIKUP(1:6,SURF,34)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVX DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,35).EQ.1.0D0) THEN
!       DELETE PIKUP PIVY
         PIKUP(1:6,SURF,35)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVY DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,36).EQ.1.0D0) THEN
!       DELETE PIKUP PIVZ
         PIKUP(1:6,SURF,36)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVZ DELETED'
         CALL SHOWIT(1)
      END IF
!
!       SET ALENS(32,SURF) IF NO OTHER PIKUPS EXIST
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
!       PROCEED WITH PIVOT ASSIGNMENT
!
      IF(W1.EQ.0.0D0.AND.W2.EQ.0.0D0.AND.W3.EQ.0.0D0) THEN
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         ALENS(59,SURF)=1.0D0
         ALENS(78,SURF)=W1
         ALENS(79,SURF)=W2
         ALENS(80,SURF)=W3
         ALENS(30,SURF)=0.0D0
         ALENS(31,SURF)=0.0D0
         ALENS(69,SURF)=0.0D0
         ALENS(114,SURF)=0.0D0
         ALENS(115,SURF)=0.0D0
         ALENS(116,SURF)=0.0D0
      ELSE
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         ALENS(59,SURF)=1.0D0
         ALENS(78,SURF)=W1
         ALENS(79,SURF)=W2
         ALENS(80,SURF)=W3
         ALENS(30,SURF)=0.0D0
         ALENS(31,SURF)=0.0D0
         ALENS(69,SURF)=0.0D0
         ALENS(114,SURF)=0.0D0
         ALENS(115,SURF)=0.0D0
         ALENS(116,SURF)=0.0D0
      END IF
!
!       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
!       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
!       IF THE CURRENT WORK HERE CHANGED THE STATUS OF ALENS(29,SURF)
!       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
!       RESOLVE THIS ISSUE.
!
      DO 100 I=0,INT(SYSTEM(20))
         IF(PIKUP(1,I,34).EQ.1.0D0) THEN
            IF(INT(PIKUP(2,I,34)).EQ.SURF) THEN
!       UPDATE THE ALENS(59) ON THE PIKING SURFACE
               ALENS(59,I)=ALENS(59,SURF)
            ELSE
!       DONT DO ANYTHING
            END IF
         ELSE
!       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
         END IF
100   CONTINUE
!
   END IF
   RETURN
END
! SUB SPIVOTD.FOR
SUBROUTINE SPIVOTD
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIVOTD WHICH IMPLEMENTS THE PIVOTD
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!               PIVOT X Y Z
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
      OUTLYNE='"PIVOTD" TAKES NO ADDITIONAL INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               PROCEED
!
!       CHECK FOR PIKUP PIVX IF FOUND REMOVE
!
      IF(PIKUP(1,SURF,34).EQ.1.0D0) THEN
!       DELETE PIKUP PIVX
         PIKUP(1:6,SURF,34)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVX DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,35).EQ.1.0D0) THEN
!       DELETE PIKUP PIVY
         PIKUP(1:6,SURF,35)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVY DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,36).EQ.1.0D0) THEN
!       DELETE PIKUP PIVZ
         PIKUP(1:6,SURF,36)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVZ DELETED'
         CALL SHOWIT(1)
      END IF
!
!       SET ALENS(32,SURF) IF NO OTHER PIKUPS EXIST
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
!       PROCEED WITH PIVOTD
!
      ALENS(59,SURF)=0.0D0
      ALENS(78:79,SURF)=0.0D0
      ALENS(80,SURF)=0.0D0
      ALENS(30:31,SURF)=0.0D0
      ALENS(69,SURF)=0.0D0
      ALENS(114:116,SURF)=0.0D0
!
!       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
!       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
!       IF THE CURRENT WORK HERE CHANGED THE STATUS OF ALENS(29,SURF)
!       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
!       RESOLVE THIS ISSUE.
!
      DO 100 I=0,INT(SYSTEM(20))
         IF(PIKUP(1,I,34).EQ.1.0D0) THEN
            IF(INT(PIKUP(2,I,34)).EQ.SURF) THEN
!       UPDATE THE ALENS(59) ON THE PIKING SURFACE
               ALENS(59,I)=ALENS(59,SURF)
            ELSE
!       DONT DO ANYTHING
            END IF
         ELSE
!       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
         END IF
100   CONTINUE
!
   END IF
   RETURN
END
! SUB SPIVAX.FOR
SUBROUTINE SPIVAX
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIVAX WHICH IMPLEMENTS THE PIVAXIS
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!       THE COMMAND IS:
!
!               PIVAXIS (LOCAL OR NORMAL)
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      OUTLYNE='"PIVAXIS" TAKES NO NUMERIC OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'LOCAL'.AND.WQ.NE.'NORMAL') THEN
      OUTLYNE=&
      &'"PIVAXIS" TAKES QUALIFIER WORDS "LOCAL" AND "NORMAL" ONLY'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SURF.EQ.0) THEN
      OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.EQ.'LOCAL') ALENS(113,SURF)=0.0D0
   IF(WQ.EQ.'NORMAL') ALENS(113,SURF)=1.0D0
   RETURN
END
! SUB SCW.FOR
SUBROUTINE SCW
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SCW WHICH IMPLEMENTS THE CW COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE CW COMMAND AT
!       THE CMD LEVEL
!
!
   IF(WC.EQ.'CW') THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
      ELSE
!       AT CMD
      END IF
!
      IF(F1.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
            OUTLYNE='AT THE CMD LEVEL, "CW" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000) INT(SYSTEM(11))
         CALL SHOWIT(0)
         RETURN
      ELSE
!               NOT AT CMD LEVEL
!
         IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
            IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1 &
            &.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
               OUTLYNE='"CW" ONLY ACCEPTS NUMERIC WORD #1 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!
            IF(DF1.EQ.1) THEN
               OUTLYNE='"CW" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
            IF(S1.EQ.0) THEN
               WRITE(OUTLYNE,1000)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,2000) INT(SYSTEM(11))
               CALL SHOWIT(0)
               RETURN
            ELSE
               IF(W1.LT.1.0.OR.W1.GT.10.0D0)THEN
                  OUTLYNE='NUMERIC INPUT TO "CW" OUTSIDE ALLOWED BOUNDS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  SYSTEM(11)=W1
                  RETURN
               END IF
            END IF
         END IF
      END IF
   END IF
   IF(WC.EQ.'PCW') THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
      ELSE
!       AT CMD
      END IF
      IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SN.EQ.1.OR.SQ.EQ.1.OR.SST.EQ.1) THEN
            OUTLYNE='AT THE CMD LEVEL, "PCW" TAKES NO INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         WRITE(OUTLYNE,3000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,4000) INT(SYSTEM(7)),INT(SYSTEM(8))
         CALL SHOWIT(0)
         RETURN
      ELSE
!               NOT AT CMD LEVEL
!
         IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
            IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
            &.OR.S5.EQ.1)THEN
!
               OUTLYNE=&
               &'"PCW" ONLY ACCEPTS NUMERIC WORD #1 AND #2 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
               OUTLYNE=&
               &'"PCW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
            IF(S1.EQ.0.AND.S2.EQ.0) THEN
               WRITE(OUTLYNE,3000)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,4000) INT(SYSTEM(7)),INT(SYSTEM(8))
               CALL SHOWIT(0)
               RETURN
            ELSE
               IF(W1.LT.1.0.OR.W1.GT.10.0.OR.W2.LT.1.0.OR.W2.GT.10.0D0)THEN
                  OUTLYNE='NUMERIC INPUT TO "PCW" OUTSIDE ALLOWED BOUNDS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  SYSTEM(7)=W1
                  SYSTEM(8)=W2
                  F22=1
                  RETURN
               END IF
            END IF
         END IF
      END IF
   END IF
   IF(WC.EQ.'SCW') THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
      END IF
      IF(F1.EQ.1) THEN
!               CHECK FOR PRESENCE OF ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
         IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
            OUTLYNE='AT THE CMD LEVEL, "SCW" TAKES NO INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         WRITE(OUTLYNE,5000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,6000) INT(SYSTEM(9)),INT(SYSTEM(10))
         CALL SHOWIT(0)
         RETURN
      ELSE
!               NOT AT CMD LEVEL
!
         IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
            IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
            &.OR.S5.EQ.1) THEN
!
               OUTLYNE=&
               &'"SCW" ONLY ACCEPTS NUMERIC WORD #1 AND #2 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!
            IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
               OUTLYNE=&
               &'"SCW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
            IF(S1.EQ.0.AND.S2.EQ.0) THEN
               WRITE(OUTLYNE,5000)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,6000) INT(SYSTEM(9)),INT(SYSTEM(10))
               CALL SHOWIT(0)
               RETURN
            ELSE
               IF(W1.LT.1.0.OR.W1.GT.10.0.OR.W2.LT.1.0.OR.W2.GT.10.0D0)THEN
                  OUTLYNE=&
                  &'NUMERIC INPUT TO "SCW" OUTSIDE ALLOWED BOUNDS'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               ELSE
                  SYSTEM(9)=W1
                  SYSTEM(10)=W2
                  F22=1
                  RETURN
               END IF
            END IF
         END IF
      END IF
   END IF
!
1000 FORMAT('CONTROL WAVELENGTH IS WAVLENGTH')
2000 FORMAT(10X,I2)
3000 FORMAT('PRIMARY WAVELENGTH PAIRS ARE WAVELENGTHS')
4000 FORMAT(5X,I2,2X,'AND',2X,I2)
5000 FORMAT('SECONDARY WAVELENGTH PAIRS ARE WAVELENGTHS')
6000 FORMAT(5X,I2,2X,'AND',2X,I2)
   RETURN
END
! SUB SCVR.FOR
SUBROUTINE SCVR
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SCVR WHICH IMPLEMENTS THE CVTOR AND RDTOR
!       COMMANDS AT THE UPDATE LENS LEVEL.
!       A YTORIC OR AN XTORIC COMMAND MUST BE ISSUED BEFORE TORIC DATA
!       CAN BE ENTERD FOR A SURFACE.
!
   INTEGER I
!
   REAL*8 DR,NEWRAD,ARG1,RADIUS,APER,WAVE,WAVER,VAL1,RAD
!
!
   IF(SURF.EQ.0) THEN
      OUTLYNE='OBJECT SURFACE MAY NOT BE A TORIC'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
   IF(WQ.NE.'DELTFR') THEN
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'CVTOR') THEN
            OUTLYNE='"CVTOR" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'RDTOR') THEN
            OUTLYNE='"RDTOR" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
   ELSE
! DELTFR
      IF(S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'CVTOR') THEN
            OUTLYNE='"CVTOR DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'RDTOR') THEN
            OUTLYNE='"RDTOR DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
      IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0 &
      &.AND.DF3.EQ.0) THEN
!
         IF(WC.EQ.'CVTOR') THEN
            OUTLYNE=&
            &'"CVTOR DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'RDTOR') THEN
            OUTLYNE=&
            &'"RDTOR DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
   END IF
   IF(SST.EQ.1) THEN
!
      IF(WC.EQ.'CVTOR') THEN
         OUTLYNE='"CVTOR" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'RDTOR') THEN
         OUTLYNE='"RDTOR" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      OUTLYNE=&
      &'"'//WC(1:5)//&
      &'" TAKES NO QUALIFIER INPUT DURING LENS INPUT MODE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT'.AND.&
      &WQ.NE.'DELTFR') THEN
         OUTLYNE=&
         &'INVALID QUALIFIER USED WITH "'//WC(1:5)//'"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      OUTLYNE=&
      &'"'//WC(1:5)//'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF

   IF(WC.EQ.'RDTOR'.OR.WC.EQ.'CVTOR') THEN
!
!       IF THE SURFACE WAS NOT A TORIC THEN SAY SO AND RETURN
!
      IF(ALENS(23,SURF).EQ.0.0D0) THEN
         OUTLYNE='A "YTORIC" OR "XTORIC" COMMAND MUST BE ENTERED'
         CALL SHOWIT(1)
         OUTLYNE='PRIOR TO TORIC DATA BEING ENTERED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!       ALENS(23,SURF) NOT ZERO, MUST BE 1.0 (Y-TORIC)
!       OR 2.0 (X-TORIC).
!
      IF(WC.EQ.'CVTOR') THEN
         IF(SQ.EQ.0) ALENS(24,SURF)=W1
         IF(WQ.EQ.'DELT') ALENS(24,SURF)=ALENS(24,SURF)+W1
         IF(WQ.EQ.'CENT')&
         &ALENS(24,SURF)=ALENS(24,SURF)+(W1*0.01D0*ALENS(24,SURF))
         IF(WQ.EQ.'DELTFR') THEN
            IF(DF2.EQ.0) WAVER=W2
            IF(DF2.EQ.1) WAVER=0.5461D0
            IF(DF3.EQ.0) APER=W3
!
            IF(DF3.EQ.1) THEN
               IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(126,SURF).NE.0.0D0) THEN
!     USE LAST PARAXIAL DATA
                  APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
               ELSE
                  IF(ALENS(9,SURF).EQ.1.0D0) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                     IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                     &APER=2.0D0*DABS(ALENS(11,SURF))
                     IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                     &APER=2.0D0*DABS(ALENS(10,SURF))
                  END IF
                  IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                     IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                     &APER=2.0D0*DABS(ALENS(10,SURF))
                     IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                     &APER=2.0D0*DABS(ALENS(11,SURF))
                  END IF
               END IF
            END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
            IF(SYSTEM(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
            IF(SYSTEM(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
            IF(SYSTEM(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
            IF(SYSTEM(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
!
            IF(ALENS(24,SURF).NE.0.0D0) THEN
!     CURVED SURFACE
               RAD=1.0D0/ALENS(24,SURF)
               RADIUS=DABS(1.0D0/ALENS(24,SURF))
               ARG1=(RADIUS**2)-((APER/2.0D0)**2)
               IF(ARG1.LT.0.0D0) THEN
                  OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
                  CALL SHOWIT(1)
                  OUTLYNE='RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  CALL MACFAL
                  RETURN
               END IF
               IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
               IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)&
               &DR=W1*(DABS((WAVE*DSQRT(ARG1))&
               &/(2.0D0*(DSQRT(ARG1)-RADIUS))))
               IF(DR.EQ.0.0D0) ALENS(24,SURF)=ALENS(24,SURF)
               IF(DR.NE.0.0D0) THEN
                  NEWRAD=RAD+DR
                  IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
                  IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
               END IF
            ELSE
!     SURFACE WAS FLAT
               NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))&
               &+(W1*WAVE/4.0D0)
               IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
               IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
!
            END IF
!     DONE WITH DELTFR
!
         END IF
         RETURN
      ELSE
      END IF
!     WC MUST BE RDTOR
      IF(WQ.EQ.'DELTFR') THEN
         IF(DF2.EQ.0) WAVER=W2
         IF(DF2.EQ.1) WAVER=0.5461D0
         IF(DF3.EQ.0) APER=W3
         IF(DF3.EQ.1) THEN
            IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(126,SURF).NE.0.0D0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(ALENS(9,SURF).EQ.1.0D0) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
               END IF
               IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
         IF(SYSTEM(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
         IF(SYSTEM(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
         IF(SYSTEM(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
         IF(SYSTEM(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
         IF(ALENS(24,SURF).NE.0.0D0) RADIUS=DABS(1.0D0/ALENS(24,SURF))
         IF(ALENS(24,SURF).NE.0.0D0) RAD=1.0D0/ALENS(24,SURF)
         IF(ALENS(24,SURF).EQ.0.0D0) RADIUS=0.0D0
!
         IF(RADIUS.NE.0.0D0) THEN
!     CURVED SURFACE
            ARG1=(RADIUS**2)-((APER/2.0D0)**2)
            IF(ARG1.LT.0.0D0) THEN
               OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
            IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)&
            &DR=W1*(DABS((WAVE*DSQRT(ARG1))&
            &/(2.0D0*(DSQRT(ARG1)-RADIUS))))
            IF(DR.EQ.0.0D0) ALENS(24,SURF)=ALENS(24,SURF)
            IF(DR.NE.0.0D0) THEN
               NEWRAD=RAD+DR
               IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
               IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
            END IF
         ELSE
!     SURFACE WAS FLAT
            NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))&
            &+(W1*WAVE/4.0D0)
            IF(NEWRAD.EQ.0.0D0) ALENS(24,SURF)=0.0D0
            IF(NEWRAD.NE.0.0D0) ALENS(24,SURF)=1.0D0/NEWRAD
!
         END IF
!     DONE WITH DELTFR
      END IF
      IF(W1.EQ.0.0D0) THEN
         IF(SQ.EQ.0) ALENS(24,SURF)=0.0D0
         IF(WQ.EQ.'DELT') ALENS(24,SURF)=ALENS(24,SURF)
         IF(WQ.EQ.'CENT') ALENS(24,SURF)=ALENS(24,SURF)
      ELSE
         IF(SQ.EQ.0) ALENS(24,SURF)=1.0D0/(W1)
         IF(WQ.EQ.'DELT') THEN
            IF(ALENS(24,SURF).EQ.0.0D0) VAL1=0.0D0
            IF(ALENS(24,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(24,SURF)
            VAL1=VAL1+W1
            IF(VAL1.EQ.0.0D0) ALENS(24,SURF)=0.0D0
            IF(VAL1.NE.0.0D0) ALENS(24,SURF)=1.0D0/VAL1
         END IF
         IF(WQ.EQ.'CENT') THEN
            IF(ALENS(24,SURF).EQ.0.0D0) VAL1=0.0D0
            IF(ALENS(24,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(24,SURF)
            VAL1=VAL1+(W1*0.01D0*VAL1)
            IF(VAL1.EQ.0.0D0) ALENS(24,SURF)=0.0D0
            IF(VAL1.NE.0.0D0) ALENS(24,SURF)=1.0D0/VAL1
         END IF
      END IF
!
!       IF AN RDTOR OR CVTOR PIKUP EXISTS, GET RID OF IT
!
      IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
         ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
         CALL SHOWIT(1)
         PIKUP(1:6,SURF,9)=0.0D0
      END IF
      IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
         ALENS(32,SURF)=ALENS(32,SURF)-1.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
         CALL SHOWIT(1)
         PIKUP(1:6,SURF,10)=0.0D0
      END IF
!       NOW CHECK FOR CURVATURE SOLVES AND DELETE THE APPROPRIATE
!       ONES. IF THE SURFACE IS A Y-TORIC, DELETE XZ CURVATURE
!       SOLVES. IF THE SURFACE IS AN X-TORIC DELETE YZ CURVATURE SOLVES
!
      IF(ALENS(23,SURF).EQ.1.0D0) THEN
!       Y-TORIC, ARE THERE XZ CURVATURE SOLVES
         IF(SOLVE(2,SURF).GT.0.0D0) THEN
!       SOLVE TO DELETE IS FOUND
            SOLVE(2,SURF)=0.0D0
            SOLVE(1,SURF)=0.0D0
            WRITE(OUTLYNE,*)&
            &'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVE DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(ALENS(23,SURF).EQ.2.0D0) THEN
!       X-TORIC, ARE THERE YZ CURVATURE SOLVES
         IF(SOLVE(8,SURF).GT.0.0D0) THEN
!       SOLVE TO DELETE IS FOUND
            SOLVE(8,SURF)=0.0D0
            SOLVE(9,SURF)=0.0D0
            WRITE(OUTLYNE,*)&
            &'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVE DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
!       NOW RE-CALCULATE ALENS(33,SURF) THE SOLVE COUNTER
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   ELSE
!       NOT RDTOR OR CVTOR
   END IF
   RETURN
END
! SUB SCV.FOR
SUBROUTINE SCV
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SCV WHICH IMPLEMENTS THE CV AND RD
!       COMMANDS AT THE LENS OF UPDATE LENS LEVEL.
!       THE SECOND NUMERIC WORD IS THE CONIC CONSTANT
!       ASSOCIATED WITH THIS SURFACE.
!
   INTEGER PIKCNT,I
!
   REAL*8 VAL1,WAVE,WAVER,APER,RADIUS,DR,NR,RAD &
   &,NEWRAD,ARG1
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
   IF(WQ.NE.'DELTFR'.AND.WQ.NE.'SAG') THEN
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'CV') THEN
            OUTLYNE='"CV" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'RD') THEN
            OUTLYNE='"RD" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
   ELSE
! DELTFR OR SAG
      IF(WQ.EQ.'DELTFR') THEN
         IF(S4.EQ.1.OR.S5.EQ.1) THEN
!
            IF(WC.EQ.'CV') THEN
               OUTLYNE='"CV DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               OUTLYNE='"RD DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
         IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0 &
         &.AND.DF3.EQ.0) THEN
!
            IF(WC.EQ.'CV') THEN
               OUTLYNE=&
               &'"CV DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               OUTLYNE=&
               &'"RD DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'SAG') THEN
         IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
            IF(WC.EQ.'CV') THEN
               OUTLYNE='"CV SAG" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               OUTLYNE='"RD SAG" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
         IF(W2.LE.0.0D0 &
         &.AND.DF2.EQ.0) THEN
!
            IF(WC.EQ.'CV') THEN
               OUTLYNE=&
               &'"CV SAG" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               OUTLYNE=&
               &'"RD SAG" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
      END IF
!     NOT DELTFR OR SAG
   END IF
   IF(SST.EQ.1) THEN
!
      IF(WC.EQ.'CV') THEN
         OUTLYNE='"CV" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'RD') THEN
         OUTLYNE='"RD" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT'&
      &.AND.WQ.NE.'DELTFR'.AND.WQ.NE.'SAG') THEN
!
         IF(WC.EQ.'CV') THEN
            OUTLYNE='INVALID QUALIFIER USED WITH "CV"'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'RD') THEN
            OUTLYNE='INVALID QUALIFIER USED WITH "RD"'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
   END IF
   IF(F5.EQ.1) THEN
      IF(SQ.EQ.1) THEN
!
         IF(WC.EQ.'CV') THEN
            OUTLYNE='"CV" TAKES NO QUALIFIER WORDS IN LENS INPUT MODE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'RD') THEN
            OUTLYNE='"RD" TAKES NO QUALIFIER WORDS IN LENS INPUT MODE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
!
      IF(WC.EQ.'CV') THEN
         OUTLYNE='"CV" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'RD') THEN
         OUTLYNE='"RD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
!
   IF(WC.EQ.'RD'.AND.SQ.EQ.0.AND.DF2.EQ.0) ALENS(2,SURF)=W2
!
   IF(WC.EQ.'CV') THEN
      IF(SQ.EQ.0) ALENS(1,SURF)=W1
      IF(SQ.EQ.0.AND.DF2.EQ.0) ALENS(2,SURF)=W2
      IF(WQ.EQ.'DELTFR') THEN
         IF(DF2.EQ.0) WAVER=W2
         IF(DF2.EQ.1) WAVER=0.5461D0
         IF(DF3.EQ.0) APER=2.0D0*W3
!
         IF(DF3.EQ.1) THEN
            IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(127,SURF).NE.0.0D0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(ALENS(9,SURF).EQ.1.0D0) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
               END IF
               IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
         IF(SYSTEM(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
         IF(SYSTEM(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
         IF(SYSTEM(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
         IF(SYSTEM(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
!
         IF(ALENS(1,SURF).NE.0.0D0) THEN
!     CURVED SURFACE
            RADIUS=DABS(1.0D0/ALENS(1,SURF))
            RAD=1.0D0/ALENS(1,SURF)
            ARG1=(RADIUS**2)-((APER/2.0D0)**2)
            IF(ARG1.LT.0.0D0) THEN
               OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
            IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)&
            &DR=W1*(DABS((WAVE*DSQRT(ARG1))&
            &/(2.0D0*(DSQRT(ARG1)-RADIUS))))
            IF(DR.EQ.0.0D0) ALENS(1,SURF)=ALENS(1,SURF)
            IF(DR.NE.0.0D0) THEN
               NEWRAD=RAD+DR
               IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
               IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
            END IF
         ELSE
!     SURFACE WAS FLAT
            NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))&
            &+(W1*WAVE/4.0D0)
            IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
            IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
!
         END IF
!     DONE WITH DELTFR
!
      END IF
      IF(WQ.EQ.'SAG') THEN
         IF(DF2.EQ.0) APER=2.0D0*W2
!
         IF(DF2.EQ.1) THEN
            IF(ALENS(9,SURF).EQ.0.0D0.AND.ALENS(127,SURF).NE.0.0D0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(ALENS(9,SURF).EQ.1.0D0) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
               END IF
               IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
         ARG1=(((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
         IF(ARG1.LT.0.0D0) THEN
            OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         ALENS(1,SURF)=(2.0D0*W1)/&
         &(((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
!     DONE WITH SAG
!
      END IF
      IF(WQ.EQ.'DELT') ALENS(1,SURF)=ALENS(1,SURF)+W1
      IF(WQ.EQ.'CENT')&
      &ALENS(1,SURF)=ALENS(1,SURF)+(W1*0.01D0*ALENS(1,SURF))
   ELSE
   END IF
!     WC MUST BE RD
   IF(WC.EQ.'RD') THEN
      IF(WQ.EQ.'DELTFR') THEN
         IF(DF2.EQ.0) WAVER=W2
         IF(DF2.EQ.1) WAVER=0.5461D0
         IF(DF3.EQ.0) APER=2.0D0*W3
         IF(DF3.EQ.1) THEN
            IF(ALENS(9,SURF).EQ.0.0D0.AND.ALENS(127,SURF).NE.0.0D0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(ALENS(9,SURF).EQ.1.0D0) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
               END IF
               IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
         IF(SYSTEM(6).EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
         IF(SYSTEM(6).EQ.2.0D0) WAVE=WAVER*1.0D-4
         IF(SYSTEM(6).EQ.3.0D0) WAVE=WAVER*1.0D-3
         IF(SYSTEM(6).EQ.4.0D0) WAVE=WAVER*1.0D-6
         IF(ALENS(1,SURF).NE.0.0D0) RADIUS=DABS(1.0D0/ALENS(1,SURF))
         IF(ALENS(1,SURF).NE.0.0D0) RAD=1.0D0/ALENS(1,SURF)
         IF(ALENS(1,SURF).EQ.0.0D0) RADIUS=0.0D0
!
         IF(RADIUS.NE.0.0D0) THEN
!     CURVED SURFACE
            ARG1=(RADIUS**2)-((APER/2.0D0)**2)
            IF(ARG1.LT.0.0D0) THEN
               OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
            IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)&
            &DR=W1*(DABS((WAVE*DSQRT(ARG1))&
            &/(2.0D0*(DSQRT(ARG1)-RADIUS))))
            IF(DR.EQ.0.0D0) ALENS(1,SURF)=ALENS(1,SURF)
            IF(DR.NE.0.0D0) THEN
               NEWRAD=RAD+DR
               IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
               IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
            END IF
         ELSE
!     SURFACE WAS FLAT
            NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))&
            &+(W1*WAVE/4.0D0)
            IF(NEWRAD.EQ.0.0D0) ALENS(1,SURF)=0.0D0
            IF(NEWRAD.NE.0.0D0) ALENS(1,SURF)=1.0D0/NEWRAD
!
         END IF
!     DONE WITH DELTFR
      END IF
      IF(WQ.EQ.'SAG') THEN
         IF(DF2.EQ.0) APER=2.0D0*W2
         IF(DF2.EQ.1) THEN
            IF(ALENS(9,SURF).EQ.0.0D0.OR.ALENS(127,SURF).NE.0.0D0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(ALENS(9,SURF).EQ.1.0D0) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
               END IF
               IF(ALENS(9,SURF).GE.2.0D0.AND.ALENS(9,SURF).LE.4.0D0) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(ALENS(10,SURF).GT.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(10,SURF))
                  IF(ALENS(10,SURF).LE.ALENS(11,SURF))&
                  &APER=2.0D0*DABS(ALENS(11,SURF))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
         ARG1=(((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
         IF(ARG1.LT.0.0D0) THEN
            OUTLYNE='RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         ALENS(1,SURF)=(2.0D0*W1)/&
         &(((W1**2)*(ALENS(2,SURF)+1.0D0))+((APER/2.0D0)**2))
!     DONE WITH SAG
!
      END IF
      IF(W1.EQ.0.0D0) THEN
         IF(SQ.EQ.0) ALENS(1,SURF)=0.0D0
         IF(WQ.EQ.'DELT') ALENS(1,SURF)=ALENS(1,SURF)
         IF(WQ.EQ.'CENT') ALENS(1,SURF)=ALENS(1,SURF)
      ELSE
         IF(SQ.EQ.0) ALENS(1,SURF)=1.0D0/(W1)
         IF(WQ.EQ.'DELT') THEN
            IF(ALENS(1,SURF).EQ.0.0D0) VAL1=0.0D0
            IF(ALENS(1,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(1,SURF)
            VAL1=VAL1+W1
            IF(VAL1.EQ.0.0D0) ALENS(1,SURF)=0.0D0
            IF(VAL1.NE.0.0D0) ALENS(1,SURF)=1.0D0/VAL1
         END IF
         IF(WQ.EQ.'CENT') THEN
            IF(ALENS(1,SURF).EQ.0.0D0) VAL1=0.0D0
            IF(ALENS(1,SURF).NE.0.0D0) VAL1=1.0D0/ALENS(1,SURF)
            VAL1=VAL1+(W1*0.01D0*VAL1)
            IF(VAL1.EQ.0.0D0) ALENS(1,SURF)=0.0D0
            IF(VAL1.NE.0.0D0) ALENS(1,SURF)=1.0D0/VAL1
         END IF
      END IF
   END IF
!
   IF(ALENS(1,SURF).EQ.0.0D0.AND.ALENS(2,SURF).NE.0.0D0) THEN
      OUTLYNE='WARNING:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE CONIC CONSTANT WILL BE IGNORED FOR THIS PLANO SURFACE'
      CALL SHOWIT(1)
   END IF
   IF(ALENS(1,SURF).NE.0.0D0.AND.ALENS(43,SURF).NE.0.0D0) THEN
      ALENS(43,SURF)=0.0D0
      OUTLYNE='WARNING:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
      CALL SHOWIT(1)
      OUTLYNE=&
      &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
      CALL SHOWIT(1)
   END IF
!
!       CHECK CURVATURE SOLVES
! IF THE SURFACE IS NOT A TORIC ALENS(23,SURF)=0 THEN DELETE ALL CURVATUVE SOLVES
!       AND RE-CALCULATE ALENS(33,SURF) THE SOLVE TRACKER
!       IF THE SURFACE IS AN X TORIC THEN DELETE ONLY THE XZ PLANE CURVATURE SOLVES
!       IF THE SURFACE IS AN Y TORIC THEN DELETE ONLY THE YZ PLANE CURVATURE SOLVES
!
!       CHECK FOR A TORIC
   IF(ALENS(23,SURF).EQ.0.0D0) THEN
!       NO TORIC, DELETE ALL CURVATURE SOLVE
      IF(SOLVE(8,SURF).NE.0.0D0) THEN
         SOLVE(8,SURF)=0.0D0
         SOLVE(9,SURF)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :ALL CURVATURE SOLVES DELETED'
         CALL SHOWIT(1)
         SOLVE(2,SURF)=0.0D0
         SOLVE(1,SURF)=0.0D0
      END IF
!       RE-CALCULATE ALENS(33,SURF)
      ALENS(33,SURF)=0.0D0
      IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
      IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
      IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
      IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
   ELSE
!       TORIC
!               CHECK FOR Y-TORIC
      IF(ALENS(23,SURF).EQ.1.0D0) THEN
!       Y-TORIC, DELETE ALL YZ PLANE CURVATURE SOLVES
         IF(SOLVE(8,SURF).NE.0.0D0) THEN
            SOLVE(8,SURF)=0.0D0
            SOLVE(9,SURF)=0.0D0
            WRITE(OUTLYNE,*)&
            &'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVES DELETED'
            CALL SHOWIT(1)
!       RE-CALCULATE ALENS(33,SURF)
            ALENS(33,SURF)=0.0D0
            IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
            IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
            IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
            IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
         ELSE
!       NO SOLVES
         END IF
      ELSE
         IF(ALENS(23,SURF).EQ.2.0D0) THEN
!       X-TORIC, DELETE ALL XZ PLANE CURVATURE SOLVES
            IF(SOLVE(2,SURF).NE.0.0D0) THEN
               SOLVE(2,SURF)=0.0D0
               SOLVE(1,SURF)=0.0D0
               WRITE(OUTLYNE,*)&
               &'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVES DELETED'
               CALL SHOWIT(1)
!       RE-CALCULATE ALENS(33,SURF)
               ALENS(33,SURF)=0.0D0
               IF(SOLVE(6,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+1.0D0
               IF(SOLVE(4,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.1D0
               IF(SOLVE(8,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+2.0D0
               IF(SOLVE(2,SURF).GT.0.0D0) ALENS(33,SURF)=ALENS(33,SURF)+0.2D0
            END IF
         ELSE
            OUTLYNE='SERIOUS ERROR IN ASSIGNMENT OF ALENS(23, )'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!
!       CHECK FOR RD OR CV PIKUPS AND DELETE IF FOUND
!
   IF(ALENS(32,SURF).EQ.0.0D0) THEN
!       NO PIKUPS DON'T DO ANYTHING
!
      RETURN
   ELSE
   END IF
!
!       CHECK FOR CV OR RD OR PRO OR NPRO
!       PIKUPS AND DELETE IF FOUND
!
   IF(PIKUP(1,SURF,1).EQ.0.0D0.AND.PIKUP(1,SURF,2).EQ.0.0D0.&
   &AND.PIKUP(1,SURF,11).EQ.0.0D0.AND.PIKUP(1,SURF,12)&
   &.EQ.0.0D0) THEN
!
!       NO CV RD PRO OR NPRO PIKUPS, JUST RETURN
!
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
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
!       ARE THERE MORE PIKUPS? IF NOT SET ALENS(32,SURF) TO ZERO.
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
   RETURN
END
! SUB SINS.FOR
SUBROUTINE SINS
!
   use DATSUB
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SINS WHICH IMPLEMENTS THE INS
!       COMMANDS AT THE UPDATE LENS LEVEL.
!       THE FIRST NUMERIC WORD IS THE SURFACE WHICH WE WISH TO
!       ADD TO THE LENS. VALID VALUES ARE FROM 1 TO THE INTEGER VALUE
!       STORED IN SYSTEM(20). FOR EXAMPLE, IF THE COMMAND
!
!                       INS,15 IS ISSUED AND SYSTEM(20)=25
!       THEN A DUMMY BLANK DEFAULT SURFACE IS ADDED TO ALENS
!       BETWEEN EXISTING SURFACES 14 AND 15. OLD SURFACE
!       15 BECOMES SURFACE 16 AND SYSTEM(20) IS INCREMENTED BY 1.
!       THE CURRENT SURFACE BECOMES THE SURFACE INSERTER. IF INS
!       IS ISSUED WITH NO NUMERIC INPUT, A DUMMY SURFACE IS ENTERED
!       AHEAD OF THE CURRENT, CURRENT SURFACE. AFTER THE INSERTION
!       THE NEWLY INSERTED SURFACE BECOMES THE NEW CURRENT SURFACE.
!       SOLVE ARRAY AND PIKUPS ALSO UPDATED
!
   INTEGER K,L,I,J,JK,II,III,IV
!
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
   &.OR.S5.EQ.1) THEN
      OUTLYNE='"INS" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1) THEN
      W1=DBLE(SURF)
      DF1=0
   ELSE
   END IF
   IF(DF2.EQ.1) THEN
      W2=1.0D0
      DF2=0
   ELSE
   END IF
   IF(GLANAM(INT(W1)-1,2).EQ.'PERFECT      ') THEN
      OUTLYNE=&
      &'A SURFACE MAY NOT BE INSERTED BEHIND A "PERFECT" SURFACE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(GLANAM(INT(W1)-1,2).EQ.'IDEAL        ') THEN
      OUTLYNE=&
      &'A SURFACE MAY NOT BE INSERTED BEHIND A "IDEAL" SURFACE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       IS DEFAULT SURFACE ENTRY IN EFFECT?
!
   DO II=1,INT(W2)
!
!       USE THE NUMERIC
!       VALUE OF NUMERIC WORD 1 AS THE LOCATION OF THE SURFACE
!       INSERTION.
      IF(W1.EQ.0.0D0) THEN
         OUTLYNE=&
         &'SURFACE INSERTION INFRONT OF OBJECT SURF. NOT ALLOWED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      IF(W1.LT.0.0D0) W1=SYSTEM(20)+W1
      IF(W1.LT.1.0D0.OR.W1.GT.SYSTEM(20)) THEN
!
!       TRYING TO INSERT A SURFACE NUMBER INFRONT OF THE
!       OBJECT SURFACE OR AFTER THE IMAGE SURFACE. THIS OPERATION
!       IS NOT ALLOWED.
!       PRINT ERROR AND RETURN.
         OUTLYNE='INVALID LOCATION FOR INSERTED SURFACE'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
!       SURFACE NUMBER WITHIN VALID RANGE.
!
!
!       IS SURFACE INSERTED, INFRONT OF REFERENCE SURFACE ?
         IF(INT(W1).LE.INT(SYSTEM(25))) THEN
            SYSTEM(25)=SYSTEM(25)+1.0D0
            NEWREF=INT(SYSTEM(25))
         ELSE
!       NOT IN FRONT OF REF SURF
         END IF
!       IS SURFACE INSERTED, INFRONT OF ASTOP SURFACE ?
         IF(INT(W1).LE.INT(SYSTEM(26)).AND.&
         &SYSTEM(26).NE.-99.0D0) THEN
            SYSTEM(26)=SYSTEM(26)+1.0D0
         ELSE
!       NOT IN FRONT OF ASTOP SURFOR THERE IS NO ASTOP SURFACE
         END IF
!
         SYSTEM(20)=SYSTEM(20)+1.0D0
         K=INT(W1)
         L=INT(SYSTEM(20))
         DO J=(L-1),K,-1
            ALENS(1:LSIZ,(J+1))=ALENS(1:LSIZ,J)
         END DO
         DO J=(L-1),K,-1
            LBL(J+1)(1:80)=LBL(J)(1:80)
         END DO
!     NOW SPSRF DATA
         DO  J=(L-1),K,-1
            FTFL01(1:96,J+1)=FTFL01(1:96,J)
         END DO
         DO J=(L-1),K,-1
            GLANAM((J+1),1)=GLANAM(J,1)
            GLANAM((J+1),2)=GLANAM(J,2)
            SOLVE(0:9,J+1)=SOLVE(0:9,J)
         END DO
!
!     L IS THE NEW FINAL SURFACE OF THE LENS
         DO J=0,L
            DO I=1,PSIZ
               IF(I.NE.32) THEN
!     NOT THOAL
!       IF THE SURFACE NUMBER OF THE SURFACE BEING INSERTED IS
!       NUMERICALLY LESS THAN OR EQUAL TO SURFACE NUMBER REFERENCED
!       BY THE PIKUP (PIKUP(2,J,I) THEN
                  IF(ALENS(32,J).NE.0.0D0) THEN
!     THERE IS A PIKUP ON SURFACE J
                     IF(K.LE.INT(PIKUP(2,J-1,I))) THEN
                        PIKUP(2,J-1,I)=PIKUP(2,J-1,I)+1.0D0
                     ELSE
!       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
!       THE PIKUP
                     END IF
                  END IF
               ELSE
!     THOAL
                  IF(ALENS(32,J).NE.0.0D0) THEN
!     THERE IS A PIKUP ON SURFACE J
                     IF(K.LE.INT(PIKUP(2,J-1,I))) THEN
                        PIKUP(2,J-1,I)=PIKUP(2,J-1,I)+1.0D0
                     ELSE
!       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
!       THE PIKUP
                     END IF
                     IF(K.LE.INT(PIKUP(3,J-1,I))) THEN
                        PIKUP(3,J-1,I)=PIKUP(3,J-1,I)+1.0D0
                     ELSE
!       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
!       THE PIKUP
                     END IF
                  END IF

               END IF
            END DO
         END DO
!
!     K IS THE SURFACE BEING INSERTED
         DO J=(L-1),K,-1
            PIKUP(1:6,J+1,1:PSIZ)=PIKUP(1:6,J,1:PSIZ)
         END DO
!
         SURF=INT(W1)
         CALL FLUSHNEXT
         IF(VBCNT.NE.0) THEN
            DO III=1,VBCNT
               IF(VARABL(III,3).GE.W1) VARABL(III,3)=VARABL(III,3)+1.0D0
            END DO
         END IF
         IF(TVBCNT.NE.0) THEN
            DO III=1,TVBCNT
               IV=III+MAXCMP
               IF(VARABL(IV,3).GE.W1) VARABL(IV,3)=VARABL(IV,3)+1.0D0
            END DO
         END IF
         IF(CMPCNT.NE.0) THEN
            DO III=1,CMPCNT
               IF(VARABL(III,3).GE.W1) VARABL(III,3)=VARABL(III,3)+1.0D0
            END DO
         END IF
!     HERE IS WHERE WE CALL CFGFX1.FOR TO FIX POSSIBLE
!     CONFIGS DATA IMPACTED BY THE INSERTION
         CALL CFGFX1(INT(W1))
!
      END IF
!
!       PRINT INSERTION MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :SURFACE INSERTED'
      CALL SHOWIT(1)
      F22=1
!       NOW SURFACE NUMBER SURF HAS BEEN INSERTED
      DO I=0,INT(SYSTEM(20))
         IF(ALENS(25,I).EQ.6.0D0.OR.ALENS(25,I).EQ.1.0D0.AND.&
         &ALENS(77,I).EQ.1) THEN
!       FOUND A TILT RET
            IF((ALENS(70,I)).GE.SURF) ALENS(70,I)=ALENS(70,I)+1.0D0
         END IF
      END DO
   END DO

   RETURN
END
! SUB SDEL.FOR
SUBROUTINE SDEL
!
   use DATSUB
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SDEL WHICH IMPLEMENTS THE DEL
!       COMMANDS AT THE UPDATE LENS LEVEL.
!       THE FIRST NUMERIC WORD IS THE SURFACE WHICH WE WISH TO
!       DELETE FROM THE LENS. VALID VALUES ARE FROM 1 TO THE INTEGER VALUE
!       STORED IN (SYSTEM(20) MINUS 1).
!
!       IT IS NOT PERMITTED TO DELETE THE OBJECT OR IMAGE SURFACES.
!
!        FOR EXAMPLE, IF THE COMMAND
!
!                       DEL,15 IS ISSUED AND SYSTEM(20)=25
!       THEN SURFACE 15 IS DELETE. OLD SURFACE 16 BECOMES NEW 15
!       AND SO FORTH AND SYSTEM(20) IS DECREMENTED BY 1.0D0. THE CURRENT
!       SURFACE BECOMES THE NEW SURFACE 15.
!       ISSUING DEL WITH NO NUMERIC WORD CAUSES THE CURRENT SURFACE
!       TO BE DELETED IF THE CURRENT SURFACE IS NOT THE OBJECT OR
!       IMMAGE SURFACE. IF THE CURRENT SURFACE IS THE OBJECT SURFACE,
!       AN MESSAGE IS PRINTED STATING THAT THE OBJECT SURFACE MAY NOT
!       BE DELETED. IF THE CURRENT SURFACE IS THE IMAGE SURFACE THEN
!       THE CURRENT SURFACE IS DECREMENTED BY ONE AND THEN THE DELETION
!       IS PERFORMED. THIS MAY BE REPEATED UNTIL ONLY THE OBJECT AND
!       IMAGE SURFACES REMAIN. SOLVE DATA ALSO FIXED.
!
   INTEGER PIKCNT,J,I,K,III,II,SNUMBER,IIII,IV,JJJJ,V
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
   &.OR.S5.EQ.1) THEN
      OUTLYNE='"DEL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!
!               WE ARE AT LENS UPDATE LEVEL
!
!       IS DEFAULT SURFACE ENTRY IN EFFECT?
!
   IF(DF1.EQ.1) THEN
      W1=DBLE(SURF)
      DF1=0
   ELSE
   END IF
   IF(W1.LT.0.0D0) W1=SYSTEM(20)+W1
!
   IF(DF2.EQ.1) THEN
      W2=W1
      DF2=0
   ELSE
   END IF
!
   IF(W2.LT.W1) THEN
      OUTLYNE=&
      &'STARTING SURFACE # MAY NOT BE GREATER THAN ENDING SURFACE #'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(W1.LE.0.0D0.OR.W1.GE.SYSTEM(20).OR.W2.LE.0.0D0.OR.&
   &W2.GE.SYSTEM(20)) THEN
!
!       TRYING TO DELETE A SURFACE NUMBER LESS THAN OR EQUAL TO THE
!       OBJECT SURFACE OR GREATER THAN OR EQUAL TO THE IMAGE SURFACE
!       IS NOT ALLOWED.
!       PRINT ERROR AND RETURN.
!
      IF(W1.EQ.0.0.OR.W2.EQ.0.0D0) THEN
         OUTLYNE='OBJECT SURFACE DELETION NOT ALLOWED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(W1.EQ.SYSTEM(20).OR.W2.EQ.SYSTEM(20)) THEN
         OUTLYNE='IMAGE SURFACE DELETION NOT ALLOWED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!
   SNUMBER=INT(W1)
   SURF=INT(W1)
   DO II=1,INT(W2)-INT(W1)+1
!
!       DELETION MESSAGE
      IF(VBCNT.NE.0) THEN
         DO III=1,VBCNT
            IF(INT(VARABL(III,3)).EQ.SNUMBER) THEN
!     DELETE AN ENTRY
               DO IV=III,VBCNT-1
                  VARABL(IV,1:17)=VARABL(IV+1,1:17)
               END DO
               VBCNT=VBCNT-1
               GO TO 20
            END IF
            IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
!     MOVE ALL SURF NUMBERS UP
               DO IIII=1,VBCNT
                  VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
               END DO
            END IF
20          CONTINUE
         END DO
      END IF
      IF(TVBCNT.NE.0) THEN
         DO III=1,TVBCNT
            V=III+MAXCMP
            IF(INT(VARABL(V,3)).EQ.SNUMBER) THEN
!     DELETE AN ENTRY
               DO IV=III,TVBCNT-1
                  V=IV+MAXCMP
                  VARABL(IV,1:17)=VARABL(IV+1,1:17)
               END DO
               TVBCNT=TVBCNT-1
               GO TO 30
            END IF
            IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
!     MOVE ALL SURF NUMBERS UP
               DO IIII=1,TVBCNT
                  VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
               END DO
            END IF
30          CONTINUE
         END DO
      END IF
      IF(CMPCNT.NE.0) THEN
         DO III=1,CMPCNT
            IF(INT(VARABL(III,3)).EQ.SNUMBER) THEN
!     DELETE AN ENTRY
               DO IV=III,CMPCNT-1
                  VARABL(IV,1:17)=VARABL(IV+1,1:17)
               END DO
               CMPCNT=CMPCNT-1
               GO TO 40
            END IF
            IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
!     MOVE ALL SURF NUMBERS UP
               DO IIII=1,CMPCNT
                  VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
               END DO
            END IF
40          CONTINUE
         END DO
      END IF
      WRITE(OUTLYNE,*)'DELETING SURFACE ',SNUMBER
      CALL SHOWIT(1)
!
!       IS THE SURFACE THE REFERENCE SURFACE ?
      IF(SURF.EQ.INT(SYSTEM(25))) THEN
         OUTLYNE='CURRENT REFERENCE SURFACE BEING DELETED'
         CALL SHOWIT(1)
         OUTLYNE='REFERENCE SURFACE SHIFTED TO SURFACE 1'
         CALL SHOWIT(1)
         SYSTEM(25)=1.0D0
      END IF
!       IS SURFACE DELETED, INFRONT OF REFERENCE SURFACE ?
      IF(SURF.LT.INT(SYSTEM(25))) THEN
         SYSTEM(25)=SYSTEM(25)-1.0D0
         NEWREF=INT(SYSTEM(25))
      ELSE
!       NOT IN FRONT OF REF SURF
      END IF
!       IS THE SURFACE THE ASTOP SURFACE ?
      IF(SURF.EQ.INT(SYSTEM(26))) THEN
         OUTLYNE='CURRENT APERTURE STOP SURFACE BEING DELETED'
         CALL SHOWIT(1)
         SYSTEM(26)=-99.0D0
         SYSTEM(27)=0.0D0
      END IF
!       IS SURFACE DELETED, INFRONT OF ASTOP SURFACE ?
      IF(SURF.LT.INT(SYSTEM(26)).AND.&
      &SYSTEM(26).NE.-99.0D0) THEN
         SYSTEM(26)=SYSTEM(26)-1.0D0
      ELSE
!       NOT IN FRONT OF ASTOP SURF OR NO ASTOP DEFINED.
      END IF
!
!       DELETE SURFACE
!       REPRESENTED BY SURF,SURF EITHER INPUT ON COMMAND LINE
!       OR SET BY ABOVE DEFAULT PROCEEDURE.
!
!       BEFORE DELETING THE SURFACE,CHECK TO SEE IF THERE ARE
!       ANY SOLVES OR PIKUPS ON THAT SURFACE AND IF THERE
!       ARE ANY, PRINT A MESSAGE THAT EITHER SOLVES OR PIKUPS
!       ARE BEING DELETED WITH THE SURFACE DELETION.
!
!       FIRST SOLVES
!
      IF(ALENS(32,SURF).NE.0.0D0) THEN
         OUTLYNE=&
         &'PIKUP DATA BEING DELETED WITH SURFACE DELETION'
         CALL SHOWIT(1)
      END IF
!
!       THEN PIKUPS
!
      IF(ALENS(33,SURF).NE.0.0D0) THEN
         OUTLYNE=&
         &'SOLVE DATA BEING DELETED WITH SURFACE DELETION'
         CALL SHOWIT(1)
      END IF
!
!       THEN SPECIAL SURFACE DATA
!
!       NOW WHAT IF THE SURFACE TO BE DELETED IS THE SURFACE
!       FROM WHICH SOMETHING IS BEING PIKED UP FROM. IN THAT CASE
!       THE PIKUP SHOULD BE DELETED WITH THE ASSOCIATED PARAMETER
!       LEFT AT ITS CURRENT VALUE.
!
!       THE ENTIRE LENS MUST BE CHECKED TO SEE IF A PIKUP
!       REFERS TO THE SURFACE TO BE DELETED. THAT PIKUP MUST BE
!       DELETED. IF THAT WAS THE LAST PIKUP ON THAT SURFACE THEN
!       ALENS(32,(FOR THE SURFACE WITH PIKUPS)) IS SET TO ZERO
!
      DO J=0,INT(SYSTEM(20))
         IF(ALENS(32,J).NE.0D0) THEN
!       FOUND A SURFACE WITH PIKUPS
            DO I=1,PSIZ
               IF(PIKUP(1,J,I).EQ.1) THEN
                  IF(INT(PIKUP(2,J,I)).EQ.SURF) THEN
!       FOUND A PIKUP WHICH REFERENCES A SURFACE TO BE DELETED
!       DELETE THE PIKUP
!       AND PRINT MESSAGE
!
                     PIKUP(1:6,J,I)=0.0D0
                     OUTLYNE='PIKUP REFERENCED BY DELETED SURFACE HAS BEEN DELETED'
                     CALL SHOWIT(1)
                     OUTLYNE='ASSOCIATED PARAMETER FROZEN AT CURRENT VALUE'
                     CALL SHOWIT(1)
                  END IF
               END IF
            END DO
         END IF
!       CHECK THE NEXT SURFACE
      END DO
!
!               NOW FIX THE STATUS OF ALENS(32,K) FOR ALL
!       SURFACES K=0 TO INT(SYSTEM(20))
!
      DO 405 K=0,INT(SYSTEM(20))
         IF(ALENS(32,K).NE.0.0D0) THEN
!       THE MIGHT BE PIKUPS
!       SHOULD THE STATUS BE CHANGED
            PIKCNT=0
            DO 406 I=1,PSIZ
               IF(PIKUP(1,K,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
               ELSE
!       PROCEED
               END IF
406         CONTINUE
!       IF NO PIKUPS FOUND, CHANGE STATUS
            IF(PIKCNT.EQ.0) ALENS(32,K)=0.0D0
!       PROCEED TO CHECK THE NEXT SURFACE FOR PIKUPS TO BE
!       STATUS RESOLVED
         ELSE
         END IF
405   CONTINUE
!
      IF(ALENS(34,SURF).NE.0.0D0) THEN
         OUTLYNE=&
         &'SPECIAL SURFACE DATA BEING DELETED WITH SURFACE DELETION'
         CALL SHOWIT(1)
      END IF
!
      K=INT(SYSTEM(20))-1
      DO J=SURF,K
         ALENS(1:LSIZ,J)=ALENS(1:LSIZ,(J+1))
      END DO
!
      DO I=SURF,K
         LBL(I)(1:80)=LBL(I+1)(1:80)
      END DO
!
      DO J=SURF,K
         FTFL01(1:96,J)=FTFL01(1:96,(J+1))
      END DO
      DO J=SURF,K
         PRINT *, "GLANAM(J,1) is ", GLANAM(J,1)
         PRINT *, "GLANAM(J,2) is ", GLANAM(J,2)
         GLANAM(J,1)=GLANAM((J+1),1)
         GLANAM(J,2)=GLANAM((J+1),2)
         SOLVE(0:9,J) = SOLVE(0:9,(J+1))
      END DO
!     THE FIRST SURFACE IN THE LOOP IS THE SURFACE 0
!     SNUMBER IS THE CURRENT SURFACE
      DO J=0,K
         DO I=1,PSIZ
            IF(I.NE.32) THEN
!     NOT THOAL
!       IF THE SURFACE NUMBER OF THE SURFACE BEING DELETED IS
!       NUMERICALLY LESS THAN OR EQUAL TO SURFACE NUMBER REFERENCED
!       BY THE PIKUP (PIKUP(2,J+1,I)
               IF(SURF.LE.INT(PIKUP(2,J+1,I)))&
               &PIKUP(2,J+1,I)=PIKUP(2,J+1,I)-1.0D0
            ELSE
!     THOAL
               IF(SURF.LE.INT(PIKUP(2,J+1,I)))&
               &PIKUP(2,J+1,I)=PIKUP(2,J+1,I)-1.0D0
               IF(SURF.LE.INT(PIKUP(3,J+1,I)))&
               &PIKUP(3,J+1,I)=PIKUP(3,J+1,I)-1.0D0
            END IF
         END DO
      END DO
      DO J=SURF,K
         PIKUP(1:6,J,1:PSIZ)=PIKUP(1:6,J+1,1:PSIZ)
      END DO
!
!       DECREMENT THE IMAGE SURFACE NUMBER COUNTER SYSTEM(20) BY 1.0D0
      IF(SYSTEM(20).GT.0.0D0) THEN
         SYSTEM(20)=(SYSTEM(20)-1.0D0)
      ELSE
         SYSTEM(20)=0.0D0
      END IF
!
!       IF THE CURRENT SURFACE IS THE IMAGE SURFACE,
!       PRINT WARNING MESSAGE
!
      IF(SURF.EQ.INT(SYSTEM(20)))THEN
         OUTLYNE='CURRENT SURFACE IS NOW THE IMAGE SURFACE'
         CALL SHOWIT(1)
      END IF
      F22=1
!     HERE IS WHERE WE CALL CFGFX2.FOR  AND CFGFX3.FOR TO FIX POSSIBLE
!     CONFIGS DATA IMPACTED BY THE DELETION
      CALL CFGFX2(SURF)
      CALL CFGFX3(SURF)
!     SURF WAS DELETED
      DO I=0,INT(SYSTEM(20))
         IF(ALENS(25,I).EQ.6.0D0.OR.ALENS(25,I).EQ.1.0D0.AND.&
         &ALENS(77,I).EQ.1) THEN
!       FOUND A TILT RET
            IF((ALENS(70,I)).GE.SURF) ALENS(70,I)=ALENS(70,I)-1.0D0
         END IF
      END DO
   END DO
   RETURN
END
! SUB SDEC.FOR
SUBROUTINE SDEC
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SDEC WHICH IMPLEMENTS THE DEC
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!       THE DEC COMMAND IS:
!
!               DEC, YD XD ZD
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE='"DEC" TAKES NO QUALIFIER OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE='"DEC" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
      OUTLYNE='"DEC" REQUIRES SOME EXPLICIT NUMERIC INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         OUTLYNE='OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               PROCEED
!
!       DON'T SET DEC FLAG (ALENS(29,SURF) IF YD AND XD ARE BOTH 0.0D0
!       BUT CLEAR IT.
!
!       CHECK FOR PIKUP YD OR PIKUP XD OR ZD AND IF FOUND REMOVE
!
      IF(PIKUP(1,SURF,13).EQ.1.0D0) THEN
!       DELETE PIKUP YD
         PIKUP(1:6,SURF,13)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (YD) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,33).EQ.1.0D0) THEN
!       DELETE PIKUP YD
         PIKUP(1:6,SURF,33)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ZD) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,14).EQ.1.0D0) THEN
!       DELETE PIKUP XD
         PIKUP(1:6,SURF,14)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (XD) DELETED'
         CALL SHOWIT(1)
      END IF
!
!       SET ALENS(32,SURF) IF NO OTHER PIKUPS EXIST
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) ALENS(32,SURF)=0.0D0
!
!       PROCEED WITH DEC ASSIGNMENT
!
      IF(W1.EQ.0.0D0.AND.W2.EQ.0.0D0.AND.W3.EQ.0.0D0) THEN
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         ALENS(29,SURF)=0.0D0
         ALENS(30,SURF)=W1
         ALENS(31,SURF)=W2
         ALENS(69,SURF)=W3
         ALENS(115,SURF)=W1
         ALENS(114,SURF)=W2
         ALENS(116,SURF)=W3
      ELSE
!       NOT BOTH W1 AND W2 ARE ZERO, PROCEED
!       SET DEC FLAG AND VALUES.
         IF(DF1.EQ.1) W1=ALENS(30,SURF)
         IF(DF2.EQ.1) W2=ALENS(31,SURF)
         IF(DF3.EQ.1) W3=ALENS(69,SURF)
         ALENS(29,SURF)=1.0D0
         ALENS(30,SURF)=W1
         ALENS(31,SURF)=W2
         ALENS(69,SURF)=W3
         ALENS(115,SURF)=W1
         ALENS(114,SURF)=W2
         ALENS(116,SURF)=W3
      END IF
!
!       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
!       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
!       IF THE CURRENT WORK HERE CHANGED THE STATUS OF ALENS(29,SURF)
!       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
!       RESOLVE THIS ISSUE.
!
      DO 100 I=0,INT(SYSTEM(20))
         IF(PIKUP(1,I,13).EQ.1.0D0.OR.&
         &PIKUP(1,I,14).EQ.1.0D0.OR.PIKUP(1,I,33).EQ.1.0D0.OR.&
         &PIKUP(1,I,13).EQ.1.0D0.AND.PIKUP(1,I,14).EQ.1.0D0 &
         &.AND.PIKUP(1,I,33).EQ.1.0D0) THEN
            IF(INT(PIKUP(2,I,13)).EQ.SURF.OR.&
            &INT(PIKUP(2,I,14)).EQ.SURF.OR.INT(PIKUP(2,I,33)).EQ.&
            &SURF) THEN
!       UPDATE THE ALENS(29) ON THE PIKING SURFACE
               ALENS(29,I)=ALENS(29,SURF)
            ELSE
!       DONT DO ANYTHING
            END IF
         ELSE
!       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
         END IF
100   CONTINUE
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
      IF(ALENS(25,SURF).EQ.2.0D0.OR.ALENS(25,SURF).EQ.&
      &3.0D0) THEN
         ALENS(25,SURF)=1.0D0
         IF(ALENS(25,SURF).EQ.2.0D0) OUTLYNE=&
         &'"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
         IF(ALENS(25,SURF).EQ.3.0D0) OUTLYNE=&
         &'"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
         CALL SHOWIT(1)
      END IF
   END IF
   RETURN
END
