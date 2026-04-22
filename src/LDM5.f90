!       FIFTH FILE FOR LENS DATABASE MANAGER FILES

! SUB SNAO.FOR
SUBROUTINE SNAO
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SNAO WHICH IMPLEMENTS THE NAO(X OR Y) COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE NAO(X OR Y) COMMAND AT
!       THE CMD LEVEL
!
!
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
         OUTLYNE='AT THE CMD LEVEL, "NAOY" AND "NAOX"'
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
         IF(WC.EQ.'NAOY') THEN
            IF(SYSTEM(64).NE.1.0D0.AND.SYSTEM(64).NE.3.0D0) THEN
               OUTLYNE='NOTE:'
               CALL SHOWIT(1)
               OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
               CALL SHOWIT(1)
               IF(INT(SYSTEM(11)).GE.1.AND.INT(SYSTEM(11)).LE.5) THEN
                  SYSTEM(65)=(ALENS(45+INT(SYSTEM(11)),0)*SYSTEM(12))/&
                  &DSQRT((ALENS(3,0)**2)+(SYSTEM(12)**2))
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
               IF(INT(SYSTEM(11)).GE.6.AND.INT(SYSTEM(11)).LE.10) THEN
                  SYSTEM(65)=(ALENS(70-5+INT(SYSTEM(11)),0)*SYSTEM(12))/&
                  &DSQRT((ALENS(3,0)**2)+(SYSTEM(12)**2))
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
            END IF
            WRITE(OUTLYNE,2000) SYSTEM(65)
            CALL SHOWIT(0)
            RETURN
         END IF
         IF(WC.EQ.'NAOX') THEN
            IF(SYSTEM(64).NE.2.0D0.AND.SYSTEM(64).NE.3.0D0) THEN
               OUTLYNE='NOTE:'
               CALL SHOWIT(1)
               OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
               CALL SHOWIT(1)
               IF(INT(SYSTEM(11)).GE.1.AND.INT(SYSTEM(11)).LE.5) THEN
                  SYSTEM(66)=(ALENS(45+INT(SYSTEM(11)),0)*SYSTEM(13))/&
                  &DSQRT((ALENS(3,0)**2)+(SYSTEM(13)**2))
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
               IF(INT(SYSTEM(11)).GE.6.AND.INT(SYSTEM(11)).LE.10) THEN
                  SYSTEM(66)=(ALENS(70-5+INT(SYSTEM(11)),0)*SYSTEM(13))/&
                  &DSQRT((ALENS(3,0)**2)+(SYSTEM(13)**2))
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
            END IF
            WRITE(OUTLYNE,2001)
            CALL SHOWIT(0)
            WRITE(OUTLYNE,3000) SYSTEM(66)
            CALL SHOWIT(0)
            RETURN
         END IF
      END IF
!               NOT AT CMD LEVEL
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(STI.EQ.1) THEN
         OUTLYNE=&
         &'QUERRY OBJECT N.A. VALUES FROM THE CMD LEVEL WITH THE'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'"NAOY" OR "NAOX" COMMANDS'
         CALL SHOWIT(1)
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1) THEN
!
         IF(WC.EQ.'NAOY') THEN
            OUTLYNE='"NAOY" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'NAOX') THEN
            OUTLYNE='"NAOX" TAKES NO STRING INPUT'
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
         IF(WC.EQ.'NAOY') THEN
            OUTLYNE='"NAOY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'NAOX') THEN
            OUTLYNE='"NAOX" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.1.AND.F5.EQ.1) THEN
!
         IF(WC.EQ.'NAOY') THEN
            OUTLYNE='"NAOY" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'NAOX') THEN
            OUTLYNE='"NAOX" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'
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
            IF(WC.EQ.'NAOY') THEN
               OUTLYNE='INVALID QUALIFIER WORD USED WITH "NAOY"'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'NAOX') THEN
               OUTLYNE='INVALID QUALIFIER WORD USED WITH "NAOX"'
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
         IF(WC.EQ.'NAOY') THEN
            OUTLYNE='"NAOY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'NAOX') THEN
            OUTLYNE='"NAOX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(DABS(ALENS(3,0)).GE.1.0D10) THEN
!     INFINITE OBJECT THICKNESS, DON'T ALLOW
         IF(WC.EQ.'NAOY') THEN
            OUTLYNE='OBJECT DISTANCE IS GREATER THAN +/-1.0D10'
            CALL SHOWIT(1)
            OUTLYNE='"NAOY" MAY NOT BE USED'
            CALL SHOWIT(1)
            OUTLYNE='RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(WC.EQ.'NAOX') THEN
            OUTLYNE='OBJECT DISTANCE IS GREATER THAN +/-1.0D10'
            CALL SHOWIT(1)
            OUTLYNE='"NAOX" MAY NOT BE USED'
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
         IF(WC.EQ.'NAOY') THEN
            WRITE(OUTLYNE,2000) SYSTEM(65)
            CALL SHOWIT(0)
         END IF
         IF(WC.EQ.'NAOX') THEN
            WRITE(OUTLYNE,3000) SYSTEM(66)
            CALL SHOWIT(0)
            IF(SYSTEM(49).EQ.0.0D0) SYSTEM(49)=1.0D0
            IF(SYSTEM(49).EQ.2.0D0) SYSTEM(49)=3.0D0
         END IF
         RETURN
      ELSE
         IF(W1.EQ.0.0D0) THEN
            IF(WC.EQ.'NAOY') THEN
               OUTLYNE='"NAOY" MAY NOT BE SET TO ZERO'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            IF(WC.EQ.'NAOX') THEN
               OUTLYNE='"NAOX" MAY NOT BE SET TO ZERO'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'NAOY') THEN
               SYSTEM(83)=0.0D0
               SYSTEM(84)=0.0D0
               IF(SYSTEM(64).NE.3.0D0.AND.SYSTEM(64).NE.1.0D0) THEN
                  IF(SYSTEM(64).EQ.0.0D0) SYSTEM(64)=1.0D0
                  IF(SYSTEM(64).EQ.2.0D0) SYSTEM(64)=3.0D0
               END IF
            END IF
            IF(WC.EQ.'NAOX') THEN
               IF(SYSTEM(64).NE.3.0D0.AND.SYSTEM(64).NE.2.0D0) THEN
                  IF(SYSTEM(64).EQ.0.0D0) SYSTEM(64)=2.0D0
                  IF(SYSTEM(64).EQ.1.0D0) SYSTEM(64)=3.0D0
                  SYSTEM(83)=0.0D0
                  SYSTEM(84)=0.0D0
               END IF
            END IF
            SYSTEM(67)=0.0D0
            IF(WC.EQ.'NAOY') SYSTEM(65)=DABS(W1)
            IF(WC.EQ.'NAOX') SYSTEM(66)=DABS(W1)
         END IF
         IF(WQ.EQ.'DELT') THEN
            SYSTEM(67)=0.0D0
            IF(WC.EQ.'NAOY') SYSTEM(65)=SYSTEM(65)+(W1)
            IF(WC.EQ.'NAOX') SYSTEM(66)=SYSTEM(66)+(W1)
         END IF
         IF(WQ.EQ.'CENT') THEN
            SYSTEM(67)=0.0D0
            IF(WC.EQ.'NAOY') SYSTEM(65)=SYSTEM(65)+(W1*0.0D0*SYSTEM(65))
            IF(WC.EQ.'NAOX') SYSTEM(66)=SYSTEM(66)+(W1*0.0D0*SYSTEM(66))
         END IF
         RETURN
      END IF
   END IF
2001 FORMAT(1X)
2000 FORMAT('NAOY=',1X,D23.15)
3000 FORMAT('NAOX=',1X,D23.15)
   RETURN
END
! SUB SMODE.FOR
SUBROUTINE SMODE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SMODE WHICH IMPLEMENTS THE MODE
!       COMMAND AT THE LENS INPUT, UPDATE LENS AND CMD LEVEL.
!
   INTEGER AMODE
!
!
   IF(STI.EQ.1) THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
         AMODE=INT(SYSTEM(30))
         IF(AMODE.EQ.1) THEN
            WRITE(OUTLYNE,1000)
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(AMODE.EQ.2) THEN
            WRITE(OUTLYNE,2000)
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(AMODE.EQ.3) THEN
            WRITE(OUTLYNE,3000)
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(AMODE.EQ.4) THEN
            WRITE(OUTLYNE,4000)
            CALL SHOWIT(0)
         ELSE
         END IF
         RETURN
      ELSE
!       AT CMD
      END IF
   ELSE
!       NOT STI
   END IF
!       BEHAVIOR AT CMD LEVEL
   AMODE=INT(SYSTEM(30))
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         OUTLYNE='"MODE" TAKES NO STRING OR NUMERIC INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!       MODE CHANGE AT CMD LEVEL. INORDER TO BE COMPATABLE
!       WITH ACCOS V AND HEXAGON, THE MODE MAY BE CHANGED AT
!       THE CMD LEVEL.
!
      IF(SQ.EQ.1) THEN
         IF(WQ.EQ.'FOCAL')THEN
            SYSTEM(30)=1.0
            IF(F12.EQ.1) SYSP(30)=1.0
            RETURN
         ELSE
         END IF
         IF(WQ.EQ.'UFOCAL')THEN
            SYSTEM(30)=2.0
            IF(F12.EQ.1) SYSP(30)=2.0
            RETURN
         ELSE
         END IF
         IF(WQ.EQ.'AFOCAL')THEN
            SYSTEM(30)=3.0
            IF(F12.EQ.1) SYSP(30)=3.0
            RETURN
         ELSE
         END IF
         IF(WQ.EQ.'UAFOCAL')THEN
            SYSTEM(30)=4.0
            IF(F12.EQ.1) SYSP(30)=4.0
            RETURN
         ELSE
         END IF
         OUTLYNE='INVALID QUALIFIER INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
      IF(AMODE.EQ.1) THEN
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
      ELSE
      END IF
      IF(AMODE.EQ.2) THEN
         WRITE(OUTLYNE,2000)
         CALL SHOWIT(0)
      ELSE
      END IF
      IF(AMODE.EQ.3) THEN
         WRITE(OUTLYNE,3000)
         CALL SHOWIT(0)
      ELSE
      END IF
      IF(AMODE.EQ.4) THEN
         WRITE(OUTLYNE,4000)
         CALL SHOWIT(0)
      ELSE
      END IF
      RETURN
   ELSE
!       NOT AT CMD LEVEL
   END IF
!
!
!       BEHAVIOR AT LENS INPUT AND LENS UPDATE LEVEL
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF STRING OR NUMERIC
!               INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         OUTLYNE='"MODE" ACCEPTS NO STRING OR NUMERIC INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      IF(SQ.EQ.0) THEN
         OUTLYNE='"MODE" REQUIRES EXPLICIT NUMERIC INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
!
      IF(WQ.NE.'FOCAL'.AND.WQ.NE.'UFOCAL'.AND.&
      &WQ.NE.'AFOCAL'.AND.WQ.NE.'UAFOCAL'.AND.WQ.NE.' ') THEN
         OUTLYNE='INVALID QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
         IF(WQ.EQ.'FOCAL') THEN
            SYSTEM(30)=1.0
            IF(F12.EQ.1) SYSP(30)=1.0
         ELSE
         END IF
         IF(WQ.EQ.'UFOCAL') THEN
            SYSTEM(30)=2.0
            IF(F12.EQ.1) SYSP(30)=2.0
         ELSE
         END IF
         IF(WQ.EQ.'AFOCAL') THEN
            SYSTEM(30)=3.0
            IF(F12.EQ.1) SYSP(30)=3.0
         ELSE
         END IF
         IF(WQ.EQ.'UAFOCAL') THEN
            SYSTEM(30)=4.0
            IF(F12.EQ.1) SYSP(30)=4.0
         ELSE
         END IF
      END IF
!
   ELSE
   END IF
1000 FORMAT('EVALUATION MODE IS FOCAL')
2000 FORMAT('EVALUATION MODE IS UFOCAL')
3000 FORMAT('EVALUATION MODE IS AFOCAL')
4000 FORMAT('EVALUATION MODE IS UAFOCAL')
!
   RETURN
END
! SUB SMAG.FOR
SUBROUTINE SMAG
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SMAG WHICH IMPLEMENTS THE MAGY AND MAGX
!       COMMANDS AT THE CMD LEVEL.
!
   REAL*8 MAG
!
   INTEGER I,J
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
      IF(WC.EQ.'MAGY') THEN
         OUTLYNE='"MAGY" TAKES ONLY NUMERIC WORDS #1,#2 AND #3 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
      END IF
      IF(WC.EQ.'MAGX') THEN
         OUTLYNE='"MAGX" TAKES ONLY NUMERIC WORD #1, #2 AND #3 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      IF(WC.EQ.'MAGY') THEN
         OUTLYNE=&
         &'"MAGY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      ELSE
      END IF
      IF(WC.EQ.'MAGX') THEN

         OUTLYNE=&
         &'"MAGX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(DF2.EQ.1) W2=0.0D0
   IF(DF2.EQ.1) W3=SYSTEM(20)
   IF(W2.LT.0.0.OR.W3.GT.SYSTEM(20)) THEN
      OUTLYNE=&
      &'REQUESTED SURFACES BEYOND LEGAL RANGE FOR CURRENT LENS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!       NUMERIC INPUT NOT DEFAULT
   IF(WC.EQ.'MAGY') THEN
      IF(W1.EQ.0.0D0) W1=1.0D0
      IF(W2.EQ.0.0D0) W2=0.0D0
      IF(W3.EQ.0.0D0) W3=SYSTEM(20)
      GO TO 25
   ELSE
!       WC MUST BE 'MAGX'
      IF(W1.EQ.0.0D0) W1=1.0D0
      IF(W2.EQ.0.0D0) W2=0.0D0
      IF(W3.EQ.0.0D0) W3=SYSTEM(20)
   END IF
   GO TO 35
25 CONTINUE
   MAG=W1
   I=INT(W2)
   J=INT(W3)
   PRINT *, "MAG is ", W1
   PRINT *, "Surf1 is ", W2
   PRINT *, "Surf2 is ", W3


!       HERE IS WHERE THE MAGY ADJUSTMENT IS MADE BY CALLING
!       SUBROUTINE MGYADJ.FOR
   CALL MGADJ(MAG,I,J)
   RETURN
35 CONTINUE
   MAG=W1
   I=INT(W2)
   J=INT(W3)
!       HERE IS WHERE THE MAGX ADJUSTMENT IS MADE BY CALLING
!       SUBROUTINE MGXADJ.FOR
   CALL MGADJ(MAG,I,J)
   RETURN
END
! SUB SLVRS.FOR
SUBROUTINE SLVRS
   use global_widgets, only: curr_par_ray_trace, curr_lens_data
   use type_utils, only: int2str
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SLVRS . IT IS USED TO HANDLE
!       THE RESOLUTION OF ALL Y-Z/X-Z PLANE SOLVES ON SURFACE SL.
!       IT IS CALLED PRIMARILY FROM SUBROUTINE PRTRA.
!       CAY AND CAX SOLVES DO AMAMORPHIC ASPERES AS OF 1 FEB 1992
!
   INTEGER TAR,ITP,SL,L,I,SLV1,SLV2,FINY
!
   COMMON/CSLVRS/SLV1,SLV2
!
   COMMON/FINER/FINY
!
   REAL*8 ARG,DIS,EDGVAL,SAGL,SAGLP1,N,J_NP, newThick
!
!
   L=SLV1
   IF(SLV2.EQ.1) THEN
!       THE SURFACE OF SOLVE EVALUATION IS THE PASSED PARAMETER L.
!       NOW HANDLE ALL SOLVES ON SURFACE L
!       SIMPLIFY THE REPRESENTATION OF THE REFRACTIVE INDICES
!       AT THE CONTROL WAVELENGTH AT L-1 AND L
      IF(L.NE.0) THEN
         IF(INT(SYSTEM(11)).GE.1.AND.INT(SYSTEM(11)).LE.5) THEN
            N=ALENS(45+INT(SYSTEM(11)),(L-1))
            J_NP=ALENS(45+INT(SYSTEM(11)),(L))
         END IF
         IF(INT(SYSTEM(11)).GE.6.AND.INT(SYSTEM(11)).LE.10) THEN
            N=ALENS(65+INT(SYSTEM(11)),(L-1))
            J_NP=ALENS(65+INT(SYSTEM(11)),(L))
         END IF
      END IF
!
!                       FIRST,CURVATURE SOLVES
!***************************************************************************
!                       PUY SOLVES
!***************************************************************************
!       IS THERE A PUY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.3.0D0) THEN
!       YES THERE IS A PUY SOLVE ON SURFACE L, HANDLE IT
!       FOR A PUY SOLVE
!       CV(L)=(-SOLVE TARGET+(N/N')*PUY(L-1))*(N'/(N'-N))*(1/PY(L))
!       AND PUY(L)=SOLVE TARGET
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC
            PXTRAY(2,L)=SOLVE(9,L)
            IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=((-PXTRAY(2,L))+((N/J_NP)*PXTRAY(2,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAY(1,L))
            END IF
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            PXTRAY(2,L)=SOLVE(9,L)
            IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=((-PXTRAY(2,L))+((N/J_NP)*PXTRAY(2,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAY(1,L))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PUY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PUY SOLVE
      END IF
!*****************************************************************************
!                       PUCY SOLVES
!***************************************************************************
!       IS THERE A PUCY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.6.0D0) THEN
!       YES THERE IS A PUCY SOLVE ON SURFACE L, HANDLE IT
!       FOR A PUCY SOLVE
!       CV(L)=(-SOLVE TARGET+(N/N')*PUCY(L-1))*(N'/(N'-N))*(1/PCY(L))
!       AND PUCY(L)=SOLVE TARGET
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC
            PXTRAY(6,L)=SOLVE(9,L)
            IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=((-PXTRAY(6,L))+((N/J_NP)*PXTRAY(6,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAY(5,L))
            END IF
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            PXTRAY(6,L)=SOLVE(9,L)
            IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=((-PXTRAY(6,L))+((N/J_NP)*PXTRAY(6,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAY(5,L))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PUCY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PUCY SOLVE
      END IF
!***************************************************************************
!                       PIY SOLVES
!***************************************************************************
!       IS THERE A PIY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.2.0D0) THEN
!       YES THERE IS A PIY SOLVE ON SURFACE L, HANDLE IT
!       FOR A PIY SOLVE
!       CV(L)=(SOLVE TARGET-PUY(L-1))/PY(L)
!       AND PIY(L)=SOLVE TARGET
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC
            PXTRAY(3,L)=SOLVE(9,L)
            IF(PXTRAY(1,L).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=(PXTRAY(3,L)-PXTRAY(2,(L-1)))/&
               &(PXTRAY(1,(L)))
            END IF
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            PXTRAY(3,L)=SOLVE(9,L)
            IF(PXTRAY(1,L).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=(PXTRAY(3,L)-PXTRAY(2,(L-1)))/&
               &(PXTRAY(1,(L)))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PIY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PIY SOLVE
      END IF
!*****************************************************************************
!                       PICY SOLVES
!***************************************************************************
!       IS THERE A PICY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.5.0D0) THEN
!       YES THERE IS A PICY SOLVE ON SURFACE L, HANDLE IT
!       FOR A PICY SOLVE
!       CV(L)=(SOLVE TARGET-PUCY(L-1))/PCY(L)
!       AND PICY(L)=SOLVE TARGET
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC
            PXTRAY(7,L)=SOLVE(9,L)
            IF(PXTRAY(5,L).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=(PXTRAY(7,L)-PXTRAY(6,(L-1)))/&
               &(PXTRAY(5,(L)))
            END IF
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            PXTRAY(7,L)=SOLVE(9,L)
            IF(PXTRAY(5,L).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=(PXTRAY(7,L)-PXTRAY(6,(L-1)))/&
               &(PXTRAY(5,(L-1)))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PICY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PICY SOLVE
      END IF
!*****************************************************************************
!                       APY SOLVES
!***************************************************************************
!       IS THERE A APY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.1.0D0) THEN
!       YES THERE IS A APY SOLVE ON SURFACE L, HANDLE IT
!       FOR AN APY SOLVE
!       CV(L)=-(PUY(L-1)/PY(L))*((N'+N)/N)
!       THEN
!       PUY(L)=(N/N')*PUY(L-1)-CV(L)*PY(L)*((N'-N)/N')
!       PIY(L)=CV(L)*PY(L)+PUY(L-1)
!       PIY'(L)=(N/N')*PIY(L)
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC, PROCEED
            IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=-(PXTRAY(2,(L-1))/PXTRAY(1,L))*((J_NP+N)/N)
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAY(2,L)=((N/J_NP)*PXTRAY(2,(L-1)))-(ALENS(24,L)*&
            &PXTRAY(1,L)*((J_NP-N)/J_NP))
            PXTRAY(3,L)=(ALENS(24,L)*PXTRAY(1,L))+PXTRAY(2,(L-1))
            PXTRAY(4,L)=(N/J_NP)*PXTRAY(3,L)
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            IF(PXTRAY(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=-(PXTRAY(2,(L-1))/PXTRAY(1,L))*((J_NP+N)/N)
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAY(2,L)=((N/J_NP)*PXTRAY(2,(L-1)))-(ALENS(1,L)*&
            &PXTRAY(1,L)*((J_NP-N)/J_NP))
            PXTRAY(3,L)=(ALENS(1,L)*PXTRAY(1,L))+PXTRAY(2,(L-1))
            PXTRAY(4,L)=(N/J_NP)*PXTRAY(3,L)
         END IF
!       APY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO APY SOLVE
      END IF
!***************************************************************************
!                       APCY SOLVES
!***************************************************************************
!       IS THERE A APCY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.4.0D0) THEN
!       YES THERE IS A APCY SOLVE ON SURFACE L, HANDLE IT
!       FOR AN APCY SOLVE
!       CV(L)=-(PUCY(L-1)/PCY(L))*((N'+N)/N)
!       THEN
!       PUCY(L)=(N/N')*PUCY(L-1)-CV(L)*PCY(L)*((N'-N)/N')
!       PICY(L)=CV(L)*PCY(L)+PUCY(L-1)
!       PICY'(L)=(N/N')*PICY(L)
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC, PROCEED
            IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=-(PXTRAY(6,(L-1))/PXTRAY(5,L))*((J_NP+N)/N)
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(24,L)*&
            &PXTRAY(5,L)*((J_NP-N)/J_NP))
            PXTRAY(7,L)=(ALENS(24,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
            PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            IF(PXTRAY(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=-(PXTRAY(6,(L-1))/PXTRAY(5,L))*((J_NP+N)/N)
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(1,L)*&
            &PXTRAY(5,L)*((J_NP-N)/J_NP))
            PXTRAY(7,L)=(ALENS(1,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
            PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
         END IF
!       APCY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO APCY SOLVE
      END IF
!**************************************************************************
!                       COCY SOLVES
!***************************************************************************
!       IS THERE A COCY SOLVE ON SURFACE L
      IF(SOLVE(8,L).EQ.7.0D0) THEN
         DIS=0.0D0
!       YES THERE IS A COCY SOLVE ON SURFACE L, HANDLE IT
!       FOR AN COCY SOLVE
!       CV(L)=SIGNED DISTANCE TO THE SPECIFIED SURFACE(TAR)
!       FROM SURFACE L EQUAL TO DIS
         TAR=INT(SOLVE(9,L))
         IF(TAR.LT.L) THEN
            DO I=TAR,(L-1)
               DIS=DIS+ALENS(3,I)
            END DO
         ELSE
         END IF
         IF(TAR.GT.L) THEN
            DO I=L,(TAR-1)
               DIS=DIS+ALENS(3,I)
            END DO
         ELSE
         END IF
!       CASE OF TAR=L ELIMINATED INSIDE OD SUBROUTINE LNSEOS.
!       THEN RECALCULATE
!       PUCY(L)=(N/N')*PUCY(L-1)-CV(L)*PCY(L)*((N'-N)/N')
!       PICY(L)=CV(L)*PCY(L)+PUCY(L-1)
!       PICY'(L)=(N/N')*PICY(L)
!       IS SURFACE L AN X-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.2.0D0) THEN
!       SURFACE IS X-TORIC, PROCEED
            IF(DIS.NE.0.0D0) THEN
               IF(TAR.LT.L) ALENS(24,L)=-1.0D0/DIS
               IF(TAR.GT.L) ALENS(24,L)= 1.0D0/DIS
            ELSE
!       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
               OUTLYNE='(COCY) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
               CALL SHOWIT(1)
               OUTLYNE='(COCY) SOLVE REMOVED'
               CALL SHOWIT(1)
               SOLVE(8,L)=0.0D0
               SOLVE(9,0)=0.0D0
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(24,L)*&
            &PXTRAY(5,L)*((J_NP-N)/J_NP))
            PXTRAY(7,L)=(ALENS(24,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
            PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
         ELSE
!       SURFACE NOT X-TORIC,PROCEED
            IF(DIS.NE.0.0D0) THEN
               IF(TAR.LT.L) ALENS(1,L)=-1.0D0/DIS
               IF(TAR.GT.L) ALENS(1,L)=1.0D0/DIS
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
!       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
               OUTLYNE='(COCY) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
               CALL SHOWIT(1)
               OUTLYNE='(COCY) SOLVE REMOVED'
               CALL SHOWIT(1)
               SOLVE(8,L)=0.0D0
               SOLVE(9,0)=0.0D0
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAY(6,L)=((N/J_NP)*PXTRAY(6,(L-1)))-(ALENS(1,L)*&
            &PXTRAY(5,L)*((J_NP-N)/J_NP))
            PXTRAY(7,L)=(ALENS(1,L)*PXTRAY(5,L))+PXTRAY(6,(L-1))
            PXTRAY(8,L)=(N/J_NP)*PXTRAY(7,L)
         END IF
!       COCY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO COCY SOLVE
      END IF
!**************************************************************************
!               NOW THICKNESS SOLVES
!**************************************************************************
!                       PY SOLVES
!***************************************************************************
!       IS THERE A PY SOLVE ON SURFACE L
      IF(SOLVE(6,L).EQ.1.0D0) THEN
!       YES THERE IS A PY SOLVE ON SURFACE L
!       WHICH AFFECTS THE PY VALUE ON (L+1), HANDLE IT
!       TH(L)=(SOLVE TARGET-PY(L))/PUY(L)
!
         IF(PXTRAY(2,L).EQ.0.0D0) THEN
            ALENS(3,L)=1D20
         ELSE
            ALENS(3,L)=(SOLVE(7,L)-PXTRAY(1,(L)))/(PXTRAY(2,L))
            IF(DABS(ALENS(3,L)).GT.1.0D20)THEN
               IF(ALENS(3,L).GT.0.0D0) THEN
                  ALENS(3,L)=1.0D20
               ELSE
               END IF
               IF(ALENS(3,L).LT.0.0D0) THEN
                  ALENS(3,L)=-1.0D20
               ELSE
               END IF
            ELSE
            END IF
         END IF
!       PY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PY SOLVE
      END IF
!**************************************************************************
!                       PCY SOLVES
!***************************************************************************
!       IS THERE A PCY SOLVE ON SURFACE L
      IF(SOLVE(6,L).EQ.2.0D0) THEN
!       YES THERE IS A PCY SOLVE ON SURFACE L
!       WHICH AFFECTS THE PCY VALUE ON (L+1), HANDLE IT
!       TH(L)=(SOLVE TARGET-PCY(L))/PUCY(L)
!
         IF(PXTRAY(6,L).EQ.0.0D0) THEN
            ALENS(3,L)=1.0D20
         ELSE
            ALENS(3,L)=(SOLVE(7,L)-PXTRAY(5,(L)))/(PXTRAY(6,L))
            IF(DABS(ALENS(3,L)).GT.1D20)THEN
               IF(ALENS(3,L).GT.0.0D0) THEN
                  ALENS(3,L)=1.0D20
               ELSE
               END IF
               IF(ALENS(3,L).LT.0.0D0) THEN
                  ALENS(3,L)=-1.0D20
               ELSE
               END IF
            ELSE
            END IF
         END IF
!       PCY SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PCY SOLVE
      END IF
!***************************************************************************
!                       CAY SOLVES
!******************************************************************************
!       CHECK FOR CAY SOLVE ON (L) AND IF FOUND, HANDLE IT.
      IF(SOLVE(6,(L)).EQ.3.0D0) THEN
!       THERE IS A CAY SOLVE ON (L) WHICH AFFECTS TH(L), HANDLE IT.
!       IS THER A CIRCULAR CLEAR APERTURE ON SURFACE L
         IF(ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0.OR.&
         &ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0) THEN
!       YES THERE IS, SET EDGVAL TO IT
            IF(ALENS(10,L).LE.ALENS(11,L)) THEN
               EDGVAL=DABS(ALENS(10,(L)))
            ELSE
               EDGVAL=DABS(ALENS(11,(L)))
            END IF
         ELSE
!       NO CIRCULAR CLAP, USE PY(L)+PCY(L) FOR EDGVAL
            EDGVAL=DABS(PXTRAY(1,(L)))+DABS(PXTRAY(5,(L)))
         END IF
!                       NOW EDGEVAL IS SET
!       NOW CALCULATE THE SAG OF SURFACES L AND L+1
!       CONSIDER THE YZ-PLANE ONLY AND IGNORE TILTS,
!       DECENTERS AND SPECIAL SURFACE DATA. CONSIDER
!       TORICS IF PRESENT. CONSIDER CONIC AND ASPHERIC
!       TERMS IF PRESENT.
!       CHECK FOR ANAMORPHICS
         IF(ALENS(36,L).EQ.0.0D0) THEN
!       IS THE SURFACE L X-TORIC BUT NOT ANAMORPHIC ASPHERE?
            IF(ALENS(23,(L)).EQ.2.0D0) THEN
!       USE TORIC DATA IN SAG CALC
               ARG=(1.0D0-((1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(24,(L))))/&
               &(1.0D0+DSQRT(ARG)))
            ELSE
!       NOT X-TORIC
               ARG=(1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(1,(L))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L))*(EDGVAL**4))+&
               &(ALENS(5,(L))*(EDGVAL**6))+&
               &(ALENS(6,(L))*(EDGVAL**8))+&
               &(ALENS(7,(L))*(EDGVAL**10))+&
               &(ALENS(81,(L))*(EDGVAL**12))+&
               &(ALENS(82,(L))*(EDGVAL**14))+&
               &(ALENS(83,(L))*(EDGVAL**16))+&
               &(ALENS(84,(L))*(EDGVAL**18))+&
               &(ALENS(85,(L))*(EDGVAL**20))
            END IF
         ELSE
!       ANAMORPHIC ASPHERIC
            IF(ALENS(23,L).EQ.1.0D0) THEN
!       Y-TORIC
               ARG=(1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(1,L)))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,L)*(((1.0D0-ALENS(37,L))*(EDGVAL**2))**2))+&
               &(ALENS(5,L)*(((1.0D0-ALENS(38,L))*(EDGVAL**2))**3))+&
               &(ALENS(6,L)*(((1.0D0-ALENS(39,L))*(EDGVAL**2))**4))+&
               &(ALENS(7,L)*(((1.0D0-ALENS(40,L))*(EDGVAL**2))**5))
            ELSE
            END IF
            IF(ALENS(23,L).EQ.2.0D0) THEN
!       X-TORIC
               ARG=(1.0D0-((ALENS(41,L)+1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(24,L)))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,L)*(((1.0D0+ALENS(37,L))*(EDGVAL**2))**2))+&
               &(ALENS(5,L)*(((1.0D0+ALENS(38,L))*(EDGVAL**2))**3))+&
               &(ALENS(6,L)*(((1.0D0+ALENS(39,L))*(EDGVAL**2))**4))+&
               &(ALENS(7,L)*(((1.0D0+ALENS(40,L))*(EDGVAL**2))**5))
            ELSE
            END IF
         END IF
!       CHECK FOR ANAMORPHICS
         IF(ALENS(36,(L+1)).EQ.0.0D0) THEN
!       IS THE SURFACE L+1 X-TORIC BUT NOT ANAMORPHIC ASPHERE ?
            IF(ALENS(23,(L+1)).EQ.2.0D0) THEN
!       USE TORIC DATA IN SAG CALC
               ARG=(1.0D0-((1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGLP1=(((EDGVAL**2)*(ALENS(24,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))
            ELSE
!       NOT X-TORIC
               ARG=(1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGLP1=(((EDGVAL**2)*(ALENS(1,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L+1))*(EDGVAL**4))+&
               &(ALENS(5,(L+1))*(EDGVAL**6))+&
               &(ALENS(6,(L+1))*(EDGVAL**8))+&
               &(ALENS(7,(L+1))*(EDGVAL**10))+&
               &(ALENS(81,(L+1))*(EDGVAL**12))+&
               &(ALENS(82,(L+1))*(EDGVAL**14))+&
               &(ALENS(83,(L+1))*(EDGVAL**16))+&
               &(ALENS(84,(L+1))*(EDGVAL**18))+&
               &(ALENS(85,(L+1))*(EDGVAL**20))
            END IF
         ELSE
!       ANAMORPHIC ASPHERIC
            IF(ALENS(23,(L+1)).EQ.1.0D0) THEN
!       Y-TORIC
               ARG=(1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(1,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L+1))*(((1.0D0-ALENS(37,(L+1)))*(EDGVAL**2))**2))+&
               &(ALENS(5,(L+1))*(((1.0D0-ALENS(38,(L+1)))*(EDGVAL**2))**3))+&
               &(ALENS(6,(L+1))*(((1.0D0-ALENS(39,(L+1)))*(EDGVAL**2))**4))+&
               &(ALENS(7,(L+1))*(((1.0D0-ALENS(40,(L+1)))*(EDGVAL**2))**5))
            ELSE
            END IF
            IF(ALENS(23,L).EQ.2.0D0) THEN
!       X-TORIC
               ARG=(1.0D0-((ALENS(41,L+1)+1.0D0)*(ALENS(24,L+1)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(24,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L+1))*(((1.0D0+ALENS(37,(L+1)))*(EDGVAL**2))**2))+&
               &(ALENS(5,(L+1))*(((1.0D0+ALENS(38,(L+1)))*(EDGVAL**2))**3))+&
               &(ALENS(6,(L+1))*(((1.0D0+ALENS(39,(L+1)))*(EDGVAL**2))**4))+&
               &(ALENS(7,(L+1))*(((1.0D0+ALENS(40,(L+1)))*(EDGVAL**2))**5))
            ELSE
            END IF
         END IF
!       NOW THE SAGS OF SURFACES L AND L+1 HAVE BEEN
!       CALCULATED. NOW CALCULATE THE NEW ADJUSTED
!       THICKNESS, TH(L).
!       RULES:
!               IF SAGL-SAGLP1>0 THEN
!               TH(L)=(CAY TARGET VALUE)+(SAGL-SAGLP1)
!               IF SAGL-SAGLP1)<OR= 0 THEN
!               TH(L)=(CAY TARGET VALUE)
         IF((SAGL-SAGLP1).GT.0.0D0)&
         &ALENS(3,(L))=SOLVE(7,(L))+(SAGL-SAGLP1)
         IF((SAGL-SAGLP1).LE.0.0D0)&
         &ALENS(3,(L))=SOLVE(7,(L))
!       HANDLING OF CAY SOLVE IS COMPLETED
      ELSE
!       NO CAY SOLVE IS PRESENT, PROCEED
      END IF

      IF(SOLVE(6,L).EQ.7.0D0) THEN

         !     DO I=1,8
         !         PRINT *, "I IS ", I
         !         PRINT *, "PXTRA ", PXTRAY(I,0:INT(SYSTEM(20)))

         !     END DO

         newThick =&
         &curr_par_ray_trace%getObjectThicknessToSetParaxialMag(&
         &-1*SOLVE(7,L), curr_lens_data)

         ALENS(3,0) = newThick

      END IF


!       FINISHED WITH POSSIBLE CAY SOLVES ON TH(L)
!
!               ALL SURFACE (L) SOLVES NOW HANDLED.
!               NOW RETURN TO THE CALLING SUBROUTINE
      IF(SOLVE(6,L).NE.0.0D0.OR.SOLVE(8,L).NE.0.0D0) THEN
!     THERE WERE YZ=PLANE SOLVES, CALC VALUES
         FINY=L
         CALL FINIYZ
      ELSE
      END IF
      RETURN
   ELSE
!       SLV2 NOT 1
   END IF
   IF(SLV2.EQ.2) THEN
!
!       THE SURFACE OF SOLVE EVALUATION IS THE PASSED PARAMETER L.
!       NOW HANDLE ALL SOLVES ON SURFACE L

      ! JN:  First check if this is the object surface, since
      ! these refractive index simplifications break for L=0
      ! Check for mag solve


      IF(L.GT.0) THEN

!       SIMPLIFY THE REPRESENTATION OF THE REFRACTIVE INDICES
!       AT THE CONTROL WAVELENGTH AT L-1 AND L
         IF(INT(SYSTEM(11)).GE.1.AND.INT(SYSTEM(11)).LE.5) THEN
            N=ALENS(45+INT(SYSTEM(11)),(L-1))
            J_NP=ALENS(45+INT(SYSTEM(11)),(L))
         END IF
         IF(INT(SYSTEM(11)).GE.6.AND.INT(SYSTEM(11)).LE.10) THEN
            N=ALENS(65+INT(SYSTEM(11)),(L-1))
            J_NP=ALENS(65+INT(SYSTEM(11)),(L))
         END IF

      END IF



!
!                       FIRST,CURVATURE SOLVES
!***************************************************************************
!                       PUX SOLVES
!***************************************************************************
!       IS THERE A PUX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.10.0D0) THEN
!       YES THERE IS A PUX SOLVE ON SURFACE L, HANDLE IT
!       FOR A PUX SOLVE
!       CV(L)=(-SOLVE TARGET+(N/N')*PUX(L-1))*(N'/(N'-N))*(1/PX(L))
!       AND PUX(L)=SOLVE TARGET
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC
            PXTRAX(2,L)=SOLVE(1,L)
            IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=((-PXTRAX(2,L))+((N/J_NP)*PXTRAX(2,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAX(1,L))
            END IF
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            PXTRAX(2,L)=SOLVE(1,L)
            IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=((-PXTRAX(2,L))+((N/J_NP)*PXTRAX(2,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAX(1,L))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PUX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PUX SOLVE
      END IF
!*****************************************************************************
!                       PUCX SOLVES
!***************************************************************************
!       IS THERE A PUCX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.13.0D0) THEN
!       YES THERE IS A PUCX SOLVE ON SURFACE L, HANDLE IT
!       FOR A PUCX SOLVE
!       CV(L)=(-SOLVE TARGET+(N/N')*PUCX(L-1))*(N'/(N'-N))*(1/PCX(L))
!       AND PUCX(L)=SOLVE TARGET
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC
            PXTRAX(6,L)=SOLVE(1,L)
            IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=((-PXTRAX(6,L))+((N/J_NP)*PXTRAX(6,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAX(5,L))
            END IF
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            PXTRAX(6,L)=SOLVE(1,L)
            IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=((-PXTRAX(6,L))+((N/J_NP)*PXTRAX(6,(L-1))))*&
               &(J_NP/(J_NP-N))*(1.0D0/PXTRAX(5,L))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PUCX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PUCX SOLVE
      END IF
!***************************************************************************
!                       PIX SOLVES
!***************************************************************************
!       IS THERE A PIX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.9.0D0) THEN
!       YES THERE IS A PIX SOLVE ON SURFACE L, HANDLE IT
!       FOR A PIX SOLVE
!       CV(L)=(SOLVE TARGET-PUX(L-1))/PX(L)
!       AND PIX(L)=SOLVE TARGET
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC
            PXTRAX(3,L)=SOLVE(1,L)
            IF(PXTRAX(1,L).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=(PXTRAX(3,L)-PXTRAX(2,(L-1)))/&
               &(PXTRAX(1,(L)))
            END IF
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            PXTRAX(3,L)=SOLVE(1,L)
            IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP-N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=(PXTRAX(3,L)-PXTRAX(2,(L-1)))/&
               &(PXTRAX(1,(L)))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PIX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PIX SOLVE
      END IF
!*****************************************************************************
!                       PICX SOLVES
!***************************************************************************
!       IS THERE A PICX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.12.0D0) THEN
!       YES THERE IS A PICX SOLVE ON SURFACE L, HANEEL IT
!       FOR A PICX SOLVE
!       CV(L)=(SOLVE TARGET-PUCX(L-1))/PCX(L)
!       AND PICX(L)=SOLVE TARGET
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC
            PXTRAX(7,L)=SOLVE(1,L)
            IF(PXTRAX(5,L).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=(PXTRAX(7,L)-PXTRAX(6,(L-1)))/&
               &(PXTRAX(5,(L)))
            END IF
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            PXTRAX(7,L)=SOLVE(1,L)
            IF(PXTRAX(5,L).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=(PXTRAX(7,L)-PXTRAX(6,(L-1)))/&
               &(PXTRAX(5,(L-1)))
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
         END IF
!       PICX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PICX SOLVE
      END IF
!*****************************************************************************
!                       APX SOLVES
!***************************************************************************
!       IS THERE A APX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.8.0D0) THEN
!       YES THERE IS A APX SOLVE ON SURFACE L, HANDLE IT
!       FOR AN APX SOLVE
!       CV(L)=-(PUX(L-1)/PX(L))*((N'+N)/N)
!       THEN
!       PUX(L)=(N/N')*PUX(L-1)-CV(L)*PX(L)*((N'-N)/N')
!       PIX(L)=CV(L)*PX(L)+PUX(L-1)
!       PIX'(L)=(N/N')*PIX(L)
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC, PROCEED
            IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=-(PXTRAX(2,(L-1))/PXTRAX(1,L))*((J_NP+N)/N)
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAX(2,L)=((N/NP)*PXTRAX(2,(L-1)))-(ALENS(24,L)*&
            &PXTRAX(1,L)*((J_NP-N)/J_NP))
            PXTRAX(3,L)=(ALENS(24,L)*PXTRAX(1,L))+PXTRAX(2,(L-1))
            PXTRAX(4,L)=(N/J_NP)*PXTRAX(3,L)
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            IF(PXTRAX(1,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=-(PXTRAX(2,(L-1))/PXTRAX(1,L))*((J_NP+N)/N)
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAX(2,L)=((N/J_NP)*PXTRAX(2,(L-1)))-(ALENS(1,L)*&
            &PXTRAX(1,L)*((J_NP-N)/J_NP))
            PXTRAX(3,L)=(ALENS(1,L)*PXTRAX(1,L))+PXTRAX(2,(L-1))
            PXTRAX(4,L)=(N/J_NP)*PXTRAX(3,L)
         END IF
!       APX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO APX SOLVE
      END IF
!***************************************************************************
!                       APCX SOLVES
!***************************************************************************
!       IS THERE A APCX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.11.0D0) THEN
!       YES THERE IS A APCX SOLVE ON SURFACE L, HANDLE IT
!       FOR AN APCX SOLVE
!       CV(L)=-(PUCX(L-1)/PCX(L))*((N'+N)/N)
!       THEN
!       PUCX(L)=(N/N')*PUCX(L-1)-CV(L)*PCX(L)*((N'-N)/N')
!       PICX(L)=CV(L)*PCX(L)+PUCX(L-1)
!       PICX'(L)=(N/N')*PICX(L)
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC, PROCEED
            IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(24,L)=0.0D0
            ELSE
               ALENS(24,L)=-(PXTRAX(6,(L-1))/PXTRAX(5,L))*((J_NP+N)/N)
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(24,L)*&
            &PXTRAX(5,L)*((J_NP-N)/J_NP))
            PXTRAX(7,L)=(ALENS(24,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
            PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            IF(PXTRAX(5,L).EQ.0.0D0.OR.(J_NP+N).EQ.0.0D0) THEN
               ALENS(1,L)=0.0D0
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
               ALENS(1,L)=-(PXTRAX(6,(L-1))/PXTRAX(5,L))*((J_NP+N)/N)
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(1,L)*&
            &PXTRAX(5,L)*((J_NP-N)/J_NP))
            PXTRAX(7,L)=(ALENS(1,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
            PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
         END IF
!       APCX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO APCX SOLVE
      END IF
!**************************************************************************
!                       COCX SOLVES
!***************************************************************************
!       IS THERE A COCX SOLVE ON SURFACE L
      IF(SOLVE(2,L).EQ.14.0D0) THEN
         DIS=0.0D0
!       YES THERE IS A COCX SOLVE ON SURFACE L, HANDLE IT
!       FOR AN COCX SOLVE
!       CV(L)=SIGNED DISTANCE TO THE SPECIFIED SURFACE(TAR)
!       FROM SURFACE L EQUAL TO DIS
         TAR=INT(SOLVE(1,L))
         IF(TAR.LT.L) THEN
            DO I=TAR,(L-1)
               DIS=DIS+ALENS(3,I)
            END DO
         END IF
         IF(TAR.GT.L) THEN
            DO I=L,(TAR-1)
               DIS=DIS+ALENS(3,I)
            END DO
         END IF
!       CASE OF TAR=L ELIMINATED INSIDE OD SUBROUTINE LNSEOS.
!       THEN RECALCULATE
!       PUCX(L)=(N/N')*PUCX(L-1)-CV(L)*PCX(L)*((N'-N)/N')
!       PICX(L)=CV(L)*PCX(L)+PUCX(L-1)
!       PICX'(L)=(N/N')*PICX(L)
!       IS SURFACE L AN Y-TORIC,IF SO THE TORIC CURVATURE AND
!       NOT THE MAIN CURVATURE IS CHANGED.
         IF(ALENS(23,L).EQ.1.0D0) THEN
!       SURFACE IS Y-TORIC, PROCEED
            IF(DIS.NE.0.0D0) THEN
               IF(TAR.LT.L) ALENS(24,L)=-1.0D0/DIS
               IF(TAR.GT.L) ALENS(24,L)= 1.0D0/DIS
            ELSE
!       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
               OUTLYNE='(COCX) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
               CALL SHOWIT(1)
               OUTLYNE='(COCX) SOLVE REMOVED'
               CALL SHOWIT(1)
               SOLVE(2,L)=0.0D0
               SOLVE(1,0)=0.0D0
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(24,L)*&
            &PXTRAX(5,L)*((J_NP-N)/J_NP))
            PXTRAX(7,L)=(ALENS(24,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
            PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
         ELSE
!       SURFACE NOT Y-TORIC,PROCEED
            IF(DIS.NE.0.0D0) THEN
               IF(TAR.LT.L) ALENS(1,L)=-1.0D0/DIS
               IF(TAR.GT.L) ALENS(1,L)=1.0D0/DIS
!
               IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(2,L).NE.0.0D0) THEN
                  ALENS(2,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE CONIC CONSTANT RESET TO 0.0 FOR THIS PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
               IF(ALENS(1,L).NE.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                  ALENS(43,L)=0.0D0
                  OUTLYNE='WARNING:'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*) 'FOR SURFACE ',L
                  CALL SHOWIT(1)
                  OUTLYNE=&
                  &'THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
                  CALL SHOWIT(1)
               END IF
!
            ELSE
!       DIS IS ZERO, CONCENTRIC SOLVE IGNORED
               OUTLYNE='(COCX) SOLVE RESULTS IN A ZERO RADIUS OF CURVATURE'
               CALL SHOWIT(1)
               OUTLYNE='(COCX) SOLVE REMOVED'
               CALL SHOWIT(1)
               SOLVE(2,L)=0.0D0
               SOLVE(1,0)=0.0D0
            END IF
!       RECALCULATE ANGLES OF INCIDENCE,REFLECTION/REFRACTION
!       AND EXITING SLOPE ANGLE
            PXTRAX(6,L)=((N/J_NP)*PXTRAX(6,(L-1)))-(ALENS(1,L)*&
            &PXTRAX(5,L)*((J_NP-N)/J_NP))
            PXTRAX(7,L)=(ALENS(1,L)*PXTRAX(5,L))+PXTRAX(6,(L-1))
            PXTRAX(8,L)=(N/J_NP)*PXTRAX(7,L)
         END IF
!       COCX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO COCX SOLVE
      END IF
!**************************************************************************
!               NOW THICKNESS SOLVES
!**************************************************************************
!                       PX SOLVES
!***************************************************************************
!       IS THERE A PX SOLVE ON SURFACE L
      IF(SOLVE(4,L).EQ.4.0D0) THEN
!       YES THERE IS A PX SOLVE ON SURFACE L
!       WHICH AFFECTS THE PX VALUE ON (L+1), HANDLE IT
!       TH(L)=(SOLVE TARGET-PX(L))/PUX(L)
!
         IF(PXTRAX(2,L).EQ.0.0D0) THEN
            ALENS(3,L)=1D20
         ELSE
            ALENS(3,L)=(SOLVE(3,L)-PXTRAX(1,(L)))/(PXTRAX(2,L))
            IF(DABS(ALENS(3,L)).GT.1D20) THEN
               IF(ALENS(3,L).GT.0.0D0)THEN
                  ALENS(3,L)=1D20
               ELSE
               END IF
               IF(ALENS(3,L).LT.0.0D0) THEN
                  ALENS(3,L)=-1D20
               ELSE
               END IF
            ELSE
            END IF
         END IF
!       PX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PX SOLVE
      END IF
!**************************************************************************
!                       PCX SOLVES
!***************************************************************************
!       IS THERE A PCX SOLVE ON SURFACE L
      IF(SOLVE(4,L).EQ.5.0D0) THEN
!       YES THERE IS A PCX SOLVE ON SURFACE L
!       WHICH AFFECTS THE PCX VALUE ON (L+1), HANDLE IT
!       TH(L)=(SOLVE TARGET-PCX(L))/PUCX(L)
!
         IF(PXTRAX(6,L).EQ.0.0D0) THEN
            ALENS(3,L)=1D20
         ELSE
            ALENS(3,L)=(SOLVE(3,L)-PXTRAX(5,(L)))/(PXTRAX(6,L))
            IF(DABS(ALENS(3,L)).GT.1D20) THEN
               IF(ALENS(3,L).GT.0.0D0)THEN
                  ALENS(3,L)=1D20
               ELSE
               END IF
               IF(ALENS(3,L).LT.0.0D0) THEN
                  ALENS(3,L)=-1D20
               ELSE
               END IF
            ELSE
            END IF
         END IF
!       PCX SOLVE FOR SURFACE L HANDLED.
      ELSE
!       NO PCX SOLVE
      END IF
!***************************************************************************
!                       CAX SOLVES
!******************************************************************************
!       CHECK FOR CAX SOLVE ON (L) AND IF FOUND, HANDLE IT.
      IF(SOLVE(4,(L)).EQ.6.0D0) THEN
!       THERE IS A CAX SOLVE ON (L) WHICH AFFECTS TH(L), HANDLE IT.
!       IS THER A CIRCULAR CLEAR APERTURE ON SURFACE L
         IF(ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0.OR.&
         &ALENS(9,(L)).EQ.1.0D0.AND.ALENS(127,L).NE.0.0D0) THEN
!       YES THERE IS, SET EDGVAL TO IT
            IF(ALENS(10,L).LE.ALENS(11,L)) THEN
               EDGVAL=DABS(ALENS(10,(L)))
            ELSE
               EDGVAL=DABS(ALENS(11,(L)))
            END IF
         ELSE
!       NO CIRCULAR CLAP, USE PX(L)+PCX(L) FOR EDGVAL
            EDGVAL=DABS(PXTRAX(1,(L)))+DABS(PXTRAX(5,(L)))
         END IF
!                       NOW EDGEVAL IS SET
!       NOW CALCULATE THE SAG OF SURFACES L AND L+1
!       CONSIDER THE XZ-PLANE ONLY AND IGNORE TILTS,
!       DECENTERS AND SPECIAL SURFACE DATA. CONSIDER
!       TORICS IF PRESENT. CONSIDER CONIC AND ASPHERIC
!       TERMS IF PRESENT.
!       CHECK FOR ANAMORPHICS
         IF(ALENS(36,L).EQ.0.0D0) THEN
!       IS THE SURFACE L Y-TORIC BUT NOT ANAMORPHIC ASPHERIC ?
            IF(ALENS(23,(L)).EQ.1D0) THEN
!       USE TORIC DATA IN SAG CALC
               ARG=(1.0D0-(1.0D0*(ALENS(1,L)**2)*(EDGVAL**2)))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(24,(L))))/&
               &(1.0D0+DSQRT(ARG)))
            ELSE
!       NOT Y-TORIC
               ARG=1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(1,(L))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L))*(EDGVAL**4))+&
               &(ALENS(5,(L))*(EDGVAL**6))+&
               &(ALENS(6,(L))*(EDGVAL**8))+&
               &(ALENS(7,(L))*(EDGVAL**10))+&
               &(ALENS(81,(L))*(EDGVAL**12))+&
               &(ALENS(82,(L))*(EDGVAL**14))+&
               &(ALENS(83,(L))*(EDGVAL**16))+&
               &(ALENS(84,(L))*(EDGVAL**18))+&
               &(ALENS(85,(L))*(EDGVAL**20))
            END IF
         ELSE
!       ANAMORPHIC ASPHERIC
            IF(ALENS(23,L).EQ.1.0D0) THEN
!       Y-TORIC
               ARG=1.0D0-((ALENS(41,L)+1.0D0)*(ALENS(24,L)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(24,L)))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,L)*(((1.0D0+ALENS(37,L))*(EDGVAL**2))**2))+&
               &(ALENS(5,L)*(((1.0D0+ALENS(38,L))*(EDGVAL**2))**3))+&
               &(ALENS(6,L)*(((1.0D0+ALENS(39,L))*(EDGVAL**2))**4))+&
               &(ALENS(7,L)*(((1.0D0+ALENS(40,L))*(EDGVAL**2))**5))
            ELSE
            END IF
            IF(ALENS(23,L).EQ.2.0D0) THEN
!       X-TORIC
               ARG=1.0D0-((ALENS(2,L)+1.0D0)*(ALENS(1,L)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(1,L)))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,L)*(((1.0D0-ALENS(37,L))*(EDGVAL**2))**2))+&
               &(ALENS(5,L)*(((1.0D0-ALENS(38,L))*(EDGVAL**2))**3))+&
               &(ALENS(6,L)*(((1.0D0-ALENS(39,L))*(EDGVAL**2))**4))+&
               &(ALENS(7,L)*(((1.0D0-ALENS(40,L))*(EDGVAL**2))**5))
            ELSE
            END IF
         END IF
!       CHECK FOR ANAMORPHICS
         IF(ALENS(36,(L+1)).EQ.0.0D0) THEN
!       IS THE SURFACE L+1 Y-TORIC BUT NOT ANAMORPHIC ASPHERIC ?
            IF(ALENS(23,(L+1)).EQ.1.0D0) THEN
!       USE TORIC DATA IN SAG CALC
               ARG=1.0D0-((1.0D0)*(ALENS(2,L)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGLP1=(((EDGVAL**2)*(ALENS(24,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))
            ELSE
!       NOT Y-TORIC
               ARG=1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGLP1=(((EDGVAL**2)*(ALENS(1,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L+1))*(EDGVAL**4))+&
               &(ALENS(5,(L+1))*(EDGVAL**6))+&
               &(ALENS(6,(L+1))*(EDGVAL**8))+&
               &(ALENS(7,(L+1))*(EDGVAL**10))+&
               &(ALENS(81,(L+1))*(EDGVAL**12))+&
               &(ALENS(82,(L+1))*(EDGVAL**14))+&
               &(ALENS(83,(L+1))*(EDGVAL**16))+&
               &(ALENS(84,(L+1))*(EDGVAL**18))+&
               &(ALENS(85,(L+1))*(EDGVAL**20))
            END IF
         ELSE
!       ANAMORPHIC ASPHERIC
            IF(ALENS(23,(L+1)).EQ.1.0D0) THEN
!       Y-TORIC
               ARG=1.0D0-((ALENS(41,L+1)+1.0D0)*(ALENS(24,L+1)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(24,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L+1))*(((1.0D0+ALENS(37,(L+1)))*(EDGVAL**2))**2))+&
               &(ALENS(5,(L+1))*(((1.0D0+ALENS(38,(L+1)))*(EDGVAL**2))**3))+&
               &(ALENS(6,(L+1))*(((1.0D0+ALENS(39,(L+1)))*(EDGVAL**2))**4))+&
               &(ALENS(7,(L+1))*(((1.0D0+ALENS(40,(L+1)))*(EDGVAL**2))**5))
            ELSE
            END IF
            IF(ALENS(23,L).EQ.2.0D0) THEN
!       X-TORIC
               ARG=1.0D0-((ALENS(2,L+1)+1.0D0)*(ALENS(1,L+1)**2)*(EDGVAL**2))
               IF(ARG.LT.0.0D0) ARG=0.0D0
               SAGL=(((EDGVAL**2)*(ALENS(1,(L+1))))/&
               &(1.0D0+DSQRT(ARG)))+&
               &(ALENS(4,(L+1))*(((1.0D0-ALENS(37,(L+1)))*(EDGVAL**2))**2))+&
               &(ALENS(5,(L+1))*(((1.0D0-ALENS(38,(L+1)))*(EDGVAL**2))**3))+&
               &(ALENS(6,(L+1))*(((1.0D0-ALENS(39,(L+1)))*(EDGVAL**2))**4))+&
               &(ALENS(7,(L+1))*(((1.0D0-ALENS(40,(L+1)))*(EDGVAL**2))**5))
            ELSE
            END IF
         END IF
!       NOW THE SAGS OF SURFACES L AND L+1 HAVE BEEN
!       CALCULATED. NOW CALCULATE THE NEW ADJUSTED
!       THICKNESS, TH(L).
!       RULES:
!               IF SAGL-SAGLP1>0 THEN
!               TH(L)=(CAX TARGET VALUE)+(SAGL-SAGLP1)
!               IF SAGL-SAGLP1)<OR= 0 THEN
!               TH(L)=(CAX TARGET VALUE)
         IF((SAGL-SAGLP1).GT.0.0D0)&
         &ALENS(3,(L))=SOLVE(3,(L))+(SAGL-SAGLP1)
         IF((SAGL-SAGLP1).LE.0.0D0)&
         &ALENS(3,(L))=SOLVE(3,(L))
!       HANDLEING OF CAX SOLVE IS COMPLETED
      ELSE
!       NO CAX SOLVE IS PRESENT, PROCEED
      END IF
!       FINISHED WITH POSSIBLE CAX SOLVES ON TH(L)
      IF(SOLVE(4,L).NE.0.0D0.OR.SOLVE(2,L).NE.0.0D0) THEN
!     THERE WERE XZ=PLANE SOLVES, CALC VALUES
         FINY=L
         CALL FINIXZ
      ELSE
      END IF
!               ALL SURFACE (L) SOLVES NOW HANDLED.
!               NOW RETURN TO THE CALLING SUBROUTINE
      RETURN
   ELSE
!       SLV2 NOT 2
   END IF
   RETURN
END
! SUB SLI.FOR
SUBROUTINE SLI
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SLI WHICH IMPLEMENTS THE LI AND LIC
!       COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE LI COMMAND AT
!       THE CMD LEVEL
!
   CHARACTER DTY*10,TMY*8,NNTT1*80
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!
   PRINT *, "SLI SUBROUTINE"
   IF(F1.EQ.1) THEN
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1)THEN
         OUTLYNE=&
         &'"LI" AND "LIC" TAKE NO EXPLICIT INPUT AT THE CMD LEVEL"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SYSTEM(20).EQ.0.0D0) THEN
         OUTLYNE='LENS IDENTIFIER DOES NOT EXIST'
         CALL SHOWIT(1)
         OUTLYNE='LENS SYSTEM HAS NO SURFACES'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF

      IF(WC.EQ.'LI') THEN
         IF(LI.EQ.CNULL) WRITE(OUTLYNE,1001)
         IF(LI.EQ.CNULL) CALL SHOWIT(0)
         IF(STMPT) CALL MYTIME(TMY)
         IF(STMPD) CALL MYDATE(DTY)
         IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
         IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
         IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
         IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
         IF(LI.NE.CNULL) WRITE(OUTLYNE,1000) NNTT1
         IF(LI.NE.CNULL) CALL SHOWIT(0)
         IF(LIC(1).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(1)
            CALL SHOWIT(0)
         END IF
         IF(LIC(2).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(2)
            CALL SHOWIT(0)
         END IF
         IF(LIC(2).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(3)
            CALL SHOWIT(0)
         END IF
         IF(LIC(4).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(4)
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
!     NOT LI
      END IF
      IF(WC.EQ.'LIC') THEN
         IF(LIC(1).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(1)
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1002)
            CALL SHOWIT(0)
         END IF
         IF(LIC(2).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(2)
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(LIC(3).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(3)
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(LIC(4).NE.CNULL) THEN
            WRITE(OUTLYNE,1000) LIC(4)
            CALL SHOWIT(0)
         ELSE
         END IF
         RETURN
      ELSE
!     NOT LIC
      END IF
   ELSE
!               NOT AT CMD LEVEL
!
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
         IF(WC.EQ.'LIC'.OR.WC.EQ.'LI') THEN
            IF(SST.EQ.0) THEN
               OUTLYNE='"LI" AND "LIC" REQUIRE STRING INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
         END IF
         IF(WC.EQ.'LIC') THEN
!       DO AN LIC(LICCNT) ASSIGNMENT
!
!
            LIC(LICCNT)=WS
!       ADVANCE THE LICCNT COUNTER IF LICCNT LESS THAN 4
!
            IF(LICCNT.LT.4) THEN
               LICCNT=LICCNT+1
            ELSE
               LICCNT=LICCNT
            END IF
         END IF
      END IF
!
      IF(WC.EQ.'LI') THEN
         LI=WS
         LIC(1)=CNULL
         LIC(2)=CNULL
         LIC(3)=CNULL
         LIC(4)=CNULL
         LICCNT=1
      ELSE
!       NOT LI
      END IF
   END IF
!
1000 FORMAT(A79)
1001 FORMAT('THE CURRENT LI IS BLANK')
1002 FORMAT('THE CURRENT LIC IS BLANK')
   RETURN
END
! SUB SINI.FOR
SUBROUTINE SINI
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SINI WHICH IMPLEMENTS THE INI
!       COMMAND
!
   CHARACTER NNTT1*80
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!
   IF(F1.EQ.1) THEN
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1)THEN
         OUTLYNE=&
         &'"INI" TAKES NO EXPLICIT INPUT AT THE CMD LEVEL"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SYSTEM(20).EQ.0.0D0) THEN
         OUTLYNE='DESIGNER INITIALS DO NOT EXIST'
         CALL SHOWIT(1)
         OUTLYNE='LENS SYSTEM HAS NO SURFACES'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INNI.EQ.CNULL) WRITE(OUTLYNE,1001)
      IF(INNI.EQ.CNULL) CALL SHOWIT(0)
      NNTT1(1:80)=INNI(1:70)
      IF(INNI.NE.CNULL) WRITE(OUTLYNE,1000) NNTT1(1:70)
      IF(INNI.NE.CNULL) CALL SHOWIT(0)
      RETURN
   ELSE
!               NOT AT CMD LEVEL
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      IF(SST.EQ.0) THEN
         OUTLYNE='"INI" REQUIRES STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      INNI(1:80)=WS(1:70)
   END IF
!
1000 FORMAT('INI = ',A70)
1001 FORMAT('THE CURRENT INI IS BLANK')
   RETURN
END
! SUB SLTYPE.FOR
SUBROUTINE SLTYPE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SLTYPE WHICH IMPLEMENTS THE LTYPE
!       COMMAND
!
   CHARACTER NNTT1*80
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!
   IF(F1.EQ.1) THEN
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1)THEN
         OUTLYNE=&
         &'"LTYPE" TAKES NO EXPLICIT INPUT AT THE CMD LEVEL"'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(SYSTEM(20).EQ.0.0D0) THEN
         OUTLYNE='LENS TYPE IDENTIFIER DOES NOT EXIST'
         CALL SHOWIT(1)
         OUTLYNE='LENS SYSTEM HAS NO SURFACES'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(LLTYPE(1:5).EQ.CNULL(1:5)) WRITE(OUTLYNE,1001)
      IF(LLTYPE(1:5).EQ.CNULL(1:5)) CALL SHOWIT(0)
      NNTT1(1:80)=LLTYPE(1:5)
      IF(LLTYPE(1:5).NE.CNULL(1:5)) WRITE(OUTLYNE,1000) NNTT1(1:5)
      IF(LLTYPE.NE.CNULL) CALL SHOWIT(0)
      RETURN
   ELSE
!               NOT AT CMD LEVEL
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      IF(SST.EQ.0) THEN
         OUTLYNE='"LTYPE" REQUIRES STRING INPUT'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      LLTYPE(1:80)=WS(1:5)
   END IF
!
1000 FORMAT('LTYPE = ',A5)
1001 FORMAT('THE CURRENT LTYPE IS BLANK')
   RETURN
END
