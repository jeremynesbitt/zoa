! Real ray trace refactoring
!       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
!       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
!     ZERO IF I-1 IS PERFECT
!       RAYRAY(9,SURF)=COSINE(I)
!       RAYRAY(10,SURF)=COSINE(IP)
!       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
!       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
!       RAYRAY(13,SURF)=LN - SURFACE NORMAL DIRECTION COSINE
!       RAYRAY(14,SURF)=MN - SURFACE NORMAL DIRECTION COSINE
!       RAYRAY(15,SURF)=NN - SURFACE NORMAL DIRECTION COSINE
!       RAYRAY(16,SURF)=XOLD  The next 6 items are the ray values
!       RAYRAY(17,SURF)=YOLD  just before interaction with the surface
!       RAYRAY(18,SURF)=ZOLD  in the surface local coordinate system.
!       RAYRAY(19,SURF)=OLDL
!       RAYRAY(20,SURF)=OLDM
!       RAYRAY(21,SURF)=OLDN
!     THESE LAST 6 ITEMS ARE THE COORDINATES OF THE RAY AT
!     SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
!       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
!     OR TO I-1 IF I-1 IS "PERFECT'
!       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
!       RAYRAY(24,SURF)=1.0D0 FOR POSRAY,-1 FOR NEG RAY
!       RAYRAY(25,SURF)=RAY ENERGY TERM
!       RAYRAY(26,SURF)=RAY XL DIR COS
!       RAYRAY(27,SURF)=RAY XM DIR COS
!       RAYRAY(28,SURF)=RAY XN DIR COS
!       RAYRAY(29,SURF)=RAY YL DIR COS
!       RAYRAY(30,SURF)=RAY YM DIR COS
!       RAYRAY(31,SURF)=RAY YN DIR COS
!       RAYRAY(34,SURF)=FACT_PAR
!       RAYRAY(35,SURF)=FACT_PER
!       RAYRAY(36,SURF)=PHASE_PAR
!       RAYRAY(37,SURF)=PHASE_PER
!       RAYRAY(38,SURF)=POLARIZATION ANGLE IN DEGREES BETWEEN Y-RAY VECTOR AND PARALLEL PLANE
!       RAYRAY(39,SURF)=Y-COMPONENT OF THE ANGLE OF INCIDENCE
!       RAYRAY(40,SURF)=X-COMPONENT OF THE ANGLE OF INCIDENCE
!       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED


module real_ray_trace

contains
    ! I started this to refactor the real ray tracing.  There are three subs that 
    ! that do real ray tracing.  RAYTRA and RAYTRA2 in RAYTRA5.FOR and
    ! REFRAY in RAYTRA12.FOR.
    ! I tried to refactor the RAYTRA sub as a start but somehow broke it when doing
    ! minor things so gave up.  Need to come up with a very thorough test process to
    ! make sure I don't break anything before I try again.
    SUBROUTINE RAYTRA_NEW
        USE GLOBALS
!
        IMPLICIT NONE
!
!       THIS IS SUBROUTINE RAYTRA.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE TRACING OF A RAY. THIS IS INITIATED
!       BY THE "RAY" COMMAND.
!
        INTEGER IPASS1,JK,I,ISYS20,KKK,J,ISURF,IK,WA3

        INTEGER CAERAS,COERAS,N_HITS
!
        REAL*8 OPLXCOR(1:2),OPLYCOR(1:2),OPLZCOR(1:2)
!
        COMMON/COROPL/OPLXCOR,OPLYCOR,OPLZCOR
!
        REAL*8 X,Y,Z,L,M,N,WW1W,WW2W,LER,MER,NER,TANN1,TANN2,IA,IAP &
      ,D21,D22,GAMMA,XXX,YYY,TWW1,TWW2,XL,XM,XN,YL,YM,YN,RN1,RN2, &
      WWW1,WWW2,D11,D12,LS,SNINDX,SNIND2,WW1WW,WW2WW,JK1,JK2,JK3, &
      LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,ENERGY_FACTOR, &
      Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY, &
      XC1,YC1,ZC1,MF1,MF2,DET,TARX,TARY,XVALUE,YVALUE, &
      TEST,MAG,VALUE,LOLD,MOLD,NOLD,TARRY,TARRX,JKX,JKY, &
      JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2,JK_CPL,JK_CPM,JK_CPN &
      ,SA_CPL,SA_CPM,SA_CPN,DP,POLANG,FACT_PAR,FACT_PER,PHASE_PAR &
      ,PHASE_PER,PATHL,STEPL,STEPL1
!
        COMMON/CACO/CAERAS,COERAS,LS
!
!     VARIABLES FOR SPOT TRACING
      LOGICAL TCLPRF,SPDTRA,MMSG
        LOGICAL AIMOK,CLAPT,OLDPASS,GERROR,DELFAIL
        COMMON/PASSOLD/OLDPASS
!
      INTEGER SPDCD1,SPDCD2
!
      COMMON/SPRA1/SPDTRA
      COMMON/SPRA2/SPDCD1,SPDCD2
!
      REAL*8 XPASS,YPASS,ZPASS
!
      COMMON/SAGPAS/XPASS,YPASS,ZPASS

      REAL*8 AOI,D,H,S,FACTOR
!
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'

!
                        KKK=0
!                       DDELX=0.001D0*SYSTEM(12)
!                       DDELY=0.001D0*SYSTEM(13)
                        DDELX=0.001D0
                        DDELY=0.001D0
!     PROPER INITIALIZE PROCEEDURE 3/3/96
                R_X=0.0D0
                R_Y=0.0D0
                R_Z=0.0D0
                XOLD=0.0D0
                YOLD=0.0D0
                ZOLD=0.0D0
                LOLD=0.0D0
                MOLD=0.0D0
                NOLD=0.0D0
!
!       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
!       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
!       RAY AIMING.
!
!
      IF(WW3.GE.1.0D0.AND.WW3.LE.5.0D0) THEN
      IF(SYSTEM(INT(WW3)).EQ.0.0D0) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
        OUTLYNE= &
      'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=12
                RAYCOD(2)=NEWOBJ
                RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        RETURN
!       PROCEED
                        END IF
!       PROCEED
                        END IF
      IF(WW3.GE.6.0D0.AND.WW3.LE.10.0D0) THEN
      IF(SYSTEM(65+INT(WW3)).EQ.0.0D0) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',NEWOBJ
      CALL SHOWIT(1)
        OUTLYNE= &
      'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=12
                RAYCOD(2)=NEWOBJ
                RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        RETURN
                      END IF
                      END IF
!
!       SET RAYCOD DEFAULTS
        RAYCOD(1)=-1
        RAYCOD(2)=-1
!
                AIMOK=.FALSE.
!
                        RELY=WW1
                        RELX=WW2
!
!       SET INITIAL VALUES FOR X1ONE, Y1ONE, X1LAST AND Y1LAST
!
        LARGE=-99999.9D0
        X1ONE=LARGE
        Y1ONE=LARGE
        X1LAST=LARGE
        Y1LAST=LARGE
!
!       SET INITIAL VALUES FOR RXONE, RYONE, RXLAST AND RYLAST
        RXONE=LARGE
        RYONE=LARGE
        RXLAST=LARGE
        RYLAST=LARGE
!
!       THE FIRST THING TO DO IS TO DETERMINE THE STARTING
!       COORDINATES OF THE RAY AT THE OBJECT SURFACE.
!       THE STARTING RAY COORDINATES AT SURFACE NEWOBJ
!       ARE JUST THE RAY COORDINATES AT THIS SURFACE
!
!
        XSTRT=REFRY(1,NEWOBJ)
        YSTRT=REFRY(2,NEWOBJ)
        ZSTRT=REFRY(3,NEWOBJ)

        ! JN:  If object distance is at infinity, starting pos
        ! should be based on aperture diameter
        ! if(objectDistanceAtinfinity) then
        !  YSTRT = rayRatio*epd/2
        ! PRINT *, "WWQ is ", WWQ
        ! PRINT *, "WW1 is ", WW1
        ! PRINT *, "WW2 is ", WW2
        ! PRINT *, "WW3 is ", WW3
        ! PRINT *, "SAY is ",  SYSTEM(12)
        ! PRINT *, "SAX is ",  SYSTEM(13)
        ! TODO fix this prototype code to better match
        ! pseudocode above
        !  IF(ABS(SYSTEM(14)).GT.1E16) THEN
        !
        !   CALL PROCESKDP("SHO ENPOSZ")
        !   PRINT *, "Entrance pupil position ", REG(9)
        !   PRINT *, "Last FOB Y is ", LFOB(1)
        !   PRINT *, "SYSTEM(14) ", SYSTEM(14)
        !   !PRINT *, "TST YSTRT ", LFOB(1)*REG(9)*TAND(SYSTEM(21))
        !   !SYSTEM(14)
        !   YSTRT = WW1*SYSTEM(12)-LFOB(1)*REG(9)*TAND(SYSTEM(21))
        !   XSTRT = WW2*SYSTEM(13)
        ! END IF
        !PRINT *, "YSTRT is ", YSTRT


!
!       THE WAVELENGTH NUMBER FOR THE RAY TRACE IS:
!       INT(WW3) OR INT(SYSTEM(11))
!
!       NOW WHAT ARE THE STARTING DIRECTION COSINES AND HOW ARE
!       THEY DETERMINED?
!
!       START BY LOOKING AT THE PY+ PCY AND PX+PCX
!       VALUES AT SURFACE NEWOBJ+1.
!       THESE VALUES CORRESPOND TO THE INTERSECTIONS OF CHIEF
!       PLUS MARGINAL RAYS AT FULL 1.0 FRACTIONAL OBJECT HEIGHTS
!       AND FULL REFERENCE APERTURE HEIGHTS.
!       FOR FOB 1.0 1.0 THE TARGET RAY HEIGHTS AT SURFACE NEWOBJ+1
!       FOR THE FULL APERTURE RAY ARE
!       ARE X=PX(NEWOBJ+1)+PCX(NEWOBJ+1)
!       AND Y=PY(NEWOBJ+1)+PCY(NEWOBJ+1).
!
!       IN GENERAL THE RAY INTERESECTION
!       POINTS FOR FIRST GUESS AIMING WILL BE:
!       THE POINT AT SURFACE NEWOBJ+1 AT WHICH THE
!       PARAXIAL CHIEF RAY PLUS THE PARAXIAL MARGINAL RAY
      IF(SYSTEM(63).EQ.0.0D0) THEN
!       TELECENTRIC AIMING IS OFF
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
      IF(ALENS(9,NEWOBJ+1).EQ.1.0D0) THEN
!     CIRCULAR AP
      IF(ALENS(10,NEWOBJ+1).LE.ALENS(11,NEWOBJ+1)) THEN
      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1))) &
      JKX=DABS(ALENS(10,NEWOBJ+1))
      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1))) &
      JKY=DABS(ALENS(10,NEWOBJ+1))
                           ELSE
      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1))) &
      JKX=DABS(ALENS(11,NEWOBJ+1))
      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1))) &
      JKY=DABS(ALENS(11,NEWOBJ+1))
                           END IF
              END IF
      IF(ALENS(9,NEWOBJ+1).EQ.5.0D0) THEN
!     CIRCULAR AP
      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1))) &
      JKX=DABS(ALENS(10,NEWOBJ+1))
      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1))) &
      JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
      IF(ALENS(9,NEWOBJ+1).EQ.6.0D0) THEN
!     CIRCULAR AP
      IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1))) &
      JKX=DABS(ALENS(14,NEWOBJ+1))
      IF(DABS(ALENS(14,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1))) &
      JKY=DABS(ALENS(14,NEWOBJ+1))
              END IF
      IF(ALENS(9,NEWOBJ+1).GT.1.0D0.AND.ALENS(9,NEWOBJ+1) &
      .LE.4.0D0) THEN
!     OTHER AP
      IF(DABS(ALENS(11,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1))) &
      JKX=DABS(ALENS(11,NEWOBJ+1))
      IF(DABS(ALENS(10,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1))) &
      JKY=DABS(ALENS(10,NEWOBJ+1))
              END IF
                        ELSE
!       TELECENTRIC AIMING IS ON
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
                        END IF
!
 989                  CONTINUE
      IF(RAYCOD(1).EQ.1.AND.KKK.GT.1) THEN
                        PRINT *, "RAYTRA FAILED LINE 1928 RAYTRA5.FOR"
                        STOPP=1
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        FAIL=.TRUE.
                        RETURN
                        END IF
      IF(.NOT.ITRACE) THEN
      IF(NULL.AND..NOT.REFEXT) THEN
!     NULL WITH FAILED CHIEF RAY
      IF(SYSTEM(62).EQ.0.0D0) THEN
!     NO RAY AIMING
      IF(SYSTEM(63).EQ.0.0D0) THEN
!     TEL OFF
!     RAY AIMING IS OFF, TELECENTRIC RAY AIMING IS OFF
        X1AIM=WW2*JKX
        X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
        Y1AIM=WW1*JKY
        Y1AIM=(Y1AIM)-ALENS(30,NEWOBJ+1)
        Z1AIM=0.0D0
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
!
                        ELSE
!     TEL ON
      IF(SYSTEM(16).NE.0.0D0) &
      X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ)))+(WW2*JKX)
      IF(SYSTEM(16).EQ.0.0D0) &
      X1AIM=(WW2*JKX)
      IF(SYSTEM(14).NE.0.0D0) &
      Y1AIM=(LFOB(1)*(PXTRAY(5,NEWOBJ)))+(WW1*JKY)
      IF(SYSTEM(14).EQ.0.0D0) &
      Y1AIM=(WW1*JKY)
        Z1AIM=0.0D0
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
                        END IF
                        ELSE
!     RAY AIMING
        X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ+1)))+(WW2*JKX)
        X1AIM=(X1AIM)-ALENS(31,NEWOBJ+1)
        Y1AIM=(LFOB(1)*DABS(PXTRAY(5,NEWOBJ+1)))+(WW1*JKY)
        Y1AIM=(Y1AIM)-ALENS(30,NEWOBJ+1)
        Z1AIM=0.0D0
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(XC.LT.0.0D0) DDELX=-DDELX
      IF(YC.LT.0.0D0) DDELY=-DDELY
                        END IF
                        END IF
!     CHIEF RAY EXISTS
      IF(SYSTEM(62).EQ.0.0D0) THEN
!     NO RAY AIMING
      IF(SYSTEM(63).EQ.0.0D0) THEN
!     TEL OFF
        X1AIM=((REFRY(1,(NEWOBJ+1)))+(WW2*JKX))
        Y1AIM=((REFRY(2,(NEWOBJ+1)))+(WW1*JKY))
        Z1AIM=REFRY(3,(NEWOBJ+1))
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
                        ELSE
!     TEL ON
        X1AIM=((REFRY(1,NEWOBJ))+(WW2*JKX))
        Y1AIM=((REFRY(2,NEWOBJ))+(WW1*JKY))
        Z1AIM=0.0D0
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
                        END IF
                        ELSE
!     RAY AIMING
        X1AIM=REFRY(1,NEWOBJ+1)
        Y1AIM=REFRY(2,NEWOBJ+1)
        Z1AIM=REFRY(3,NEWOBJ+1)
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(XC.LT.0.0D0) DDELX=-DDELX
      IF(YC.LT.0.0D0) DDELY=-DDELY
                        END IF
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
!     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
               END IF
      !call logger%logTextWithNum("RAYTRAY Init Y1AIM = ",Y1AIM)
      !call logger%logTextWithNum("RAYTRAY Init R_TY = ", R_TY)


      IF(ITRACE) THEN
!     ILLUMINATION TRACE
!     TANGENTS OF CHIEF RAY SLOPE
      TANN1=REFRY(4,0)/REFRY(6,0)
      TANN2=REFRY(5,0)/REFRY(6,0)
                   END IF
!
!       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
!       THEY ARE CALLED LSTART,MSTART AND NSTART
!
!       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
!       ANGULAR VALUES AS IN HEXAGON AND CODE V.
!       YANG AND XANG ANGLES IN RADIANS.
                        STOPP=0
                        RAYEXT=.TRUE.
                        FAIL=.FALSE.
!
!       THE STARTING COORDINATES AND DIRECTION COSINES FOR THE VERY
!       FIRST PART OF THE RAY TRACE HAVE BEEN CALCULATED
!       BASED UPON THE RESULTS OF THE CURRENT PARAXIAL RAY TRACE AND
!       ASSUMING THAT SURFACE NEWOBJ+1 IS PLANO.
!
!       NEXT STEPS
!
!       1. TRANSLATE TO THE NEXT SURFACE
!       2. IF THAT SURFACE IS TILTED AND/OR DECENTERED,
!          TRANSFORM TO THAT SURFACES COORDINATE SYSTEM.
!       3. INTERSECT THE SURFACE
!       4. INTERACT WITH THAT SURFACE
!       5. REPEAT STEP 1.
!
!       1. TRANSFER TO NEXT SURFACES COORDINATE SYSTEM LOCATED
!       AT THE NEXT SURFACES VERTEX (THIS INCLUDES THE AXIAL THICKNESS).
!       AND TILTS AND DECENTERS.
!
!       CALL TRNSF2
!
!       2. INTERSECT AND INTERACT
!
!       CALL HITSUR
!
!       3. REPEAT WITH A DO LOOP
!
!       STORE THE RAY DATA FOR SURFACE 0 IN THE ARRAY
!       RAYRAY(1:50,0:MAXSUR)
!
!       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
!       SURFACE POINT INTERSECTION
 9                      CONTINUE
      IF(ALENS(3,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
      IF(ALENS(3,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
                        RV=.FALSE.
                        KKK=KKK+1
!       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
!       AND STOP SEARCHING.
        IF(KKK.GT.NRAITR) THEN
                        IF(MSG) THEN
      WRITE(OUTLYNE,*) &
        'RAY FAILURE OCCURRED AT SURFACE ',NEWREF
      CALL SHOWIT(1)
        OUTLYNE= &
        'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
      CALL SHOWIT(1)
                        END IF
                RAYCOD(1)=3
                RAYCOD(2)=NEWREF
                SPDCD1=RAYCOD(1)
                SPDCD2=NEWREF
                        STOPP=1
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        FAIL=.TRUE.
                        RETURN
                        ELSE
                        STOPP=0
                        RAYEXT=.TRUE.
                        FAIL=.FALSE.
!        PROCEED
                        END IF
!
!       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
!       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
!       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
!       STATEMENT NEAR THE END OF THIS ROUTINE.
!     COMPUTE DIR COS DIRECTLY FROM POSITIONS
!
      IF(.NOT.ITRACE) THEN
!     NOT ILLUMINATION TRACING
      MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2) &
      +((ZSTRT-Z1AIM)**2))


        LSTART=(X1AIM-XSTRT)/MAG
        MSTART=(Y1AIM-YSTRT)/MAG
        NSTART=(Z1AIM-ZSTRT)/MAG

      !call logger%logTextWithNum("RAYTRA AIM Iter is ",KKK)
      !call logger%logTextWithNum("RAYTRA Mag is ",MAG)
      !call logger%logTextWithNum("RAYTRA MSTART is ",MSTART)
      !call logger%logTextWithNum("RAYTRA Y1AIM is ", Y1AIM)
      !call logger%logTextWithNum("RAYTRA NSTART is ",NSTART)

                   NINTY=.FALSE.
      IF(NSTART.LT.0.0D0) NINTY=.TRUE.
      IF(NINTY) RVSTART=.TRUE.
        IF(NSTART.EQ.0.0D0) THEN
        YANG=PII/2.0D0
        XANG=PII/2.0D0
                   ELSE
      IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
        YANG=0.0D0
        ELSE
        YANG=DATAN2(MSTART,NSTART)
        END IF
      IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
        XANG=0.0D0
        ELSE
        XANG=DATAN2(LSTART,NSTART)
        END IF
                   END IF
                   ELSE
!
!     ILLUMINATION TRACING
!
      IF(WW1.GT.89.9999D0.AND. &
      WW1.LT.90.0001D0)  WW1=89.9999D0
!
!     CONVERT TO RADIANS
      TWW1=WW1
      TWW2=WW2
      WW1=WW1*PII/180.0D0
      WW2=WW2*PII/180.0D0
!
!     CONVERT WW1 AND WW2 FROM THETA AND PHI TO
!     XANG AND YANG
      WW1WW=DTAN(WW1)*DSIN(WW2)
      WW2WW=DTAN(WW1)*DCOS(WW2)
!     CALC X AND Y TANGENTS OF REGULAR RAY INCLUDING CHIEF RAY ANGLE
      WW1W=(WW1WW+TANN2)/(1.0D0-(WW1WW*TANN2))
      WW2W=(WW2WW+TANN1)/(1.0D0-(WW2WW*TANN1))
!
!
!     DETERMINE LSTART,MSTART AND NSTART DIRECTLY FRON WW1 AND WW2
      NSTART=DSQRT(1.0D0/(((WW1W)**2)+((WW2W)**2)+1.0D0))
      IF(TWW1.GT.90.0D0) NSTART=-NSTART
      LSTART=NSTART*(WW2W)
      MSTART=NSTART*(WW1W)
      MAG=DSQRT((LSTART**2)+(MSTART**2)+(NSTART**2))
      LSTART=LSTART/MAG
      MSTART=MSTART/MAG
      NSTART=NSTART/MAG
      IF(DABS(MSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
        YANG=0.0D0
        ELSE
        YANG=DATAN2(MSTART,NSTART)
        END IF
      IF(DABS(LSTART).EQ.0.0D0.AND.DABS(NSTART).EQ.0.0D0) THEN
        XANG=0.0D0
        ELSE
        XANG=DATAN2(LSTART,NSTART)
        END IF
!     CALCULATE X1AIM,Y1AIM AND Z1AIM IN THE COORDINATE SYSTEM OBJ SURF
!     DON'T NEED BAKONE HERE
      X1AIM=WW2WW*ALENS(3,0)
      Y1AIM=WW1WW*ALENS(3,0)
      !call logger%logTextWithNum("RAYTRA Y1AIM 2209 is ",Y1AIM)

      Z1AIM=0.0D0
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
                   END IF
!
!       FOR SURFACE NEWOBJ
!       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
!       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
!     ZERO IF I-1 IS PERFECT
!       RAYRAY(9,SURF)=COSINE(I)
!       RAYRAY(10,SURF)=COSINE(IP)
!       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
!       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
!       RAYRAY(13,SURF)=LN - SURFACE NORMAL DIRECTION COSINE
!       RAYRAY(14,SURF)=MN - SURFACE NORMAL DIRECTION COSINE
!       RAYRAY(15,SURF)=NN - SURFACE NORMAL DIRECTION COSINE
!       RAYRAY(16,SURF)=XOLD  The next 6 items are the ray values
!       RAYRAY(17,SURF)=YOLD  just before interaction with the surface
!       RAYRAY(18,SURF)=ZOLD  in the surface local coordinate system.
!       RAYRAY(19,SURF)=OLDL
!       RAYRAY(20,SURF)=OLDM
!       RAYRAY(21,SURF)=OLDN
!     THESE LAST 6 ITEMS ARE THE COORDINATES OF THE RAY AT
!     SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
!       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
!     OR TO I-1 IF I-1 IS "PERFECT'
!       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
!       RAYRAY(24,SURF)=1.0D0 FOR POSRAY,-1 FOR NEG RAY
!       RAYRAY(25,SURF)=RAY ENERGY TERM
!       RAYRAY(26,SURF)=RAY XL DIR COS
!       RAYRAY(27,SURF)=RAY XM DIR COS
!       RAYRAY(28,SURF)=RAY XN DIR COS
!       RAYRAY(29,SURF)=RAY YL DIR COS
!       RAYRAY(30,SURF)=RAY YM DIR COS
!       RAYRAY(31,SURF)=RAY YN DIR COS
!       RAYRAY(34,SURF)=FACT_PAR
!       RAYRAY(35,SURF)=FACT_PER
!       RAYRAY(36,SURF)=PHASE_PAR
!       RAYRAY(37,SURF)=PHASE_PER
!       RAYRAY(38,SURF)=POLARIZATION ANGLE IN DEGREES BETWEEN Y-RAY VECTOR AND PARALLEL PLANE
!       RAYRAY(39,SURF)=Y-COMPONENT OF THE ANGLE OF INCIDENCE
!       RAYRAY(40,SURF)=X-COMPONENT OF THE ANGLE OF INCIDENCE
!       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED
                RAYRAY(34:35,NEWOBJ)=1.0D0
                RAYRAY(36:38,NEWOBJ)=1.0D0
                RAYRAY(32,NEWOBJ)=WW3
                RAYRAY(1,NEWOBJ)=XSTRT
                RAYRAY(2,NEWOBJ)=YSTRT
                RAYRAY(3,NEWOBJ)=ZSTRT
                RAYRAY(4,NEWOBJ)=LSTART
                RAYRAY(5,NEWOBJ)=MSTART
                RAYRAY(6,NEWOBJ)=NSTART
                RAYRAY(7,NEWOBJ)=0.0D0
                RAYRAY(8,NEWOBJ)=0.0D0
      IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
      SNIND2=DABS(ALENS(45+INT(WW3),NEWOBJ))/ALENS(45+INT(WW3),NEWOBJ)
      RN1=(ALENS(45+INT(WW3),NEWOBJ))
      RN2=(ALENS(45+INT(WW3),NEWOBJ))
                        END IF
      IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
      SNIND2=DABS(ALENS(65+INT(WW3),NEWOBJ))/ALENS(65+INT(WW3),NEWOBJ)
      RN1=(ALENS(65+INT(WW3),NEWOBJ))
      RN2=(ALENS(65+INT(WW3),NEWOBJ))
                        END IF
      IF(SNIND2.GT.0.0D0) RAYRAY(24,NEWOBJ)=1.0D0
      IF(SNIND2.LT.0.0D0) RAYRAY(24,NEWOBJ)=-1.0D0
      IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
      IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
        RAYRAY(9,NEWOBJ)=NSTART
        RAYRAY(10,NEWOBJ)=NSTART
      IA=DACOS(RAYRAY(9,NEWOBJ))
      IAP=DACOS(RAYRAY(10,NEWOBJ))
                RAYRAY(11,NEWOBJ)=XANG
      IF((RAYRAY(11,NEWOBJ)).LT.0.0D0) RAYRAY(11,NEWOBJ)= &
      RAYRAY(11,NEWOBJ)+(TWOPII)
                RAYRAY(12,NEWOBJ)=YANG
      IF((RAYRAY(12,NEWOBJ)).LT.0.0D0) RAYRAY(12,NEWOBJ)= &
      RAYRAY(12,NEWOBJ)+(TWOPII)
                RAYRAY(13,NEWOBJ)=0.0D0
                RAYRAY(14,NEWOBJ)=0.0D0
                RAYRAY(15,NEWOBJ)=1.0D0
                RAYRAY(16,NEWOBJ)=XSTRT
                RAYRAY(17,NEWOBJ)=YSTRT
                RAYRAY(18,NEWOBJ)=ZSTRT
                RAYRAY(19,NEWOBJ)=LSTART
                RAYRAY(20,NEWOBJ)=MSTART
                RAYRAY(21,NEWOBJ)=NSTART
                RAYRAY(22,NEWOBJ)=0.0D0
                RAYRAY(26,NEWOBJ)=(1.0D0*DCOS(XANG))
                RAYRAY(27,NEWOBJ)=0.0D0
                RAYRAY(28,NEWOBJ)=-(1.0D0*DSIN(XANG))
                RAYRAY(29,NEWOBJ)=0.0D0
                RAYRAY(30,NEWOBJ)=(1.0D0*DCOS(XANG))
                RAYRAY(31,NEWOBJ)=-(1.0D0*DSIN(YANG))

!
!
                X=XSTRT
                Y=YSTRT
                Z=ZSTRT
                L=LSTART
                M=MSTART
                N=NSTART
                        XL=(1.0D0*DCOS(XANG))
                        XM=0.0D0
                        XN=-(1.0D0*DSIN(XANG))
                        YL=0.0D0
                        YM=(1.0D0*DCOS(XANG))
                        YN=-(1.0D0*DSIN(YANG))
!
                ISYS20=NEWIMG
                I=0
                DO 10 I=(NEWOBJ+1),ISYS20

!
!
!       WE ARE CURRENTLY AT SURFACE I-1 AND KNOW WHERE
!       WE ARE AND WHICH DIRECTION WE WANT TO GO.
!       CALLING TRNSF2.FOR TRANSFORMS THE X,Y,Z,L,M AND N INTO
!       THE COORDINATE SYSTEM OF SURFACE I,FOB0 TRACKS IF FOB 0 IS
!       USED OR FOB (SOMETHING NON-ZERO)
                R_X=X
                R_Y=Y
                R_Z=Z
                R_L=L
                R_M=M
                R_N=N
                R_I=I
        CALL TRNSF2
                X=R_X
                Y=R_Y
                Z=R_Z
                L=R_L
                M=R_M
                N=R_N
        XOLD=X
        YOLD=Y
        ZOLD=Z
        LOLD=L
        MOLD=M
        NOLD=N
!       XOLD,YOLD AND ZOLD ARE THE COORDINATES OF THE CURRENT RAY
!       AT SURFACE I-1 IN THE COORDINATE SYSTEM OF SURFACE I
!       NOW INTERSECT THE SURFACE
!
                R_X=X
                R_Y=Y
                R_Z=Z
                R_L=L
                R_M=M
                R_N=N
                R_I=I
                R_XAIM=XAIMOL
                R_YAIM=YAIMOL
                R_ZAIM=ZAIMOL
      IF(RAYRAY(24,R_I-1).GT.0.0D0) POSRAY=.TRUE.
      IF(RAYRAY(24,R_I-1).LT.0.0D0) POSRAY=.FALSE.
                        CALL HITSUR
                        IF(STOPP.EQ.1) THEN
                          PRINT *, "RAYTRA Failed 2370"
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        RETURN
                        END IF
                X=R_X
                Y=R_Y
                Z=R_Z
                L=R_L
                M=R_M
                N=R_N
      IF(RV) RAYRAY(23,I)=-1.0D0
      IF(.NOT.RV) RAYRAY(23,I)=1.0D0
!       LOAD REF RAY REGISTERS
!       FOR SURFACE NEWOBJ
!       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
!       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
!     ZERO IF I-1 IS PERFECT
!       RAYRAY(9,SURF)=COSINE(I)
!       RAYRAY(10,SURF)=COSINE(IP)
!       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
!       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
!       RAYRAY(13,SURF)=L DIRECTION COSINE OF SURFACE NORMAL
!       RAYRAY(14,SURF)=M DIRECTION COSINE OF SURFACE NORMAL
!       RAYRAY(15,SURF)=N DIRECTION COSINE OF SURFACE NORMAL
!       RAYRAY(16,SURF)=XOLD
!       RAYRAY(17,SURF)=YOLD
!       RAYRAY(18,SURF)=ZOLD
!       RAYRAY(19,SURF)=LOLD
!       RAYRAY(20,SURF)=MOLD
!       RAYRAY(21,SURF)=NOLD
!       RAYRAY(22,SURF)=OPL - OPL FROM SURFACE NEWOBJ TO I
!     OR TO I-1 IF I-1 IS "PERFECT'
!       RAYRAY(23,SURF)=1 FOR NOT RV, -1 FOR RV
!       RAYRAY(24,SURF)=1.0D0 FOR POS RAY, -1 FOR NEG RAY
!       RAYRAY(25,SURF)=RAY ENERGY TERM
!       RAYRAY(26,SURF)=RAY XL DIR COS
!       RAYRAY(27,SURF)=RAY XM DIR COS
!       RAYRAY(28,SURF)=RAY XN DIR COS
!       RAYRAY(29,SURF)=RAY YL DIR COS
!       RAYRAY(30,SURF)=RAY YM DIR COS
!       RAYRAY(31,SURF)=RAY YN DIR COS

!       RAYRAY(41,SURF) TO RAYRAY(50,SURF) RESERVED
                RAYRAY(32,I)=WW3
                RAYRAY(1,I)=X
                RAYRAY(2,I)=Y
                RAYRAY(3,I)=Z
                RAYRAY(4,I)=L
                RAYRAY(5,I)=M
                RAYRAY(6,I)=N
                RAYRAY(26,I)=(M*YN)-(N*YM)
                RAYRAY(27,I)=-((L*YN)-(N*YL))
                RAYRAY(28,I)=(L*YM)-(M*YL)
                RAYRAY(9,I)=COSI
                RAYRAY(10,I)=COSIP
!
!     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
      IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) THEN
        SNINDX=DABS(ALENS(45+INT(WW3),I-1))/ALENS(45+INT(WW3),I-1)
        SNIND2=DABS(ALENS(45+INT(WW3),I))/ALENS(45+INT(WW3),I)
      RN1=(ALENS(45+INT(WW3),I-1))
      RN2=(ALENS(45+INT(WW3),I))
                        END IF
      IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) THEN
        SNINDX=DABS(ALENS(65+INT(WW3),I-1))/ALENS(65+INT(WW3),I-1)
        SNIND2=DABS(ALENS(65+INT(WW3),I))/ALENS(65+INT(WW3),I)
      RN1=(ALENS(65+INT(WW3),I-1))
      RN2=(ALENS(65+INT(WW3),I))
                        END IF
        RAYRAY(29,I)=YL
        RAYRAY(30,I)=YM
        RAYRAY(31,I)=YN
                RAYRAY(13,I)=LN
                RAYRAY(14,I)=MN
                RAYRAY(15,I)=NN
                RAYRAY(16,I)=XOLD
                RAYRAY(17,I)=YOLD
                RAYRAY(18,I)=ZOLD
                RAYRAY(19,I)=LOLD
                RAYRAY(20,I)=MOLD
                RAYRAY(21,I)=NOLD
!
!     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
!     DIST TRAVELED TIME THE SNINDX TIMES -1 IF RV IN THE SPACE
!     FROM I-1 TO I
!
!     THE NEXT LINES FIX THE RAY DIRECTION WHEN THERE IS
!     NO Z COMPONENT OF MOTION.
!     THE MAGNITUDE OF THE DISTANCE THE RAY TRAVELED FROM I-1 TO I
!     IS ALWAYS JUST:
!
!

        IF(NUMHITS(I-1).EQ.1.AND.I.GE.1) THEN
        RAYRAY(8,I)=DSQRT( &
        ((RAYRAY(3,I)-ZOLD)**2)+((RAYRAY(2,I)-YOLD)**2) &
        +((RAYRAY(1,I)-XOLD)**2))
                        ELSE
!       NUMHITS STORED ON LAST SURFACE WAS GREATER THAN 1
        RAYRAY(8,I)=0.0D0
!       I-1 WAS AN NSS
        XOLD=0.0D0
        YOLD=0.0D0
        ZOLD=0.0D0
                DO N_HITS=1,NUMHITS(I-1)
        STEPL=DSQRT( &
       (((MULTIRAY_DATA(3,I-1,N_HITS))-ZOLD)**2)+ &
       (((MULTIRAY_DATA(2,I-1,N_HITS))-YOLD)**2)+ &
       (((MULTIRAY_DATA(1,I-1,N_HITS))-XOLD)**2) &
      )
        IF(N_HITS.EQ.1) STEPL1=STEPL
        RAYRAY(8,I)=RAYRAY(8,I)+STEPL
!       CREATE NEW XOLD,YOLD,ZOLD
        XOLD=MULTIRAY_DATA(1,I-1,N_HITS)
        YOLD=MULTIRAY_DATA(2,I-1,N_HITS)
        ZOLD=MULTIRAY_DATA(3,I-1,N_HITS)
                        END DO
        RAYRAY(8,I)=RAYRAY(8,I)-STEPL1
                        END IF
!
      IF(RV) RAYRAY(8,I)=-RAYRAY(8,I)
      IF(.NOT.RV) RAYRAY(8,I)=RAYRAY(8,I)
      IF(DABS(RAYRAY(8,I)).GE.1.0D10) RAYRAY(8,I)=0.0D0
!
      IF(SNIND2.GT.0.0D0) RAYRAY(24,I)=1.0D0
      IF(SNIND2.LT.0.0D0) RAYRAY(24,I)=-1.0D0
!
      IF(GLANAM(I-1,2).EQ.'PERFECT      ') THEN
      RAYRAY(7,I)=0.0D0
      RAYRAY(8,I)=0.0D0
                       END IF
      IF(GLANAM(I-1,2).EQ.'IDEAL        ') THEN
      RAYRAY(8,I)=-(ALENS(121,I-1)-ALENS(3,I-1))*RAYRAY(6,I-1)
                       END IF
      IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) &
      RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(45+INT(WW3),(I-1)))
      IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) &
      RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(65+INT(WW3),(I-1)))
      IF(.NOT.RV) RAYRAY(7,I)=RAYRAY(7,I)+PHASE
      IF(RV) RAYRAY(7,I)=RAYRAY(7,I)-PHASE
!
      IF(L.EQ.0.0D0) THEN
      IF(N.GE.0.0D0) RAYRAY(11,I)=0.0D0
      IF(N.LT.0.0D0) RAYRAY(11,I)=PII
                        ELSE
        IF(DABS(L).GE.DABS(1.0D35*N)) THEN
        IF(L.GE.0.0D0) RAYRAY(11,I)=PII/2.0D0
        IF(L.LT.0.0D0) RAYRAY(11,I)=(3.0D0*PII)/2.0D0
                        ELSE
      IF(DABS(L).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
        RAYRAY(11,I)=0.0D0
        ELSE
        RAYRAY(11,I)=DATAN2(L,N)
        END IF
      IF((RAYRAY(11,I)).LT.0.0D0) RAYRAY(11,I)= &
      RAYRAY(11,I)+(TWOPII)
                        END IF
                        END IF
      IF(M.EQ.0.0D0) THEN
      IF(N.GE.0.0D0) RAYRAY(12,I)=0.0D0
      IF(N.LT.0.0D0) RAYRAY(12,I)=PII
                        ELSE
        IF(DABS(M).GE.DABS(1.0D35*N)) THEN
        IF(M.GE.0.0D0) RAYRAY(12,I)=PII/2.0D0
        IF(M.LT.0.0D0) RAYRAY(12,I)=(3.0D0*PII)/2.0D0
                        ELSE
      IF(DABS(M).EQ.0.0D0.AND.DABS(N).EQ.0.0D0) THEN
        RAYRAY(12,I)=0.0D0
        ELSE
        RAYRAY(12,I)=DATAN2(M,N)
        END IF
      IF((RAYRAY(12,I)).LT.0.0D0) RAYRAY(12,I)= &
      RAYRAY(12,I)+(TWOPII)
                        END IF
                        END IF
        RAYRAY(22,I)=RAYRAY(22,(I-1))+RAYRAY(7,I)
                IF(STOPP.EQ.1) THEN
! NEW STUFF 6/2/94
      IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1.AND..NOT.ITRACE) THEN
              JKX=0.0D0
              JKY=0.0D0
              STOPP=0
              KKK=KKK+1
              GO TO 989
              END IF
                        FAIL=.TRUE.
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
!       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
!       COORDINATES AND LOAD THEM IN ARRAY GLRAY
                IF(GLOBE) THEN
                        CALL GLBRAY
                        END IF
                 IF(GRASET) THEN
!     WE ARE PLANNING TO PLOT THE RAY
!     SHUT OFF GLOBAL IF IT IS ON AND RE-ASSIGN THE GLSURF
                IF(GLOBE) THEN
        OUTLYNE= &
        'GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION'
      CALL SHOWIT(1)
        OUTLYNE= &
        'FOR RAY PLOTTING'
      CALL SHOWIT(1)
                        END IF
                        GLSURF=-99
                        DO IK=0,NEWIMG
      IF(DABS(ALENS(3,IK)).LE.1.0D10) THEN
                        GLSURF=IK
                        GO TO 8761
                        END IF
                        END DO
 8761                   CONTINUE
      IF(GLSURF.EQ.-99) THEN
                        GLOBE=.FALSE.
      OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
      CALL SHOWIT(1)
      OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
      CALL SHOWIT(1)
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
                        CALL GLPRY
                        GLOBE=.FALSE.
                         END IF
                        RETURN
                        ELSE
                        FAIL=.FALSE.
                        STOPP=0
                        RAYEXT=.TRUE.
                        END IF
!
!       CHECK THE RAY HEIGHT AT
!       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
!       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
!       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
!       NRAITR. (DEFAULT IS 100).
!
        IF(I.EQ.NEWREF) THEN
!       CALCULATE TARX AND TARY
        IF(DABS(ALENS(9,I)).GE.1.0D0.AND. &
        DABS(ALENS(9,I)).LE.6.0D0.AND.ALENS(127,I).EQ.0.0D0) THEN
!     CLAPT=TRUE FOR CLAP TILTS AND DECENTED AND FALSE OTHERWISE
      CLAPT=.FALSE.
      IF(ALENS(12,I).NE.0.0D0.OR.ALENS(13,I).NE.0.0D0.OR.ALENS(15,I) &
      .NE.0.0D0) CLAPT=.TRUE.
!       REF SURF HAS CLAP ON IT
!       THE VALUES OF TARY AND TARX ARE COORDINATES IN THE
!       LOCAL COORDINATE SYSTEM OF THE REFERENCE SYSTEM.
!       IF THE CLAP IS DECENTERED, THE TAR() VALUES ARE
!       MODIFIED BY ADDING THE CLAP DECENTRATIONS SINCE THE RAY
!       IS AIMED TO THE RELATIVE REFERENCE SURFACE COORDINATES.
!
!       SET TARGET TO CENTER OF DECENTERED CLAP, ALENS(12,I),
!       AND ALENS(13,I) ARE CLAP DECENTRATIONS
!
      WWW1=WW1
      WWW2=WW2
      IF(SYSTEM(70).EQ.1.0D0.AND.ALENS(1,I).NE.0.0D0.AND. &
      ALENS(9,I).EQ.1.0D0.AND.ALENS(12,I).EQ.0.0D0.AND. &
      ALENS(13,I).EQ.0.0D0.AND.ALENS(15,I).EQ.0.0D0) THEN
      IF(DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(10,I)).AND. &
      DABS(1.0D0/ALENS(1,I)).GE.DABS(ALENS(11,I))) &
      CALL APLANA(I,WW1,WW2,WWW1,WWW2)
                       END IF
!
!       CIRCULAR CLAP
!
        IF(DABS(ALENS(9,I)).EQ.1.0D0) THEN
      IF(CLAPT) THEN
      IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                TARY=(ALENS(10,I)*WWW1)
                TARX=(ALENS(10,I)*WWW2)
                   ELSE
                TARY=(ALENS(12,I))+(ALENS(11,I)*WWW1)
                TARX=(ALENS(13,I))+(ALENS(11,I)*WWW2)
                   END IF
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
                TARX=TARX+ALENS(13,I)
                TARY=TARY+ALENS(12,I)
!       NOW IS THE CLAP TILTED ?
        GAMMA=(ALENS(15,I)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      ELSE
!     NO CLAP DEC OR TILTS
                TARY=(ALENS(10,I)*WWW1)
                TARX=(ALENS(10,I)*WWW2)
      IF(ALENS(10,I).LE.ALENS(11,I)) THEN
                TARY=(ALENS(10,I)*WWW1)
                TARX=(ALENS(10,I)*WWW2)
                   ELSE
                TARY=(ALENS(11,I)*WWW1)
                TARX=(ALENS(11,I)*WWW2)
                   END IF
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
!       NOW IS THE REF SURF ORIENTATION ANGLE ?
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      END IF
!       NOT CIRCULAR CLAP
                        END IF
!        RECT CLAP
!
        IF(DABS(ALENS(9,I)).EQ.2.0D0) THEN
      IF(CLAPT) THEN
      IF(ANAAIM) THEN
                TARY=(ALENS(10,I)*WW1)
                TARX=(ALENS(11,I)*WW2)
                   ELSE
      IF(DABS(ALENS(10,I)).GT.DABS(ALENS(11,I))) THEN
                TARY=(ALENS(10,I)*WW1)
                TARX=(ALENS(10,I)*WW2)
                   ELSE
                TARY=(ALENS(12,I))+(ALENS(11,I)*WW1)
                TARX=(ALENS(13,I))+(ALENS(11,I)*WW2)
                   END IF
                   END IF
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
                TARX=TARX+ALENS(13,I)
                TARY=TARY+ALENS(12,I)
!       NOW IS THE CLAP TILTED ?
        GAMMA=(ALENS(15,I)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      ELSE
                TARY=(ALENS(10,I)*WW1)
                TARX=(ALENS(11,I)*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      END IF
!       NOT RECT CLAP
                        END IF
!        ELIP CLAP
!
                YVALUE=ALENS(10,I)
                XVALUE=ALENS(11,I)
!
        IF(DABS(ALENS(9,I)).EQ.3.0D0) THEN
      IF(CLAPT) THEN
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
                TARX=TARX+ALENS(13,I)
                TARY=TARY+ALENS(12,I)
!       NOW IS THE CLAP TILTED ?
        GAMMA=(ALENS(15,I)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      ELSE
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      END IF
!       NOT ELIP CLAP
                        END IF
!        RCTK CLAP
!
        IF(DABS(ALENS(9,I)).EQ.4.0D0) THEN
      IF(CLAPT) THEN
                YVALUE=ALENS(10,I)
                XVALUE=ALENS(11,I)
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
                TARX=TARX+ALENS(13,I)
                TARY=TARY+ALENS(12,I)
!       NOW IS THE CLAP TILTED ?
        GAMMA=(ALENS(15,I)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      ELSE
                YVALUE=ALENS(10,I)
                XVALUE=ALENS(11,I)
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      END IF
!       NOT RCTK CLAP
                        END IF
!        POLY CLAP
!
        IF(DABS(ALENS(9,I)).EQ.5.0D0) THEN
      IF(CLAPT) THEN
                YVALUE=ALENS(10,I)
                XVALUE=ALENS(10,I)
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
                TARX=TARX+ALENS(13,I)
                TARY=TARY+ALENS(12,I)
!       NOW IS THE CLAP TILTED ?
        GAMMA=(ALENS(15,I)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      ELSE
                YVALUE=ALENS(10,I)
                XVALUE=ALENS(10,I)
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      END IF
!       NOT POLY CLAP
                        END IF
        IF(DABS(ALENS(9,I)).EQ.6.0D0) THEN
      IF(CLAPT) THEN
                YVALUE=ALENS(14,I)
                XVALUE=ALENS(14,I)
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
                TARX=TARX+ALENS(13,I)
                TARY=TARY+ALENS(12,I)
!       NOW IS THE CLAP TILTED ?
        GAMMA=(ALENS(15,I)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      ELSE
                YVALUE=ALENS(11,I)
                XVALUE=ALENS(11,I)
                TARY=(YVALUE*WW1)
                TARX=(XVALUE*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                      END IF
!       NOT IPOLY CLAP
                        END IF
!
                        ELSE
!       NO CLAP ON REF SURF OR MULTI-CLAP
                        TARY=(PXTRAY(1,I)*WW1)
                        TARX=(PXTRAX(1,I)*WW2)
        IF(SYSTEM(128).NE.0.0D0) TARX=-TARX
        IF(SYSTEM(129).NE.0.0D0) TARY=-TARY
        GAMMA=(SYSTEM(59)*(PII))/180.0D0
        TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
        TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
        TARY=TARRY
        TARX=TARRX
                        END IF
!
        TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
        !call logger%logTextWithNum("RAYTRA AIMTOL CHECK IS ", TEST)
        IF(TEST.LE.AIMTOL &
      .OR.SYSTEM(62).EQ.0.0D0.OR.ITRACE) THEN
!       AIM IS GOOD ENOUGH, PROCEED
                        AIMOK=.TRUE.
                        GO TO 100
                        ELSE
                        AIMOK=.FALSE.
!       AIM NOT GOOD ENOUGH, IMPROVE GUESS
                        END IF
!
!       SET X1LAST TO X1ONE AND Y1LAST TO Y1ONE
!       X1ONE AND Y1ONE ARE THE FIRST SET OF RAY
!       COORDINATES AT SURFACE 1
                        X1ONE=X1LAST
                        Y1ONE=Y1LAST
!       SET XAIMOL AND YAIMOL TO X1LAST AND Y1LAST
!       X1LAST AND Y1LAST ARE THE LAST RAY COORDINATES
!       AT SURFACE 1
                        X1LAST=XAIMOL
                        Y1LAST=YAIMOL
!       SET RXONE AND RYONE TO RXLAST AND RYLAST
!       RYONE ANE RXONE ARE THE FIRST SET OF RAY
!       COORDINATES AT THE REFERENCE SURFACE
                        RXONE=RXLAST
                        RYONE=RYLAST
!       SET RXLAST AND RYLAST TO X AND Y
!       RXLAST AND RYLAST ARE THE LAST X AND Y RAY COORDINATES
!       ON THE REFERENCE SURFACE.
                        RXLAST=X
                        RYLAST=Y
!       X AND Y ARE THE CURRENT RAY COORDINATES AT THE
!       REFERENCE SURFACE.
!
!       GUESS CASE 1, REPRESENTS THE FIRST REFINEMENT
!       OF THE AIMING POINT.
!       THIS OCCURS IF KKK=1
        IF(KKK.EQ.1) THEN
!       THIS IS CASE 1
!       IN THIS CASE WE SET THE SURFACE 1 AIMING POINTS
!       EQUAL TO THE OLD AIM POINTS PLUS A SMALL DELTA
!       IN ORDER TO CALCULATE DERIVATIVES.
!
        X1AIM=XAIMOL+DDELX
        Y1AIM=YAIMOL+DDELY
        Z1AIM=ZAIMOL
      XC=X1AIM
      YC=Y1AIM
      ZC=Z1AIM
      XC1=XC
      YC1=YC
      ZC1=ZC
      IF(ALENS(1,1).NE.0.0D0) &
      CALL GETZEE1
        X1AIM=XC
        Y1AIM=YC
        Z1AIM=ZC
!
!       CALCULATE NEW SET OF VALUES FOR DERIVATIVES
!
                        XAIMOL=XC1
                        YAIMOL=YC1
                        ZAIMOL=ZC1
!     CALL BAKONE TO CONVERT TO NEWOBJ COORDINATES
                R_TX=X1AIM
                R_TY=Y1AIM
                R_TZ=Z1AIM
        CALL BAKONE
                X1AIM=R_TX
                Y1AIM=R_TY
                Z1AIM=R_TZ
                        GO TO 9
!       THIS IS NOT THE FIRST REFINEMENT, KKK NOT = 1
                        END IF
!
      ! call logger%logTextWithNum("RayDeriv for Ray Iter", KKK)
      ! call logger%logTextWithNum("X1Last = ", X1LAST)
      ! call logger%logTextWithNum("Y1Last = ", Y1LAST)
      ! call logger%logTextWithNum("X1One = ", X1ONE)
      ! call logger%logTextWithNum("Y1One = ", Y1ONE)
      ! call logger%logTextWithNum("RYOne = ", RYONE)
      ! call logger%logTextWithNum("RYLast = ", RYLAST)



      CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE &
      ,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
!
!       MF1=TARX-RXLAST, MF2=TARY-RYLAST
!
!       TARX AND TARY ARE THE COORDINATES OF THE CENTER
!       OF THE DECENTERED CLEAR APERTURE ON THE REFERENCE
!       SURFACE IF THERE IS A CLEAR APERTURE DEFINED AND IT
!       IS DECENTERED. IF THESE CONDITIONS DO NOT EXIST,
!       TARY AND TARX ARE BOTH IDENTICALLY 0.0D0.
!
!
                MF1=TARX-RXLAST
                MF2=TARY-RYLAST
        DELFAIL=.FALSE.
        CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)

        !call logger%logTextWithNum("DDELY is ", DDELY)

        IF(DELFAIL) THEN
                        RETURN
                        ELSE
                        GO TO 9
                        END IF
!       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
                        END IF
!
 100            CONTINUE
!
 10             CONTINUE
!       CACOCH IS THE FLAG WHICH TELLS WHETHER OR NOT
!       TO CHECK FOR CLAP/COBS INTERFERENCE.
!       IF CACOCH=0 DO NOT CHECK
!       IF CACOCH=1 DO THE CHECK
!       HERE WE CHECK FOR BLOCKAGE BY CLAP OR COBS.
!       AND DO THE APPROPRIATE THINGS.
        IF(CACOCH.EQ.1) THEN
               DO R_I=NEWOBJ+1,NEWIMG-1
!       CALL CLAP CHECKING ROUTINE
                R_X=RAYRAY(1,R_I)
                R_Y=RAYRAY(2,R_I)
                R_Z=RAYRAY(3,R_I)
!     DON'T CHECK ON OBJECT OR IMAGE SURFACES.
!
                MMSG=MSG
      IF(DABS(ALENS(34,R_I)).NE.24.0D0) THEN
!     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
      IF(INT(ALENS(127,R_I)).EQ.0.AND.INT(ALENS(128,R_I)).EQ.0) THEN
      CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
                ELSE
            IF(INT(ALENS(127,R_I)).NE.0) THEN
            DO JK=1,INT(ALENS(127,R_I))
      IF(MMSG) THEN
      MSG=.TRUE.
      IF(JK.LT.INT(ALENS(127,R_I))) MSG=.FALSE.
                        END IF
      JK1=MULTCLAP(JK,1,R_I)
      JK2=MULTCLAP(JK,2,R_I)
      JK3=MULTCLAP(JK,3,R_I)
      CALL CACHEK(JK1,JK2,JK3,1)
      IF(RAYCOD(1).EQ.0) THEN
                   SPDCD1=RAYCOD(1)
                   SPDCD2=RAYCOD(2)
                   STOPP=0
                   RAYEXT=.TRUE.
                   GO TO 25
                   END IF
                   END DO
 25                CONTINUE
                END IF
            IF(INT(ALENS(128,R_I)).NE.0) THEN
            DO JK=1,INT(ALENS(128,R_I))
      IF(MMSG) THEN
      MSG=.TRUE.
                        END IF
      JK1=MULTCOBS(JK,1,R_I)
      JK2=MULTCOBS(JK,2,R_I)
      JK3=MULTCOBS(JK,3,R_I)
      CALL CACHEK(JK1,JK2,JK3,2)
      IF(RAYCOD(1).NE.0) THEN
                   PRINT *, "RAYTRA FAILED RAYCOD=1 RAYTRA5.FOR L 3025"
                   SPDCD1=RAYCOD(1)
                   SPDCD2=RAYCOD(2)
                   STOPP=1
                   RAYEXT=.TRUE.
                   GO TO 26
                   END IF
                   END DO
 26                CONTINUE
                END IF
                END IF
                END IF
!
!       THIS ROUTINE SETS THE FLAGS
!       CAERAS AND COERAS
!       SET IF THE CURRENT SURFACE HAD A COBS OR
!       CLAP ERASE.
        IF(STOPP.EQ.1) GO TO 90
                        STOPP=0
                        FAIL=.FALSE.
                        RAYEXT=.TRUE.
!       CONTINUE THE RAYTRACE
                       END DO
                       GO TO 91
                           ELSE
!       NO CHECK TO BE MADE FOR CLAPS/COBS BLOCKAGE
                        END IF
                       GO TO 91
 90                    CONTINUE
                        FAIL=.TRUE.
                        RAYEXT=.FALSE.
                        POLEXT=.FALSE.
 91                    CONTINUE
!
!     FINISHED CLAP/COBS CHEKING
!       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
!       COORDINATES AND LOAD THEM IN ARRAY GLRAY
                IF(GLOBE) THEN
                        CALL GLBRAY
                        END IF
                 IF(GRASET) THEN
!     WE ARE PLANNING TO PLOT THE RAY
!     SHUT OFF GLOBAL IF IT IS ON AND RE-ASSIGN THE GLSURF
                IF(GLOBE) THEN
        OUTLYNE= &
        'GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION'
      CALL SHOWIT(1)
        OUTLYNE= &
        'FOR RAY PLOTTING'
      CALL SHOWIT(1)
                        END IF
                        GLSURF=-99
                        DO I=0,NEWIMG
      IF(DABS(ALENS(3,I)).LE.1.0D10) THEN
                        GLSURF=I
                        GO TO 876
                        END IF
                        END DO
 876                    CONTINUE
      IF(GLSURF.EQ.-99) THEN
                        GLOBE=.FALSE.
      OUTLYNE='ALL SURFACES WERE OF INFINITE THICKNESS'
      CALL SHOWIT(1)
      OUTLYNE='NO OPTICAL SYSTEM PLOT COULD BE MADE'
      CALL SHOWIT(1)
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
                        CALL GLPRY
                        GLOBE=.FALSE.
                         END IF
                        RAYRAY(25,NEWOBJ:NEWIMG)=0.0D0
                        RAYRAY(34:38,NEWOBJ:NEWIMG)=0.0D0
!     COMPUTE RAY ENERGY
                RAYRAY(34:38,I)=0.0D0
                        DO I=NEWOBJ,NEWIMG
      IF(I.EQ.NEWOBJ) THEN
        IF(REFEXT) THEN
        RAYRAY(25,I)=WW4*REFRY(9,NEWOBJ)
                ELSE
        RAYRAY(25,I)=WW4
                END IF
                        ELSE
        RAYRAY(25,I)=RAYRAY(25,I-1)
                        END IF
      IF(I.EQ.NEWOBJ) THEN
      IF(WA3.GE.1.AND.WA3.LE.5) THEN
      RN1=(ALENS(45+WA3,I))
      RN2=(ALENS(45+WA3,I))
                        END IF
      IF(WA3.GE.6.AND.WA3.LE.10) THEN
      RN1=(ALENS(65+WA3,I))
      RN2=(ALENS(65+WA3,I))
                        END IF
                        ELSE
      IF(WA3.GE.1.AND.WA3.LE.5) THEN
      RN1=(ALENS(45+WA3,I-1))
      RN2=(ALENS(45+WA3,I))
                        END IF
      IF(WA3.GE.6.AND.WA3.LE.10) THEN
      RN1=(ALENS(65+WA3,I-1))
      RN2=(ALENS(65+WA3,I))
                        END IF
                        END IF
      IF(ALENS(34,I).EQ.19.0D0) THEN
!     CALL THE GRIDS ROUTINE WITH ARG = 2
!     THIS CAUSES THE RAY ENERGY TO BE MULTIPLIED BY THE
!     APODIZATION REPRESENTED IN THE APGRI FILE
               ISURF=I
               GERROR=.FALSE.
      XPASS=X
      YPASS=Y
      IPASS1=2
               CALL GRIDS(2,ISURF,GERROR)
      IF(.NOT.GERROR) GRIDSUNLOADED19(I)=.FALSE.
      IF(GERROR) THEN
                IF(MSG) THEN
      WRITE(OUTLYNE,*)'RAY FAILURE OCCURRED AT SURFACE ',I
      CALL SHOWIT(1)
        OUTLYNE= &
      'GRID FILE DOES NOT EXIST FOR THIS SURFACE'
      CALL SHOWIT(1)
        OUTLYNE= &
      'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
      CALL SHOWIT(1)
                        END IF
                STOPP=1
                RAYCOD(1)=15
                RAYCOD(2)=I
                RAYEXT=.FALSE.
                        POLEXT=.FALSE.
                        RETURN
                   END IF
        FACT_PAR=0.0D0
        FACT_PER=0.0D0
        PHASE_PAR=0.0D0
        PHASE_PER=0.0D0
        POLANG=0.0D0
                   END IF
      IF(SYSTEM(103).EQ.1.0D0) THEN
!     SCREEN SURFACE
      IF(I.EQ.INT(SYSTEM(104))) THEN
!       GOT THE SCREEN SURFACE
      AOI=DABS(DACOS(RAYRAY(9,I)))
      D=SYSTEM(105)
      H=SYSTEM(106)
      S=SYSTEM(107)
      IF(DCOS(AOI).EQ.0.0D0.OR. &
      AOI.GE.DABS(SYSTEM(108))) THEN
                        FACTOR=0.0D0
                        ELSE
      FACTOR=PII*(((D)-(H*DSIN(AOI))) &
      *(((D)*DCOS(AOI))-(H*DSIN(AOI))))/(4.0D0*S*S*DCOS(AOI))
                        END IF
      IF(FACTOR.LT.0.0D0) FACTOR=0.0D0
      RAYRAY(25,I)=RAYRAY(25,I)*FACTOR
                END IF
                END IF
      IF(ALENS(34,I).NE.19.0D0) THEN
!
!     NOT AN APODIZATION SURFACE
      IF(DUM(I).AND.I.GT.0) THEN
                RAYRAY(34:38,I)=0.0D0
                        END IF
!       EVEN THOUGH WE DON'T ALWAYS DO POLARIZATION, WE NEED THE POLANG
!       THE UNIT VECTOR JK_CPL,JK_CPM,JK_CPN IS NORMAL TO
!       THE PLANE OF INCIDENCE AND LIES IN A PLANE
!       NORMAL TO THE SURFACE AT THE POINT OF INTERSECTION
      JK_L1=RAYRAY(19,I)
      JK_M1=RAYRAY(20,I)
      JK_N1=RAYRAY(21,I)
      JK_L2=RAYRAY(4,I)
      JK_M2=RAYRAY(5,I)
      JK_N2=RAYRAY(6,I)
      IF(JK_L1.EQ.JK_L2.AND.JK_M1.EQ.JK_M2.AND.JK_N1.EQ.JK_N2) THEN
      JK_L1=RAYRAY(13,I)
      JK_M1=RAYRAY(14,I)
      JK_N1=RAYRAY(15,I)
      JK_L2=RAYRAY(4,I)
      JK_M2=RAYRAY(5,I)
      JK_N2=RAYRAY(6,I)
                        END IF
      CALL CROSS_PRODUCT(JK_CPL,JK_CPM,JK_CPN &
      ,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
      MAG=DSQRT((JK_CPL**2)+(JK_CPM**2)+(JK_CPN**2))
      JK_CPL=DABS(JK_CPL)
      JK_CPM=DABS(JK_CPM)
      JK_CPN=DABS(JK_CPN)
      IF((MAG).NE.0.0D0) THEN
      JK_CPL=JK_CPL/MAG
      JK_CPM=JK_CPM/MAG
      JK_CPN=JK_CPN/MAG
        ELSE
      JK_CPL=1.0D0
      JK_CPM=0.0D0
      JK_CPN=0.0D0
        END IF
!       THE UNIT VECTOR IN THE
!       INCIDENT DIRECTION IS:
      JK_L1=JK_CPL
      JK_M1=JK_CPM
      JK_N1=JK_CPN
      JK_L2=RAYRAY(13,I)
      JK_M2=RAYRAY(14,I)
      JK_N2=RAYRAY(15,I)
      CALL CROSS_PRODUCT(SA_CPL,SA_CPM,SA_CPN &
      ,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
      MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
      SA_CPL=DABS(SA_CPL)
      SA_CPM=DABS(SA_CPM)
      SA_CPN=DABS(SA_CPN)
      IF((MAG).NE.0.0D0) THEN
      SA_CPL=SA_CPL/MAG
      SA_CPM=SA_CPM/MAG
      SA_CPN=SA_CPN/MAG
                ELSE
      SA_CPL=0.0D0
      SA_CPM=1.0D0
      SA_CPN=0.0D0
                END IF
!       WE NEED TO USE THIS INCIDENT DIRECTION VECTOR BUT IT NEEDS TO BE
!       MODIFIED SO THAT ITS Z-ORIENTATION IS PARALLEL TO THE Z-COORDINATE
!       OF THE RYL,RYM,RYN VECTOR
        SA_CPN = RYN(I)
        MAG=DSQRT((1.0D0-(SA_CPN**2))/((SA_CPL**2)+(SA_CPM**2)))
        SA_CPL=MAG*SA_CPL
        SA_CPM=MAG*SA_CPM
        MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
!       THE DOT PRODUCT OF THIS UNIT VECTOR WITH RYL,RYM,RYM UNIT VECTOR
!       GIVES THE COSINE OF THE ANGLE BETWEEN THE Y-VECTOR OF THE RAY
!       AND THE DIRECTION OF INCIDENCE
        JK_L1=RYL(I)
        JK_M1=RYM(I)
        JK_N1=RYN(I)
        JK_L2=SA_CPL
        JK_M2=SA_CPM
        JK_N2=SA_CPN
      CALL DOT_PRODUCT(DP,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
!       POLARIZATION COSINE IS THEN
        IF(DP.GT.1.0D0) DP=1.0D0
        IF(DP.LT.-1.0D0) DP=-1.0D0
       POLANG=DACOS(DP)
        IF(POLANG.GT.PII/2.0D0) POLANG=PII-POLANG
        IF(POLANG.LT.-PII/2.0D0) POLANG=PII+POLANG
        RAYRAY(38,I)=(POLANG*180.0D0)/PII
        RAYRAY(39,I)=DCOS(POLANG)*DACOS(RAYRAY(9,I))
        RAYRAY(40,I)=DSIN(POLANG)*DACOS(RAYRAY(9,I))
!       POL ANG DONE
      IF(COATSET) THEN
      J=INT(ALENS(112,I))
      IF(RAYRAY(9,I).GT.1.0D0) RAYRAY(9,I)=1.0D0
      IF(RAYRAY(9,I).LT.-1.0D0) RAYRAY(9,I)=-1.0D0
      IF(RAYRAY(10,I).GT.1.0D0) RAYRAY(10,I)=1.0D0
      IF(RAYRAY(10,I).LT.-1.0D0) RAYRAY(10,I)=-1.0D0
      IA=DACOS(RAYRAY(9,I))
      IAP=DACOS(RAYRAY(10,I))
      WA3=INT(WW3)
        PATHL=RAYRAY(8,I)
        IF(I.EQ.NEWOBJ) OLDABSCOEF(1:10)=0.0D0
        IF(I.EQ.NEWOBJ) ABSCOEF(1:10)=0.0D0
      CALL ENERGY_ADJUST(ENERGY_FACTOR,I,J,IA,IAP,RN1,RN2,WA3,POLANG &
      ,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL)
      IF(I.GT.NEWOBJ) RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
               END IF
      IF(ALENS(96,I).EQ.1.0D0.AND.ALENS(98,I).NE.0.0D0) THEN
      IA=DACOS(RAYRAY(9,I))
      WA3=INT(WW3)
      ENERGY_FACTOR=1.0D0
      CALL DIFFRACTION_EFFICIENCY(ENERGY_FACTOR,I,IA,WA3)
      IF(I.GT.NEWOBJ)RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
               END IF
                RAYRAY(34,I)=FACT_PAR
                RAYRAY(35,I)=FACT_PER
                RAYRAY(36,I)=PHASE_PAR
                RAYRAY(37,I)=PHASE_PER
                END IF
                        END DO
                        RETURN
end subroutine

! This is to support last surface nonzero thickness
! Will just propagate the ray based on position and
! direction cosine.  
! Do not support curvature, non air index, or 
!       LOAD REF RAY REGISTERS
!       FOR SURFACE NEWOBJ
!       RAYRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
!       RAYRAY(4,SURF)=L(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(5,SURF)=M(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(6,SURF)=N(LOCAL)- DIRECTION COSINE AT SURFACE I
!       RAYRAY(8,SURF)=LEN - PHYSICAL LENGTH ALONG RAY FROM SURF I-1 TO I
!       RAYRAY(7,SURF)=OPL - OPL FROM SURFACE I-1 TO I
!     ZERO IF I-1 IS PERFECT
!       RAYRAY(9,SURF)=COSINE(I)
!       RAYRAY(10,SURF)=COSINE(IP)
!       RAYRAY(11,SURF)=UX - (RADIAN) MARGINAL XZ SLOPE ANGLE
!       RAYRAY(12,SURF)=UY - (RADIAN) MARGINAL YZ SLOPE ANGLE
!       RAYRAY(13,SURF)=L DIRECTION COSINE OF SURFACE NORMAL
!       RAYRAY(14,SURF)=M DIRECTION COSINE OF SURFACE NORMAL
!       RAYRAY(15,SURF)=N DIRECTION COSINE OF SURFACE NORMAL
subroutine adjustLastSurface_old(L, RV)
    use DATLEN, only: RAYRAY, PHASE, WW3, ALENS
    use type_utils, only: real2str, int2str
    use mod_lens_data_manager
    integer :: L
    logical :: RV
    integer :: jj
    double precision :: angX, angY, angZ, newX, newY, newZ, dLen, dOPL



    !call LogTermFOR("Adjusting Last Surface Thickness")


    ! Test calcs
    angX = (RAYRAY(4,L))
    angY = (RAYRAY(5,L))
    angZ = (RAYRAY(6,L))
    !ang1 = ACOS(RAYRAY(4,L-1))
    !ang2 = ACOS(RAYRAY(5,L-1))
    !ang3 = ACOS(RAYRAY(6,L-1))

    ! Propagate to final plane
    newX = RAYRAY(1,L) + angX*ldm%getSurfThi(L)
    newY = RAYRAY(2,L) + angY*ldm%getSurfThi(L)
    newZ = RAYRAY(3,L) + angZ*ldm%getSurfThi(L) 




    
    RAYRAY(1,L) = newX
    RAYRAY(2,L) = newY


    ! do jj=1,15
    !     call LogTermFOR("RAYRAY "//int2str(jj)// "= "//real2str(RAYRAY(jj,L-1)))
    ! end do

    ! call LogTermFOR("ang1 is "//real2str(ang1))
    ! call LogTermFOR("ang2 is "//real2str(ang2))
    ! call LogTermFOR("ang3 is "//real2str(ang3))

    ! call LogTermFOR("newX is "//real2str(newX))
    ! call LogTermFOR("newY is "//real2str(newY))
    ! call LogTermFOR("newZ is "//real2str(newZ))   


    ! Need to adjust OPL

!       RAYRAY(16,SURF)=XOLD
!       RAYRAY(17,SURF)=YOLD
!       RAYRAY(18,SURF)=ZOLD

     
    dLen = DSQRT((angX*ldm%getSurfThi(L))**2+(angY*ldm%getSurfThi(L))**2)
    !  dLen =DSQRT( &
    !      ((RAYRAY(3,L)-RAYRAY(18,L))**2)+((RAYRAY(2,L)-RAYRAY(17,L))**2) &
    !     +((RAYRAY(1,L)-RAYRAY(16,L))**2))

      RV = .FALSE.
      if (ldm%getSurfThi(L) < 0 ) RV = .TRUE.
      IF(RV) dLen=-dLen
      IF(DABS(dLen).GE.1.0D10) dLen=0.0D0

      !dOPL = dLen/(sysConfig%getWavelength(INT(WW3))/1E3)
!
       IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) &
       dOPL=dLen*DABS(ALENS(45+INT(WW3),(L-1)))
       IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) &
       dOPL=dLen*DABS(ALENS(65+INT(WW3),(L-1)))
      IF(.NOT.RV) dOPL=dOPL+PHASE
      IF(RV) dOPL=dOPL-PHASE

    !call LogTermFOR("Wavelength is "//real2str(sysConfig%getWavelength(INT(WW3))))  
    call LogTermFOR("dLen is "//real2str(dLen))
    call LogTermFOR("dOPL is "//real2str(dOPL, precision=3))
    call LogTermFOR("NewOPL is "//real2str(dOPL+RAYRAY(7,L)))
    call LogTermFOR("NewLEN is "//real2str(dLEN+RAYRAY(8,L)))

    RAYRAY(7,L) = dOPL+RAYRAY(7,L)
    RAYRAY(8,L) = dLen+RAYRAY(8,L)
    ! TODO:  Dump all OPL here and print.
    ! I think the delta length is being calculated propoerly, but something
    ! is getting messed up in converting to OPL

     do jj=1,L
         call LogTermFOR("OPL "//int2str(jj)// "= "//real2str(RAYRAY(7,jj)))
     end do    

end subroutine


subroutine adjustLastSurface(L, rayData)
! The idea here as a quick and dirty support to adjust the last surface
! Is to propogate the ray the extra distance specified by the user
! and store it in the last ray, also correctly computing the ray distance
! so wave aberrations can be computed
! Since we only support unity index and no curvature, the calcluation is 
! pretty straightforward                    
    use DATLEN, only: PHASE, WW3, ALENS
    use type_utils, only: real2str, int2str
    use mod_lens_data_manager
    integer :: L
    double precision, intent(inout) :: rayData(1:50,0:499)
    logical :: RV
    integer :: jj
    double precision :: angX, angY, angZ, newX, newY, newZ, dLen, dOPL


    ! Angle of ray is stored in these indicdes.  These won't be updated
    ! but are used to progate the ray.
    ! Angle is stored as a direction cosine but I want to actual angle
    angX = 3.14159/2-ACOS(rayData(4,L)) 
    angY = 3.14159/2-ACOS(rayData(5,L)) 
    !angZ = 3.14159/2-ACOS(rayData(6,L)) 

    ! call LogTermFOR("ang1 is "//real2str(angX))
    ! call LogTermFOR("ang2 is "//real2str(angY))
    ! call LogTermFOR("ang3 is "//real2str(angZ))

    ! Propagate to final plane
    newX = rayData(1,L) + TAN(angX)*ldm%getSurfThi(L)
    newY = rayData(2,L) + TAN(angY)*ldm%getSurfThi(L)
    ! New z is not calculated, as I know newZ (the thickness the user entered)
    ! TODO:  Make sure this is true for all use cases
    !newZ = rayData(3,L) + TAN(angZ)*ldm%getSurfThi(L) 

    ! do jj=1,21
    !     call LogTermFOR("RAYRAY "//int2str(jj)// "= "//real2str(rayData(jj,L)))
    ! end do

    ! Need to adjust OPL

    ! Length change is 
    dLen = DSQRT((rayData(1,L)-newX)**2+(rayData(2,L)-newY)**2+(ldm%getSurfThi(L))**2) 

    rayData(1,L) = newX
    rayData(2,L) = newY


    ! Here the logic follows what is being used in the current
    ! ray tracing routines.  Once I figure out how to refactor this
    ! will come back and clean this up
      RV = .FALSE.
      if (ldm%getSurfThi(L) < 0 ) RV = .TRUE.
      IF(RV) dLen=-dLen
      IF(DABS(dLen).GE.1.0D10) dLen=0.0D0

      !dOPL = dLen/(sysConfig%getWavelength(INT(WW3))/1E3)
!
       IF(INT(WW3).GE.1.AND.INT(WW3).LE.5) &
       dOPL=dLen*DABS(ALENS(45+INT(WW3),(L-1)))
       IF(INT(WW3).GE.6.AND.INT(WW3).LE.10) &
       dOPL=dLen*DABS(ALENS(65+INT(WW3),(L-1)))
      IF(.NOT.RV) dOPL=dOPL+PHASE
      IF(RV) dOPL=dOPL-PHASE

    ! call LogTermFOR("Thickness is "//real2str(ldm%getSurfThi(L)))  
    ! call LogTermFOR("Original OPL is "//real2str(rayData(7,L)))
    ! call LogTermFOR("dOPL is "//real2str(dOPL, precision=3))    
    ! call LogTermFOR("dLen is "//real2str(dLen))
    ! call LogTermFOR("dOPL is "//real2str(dOPL, precision=3))

    rayData(7,L) = dOPL+rayData(7,L)
    rayData(8,L) = dLen+rayData(8,L)

    ! call LogTermFOR("NewOPL is "//real2str(rayData(7,L)))
    ! call LogTermFOR("NewLEN is "//real2str(rayData(8,L)))

    !    do jj=1,L
    !      call LogTermFOR("X is "//int2str(jj)// "= "//real2str(rayData(1,jj)))
    !      call LogTermFOR("Y is "//int2str(jj)// "= "//real2str(rayData(2,jj)))
    !      call LogTermFOR("Z is "//int2str(jj)// "= "//real2str(rayData(3,jj)))
    !      call LogTermFOR("OPL "//int2str(jj)// "= "//real2str(rayData(7,jj)))
    !    end do    

end subroutine

! There is a 2 here because the reference ray 
! routine has a slightly different logic.  The two differences are
! The X and Y values are taken from the OLD values (bug perhaps in original code?)
! The global variable used to store the wavelength is different. Once I properly refactor
! the ray tracing code presumably this problem will go away
subroutine adjustLastSurface_2(L, rayData)
    use DATLEN, only: PHASE, WW4, ALENS
    use type_utils, only: real2str, int2str
    use mod_lens_data_manager
    implicit none
    integer :: L
    double precision, intent(inout) :: rayData(1:50,0:499)
    logical :: RV
    integer :: jj
    double precision :: angX, angY, angZ, newX, newY, newZ, dLen, dOPL



    ! Test calcs
    angX = (rayData(4,L))
    angY = (rayData(5,L))
    angZ = (rayData(6,L))


    ! Propagate to final plane
    ! For REFRAY use XOLD, YOLD, ZOLD  Not sure 
    ! why this is the dase.  Think it is a bug.
    newX = rayData(16,L) + angX*ldm%getSurfThi(L)
    newY = rayData(17,L) + angY*ldm%getSurfThi(L)
    newZ = rayData(18,L) + angZ*ldm%getSurfThi(L) 


    
    dLen = DSQRT((angX*ldm%getSurfThi(L))**2+(angY*ldm%getSurfThi(L))**2 + &
    & (angZ*ldm%getSurfThi(L))**2)
    !  dLen =DSQRT( &
    !      ((RAYRAY(3,L)-RAYRAY(18,L))**2)+((RAYRAY(2,L)-RAYRAY(17,L))**2) &
    !     +((RAYRAY(1,L)-RAYRAY(16,L))**2))

      RV = .FALSE.
      if (ldm%getSurfThi(L) < 0 ) RV = .TRUE.
      IF(RV) dLen=-dLen
      IF(DABS(dLen).GE.1.0D10) dLen=0.0D0

      !dOPL = dLen/(sysConfig%getWavelength(INT(WW3))/1E3)
!
       ! Tricky compared to adjustLastSurface
      ! var of interest for wavelength is WW4.
       IF(INT(WW4).GE.1.AND.INT(WW4).LE.5) &
       dOPL=dLen*DABS(ALENS(45+INT(WW4),(L-1)))
       IF(INT(WW4).GE.6.AND.INT(WW4).LE.10) &
       dOPL=dLen*DABS(ALENS(65+INT(WW4),(L-1)))
      IF(.NOT.RV) dOPL=dOPL+PHASE
      IF(RV) dOPL=dOPL-PHASE


    rayData(7,L) = dOPL+rayData(7,L)
    rayData(8,L) = dLen+rayData(8,L)


    !    do jj=1,L
    !      call LogTermFOR("X is "//int2str(jj)// "= "//real2str(rayData(1,jj)))
    !      call LogTermFOR("Y is "//int2str(jj)// "= "//real2str(rayData(2,jj)))
    !      call LogTermFOR("Z is "//int2str(jj)// "= "//real2str(rayData(3,jj)))
    !      call LogTermFOR("OPL "//int2str(jj)// "= "//real2str(rayData(7,jj)))
    !    end do    

end subroutine


end module