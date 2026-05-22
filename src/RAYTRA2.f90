!       THIS IS THE SECOND FILE OF RAYTRACING ROUTINES

! SUB HOE_TRACE.FOR
SUBROUTINE HOE_TRACE
   use DATLEN
   use DATMAI
   IMPLICIT NONE
   INTEGER CURFIG,SOUFIG,REFFIG,WAVE_NUMB
   CHARACTER A*3,AFIG*3
   REAL*8 HOE_L,HOE_M,HOE_N,XO,YO,ZO
   INTEGER CURSURF
   SAVE CURSURF
   LOGICAL CFGQUIET
   COMMON/QUIETCFG/CFGQUIET
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
!       THIS ROUTINE DETERMINES THE DIRECTION COSINES FOR A TYPE 13
!       SPECIAL SURFACE
!     1.FIRST THE CURRENT LENS ARRAYS
!       ARE WRITTEN TO TEMPORARY ARRAY AREAS
!     2.THE RAY IS TRACED IN EACH OF THE TWO CONFIGS SPECIFIED IN THE
!       HOE-R DEFINITION IN RAY ARRAYS DIFFERENT FROM THE REGULAR ARRAYS
!       AND THE RAY DIRECTION COSINES ARE REMEMBERED AT THE FINAL SURFACE
!     3.THE CURRENT LENS ARRAYS ARE RESTORED.
!     4.HOE_DO_IT IS SET TO 3 IN HITSUR AFTER A RETURN IS MADE FROM
!       THIS ROUTINE
!     5.THE REGULAR RAY TRACE PROCEEDS AS USUAL
!     SINCE HOE-R IS ONLY VALID AT CONFIG 1, WHICH IS THE PERMANENT LENS
!     WE DON'T NEED TO SAVE THE CURRENT CONFIG, JUST REMEMBER WHERE THE RAY
!     TRACE SURFACE POINTER IS
   CURFIG=1
!     WHAT IS THE SOURCE POINT CONFIG NUMBER?
   SOUFIG=INT(FTFL01(13,R_I))
!     WHAT IS THE PLAYBACK CONFIG NUMBER?
   REFFIG=INT(FTFL01(14,R_I))
!     SAVE THE RAY TRACE SURFACE POINTER
   CURSURF=R_I
   CFGQUIET=.TRUE.
   XO=FTFL01(3,R_I)
   YO=FTFL01(4,R_I)
   ZO=FTFL01(5,R_I)
   RAYCLEAR=.FALSE.
!     SWITCH TO SOUFIG
   WRITE(A,10) SOUFIG
   READ(A,20) AFIG
10 FORMAT(I3)
20 FORMAT(A3)
   SAVE_KDP(16)=SAVEINPT(16)
   INPUT='CFG '//AFIG
   CALL PROCES
   REST_KDP(16)=RESTINPT(16)
!     TRACE SOUCE RAY
   CALL TRACE_HOERAY(XO,YO,ZO,HOE_L,HOE_M,HOE_N)
   IF(STOPP.EQ.1) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY FAILED IN HOE-R SOURCE CONFIGURATION RAY TRACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=21
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      SAVE_KDP(16)=SAVEINPT(16)
      INPUT='CFG 1'
      CALL PROCES
      REST_KDP(16)=RESTINPT(16)
      CFGQUIET=.FALSE.
      RAYCLEAR=.TRUE.
      CALL MACFAL
      RETURN
   END IF
   L1HOE=HOE_L
   M1HOE=HOE_M
   N1HOE=HOE_N
   XO=FTFL01(8,R_I)
   YO=FTFL01(9,R_I)
   ZO=FTFL01(10,R_I)
!     SWITCH TO REFFIG
   WRITE(A,10) REFFIG
   READ(A,20) AFIG
   SAVE_KDP(16)=SAVEINPT(16)
   INPUT='CFG '//AFIG
   CALL PROCES
   REST_KDP(16)=RESTINPT(16)
!     TRACE REFERENCE RAY
   CALL TRACE_HOERAY(XO,YO,ZO,HOE_L,HOE_M,HOE_N)
   IF(STOPP.EQ.1) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY FAILED IN HOE-R REFERENCE CONFIGURATION RAY TRACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=22
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      SAVE_KDP(16)=SAVEINPT(16)
      INPUT='CFG 1'
      CALL PROCES
      REST_KDP(16)=RESTINPT(16)
      CFGQUIET=.FALSE.
      RAYCLEAR=.TRUE.
      CALL MACFAL
      RETURN
   END IF
   L2HOE=HOE_L
   M2HOE=HOE_M
   N2HOE=HOE_N
!     SWITCH BACK TO ORIGINAL CURFIG
!     SWITCH TO SOUFIG
   SAVE_KDP(16)=SAVEINPT(16)
   INPUT='CFG 1'
   CALL PROCES
   REST_KDP(16)=RESTINPT(16)
   CFGQUIET=.FALSE.
   RAYCLEAR=.TRUE.
   R_I=CURSURF
   RETURN
END
! SUB HIT17.FOR
SUBROUTINE HIT17
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HIT17.FOR. THIS SUBROUTINE IMPLEMENTS
!       TYPE 17 SPECIAL SURFACE RAYTRACING.
!
   REAL*8 SIGNNU,RR_N,RR_Z,&
   &T0,NUSUBV,C5,TESTLEN,NORM,SNINDX,SNIND2 &
   &,MAG,J,ARG,C1,C2,C3,C4 &
   &,FPX,FPY,FPZ,NUSUBS
!
   LOGICAL REFLEC
   LOGICAL NOHITMES
   COMMON/HITMES/NOHITMES
!
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
!
   RR_N=R_N
   RR_Z=R_Z
!
!       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
   ELSE
      T0=R_Z
!       T0/N IS THE DISTANCE ALONG THE RAY FROM
!       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
!       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
!       SYSTEM OF THE CURRENT SURFACE.
!
      IF(DABS(R_N).NE.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      ELSE
         NUSUBV=-T0/R_N
         R_X=R_X+NUSUBV*R_L
         R_Y=R_Y+NUSUBV*R_M
         R_Z=0.0D0
      END IF
   END IF
   REG(40)=REG(9)
   REG(9)=R_X
   REG(10)=R_Y
   REG(11)=0.0D0
   REG(30)=REG(13)
   REG(13)=0.0
   REG(14)=0.0
   REG(15)=1.0
!
!       NOW REFINE TO INTERSECT WITH THE TYPE 17
!
!       SUBROUTINE NR4 INTERSECTS
!
   INR=surf_inr_value(R_I)
   CALL NR4
!
   R_X=REG(9)
   R_Y=REG(10)
   R_Z=REG(11)
   NORM=DSQRT((REG(13)**2)+(REG(14)**2)+(REG(15)**2))
   LN=REG(13)/NORM
   MN=REG(14)/NORM
   NN=REG(15)/NORM
!
   IF(STOPP.EQ.1) RETURN
!
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
   COSI=((R_L*LN)+(R_M*MN)+(R_N*NN))
   IF(COSI.LT.-1.0D0) COSI=-1.0D0
   IF(COSI.GT.+1.0D0) COSI=+1.0D0
!
   IF(.NOT.DUM(R_I)) THEN
      IF(SIGNNU.LT.0.0D0) THEN
!       REFLECTION
         R_L=(R_L-((2.0D0*COSI)*LN))
         R_M=(R_M-((2.0D0*COSI)*MN))
         R_N=(R_N-((2.0D0*COSI)*NN))
         MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
         R_L=R_L/MAG
         R_M=R_M/MAG
         R_N=R_N/MAG
!
         COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
         IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
         IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
      ELSE
!       NOT REFLECTION, PROCEED
         ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
         IF(ARG.LT.0.0D0) THEN
!       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
            IF(MSG) THEN
               CALL RAY_FAILURE(R_I)
               OUTLYNE='TOTAL INTERNAL REFLECTION'
               CALL SHOWIT(1)
            END IF
            RAYCOD(1)=4
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=1
            RETURN
!       NO TIR
         END IF
         IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
         IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
         J=((ldm%getSurfIndex(R_I, INT(WVN))*COSIP)&
         &-(ldm%getSurfIndex(R_I-1, INT(WVN))*COSI))/&
         &(ldm%getSurfIndex(R_I, INT(WVN)))
         R_L=((NUSUBS*R_L)+(J*LN))
         R_M=((NUSUBS*R_M)+(J*MN))
         R_N=((NUSUBS*R_N)+(J*NN))
         MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
         R_L=R_L/MAG
         R_M=R_M/MAG
         R_N=R_N/MAG
!
      END IF
!     DUM
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
!     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
!     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
!     NOT RV SET TO RV
!     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
!     THEN IF RV, THEN SET .NOT.RV AND IF
   TESTLEN=R_Z-RR_Z
!     NOT RV SET TO RV
!     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
!     RAY GETS "REVERSED"
   IF(surf_thickness(R_I).GT.0.0D0.AND.surf_thickness(R_I-1).LT. &
   &0.0D0.AND.DUM(R_I).OR.surf_thickness(R_I).LT.0.0D0.AND.surf_thickness(R_I-1)&
   &.GT.0.0D0.AND.DUM(R_I)) THEN
      IF(RV) THEN
         RV=.FALSE.
      ELSE
         RV=.TRUE.
      END IF
   END IF
!     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
!     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
!     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
!     RAY GETS "REVERSED"
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
      RV=.TRUE.
   ELSE
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
      RV=.FALSE.
      RVSTART=.FALSE.
   END IF
!       RAY TRACE AT SURFACE R_I COMPLETE.
   IF(R_I.EQ.NEWIMG) THEN
      R_L=OLDL
      R_M=OLDM
      R_N=OLDN
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
   STOPP=0
   RETURN
END
! SUB HITSUR.FOR
SUBROUTINE HITSUR
!
   use DATLEN
   use DATMAI
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITSUR.FOR. THIS SUBROUTINE IMPLEMENTS
!       SURFACE INTERSECTIONS IN RAYTRACING. IT IS THE MAIN TRACING
!       ROUTINE. INTERSECTION TO SPECIAL SURFACES IS DONE BY
!       SUBROUTINE XYZSP.FOR FOR IF SPECIAL SURFACES ARE
!       PRESENT AND SET TO "ON" AND IF THEY ARE OF A RECOGNIZED TYPE.
!
   INTEGER N_X,N_Y
!
   REAL*8 OR_N,OR_Z,AX,AY,AZ
!
   LOGICAL ERROR_NSS
!
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
!
!
   PHASE=0.0D0
!
   IF(surf_refractive_index(R_I, 1).EQ.surf_refractive_index(R_I-1, 1).AND.surf_refractive_index(R_I, 2).EQ.&
   &surf_refractive_index(R_I-1, 2).AND.surf_refractive_index(R_I, 3).EQ.surf_refractive_index(R_I-1, 3).AND.&
   &surf_refractive_index(R_I, 4).EQ.surf_refractive_index(R_I-1, 4).AND.surf_refractive_index(R_I, 5).EQ.&
   &surf_refractive_index(R_I-1, 5).AND.&
   &surf_refractive_index(R_I, 6).EQ.surf_refractive_index(R_I-1, 6).AND.surf_refractive_index(R_I, 7).EQ.&
   &surf_refractive_index(R_I-1, 7).AND.surf_refractive_index(R_I, 8).EQ.surf_refractive_index(R_I-1, 8).AND.&
   &surf_refractive_index(R_I, 9).EQ.surf_refractive_index(R_I-1, 9).AND.surf_refractive_index(R_I, 10).EQ.&
   &surf_refractive_index(R_I-1, 10)) THEN
!       SURFACE IS A DUMMY
      IF(surf_dummy_val(R_I) == 0) DUM(R_I)=.TRUE.
      IF(surf_dummy_val(R_I) == 1) DUM(R_I)=.FALSE.
   ELSE
      DUM(R_I)=.FALSE.
   END IF
   IF(.NOT.DUMMMY(R_I)) DUM(R_I)=.FALSE.
!       INTERS KEEPS TRACK OF SINGLE OR MULTIPLE SURFACE
!       INTERSECTIONS
   INTERS=0
   SEC=0
!
   OLDL=R_L
   OLDM=R_M
   OLDN=R_N
!       SET STOPP TO 0
   STOPP=0
!       IF STOPP GETS SET TO 1, THE REFRAY TRACE IS STOPPED
!
!**********************************************************************
!
!                               NOW
!
!                   INTERSECT USER-DEFINED TYPE 17 SURFACES
!
   IF(surf_special_type(R_I) == 17.AND.surf_paraxial_val(R_I) == 0) THEN
      CALL HIT17
      RETURN
   END IF
!
!**********************************************************************
!
!                               NOW
!
!                   INTERSECT FRESNEL-1 SURFACES
!
   IF(surf_special_type(R_I) == 16.AND.surf_curvature(R_I).EQ.0.0D0.AND.&
   &surf_toric_flag(R_I) == 0.OR.&
   &surf_special_type(R_I) == 16.AND.surf_toric_flag(R_I) /= 0.AND.&
   &surf_toric_curvature(R_I).EQ.0.0D0) THEN
      CALL HITFRZFL
      RETURN
   END IF
   IF(surf_special_type(R_I) == 16.AND.surf_curvature(R_I).NE.0.0D0 &
   &.AND.surf_toric_flag(R_I) == 0.OR.&
   &surf_special_type(R_I) == 16.AND.surf_toric_flag(R_I) /= 0.AND.&
   &surf_toric_curvature(R_I).NE.0.0D0) THEN
      CALL HITFRZCV
      RETURN
   END IF
!
!**********************************************************************
!
!                               NOW
!
!                   INTERSECT GRAZING INCIDENCE SURFACES
!
   IF(surf_special_type(R_I) == 18) THEN
      CALL HITGRAZ
      RETURN
   END IF
!
!**********************************************************************
!                               NOW
!
!                   INTERSECT VARIOUS SURFACE TYPES
!
!**********************************************************************
!       PARAXIAL SURFACE
!
   IF(surf_paraxial_val(R_I) == 1) THEN
      CALL HITPARAX(OR_N,OR_Z)
      IF(STOPP.EQ.0) THEN
         IF(surf_special_type(R_I) /= 13.OR.surf_special_type(R_I) == 13 &
         &.AND.F12.NE.1) THEN
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
         END IF
         IF(surf_special_type(R_I) == 13.AND.F12.EQ.1) THEN
!     SET THE TARGET POSITIONS ON THE HOE AND RETURN
            XHOE=R_X
            YHOE=R_Y
            ZHOE=R_Z
            HOE_WAV_NUM=INT(FTFL01(2,R_I))
            HOE_DO_IT=0
            CALL HOE_TRACE
            IF(STOPP.EQ.1) RETURN
            HOE_DO_IT=3
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
            RETURN
         END IF
      END IF
      RETURN
!       SURFACE NOT PARAXIAL
   END IF
!
!**********************************************************************
!       SURFACE R_I IS PLANO AND MAY CONTAIN 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
!       ASPHERIC TERMS AND SPECIAL STUFF
!
   IF(surf_curvature(R_I).EQ.0.0D0.AND.surf_toric_flag(R_I)&
   &.EQ.0.0D0) THEN
!
      IF(surf_array_parity(R_I) /= 0) THEN
!
!       CODE GOES HERE FOR ARRAY SURFACE IN SIDE
!
         A_R_X=R_X
         A_R_Y=R_Y
         A_R_Z=R_Z
         A_R_L=R_L
         A_R_M=R_M
         A_R_N=R_N
         CALL ARRAYIN_FIX(N_X,N_Y)
      END IF
      CALL HITFLA(OR_N,OR_Z)
      IF(STOPP.EQ.0) THEN
         IF(surf_special_type(R_I) /= 13.OR.surf_special_type(R_I) == 13 &
         &.AND.F12.NE.1) THEN
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
!
!       CODE GOES HERE FOR ARRAY SURFACE OUT SIDE
!
            IF(surf_array_parity(R_I) /= 0) THEN
               CALL ARRAYOUT_FIX(N_X,N_Y)
            END IF
         END IF
         IF(surf_special_type(R_I) == 13.AND.F12.EQ.1) THEN
!     SET THE TARGET POSITIONS ON THE HOE AND RETURN
            XHOE=R_X
            YHOE=R_Y
            ZHOE=R_Z
            HOE_WAV_NUM=INT(FTFL01(2,R_I))
            HOE_DO_IT=0
            CALL HOE_TRACE
            IF(STOPP.EQ.1) RETURN
            HOE_DO_IT=3
            RETURN
         END IF
      END IF
      RETURN
!       SURFACE NOT PLANO WITH ASPHERICS
   END IF
!
!**********************************************************************
!       SURFACE R_I IS SPHERICAL OR CONIC AND MAY HAVE ROTATIONALLY
!       SYMMETRIC ASPHERIC DEFORMATION COEFFICIENTS
!
   XOLD=R_X
   YOLD=R_Y
   ZOLD=R_Z
   IF(surf_curvature(R_I).NE.0.0D0.AND.surf_toric_flag(R_I) == 0) THEN
      IF(surf_array_parity(R_I) /= 0) THEN
!
!       CODE GOES HERE FOR ARRAY SURFACE IN SIDE
!
         A_R_X=R_X
         A_R_Y=R_Y
         A_R_Z=R_Z
         A_R_L=R_L
         A_R_M=R_M
         A_R_N=R_N
         CALL ARRAYIN_FIX(N_X,N_Y)
      END IF
      CALL HITASP(OR_N,OR_Z)
      IF(STOPP.EQ.0) THEN
         IF(surf_special_type(R_I) /= 13.OR.surf_special_type(R_I) == 13 &
         &.AND.F12.NE.1) THEN
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
!
!       CODE GOES HERE FOR ARRAY SURFACE OUT SIDE
!
            IF(surf_array_parity(R_I) /= 0) THEN
               CALL ARRAYOUT_FIX(N_X,N_Y)
            END IF
         END IF
         IF(surf_special_type(R_I) == 13.AND.F12.EQ.1) THEN
!     SET THE TARGET POSITIONS ON THE HOE AND RETURN
            XHOE=R_X
            YHOE=R_Y
            ZHOE=R_Z
            HOE_WAV_NUM=INT(FTFL01(2,R_I))
            HOE_DO_IT=0
            CALL HOE_TRACE
            IF(STOPP.EQ.1) RETURN
            HOE_DO_IT=3
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
            RETURN
         END IF
      END IF
      RETURN
!       SURFACE NOT SPHERICAL OR CONIC, MAY BE ROTATIONALLY SYMMETRIC
!       ASPHERIC OR A SPECIAL SURFACE SHAPE
   END IF
!**********************************************************************
!       SURFACE R_I IS TOROIDAL AND MAY BE CONIC AND ANAMORPHIC
!       ASPHERIC
!
   IF(surf_toric_flag(R_I) /= 0) THEN
!
      IF(surf_array_parity(R_I) /= 0) THEN
!
!       CODE GOES HERE FOR ARRAY SURFACE IN SIDE
!
         A_R_X=R_X
         A_R_Y=R_Y
         A_R_Z=R_Z
         A_R_L=R_L
         A_R_M=R_M
         A_R_N=R_N
         CALL ARRAYIN_FIX(N_X,N_Y)
      END IF
      CALL HITANA(OR_N,OR_Z)
      IF(STOPP.EQ.0) THEN
         IF(surf_special_type(R_I) /= 13.OR.surf_special_type(R_I) == 13 &
         &.AND.F12.NE.1) THEN
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
!
!       CODE GOES HERE FOR ARRAY SURFACE OUT SIDE
!
            IF(surf_array_parity(R_I) /= 0) THEN
               CALL ARRAYOUT_FIX(N_X,N_Y)
            END IF
         END IF
         IF(surf_special_type(R_I) == 13.AND.F12.EQ.1) THEN
!     SET THE TARGET POSITIONS ON THE HOE AND RETURN
            XHOE=R_X
            YHOE=R_Y
            ZHOE=R_Z
            HOE_WAV_NUM=INT(FTFL01(2,R_I))
            HOE_DO_IT=0
            CALL HOE_TRACE
            IF(STOPP.EQ.1) RETURN
            HOE_DO_IT=3
            CALL INTERACK(OR_N,OR_Z)
            HOE_DO_IT=0
            RETURN
         END IF
      END IF
      RETURN
!       SURFACE NOT ANAMORPHIC
   END IF
   OUTLYNE='SURFACE TYPE NOT YET IN PROGRAM'
   CALL SHOWIT(1)
   RETURN
END
!**********************************************************************
SUBROUTINE APLANA(I,WWWW1,WWWW2,WWWWW1,WWWWW2)
   use DATLEN
   use DATMAI
   use mod_surface
   IMPLICIT NONE
   REAL*8 WX,WY,PARTX,PARTY,WWWW1,WWWW2,RD,HGT,FULL &
   &,WWWWW1,WWWWW2
   INTEGER I
   IF(WWWW1.GE.0.0D0) WY= 1.0D0
   IF(WWWW1.LT.0.0D0) WY=-1.0D0
   IF(WWWW2.GE.0.0D0) WX= 1.0D0
   IF(WWWW2.LT.0.0D0) WX=-1.0D0
!     DETERMINE THE NEW WW1 AND WW2 VALUES
   IF(surf_array_parity(I) == 0) THEN
      HGT=surf_clap_type(I)
   ELSE
      HGT=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
   END IF
   RD=1.0D0/surf_curvature(I)
!     DETERMINE THE ANGLE OF THE FULL RAY
   FULL=DASIN(HGT/RD)
   PARTX=FULL*DABS(WWWW2)
   PARTY=FULL*DABS(WWWW1)
   WWWWW1=DSIN(PARTY)*RD*WY
   WWWWW2=DSIN(PARTX)*RD*WX
   RETURN
END
! SUB HITASP.FOR
SUBROUTINE HITASP(OR_N,OR_Z)
   USE GLOBALS
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   use mod_surface_type, only: surf_ray_data
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITASP.FOR. THIS SUBROUTINE IMPLEMENTS
!       ROTATIONALLY SYMMETRIC ASPHERIC SURFACE INTERSECTIONS IN RAYTRACING.
!       INTERSECTION TO SPECIAL SURFACES IS DONE BY
!       SUBROUTINE XYZSP.FOR FOR IF SPECIAL SURFACES ARE
!       PRESENT AND SET TO "ON" AND IF THEY ARE OF A RECOGNIZED TYPE.
!
   type(surf_ray_data) :: typed_ray
!
   REAL*8 A,B,C,QQ,SIGNNU,CV,CC,OR_Z,OR_N,&
   &TEST1,TEST2,NUSUBS,RR_N,TESTLEN,RR_Z &
   &,Q,SIGNB,MAG,J,ARG,C1,C2,C3,C4,ZTEST,SNIND2 &
   &,FPX,FPY,FPZ,C5,C6,HV0,HV1,HV2,ZMIN,ZMAX,&
   &X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2 &
   &,SNINDX
!
   INTEGER ZPMIN,ZPMAX,ISURF
!
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC,GERROR,ERR
!
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   !call logger%logTextWithNum("HitAsp Started for surf ", R_I)
   PHASE=0.0D0
   HV0=0.0D0
   HV1=0.0D0
   HV2=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
!
!       SURFACE IS CONIC
   CV=surf_curvature(R_I)
   CC=surf_conic(R_I)
   IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
   IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
   IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
!
   RR_N=R_N
   RR_Z=R_Z
   OR_N=RR_N
   OR_Z=RR_Z
!
!       SURFACE IS ROTATIONALLY SYMMETRIC ASPHERIC WITH NON-ZERO
!       CURVATURE
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
!       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE CONIC
   END IF
!
!     TYPED SURFACE DISPATCH (Stage 4):
!     For simple sphere/asphere (no special type, array, or paraxial),
!     delegate to the polymorphic surface object instead of the quadratic + NR2 path.
   if (surf_special_type(R_I) == 0 .and. surf_default_flag(R_I) == 0 .and. &
       surf_array_parity(R_I) == 0 .and. surf_paraxial_val(R_I) == 0) then
     if (allocated(ldm%surfaces)) then
       if (R_I >= lbound(ldm%surfaces,1)) then
         if (R_I <= ubound(ldm%surfaces,1)) then
           if (allocated(ldm%surfaces(R_I)%s)) then
             typed_ray%x = R_X; typed_ray%y = R_Y; typed_ray%z = R_Z
             typed_ray%l = R_L; typed_ray%m = R_M; typed_ray%n = R_N
             call ldm%surfaces(R_I)%s%intersect(typed_ray, SURTOL)
             R_X = typed_ray%x; R_Y = typed_ray%y; R_Z = typed_ray%z
             LN = typed_ray%ln; MN = typed_ray%mn; NN = typed_ray%nn
             return
           end if
         end if
       end if
     end if
   end if
!
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
!       NOW INTERSECT THE SPHERE OR CONIC.
!       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
!
   A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*&
   &(CC+1.0D0))))
   B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-&
   &((CV*R_Z*R_N)&
   &*(CC+1.0D0))
   B=2.0D0*B
   C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)*&
   &(CC+1.0D0)))
   C=(C+(2.0D0*R_Z))
   IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
   IF(B.EQ.0.0D0) SIGNB=1.0D0
   IF(A.EQ.0.0D0) THEN
      HV0=-(C/B)
      INTERS=1
   ELSE
!       A NOT ZERO
      ARG=((B**2)-(4.0D0*A*C))
      IF(ARG.LT.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
!       ARG NOT ZERO
      END IF
      Q=(-0.5D0*(B+(SIGNB*(DSQRT(ARG)))))
      HV1=C/Q
      HV2=Q/A
      INTERS=2
   END IF
   IF(INTERS.EQ.1) THEN
!       CASE OF ONLY ONE INTERSECTION POINT
      X1=(R_X+(HV0*R_L))
      Y1=(R_Y+(HV0*R_M))
      Z1=(R_Z+(HV0*R_N))
   END IF
   IF(INTERS.EQ.2) THEN
!       CASE OF TWO INTERSECTION POINTS
      X1=(R_X+(HV1*R_L))
      Y1=(R_Y+(HV1*R_M))
      Z1=(R_Z+(HV1*R_N))
      X2=(R_X+(HV2*R_L))
      Y2=(R_Y+(HV2*R_M))
      Z2=(R_Z+(HV2*R_N))
   END IF
!     FIX FOR HYPERBOLAS
   IF(CC.LT.-1.0D0) THEN
      ZMIN=Z2
      ZMAX=Z2
      ZPMIN=2
      ZPMAX=2
      IF(Z1.LT.Z2) ZMIN=Z1
      IF(Z1.LT.Z2) ZPMIN=1
      IF(Z1.GT.Z2) ZMAX=Z1
      IF(Z1.GT.Z2) ZPMAX=1
!     HYPERBOLA
!     CV POSITIVE
      IF(CV.GT.0.0D0) THEN
         IF(ZMIN.LT.0.0D0) THEN
!     ONLY ONE INTERSECTION POINT
            INTERS=1
            IF(ZPMIN.EQ.1) THEN
               X1=X2
               Y1=Y2
               Z1=Z2
!     LEAVE X1,Y1,Z1 ALONE
            END IF
!     TWO POINTS
         END IF
      ELSE
!    CV NEGATIVE
         IF(ZMAX.GT.0.0D0) THEN
!     ONLY ONE INTERSECTION POINT
            INTERS=1
            IF(ZPMAX.EQ.1) THEN
               X1=X2
               Y1=Y2
               Z1=Z2
!     LEAVE X1,Y1,Z1 ALONE
            END IF
!     THERE ARE TWO POINTS
         END IF
      END IF
!     NOT HYPERBOLA
   END IF
!
!
!       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
   QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X1**2)+(Y1**2)))
   IF(QQ.LT.0.0D0) THEN
      QQ=-QQ
!       QQ NOT NEGATIVE
   END IF
   QQ=DSQRT(QQ)
   QQ=1.0D0+QQ
   IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      STOPP=1
      RETURN
   END IF
   FPX=-((2.0D0*CV*X1*QQ*(QQ-1.0D0))+&
   &((CV**3)*X1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
   FPY=-((2.0D0*CV*Y1*QQ*(QQ-1.0D0))+&
   &((CV**3)*Y1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
   IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
      IF(CC.LT.-1.0D0) THEN
         FPZ=1.0D0
      ELSE
         IF(Z1.GT.ZTEST) FPZ=-1.0D0
         IF(Z1.LT.ZTEST) FPZ=1.0D0
         IF(Z1.EQ.ZTEST) FPZ=0.0D0
      END IF
   ELSE
!     CV NEG
!     USE OUTWARD NORMAL
      IF(CC.LT.-1.0D0) THEN
         FPZ=1.0D0
      ELSE
         IF(Z1.LT.ZTEST) FPZ=-1.0D0
         IF(Z1.GT.ZTEST) FPZ=1.0D0
         IF(Z1.EQ.ZTEST) FPZ=0.0D0
      END IF
   END IF
!
   MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
   LN1=FPX/MAG
   MN1=FPY/MAG
   NN1=FPZ/MAG
!       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
   IF(INTERS.EQ.2) THEN
!       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
      QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X2**2)+(Y2**2)))
      IF(QQ.LT.0.0D0) THEN
         QQ=-QQ
!       QQ NOT NEG
      END IF
      QQ=DSQRT(QQ)
      QQ=1.0D0+QQ
      IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      END IF
      FPX=-((2.0D0*CV*X2*QQ*(QQ-1.0D0))+&
      &((CV**3)*X2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
      FPY=-((2.0D0*CV*Y2*QQ*(QQ-1.0D0))+&
      &((CV**3)*Y2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
      IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
         IF(CC.LT.-1.0D0) THEN
            FPZ=1.0D0
         ELSE
            IF(Z2.GT.ZTEST) FPZ=-1.0D0
            IF(Z2.LT.ZTEST) FPZ=1.0D0
            IF(Z2.EQ.ZTEST) FPZ=0.0D0
         END IF
      ELSE
!     CV NEG
!     USE OUTWARD NORMAL
         IF(CC.LT.-1.0D0) THEN
            FPZ=1.0D0
         ELSE
            IF(Z2.LT.ZTEST) FPZ=-1.0D0
            IF(Z2.GT.ZTEST) FPZ=1.0D0
            IF(Z2.EQ.ZTEST) FPZ=0.0D0
         END IF
      END IF
!
      MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
      LN2=FPX/MAG
      MN2=FPY/MAG
      NN2=FPZ/MAG
!       NOT TWO INTERSECTION POINTS
   END IF
!       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
!       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
!
   IF(INTERS.EQ.2) THEN
!       THERE ARE TWO INTERSECTION POINTS
      TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
      TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
      IF(POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     POSRAY
      END IF
      IF(.NOT.POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     POSRAY
      END IF
   ELSE
!       NOT TWO INTERSECTIONS
!       JUST ONE INTERSECTION
      R_X=X1
      R_Y=Y1
      R_Z=Z1
      LN=LN1
      MN=MN1
      NN=NN1
   END IF
200 CONTINUE
!
!       NOW WE KNOW THE INTERSECTION POINT IF THE ASPHERIC TERMS
!       ARE ALL ZERO. WE ALSO HAVE THE SURFACE NORMALS
!
!       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED ASPHERIC
!       USING A CALL TO SUBROUTINE NR2. THIS ALSO GIVE THE DIRECTION
!       COSINES OF THE SURFACE NORMAL TO THE ASPHERIC.
!       NR2 ALSO DEALS WITH SPECIAL SURFACE TYPES:
!
   INR=surf_inr_value(R_I)
   IF(surf_is_asphere(R_I).OR.surf_special_type(R_I) > 0.AND.&
   &surf_special_type(R_I) /= 19.AND.&
   &surf_special_type(R_I) /= 20.OR.surf_default_flag(R_I) == 1) THEN
      ERR=.FALSE.
      !call logger%logTextWithNum("Call NR2 for surface ", R_I)
      CALL NR2(ERR)
      IF(ERR) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      END IF
   END IF
   RETURN
END
! SUB GETZEE1.FOR
SUBROUTINE GETZEE1
   use ieee_arithmetic
!
   use DATLEN
   use DATMAI
   use mod_surface
   IMPLICIT NONE
!
!     CALCULATES BEST INTERSECTION POINT TO FIRST SURFACE
!     WHEN SCY FANG OR SCX FANG IS SPECIFIED. RECOGNIZES UP TO
!     CONIC PROFILE ON SURFACE 1
!     USED FOR BETTER RAY AIMING
!
   REAL*8 A,B,C,CV,CC,ZTEST,MAG,&
   &Q,SIGNB,ARG,HV0,HV1,HV2,XA,YA,ZA
!
   INTEGER JIM
!
   LOGICAL ZEEERR, DEBUGZEE
   COMMON/ERRZEE/ZEEERR

   COMMON DEBUGZEE
!
   ZEEERR=.FALSE.

   HV0=0.0D0
   HV1=0.0D0
   HV2=0.0D0

   !PRINT *, "DEBUGZEE IS ", DEBUGZEE
!
!       SURFACE IS CONIC OR SPHERE
   CV=surf_curvature(1)
   CC=surf_conic(1)
   IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
   IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
   IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)

   if (DEBUGZEE) THEN
      PRINT *, "GETZEE1 1121 YC IS ", YC

      if (ieee_is_nan(YC)) THEN
         PRINT *, "YC Nan Input to GETZEE1"
         DEBUGZEE = .FALSE.
      END IF
   END IF
!     COMPUTE DIR COS DIRECTLY FROM POSITIONS IN ALL CASES
!
   XA=XC
   YA=YC
   ZA=ZC
!     THESE ARE IN LOCAL COORDINATE SYSTEM OF NEWOBJ+1
!     CONVERT TO NEWOBJ COORD SYSTEM
   R_TX=XA
   R_TY=YA
   R_TZ=ZA
   CALL BAKONE
   XA=R_TX
   YA=R_TY
   ZA=R_TZ
   MAG=DSQRT(((XSTRT-XA)**2)+((YSTRT-YA)**2)&
   &+((ZSTRT-ZA)**2))
1235 FORMAT(G15.7, G15.7, G15.7, G15.7,G15.7, G15.7)
   !PRINT *, "MAG IN GETZEE1 is ", MAG
   !PRINT 1235, XA, XSTRT, YSTRT, YA, ZA, ZSTRT

   R_L=(XA-XSTRT)/MAG
   R_M=(YA-YSTRT)/MAG
   R_N=(ZA-ZSTRT)/MAG
!     NOW PUT DIR COS INTO NEWOBJ+1 COORD SYSTEM
   R_TX=R_L
   R_TY=R_M
   R_TZ=R_N
   CALL FORONEL
!
   XA=XC
   YA=YC
   ZA=ZC
   IF (DEBUGZEE) THEN
      PRINT *, "GETZEE1 1155 YC IS ", YC
      IF (ieee_is_nan(YC)) THEN
         PRINT *, "GETZEE1 1152 YC Nan"
         DEBUGZEE = .FALSE.
      END IF
   END IF
   DO JIM=1,2
      IF(JIM.EQ.1) THEN
         R_L=0.0D0
         R_M=0.0D0
         R_N=1.0D0
      ELSE
         R_L=R_TX
         R_M=R_TY
         R_N=R_TZ
      END IF
!
      R_X=XA
      R_Y=YA
      R_Z=ZA

      !IF (ieee_is_nan(R_Y)) THEN
      !  PRINT *, "R_Y is nan 1158. YA=", YA
      !END IF


!
!       NOW INTERSECT THE SPHERE OR CONIC AT NEWOBJ+1
!       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
!
      A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*&
      &(CC+1.0D0))))
      B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-&
      &((CV*R_Z*R_N)&
      &*(CC+1.0D0))
      B=2.0D0*B
      C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)*&
      &(CC+1.0D0)))
      C=(C+(2.0D0*R_Z))
      IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
      IF(B.EQ.0.0D0) SIGNB=1.0D0
      IF(A.EQ.0.0D0) THEN
         HV0=-(C/B)
         INTERS=1
      ELSE
!       A NOT ZERO
         ARG=((B**2)-(4.0D0*A*C))
         IF(ARG.LT.0.0D0) THEN
            ZEEERR=.TRUE.
            RETURN
!       ARG NOT ZERO
         END IF
         Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
         HV1=C/Q
         HV2=Q/A
         INTERS=2
      END IF
      IF(INTERS.EQ.1) THEN
!       CASE OF ONLY ONE INTERSECTION POINT
         XC=(R_X+(HV0*R_L))
         YC=(R_Y+(HV0*R_M))
         ZC=(R_Z+(HV0*R_N))
      END IF
      IF (DEBUGZEE) THEN
         PRINT *, "GETZEE1 1219 YC IS ", YC
         IF (ieee_is_nan(YC)) THEN
            PRINT *, "GETZEE1 1209 YC Nan"
            DEBUGZEE = .FALSE.
         END IF
      END IF
      !IF (ieee_is_nan(YC)) THEN
      !  PRINT *, "YC is nan.  HV0 = ",HV0, "R_Y=",R_Y
      !END IF

      IF(INTERS.EQ.2) THEN
!       CASE OF TWO INTERSECTION POINTS
         IF(DABS(HV1).LE.DABS(HV2)) THEN
            XC=(R_X+(HV1*R_L))
            YC=(R_Y+(HV1*R_M))
            ZC=(R_Z+(HV1*R_N))
         ELSE
            XC=(R_X+(HV2*R_L))
            YC=(R_Y+(HV2*R_M))
            ZC=(R_Z+(HV2*R_N))
         END IF
         IF (DEBUGZEE) THEN
            IF (ieee_is_nan(YC)) THEN
               PRINT *, "GETZEE1 1226 YC Nan"
               PRINT *, "R_Y is ", R_Y
               PRINT *, "HV1 = ", HV1
               PRINT *, "HV2 = ", HV2
               PRINT *, "R_M = ", R_M
               PRINT *, "R_X ", R_X, "R_Y ", R_Y, "R_Z ", R_Z
               PRINT *, "R_M ", R_M, "R_M ", R_M, "R_N ", R_N
               PRINT *, "MAG = ", MAG
               PRINT 1235, XA, XSTRT, YA,YSTRT, ZA, ZSTRT
               !R_L=(XA-XSTRT)/MAG
               !R_M=(YA-YSTRT)/MAG
               !R_N=(ZA-ZSTRT)/MAG

               DEBUGZEE = .FALSE.
            END IF
         END IF
         !IF (ieee_is_nan(YC)) THEN
         !  PRINT *, "YC is nan. R_Y=", R_Y, "HV1=",HV1
         !END IF
         if (DEBUGZEE) THEN
            PRINT *, "GETZEE1 1249 YC IS ", YC
            IF (ieee_is_nan(YC)) THEN
               PRINT *, " AT END OF GETZEE1 YC Nan"
               DEBUGZEE = .FALSE.
            END IF
         end if
      END IF
!     COORDINATES ARE IN COORDINATE SYSTEM OF THE NEWOBJ+1 SURFACE
   END DO

   RETURN

END
! SUB HITFLA.FOR
SUBROUTINE HITFLA(OR_N,OR_Z)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITFLA.FOR. THIS SUBROUTINE IMPLEMENTS
!       FLAT ASPHERIC SURFACE INTERSECTIONS IN RAYTRACING.
!       INTERSECTION TO SPECIAL SURFACES IS DONE BY
!       SUBROUTINE XYZSP.FOR FOR IF SPECIAL SURFACES ARE
!       PRESENT AND SET TO "ON" AND IF THEY ARE OF A RECOGNIZED TYPE.
!
   REAL*8 SIGNNU,RR_N,RR_Z,OR_N,OR_Z &
   &,T0,NUSUBV,C5,TESTLEN,SNINDX,SNIND2 &
   &,MAG,J,ARG,C1,C2,C3,C4 &
   &,FPX,FPY,FPZ,NUSUBS
!
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC,GERROR,ERR
!
   INTEGER SPDCD1,SPDCD2,ISURF
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
   RR_N=R_N
   RR_Z=R_Z
   OR_N=RR_N
   OR_Z=RR_Z
   LN=0.0D0
   MN=0.0D0
   NN=1.0D0
!
!       SURFACE R_I IS PLANO AND CONTAINS 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
!       ASPHERIC TERMS.
!
!       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
   ELSE
      T0=R_Z
!       T0/N IS THE DISTANCE ALONG THE RAY FROM
!       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
!       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
!       SYSTEM OF THE CURRENT SURFACE.
!
      IF(((R_L*LN)+(R_M*MN)+(R_N*NN)).EQ.0.0D0) THEN
         IF(DUM(R_I)) THEN
            R_Z=0.0D0
!       SURFACE IS A FLAT DUMMY SURFACE
!       ASSIGN SOME VALUES TO X,Y AND Z AND RETURN
!       THE X,Y AND Z VALUES DO NOT CHANGE. THEY REMAIN THE COORDINATES
!       OF THE RAY AT THE PREVIOUS SURFACE IN THE COORDINATE SYSTEM OF
!       THE CURRENT SURFACE
            IF(R_L.EQ.0.0D0) THEN
!       RAY IS VERTICAL
               R_Y=0.0D0
            END IF
            IF(R_M.EQ.0.0D0) THEN
!       RAY IS HORIZONTAL
               R_X=0.0D0
            END IF
            IF(R_M.NE.0.0D0.AND.R_L.NE.0.0D0) THEN
!       RAY IS GENERALLY IN XY PLANE
!       GO TO A NEW R_X=0
               R_Y=(-(R_M/R_L)*R_X)+R_Y
               R_X=0.0D0
            END IF
            COSI=(R_L*LN)+(R_M*MN)+(R_N*NN)
            IF(COSI.LT.-1.0D0) COSI=-1.0D0
            IF(COSI.GT.+1.0D0) COSI=+1.0D0
            COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
            IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
            IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
            STOPP=0
            RAYCOD(1)=0
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            RETURN
!       RAY DIRECTION COSINES REMAIN THE SAME
         ELSE
!       NOT A DUMMY SURFACE, RAY FAILED
            IF(MSG) THEN
               CALL RAY_FAILURE(R_I)
               OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
               CALL SHOWIT(1)
            END IF
            RAYCOD(1)=1
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=1
            RETURN
         END IF
      ELSE
         NUSUBV=-T0/R_N
         R_X=R_X+NUSUBV*R_L
         R_Y=R_Y+NUSUBV*R_M
         R_Z=0.0D0
      END IF
   END IF
!
!       NOW REFINE TO INTERSECT WITH THE ASPHERIC
!
!       SUBROUTINE NR1 INTERSECTS
!       A PLANO WITH ASPERIC TERMS GIVEN THE INTERSECTION
!       WITH THE SIMPLE PLANO AS A STARTING POINT. IT
!       ALSO TAKES CARE OF SPECIAL SURFACES OF TYPES:
!
   INR=surf_inr_value(R_I)
   IF(surf_is_asphere(R_I).OR.surf_special_type(R_I) > 0.AND.&
   &surf_special_type(R_I) /= 19.AND.&
   &surf_special_type(R_I) /= 20 &
   &.OR.surf_default_flag(R_I) == 1) THEN
      ERR=.FALSE.
      CALL NR1(ERR)
      IF(ERR) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      END IF
   END IF
   RETURN
END
! SUB DPHASE.FOR
SUBROUTINE DPHASE
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!     THIS ADDS PHASE FOR GRATINGS AND HOES
!
   REAL*8 T1,CO1,CO2
!
!
!     INTERSECTION OF NON-DIFFRACTED RAY PLANE WITH SURFACE NORMAL
   T1=((((R_L0*(R_X))&
   &+(R_M0*(R_Y))&
   &+(R_N0*(R_Z)))&
   &-((R_L*(R_X))&
   &+(R_M*(R_Y))&
   &+(R_N*(R_Z)))))
   PHASE=PHASE-T1

   RETURN
END
! SUB HITPARAX.FOR
SUBROUTINE HITPARAX(OR_N,OR_Z)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITPARAX.FOR. THIS SUBROUTINE IMPLEMENTS
!       INTERSECTIONS TO PARAXIAL SURFACES
!
   REAL*8 SIGNNU,RR_N,RR_Z,OR_N,OR_Z &
   &,T0,NUSUBV,C5,TESTLEN,SNINDX,SNIND2 &
   &,MAG,J,ARG,C1,C2,C3,C4 &
   &,FPX,FPY,FPZ,NUSUBS
!
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC,GERROR
!
   INTEGER SPDCD1,SPDCD2,ISURF
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
   RR_N=R_N
   RR_Z=R_Z
   OR_N=RR_N
   OR_Z=RR_Z
   LN=0.0D0
   MN=0.0D0
   NN=1.0D0
!
!       SURFACE R_I IS PLANO AND CONTAINS 2ND, 4TH, 6TH, 8TH AND 10TH ORDER
!       ASPHERIC TERMS.
!
!       FIRST CALCULATE THE INTERSECTION TO THE SIMPLE PLANE
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
   ELSE
      T0=R_Z
!       T0/N IS THE DISTANCE ALONG THE RAY FROM
!       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
!       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
!       SYSTEM OF THE CURRENT SURFACE.
!
      IF(DABS(R_N).NE.0.0D0) THEN
         IF(DUM(R_I)) THEN
            R_Z=0.0D0
!       SURFACE IS A FLAT DUMMY SURFACE
!       ASSIGN SOME VALUES TO X,Y AND Z AND RETURN
!       THE X,Y AND Z VALUES DO NOT CHANGE. THEY REMAIN THE COORDINATES
!       OF THE RAY AT THE PREVIOUS SURFACE IN THE COORDINATE SYSTEM OF
!       THE CURRENT SURFACE
            IF(R_L.EQ.0.0D0) THEN
!       RAY IS VERTICAL
               R_Y=0.0D0
            END IF
            IF(R_M.EQ.0.0D0) THEN
!       RAY IS HORIZONTAL
               R_X=0.0D0
            END IF
            IF(R_M.NE.0.0D0.AND.R_L.NE.0.0D0) THEN
!       RAY IS GENERALLY IN XY PLANE
!       GO TO A NEW R_X=0
               R_Y=(-(R_M/R_L)*R_X)+R_Y
               R_X=0.0D0
            END IF
            COSI=(R_L*LN)+(R_M*MN)+(R_N*NN)
            IF(COSI.LT.-1.0D0) COSI=-1.0D0
            IF(COSI.GT.+1.0D0) COSI=+1.0D0
            COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
            IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
            IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
            STOPP=0
            RAYCOD(1)=0
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            RETURN
!       RAY DIRECTION COSINES REMAIN THE SAME
         ELSE
!       NOT A DUMMY SURFACE, RAY FAILED
            IF(MSG) THEN
               CALL RAY_FAILURE(R_I)
               OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
               CALL SHOWIT(1)
            END IF
            RAYCOD(1)=1
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=1
            RETURN
         END IF
      ELSE
         NUSUBV=-T0/R_N
         R_X=R_X+NUSUBV*R_L
         R_Y=R_Y+NUSUBV*R_M
         R_Z=0.0D0
      END IF
   END IF
!
!       NOW REFINE TO INTERSECT WITH THE ASPHERIC
!
!       SUBROUTINE NRPARAX COMPUTES THE SURFACE NORMAL
!
   INR=surf_inr_value(R_I)
   CALL NRPARAX
   RETURN
END
! SUB HITFRZFL.FOR
SUBROUTINE HITFRZFL
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!     FLAT FREZNEL-1 SURFACE
!
   REAL*8 T0,NUSUBV,NUSUBS,TESTLEN &
   &,MAG,J,ARG,RR_N,RR_Z &
   &,FPX,FPY,FPZ,SIGNNU,C3,C4,C5,C6,C7,C8,C9,C10,C11 &
   &,Q,C1,C2,SNINDX,SNIND2,RRXX,RRYY
!
   LOGICAL REFLEC
!
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
!
   PHASE=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
   RR_N=R_N
   RR_Z=R_Z
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
   ELSE
      T0=R_Z
      IF(DABS(R_N).NE.0.0D0) THEN
         IF(DUM(R_I)) THEN
            R_Z=0.0D0
!       SURFACE IS A FLAT DUMMY SURFACE
!       ASSIGN SOME VALUES TO X,Y AND Z AND RETURN
!       THE X,Y AND Z VALUES DO NOT CHANGE. THEY REMAIN THE COORDINATES
!       OF THE RAY AT THE PREVIOUS SURFACE IN THE COORDINATE SYSTEM OF
!       THE CURRENT SURFACE
            IF(R_L.EQ.0.0D0) THEN
!       RAY IS VERTICAL
               R_Y=0.0D0
            END IF
            IF(R_M.EQ.0.0D0) THEN
!       RAY IS HORIZONTAL
               R_X=0.0D0
            END IF
            IF(R_M.NE.0.0D0.AND.R_L.NE.0.0D0) THEN
!       RAY IS GENERALLY IN XY PLANE
!       GO TO A NEW R_X=0
               R_Y=(-(R_M/R_L)*R_X)+R_Y
               R_X=0.0D0
            END IF
            RAYCOD(1)=0
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=0
            RETURN
!       RAY DIRECTION COSINES REMAIN THE SAME
         ELSE
!       NOT A DUMMY SURFACE, RAY FAILED
            IF(MSG) THEN
               CALL RAY_FAILURE(R_I)
               OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
               CALL SHOWIT(1)
            END IF
            RAYCOD(1)=1
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=1
         END IF
         RETURN
      ELSE
!     R_N NOT NEAR ZERO, PROCEED
!       KEEP GOING
         NUSUBV=-T0/R_N
!       T0/N IS THE DISTANCE ALONG THE RAY FROM
!       THE STARTING POINT ON THE PREVIOUS SURFACE TO THE
!       TANGENT PLANE AT THE CURRENT SURFACE IN THE COORDINATE
!       SYSTEM OF THE CURRENT SURFACE.
!
         R_X=R_X+NUSUBV*R_L
         R_Y=R_Y+NUSUBV*R_M
         R_Z=0.0D0
      END IF
   END IF
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
   IF(surf_toric_flag(R_I) == 0) THEN
      RRXX=R_X
      RRYY=R_Y
   END IF
   IF(surf_toric_flag(R_I) == 1) THEN
      RRXX=R_X
      RRYY=0.0D0
   END IF
   IF(surf_toric_flag(R_I) == 2) THEN
      RRXX=0.0D0
      RRYY=R_Y
   END IF
!
!     NOW HOW DO THE SPECIAL SURFACE TERMS CHANGE THIS ?
   C1=FTFL01(1,R_I)
   C2=FTFL01(2,R_I)
   C3=FTFL01(3,R_I)
   C4=FTFL01(4,R_I)
   C5=FTFL01(5,R_I)
   C6=FTFL01(6,R_I)
   C7=FTFL01(7,R_I)
   C8=FTFL01(8,R_I)
   C9=FTFL01(9,R_I)
   C10=FTFL01(10,R_I)
   C11=FTFL01(11,R_I)
   IF(C1.EQ.0.0D0.OR.C2.EQ.-1.0D0) THEN
      Q=1.0D0
   ELSE
      Q=(1.0D0-((C2+1.0D0)*(C1**2)*&
      &((RRXX**2)+(RRYY**2))))
   END IF
   IF(Q.LE.0.0D0) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      STOPP=1
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      RETURN
   END IF
   IF(Q.GT.0.0D0) Q=DSQRT(Q)+1.0D0
!
   IF(C1.NE.0.0D0) THEN
      FPX=-(&
      &(((2.0D0*C1*RRXX*Q*(Q-1.0D0))+&
      &((C1**3)*RRXX*((RRXX**2)+(RRYY**2))*(C2+1.0D0)))&
      &/((Q-1.0D0)*(Q**2)))&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)&
      &)
      FPY=-(&
      &(((2.0D0*C1*RRYY*Q*(Q-1.0D0))+&
      &((C1**3)*RRYY*((RRXX**2)+(RRYY**2))*(C1+1.0D0)))&
      &/((Q-1.0D0)*(Q**2)))&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)&
      &)
   ELSE
      FPX=-(&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)&
      &)
      FPY=-(&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)&
      &)
   END IF
!
   FPZ=1.0D0
!
   MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
   LN=FPX/MAG
   MN=FPY/MAG
   NN=FPZ/MAG
!
   COSI=((R_L*LN)+(R_M*MN)+(R_N*NN))
   IF(COSI.LT.-1.0D0) COSI=-1.0D0
   IF(COSI.GT.+1.0D0) COSI=+1.0D0
   IF(.NOT.DUM(R_I)) THEN
      IF(SIGNNU.LT.0.0D0) THEN
!       REFLECTION
         R_L=(R_L-((2.0D0*COSI)*LN))
         R_M=(R_M-((2.0D0*COSI)*MN))
         R_N=(R_N-((2.0D0*COSI)*NN))
         MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
         R_L=R_L/MAG
         R_M=R_M/MAG
         R_N=R_N/MAG
      ELSE
!       NOT REFLECTION, PROCEED
         ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
         IF(ARG.LT.0.0D0) THEN
!       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
            IF(MSG) THEN
               CALL RAY_FAILURE(R_I)
               OUTLYNE='TOTAL INTERNAL REFLECTION'
               CALL SHOWIT(1)
            END IF
            RAYCOD(1)=4
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=1
            RETURN
!       NO TIR
         END IF
         IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
         IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
         J=((ldm%getSurfIndex(R_I, INT(WVN))*COSIP)&
         &-(ldm%getSurfIndex(R_I-1, INT(WVN))*COSI))/&
         &(ldm%getSurfIndex(R_I, INT(WVN)))
         R_L=((NUSUBS*R_L)+(J*LN))
         R_M=((NUSUBS*R_M)+(J*MN))
         R_N=((NUSUBS*R_N)+(J*NN))
         MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
         R_L=R_L/MAG
         R_M=R_M/MAG
         R_N=R_N/MAG
      END IF
!     DUM
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
!     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
!     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
!     NOT RV SET TO RV
!     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
!     THEN IF RV, THEN SET .NOT.RV AND IF
   TESTLEN=R_Z-RR_Z
!     NOT RV SET TO RV
!     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
!     RAY GETS "REVERSED"
   IF(surf_thickness(R_I).GT.0.0D0.AND.surf_thickness(R_I-1).LT. &
   &0.0D0.AND.DUM(R_I).OR.surf_thickness(R_I).LT.0.0D0.AND.surf_thickness(R_I-1)&
   &.GT.0.0D0.AND.DUM(R_I)) THEN
      IF(RV) THEN
         RV=.FALSE.
      ELSE
         RV=.TRUE.
      END IF
   END IF
!     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
!     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
!     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
!     RAY GETS "REVERSED"
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
      RV=.TRUE.
   ELSE
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
      RV=.FALSE.
      RVSTART=.FALSE.
   END IF
!       RAY TRACE AT FLAT SURFACE R_I COMPLETE.
   IF(R_I.EQ.NEWIMG) THEN
      R_L=OLDL
      R_M=OLDM
      R_N=OLDN
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
   RAYCOD(1)=0
   RAYCOD(2)=R_I
   SPDCD1=RAYCOD(1)
   SPDCD2=R_I
   STOPP=0
   RETURN
END
! SUB HITFRZCV.FOR
SUBROUTINE HITFRZCV
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!     CURVED FRESNEL-1
!
   REAL*8 A,B,C,QQ,SIGNNU,CV,CC,&
   &RR_Z,TEST1,TEST2,NUSUBS,RR_N,TESTLEN &
   &,Q,SIGNB,MAG,J,ARG,C1,C2,C3,C4,ZTEST,SNIND2 &
   &,FPX,FPY,FPZ,C5,C6,HV0,HV1,HV2,ZMIN,ZMAX,&
   &X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2 &
   &,C7,C8,C9,C10,C11,SNINDX,RRXX,RRYY
!
   INTEGER ZPMIN,ZPMAX
!
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC
!
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   HV0=0.0D0
   HV1=0.0D0
   HV2=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
!
   IF(surf_toric_flag(R_I) == 0) THEN
      CV=surf_curvature(R_I)
      CC=surf_conic(R_I)
   END IF
   IF(surf_toric_flag(R_I) /= 0) THEN
      CV=surf_toric_curvature(R_I)
      CC=surf_anamorphic_conic(R_I)
   END IF
   IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
   IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
   IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
!
   RR_N=R_N
   RR_Z=R_Z
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
!       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE CONIC
   END IF
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
!       NOW INTERSECT THE SURFACE
!       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
!
   IF(surf_toric_flag(R_I) == 0) THEN
      RRXX=R_X
      RRYY=R_Y
   END IF
   IF(surf_toric_flag(R_I) == 1) THEN
      RRXX=R_X
      RRYY=0.0D0
   END IF
   IF(surf_toric_flag(R_I) == 2) THEN
      RRXX=0.0D0
      RRYY=R_Y
   END IF
   A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*&
   &(CC+1.0D0))))
   B=R_N-(CV*RRXX*R_L)-(CV*RRYY*R_M)-&
   &((CV*R_Z*R_N)&
   &*(CC+1.0D0))
   B=2.0D0*B
   C=-CV*((RRXX**2)+(RRYY**2)+((R_Z**2)*&
   &(CC+1.0D0)))
   C=(C+(2.0D0*R_Z))
   IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
   IF(B.EQ.0.0D0) SIGNB=1.0D0
   IF(A.EQ.0.0D0) THEN
      HV0=-(C/B)
      INTERS=1
   ELSE
!       A NOT ZERO
      ARG=((B**2)-(4.0D0*A*C))
      IF(ARG.LT.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
!       ARG NOT ZERO
      END IF
      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
      HV1=C/Q
      HV2=Q/A
      INTERS=2
   END IF
   IF(INTERS.EQ.1) THEN
!       CASE OF ONLY ONE INTERSECTION POINT
      X1=(R_X+(HV0*R_L))
      Y1=(R_Y+(HV0*R_M))
      Z1=(R_Z+(HV0*R_N))
   END IF
   IF(INTERS.EQ.2) THEN
!       CASE OF TWO INTERSECTION POINTS
      X1=(R_X+(HV1*R_L))
      Y1=(R_Y+(HV1*R_M))
      Z1=(R_Z+(HV1*R_N))
      X2=(R_X+(HV2*R_L))
      Y2=(R_Y+(HV2*R_M))
      Z2=(R_Z+(HV2*R_N))
   END IF
!     FIX FOR HYPERBOLAS
   IF(CC.LT.-1.0D0) THEN
      ZMIN=Z2
      ZMAX=Z2
      ZPMIN=2
      ZPMAX=2
      IF(Z1.LT.Z2) ZMIN=Z1
      IF(Z1.LT.Z2) ZPMIN=1
      IF(Z1.GT.Z2) ZMAX=Z1
      IF(Z1.GT.Z2) ZPMAX=1
!     HYPERBOLA
!     CV POSITIVE
      IF(CV.GT.0.0D0) THEN
         IF(ZMIN.LT.0.0D0) THEN
!     ONLY ONE INTERSECTION POINT
            INTERS=1
            IF(ZPMIN.EQ.1) THEN
               X1=X2
               Y1=Y2
               Z1=Z2
!     LEAVE X1,Y1,Z1 ALONE
            END IF
!     TWO POINTS
         END IF
      ELSE
!    CV NEGATIVE
         IF(ZMAX.GT.0.0D0) THEN
!     ONLY ONE INTERSECTION POINT
            INTERS=1
            IF(ZPMAX.EQ.1) THEN
               X1=X2
               Y1=Y2
               Z1=Z2
!     LEAVE X1,Y1,Z1 ALONE
            END IF
!     THERE ARE TWO POINTS
         END IF
      END IF
!     NOT HYPERBOLA
   END IF
!
!
   IF(surf_toric_flag(R_I) == 0) THEN
      RRXX=X1
      RRYY=Y1
   END IF
   IF(surf_toric_flag(R_I) == 1) THEN
      RRXX=X1
      RRYY=0.0D0
   END IF
   IF(surf_toric_flag(R_I) == 2) THEN
      RRXX=0.0D0
      RRYY=Y1
   END IF
!       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
   QQ=1.0D0-((CC+1.0D0)*(CV**2)*((RRXX**2)+(RRYY**2)))
   IF(QQ.LT.0.0D0) THEN
      QQ=-QQ
!       QQ NOT NEGATIVE
   END IF
   QQ=DSQRT(QQ)
   QQ=1.0D0+QQ
   IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      STOPP=1
      RETURN
   END IF
   FPX=-((2.0D0*CV*RRXX*QQ*(QQ-1.0D0))+&
   &((CV**3)*RRXX*((RRXX**2)+(RRYY**2))*&
   &(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
   FPY=-((2.0D0*CV*RRYY*QQ*(QQ-1.0D0))+&
   &((CV**3)*RRYY*((RRXX**2)+(RRYY**2))&
   &*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
   IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
      IF(CC.LT.-1.0D0) THEN
         FPZ=1.0D0
      ELSE
         IF(Z1.GT.ZTEST) FPZ=-1.0D0
         IF(Z1.LT.ZTEST) FPZ=1.0D0
         IF(Z1.EQ.ZTEST) FPZ=0.0D0
      END IF
   ELSE
!     CV NEG
!     USE OUTWARD NORMAL
      IF(CC.LT.-1.0D0) THEN
         FPZ=1.0D0
      ELSE
         IF(Z1.LT.ZTEST) FPZ=-1.0D0
         IF(Z1.GT.ZTEST) FPZ=1.0D0
         IF(Z1.EQ.ZTEST) FPZ=0.0D0
      END IF
   END IF
!
   MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
   LN1=FPX/MAG
   MN1=FPY/MAG
   NN1=FPZ/MAG
!       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
   IF(INTERS.EQ.2) THEN
!       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
      IF(surf_toric_flag(R_I) == 0) THEN
         RRXX=X2
         RRYY=Y2
      END IF
      IF(surf_toric_flag(R_I) == 1) THEN
         RRXX=X2
         RRYY=0.0D0
      END IF
      IF(surf_toric_flag(R_I) == 2) THEN
         RRXX=0.0D0
         RRYY=Y2
      END IF
      QQ=1.0D0-((CC+1.0D0)*(CV**2)*((RRXX**2)+(RRYY**2)))
      IF(QQ.LT.0.0D0) THEN
         QQ=-QQ
!       QQ NOT NEG
      END IF
      QQ=DSQRT(QQ)
      QQ=1.0D0+QQ
      IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      END IF
      FPX=-((2.0D0*CV*RRXX*QQ*(QQ-1.0D0))+&
      &((CV**3)*RRXX*((RRXX**2)+(RRYY**2))*&
      &(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
      FPY=-((2.0D0*CV*RRYY*QQ*(QQ-1.0D0))+&
      &((CV**3)*RRYY*((RRXX**2)+(RRYY**2))&
      &*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
      IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
         IF(CC.LT.-1.0D0) THEN
            FPZ=1.0D0
         ELSE
            IF(Z2.GT.ZTEST) FPZ=-1.0D0
            IF(Z2.LT.ZTEST) FPZ=1.0D0
            IF(Z2.EQ.ZTEST) FPZ=0.0D0
         END IF
      ELSE
!     CV NEG
!     USE OUTWARD NORMAL
         IF(CC.LT.-1.0D0) THEN
            FPZ=1.0D0
         ELSE
            IF(Z2.LT.ZTEST) FPZ=-1.0D0
            IF(Z2.GT.ZTEST) FPZ=1.0D0
            IF(Z2.EQ.ZTEST) FPZ=0.0D0
         END IF
      END IF
!
      MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
      LN2=FPX/MAG
      MN2=FPY/MAG
      NN2=FPZ/MAG
!       NOT TWO INTERSECTION POINTS
   END IF
!       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
!       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
!
   IF(INTERS.EQ.2) THEN
!       THERE ARE TWO INTERSECTION POINTS
      TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
      TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
      IF(POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     POSRAY
      END IF
      IF(.NOT.POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     POSRAY
      END IF
   ELSE
!       NOT TWO INTERSECTIONS
!       JUST ONE INTERSECTION
      R_X=X1
      R_Y=Y1
      R_Z=Z1
      LN=LN1
      MN=MN1
      NN=NN1
   END IF
200 CONTINUE
!
!       NOW WE KNOW THE INTERSECTION POINT IF THE ASPHERIC TERMS
!       ARE ALL ZERO. WE ALSO HAVE THE SURFACE NORMALS
!
!       NOW CALCULATE THE RAY REAL SURFACE NORMALS DUE TO THE
!       FRESNEL SURFACE
!
   C1=FTFL01(1,R_I)
   C2=FTFL01(2,R_I)
   C3=FTFL01(3,R_I)
   C4=FTFL01(4,R_I)
   C5=FTFL01(5,R_I)
   C6=FTFL01(6,R_I)
   C7=FTFL01(7,R_I)
   C8=FTFL01(8,R_I)
   C9=FTFL01(9,R_I)
   C10=FTFL01(10,R_I)
   C11=FTFL01(11,R_I)
   IF(surf_toric_flag(R_I) == 0) THEN
      RRXX=R_X
      RRYY=R_Y
   END IF
   IF(surf_toric_flag(R_I) == 1) THEN
      RRXX=R_X
      RRYY=0.0D0
   END IF
   IF(surf_toric_flag(R_I) == 1) THEN
      RRXX=0.0D0
      RRYY=R_Y
   END IF
!
   IF(C1.EQ.0.0D0.OR.C2.EQ.-1.0D0) THEN
      Q=1.0D0
   ELSE
      Q=(1.0D0-((C2+1.0D0)*(C1**2)*&
      &((RRXX**2)+(RRYY**2))))
   END IF
   IF(Q.LE.0.0D0) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      STOPP=1
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      RETURN
   END IF
!
   IF(Q.GT.0.0D0) Q=DSQRT(Q)+1.0D0
!
   IF(C1.NE.0.0D0) THEN
      FPX=-(&
      &(((2.0D0*C1*RRXX*Q*(Q-1.0D0))+&
      &((C1**3)*RRXX*((RRXX**2)+(RRYY**2))*(C2+1.0D0)))&
      &/((Q-1.0D0)*(Q**2)))&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)&
      &)
      FPY=-(&
      &(((2.0D0*C1*RRYY*Q*(Q-1.0D0))+&
      &((C1**3)*RRYY*((RRXX**2)+(RRYY**2))*(C1+1.0D0)))&
      &/((Q-1.0D0)*(Q**2)))&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)&
      &)
   ELSE
      FPX=-(&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRXX)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRXX)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRXX)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRXX)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRXX)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRXX)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRXX)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRXX)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRXX)*C11)&
      &)
      FPY=-(&
      &+((2.0D0*((RRXX**2)+(RRYY**2))*2.0D0*RRYY)*C3)&
      &+((3.0D0*(((RRXX**2)+(RRYY**2))**2)*2.0D0*RRYY)*C4)&
      &+((4.0D0*(((RRXX**2)+(RRYY**2))**3)*2.0D0*RRYY)*C5)&
      &+((5.0D0*(((RRXX**2)+(RRYY**2))**4)*2.0D0*RRYY)*C6)&
      &+((6.0D0*(((RRXX**2)+(RRYY**2))**5)*2.0D0*RRYY)*C7)&
      &+((7.0D0*(((RRXX**2)+(RRYY**2))**6)*2.0D0*RRYY)*C8)&
      &+((8.0D0*(((RRXX**2)+(RRYY**2))**7)*2.0D0*RRYY)*C9)&
      &+((9.0D0*(((RRXX**2)+(RRYY**2))**8)*2.0D0*RRYY)*C10)&
      &+((10.0D0*(((RRXX**2)+(RRYY**2))**9)*2.0D0*RRYY)*C11)&
      &)
   END IF
   FPZ=1.0D0
!
!     X AND Y DERIVATIVES OF THE SURFACE HAVE BEEN CALCULATED
!
   MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
   LN=FPX/MAG
   MN=FPY/MAG
   NN=FPZ/MAG
!
!       THE COSINE SQUARED OF THE ANGLE OF INCIDENCE IS JUST
   COSI=(LN*R_L)+(MN*R_M)+(NN*R_N)
   IF(COSI.LT.-1.0D0) COSI=-1.0D0
   IF(COSI.GT.+1.0D0) COSI=+1.0D0
   IF(.NOT.DUM(R_I)) THEN
      IF(SIGNNU.LT.0.0D0) THEN
!       REFLECTION
         R_L=(R_L-((2.0D0*COSI)*LN))
         R_M=(R_M-((2.0D0*COSI)*MN))
         R_N=(R_N-((2.0D0*COSI)*NN))
         MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
         R_L=R_L/MAG
         R_M=R_M/MAG
         R_N=R_N/MAG
      ELSE
!       NOT REFLECTION, PROCEED
         ARG=(1.0D0-(NUSUBS**2)*(1.0D0-(COSI**2)))
         IF(ARG.LT.0.0D0) THEN
!       TIR OCCURRED, PRINT MESSAGE AND STOP TRACING
            IF(MSG) THEN
               CALL RAY_FAILURE(R_I)
               OUTLYNE='TOTAL INTERNAL REFLECTION'
               CALL SHOWIT(1)
            END IF
            RAYCOD(1)=4
            RAYCOD(2)=R_I
            SPDCD1=RAYCOD(1)
            SPDCD2=R_I
            STOPP=1
            RETURN
!       NO TIR
         END IF
         IF(COSI.LE.0.0D0) COSIP=-DSQRT(ARG)
         IF(COSI.GT.0.0D0) COSIP=DSQRT(ARG)
         J=((ldm%getSurfIndex(R_I, INT(WVN))*COSIP)&
         &-(ldm%getSurfIndex(R_I-1, INT(WVN))*COSI))/&
         &(ldm%getSurfIndex(R_I, INT(WVN)))
         R_L=((NUSUBS*R_L)+(J*LN))
         R_M=((NUSUBS*R_M)+(J*MN))
         R_N=((NUSUBS*R_N)+(J*NN))
         MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
         R_L=R_L/MAG
         R_M=R_M/MAG
         R_N=R_N/MAG
      END IF
!     DUM
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
!     IF AT A DUMMY SURFACE, THE THICKNESS GOES FROM POS TO NEG
!     OR NEG TO POS, THEN IF RV, THEN SET .NOT.RV AND IF
!     NOT RV SET TO RV
!     OR AT A DUMMY SURFACE, N POS INTO NEG TH OR N NEG INTO POS TH
!     THEN IF RV, THEN SET .NOT.RV AND IF
   TESTLEN=R_Z-RR_Z
!     NOT RV SET TO RV
!     IF AT A DUMMY SURFACE, THE THICKNESS CHANGES SIGN, THEN THE
!     RAY GETS "REVERSED"
   IF(surf_thickness(R_I).GT.0.0D0.AND.surf_thickness(R_I-1).LT. &
   &0.0D0.AND.DUM(R_I).OR.surf_thickness(R_I).LT.0.0D0.AND.surf_thickness(R_I-1)&
   &.GT.0.0D0.AND.DUM(R_I)) THEN
      IF(RV) THEN
         RV=.FALSE.
      ELSE
         RV=.TRUE.
      END IF
   END IF
!     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
!     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
!     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
!     RAY GETS "REVERSED"
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
      RV=.TRUE.
   ELSE
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
      RV=.FALSE.
      RVSTART=.FALSE.
   END IF
!       RAYTRACE THROUGH ASPHERE COMPLETED
   IF(R_I.EQ.NEWIMG) THEN
      R_L=OLDL
      R_M=OLDM
      R_N=OLDN
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
   RAYCOD(1)=0
   RAYCOD(2)=R_I
   SPDCD1=RAYCOD(1)
   SPDCD2=R_I
   STOPP=0
   RETURN
END
! SUB HITGRAZ.FOR
SUBROUTINE HITGRAZ
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITGRAZ.FOR. THIS SUBROUTINE IMPLEMENTS
!       A GRAZING INCIDENCE RAY TRACE FOR GRAZING INCIDENCE,
!       REFLECTIVE SURFACES
!
   REAL*8 A,B,C,QQ,SIGNNU,CV,CC,&
   &TEST1,TEST2,NUSUBS,RR_N,TESTLEN &
   &,Q,SIGNB,MAG,J,ARG,C1,C2,C3,C4,ZTEST,SNIND2 &
   &,FPX,FPY,FPZ,C5,C6,HV0,HV1,HV2,ZMIN,ZMAX,&
   &X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2 &
   &,SNINDX,HV_JK,RR_Z
!
   INTEGER ZPMIN,ZPMAX
!
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC
!
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   RR_N=R_N
   HV0=0.0D0
   HV1=0.0D0
   HV2=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
!
!       SURFACE IS CONIC BY DEFINITION
   CV=surf_curvature(R_I)
   CC=surf_conic(R_I)
   IF(CC.EQ.-1.0D0.AND.CV.GT.0.0D0) ZTEST=1.0D20
   IF(CC.EQ.-1.0D0.AND.CV.LT.0.0D0) ZTEST=-1.0D20
   IF(CC.NE.-1.0D0) ZTEST=1.0D0/((CC+1.0D0)*CV)
!
   RR_N=R_N
   RR_Z=R_Z
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
!       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE CONIC
   END IF
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
!       NOW INTERSECT THE SPHERE OR CONIC.
!       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
!
   A=-(CV*((R_L**2)+(R_M**2)+((R_N**2)*&
   &(CC+1.0D0))))
   B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-&
   &((CV*R_Z*R_N)&
   &*(CC+1.0D0))
   B=2.0D0*B
   C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)*&
   &(CC+1.0D0)))
   C=(C+(2.0D0*R_Z))
   IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
   IF(B.EQ.0.0D0) SIGNB=1.0D0
   IF(A.EQ.0.0D0) THEN
      HV0=-(C/B)
      INTERS=1
   ELSE
!       A NOT ZERO
      ARG=((B**2)-(4.0D0*A*C))
      IF(ARG.LT.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
!       ARG NOT ZERO
      END IF
      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
      HV1=C/Q
      HV2=Q/A
      INTERS=2
      IF(HV2.EQ.0.0D0.OR.HV1.EQ.0.0D0) THEN
         INTERS=1
         IF(HV1.EQ.0.0D0.AND.HV2.NE.0.0D0) HV_JK=HV2
         IF(HV1.NE.0.0D0.AND.HV2.EQ.0.0D0) HV_JK=HV1
         IF(HV1.EQ.0.0D0.AND.HV2.EQ.0.0D0) HV_JK=0.0D0
         HV1=HV_JK
         HV2=0.0D0
      END IF
!
   END IF
   IF(INTERS.EQ.1) THEN
!       CASE OF ONLY ONE INTERSECTION POINT
      X1=(R_X+(HV0*R_L))
      Y1=(R_Y+(HV0*R_M))
      Z1=(R_Z+(HV0*R_N))
   END IF
   IF(INTERS.EQ.2) THEN
!       CASE OF TWO INTERSECTION POINTS
      X1=(R_X+(HV1*R_L))
      Y1=(R_Y+(HV1*R_M))
      Z1=(R_Z+(HV1*R_N))
      X2=(R_X+(HV2*R_L))
      Y2=(R_Y+(HV2*R_M))
      Z2=(R_Z+(HV2*R_N))
   END IF
!     FIX FOR HYPERBOLAS
   IF(CC.LT.-1.0D0) THEN
      ZMIN=Z2
      ZMAX=Z2
      ZPMIN=2
      ZPMAX=2
      IF(Z1.LT.Z2) ZMIN=Z1
      IF(Z1.LT.Z2) ZPMIN=1
      IF(Z1.GT.Z2) ZMAX=Z1
      IF(Z1.GT.Z2) ZPMAX=1
!     HYPERBOLA
!     CV POSITIVE
      IF(CV.GT.0.0D0) THEN
         IF(ZMIN.LT.0.0D0) THEN
!     ONLY ONE INTERSECTION POINT
            INTERS=1
            IF(ZPMIN.EQ.1) THEN
               X1=X2
               Y1=Y2
               Z1=Z2
!     LEAVE X1,Y1,Z1 ALONE
            END IF
!     TWO POINTS
         END IF
      ELSE
!    CV NEGATIVE
         IF(ZMAX.GT.0.0D0) THEN
!     ONLY ONE INTERSECTION POINT
            INTERS=1
            IF(ZPMAX.EQ.1) THEN
               X1=X2
               Y1=Y2
               Z1=Z2
!     LEAVE X1,Y1,Z1 ALONE
            END IF
!     THERE ARE TWO POINTS
         END IF
      END IF
!     NOT HYPERBOLA
   END IF
!
!
!       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
   QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X1**2)+(Y1**2)))
   IF(QQ.LT.0.0D0) THEN
      QQ=-QQ
!       QQ NOT NEGATIVE
   END IF
   QQ=DSQRT(QQ)
   QQ=1.0D0+QQ
   IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      STOPP=1
      RETURN
   END IF
   FPX=-((2.0D0*CV*X1*QQ*(QQ-1.0D0))+&
   &((CV**3)*X1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
   FPY=-((2.0D0*CV*Y1*QQ*(QQ-1.0D0))+&
   &((CV**3)*Y1*((X1**2)+(Y1**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
   IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
      IF(CC.LT.-1.0D0) THEN
         FPZ=1.0D0
      ELSE
         IF(Z1.GT.ZTEST) FPZ=-1.0D0
         IF(Z1.LT.ZTEST) FPZ=1.0D0
         IF(Z1.EQ.ZTEST) FPZ=0.0D0
      END IF
   ELSE
!     CV NEG
!     USE OUTWARD NORMAL
      IF(CC.LT.-1.0D0) THEN
         FPZ=1.0D0
      ELSE
         IF(Z1.LT.ZTEST) FPZ=-1.0D0
         IF(Z1.GT.ZTEST) FPZ=1.0D0
         IF(Z1.EQ.ZTEST) FPZ=0.0D0
      END IF
   END IF
!
   MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
   LN1=FPX/MAG
   MN1=FPY/MAG
   NN1=FPZ/MAG
!       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
   IF(INTERS.EQ.2) THEN
!       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
      QQ=1.0D0-((CC+1.0D0)*(CV**2)*((X2**2)+(Y2**2)))
      IF(QQ.LT.0.0D0) THEN
         QQ=-QQ
!       QQ NOT NEG
      END IF
      QQ=DSQRT(QQ)
      QQ=1.0D0+QQ
      IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      END IF
      FPX=-((2.0D0*CV*X2*QQ*(QQ-1.0D0))+&
      &((CV**3)*X2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
      FPY=-((2.0D0*CV*Y2*QQ*(QQ-1.0D0))+&
      &((CV**3)*Y2*((X2**2)+(Y2**2))*(CC+1.0D0)))/((QQ-1.0D0)*(QQ**2))
      IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
         IF(CC.LT.-1.0D0) THEN
            FPZ=1.0D0
         ELSE
            IF(Z2.GT.ZTEST) FPZ=-1.0D0
            IF(Z2.LT.ZTEST) FPZ=1.0D0
            IF(Z2.EQ.ZTEST) FPZ=0.0D0
         END IF
      ELSE
!     CV NEG
!     USE OUTWARD NORMAL
         IF(CC.LT.-1.0D0) THEN
            FPZ=1.0D0
         ELSE
            IF(Z2.LT.ZTEST) FPZ=-1.0D0
            IF(Z2.GT.ZTEST) FPZ=1.0D0
            IF(Z2.EQ.ZTEST) FPZ=0.0D0
         END IF
      END IF
!
      MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
      LN2=FPX/MAG
      MN2=FPY/MAG
      NN2=FPZ/MAG
!       NOT TWO INTERSECTION POINTS
   END IF
!       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
!       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
!
   IF(INTERS.EQ.2) THEN
!       THERE ARE TWO INTERSECTION POINTS
      TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
      TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
      IF(POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     POSRAY
      END IF
      IF(.NOT.POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     POSRAY
      END IF
   ELSE
!       NOT TWO INTERSECTIONS
!       JUST ONE INTERSECTION
      R_X=X1
      R_Y=Y1
      R_Z=Z1
      LN=LN1
      MN=MN1
      NN=NN1
!
   END IF
200 CONTINUE
!
!       NOW WE KNOW THE INTERSECTION POINT IF THE FOURIER LEGENDRE TERMS
!       ARE ALL ZERO. WE ALSO HAVE THE SURFACE NORMALS
!
!       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED SURFACE
!       USING A CALL TO SUBROUTINE NR5. THIS ALSO GIVES THE DIRECTION
!       COSINES OF THE SURFACE NORMAL TO THE DEFORMED SURFACE.
!       ONLY IF THERE ARE NON-ZERO FOURIER-LEGENDRE COEFFS
!
   IF(FTFL01(4,R_I).NE.0.0D0.OR.FTFL01(5,R_I).NE.0.0D0 &
   &.OR.FTFL01(6,R_I).NE.0.0D0.OR.FTFL01(7,R_I).NE.0.0D0 &
   &.OR.FTFL01(8,R_I).NE.0.0D0.OR.FTFL01(9,R_I).NE.0.0D0 &
   &.OR.FTFL01(10,R_I).NE.0.0D0.OR.FTFL01(11,R_I).NE.0.0D0 &
   &.OR.FTFL01(12,R_I).NE.0.0D0.OR.FTFL01(13,R_I).NE.0.0D0 &
   &.OR.FTFL01(14,R_I).NE.0.0D0.OR.FTFL01(15,R_I).NE.0.0D0 &
   &.OR.FTFL01(16,R_I).NE.0.0D0.OR.FTFL01(17,R_I).NE.0.0D0 &
   &.OR.FTFL01(18,R_I).NE.0.0D0) THEN
      INR=surf_inr_value(R_I)
      CALL NR5
   END IF
!
   IF(R_Z.LT.FTFL01(1,R_I).OR.R_Z.GT.FTFL01(2,R_I)) THEN
!     MISSED SURFACE
      RAYCOD(1)=13
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY MISSED GRAZING INCIDENCE SURFACE SECTION'
         CALL SHOWIT(1)
      END IF
      STOPP=1
   END IF
   IF(STOPP.EQ.1) RETURN
!
!       THE RETURNED VALUES OF X,Y,Z ARE THE CORRECT INTERSECTIONS
!       TO WITHIN THE 1.0D-25 OR SURTOL WHICHEVER IS THE SMALLER VALUE
!
!       THE COSINE SQUARED OF THE ANGLE OF INCIDENCE IS JUST
   COSI=(LN*R_L)+(MN*R_M)+(NN*R_N)
   IF(COSI.LT.-1.0D0) COSI=-1.0D0
   IF(COSI.GT.+1.0D0) COSI=+1.0D0
!
!       REFLECTION ALWAYS
   R_L=(R_L-((2.0D0*COSI)*LN))
   R_M=(R_M-((2.0D0*COSI)*MN))
   R_N=(R_N-((2.0D0*COSI)*NN))
!       RE-NORMALIZE
   MAG=DSQRT((R_L**2)+(R_M**2)+(R_N**2))
   R_L=R_L/MAG
   R_M=R_M/MAG
   R_N=R_N/MAG
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
!     IF THE RAY HAS TRAVELED A NEG DIST WITH A POS DIRCOS
!     OR THE RAY HAS TRAVELED A POS DIST WITH A NEG DIR COS
!     AND THE RAY WAS NOT REVERESED, THEN SET IT TO "REVERSED"
!     RAY GETS "REVERSED"
   TESTLEN=R_Z-RR_Z
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &SNINDX.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR.OR.&
   &SNINDX.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR) THEN
      RV=.TRUE.
   ELSE
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.RVSTART.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.RVSTART) THEN
      RV=.FALSE.
   END IF
   IF(RR_N.GT.0.0D0.AND.TESTLEN.GT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.LT.0.0D0.AND..NOT.REVSTR.OR.&
   &RR_N.GT.0.0D0.AND.TESTLEN.LT.0.0D0.AND.REVSTR.OR.&
   &RR_N.LT.0.0D0.AND.TESTLEN.GT.0.0D0.AND.REVSTR) THEN
      RV=.FALSE.
      RVSTART=.FALSE.
   END IF
!       RAYTRACE OFF A GRAZING SURFACE COMPLETED
   IF(R_I.EQ.NEWIMG) THEN
      R_L=OLDL
      R_M=OLDM
      R_N=OLDN
   END IF
   COSIP=(R_L*LN)+(R_M*MN)+(R_N*NN)
   IF(COSIP.LT.-1.0D0) COSIP=-1.0D0
   IF(COSIP.GT.+1.0D0) COSIP=+1.0D0
   STOPP=0
   RAYCOD(1)=0
   RAYCOD(2)=R_I
   SPDCD1=RAYCOD(1)
   SPDCD2=R_I
   RETURN
END
!**********************************************************************
SUBROUTINE ARRAYIN_FIX(N_X,N_Y)
   use DATLEN
   use DATMAI
   IMPLICIT NONE
   REAL*8 X,Y
   INTEGER I,N_X,N_Y
   I=R_I
   X=R_X
   Y=R_Y
   CALL POSARRAY1(I,X,Y,N_X,N_Y)
   R_X=X
   R_Y=Y

   RETURN
END
!**********************************************************************
SUBROUTINE ARRAYOUT_FIX(N_X,N_Y)
   use DATLEN
   use DATMAI
   IMPLICIT NONE
   INTEGER I,N_X,N_Y
   REAL*8 X,Y
   I=R_I
   X=R_X
   Y=R_Y
   CALL POSARRAY2(I,X,Y,N_X,N_Y)
   R_X=X
   R_Y=Y
   RETURN
END
!**********************************************************************
SUBROUTINE POSARRAY1(I,X,Y,N_X,N_Y)
   use DATLEN
   use DATMAI
   use mod_surface
   IMPLICIT NONE
   REAL*8 X,Y,DX,DY,XWORKING,YWORKING,SGNX,SGNY
   INTEGER I,N_X,N_Y
!       X AND Y  PASS IN AS THE X AND Y COORDINATES AND PASS BACK AS THE X AND Y
!       COORDINATES AT A SINGLE LENSLET
   DX=surf_array_dx(I)
   DY=surf_array_dy(I)
   IF(surf_array_parity(I).EQ.-1.0D0) THEN
!       ODD
      N_X=DBLE(NINT(X/DX))
      N_Y=DBLE(NINT(Y/DY))
      XWORKING=X-(N_X*DX)
      YWORKING=Y-(N_Y*DY)
      X=XWORKING
      Y=YWORKING
   END IF
   IF(surf_array_parity(I) == 1) THEN
!       EVEN
      IF(X.EQ.0.0D0) THEN
         SGNX=1.0D0
      ELSE
         SGNX=X/DABS(X)
      END IF
      IF(Y.EQ.0.0D0) THEN
         SGNY=1.0D0
      ELSE
         SGNY=Y/DABS(Y)
      END IF
      N_X=(DBLE(INT(X/DX))*2.0D0)+SGNX
      N_Y=(DBLE(INT(Y/DY))*2.0D0)+SGNY
      XWORKING=X-(N_X*DX/2.0D0)
      YWORKING=Y-(N_Y*DY/2.0D0)
      X=XWORKING
      Y=YWORKING
   END IF
   RETURN
END
!**********************************************************************
SUBROUTINE POSARRAY2(I,X,Y,N_X,N_Y)
   use DATLEN
   use DATMAI
   use mod_surface
   IMPLICIT NONE
   REAL*8 X,Y,DX,DY,XWORKING,YWORKING
   INTEGER I,N_X,N_Y
!       INVERSE OF POSARRAY1
   DX=surf_array_dx(I)
   DY=surf_array_dy(I)
   IF(surf_array_parity(I).EQ.-1.0D0) THEN
!       ODD
      XWORKING=X+(N_X*DX)
      YWORKING=Y+(N_Y*DY)
      X=XWORKING
      Y=YWORKING
   END IF
   IF(surf_array_parity(I) == 1) THEN
!       EVEN
      XWORKING=X+(N_X*DX/2.0D0)
      YWORKING=Y+(N_Y*DY/2.0D0)
      X=XWORKING
      Y=YWORKING
   END IF
   RETURN
END
!**********************************************************************
! SUB HITANA_old.FOR
SUBROUTINE HITANA_old(OR_N,OR_Z)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITANA.FOR. THIS SUBROUTINE IMPLEMENTS
!       SURFACE INTERSECTIONS TO ANAMORPHICS IN RAYTRACING.
!
   REAL*8 A,B,C,QQ,SIGNNU,CV,CC,C1,C2,C3,C4,C5,C6,&
   &C7,C8,C9,C10,C11,C12,C13,C14,NUSUBS,OR_N,OR_Z,&
   &TEST1,TEST2,RR_N,TESTLEN,ZTEST,RR_Z &
   &,Q,SIGNB,MAG,J,ARG,SNINDX,SNIND2 &
   &,FPX,FPY,FPZ,HV0,HV1,HV2,&
   &X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2
!
!     DIFFRACTION GRATING STUFF
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 RD,R1,R2,LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC,GERROR,ERR
!
   INTEGER SPDCD1,SPDCD2,ISURF
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   HV0=0.0D0
   HV1=0.0D0
   HV2=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
   R1=0.0D0
   R2=0.0D0
   IF(surf_curvature(R_I).NE.0.0D0) R1=1.0D0/surf_curvature(R_I)
   IF(surf_toric_curvature(R_I).NE.0.0D0) R2=1.0D0/surf_toric_curvature(R_I)
   RD=(R1+R2)/2.0D0
   CV=0.0D0
   IF(RD.NE.0.0D0) CV=1.0D0/RD
!
   IF(CV.NE.0.0D0) ZTEST=1.0D0/CV
!
   RR_N=R_N
   RR_Z=R_Z
   OR_Z=RR_Z
   OR_N=RR_N
!
!       SURFACE IS AN ANAMORPH
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
!       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE SPHERE
   END IF
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
!       NOW INTERSECT THE LARGEST SPHERE.
!       THE FOLLOWING CALCULATIONS ARE INTERMEDIATE STEPS:
!
   A=-(CV*((R_L**2)+(R_M**2)+((R_N**2))))
   B=R_N-(CV*R_X*R_L)-(CV*R_Y*R_M)-&
   &((CV*R_Z*R_N))
   B=2.0D0*B
   C=-CV*((R_X**2)+(R_Y**2)+((R_Z**2)))
   C=(C+(2.0D0*R_Z))
   IF(B.NE.0.0D0) SIGNB=((DABS(B))/(B))
   IF(B.EQ.0.0D0) SIGNB=1.0D0
   IF(A.EQ.0.0D0) THEN
      HV0=-(C/B)
      INTERS=1
   ELSE
!       A NOT ZERO
      ARG=((B**2)-(4.0D0*A*C))
      IF(ARG.LT.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
!       ARG NOT ZERO
      END IF
      Q=(-0.5D0*(B+(SIGNB*(DSQRT((B**2)-(4.0D0*A*C))))))
      HV1=C/Q
      HV2=Q/A
      INTERS=2
   END IF
   IF(INTERS.EQ.1) THEN
!       CASE OF ONLY ONE INTERSECTION POINT
      X1=(R_X+(HV0*R_L))
      Y1=(R_Y+(HV0*R_M))
      Z1=(R_Z+(HV0*R_N))
   END IF
   IF(INTERS.EQ.2) THEN
!       CASE OF TWO INTERSECTION POINTS
      X1=(R_X+(HV1*R_L))
      Y1=(R_Y+(HV1*R_M))
      Z1=(R_Z+(HV1*R_N))
      X2=(R_X+(HV2*R_L))
      Y2=(R_Y+(HV2*R_M))
      Z2=(R_Z+(HV2*R_N))
   END IF
!
!       CALCULATE THE DIRECTION COSINES OF THE FIRST POINT
   QQ=1.0D0-((CV**2)*((X1**2)+(Y1**2)))
   IF(QQ.LT.0.0D0) THEN
      QQ=-QQ
!       QQ NOT NEGATIVE
   END IF
   QQ=DSQRT(QQ)
   QQ=1.0D0+QQ
   IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      STOPP=1
      RETURN
   END IF
   FPX=-((2.0D0*CV*X1*QQ*(QQ-1.0D0))+&
   &((CV**3)*X1*((X1**2)+(Y1**2))))/((QQ-1.0D0)*(QQ**2))
   FPY=-((2.0D0*CV*Y1*QQ*(QQ-1.0D0))+&
   &((CV**3)*Y1*((X1**2)+(Y1**2))))/((QQ-1.0D0)*(QQ**2))
!
   IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
      IF(Z1.GT.ZTEST) FPZ=-1.0D0
      IF(Z1.LT.ZTEST) FPZ=1.0D0
      IF(Z1.EQ.ZTEST) FPZ=0.0D0
   ELSE
!     CV NEG
!     USE OUTWARD NORMAL
      IF(Z1.LT.ZTEST) FPZ=-1.0D0
      IF(Z1.GT.ZTEST) FPZ=1.0D0
      IF(Z1.EQ.ZTEST) FPZ=0.0D0
   END IF
!
   MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
   LN1=FPX/MAG
   MN1=FPY/MAG
   NN1=FPZ/MAG
!       NOW THE DIRECTION COSINES OF THE SECOND POINT IF IT EXISTS
   IF(INTERS.EQ.2) THEN
!       FIRST THE DIRECTION COSINES OF THE SURFACE NORMAL
      QQ=1.0D0-((CV**2)*((X2**2)+(Y2**2)))
      IF(QQ.LT.0.0D0) THEN
         QQ=-QQ
!       QQ NOT NEG
      END IF
      QQ=DSQRT(QQ)
      QQ=1.0D0+QQ
      IF(((QQ-1.0D0)*(QQ**2)).EQ.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(R_I)
            OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
            CALL SHOWIT(1)
         END IF
         RAYCOD(1)=1
         RAYCOD(2)=R_I
         SPDCD1=RAYCOD(1)
         SPDCD2=R_I
         STOPP=1
         RETURN
      END IF
      FPX=-((2.0D0*CV*X2*QQ*(QQ-1.0D0))+&
      &((CV**3)*X2*((X2**2)+(Y2**2))))/((QQ-1.0D0)*(QQ**2))
      FPY=-((2.0D0*CV*Y2*QQ*(QQ-1.0D0))+&
      &((CV**3)*Y2*((X2**2)+(Y2**2))))/((QQ-1.0D0)*(QQ**2))
      IF(CV.GT.0.0D0) THEN
!     USE INWARD NORMAL
         IF(Z2.GT.ZTEST) FPZ=-1.0D0
         IF(Z2.LT.ZTEST) FPZ=1.0D0
         IF(Z2.EQ.ZTEST) FPZ=0.0D0
      ELSE
!     CV NEG
!     USE OUTWARD NORMAL
         IF(Z2.LT.ZTEST) FPZ=-1.0D0
         IF(Z2.GT.ZTEST) FPZ=1.0D0
         IF(Z2.EQ.ZTEST) FPZ=0.0D0
      END IF
!
      write(outlyne,*) fpx,fpy,fpz
      call showit(1)
      MAG=DSQRT((FPX**2)+(FPY**2)+(FPZ**2))
      LN2=FPX/MAG
      MN2=FPY/MAG
      NN2=FPZ/MAG
!       NOT TWO INTERSECTION POINTS
   END IF
!       NOW FIGURE OUT WHICH INTERSECTION POINT TO USE
!       DOT THE INCOMMING DIR COS WITH EACH SURFACE NORMAL
!
   IF(INTERS.EQ.2) THEN
!       THERE ARE TWO INTERSECTION POINTS
      TEST1=((R_L*LN1)+(R_M*MN1)+(R_N*NN1))
      TEST2=((R_L*LN2)+(R_M*MN2)+(R_N*NN2))
      IF(POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     NOT POSRAY
      END IF
      IF(.NOT.POSRAY.AND..NOT.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         ELSE
!       TEST1 NOT >0
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         END IF
         GO TO 200
!     POSRAY
      END IF
      IF(.NOT.POSRAY.AND.REVSTR) THEN
!     FIRST  INTERSECTION POINT = TEST()>0
!     SECOND INTERSECTION POINT = TEST()<0
         IF(TEST1.GT.0.0D0) THEN
            R_X=X1
            R_Y=Y1
            R_Z=Z1
            LN=LN1
            MN=MN1
            NN=NN1
            SEC=1
         ELSE
!       TEST1 NOT >0
            R_X=X2
            R_Y=Y2
            R_Z=Z2
            LN=LN2
            MN=MN2
            NN=NN2
            SEC=2
         END IF
         GO TO 200
!     POSRAY
      END IF
   ELSE
!       NOT TWO INTERSECTIONS
!       JUST ONE INTERSECTION
      R_X=X1
      R_Y=Y1
      R_Z=Z1
      LN=LN1
      MN=MN1
      NN=NN1
   END IF
200 CONTINUE
!
!       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED TORIC
!       USING A CALL TO SUBROUTINE NR3
!       THIS ALSO DEALS WITH SPECIAL SURFACE TYPES:
!                       1, 2 AND 3
!
   INR=surf_inr_value(R_I)
   ERR=.FALSE.
   CALL NR3(ERR)
   IF(ERR) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      STOPP=1
      RETURN
   END IF
   RETURN
!
END
!**********************************************************************
! SUB HITANA.FOR
SUBROUTINE HITANA(OR_N,OR_Z)
!
   use DATLEN
   use DATMAI
   use mod_surface
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE HITANA.FOR. THIS SUBROUTINE IMPLEMENTS
!       SURFACE INTERSECTIONS TO ANAMORPHICS IN RAYTRACING.
!
   REAL*8 A,B,C,QQ,SIGNNU,CV,CC,C1,C2,C3,C4,C5,C6,&
   &C7,C8,C9,C10,C11,C12,C13,C14,NUSUBS,OR_N,OR_Z,&
   &TEST1,TEST2,RR_N,TESTLEN,ZTEST,RR_Z &
   &,Q,SIGNB,MAG,J,ARG,SNINDX,SNIND2 &
   &,FPX,FPY,FPZ,HV0,HV1,HV2,&
   &X1,X2,Y1,Y2,Z1,Z2,LN1,LN2,MN1,MN2,NN1,NN2
!
!     DIFFRACTION GRATING STUFF
   REAL*8 CHIX,CHIY,CHIZ,CHINOR,BLAM,ATERM,BTERM,FACTOR,&
   &SMU,WLU,BGAM,DD,DSPACE,&
   &QX,QY,QZ,PX,PY,PZ,PNOR,QNOR
!
!     HOE STUFF
   REAL*8 RD,R1,R2,LO,MO,NO,LR,MR,NR,LAMC,LAMP,EMM,BTA
!
   LOGICAL REFLEC,GERROR,ERR
!
   INTEGER SPDCD1,SPDCD2,ISURF
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   PHASE=0.0D0
   HV0=0.0D0
   HV1=0.0D0
   HV2=0.0D0
   SNINDX=DABS(ldm%getSurfIndex(R_I-1, INT(WVN)))/ldm%getSurfIndex(R_I-1, INT(WVN))
   SNIND2=DABS(ldm%getSurfIndex(R_I, INT(WVN)))/ldm%getSurfIndex(R_I, INT(WVN))
   R1=0.0D0
   R2=0.0D0
   IF(surf_curvature(R_I).NE.0.0D0) R1=1.0D0/surf_curvature(R_I)
   IF(surf_toric_curvature(R_I).NE.0.0D0) R2=1.0D0/surf_toric_curvature(R_I)
   RD=(R1+R2)/2.0D0
   CV=0.0D0
   IF(RD.NE.0.0D0) CV=1.0D0/RD
!
   IF(CV.NE.0.0D0) ZTEST=1.0D0/CV
!
   RR_N=R_N
   RR_Z=R_Z
   OR_Z=RR_Z
   OR_N=RR_N
!
!       SURFACE IS AN ANAMORPH
!
   IF(R_I.EQ.(NEWOBJ+1)) THEN
      R_X=R_XAIM
      R_Y=R_YAIM
      R_Z=R_ZAIM
!       JUST PROCEED WITH THE DIRECT INTERSECTION TO THE ANAMORPH
   END IF
   NUSUBS=((ldm%getSurfIndex(R_I-1, INT(WVN)))/&
   &(ldm%getSurfIndex(R_I, INT(WVN))))
   SIGNNU=DABS(NUSUBS)/NUSUBS
   NUSUBS=DABS(NUSUBS)
!
!       NOW CALCULATE THE RAY INTERSECTION WITH THE DEFORMED TORIC
!       USING A CALL TO SUBROUTINE NR3
!       THIS ALSO DEALS WITH SPECIAL SURFACE TYPES:
!                       1, 2 AND 3
!
   INR=surf_inr_value(R_I)
   ERR=.FALSE.
   CALL NR3(ERR)
   IF(ERR) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(R_I)
         OUTLYNE='RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(1)
      END IF
      RAYCOD(1)=1
      RAYCOD(2)=R_I
      SPDCD1=RAYCOD(1)
      SPDCD2=R_I
      STOPP=1
      RETURN
   END IF
   RETURN
!
END
