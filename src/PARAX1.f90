!       FIRST SET OF PARAXIAL ROUTINES GO HERE

! SUB PRCOL.FOR
SUBROUTINE PRCOL
!
   use DATLEN
   use DATMAI
   use mod_surface, only: surf_curvature, surf_thickness, surf_toric_flag, surf_toric_curvature, surf_asphere_coeff
   use mod_system, only: sys_last_surf
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PRCOL. THIS DOES A PARAXIAL
!       TRACE AT WAVELENGTH NUMBER WV WITHOUT RAY AIMING
!       OR SOLVE RESOLUTION. ITYP=1=YZ,ITYP=2=XZ
!
   INTEGER ITYP,I,J,L
!
   REAL*8 CURV,WV
!
   COMMON/PRCOM/WV,ITYP
!

   INTEGER WWVN
   IF(INT(WV).EQ.1) WWVN=46
   IF(INT(WV).EQ.2) WWVN=47
   IF(INT(WV).EQ.3) WWVN=48
   IF(INT(WV).EQ.4) WWVN=49
   IF(INT(WV).EQ.5) WWVN=50
   IF(INT(WV).EQ.6) WWVN=71
   IF(INT(WV).EQ.7) WWVN=72
   IF(INT(WV).EQ.8) WWVN=73
   IF(INT(WV).EQ.9) WWVN=74
   IF(INT(WV).EQ.10) WWVN=75
!
   IF(ITYP.EQ.1) THEN
!
!       ALL PARAXIAL TRACING OPERATES ON THE CURRENT LENS
!       THIS COULD BE AN ALTERNATE CONFIGURATION.
!       THE PARAXIAL RAYTRACE PERFORMED HERE IGNORS
!       ALL YZ- PLANE SOLVES.
!
!       THE WAVELENGTH USED IN THE EQUATIONS TO FOLLOW IS
!
!                       WV
!
!               INITIAL VALUES AT SURFACE 0 COME FROM THE
!       PARAXIAL TRACE DATA AT THE CONTROL WAVELENGTH
!***************************************************************
!               INITIAL VALUES AT SURFACE 0
      COLY(1,0)=PXTRAY(1,0)
      COLY(2,0)=PXTRAY(2,0)
      COLY(3,0)=PXTRAY(3,0)
      COLY(4,0)=PXTRAY(4,0)
      COLY(5,0)=PXTRAY(5,0)
      COLY(6,0)=PXTRAY(6,0)
      COLY(7,0)=PXTRAY(7,0)
      COLY(8,0)=PXTRAY(8,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       COLY(1,1)
      COLY(1,1)=PXTRAY(1,1)
!       COLY(2,1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 2) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLY(2,1)=-CURV*COLY(1,1)*&
      &(((ALENS(WWVN,1))-&
      &(ALENS(WWVN,0)))/&
      &(ALENS(WWVN,1)))+&
      &((ALENS(WWVN,0))/&
      &(ALENS(WWVN,1)))*COLY(2,0)
!
!       COLY(3,1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 2) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLY(3,1)=CURV*COLY(1,1)+COLY(2,0)
!       COLY(4,1)
      COLY(4,1)=((ALENS((WWVN),0))/&
      &(ALENS((WWVN),1)))*COLY(3,1)
!
!       COLY(5,1)
      COLY(5,1)=PXTRAY(5,1)
!
!       COLY(6,1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 2) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLY(6,1)=-CURV*COLY(5,1)*&
      &(((ALENS(WWVN,1))-&
      &(ALENS(WWVN,0)))/&
      &(ALENS(WWVN,1)))+&
      &((ALENS(WWVN,0))/&
      &(ALENS(WWVN,1)))*COLY(6,0)
!
!       COLY(7,1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 2) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLY(7,1)=(CURV*COLY(5,1))+COLY(6,0)
!       COLY(8,1)
      COLY(8,1)=((ALENS((WWVN),0))/&
      &(ALENS((WWVN),1)))*COLY(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
      DO 80 L=2,INT(sys_last_surf())
!               VALUES AT SURFACE L
!******************************************************************************
!       NOW CALCULATE COLY(1,L) VALUE
         COLY(1,L)=COLY(1,(L-1))+(surf_thickness(L-1)*COLY(2,(L-1)))
!******************************************************************************
!       NOW CALCULATE COLY(2,L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 2) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLY(2,L)=-CURV*COLY(1,L)*&
         &(((ALENS(WWVN,L))-&
         &(ALENS(WWVN,(L-1))))/&
         &(ALENS(WWVN,L)))+&
         &((ALENS(WWVN,(L-1)))/&
         &(ALENS(WWVN,L)))*COLY(2,(L-1))
!*****************************************************************
!       COLY(3,L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 2) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLY(3,L)=CURV*COLY(1,L)+COLY(2,(L-1))
!*******************************************************************
!       COLY(4,L)
         COLY(4,L)=((ALENS((WWVN),(L-1)))/&
         &(ALENS((WWVN),L)))*COLY(3,L)
!*******************************************************************
!       COLY(5,L)
         COLY(5,L)=COLY(5,(L-1))+(surf_thickness(L-1)*COLY(6,(L-1)))
!************************************************************************
!       COLY(6,L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 2) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLY(6,L)=-CURV*COLY(5,L)*&
         &(((ALENS(WWVN,L))-&
         &(ALENS(WWVN,(L-1))))/&
         &(ALENS(WWVN,L)))+&
         &((ALENS(WWVN,(L-1)))/&
         &(ALENS(WWVN,L)))*COLY(6,(L-1))
!**************************************************************
!       COLY(7,L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 2) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLY(7,L)=(CURV*COLY(5,L))+COLY(6,(L-1))
!***************************************************************
         COLY(8,L)=((ALENS((WWVN),(L-1)))/&
         &(ALENS((WWVN),L)))*COLY(7,L)
!***************************************************************
80    CONTINUE
!       PARAXIAL TRACE COMPLETED
      RETURN
   ELSE
!       ITYP NOT 1
   END IF
   IF(ITYP.EQ.2) THEN
!
!       THE WAVELENGTH USED IN THE EQUATIONS TO FOLLOW IS
!
!                       WV
!
!               INITIAL VALUES AT SURFACE 0 COME FROM THE
!       PARAXIAL TRACE DATA AT THE CONTROL WAVELENGTH
!***************************************************************
!               INITIAL VALUES AT SURFACE 0
      COLX(1,0)=PXTRAX(1,0)
      COLX(2,0)=PXTRAX(2,0)
      COLX(3,0)=PXTRAX(3,0)
      COLX(4,0)=PXTRAX(4,0)
      COLX(5,0)=PXTRAX(5,0)
      COLX(6,0)=PXTRAX(6,0)
      COLX(7,0)=PXTRAX(7,0)
      COLX(8,0)=PXTRAX(8,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       COLX(1,1)
      COLX(1,1)=PXTRAX(1,1)
!       COLX(2,1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 1) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLX(2,1)=-CURV*COLX(1,1)*&
      &(((ALENS(WWVN,1))-&
      &(ALENS(WWVN,0)))/&
      &(ALENS(WWVN,1)))+&
      &((ALENS(WWVN,0))/&
      &(ALENS(WWVN,1)))*COLX(2,0)
!
!       COLX(3,1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 1) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLX(3,1)=CURV*COLX(1,1)+COLX(2,0)
!       COLX(4,1)
      COLX(4,1)=((ALENS((WWVN),0))/&
      &(ALENS((WWVN),1)))*COLX(3,1)
!
!       COLX(5,1)
      COLX(5,1)=PXTRAX(5,1)
!
!       COLX(6,1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 1) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLX(6,1)=-CURV*COLX(5,1)*&
      &(((ALENS(WWVN,1))-&
      &(ALENS(WWVN,0)))/&
      &(ALENS(WWVN,1)))+&
      &((ALENS(WWVN,0))/&
      &(ALENS(WWVN,1)))*COLX(6,0)
!
!       COLX(7,1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
      IF(surf_toric_flag(1) == 1) THEN
         CURV=surf_toric_curvature(1)
      ELSE
         IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
            CURV=surf_asphere_coeff(1,2)*2.0D0
         ELSE
            CURV=surf_curvature(1)
         END IF
      END IF
      COLX(7,1)=(CURV*COLX(5,1))+COLX(6,0)
!       COLX(8,1)
      COLX(8,1)=((ALENS((WWVN),0))/&
      &(ALENS((WWVN),1)))*COLX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
      DO 800 L=2,INT(sys_last_surf())
!               VALUES AT SURFACE L
!******************************************************************************
!       NOW CALCULATE COLX(1,L) VALUE
         COLX(1,L)=COLX(1,(L-1))+(surf_thickness(L-1)*COLX(2,(L-1)))
!******************************************************************************
!       NOW CALCULATE COLX(2,L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 1) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLX(2,L)=-CURV*COLX(1,L)*&
         &(((ALENS(WWVN,L))-&
         &(ALENS(WWVN,(L-1))))/&
         &(ALENS(WWVN,L)))+&
         &((ALENS(WWVN,(L-1)))/&
         &(ALENS(WWVN,L)))*COLX(2,(L-1))
!*****************************************************************
!       COLX(3,L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 1) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLX(3,L)=CURV*COLX(1,L)+COLX(2,(L-1))
!*******************************************************************
!       COLX(4,L)
         COLX(4,L)=((ALENS((WWVN),(L-1)))/&
         &(ALENS((WWVN),L)))*COLX(3,L)
!*******************************************************************
!       COLX(5,L)
         COLX(5,L)=COLX(5,(L-1))+(surf_thickness(L-1)*COLX(6,(L-1)))
!************************************************************************
!       COLX(6,L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 1) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLX(6,L)=-CURV*COLX(5,L)*&
         &(((ALENS(WWVN,L))-&
         &(ALENS(WWVN,(L-1))))/&
         &(ALENS(WWVN,L)))+&
         &((ALENS(WWVN,(L-1)))/&
         &(ALENS(WWVN,L)))*COLX(6,(L-1))
!**************************************************************
!       COLX(7,L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(L) == 1) THEN
            CURV=surf_toric_curvature(L)
         ELSE
            IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(L,2)*2.0D0
            ELSE
               CURV=surf_curvature(L)
            END IF
         END IF
         COLX(7,L)=(CURV*COLX(5,L))+COLX(6,(L-1))
!***************************************************************
         COLX(8,L)=((ALENS((WWVN),(L-1)))/&
         &(ALENS((WWVN),L)))*COLX(7,L)
!***************************************************************
800   CONTINUE
!       PARAXIAL TRACE COMPLETED
      RETURN
   ELSE
!       ITYP NOT 2
   END IF
END

! SUB PRTRA.FOR
SUBROUTINE PRTRA
   use paraxial_ray_trace_test
   CALL PRTRA_NEW
   !CALL PRTRA_OLD

END SUBROUTINE


SUBROUTINE PRTRA_OLD
   use iso_fortran_env, only: real64
   use type_utils, only: real2str, bool2str
   use parax_calcs
!
   use global_widgets


   use DATLEN
   use DATMAI
   use mod_surface, only: surf_curvature, surf_thickness, surf_toric_flag, surf_toric_curvature, surf_asphere_coeff, surf_ideal_efl, surf_pickup_count
   use mod_system, only: sys_astop, sys_last_surf, sys_sax, sys_say, sys_scx, sys_scy, &
      & sys_telecentric, sys_wl_ref, sys_x1_scx, sys_y1_scy, &
      & sys_set_x1_scx, sys_set_y1_scy
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PRTRA. THIS IS THE
!       SUBROUTINE WHICH IMPLEMENTS THE PARAXIAL RAY TRACE.
!       IT IS CALLED FROM LNSEOS. THIS SUBROUTINE
!       TRACES IN THE YZ PLANE IF ITYPW=1, THE XZ PLANE
!       IF ITYPEP=2 AND THE YZ AND XZ PLANE IF ITYPEP=3.
!       PIKUPS ARE ALSO RESOLVED BY
!       CALLS TO PIKRES FOR EACH SURFACE FROM THIS
!       SUBROUTINE. THE COLOR CONTRIBUTIONS FOR THE YZ PLANE
!       ARE CALCULATED WITH A CALL TO CCOL(1) AND XZ BY CCOL(2).
!
   INTEGER LL,CW,ITYPEP,I,J,JK,L,K,ITYP,SLV1,SLV2,COMI, SF, JJ
!
   COMMON/PTYPER/ITYPEP
!
   COMMON/CSLVRS/SLV1,SLV2

   REAL*8 RD, INDEX, VNUM
   real(kind=real64) :: Lo, thetao, nao
   real(kind=real64) :: marPos1, marAng0

!
   REAL*8 SYS13,TMP15A,TMP15B,&
   &CON,CURV,TMP17A,TMP17B,WV
!
   COMMON/PRCOM/WV,ITYP
!
   COMMON/PIKCOM/COMI
!
   INTEGER WWVN
   IF(INT(sys_wl_ref()).EQ.1) WWVN=46
   IF(INT(sys_wl_ref()).EQ.2) WWVN=47
   IF(INT(sys_wl_ref()).EQ.3) WWVN=48
   IF(INT(sys_wl_ref()).EQ.4) WWVN=49
   IF(INT(sys_wl_ref()).EQ.5) WWVN=50
   IF(INT(sys_wl_ref()).EQ.6) WWVN=71
   IF(INT(sys_wl_ref()).EQ.7) WWVN=72
   IF(INT(sys_wl_ref()).EQ.8) WWVN=73
   IF(INT(sys_wl_ref()).EQ.9) WWVN=74
   IF(INT(sys_wl_ref()).EQ.10) WWVN=75
!

   OUTLYNE = "PRTRA ROUTINE STARTED! "
   CALL SHOWIT(19)



!       Update Lens data class
!     Dump data to interface modules
   call curr_lens_data%update()

   ! To deal with setting this properly with
   ! finite conjugate sytems and support the use case of this
   ! sub to ray trace to compute first order parameters
   call computeMarginalRayPosition(marPos1, marAng0)
   ! Calc Object NA
   ! Marginal ray height on surface 1
   ! For finite conjugate object, should be object NA % thickness of surface 0
   Lo = curr_lens_data%thicknesses(1)+ENPUZ
   PRINT *, "thick1 is ", curr_lens_data%thicknesses(1)
   PRINT *, "ENPUPOS is ", ENPUZ
   PRINT *, "Lo is ", Lo
   PRINT *, "sys_say() is ", sys_say()
   print *, "2*Lo is ", (2*Lo)
   thetao = ATAN(sys_say()/(Lo))

   PRINT *, "theta0 is ", thetao
   nao = sin(thetao)
   print *, "nao is ", nao
   print *, "new chief ray pos on surf 1 is ",&
   &tan(thetao)*curr_lens_data%thicknesses(1)



! C       PRINT THE PIKUP

!         ! Pickup storage
!         curr_lens_data % pickups(1:6,JJ+1,1:45) = PIKUP(1:6,JJ,1:45)
!         curr_lens_data % solves(1:6,JJ+1) = SOLVE(1:6,JJ)

!         !ARE THERE MORE PIKUPS? IF NOT SET surf_pickup_count(SURF) TO ZERO.


!       END DO

   !Refresh lens editor
   !PRINT *, "lens_editor_window is ", lens_editor_window
   !PRINT *, "c_Ass ", c_associated(lens_editor_window)


   CON=sys_y1_scy()
   IF(ITYPEP.EQ.1 .OR.ITYPEP.EQ.3) THEN
!
!       THIS IS THE FIRST OF THE PARAXIAL RAY TRACING SUBROUTINES
!       IT IS AUTOMATICALLY CALLED FROM LNSEOS AFTER LENS INPUT
!       OR LENS UPDATE AND IS ALSO CALLED WHEN SPECIFIC PARAXIAL
!       DATA IS NEEDED WHICH IS NOT ALREADY ON-FILE IN THE
!       PARAXIAL RAYTRACE STORAGE ARRAY.
!
!       ALL PARAXIAL TRACING OPERATES ON THE CURRENT LENS
!       THIS COULD BE AN ALTERNATE CONFIGURATION.
!       THE PARAXIAL RAYTRACE PERFORMED HERE HANDLES
!       ALL YZ- PLANE SOLVES THROUGH A CALL TO SUBROUTINE
!       SLVRSY.
!       7/23/91 SET CON = sys_y1_scy() FOR THE YZ PLANE TRACE
!
!       IF AN APERTURE STOP IS DEFINED ON ANY SURFACE
!       THE VALUE OF sys_y1_scy() NEEDS TO BE REFINED.
!
!       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
!       UP TO THE APERTURE STOP SURFACE (UNLESS THERE IS NO
!       APERTURE STOP DEFINED) USING TWO DIFFERENT VALUES
!       OF sys_y1_scy() [HEIGTH OF CHIEF RAY AT SURF 1]
!
!       THE TWO VALUES USED ARE 0.0 AND 0.1
!
!       THE CORRECET VALUE OF sys_y1_scy() WHICH MAKES PCY ON THE
!       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
!
!       PCY(AT ASTOP FOR sys_y1_scy()=0.0) IS CALLED TMP15A
!       PCY(AT ASTOP FOR sys_y1_scy()=0.1) IS CALLED TMP15B
!
!       sys_y1_scy()=((-.1*TMP15A)/(TMP15B-TMP15A))+sys_y1_scy()
!
      IF(sys_astop().GT.0.0D0 .AND.sys_telecentric().EQ.0.0D0) THEN
!
!       RECALCULATE THE CORRECT VALUE OF sys_y1_scy()
!       OTHERWISE, USE THE USER PROVIDED VALUE OF sys_y1_scy()
!
!                       RAY TARGETING INFORMATION
!
         DO 60 JK=1,2
            IF(JK.EQ.1) CON=CON+0.0D0
            IF(JK.EQ.2) CON=CON+0.1D0
!*************************************************************************
!       INITIAL TARGET OF RAY TRACE
!               THE INITIAL PARAXIAL RAYTRACE TARGETING
!               DATA IS:
!                       AT SURFACE 0 (OBJ)
!
!       STARTING MARGINAL RAY HEIGHT = 0
!       STARTING CHIEF RAY HEIGHT    = SCY
!                       AT SURFACE 1 (INITIAL REF SURF)
!       STARTING MARGINAL RAY HEIGHT = SAY
!       STARTING CHIEF RAY HEIGHT = CON
!
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!       CALL PIKRES FOR THE OBJECT SURFACE
            COMI=0
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(0)=0,  ALWAYS
            PXTRAY(1,0)=0.0D0
!
!       PUY(0)=SAY/TH(0)
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            ! Experimental code !  go back to original and fix it correctly!
            PXTRAY(2,0) = marAng0
!         if (ENPUZ == 0) then
!                 PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!                 call LogTermFOR("PRTRA LN 593 isDataValid " //
!      1 bool2str(curr_par_ray_trace%isFirstOrderDataValid()))

!                 call LogTermFOR("Pos should be "//
!      1           real2str(marPos1))
!         else
!                 call LogTermFOR("PRTRA LN 594 isDataValid " //
!      1 bool2str(curr_par_ray_trace%isFirstOrderDataValid()))
!                 call computeMarginalRayPosition(marPos1, marAng0)
!                 call LogTermFOR("Pos should be "//
!      1           real2str(marPos1))
!                 PXTRAY(2,0)=nao
!         end if

            ! Original Code!!
            !        PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!
!       PIY(0) =PUY(0)
            PXTRAY(3,0)=PXTRAY(2,0)
!
!       PIY'(0)=PUY(0)
            PXTRAY(4,0)=PXTRAY(3,0)
!
!       PCY(0) =-SCY
            PXTRAY(5,0)=(sys_scy())
            IF(sys_scy().EQ.0.0D0) PXTRAY(5,0)=1.0D0
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       CON IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAY(6,0)=-((sys_scy())-CON)/surf_thickness(0)
            IF(sys_scy().EQ.0.0D0) PXTRAY(6,0)=&
            &-(1.0D0-CON)/surf_thickness(0)
!
!       PICY(0) AT OBJECT, PICY = PUCY
            PXTRAY(7,0)=PXTRAY(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
            PXTRAY(8,0)=PXTRAY(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
            !JN:  If there is a mag solve set thickness here:
            L = 0
            IF(SOLVE(6,L).NE.0.0D0) THEN
               SLV1 = L
               CALL SLVRS
            END IF

            L=1
            SLV1=L
            SLV2=1
            IF(SOLVE(6,L).NE.0.0D0 .OR.&
            &SOLVE(8,L).NE.0.0D0) THEN
               CALL SLVRS
            END IF
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
            COMI=1
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!


            ! Experimental code !  go back to original and fix it correctly!
            PXTRAY(1,1) = marPos1
            ! if (ENPUZ == 0) then
            !         PXTRAY(1,1)=(sys_say())
            ! else
            !         PXTRAY(1,1)=tan(thetao)*curr_lens_data%thicknesses(1)
            ! end if
            ! PRINT *, "PXTRAY(1,1) i ", PXTRAY(1,1)
            ! Original code
!       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN sys_say()
            ! PXTRAY(1,1)=(sys_say())
!

!       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
!
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(2,1)=-CURV*PXTRAY(1,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAY(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAY(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAY(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(2,1-1)

!
!       PIY(1)=CV(1)*PY(1)+PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
!
!       PIY'(1)=(N/N')*PIY(1)
            PXTRAY(4,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAY(3,1)
!
!       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            PXTRAY(5,1)=CON
!
!       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(6,1)=-CURV*PXTRAY(5,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAY(6,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAY(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(5,1))+PXTRAY(6,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAY(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(5,1))+PXTRAY(6,1-1)
!
!       PICY(1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
!       PICY'(1)
            PXTRAY(8,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAY(7,1)

            if(DABS(marAng0).GT.001D0) then
               PRINT *, "Finite mar0 PRTRA PXTRAY(1:8,0:1) is ",&
               &PXTRAY(1:8,0:1)
            end if
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:

            DO 50 L=2,INT(sys_astop())
!               VALUES AT SURFACE L
               SLV1=L
               SLV2=1
               IF(SOLVE(6,L).NE.0.0D0 .OR.&
               &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!       CALL PIKRES FOR THE SURFACE L
               COMI=L
               IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
               PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
               PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
!       WHEN ASTOP IS NOT ON SURFACE 1
!
50          CONTINUE
            IF(JK.EQ.1) TMP15A=PXTRAY(5,(INT(sys_astop())))
            IF(JK.EQ.2) TMP15B=PXTRAY(5,(INT(sys_astop())))
60       CONTINUE
         IF(TMP15A.EQ.TMP15B) THEN
            OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
            CALL SHOWIT(1)
            OUTLYNE='APERTURE STOP SURFACE.'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            call sys_set_y1_scy(((-0.1D0*TMP15A)/(TMP15B-TMP15A))+sys_y1_scy())
         END IF
!
!       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
!       USING THIS VALUE OF sys_y1_scy()
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(0)=0,  ALWAYS
         PXTRAY(1,0)=0.0D0
         PXTRAY(2,0) = marAng0
         ! Experimental code !  go back to original and fix it correctly!
         ! if (ENPUZ == 0) then
         !         PXTRAY(2,0)=(sys_say())/surf_thickness(0)
         ! else
         !         PXTRAY(2,0)=nao
         ! end if

!
!       PUY(0)=SAY/TH(0)
         !        PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!
!       PIY(0) =PUY(0)
         PXTRAY(3,0)=PXTRAY(2,0)
!
!       PIY'(0)=PUY(0)
         PXTRAY(4,0)=PXTRAY(3,0)
!
!       PCY(0) =-SCY
         PXTRAY(5,0)=(sys_scy())
         IF(sys_scy().EQ.0.0D0) PXTRAY(5,0)=1.0D0
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_y1_scy() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         PXTRAY(6,0)=-((sys_scy())-sys_y1_scy())/surf_thickness(0)
         IF(sys_scy().EQ.0.0D0) PXTRAY(6,0)=&
         &-(1.0D0-sys_y1_scy())/surf_thickness(0)
!
!       PICY(0) AT OBJECT, PICY = PUCY
         PXTRAY(7,0)=PXTRAY(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
         PXTRAY(8,0)=PXTRAY(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
         L=1
         SLV1=L
         SLV2=1
         IF(SOLVE(6,L).NE.0.0D0 .OR.&
         &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES

         PXTRAY(1,1) = marPos1
         ! Experimental code !  go back to original and fix it correctly!

         ! if (ENPUZ == 0) then
         !         PXTRAY(1,1)=(sys_say())
         ! else
         !         PXTRAY(1,1)=tan(thetao)*curr_lens_data%thicknesses(1)
         ! end if
         ! PRINT *, "PXTRAY(1,1) i ", PXTRAY(1,1)
!
!       Original Code
!       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN sys_say()
         !PXTRAY(1,1)=(sys_say())
!
!       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(2,1)=-CURV*PXTRAY(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
!
!       PIY(1)=CV(1)*PY(1)+PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
!
!       PIY'(1)=(N/N')*PIY(1)
         PXTRAY(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(3,1)
!
!       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         PXTRAY(5,1)=sys_y1_scy()
!
!       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(6,1)=-CURV*PXTRAY(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(5,1))+PXTRAY(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(5,1))+PXTRAY(6,1-1)
!
!       PICY(1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
!       PICY'(1)
         PXTRAY(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
         DO 70 L=2,INT(sys_astop())
            SLV1=L
            SLV2=1
            IF(SOLVE(6,L).NE.0.0D0 .OR.&
            &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
            PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
            PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
!
70       CONTINUE
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
!       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
!       HERE. WE HAVE DEFINED. THIS IS THE
!       APERTURE STOP SURFACE. REDEFINE IT AS L.
         L=INT(sys_astop())
!       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
!       THIS IS DONE BY CALLING SUBROUTINE
         SLV1=L
         SLV2=1
         IF(SOLVE(6,L).NE.0.0D0 .OR.&
         &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!
!               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
!               PROCEED TO NEXT SURFACES
!
!*******************************************************************************
!       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE,
!       CORRECTLY HANDLING SOLVES
!       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
!       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
!       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
!       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
!       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
!
!       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (sys_astop()+1)
!       TO THE IMAGE SURFACE  WHERE:
         DO 90 L=((INT(sys_astop()))+1),INT(sys_last_surf())
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!               VALUES AT SURFACE L
!
!       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
!       NOW CALCULATE PY VALUE
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       FINISHED WITH PY(L)
!*******************************************************************************
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
            PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
!       NO CALCULATE PCY VALUE
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       FINISHED WITH PCY(L)
!************************************************************************
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
            PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
!                       SOLVES ON SURFACE L
!***************************************************************************
!       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
            SLV1=L
            SLV2=1
            IF(SOLVE(6,L).NE.0.0D0 .OR.&
            &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!
!               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
!
90       CONTINUE
!       TRACE COMPLETED
!*******************************************************************************
      ELSE
!
!       NO ASTOP OR TEL SET, USE THE EXISTING VALUE OF sys_y1_scy()
!
!*******************************************************************************
!       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
!       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
!       0 OR 1.
!               INITIAL VALUES AT SURFACE 0
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!
!       PY(0)=0,  ALWAYS
         PXTRAY(1,0)=0.0D0
!
         ! Experimental code !  go back to original and fix it correctly!
         PXTRAY(2,0) = marAng0
         ! if (ENPUZ == 0) then
         !         PXTRAY(2,0)=(sys_say())/surf_thickness(0)
         ! else
         !         PXTRAY(2,0)=nao
         ! end if

!       PUY(0)=SAY/TH(0)
         !PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!
!       PIY(0) =PUY(0)
         PXTRAY(3,0)=PXTRAY(2,0)
!
!       PIY'(0)=PUY(0)
         PXTRAY(4,0)=PXTRAY(3,0)
!
!       PCY(0) =-SCY
         PXTRAY(5,0)=(sys_scy())
         IF(sys_scy().EQ.0.0D0) PXTRAY(5,0)=1.0D0
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_y1_scy() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         IF(sys_telecentric().EQ.0.0D0)&
         &PXTRAY(6,0)=-((sys_scy())-sys_y1_scy())/surf_thickness(0)
         IF(sys_scy().EQ.0.0D0) PXTRAY(6,0)=&
         &-(1.0D0-sys_y1_scy())/surf_thickness(0)
         IF(sys_telecentric().EQ.1.0D0)&
         &PXTRAY(6,0)=0.0D0
!
!       PICY(0) AT OBJECT, PICY = PUCY
         PXTRAY(7,0)=PXTRAY(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
         PXTRAY(8,0)=PXTRAY(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES

         ! Experimental code !  go back to original and fix it correctly!
         PXTRAY(1,1) = marPos1
!         if (ENPUZ == 0) then
!                 PXTRAY(1,1)=(sys_say())
!         else
!                 PXTRAY(1,1)=tan(thetao)*curr_lens_data%thicknesses(1)
!         end if
!         PRINT *, "PXTRAY(1,1) i ", PXTRAY(1,1)
! C
!       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN sys_say()
         !PXTRAY(1,1)=(sys_say())
!
!       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(2,1)=-CURV*PXTRAY(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
!
!       PIY(1)=CV(1)*PY(1)+PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
!
!       PIY'(1)=(N/N')*PIY(1)
         PXTRAY(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(3,1)
!
!       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         IF(sys_telecentric().EQ.0.0D0) PXTRAY(5,1)=sys_y1_scy()
         IF(sys_telecentric().EQ.1.0D0) PXTRAY(5,1)=PXTRAY(5,0)
!
!       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(6,1)=-CURV*PXTRAY(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(5,1))+PXTRAY(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(5,1))+PXTRAY(6,1-1)
!
!       PICY(1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
!       PICY'(1)
         PXTRAY(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
!       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
!       HERE. REDEFINE IT AS L.
         L=1
!       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE
!       DO THIS BY CALLING SLVRS
         SLV1=L
         SLV2=1
         IF(SOLVE(6,L).NE.0.0D0 .OR.&
         &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
!               PROCEED TO NEXT SURFACES
! *****************************************************************************
!       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
!       CORRECTLY HANDLING SOLVES
!       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
!       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
!       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
!       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
!       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
         DO 80 L=2,INT(sys_last_surf())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!*******************************************************************************
!       PY(L)=PY(L-1)+CV(L-1)*PUY(L-1)
!       NOW CALCULATE PY VALUE
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       FINISHED WITH PY(L)
!
!*******************************************************************************
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
            PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
!*******************************************************************************
!       NO CALCULATE PCY VALUE
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       FINISHED WITH PCY(L)
!************************************************************************
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(5,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
            PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
!                       NOW HANDLE SOLVES ON SURFACE L
!***************************************************************************
!       DO THIS BY CALLING SLVRS
            SLV1=L
            SLV2=1
            IF(SOLVE(6,L).NE.0.0D0 .OR.&
            &SOLVE(8,L).NE.0.0D0) CALL SLVRS
!       ALL SOLVES ON SURFACE L HANDLED
!
80       CONTINUE
!       PARAXIAL TRACE COMPLETED
      END IF
!       NOW CALL ENEXES TO RESOLVE ANY ASTOP EN AND/OR EX
!       ENTRANCE/EXIT PUPIL ADJUSTMENTS.
!       THIS CALL IS ONLY MADE FROM PRTRA1 AND ONLY
!       USES YZ-PLANE DATA.
      CALL ENEXRS
!
   ELSE
!       ITYPEP NOT 1 OR 3
   END IF
!
!       SET CON = sys_x1_scx()
   CON=sys_x1_scx()
!
   IF(ITYPEP.EQ.2 .OR.ITYPEP.EQ.3) THEN
!
!       THIS IS SUBROUTINE PRTRA2. THIS IS THE FIRST OF THE
!       SUBROUTINES WHICH IMPLEMENT THE PARAXIAL RAY TRACE.
!       IT IS CALLED FROM LNSEOS. THIS SUBROUTINE
!       TRACES ONLY IN THE XZ PLANE. PIKUPS ARE ALSO RESOLVED BY
!       CALLS TO PIKRES FOR EACH SURFACE FROM THIS
!       SUBROUTINE. THE XZ PLANE CROMATIC SURFACE COEFICIENTS
!       ARE CALCULATED WITH A CALL TO CCOLX.FOR
!
      SYS13=sys_sax()
!
!       THIS IS THE FIRST OF THE PARAXIAL RAY TRACING SUBROUTINES
!       IT IS AUTOMATICALLY CALLED FROM LNSEOS AFTER LENS INPUT
!       OR LENS UPDATE AND IS ALSO CALLED WHEN SPECIFIC PARAXIAL
!       DATA IS NEEDED WHICH IS NOT ALREADY ON-FILE IN THE
!       PARAXIAL RAYTRACE STORAGE ARRAY.
!
!       ALL XZ PLANE SOLVES ARE HANDLED THROUGH CALLS TO
!       SUBROUTINE SLVRSX
!
!       ALL PARAXIAL TRACING OPERATES ON THE CURRENT LENS
!       THIS COULD BE AN ALTERNATE CONFIGURATION.
!
!       IF AN APERTURE STOP IS DEFINED ON ANY SURFACE
!       THE VALUE OF sys_x1_scx() NEEDS TO BE
!       REFINED.
!
!       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
!       UP TO THE APERTURE STOP SURFACE (WHEN THE APERTURE STOP
!       IS NOT ON SURFACE 1) USING TWO DIFFERENT VALUES
!       OF sys_x1_scx() [HEIGTH OF CHIEF RAY AT SURF 1]
!
!       THE TWO VALUES USED ARE 0.0 AND 0.1
!
!       THE CORRECET VALUE OF sys_x1_scx() WHICH MAKES PCX ON THE
!       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
!
!       PCX(AT ASTOP FOR sys_x1_scx()=0.0) IS CALLED TMP17A
!       PCX(AT ASTOP FOR sys_x1_scx()=0.1) IS CALLED TMP17B
!
!       sys_x1_scx()=((-.1D0*TMP17A)/(TMP17B-TMP17A))+sys_x1_scx()
!
      IF(sys_astop().GT.0.0D0 .AND.sys_telecentric().EQ.0.0D0) THEN
!
!       RECALCULATE THE CORRECT VALUE OF sys_x1_scx()
!       OTHERWISE, USE THE USER PROVIDED VALUE OF sys_x1_scx()
!
!                       RAY
         DO 6000 JK=1,2
            IF(JK.EQ.1) CON=CON+0.0D0
            IF(JK.EQ.2) CON=CON+0.1D0
!*************************************************************************
!       INITIAL TARGET OF RAY TRACE
!               THE INITIAL PARAXIAL RAYTRACE TARGETING
!               DATA IS:
!                       AT SURFACE 0 (OBJ)
!
!       STARTING MARGINAL RAY HEIGHT = 0
!       STARTING CHIEF RAY HEIGHT    = SCX
!                       AT SURFACE 1 (INITIAL REF SURF)
!       STARTING MARGINAL RAY HEIGHT = SAX
!
!       STARTING CHIEF RAY HEIGHT = CON
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!       CALL PIKRES FOR THE OBJECT SURFACE
            COMI=0
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(0)=0,  ALWAYS
            PXTRAX(1,0)=0.0D0
!
!       PUX(0)=SAX/TH(0)
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAX(2,0)=(SYS13)/surf_thickness(0)
!
!       PIX(0) =PUX(0)
            PXTRAX(3,0)=PXTRAX(2,0)
!
!       PIX'(0)=PUX(0)
            PXTRAX(4,0)=PXTRAX(3,0)
!
!       PCX(0) =-SCX
            PXTRAX(5,0)=(sys_scx())
            IF(sys_scx().EQ.0.0D0) PXTRAX(5,0)=1.0D0
!
!       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
!       CON IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAX(6,0)=-((sys_scx())-CON)/surf_thickness(0)
            IF(sys_scx().EQ.0.0D0) PXTRAX(6,0)=&
            &-(1.0D0-CON)/surf_thickness(0)
!
!       PICX(0) AT OBJECT, PICX = PUCX
            PXTRAX(7,0)=PXTRAX(6,0)
!
!       PICX'(0) AT OBJECT PICX'=PICX
            PXTRAX(8,0)=PXTRAX(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
            L=1
            SLV1=L
            SLV2=2
            IF(SOLVE(4,L).NE.0.0D0 .OR.&
            &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
            COMI=1
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
            PXTRAX(1,1)=(SYS13)
!

!       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
!
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAX(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAX(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(2,1-1)

!
!       PIX(1)=CV(1)*PX(1)+PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
!
!       PIX'(1)=(N/N')*PIX(1)
            PXTRAX(4,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAX(3,1)
!
!       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            PXTRAX(5,1)=CON
!
!       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAX(6,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAX(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(5,1))+PXTRAX(6,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAX(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(5,1))+PXTRAX(6,1-1)
!
!       PICY(1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
!       PICX'(1)
            PXTRAX(8,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
            DO 5000 L=2,INT(sys_astop())
               SLV1=L
               SLV2=2
               IF(SOLVE(4,L).NE.0.0D0 .OR.&
               &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
               COMI=L
               IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
               PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
               PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
!       WHEN ASTOP IS NOT ON SURFACE 1
!
5000        CONTINUE
            IF(JK.EQ.1) TMP17A=PXTRAX(5,(INT(sys_astop())))
            IF(JK.EQ.2) TMP17B=PXTRAX(5,(INT(sys_astop())))
6000     CONTINUE
         IF(TMP17A.EQ.TMP17B) THEN
            OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
            CALL SHOWIT(1)
            OUTLYNE='APERTURE STOP SURFACE.'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            call sys_set_x1_scx(((-0.1D0*TMP17A)/(TMP17B-TMP17A))+sys_x1_scx())
         END IF
!
!       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
!       USING THIS VALUE OF sys_x1_scx()
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(0)=0,  ALWAYS
         PXTRAX(1,0)=0.0D0
!
!       PUX(0)=SAX/TH(0)
         PXTRAX(2,0)=(SYS13)/surf_thickness(0)
!
!       PIX(0) =PUX(0)
         PXTRAX(3,0)=PXTRAX(2,0)
!
!       PIX'(0)=PUX(0)
         PXTRAX(4,0)=PXTRAX(3,0)
!
!       PCX(0) =-SCX
         PXTRAX(5,0)=(sys_scx())
         IF(sys_scx().EQ.0.0D0) PXTRAX(5,0)=1.0D0
!
!       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_x1_scx() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         PXTRAX(6,0)=-((sys_scx())-sys_x1_scx())/surf_thickness(0)
         IF(sys_scx().EQ.0.0D0) PXTRAX(6,0)=&
         &-(1.0D0-sys_x1_scx())/surf_thickness(0)
!
!       PICX(0) AT OBJECT, PICX = PUCX
         PXTRAX(7,0)=PXTRAX(6,0)
!
!       PICX'(0) AT OBJECT PICX'=PICX
         PXTRAX(8,0)=PXTRAX(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!

         SLV1=L
         SLV2=2
         IF(SOLVE(4,L).NE.0.0D0 .OR.&
         &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
         PXTRAX(1,1)=(SYS13)
!
!       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(2,1)=-CURV*PXTRAX(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
!
!       PIX(1)=CV(1)*PX(1)+PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
!
!       PIX'(1)=(N/N')*PIX(1)
         PXTRAX(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(3,1)
!
!       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         PXTRAX(5,1)=sys_x1_scx()
!
!       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(6,1)=-CURV*PXTRAX(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(5,1))+PXTRAX(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(5,1))+PXTRAX(6,1-1)
!
!       PICX(1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
!       PICX'(1)
         PXTRAX(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
         DO 7000 L=2,INT(sys_astop())
            SLV1=L
            SLV2=2
            IF(SOLVE(4,L).NE.0.0D0 .OR.&
            &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
            PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
            PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
!
7000     CONTINUE
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
!       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
!       HERE. WE HAVE DEFINED . THIS IS THE
!       APERTURE STOP SURFACE. REDEFINE IT AS L.
         L=INT(sys_astop())
!       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
!       THIS IS DONE BY CALLING SUBROUTINE
!                               SLVRS
         SLV1=L
         SLV2=2
         IF(SOLVE(4,L).NE.0.0D0 .OR.&
         &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!
!               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
!               PROCEED TO NEXT SURFACES
!
!*******************************************************************************
!       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE,
!       CORRECTLY HANDLING SOLVES
!       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
!       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
!       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
!       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
!       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
!
!       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (SYSTEM+1)
!       TO THE IMAGE SURFACE  WHERE:
         DO 9000 L=((INT(sys_astop()))+1),INT(sys_last_surf())
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!               VALUES AT SURFACE L
!
!       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
!       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       FINISHED WITH PX(L)
!*******************************************************************************
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
            PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
!       NO CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       FINISHED WITH PCX(L)
!************************************************************************
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
            PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
!                       SOLVES ON SURFACE L
!***************************************************************************
!       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
!                       SLVRS
            SLV1=L
            SLV2=2
            IF(SOLVE(4,L).NE.0.0D0 .OR.&
            &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!
!               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
!
9000     CONTINUE
!       TRACE COMPLETED
!*******************************************************************************
      ELSE
!
!       NO ASTOP ASSIGNED OR TEL ON, USE THE EXISTING VALUE OF sys_x1_scx()
!
!*******************************************************************************
!       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
!       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
!       0 OR 1.
!               INITIAL VALUES AT SURFACE 0
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!
!       PX(0)=0,  ALWAYS
         PXTRAX(1,0)=0.0D0
!
!       PUX(0)=SAX/TH(0)
         PXTRAX(2,0)=(SYS13)/surf_thickness(0)
!
!       PIX(0) =PUX(0)
         PXTRAX(3,0)=PXTRAX(2,0)
!
!       PIX'(0)=PUX(0)
         PXTRAX(4,0)=PXTRAX(3,0)
!
!       PCX(0) =-SCX
         PXTRAX(5,0)=(sys_scx())
         IF(sys_scx().EQ.0.0D0) PXTRAX(5,0)=1.0D0
!
!       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_x1_scx() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         IF(sys_telecentric().EQ.0.0D0)&
         &PXTRAX(6,0)=-((sys_scx())-sys_x1_scx())/surf_thickness(0)
         IF(sys_scx().EQ.0.0D0) PXTRAX(6,0)=&
         &-(1.0D0-sys_x1_scx())/surf_thickness(0)
         IF(sys_telecentric().EQ.1.0D0)&
         &PXTRAX(6,0)=0.0D0
!
!       PICX(0) AT OBJECT, PICX = PUCX
         PXTRAX(7,0)=PXTRAX(6,0)
!
!       PICX'(0) AT OBJECT PICX'=PICX
         PXTRAX(8,0)=PXTRAX(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
         L=1
         SLV1=L
         SLV2=2
         IF(SOLVE(4,L).NE.0.0D0 .OR.&
         &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
         PXTRAX(1,1)=(SYS13)
!
!       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(2,1)=-CURV*PXTRAX(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
!
!       PIX(1)=CV(1)*PX(1)+PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
!
!       PIX'(1)=(N/N')*PIX(1)
         PXTRAX(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(3,1)
!
!       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         IF(sys_telecentric().EQ.0.0D0) PXTRAX(5,1)=sys_x1_scx()
         IF(sys_telecentric().EQ.1.0D0) PXTRAX(5,1)=PXTRAX(5,0)
!
!       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(6,1)=-CURV*PXTRAX(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(5,1))+PXTRAX(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(5,1))+PXTRAX(6,1-1)
!
!       PICX(1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
!       PICX'(1)
         PXTRAX(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
!       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
!       HERE. REDEFINE IT AS L.
         L=1
!       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE
!       DO THIS BY CALLING SLVRS
         SLV1=L
         SLV2=2
         IF(SOLVE(4,L).NE.0.0D0 .OR.&
         &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!               ALL APERTURE STOP SURFACE SOLVES NOW HANDLED.
!               PROCEED TO NEXT SURFACES
! *****************************************************************************
!       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
!       CORRECTLY HANDLING SOLVES
!       AND PIKUPS ALONG THE WAY. THEN PROCEED. CURVATURE SOLVES
!       AFFECT THE CURVATURE OF THE SURFACE THAT THE SOLVES ARE ON.
!       THICKNESS SOLVES AFFECT THE DISTANCE FROM THE SURFACE THAT THE
!       SOLVES ARE ON TO THE NEXT SURFACE. BECAUSE OF THIS, CURVATURE
!       SOLVES MUST BE HANDLED FIRST FOLLOWED BY THICKNESS SOLVES.
         DO 8000 L=2,INT(sys_last_surf())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!*******************************************************************************
!       PX(L)=PX(L-1)+CV(L-1)*PUX(L-1)
!       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       FINISHED WITH PX(L)
!
!*******************************************************************************
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
            PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
!*******************************************************************************
!       NOW CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       FINISHED WITH PCX(L)
!************************************************************************
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(5,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
            PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
!                       NOW HANDLE SOLVES ON SURFACE L
!***************************************************************************
!       DO THIS BY CALLING SLVRS
            SLV1=L
            SLV2=2
            IF(SOLVE(4,L).NE.0.0D0 .OR.&
            &SOLVE(2,L).NE.0.0D0) CALL SLVRS
!       ALL SOLVES ON SURFACE L HANDLED
!
!
8000     CONTINUE
!       PARAXIAL TRACE COMPLETED
      END IF
!    Populate data object
!     Dump data to paraxial ray trace interface

      SF = INT(sys_last_surf())
      call curr_par_ray_trace%add_lens_data(curr_lens_data)
      PRINT *, "NUM SURFACES IS ", curr_par_ray_trace%num_surfaces

      curr_par_ray_trace%marginal_ray_height = PXTRAY(1,0:SF)
      curr_par_ray_trace%marginal_ray_angle = PXTRAY(2,0:SF)
      curr_par_ray_trace%chief_ray_height = PXTRAY(5,0:SF)
      curr_par_ray_trace%chief_ray_angle = PXTRAY(6,0:SF)
      ! TODO: Not sure these are the right values - check this
      curr_par_ray_trace%chief_ray_aoi = PXTRAY(7,0:SF)
      curr_par_ray_trace%marginal_ray_aoi = PXTRAY(3,0:SF)
!       TMAG
      IF(DABS(PXTRAY(5,0)).GE.1.0D-15) THEN
         curr_par_ray_trace%t_mag=PXTRAY(5,SF)/PXTRAY(5,0)
      ELSE
         curr_par_ray_trace%t_mag=0.0
      END IF



!
   ELSE
!       ITYPEP NOT 2 OR 3)
   END IF
   RETURN

END
! SUB PRTRB.FOR
SUBROUTINE PRTRB
!
   use DATLEN
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PRTRB. THIS IS THE
!       SUBROUTINE WHICH IMPLEMENTS PARAXIAL COLOR TRACE
!       THIS SUBROUTINE
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
!       NOW CALL CCOLY.FOR WHICH CALCULATES THE
!       CHROMATIC ABERRATION SURFACE COEFICIENTS FROM THE YZ PLANE
!       PARAXIAL TRACE
!
   ITYPEP=1
   CALL CCOL
!
!       NOW CALL CCOLX.FOR TO CALCULATE THE XZ-PLANE CHROMATIC
!       ABERRATION SURFACE COEFICIENTS.
!
   ITYPEP=2
   CALL CCOL
!
   RETURN
END
! SUB PRTRC.FOR
SUBROUTINE PRTRC
!
   use DATLEN
   use DATMAI
   use mod_system, only: sys_wl_ref
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PRTRC. THIS IS THE
!       SUBROUTINE WHICH IMPLEMENTS MONOCHROMATIC 3, 5 AND 7 ABERRATIONS
!       THIS SUBROUTINE
!
   INTEGER CW,AITYPE
!
   COMMON/PAS357/CW,AITYPE
!
!

   CW=INT(sys_wl_ref())
   AITYPE=1
   CALL AB357
!
   CW=INT(sys_wl_ref())
   AITYPE=2
   CALL AB357
   RETURN
END
! SUB PRTRD.FOR
SUBROUTINE PRTRD
!
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf, sys_wl_pri1, sys_wl_pri2, sys_wl_ref, &
      & sys_wl_sec1, sys_wl_sec2
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PRTRD. THIS IS THE
!       SUBROUTINE WHICH IMPLEMENTS 3, 5 AND 7 COLOR CONTRIBUTIONS
!       THIS SUBROUTINE
!
   INTEGER CW,I,J,K,ITYP,AITYPE
!
   REAL*8 WV
!
   COMMON/PRCOM/WV,ITYP
!
   COMMON/PAS357/CW,AITYPE
!
!
!       NOW CALL AB357 TO CALCULATE THIRD ORDER ABERRATIONS
!       AND THE CHROMATIC ABERRATION DIFFERENCES.
!
!       SAVE THE ORIGINAL PARAXIAL DATA
   I=INT(sys_last_surf())
   SAVE(1:8,0:I)=PXTRAY(1:8,0:I)
!
!       NOW CALL PRCOL(sys_wl_pri1(),1)
   WV=sys_wl_pri1()
   ITYP=1
   CALL PRCOL
!       SWAP COLY INTO PXTRAY
   I=INT(sys_last_surf())
   PXTRAY(1:8,0:I)=COLY(1:8,0:I)
   CW=INT(sys_wl_pri1())
   AITYPE=1
   CALL AB357
!
!       SAVE THE DATA IN PDF3 AND PDF57
   I=INT(sys_last_surf())
   PDF3(1:10,0:I)=MAB3(1:10,0:I)
   PDF57(1:20,0:I)=MAB57(1:20,0:I)
!
!       NOW CALL PRCOL(sys_wl_pri2(),1)
   WV=sys_wl_pri2()
   ITYP=1
   CALL PRCOL
!       SWAP COLY INTO PXTRAY
   I=INT(sys_last_surf())
   PXTRAY(1:8,0:I)=COLY(1:8,0:I)
!       CALL AB357 AGAIN
   CW=INT(sys_wl_pri2())
   AITYPE=1
   CALL AB357
!       SUBTRACT THE DATA IN PDF3 AND PDF57
   I=INT(sys_last_surf())
   PDF3(1:10,0:I)=PDF3(1:10,0:I)-MAB3(1:10,0:I)
   PDF57(1:20,0:I)=PDF57(1:20,0:I)-MAB57(1:20,0:I)
!       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
!       YZ PLANE HAVE BEEN FORMED
!
!       NOW FOR THE SECONDARY DIFFERENCES
!
!       NOW CALL PRCOL(sys_wl_sec1(),1)
   WV=sys_wl_sec1()
   ITYP=1
   CALL PRCOL
!       SWAP COLY INTO PXTRAY
   I=INT(sys_last_surf())
   PXTRAY(1:8,0:I)=COLY(1:8,0:I)
   CW=INT(sys_wl_sec1())
   AITYPE=1
   CALL AB357
!
!       SAVE THE DATA IN SDF3 AND SDF57
   I=INT(sys_last_surf())
   SDF3(1:10,0:I)=MAB3(1:10,0:I)
   SDF57(1:20,0:I)=MAB57(1:20,0:I)
!
!       NOW CALL PRCOL(sys_wl_sec2(),1)
   WV=sys_wl_sec2()
   ITYP=1
   CALL PRCOL
!       SWAP COLY INTO PXTRAY
   I=INT(sys_last_surf())
   PXTRAY(1:8,I)=COLY(1:8,I)
!       CALL AB357 AGAIN
   CW=INT(sys_wl_sec2())
   AITYPE=1
   CALL AB357
!       SUBTRACT THE DATA IN PDF3 AND PDF57
   I=INT(sys_last_surf())
   SDF3(1:10,0:I)=SDF3(1:10,0:I)-MAB3(1:10,0:I)
   SDF57(1:20,0:I)=SDF57(1:20,0:I)-MAB57(1:20,0:I)
!       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
!       YZ PLANE HAVE BEEN FORMED
!
!       RESTORE OLD YZ PARAXIAL TRACE DATA
!
   I=INT(sys_last_surf())
   PXTRAY(1:8,0:I)=SAVE(1:8,0:I)
   CW=INT(sys_wl_ref())
!
!       SAVE THE ORIGINAL PARAXIAL DATA
   I=INT(sys_last_surf())
   SAVE(1:8,0:I)=PXTRAX(1:8,0:I)
!
!       NOW CALL PRCOL(sys_wl_pri1(),2)
   WV=sys_wl_pri1()
   ITYP=2
   CALL PRCOL
!       SWAP COLX INTO PXTRAX
   I=INT(sys_last_surf())
   PXTRAX(1:8,0:I)=COLX(1:8,0:I)
   CW=INT(sys_wl_pri1())
   AITYPE=2
   CALL AB357
!
!       SAVE THE DATA IN XPDF3 AND XPDF57
   I=INT(sys_last_surf())
   XPDF3(1:10,0:I)=XMAB3(1:10,0:I)
   XPDF57(1:20,0:I)=XMAB57(1:20,0:I)
!
!       NOW CALL PRCOL(sys_wl_pri2(),2)
   WV=sys_wl_pri2()
   ITYP=2
   CALL PRCOL
!       SWAP COLX INTO PXTRAX
   I=INT(sys_last_surf())
   PXTRAX(1:8,0:I)=COLX(1:8,0:I)
!             CALL AB357 AGAIN
   CW=INT(sys_wl_pri2())
   AITYPE=2
   CALL AB357
!       SUBTRACT THE DATA IN PDF3 AND PDF57
   I=INT(sys_last_surf())
   XPDF3(1:10,0:I)=XPDF3(1:10,0:I)-XMAB3(1:10,0:I)
   XPDF57(1:20,0:I)=XPDF57(1:20,0:I)-XMAB57(1:20,0:I)
!       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
!       XZ PLANE HAVE BEEN FORMED
!
!       NOW FOR THE SECONDARY DIFFERENCES
!
!       NOW CALL PRCOL(sys_wl_sec1()2)
   WV=sys_wl_sec1()
   ITYP=2
   CALL PRCOL
!       SWAP COLY INTO PXTRAX
   I=INT(sys_last_surf())
   PXTRAX(1:8,0:I)=COLX(1:8,0:I)
   CW=INT(sys_wl_sec1())
   AITYPE=2
   CALL AB357
!       SAVE THE DATA IN XSDF3 AND XSDF57
   I=INT(sys_last_surf())
   XSDF3(1:10,0:I)=XMAB3(1:10,0:I)
   XSDF57(1:20,0:I)=XMAB57(1:20,0:I)
!
!       NOW CALL PRCOL(sys_wl_sec2(),2)
   WV=sys_wl_sec2()
   ITYP=2
   CALL PRCOL
!       SWAP COLX INTO PXTRAX
   I=INT(sys_last_surf())
   PXTRAX(1:8,0:I)=COLX(1:8,0:I)
!       CALL AB357 AGAIN
   CW=INT(sys_wl_sec2())
   AITYPE=2
   CALL AB357
!       SUBTRACT THE DATA IN XPDF3 AND XPDF57
   I=INT(sys_last_surf())
   XSDF3(1:10,0:I)=XSDF3(1:10,0:I)-XMAB3(1:10,0:I)
   XSDF57(1:20,0:I)=XSDF57(1:20,0:I)-XMAB57(1:20,0:I)
!       ALL OF THE PRIMARY CHROMATIC DIFFERENCES IN THE
!       XZ PLANE HAVE BEEN FORMED
!
!       RESTORE OLD XZ PARAXIAL TRACE DATA AND CALL
!       AB357 FOR THE LAST TIME
!
   I=INT(sys_last_surf())
   PXTRAX(1:8,0:I)=SAVE(1:8,0:I)
   CW=INT(sys_wl_ref())
   AITYPE=1
   CALL AB357
   RETURN
END
! SUB PAROUT.FOR
SUBROUTINE PAROUT

!
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PAROUT. THIS SUBROUTINE IMPLEMENTS
!       THE PARAXIAL RAY TRACE PRINTOUT COMMANDS.
!
!
   INTEGER SF,I,J
!
!
   IF(SST.EQ.1 .OR.S2.EQ.1 .OR.S3.EQ.1 .OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'PARAXIAL RAYTRACE OUTPUT COMMANDS ONLY TAKE QUALIFIER OR'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1 .AND.S1.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'PARAXIAL RAYTRACE OUTPUT COMMANDS TAKE EITHER QUALIFIER OR'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'NUMERIC WORD #1 INPUT BUT NOT BOTH'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(sys_last_surf().EQ.0.0) THEN
      WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1 .AND.WQ.EQ.'ALL') THEN
      SF=INT(sys_last_surf())
      IF(WC.EQ.'PXTX') WRITE(OUTLYNE,6001) INT(F12)
      IF(WC.EQ.'PXTY') WRITE(OUTLYNE,6004) INT(F12)
      IF(WC.EQ.'PITX') WRITE(OUTLYNE,6007) INT(F12)
      IF(WC.EQ.'PITY') WRITE(OUTLYNE,6010) INT(F12)
      IF(WC.EQ.'PRTX') WRITE(OUTLYNE,6013) INT(F12)
      IF(WC.EQ.'PRTY') WRITE(OUTLYNE,6016) INT(F12)
      CALL SHOWIT(0)
      IF(WC.EQ.'PXTX') WRITE(OUTLYNE,6002)
      IF(WC.EQ.'PXTY') WRITE(OUTLYNE,6005)
      IF(WC.EQ.'PITX') WRITE(OUTLYNE,6008)
      IF(WC.EQ.'PITY') WRITE(OUTLYNE,6011)
      IF(WC.EQ.'PRTX') WRITE(OUTLYNE,6014)
      IF(WC.EQ.'PRTY') WRITE(OUTLYNE,6017)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,2501)
      CALL SHOWIT(0)
      IF(WC.EQ.'PXTX') WRITE(OUTLYNE,6000)
      IF(WC.EQ.'PXTY') WRITE(OUTLYNE,6003)
      IF(WC.EQ.'PITX') WRITE(OUTLYNE,6006)
      IF(WC.EQ.'PITY') WRITE(OUTLYNE,6009)
      IF(WC.EQ.'PRTX') WRITE(OUTLYNE,6012)
      IF(WC.EQ.'PRTY') WRITE(OUTLYNE,6015)
      CALL SHOWIT(0)



      DO I=0,SF
         DO J=1,8
            IF(DABS(PXTRAX(J,I)).LT.1.0D-14) PXTRAX(J,I)=0.0D0
            IF(DABS(PXTRAY(J,I)).LT.1.0D-14) PXTRAY(J,I)=0.0D0
         END DO
      END DO
      DO 10 I=0,SF
         IF(WC.EQ.'PXTX') WRITE(OUTLYNE,2000)I,PXTRAX(1,I),PXTRAX(2,I)&
         &,PXTRAX(5,I),PXTRAX(6,I)
         IF(WC.EQ.'PXTY') WRITE(OUTLYNE,2000)I,PXTRAY(1,I),PXTRAY(2,I)&
         &,PXTRAY(5,I),PXTRAY(6,I)
         IF(WC.EQ.'PITX') WRITE(OUTLYNE,2000)I,PXTRAX(1,I),PXTRAX(3,I)&
         &,PXTRAX(5,I),PXTRAX(7,I)
         IF(WC.EQ.'PITY') WRITE(OUTLYNE,2000)I,PXTRAY(1,I),PXTRAY(3,I)&
         &,PXTRAY(5,I),PXTRAY(7,I)
         IF(WC.EQ.'PRTX') WRITE(OUTLYNE,2000)I,PXTRAX(1,I),PXTRAX(4,I)&
         &,PXTRAX(5,I),PXTRAX(8,I)
         IF(WC.EQ.'PRTY') WRITE(OUTLYNE,2000)I,PXTRAY(1,I),PXTRAY(4,I)&
         &,PXTRAY(5,I),PXTRAY(8,I)
         CALL SHOWIT(0)
10    CONTINUE
      RETURN
   END IF
   IF(SQ.EQ.1 .AND.WQ.EQ.'OBJ'.OR.SQ.EQ.1 .AND.WQ.EQ.'OB') THEN
      SF=0
      IF(WC.EQ.'PXTX')THEN
         IF(HEADIN) THEN
         END IF
         IF(HEADIN) WRITE(OUTLYNE,6000)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(2,SF)&
         &,PXTRAX(5,SF),PXTRAX(6,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PXTY')THEN
         IF(HEADIN) WRITE(OUTLYNE,6003)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(2,SF)&
         &,PXTRAY(5,SF),PXTRAY(6,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PITX')THEN
         IF(HEADIN) WRITE(OUTLYNE,6006)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(3,SF)&
         &,PXTRAX(5,SF),PXTRAX(7,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PITY')THEN
         IF(HEADIN) WRITE(OUTLYNE,6009)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(3,SF)&
         &,PXTRAY(5,SF),PXTRAY(7,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTX')THEN
         IF(HEADIN) WRITE(OUTLYNE,6012)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(4,SF)&
         &,PXTRAX(5,SF),PXTRAX(8,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTY')THEN
         IF(HEADIN) WRITE(OUTLYNE,6015)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(4,SF)&
         &,PXTRAY(5,SF),PXTRAY(8,SF)
         CALL SHOWIT(0)
      END IF
      RETURN
   END IF
   IF(SQ.EQ.1 .AND.WQ.NE.'OBJ'.OR.SQ.EQ.1 .AND.&
   &WQ.NE.'ALL'.OR.SQ.EQ.1 .AND.WQ.NE.'OB') THEN
      WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.0 .AND.DF1.EQ.1) THEN
!       OUTPUT IMAGE SURFACE
      SF=INT(sys_last_surf())
      IF(WC.EQ.'PXTX')THEN
         IF(HEADIN) WRITE(OUTLYNE,6000)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(2,SF)&
         &,PXTRAX(5,SF),PXTRAX(6,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PXTY')THEN
         IF(HEADIN) WRITE(OUTLYNE,6003)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(2,SF)&
         &,PXTRAY(5,SF),PXTRAY(6,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PITX')THEN
         IF(HEADIN) WRITE(OUTLYNE,6006)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(3,SF)&
         &,PXTRAX(5,SF),PXTRAX(7,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PITY')THEN
         IF(HEADIN) WRITE(OUTLYNE,6009)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(3,SF)&
         &,PXTRAY(5,SF),PXTRAY(7,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTX')THEN
         IF(HEADIN) WRITE(OUTLYNE,6012)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAX(1,SF),PXTRAX(4,SF)&
         &,PXTRAX(5,SF),PXTRAX(8,SF)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTY')THEN
         IF(HEADIN) WRITE(OUTLYNE,6015)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500) SF,PXTRAY(1,SF),PXTRAY(4,SF)&
         &,PXTRAY(5,SF),PXTRAY(8,SF)
         CALL SHOWIT(0)
      END IF
      RETURN
   END IF
   IF(SQ.EQ.0 .AND.DF1.NE.1) THEN
      I=INT(W1)
      SF=INT(sys_last_surf())
      IF(I.GT.SF.OR.I.LT.0) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'PXTX') THEN
         IF(HEADIN) WRITE(OUTLYNE,6000)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)I,PXTRAX(1,I),PXTRAX(2,I)&
         &,PXTRAX(5,I),PXTRAX(6,I)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PXTY') THEN
         IF(HEADIN) WRITE(OUTLYNE,6003)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)I,PXTRAY(1,I),PXTRAY(2,I)&
         &,PXTRAY(5,I),PXTRAY(6,I)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PITX') THEN
         IF(HEADIN) WRITE(OUTLYNE,6006)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)I,PXTRAX(1,I),PXTRAX(3,I)&
         &,PXTRAX(5,I),PXTRAX(7,I)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PITY') THEN
         IF(HEADIN) WRITE(OUTLYNE,6009)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)I,PXTRAY(1,I),PXTRAY(3,I)&
         &,PXTRAY(5,I),PXTRAY(7,I)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTX') THEN
         IF(HEADIN) WRITE(OUTLYNE,6012)
         IF(HEADIN) CALL SHOWIT(0)
         WRITE(OUTLYNE,1500)I,PXTRAX(1,I),PXTRAX(4,I)&
         &,PXTRAX(5,I),PXTRAX(8,I)
         CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTY') THEN
         IF(HEADIN) WRITE(OUTLYNE,6015)
         IF(HEADIN) CALL SHOWIT(0)
      END IF
      IF(WC.EQ.'PRTY') THEN
         WRITE(OUTLYNE,1500)I,PXTRAY(1,I),PXTRAY(4,I)&
         &,PXTRAY(5,I),PXTRAY(8,I)
         CALL SHOWIT(0)
      END IF
   END IF
1500 FORMAT(I3,2X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
2000 FORMAT(I3,1X,G13.6,2X,G13.6,2X,G13.6,2X,G13.6)
!
!       PXTX
!
6000 FORMAT('SURF',6X,'PX',12X,'PUX',13X,'PCX',11X,'PUCX')
6001 FORMAT('PARAXIAL RAYTRACE DATA (XZ-PLANE)',&
   &' - (CFG #',I2,')')
6002 FORMAT(&
   &'(PUX AND PUCX) MEASURED WITH RESPECT TO THE Z-AXIS')
!
!       PXTY
!
6003 FORMAT('SURF',6X,'PY',12X,'PUY',13X,'PCY',11X,'PUCY')
6004 FORMAT('PARAXIAL RAYTRACE DATA (YZ-PLANE)',&
   &' - (CFG #',I2,')')
6005 FORMAT(&
   &'(PUY AND PUCY) MEASURED WITH RESPECT TO THE Z-AXIS')
2501 FORMAT(1X)
!
!       PITX
!
6006 FORMAT('SURF',6X,'PX',12X,'PIX',13X,'PCX',11X,'PICX')
6007 FORMAT('PARAXIAL RAYTRACE DATA (XZ-PLANE)',&
   &' - (CFG # ',I2,')')
6008 FORMAT('(PIX AND PICX) - ANGLES OF INCIDENCE')
!
!       PITY
!
6009 FORMAT('SURF',6X,'PY',12X,'PIY',13X,'PCY',11X,'PICY')
6010 FORMAT('PARAXIAL RAYTRACE DATA (YZ-PLANE)',&
   &' - (CFG # ',I2,')')
6011 FORMAT('(PIY AND PICY) - ANGLES OF INCIDENCE')
!
!       PRTX
!
6012 FORMAT('SURF',6X,'PX',12X,'PRX',13X,'PCX',11X,'PRCX')
6013 FORMAT('PARAXIAL RAYTRACE DATA (XZ-PLANE)',&
   &' - (CFG #',I2,')')
6014 FORMAT('(PRX AND PRCX) - ANGLES OF REFRACTION/REFLECTION')
!
!       PRTY
!
6015 FORMAT('SURF',6X,'PY',12X,'PRY',13X,'PCY',11X,'PRCY')
6016 FORMAT('PARAXIAL RAYTRACE DATA (YZ-PLANE)',&
   &' - (CFG #',I2,')')
6017 FORMAT('(PRY AND PRCY) - ANGLES OF REFRACTION/REFLECTION')
END
! SUB TR.FOR
SUBROUTINE TR
!
   use DATLEN
   use DATMAI
   use mod_surface, only: surf_curvature, surf_thickness, surf_toric_flag, surf_toric_curvature, surf_asphere_coeff, surf_ideal_efl, surf_pickup_count
   use mod_system, only: sys_astop, sys_last_surf, sys_sax, sys_say, sys_scx, sys_scy, &
      & sys_telecentric, sys_wl_ref, sys_x1_scx, sys_y1_scy, &
      & sys_set_x1_scx, sys_set_y1_scy
   IMPLICIT NONE
!
!       THIS IS CALLED BY SUBROUTINE FADJ AND ERADJ.
!
   INTEGER I,J,JK,L,COMI
!
   COMMON/PIKCOM/COMI
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
   REAL*8 TMP15A,TMP15B,CON,CURV,SYS13,TMP17A,TMP17B
!
   INTEGER WWVN
   IF(INT(sys_wl_ref()).EQ.1) WWVN=46
   IF(INT(sys_wl_ref()).EQ.2) WWVN=47
   IF(INT(sys_wl_ref()).EQ.3) WWVN=48
   IF(INT(sys_wl_ref()).EQ.4) WWVN=49
   IF(INT(sys_wl_ref()).EQ.5) WWVN=50
   IF(INT(sys_wl_ref()).EQ.6) WWVN=71
   IF(INT(sys_wl_ref()).EQ.7) WWVN=72
   IF(INT(sys_wl_ref()).EQ.8) WWVN=73
   IF(INT(sys_wl_ref()).EQ.9) WWVN=74
   IF(INT(sys_wl_ref()).EQ.10) WWVN=75
   CON=sys_y1_scy()
!
   IF(ITYPEP.EQ.1) THEN
!       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
!
!       IF AN APERTURE STOP IS DEFINED,
!       THE VALUE OF sys_y1_scy() NEEDS TO BE
!       REFINED.
!
!       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
!       UP TO THE APERTURE STOP SURFACE
!       USING TWO DIFFERENT VALUES
!       OF sys_y1_scy() [HEIGTH OF CHIEF RAY AT SURF 1]
!
!       THE TWO VALUES USED ARE 0.0 AND 0.1
!
!       THE CORRECET VALUE OF sys_y1_scy() WHICH MAKES PCY ON THE
!       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
!
!       PCY(AT ASTOP FOR sys_y1_scy()=0.0) IS CALLED TMP15A
!       PCY(AT ASTOP FOR sys_y1_scy()=0.1) IS CALLED TMP15B
!
!       sys_y1_scy()=((-.1*TMP15A)/(TMP15B-TMP15A))+sys_y1_scy()
!
      IF(sys_astop().GT.0.0D0 &
      &.AND.sys_telecentric().EQ.0.0D0) THEN
!
!       RECALCULATE THE CORRECT VALUE OF sys_y1_scy()
!       OTHERWISE, USE THE USER PROVIDED VALUE OF sys_y1_scy()
!
!                       RAY TARGETING INFORMATION
!
         DO 60 JK=1,2
            IF(JK.EQ.1) CON=CON+0.0D0
            IF(JK.EQ.2) CON=CON+0.1D0
!*************************************************************************
!       INITIAL TARGET OF RAY TRACE
!               THE INITIAL PARAXIAL RAYTRACE TARGETING
!               DATA IS:
!                       AT SURFACE 0 (OBJ)
!
!       STARTING MARGINAL RAY HEIGHT = 0
!       STARTING CHIEF RAY HEIGHT    = SCY
!                       AT SURFACE 1 (INITIAL REF SURF)
!       STARTING MARGINAL RAY HEIGHT = SAY
!       STARTING CHIEF RAY HEIGHT = CON
!
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!       CALL PIKRES FOR THE OBJECT SURFACE
            COMI=0
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(0)=0,  ALWAYS
            PXTRAY(1,0)=0.0D0
!
!       PUY(0)=SAY/TH(0)
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!
!       PIY(0) =PUY(0)
            PXTRAY(3,0)=PXTRAY(2,0)
!
!       PIY'(0)=PUY(0)
            PXTRAY(4,0)=PXTRAY(3,0)
!
!       PCY(0) =-SCY
            PXTRAY(5,0)=-(sys_scy())
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       CON IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAY(6,0)=-((sys_scy())-CON)/surf_thickness(0)
!
!       PICY(0) AT OBJECT, PICY = PUCY
            PXTRAY(7,0)=PXTRAY(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
            PXTRAY(8,0)=PXTRAY(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
            COMI=1
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN sys_say()
            PXTRAY(1,1)=(sys_say())
!

!       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
!
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(2,1)=-CURV*PXTRAY(1,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAY(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAY(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAY(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(2,1-1)

!
!       PIY(1)=CV(1)*PY(1)+PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
!
!       PIY'(1)=(N/N')*PIY(1)
            PXTRAY(4,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAY(3,1)
!
!       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            PXTRAY(5,1)=CON
!
!       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(6,1)=-CURV*PXTRAY(5,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAY(6,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAY(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(6,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAY(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(6,1-1)
!
!       PICY(1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 2) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
!       PICY'(1)
            PXTRAY(8,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAY(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
            DO 50 L=2,INT(sys_astop())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
               COMI=L
               IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)

!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
               PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 2) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
               PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
!
50          CONTINUE
            IF(JK.EQ.1) TMP15A=PXTRAY(5,(INT(sys_astop())))
            IF(JK.EQ.2) TMP15B=PXTRAY(5,(INT(sys_astop())))
60       CONTINUE
         IF(TMP15A.EQ.TMP15B) THEN
            OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
            CALL SHOWIT(1)
            OUTLYNE='APERTURE STOP SURFACE.'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            call sys_set_y1_scy(((-0.1D0*TMP15A)/(TMP15B-TMP15A))+sys_y1_scy())
         END IF
!
!       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
!       USING THIS VALUE OF sys_y1_scy()
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(0)=0,  ALWAYS
         PXTRAY(1,0)=0.0D0
!
!       PUY(0)=SAY/TH(0)
         PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!
!       PIY(0) =PUY(0)
         PXTRAY(3,0)=PXTRAY(2,0)
!
!       PIY'(0)=PUY(0)
         PXTRAY(4,0)=PXTRAY(3,0)
!
!       PCY(0) =-SCY
         PXTRAY(5,0)=-(sys_scy())
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_y1_scy() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         PXTRAY(6,0)=-((sys_scy())-sys_y1_scy())/surf_thickness(0)
!
!       PICY(0) AT OBJECT, PICY = PUCY
         PXTRAY(7,0)=PXTRAY(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
         PXTRAY(8,0)=PXTRAY(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN sys_say()
         PXTRAY(1,1)=(sys_say())
!
!       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(2,1)=-CURV*PXTRAY(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
!
!       PIY(1)=CV(1)*PY(1)+PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
!
!       PIY'(1)=(N/N')*PIY(1)
         PXTRAY(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(3,1)
!
!       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         PXTRAY(5,1)=sys_y1_scy()
!
!       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(6,1)=-CURV*PXTRAY(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(6,1-1)
!
!       PICY(1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
!       PICY'(1)
         PXTRAY(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE WHERE:
         DO 70 L=2,INT(sys_astop())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
            PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
            PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
!
70       CONTINUE
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
!       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
!       HERE. THIS IS THE
!       APERTURE STOP SURFACE. REDEFINE IT AS L.
         L=INT(sys_astop())
!       PROCEED TO NEXT SURFACES
!
!       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE,
!       AND PIKUPS ALONG THE WAY. THEN PROCEED.
!       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (sys_astop()+1)
!       TO THE IMAGE SURFACE  WHERE:
         DO 90 L=((INT(sys_astop()))+1),INT(sys_last_surf())
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!               VALUES AT SURFACE L
!
!       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
!       NOW CALCULATE PY VALUE
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       FINISHED WITH PY(L)
!**********************************************************************************
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
            PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
!       NO CALCULATE PCY VALUE
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       FINISHED WITH PCY(L)
!************************************************************************
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
            PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
90       CONTINUE
!       TRACE COMPLETED
!******************************************************************************
      ELSE
!
!       NO ASTOP WAS DEFINED OR TEL ON.
!       IF NO ASTOP IS DEFINED, sys_y1_scy() IS USED AS IT WAS STORED
!       DURING LENS INPUT.
!******************************************************************************
!       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
!       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
!       0 OR 1.
!               INITIAL VALUES AT SURFACE 0
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!
!       PY(0)=0,  ALWAYS
         PXTRAY(1,0)=0.0D0
!
!       PUY(0)=SAY/TH(0)
         PXTRAY(2,0)=(sys_say())/surf_thickness(0)
!
!       PIY(0) =PUY(0)
         PXTRAY(3,0)=PXTRAY(2,0)
!
!       PIY'(0)=PUY(0)
         PXTRAY(4,0)=PXTRAY(3,0)
!
!       PCY(0) =-SCY
         PXTRAY(5,0)=-(sys_scy())
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_y1_scy() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         IF(sys_telecentric().EQ.0.0D0)&
         &PXTRAY(6,0)=-((sys_scy())-sys_y1_scy())/surf_thickness(0)
         IF(sys_telecentric().EQ.1.0D0)&
         &PXTRAY(6,0)=0.0D0
!
!       PICY(0) AT OBJECT, PICY = PUCY
         PXTRAY(7,0)=PXTRAY(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
         PXTRAY(8,0)=PXTRAY(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN sys_say()
         PXTRAY(1,1)=(sys_say())
!
!       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(2,1)=-CURV*PXTRAY(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(2,1-1)
!
!       PIY(1)=CV(1)*PY(1)+PUY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
!
!       PIY'(1)=(N/N')*PIY(1)
         PXTRAY(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(3,1)
!
!       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         IF(sys_telecentric().EQ.0.0D0) PXTRAY(5,1)=sys_y1_scy()
         IF(sys_telecentric().EQ.1.0D0) PXTRAY(5,1)=PXTRAY(5,0)
!
!       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(6,1)=-CURV*PXTRAY(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAY(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAY(1,1))+PXTRAY(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAY(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAY(1,1))+PXTRAY(6,1-1)
!
!       PICY(1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 2) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
!       PICY'(1)
         PXTRAY(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAY(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
!       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
!       HERE. REDEFINE IT AS L.
         L=1
!       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
!       CORRECTLY HANDLING SOLVES
!       AND PIKUPS ALONG THE WAY. THEN PROCEED.
         DO 80 L=2,INT(sys_last_surf())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!******************************************************************************
!       PY(L)=PY(L-1)+CV(L-1)*PUY(L-1)
!       NOW CALCULATE PY VALUE
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(surf_thickness(L-1)*PXTRAY(2,(L-1)))
!
!       FINISHED WITH PY(L)
!
!******************************************************************************
!
!       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(2,L-1)
!
!       PIY(L)=CV(1)*PY(L)+PUY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
!
!       PIY'(L)=(N/N')*PIY(L)
            PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(3,L)
!
!       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
!******************************************************************************
!       NO CALCULATE PCY VALUE
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(surf_thickness(L-1)*PXTRAY(6,(L-1)))
!
!       FINISHED WITH PCY(L)
!************************************************************************
!
!       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAY(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAY(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAY(1,L))+PXTRAY(6,L-1)
!
!       PICY(L)
!       CHECK FOR X-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 2) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
!       PICY'(L)
            PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAY(7,L)
!
80       CONTINUE
!       PARAXIAL TRACE COMPLETED
      END IF
      RETURN
   ELSE
!       ITYPEP NOT 1
   END IF
   CON=sys_x1_scx()
   IF(ITYPEP.EQ.2) THEN
!
      SYS13=sys_sax()
!
!       NOW WE PERFORM A PARAXIAL RAY TRACE WITHOUT AND SOLVES
!
!       IF AN APERTURE STOP IS DEFINED
!       , THE VALUE OF sys_x1_scx() NEEDS TO BE
!       REFINED.
!
!       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
!       UP TO THE APERTURE STOP SURFACE
!       USING TWO DIFFERENT VALUES
!       OF sys_x1_scx() [HEIGTH OF CHIEF RAY AT SURF 1]
!
!       THE TWO VALUES USED ARE 0.0 AND 0.1
!
!       THE CORRECET VALUE OF sys_x1_scx() WHICH MAKES PCX ON THE
!       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
!
!       PCX(AT ASTOP FOR sys_x1_scx()=0.0) IS CALLED TMP17A
!       PCX(AT ASTOP FOR sys_x1_scx()=0.1) IS CALLED TMP17B
!
!       sys_x1_scx()=((-.1*TMP17A)/(TMP17B-TMP17A))+sys_x1_scx()
!
      IF(sys_astop().GT.0.0D0 .AND.sys_telecentric().EQ.0.0D0) THEN
!
!       RECALCULATE THE CORRECT VALUE OF sys_x1_scx()
!       OTHERWISE, USE THE USER PROVIDED VALUE OF sys_x1_scx()
!
!                       RAY
         DO 600 JK=1,2
            IF(JK.EQ.1) CON=CON+0.0D0
            IF(JK.EQ.2) CON=CON+0.1D0
!*************************************************************************
!       INITIAL TARGET OF RAY TRACE
!               THE INITIAL PARAXIAL RAYTRACE TARGETING
!               DATA IS:
!                       AT SURFACE 0 (OBJ)
!
!       STARTING MARGINAL RAY HEIGHT = 0
!       STARTING CHIEF RAY HEIGHT    = SCX
!                       AT SURFACE 1 (INITIAL REF SURF)
!       STARTING MARGINAL RAY HEIGHT = SAX
!       STARTING CHIEF RAY HEIGHT = CON
!
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!       CALL PIKRES FOR THE OBJECT SURFACE
            COMI=0
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(0)=0,  ALWAYS
            PXTRAX(1,0)=0.0D0
!
!       PUX(0)=SAX/TH(0)
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAX(2,0)=(SYS13)/surf_thickness(0)
!
!       PIX(0) =PUX(0)
            PXTRAX(3,0)=PXTRAX(2,0)
!
!       PIX'(0)=PUX(0)
            PXTRAX(4,0)=PXTRAX(3,0)
!
!       PCX(0) =-SCX
            PXTRAX(5,0)=-(sys_scx())
!
!       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
!       CON IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
!
            IF(surf_thickness(0).EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED', 1)
               RETURN
            END IF
            PXTRAX(6,0)=-((sys_scx())-CON)/surf_thickness(0)
!
!       PICX(0) AT OBJECT, PICX = PUCX
            PXTRAX(7,0)=PXTRAX(6,0)
!
!       PICX'(0) AT OBJECT PICX'=PICX
            PXTRAX(8,0)=PXTRAX(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
            COMI=1
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
            PXTRAX(1,1)=(SYS13)
!

!       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
!
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAX(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAX(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(2,1-1)

!
!       PIX(1)=CV(1)*PX(1)+PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
!
!       PIX'(1)=(N/N')*PIX(1)
            PXTRAX(4,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAX(3,1)
!
!       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            PXTRAX(5,1)=CON
!
!       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)*&
            &(((ALENS(WWVN,1))-&
            &(ALENS(WWVN,0)))/&
            &(ALENS(WWVN,1)))+&
            &((ALENS(WWVN,0))/&
            &(ALENS(WWVN,1)))*PXTRAX(6,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ')&
            &PXTRAX(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(6,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ')&
            &PXTRAX(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(6,1-1)
!
!       PICY(1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(1) == 1) THEN
               CURV=surf_toric_curvature(1)
            ELSE
               IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(1,2)*2.0D0
               ELSE
                  CURV=surf_curvature(1)
               END IF
            END IF
            PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
!       PICX'(1)
            PXTRAX(8,1)=((ALENS((WWVN),0))/&
            &(ALENS((WWVN),1)))*PXTRAX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE WHERE:
            DO 500 L=2,INT(sys_astop())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
               COMI=L
               IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)

!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
               PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
               PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
               &(((ALENS(WWVN,L))-&
               &(ALENS(WWVN,(L-1))))/&
               &(ALENS(WWVN,L)))+&
               &((ALENS(WWVN,(L-1)))/&
               &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
               IF(GLANAM(L,2).EQ.'PERFECT      ')&
               &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
               IF(GLANAM(L,2).EQ.'IDEAL        ')&
               &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
               IF(surf_toric_flag(L) == 1) THEN
                  CURV=surf_toric_curvature(L)
               ELSE
                  IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                     CURV=surf_asphere_coeff(L,2)*2.0D0
                  ELSE
                     CURV=surf_curvature(L)
                  END IF
               END IF
               PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
               PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
               &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
!
500         CONTINUE
            IF(JK.EQ.1) TMP17A=PXTRAX(5,(INT(sys_astop())))
            IF(JK.EQ.2) TMP17B=PXTRAX(5,(INT(sys_astop())))
600      CONTINUE
         IF(TMP17A.EQ.TMP17B) THEN
            OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
            CALL SHOWIT(1)
            OUTLYNE='APERTURE STOP SURFACE.'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         ELSE
            call sys_set_x1_scx(((-0.1D0*TMP17A)/(TMP17B-TMP17A))+sys_x1_scx())
         END IF
!
!       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
!       USING THIS VALUE OF sys_x1_scx()
!
!               INITIAL VALUES AT SURFACE 0
!***************************************************************
!
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(0)=0,  ALWAYS
         PXTRAX(1,0)=0.0D0
!
!       PUX(0)=SAX/TH(0)
         PXTRAX(2,0)=(SYS13)/surf_thickness(0)
!
!       PIX(0) =PUX(0)
         PXTRAX(3,0)=PXTRAX(2,0)
!
!       PIX'(0)=PUX(0)
         PXTRAX(4,0)=PXTRAX(3,0)
!
!       PCX(0) =-SCX
         PXTRAX(5,0)=-(sys_scx())
!
!       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_x1_scx() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         PXTRAX(6,0)=-((sys_scx())-sys_x1_scx())/surf_thickness(0)
!
!       PICX(0) AT OBJECT, PICX = PUCX
         PXTRAX(7,0)=PXTRAX(6,0)
!
!       PICX'(0) AT OBJECT PICX'=PICX
         PXTRAX(8,0)=PXTRAX(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
         PXTRAX(1,1)=(SYS13)
!
!       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(2,1)=-CURV*PXTRAX(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
!
!       PIX(1)=CV(1)*PX(1)+PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
!
!       PIX'(1)=(N/N')*PIX(1)
         PXTRAX(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(3,1)
!
!       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         PXTRAX(5,1)=sys_x1_scx()
!
!       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(6,1)=-CURV*PXTRAX(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(6,1-1)
!
!       PICX(1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
!       PICX'(1)
         PXTRAX(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
! *****************************************************************************
!       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
         DO 700 L=2,INT(sys_astop())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
            PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
            PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
!
700      CONTINUE
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
!       WE HAVE DEFINED. THIS IS THE
!       APERTURE STOP SURFACE. REDEFINE IT AS L.
!       NOW TRACE FROM SURFACE AFTER THE ASTOP TO IMAGE PLANE.
!
!       NOW TRACE FROM THE APERTURE STOP SURFACE+1
!       TO THE IMAGE SURFACE  WHERE:
         DO 900 L=((INT(sys_astop()))+1),INT(sys_last_surf())
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!               VALUES AT SURFACE L
!
!       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
!       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       FINISHED WITH PX(L)
!**********************************************************************************
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
            PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
!       NO CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       FINISHED WITH PCX(L)
!************************************************************************
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
            PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
900      CONTINUE
!       TRACE COMPLETED
!******************************************************************************
      ELSE
!
!       NO ASTOP WAS DEFINED OR TEL ON.
!       IF NO ASTOP IS DEFINED, sys_x1_scx() IS USED AS IT WAS STORED
!       DURING LENS INPUT.
!******************************************************************************
!       TRACE FROM THE OBJECT TO THE IMAGE.
!               INITIAL VALUES AT SURFACE 0
!       CALL PIKRES FOR THE OBJECT SURFACE
         COMI=0
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!
!       PX(0)=0,  ALWAYS
         PXTRAX(1,0)=0.0D0
!
!       PUX(0)=SAX/TH(0)
         PXTRAX(2,0)=(SYS13)/surf_thickness(0)
!
!       PIX(0) =PUX(0)
         PXTRAX(3,0)=PXTRAX(2,0)
!
!       PIX'(0)=PUX(0)
         PXTRAX(4,0)=PXTRAX(3,0)
!
!       PCX(0) =-SCX
         PXTRAX(5,0)=-(sys_scx())
!
!       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
!       sys_x1_scx() IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
         IF(sys_telecentric().EQ.0.0D0)&
         &PXTRAX(6,0)=-((sys_scx())-sys_x1_scx())/surf_thickness(0)
         IF(sys_telecentric().EQ.1.0D0)&
         &PXTRAX(6,0)=0.0D0
!
!       PICX(0) AT OBJECT, PICX = PUCX
         PXTRAX(7,0)=PXTRAX(6,0)
!
!       PICX'(0) AT OBJECT PICX'=PICX
         PXTRAX(8,0)=PXTRAX(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
!               INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
         COMI=1
         IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!
!       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
         PXTRAX(1,1)=(SYS13)
!
!       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(2,1)=-CURV*PXTRAX(1,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(2,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(2,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(2,1-1)
!
!       PIX(1)=CV(1)*PX(1)+PUX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
!
!       PIX'(1)=(N/N')*PIX(1)
         PXTRAX(4,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(3,1)
!
!       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
         IF(sys_telecentric().EQ.0.0D0) PXTRAX(5,1)=sys_x1_scx()
         IF(sys_telecentric().EQ.1.0D0) PXTRAX(5,1)=PXTRAX(5,0)
!
!       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(6,1)=-CURV*PXTRAX(5,1)*&
         &(((ALENS(WWVN,1))-&
         &(ALENS(WWVN,0)))/&
         &(ALENS(WWVN,1)))+&
         &((ALENS(WWVN,0))/&
         &(ALENS(WWVN,1)))*PXTRAX(6,0)
         IF(GLANAM(1,2).EQ.'PERFECT      ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_thickness(1))*PXTRAX(1,1))+PXTRAX(6,1-1)
         IF(GLANAM(1,2).EQ.'IDEAL        ')&
         &PXTRAX(6,1)=(-(1.0D0/surf_ideal_efl(1))*PXTRAX(1,1))+PXTRAX(6,1-1)
!
!       PICX(1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
         IF(surf_toric_flag(1) == 1) THEN
            CURV=surf_toric_curvature(1)
         ELSE
            IF(surf_curvature(1).EQ.0.0D0 .AND.surf_asphere_coeff(1,2).NE.0.0D0) THEN
               CURV=surf_asphere_coeff(1,2)*2.0D0
            ELSE
               CURV=surf_curvature(1)
            END IF
         END IF
         PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
!       PICX'(1)
         PXTRAX(8,1)=((ALENS((WWVN),0))/&
         &(ALENS((WWVN),1)))*PXTRAX(7,1)
!
!       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
!
!       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
!       OBJECT THROUGH SURFACE 1 IN THE ABSCENSE OF ANY SOLVES.
!       NOW TRACE FROM SURFACE 2 TO IMAGE PLANE,
         DO 800 L=2,INT(sys_last_surf())
!               VALUES AT SURFACE L
!       CALL PIKRES FOR THE SURFACE L
            COMI=L
            IF(surf_pickup_count(COMI) /= 0) CALL PIKRES
!******************************************************************************
!       PX(L)=PX(L-1)+CV(L-1)*PUX(L-1)
!       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(surf_thickness(L-1)*PXTRAX(2,(L-1)))
!
!       FINISHED WITH PX(L)
!
!******************************************************************************
!
!       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(2,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(2,L-1)
!
!       PIX(L)=CV(1)*PX(L)+PUX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
!
!       PIX'(L)=(N/N')*PIX(L)
            PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(3,L)
!
!       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
!******************************************************************************
!       NOW CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(surf_thickness(L-1)*PXTRAX(6,(L-1)))
!
!       FINISHED WITH PCX(L)
!************************************************************************
!
!       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)*&
            &(((ALENS(WWVN,L))-&
            &(ALENS(WWVN,(L-1))))/&
            &(ALENS(WWVN,L)))+&
            &((ALENS(WWVN,(L-1)))/&
            &(ALENS(WWVN,L)))*PXTRAX(6,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_thickness(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ')&
            &PXTRAX(6,L)=(-(1.0D0/surf_ideal_efl(L))*PXTRAX(1,L))+PXTRAX(6,L-1)
!
!       PICX(L)
!       CHECK FOR Y-TORIC. IF FOUND SET CURV=surf_toric_curvature(-)
!       ELSE SET CURV=surf_curvature(-)
            IF(surf_toric_flag(L) == 1) THEN
               CURV=surf_toric_curvature(L)
            ELSE
               IF(surf_curvature(L).EQ.0.0D0 .AND.surf_asphere_coeff(L,2).NE.0.0D0) THEN
                  CURV=surf_asphere_coeff(L,2)*2.0D0
               ELSE
                  CURV=surf_curvature(L)
               END IF
            END IF
            PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
!       PICX'(L)
            PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/&
            &(ALENS((WWVN),L)))*PXTRAX(7,L)
!
800      CONTINUE
!       PARAXIAL TRACE COMPLETED
      END IF
      RETURN
   ELSE
!       ITYPEP NOT 2
   END IF
END
