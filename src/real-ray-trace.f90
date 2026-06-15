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
   use iso_fortran_env, only: real64

contains

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
    !  dLen =DSQRT( !      ((RAYRAY(3,L)-RAYRAY(18,L))**2)+((RAYRAY(2,L)-RAYRAY(17,L))**2) !     +((RAYRAY(1,L)-RAYRAY(16,L))**2))

      RV = .FALSE.
      if (ldm%getSurfThi(L) < 0 ) RV = .TRUE.
      IF(RV) dLen=-dLen
      IF(DABS(dLen).GE.1.0D10) dLen=0.0D0

      !dOPL = dLen/(sysConfig%getWavelength(INT(WW3))/1E3)
!
       dOPL=dLen*DABS(ldm%getSurfIndex(L-1,INT(WW3)))
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
       dOPL=dLen*DABS(ldm%getSurfIndex(L-1,INT(WW3)))
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
   use iso_fortran_env, only: real64
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


    
    dLen = DSQRT((angX*ldm%getSurfThi(L))**2+(angY*ldm%getSurfThi(L))**2 +  (angZ*ldm%getSurfThi(L))**2)
    !  dLen =DSQRT( !      ((RAYRAY(3,L)-RAYRAY(18,L))**2)+((RAYRAY(2,L)-RAYRAY(17,L))**2) !     +((RAYRAY(1,L)-RAYRAY(16,L))**2))

      RV = .FALSE.
      if (ldm%getSurfThi(L) < 0 ) RV = .TRUE.
      IF(RV) dLen=-dLen
      IF(DABS(dLen).GE.1.0D10) dLen=0.0D0

      !dOPL = dLen/(sysConfig%getWavelength(INT(WW3))/1E3)
!
       ! Tricky compared to adjustLastSurface
      ! var of interest for wavelength is WW4.
       dOPL=dLen*DABS(ldm%getSurfIndex(L-1,INT(WW4)))
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


! -----------------------------------------------------------------------
! real_ray_trace_core — unified real ray tracer
!
! This is the single merged implementation that replaced the two
! near-identical routines RAYTRA_OLD (.false.) and RAYTRA2 (.true.).
!
! Behavioral differences gated on for_optimization:
!
!  1. Zero-wavelength early exit: RAYTRA_OLD only (.not. for_optimization)
!  2. outer_retry failure:
!       RAYTRA_OLD: PRINT + RETURN
!       RAYTRA2:    silent RETURN + CALL MACFAL (gated on SPDTRA/F34/F58)
!  3. JKX/JKY setup:
!       RAYTRA_OLD: uses clear_aperture type (cap%) to read clap dims
!       RAYTRA2:    uses surf_clap_* accessors directly — FUNCTIONALLY
!                   IDENTICAL at run-time (same ALENS data), so we use
!                   the cap% path for both (already modernized in Phase 1)
!  4. KKK>NRAITR failure:
!       RAYTRA_OLD: MSG guard + zoa_emit, no MACFAL
!       RAYTRA2:    no message + CALL MACFAL
!  5. REVSTR: RAYTRA_OLD uses ALENS(A_THI,NEWOBJ); RAYTRA2 surf_thickness()
!       — same value; we use surf_thickness() for both (already done in 1b)
!       Actually RAYTRA_OLD still has ALENS(A_THI,...) — kept as-is.
!  6. Direction cosine init (.NOT.ITRACE branch):
!       RAYTRA_OLD: has the ITRACE illumination path
!       RAYTRA2:    also has ITRACE (identical) — shared, no gating needed
!  7. RAYRAY(36:38,NEWOBJ) init:
!       RAYTRA_OLD: 1.0D0
!       RAYTRA2:    0.0D0  — GATED on for_optimization
!  8. RN1/RN2 init at NEWOBJ:
!       RAYTRA_OLD: unconditional ldm%getSurfIndex
!       RAYTRA2:    guarded by IF(INT(WW3).GE.1.AND.INT(WW3).LE.10)
!       Both produce same result in valid WW3 range; we use unconditional.
!  9. HITSUR STOPP handling:
!       RAYTRA_OLD: PRINT *, "RAYTRA Failed 2370" before RETURN
!       RAYTRA2:    no PRINT — gated on .not. for_optimization
! 10. STOPP inner loop failure:
!       RAYTRA_OLD: GRASET plot-ray block (GUI path)
!       RAYTRA2:    no GRASET block + CALL MACFAL
!  11. NEWREF aim success:
!       RAYTRA_OLD: just CYCLE
!       RAYTRA2:    REFMISS=.FALSE. + CALL MISSREF(X,Y) before CYCLE
!  12. KKK=1 GETZEE1 guard:
!       RAYTRA_OLD: IF(ALENS(A_CURV,1).NE.0.0D0)CALL GETZEE1
!       RAYTRA2:    IF(surf_curvature(1).NE.0.0D0)CALL GETZEE1
!       Same value; we keep ALENS(A_CURV,1) from RAYTRA_OLD path.
! 13. CACOCH outer_retry: RAYTRA_OLD uses named cacoch_loop with do-label;
!       RAYTRA2 uses plain DO loop. No behavioral difference.
! 14. ray_blocked tail:
!       RAYTRA_OLD: no MACFAL
!       RAYTRA2:    CALL MACFAL
! 15. End of routine:
!       RAYTRA_OLD: GRASET tail block + call compute_ray_energy
!       RAYTRA2:    no GRASET + inline energy loop
!       → Gated: for_optimization uses RAYTRA2's inline loop;
!                 .not. for_optimization calls compute_ray_energy.
!         The inline loops are functionally equivalent to compute_ray_energy
!         for the optimization path but use ldm%getSurfIndex / surf_* accessors.
!         We inline both to preserve each path's original accessors.
! -----------------------------------------------------------------------
subroutine real_ray_trace_core(for_optimization)
   use GLOBALS
   use mod_lens_data_manager, only: ldm
   use mod_system, only: sys_ray_aiming, sys_scx, sys_scy, &
      & sys_screen, sys_screen_d, sys_screen_h, sys_screen_s, sys_screen_surf, &
      & sys_telecentric, sys_wavelength, sys_screen_excl_angle
   use clear_apertures, only: clear_aperture
   use zoa_output, only: zoa_emit
   use mod_surface
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   implicit none

   logical, intent(in) :: for_optimization

   type(clear_aperture) :: cap

   INTEGER IPASS1,JK,I,ISYS20,KKK,J,ISURF,IK,WA3

   INTEGER CAERAS,COERAS,N_HITS

   real(real64) OPLXCOR(1:2),OPLYCOR(1:2),OPLZCOR(1:2)

   COMMON/COROPL/OPLXCOR,OPLYCOR,OPLZCOR

   real(real64) X,Y,Z,L,M,N,WW1W,WW2W,LER,MER,NER,TANN1,TANN2,IA,IAP,D21,D22,GAMMA,XXX,YYY,TWW1,TWW2,XL,XM,XN,YL,YM,YN,RN1,RN2,WWW1,WWW2,D11,D12,LS,SNINDX,SNIND2,WW1WW,WW2WW,JK1,JK2,JK3,LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,ENERGY_FACTOR,Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,XC1,YC1,ZC1,MF1,MF2,DET,TARX,TARY,XVALUE,YVALUE,TEST,MAG,VALUE,LOLD,MOLD,NOLD,TARRY,TARRX,JKX,JKY,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2,JK_CPL,JK_CPM,JK_CPN,SA_CPL,SA_CPM,SA_CPN,DP,POLANG,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL,STEPL,STEPL1

   COMMON/CACO/CAERAS,COERAS,LS

   LOGICAL TCLPRF,SPDTRA,MMSG
   LOGICAL AIMOK,CLAPT,OLDPASS,GERROR,DELFAIL,ray_blocked
   COMMON/PASSOLD/OLDPASS

   INTEGER SPDCD1,SPDCD2

   COMMON/SPRA1/SPDTRA
   COMMON/SPRA2/SPDCD1,SPDCD2

   real(real64) XPASS,YPASS,ZPASS

   COMMON/SAGPAS/XPASS,YPASS,ZPASS

   real(real64) AOI,D,H,S,FACTOR

   KKK=0
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

!  [DIFF 1] Zero-wavelength check: RAYTRA_OLD only
   if (.not. for_optimization) then
      IF(WW3.GE.1.0D0.AND.WW3.LE.5.0D0) THEN
         IF(sys_wavelength(INT(WW3)).EQ.0.0D0) THEN
            IF(MSG) THEN
               CALL RAY_FAILURE(NEWOBJ)
               call zoa_emit('RAY CAN NOT BE TRACED AT ZERO WAVELENGTH', 'black')
            END IF
            STOPP=1
            RAYCOD(1)=12
            RAYCOD(2)=NEWOBJ
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            RETURN
         END IF
      END IF
      IF(WW3.GE.6.0D0.AND.WW3.LE.10.0D0) THEN
         IF(sys_wavelength(INT(WW3)).EQ.0.0D0) THEN
            IF(MSG) THEN
               CALL RAY_FAILURE(NEWOBJ)
               call zoa_emit('RAY CAN NOT BE TRACED AT ZERO WAVELENGTH', 'black')
            END IF
            STOPP=1
            RAYCOD(1)=12
            RAYCOD(2)=NEWOBJ
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            RETURN
         END IF
      END IF
   end if

   RAYCOD(1)=-1
   RAYCOD(2)=-1

   AIMOK=.FALSE.

   RELY=WW1
   RELX=WW2

   LARGE=-99999.9D0
   X1ONE=LARGE
   Y1ONE=LARGE
   X1LAST=LARGE
   Y1LAST=LARGE

   RXONE=LARGE
   RYONE=LARGE
   RXLAST=LARGE
   RYLAST=LARGE

   XSTRT=REFRY(1,NEWOBJ)
   YSTRT=REFRY(2,NEWOBJ)
   ZSTRT=REFRY(3,NEWOBJ)

   IF(sys_telecentric().EQ.0.0D0) THEN
!       TELECENTRIC AIMING IS OFF
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
      call cap%from_alens(NEWOBJ+1)
      IF(cap%shape == 1) THEN
!     CIRCULAR AP
         IF(cap%dim1 .LE. cap%dim2) THEN
            IF(DABS(cap%dim1).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(cap%dim1)
            IF(DABS(cap%dim1).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(cap%dim1)
         ELSE
            IF(DABS(cap%dim2).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(cap%dim2)
            IF(DABS(cap%dim2).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(cap%dim2)
         END IF
      END IF
      IF(cap%shape == 5) THEN
!     CIRCULAR AP
         IF(DABS(cap%dim1).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(cap%dim1)
         IF(DABS(cap%dim1).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(cap%dim1)
      END IF
      IF(cap%shape == 6) THEN
!     CIRCULAR AP
         IF(DABS(cap%dim5).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(cap%dim5)
         IF(DABS(cap%dim5).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(cap%dim5)
      END IF
      IF(cap%shape .GT. 1 .AND. cap%shape .LE. 4) THEN
!     OTHER AP
         IF(DABS(cap%dim2).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(cap%dim2)
         IF(DABS(cap%dim1).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(cap%dim1)
      END IF
   ELSE
!       TELECENTRIC AIMING IS ON
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
   END IF

   outer_retry: do
   IF(RAYCOD(1).EQ.1.AND.KKK.GT.1) THEN
!     [DIFF 2] outer_retry convergence failure
      STOPP=1
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
      FAIL=.TRUE.
      if (.not. for_optimization) then
         PRINT *, "RAYTRA FAILED LINE 1928 RAYTRA5.FOR"
      else
         IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
            CALL MACFAL
         END IF
      end if
      RETURN
   END IF
   IF(.NOT.ITRACE) THEN
      IF(NULL.AND..NOT.REFEXT) THEN
!     NULL WITH FAILED CHIEF RAY
         IF(sys_ray_aiming().EQ.0.0D0) THEN
!     NO RAY AIMING
            IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
               X1AIM=WW2*JKX
               X1AIM=(X1AIM)-ldm%getSurfXDec(NEWOBJ+1)
               Y1AIM=WW1*JKY
               Y1AIM=(Y1AIM)-ldm%getSurfYDec(NEWOBJ+1)
               Z1AIM=0.0D0
               XC=X1AIM
               YC=Y1AIM
               ZC=Z1AIM
               XC1=XC
               YC1=YC
               ZC1=ZC
            ELSE
!     TEL ON
               IF(sys_scx().NE.0.0D0)X1AIM=(LFOB(2)*DABS(PXTRAX(5,NEWOBJ)))+(WW2*JKX)
               IF(sys_scx().EQ.0.0D0)X1AIM=(WW2*JKX)
               IF(sys_scy().NE.0.0D0)Y1AIM=(LFOB(1)*(PXTRAY(5,NEWOBJ)))+(WW1*JKY)
               IF(sys_scy().EQ.0.0D0)Y1AIM=(WW1*JKY)
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
            X1AIM=(X1AIM)-ldm%getSurfXDec(NEWOBJ+1)
            Y1AIM=(LFOB(1)*DABS(PXTRAY(5,NEWOBJ+1)))+(WW1*JKY)
            Y1AIM=(Y1AIM)-ldm%getSurfYDec(NEWOBJ+1)
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
      IF(sys_ray_aiming().EQ.0.0D0) THEN
!     NO RAY AIMING
         IF(sys_telecentric().EQ.0.0D0) THEN
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

   IF(ITRACE) THEN
!     ILLUMINATION TRACE
!     TANGENTS OF CHIEF RAY SLOPE
      TANN1=REFRY(4,0)/REFRY(6,0)
      TANN2=REFRY(5,0)/REFRY(6,0)
   END IF

   STOPP=0
   RAYEXT=.TRUE.
   FAIL=.FALSE.

   newton_raphson: do
   IF(ldm%getSurfThi(NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
   IF(ldm%getSurfThi(NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
   RV=.FALSE.
   KKK=KKK+1
!     [DIFF 4] KKK>NRAITR failure
   IF(KKK.GT.NRAITR) THEN
      if (.not. for_optimization) then
         IF(MSG) THEN
            CALL RAY_FAILURE(NEWREF)
            call zoa_emit('RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT', 'black')
         END IF
      end if
      RAYCOD(1)=3
      RAYCOD(2)=NEWREF
      SPDCD1=RAYCOD(1)
      SPDCD2=NEWREF
      STOPP=1
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
      FAIL=.TRUE.
      if (for_optimization) then
         IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
            CALL MACFAL
         END IF
      end if
      RETURN
   ELSE
      STOPP=0
      RAYEXT=.TRUE.
      FAIL=.FALSE.
   END IF

   IF(.NOT.ITRACE) THEN
!     NOT ILLUMINATION TRACING
      MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)+((ZSTRT-Z1AIM)**2))
      LSTART=(X1AIM-XSTRT)/MAG
      MSTART=(Y1AIM-YSTRT)/MAG
      NSTART=(Z1AIM-ZSTRT)/MAG

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
      IF(WW1.GT.89.9999D0.AND.WW1.LT.90.0001D0)  WW1=89.9999D0

!     CONVERT TO RADIANS
      TWW1=WW1
      TWW2=WW2
      WW1=WW1*PII/180.0D0
      WW2=WW2*PII/180.0D0

!     CONVERT WW1 AND WW2 FROM THETA AND PHI TO XANG AND YANG
      WW1WW=DTAN(WW1)*DSIN(WW2)
      WW2WW=DTAN(WW1)*DCOS(WW2)
!     CALC X AND Y TANGENTS OF REGULAR RAY INCLUDING CHIEF RAY ANGLE
      WW1W=(WW1WW+TANN2)/(1.0D0-(WW1WW*TANN2))
      WW2W=(WW2WW+TANN1)/(1.0D0-(WW2WW*TANN1))

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
      X1AIM=WW2WW*ldm%getSurfThi(0)
      Y1AIM=WW1WW*ldm%getSurfThi(0)
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

!  [DIFF 7] RAYRAY(36:38,NEWOBJ) init: 1.0 for non-opt, 0.0 for opt
   RAYRAY(34:35,NEWOBJ)=1.0D0
   if (for_optimization) then
      RAYRAY(36:38,NEWOBJ)=0.0D0
   else
      RAYRAY(36:38,NEWOBJ)=1.0D0
   end if
   RAYRAY(32,NEWOBJ)=WW3
   RAYRAY(1,NEWOBJ)=XSTRT
   RAYRAY(2,NEWOBJ)=YSTRT
   RAYRAY(3,NEWOBJ)=ZSTRT
   RAYRAY(4,NEWOBJ)=LSTART
   RAYRAY(5,NEWOBJ)=MSTART
   RAYRAY(6,NEWOBJ)=NSTART
   RAYRAY(7,NEWOBJ)=0.0D0
   RAYRAY(8,NEWOBJ)=0.0D0
   RN1=ldm%getSurfIndex(NEWOBJ,INT(WW3))
   RN2=RN1
   SNIND2=DABS(RN1)/RN1
   IF(SNIND2.GT.0.0D0) RAYRAY(24,NEWOBJ)=1.0D0
   IF(SNIND2.LT.0.0D0) RAYRAY(24,NEWOBJ)=-1.0D0
   IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
   IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
   RAYRAY(9,NEWOBJ)=NSTART
   RAYRAY(10,NEWOBJ)=NSTART
   IA=DACOS(RAYRAY(9,NEWOBJ))
   IAP=DACOS(RAYRAY(10,NEWOBJ))
   RAYRAY(11,NEWOBJ)=XANG
   IF((RAYRAY(11,NEWOBJ)).LT.0.0D0) RAYRAY(11,NEWOBJ)=RAYRAY(11,NEWOBJ)+(TWOPII)
   RAYRAY(12,NEWOBJ)=YANG
   IF((RAYRAY(12,NEWOBJ)).LT.0.0D0) RAYRAY(12,NEWOBJ)=RAYRAY(12,NEWOBJ)+(TWOPII)
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

   ISYS20=NEWIMG
   surface_loop: do I=(NEWOBJ+1),ISYS20

      CALL TRNSF2_ARGS(I, X, Y, Z, L, M, N)
      XOLD=X
      YOLD=Y
      ZOLD=Z
      LOLD=L
      MOLD=M
      NOLD=N

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
!        [DIFF 9] HITSUR STOPP: PRINT only for non-opt
         if (.not. for_optimization) PRINT *, "RAYTRA Failed 2370"
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

      RN1=ldm%getSurfIndex(I-1,INT(WW3))
      RN2=ldm%getSurfIndex(I,INT(WW3))
      SNINDX=DABS(RN1)/RN1
      SNIND2=DABS(RN2)/RN2
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

      IF(NUMHITS(I-1).EQ.1.AND.I.GE.1) THEN
         RAYRAY(8,I)=DSQRT(((RAYRAY(3,I)-ZOLD)**2)+((RAYRAY(2,I)-YOLD)**2)+((RAYRAY(1,I)-XOLD)**2))
      ELSE
!       NUMHITS STORED ON LAST SURFACE WAS GREATER THAN 1
         RAYRAY(8,I)=0.0D0
!       I-1 WAS AN NSS
         XOLD=0.0D0
         YOLD=0.0D0
         ZOLD=0.0D0
         DO N_HITS=1,NUMHITS(I-1)
            STEPL=DSQRT((((MULTIRAY_DATA(3,I-1,N_HITS))-ZOLD)**2)+(((MULTIRAY_DATA(2,I-1,N_HITS))-YOLD)**2)+(((MULTIRAY_DATA(1,I-1,N_HITS))-XOLD)**2))
            IF(N_HITS.EQ.1) STEPL1=STEPL
            RAYRAY(8,I)=RAYRAY(8,I)+STEPL
!       CREATE NEW XOLD,YOLD,ZOLD
            XOLD=MULTIRAY_DATA(1,I-1,N_HITS)
            YOLD=MULTIRAY_DATA(2,I-1,N_HITS)
            ZOLD=MULTIRAY_DATA(3,I-1,N_HITS)
         END DO
         RAYRAY(8,I)=RAYRAY(8,I)-STEPL1
      END IF

      IF(RV) RAYRAY(8,I)=-RAYRAY(8,I)
      IF(.NOT.RV) RAYRAY(8,I)=RAYRAY(8,I)
      IF(DABS(RAYRAY(8,I)).GE.1.0D10) RAYRAY(8,I)=0.0D0

      IF(SNIND2.GT.0.0D0) RAYRAY(24,I)=1.0D0
      IF(SNIND2.LT.0.0D0) RAYRAY(24,I)=-1.0D0

      IF(GLANAM(I-1,2).EQ.'PERFECT      ') THEN
         RAYRAY(7,I)=0.0D0
         RAYRAY(8,I)=0.0D0
      END IF
      IF(GLANAM(I-1,2).EQ.'IDEAL        ') THEN
         RAYRAY(8,I)=-(ldm%getSurfIdealEFL(I-1)-ldm%getSurfThi(I-1))*RAYRAY(6,I-1)
      END IF
      IF(INT(WW3).GE.1.AND.INT(WW3).LE.10)RAYRAY(7,I)=RAYRAY(8,I)*DABS(ldm%getSurfIndex(I-1, INT(WW3)))
      IF(.NOT.RV) RAYRAY(7,I)=RAYRAY(7,I)+PHASE
      IF(RV) RAYRAY(7,I)=RAYRAY(7,I)-PHASE

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
            IF((RAYRAY(11,I)).LT.0.0D0) RAYRAY(11,I)=RAYRAY(11,I)+(TWOPII)
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
            IF((RAYRAY(12,I)).LT.0.0D0) RAYRAY(12,I)=RAYRAY(12,I)+(TWOPII)
         END IF
      END IF
      RAYRAY(22,I)=RAYRAY(22,(I-1))+RAYRAY(7,I)
      if(I == ISYS20 .AND.ldm%getSurfThi(I) .NE. 0) then
         call adjustLastSurface(I, RAYRAY)
      end if
      IF(STOPP.EQ.1) THEN
! NEW STUFF 6/2/94
         IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1.AND..NOT.ITRACE) THEN
            JKX=0.0D0
            JKY=0.0D0
            STOPP=0
            KKK=KKK+1
            CYCLE outer_retry
         END IF
         FAIL=.TRUE.
         RAYEXT=.FALSE.
         POLEXT=.FALSE.
         if (.not. for_optimization) then
!           [DIFF 10] GRASET plot-ray block: RAYTRA_OLD only
            IF(GLOBE) THEN
               CALL GLBRAY
            END IF
            IF(GRASET) THEN
               IF(GLOBE) THEN
                  call zoa_emit('GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION', 'black')
                  call zoa_emit('FOR RAY PLOTTING', 'black')
               END IF
               GLSURF=-99
               DO IK=0,NEWIMG
                  IF(DABS(ldm%getSurfThi(IK)).LE.1.0D10) THEN
                     GLSURF=IK
                     EXIT
                  END IF
               END DO
               IF(GLSURF.EQ.-99) THEN
                  GLOBE=.FALSE.
                  call zoa_emit('ALL SURFACES WERE OF INFINITE THICKNESS', 'black')
                  call zoa_emit('NO OPTICAL SYSTEM PLOT COULD BE MADE', 'black')
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
         else
!           [DIFF 10] RAYTRA2 MACFAL path
            IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
               CALL MACFAL
            END IF
            IF(GLOBE) THEN
               CALL GLBRAY
            END IF
         end if
         RETURN
      ELSE
         FAIL=.FALSE.
         STOPP=0
         RAYEXT=.TRUE.
      END IF

      IF(I.EQ.NEWREF) THEN
!       CALCULATE TARX AND TARY
         call compute_aim_target(I, WW1, WW2, TARX, TARY)

         TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
         IF(TEST.LE.AIMTOL.OR.sys_ray_aiming().EQ.0.0D0.OR.ITRACE) THEN
!       AIM IS GOOD ENOUGH, PROCEED
            AIMOK=.TRUE.
!           [DIFF 11] RAYTRA2 sets REFMISS and calls MISSREF
            if (for_optimization) then
               REFMISS=.FALSE.
               CALL MISSREF(X,Y)
            end if
            CYCLE surface_loop
         ELSE
            AIMOK=.FALSE.
!       AIM NOT GOOD ENOUGH, IMPROVE GUESS
         END IF

         X1ONE=X1LAST
         Y1ONE=Y1LAST
         X1LAST=XAIMOL
         Y1LAST=YAIMOL
         RXONE=RXLAST
         RYONE=RYLAST
         RXLAST=X
         RYLAST=Y

         IF(KKK.EQ.1) THEN
            X1AIM=XAIMOL+DDELX
            Y1AIM=YAIMOL+DDELY
            Z1AIM=ZAIMOL
            XC=X1AIM
            YC=Y1AIM
            ZC=Z1AIM
            XC1=XC
            YC1=YC
            ZC1=ZC
!           [DIFF 12] GETZEE1 guard: both use same logic
            IF(surf_curvature(1).NE.0.0D0)CALL GETZEE1
            X1AIM=XC
            Y1AIM=YC
            Z1AIM=ZC
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
            CYCLE newton_raphson
         END IF

         CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)

         MF1=TARX-RXLAST
         MF2=TARY-RYLAST
         DELFAIL=.FALSE.
         CALL NEWDEL(MF1,MF2,D11,D12,D21,D22,DELFAIL)
         IF(DELFAIL) THEN
            RETURN
         ELSE
            CYCLE newton_raphson
         END IF
      END IF

   end do surface_loop
   EXIT outer_retry
   end do newton_raphson
   end do outer_retry

!  CACOCH clap/cobs blockage check
   IF(CACOCH.EQ.1) THEN
      ray_blocked = .false.
      cacoch_loop: do R_I=NEWOBJ+1,NEWIMG-1
         R_X=RAYRAY(1,R_I)
         R_Y=RAYRAY(2,R_I)
         R_Z=RAYRAY(3,R_I)

         MMSG=MSG
         IF(ABS(ldm%getSurfSpecialType(R_I)).NE.24) THEN
            IF(surf_multi_clap_flag(R_I).EQ.0.AND.surf_multi_cobs_flag(R_I).EQ.0) THEN
               CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
            ELSE
               IF(surf_multi_clap_flag(R_I).NE.0) THEN
                  DO JK=1,surf_multi_clap_flag(R_I)
                     IF(MMSG) THEN
                        MSG=.TRUE.
                        IF(JK.LT.surf_multi_clap_flag(R_I)) MSG=.FALSE.
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
                        EXIT
                     END IF
                  END DO
               END IF
               IF(surf_multi_cobs_flag(R_I).NE.0) THEN
                  DO JK=1,surf_multi_cobs_flag(R_I)
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
                        EXIT
                     END IF
                  END DO
               END IF
            END IF
         END IF

         IF(STOPP.EQ.1) THEN
            ray_blocked = .true.
            EXIT cacoch_loop
         END IF
         STOPP=0
         FAIL=.FALSE.
         RAYEXT=.TRUE.
      end do cacoch_loop
   ELSE
      ray_blocked = .false.
   END IF

!  [DIFF 14] ray_blocked tail: RAYTRA2 calls MACFAL
   IF(ray_blocked) THEN
      FAIL=.TRUE.
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
      if (for_optimization) then
         IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
            CALL MACFAL
         END IF
      end if
   END IF

   IF(GLOBE) THEN
      CALL GLBRAY
   END IF

!  [DIFF 15] Post-trace tail: RAYTRA_OLD has GRASET block + compute_ray_energy;
!            RAYTRA2 has no GRASET + inline energy loop.
   if (.not. for_optimization) then
!     RAYTRA_OLD tail
      IF(GRASET) THEN
         IF(GLOBE) THEN
            call zoa_emit('GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION', 'black')
            call zoa_emit('FOR RAY PLOTTING', 'black')
         END IF
         GLSURF=-99
         glsurf_search: do I=0,NEWIMG
            IF(DABS(ldm%getSurfThi(I)).LE.1.0D10) THEN
               GLSURF=I
               EXIT glsurf_search
            END IF
         end do glsurf_search
         IF(GLSURF.EQ.-99) THEN
            GLOBE=.FALSE.
            call zoa_emit('ALL SURFACES WERE OF INFINITE THICKNESS', 'black')
            call zoa_emit('NO OPTICAL SYSTEM PLOT COULD BE MADE', 'black')
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
      call compute_ray_energy
   else
!     RAYTRA2 inline energy computation (uses ldm/surf_* accessors)
      WA3=INT(WW3)
      RAYRAY(25,NEWOBJ:NEWIMG)=0.0D0
      RAYRAY(34:38,NEWOBJ:NEWIMG)=0.0D0
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
            IF(WA3.GE.1.AND.WA3.LE.10) THEN
               RN1 = ldm%getSurfIndex(I, WA3)
               RN2 = RN1
            END IF
         ELSE
            IF(WA3.GE.1.AND.WA3.LE.10) THEN
               RN1 = ldm%getSurfIndex(I-1, WA3)
               RN2 = ldm%getSurfIndex(I,   WA3)
            END IF
         END IF
         IF(surf_special_type(I) == 19) THEN
            ISURF=I
            GERROR=.FALSE.
            XPASS=X
            YPASS=Y
            IPASS1=2
            CALL GRIDS(2,ISURF,GERROR)
            IF(.NOT.GERROR) GRIDSUNLOADED19(I)=.FALSE.
            IF(GERROR) THEN
               IF(MSG) THEN
                  CALL RAY_FAILURE(I)
                  call zoa_emit('GRID FILE DOES NOT EXIST FOR THIS SURFACE', 'black')
                  call zoa_emit('OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE', 'black')
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
         IF(sys_screen().EQ.1.0D0) THEN
            IF(I.EQ.INT(sys_screen_surf())) THEN
               AOI=DABS(DACOS(RAYRAY(9,I)))
               D=sys_screen_d()
               H=sys_screen_h()
               S=sys_screen_s()
               IF(DCOS(AOI).EQ.0.0D0.OR.AOI.GE.DABS(sys_screen_excl_angle())) THEN
                  FACTOR=0.0D0
               ELSE
                  FACTOR=PII*(((D)-(H*DSIN(AOI)))*(((D)*DCOS(AOI))-(H*DSIN(AOI))))/(4.0D0*S*S*DCOS(AOI))
               END IF
               IF(FACTOR.LT.0.0D0) FACTOR=0.0D0
               RAYRAY(25,I)=RAYRAY(25,I)*FACTOR
            END IF
         END IF
         IF(surf_special_type(I) /= 19) THEN
            IF(DUM(I).AND.I.GT.0) THEN
               RAYRAY(34:38,I)=0.0D0
            END IF
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
            CALL CROSS_PRODUCT(JK_CPL,JK_CPM,JK_CPN,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
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
            JK_L1=JK_CPL
            JK_M1=JK_CPM
            JK_N1=JK_CPN
            JK_L2=RAYRAY(13,I)
            JK_M2=RAYRAY(14,I)
            JK_N2=RAYRAY(15,I)
            CALL CROSS_PRODUCT(SA_CPL,SA_CPM,SA_CPN,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
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
            SA_CPN = RYN(I)
            MAG=DSQRT((1.0D0-(SA_CPN**2))/((SA_CPL**2)+(SA_CPM**2)))
            SA_CPL=MAG*SA_CPL
            SA_CPM=MAG*SA_CPM
            MAG=DSQRT((SA_CPL**2)+(SA_CPM**2)+(SA_CPN**2))
            JK_L1=RYL(I)
            JK_M1=RYM(I)
            JK_N1=RYN(I)
            JK_L2=SA_CPL
            JK_M2=SA_CPM
            JK_N2=SA_CPN
            CALL DOT_PRODUCT(DP,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2)
            IF(DP.GT.1.0D0) DP=1.0D0
            IF(DP.LT.-1.0D0) DP=-1.0D0
            POLANG=DACOS(DP)
            IF(POLANG.GT.PII/2.0D0) POLANG=PII-POLANG
            IF(POLANG.LT.-PII/2.0D0) POLANG=PII+POLANG
            RAYRAY(38,I)=(POLANG*180.0D0)/PII
            RAYRAY(39,I)=DCOS(POLANG)*DACOS(RAYRAY(9,I))
            RAYRAY(40,I)=DSIN(POLANG)*DACOS(RAYRAY(9,I))
            IF(COATSET) THEN
               J=INT(surf_coating_index(I))
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
               CALL ENERGY_ADJUST(ENERGY_FACTOR,I,J,IA,IAP,RN1,RN2,WA3,POLANG,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL)
               IF(I.GT.NEWOBJ) RAYRAY(25,I)=RAYRAY(25,I)*ENERGY_FACTOR
            END IF
            IF(surf_diffraction_flag(I) == 1.AND.surf_grating_spacing(I).NE.0.0D0) THEN
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
   end if
   RETURN

end subroutine real_ray_trace_core


end module
