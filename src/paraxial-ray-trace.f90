! This is a refactored version of the original KDP PRTRA sub in PARAX1.FOR.
! Wanted to make some changes (implement defocus at image surface) and this 
! refacgtoring is step 1
module paraxial_ray_trace_test
    use iso_fortran_env, only: real64
    contains
        SUBROUTINE PRTRA_NEW

            
            use type_utils, only: real2str, bool2str, int2str
            use parax_calcs
            use DATLEN
            use mod_lens_data_manager
    !
            use global_widgets
    
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
            INTEGER JK,L,ITYP,SLV1,SLV2,COMI, SF
    !
            COMMON/CSLVRS/SLV1,SLV2
    
            real(kind=real64) :: marPos1, marAng0

            logical :: newWay = .TRUE.
    
    !
            REAL*8 SYS13,TMP15A,TMP15B, &
            CON,CURV,TMP17A,TMP17B,WV
    !
            COMMON/PRCOM/WV,ITYP
    !
            COMMON/PIKCOM/COMI
    !
            INCLUDE 'DATMAI.INC'
            !INCLUDE 'DATLEN.INC'
          INTEGER WWVN
          IF(INT(SYSTEM(11)).EQ.1) WWVN=46
          IF(INT(SYSTEM(11)).EQ.2) WWVN=47
          IF(INT(SYSTEM(11)).EQ.3) WWVN=48
          IF(INT(SYSTEM(11)).EQ.4) WWVN=49
          IF(INT(SYSTEM(11)).EQ.5) WWVN=50
          IF(INT(SYSTEM(11)).EQ.6) WWVN=71
          IF(INT(SYSTEM(11)).EQ.7) WWVN=72
          IF(INT(SYSTEM(11)).EQ.8) WWVN=73
          IF(INT(SYSTEM(11)).EQ.9) WWVN=74
          IF(INT(SYSTEM(11)).EQ.10) WWVN=75
    !
    
          OUTLYNE = "NEW PRTRA ROUTINE STARTED! "
          CALL SHOWIT(19)
    
    
    
    !       Update Lens data class
    !     Dump data to interface modules
          call curr_lens_data%update()
    
          ! To deal with setting this properly with
          ! finite conjugate sytems and support the use case of this
          ! sub to ray trace to compute first order parameters
          call computeMarginalRayPosition(marPos1, marAng0)


          call LogTermFOR("IN PRTRANEW For real?")
            CON=SYSTEM(15)
            IF(systemHasYZPlane()) THEN
            !IF(ITYPEP.EQ.1.OR.ITYPEP.EQ.3) THEN
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
    !       7/23/91 SET CON = SYSTEM(15) FOR THE YZ PLANE TRACE
    !
    !       IF AN APERTURE STOP IS DEFINED ON ANY SURFACE
    !       THE VALUE OF SYSTEM(15) NEEDS TO BE REFINED.
    !
    !       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
    !       UP TO THE APERTURE STOP SURFACE (UNLESS THERE IS NO
    !       APERTURE STOP DEFINED) USING TWO DIFFERENT VALUES
    !       OF SYSTEM(15) [HEIGTH OF CHIEF RAY AT SURF 1]
    !
    !       THE TWO VALUES USED ARE 0.0 AND 0.1
    !
    !       THE CORRECET VALUE OF SYSTEM(15) WHICH MAKES PCY ON THE
    !       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
    !
    !       PCY(AT ASTOP FOR SYSTEM(15)=0.0) IS CALLED TMP15A
    !       PCY(AT ASTOP FOR SYSTEM(15)=0.1) IS CALLED TMP15B
    !
    !       SYSTEM(15)=((-.1*TMP15A)/(TMP15B-TMP15A))+SYSTEM(15)
    !
            IF(SYSTEM(26).GT.0.0D0.AND.SYSTEM(63).EQ.0.0D0) THEN
    !
    !       RECALCULATE THE CORRECT VALUE OF SYSTEM(15)
    !       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM(15)
    !
    !                       RAY TARGETING INFORMATION
    !
        DO JK=1,2
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
            call resolvePikup(0)                    


            PXTRAY(1:8,0:1) = setInitialParaxialRays(CON) 
            call LogTermFOR("New way implemented!")

    
            DO L=2,INT(SYSTEM(26))
    !               VALUES AT SURFACE L
                call resolveSolve(L)
    !       CALL PIKRES FOR THE SURFACE L
                call resolvePikup(L)      

            PXTRAY(1:4,L) = traNextSurf(PXTRAY(1:4,L-1),L, INT(SYSTEM(11)))
            PXTRAY(5:8,L) = traNextSurf(PXTRAY(5:8,L-1),L, INT(SYSTEM(11)))

    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
    !       WHEN ASTOP IS NOT ON SURFACE 1
    !
                END DO ! Trace to surface stop
        IF(JK.EQ.1) TMP15A=PXTRAY(5,(INT(SYSTEM(26))))
        IF(JK.EQ.2) TMP15B=PXTRAY(5,(INT(SYSTEM(26))))
            END DO ! Trace at two slightly different stop positions
            IF(TMP15A.EQ.TMP15B) THEN
               OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
               CALL SHOWIT(1)
               OUTLYNE='APERTURE STOP SURFACE.'
               CALL SHOWIT(1)
               OUTLYNE= 'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
               CALL SHOWIT(1)
                CALL MACFAL
                RETURN
            ELSE
                SYSTEM(15)=((-.1D0*TMP15A)/(TMP15B-TMP15A))+SYSTEM(15)
        END IF
    !
    !       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
    !       USING THIS VALUE OF SYSTEM(15)
            PXTRAY(1:8,0:1) = setInitialParaxialRays(SYSTEM(15))                 
 
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
            DO L=2,INT(SYSTEM(26))

            call resolveSolve(L)

    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
            call resolvePikup(L)    

            PXTRAY(1:4,L) = traNextSurf(PXTRAY(1:4,L-1),L, INT(SYSTEM(11)))
            PXTRAY(5:8,L) = traNextSurf(PXTRAY(5:8,L-1),L, INT(SYSTEM(11)))            

            END DO
    !
    !       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
    !       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
    !       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
    !       HERE. WE HAVE DEFINED. THIS IS THE
    !       APERTURE STOP SURFACE. REDEFINE IT AS L.
            L=INT(SYSTEM(26))
    !       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
    !       THIS IS DONE BY CALLING SUBROUTINE
            call resolveSolve(L)
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
    !       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (SYSTEM(26)+1)
    !       TO THE IMAGE SURFACE  WHERE:
            DO L=((INT(SYSTEM(26)))+1),INT(SYSTEM(20))
    !       CALL PIKRES FOR THE SURFACE L
            call resolvePikup(L)    
            PXTRAY(1:4,L) = traNextSurf(PXTRAY(1:4,L-1),L, INT(SYSTEM(11)))
            PXTRAY(5:8,L) = traNextSurf(PXTRAY(5:8,L-1),L, INT(SYSTEM(11)))            

    !
    !                       SOLVES ON SURFACE L
    !***************************************************************************
    !       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
            call resolveSolve(L)
    !
    !               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
            call LogTermFOR("L is "//int2str(L))
            if(L.EQ.INT(SYSTEM(20)).AND.ALENS(3,L).NE.0) then 
                call LogTermFOR("Nonzero image surf thickness!")
                PXTRAY(1:4,L) = traNextSurf(PXTRAY(1:4,L),L+1, INT(SYSTEM(11)))
                PXTRAY(5:8,L) = traNextSurf(PXTRAY(5:8,L),L+1, INT(SYSTEM(11)))                    
            end if
    !
            END DO
    !       TRACE COMPLETED
    !*******************************************************************************
                            ELSE
    !
    !       NO ASTOP OR TEL SET, USE THE EXISTING VALUE OF SYSTEM(15)
    !
    !*******************************************************************************
    !       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
    !       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
    !       0 OR 1.
    !               INITIAL VALUES AT SURFACE 0
    !       CALL PIKRES FOR THE OBJECT SURFACE
            call resolvePikup(0)    

            ! TODO:  Test this and clean it up.  Too confusing to read
    !       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            IF(SYSTEM(63).EQ.0.0D0) THEN
                PXTRAY(1:8,0:1) = setInitialParaxialRays(SYSTEM(15)) 
            END IF
            IF(SYSTEM(63).EQ.1.0D0) THEN
                
            IF(SYSTEM(14).EQ.0.0D0) THEN
                   
                PXTRAY(1:8,0:1) = setInitialParaxialRays(1.0D0)
            ELSE 
                PXTRAY(1:8,0:1) = setInitialParaxialRays(SYSTEM(14))
            END IF   
        END IF
            call resolvePikup(1)
 



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
            call resolveSolve(1)

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
                            DO L=2,INT(SYSTEM(20))
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
            call resolvePikup(L)
            PXTRAY(1:4,L) = traNextSurf(PXTRAY(1:4,L-1),L, INT(SYSTEM(11)))
            PXTRAY(5:8,L) = traNextSurf(PXTRAY(5:8,L-1),L, INT(SYSTEM(11)))  
            call resolveSolve(L)  
    !
    !       ALL SOLVES ON SURFACE L HANDLED
    !
                            END DO
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
    !       SET CON = SYSTEM(17)
                            CON=SYSTEM(17)
    !
            IF(systemHasXZPlane()) THEN
            !IF(ITYPEP.EQ.2.OR.ITYPEP.EQ.3) THEN
    !
    !       THIS IS SUBROUTINE PRTRA2. THIS IS THE FIRST OF THE
    !       SUBROUTINES WHICH IMPLEMENT THE PARAXIAL RAY TRACE.
    !       IT IS CALLED FROM LNSEOS. THIS SUBROUTINE
    !       TRACES ONLY IN THE XZ PLANE. PIKUPS ARE ALSO RESOLVED BY
    !       CALLS TO PIKRES FOR EACH SURFACE FROM THIS
    !       SUBROUTINE. THE XZ PLANE CROMATIC SURFACE COEFICIENTS
    !       ARE CALCULATED WITH A CALL TO CCOLX.FOR
    !
                            SYS13=SYSTEM(13)
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
    !       THE VALUE OF SYSTEM(17) NEEDS TO BE
    !       REFINED.
    !
    !       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
    !       UP TO THE APERTURE STOP SURFACE (WHEN THE APERTURE STOP
    !       IS NOT ON SURFACE 1) USING TWO DIFFERENT VALUES
    !       OF SYSTEM(17) [HEIGTH OF CHIEF RAY AT SURF 1]
    !
    !       THE TWO VALUES USED ARE 0.0 AND 0.1
    !
    !       THE CORRECET VALUE OF SYSTEM(17) WHICH MAKES PCX ON THE
    !       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
    !
    !       PCX(AT ASTOP FOR SYSTEM(17)=0.0) IS CALLED TMP17A
    !       PCX(AT ASTOP FOR SYSTEM(17)=0.1) IS CALLED TMP17B
    !
    !       SYSTEM(17)=((-.1D0*TMP17A)/(TMP17B-TMP17A))+SYSTEM(17)
    !
            IF(SYSTEM(26).GT.0.0D0.AND.SYSTEM(63).EQ.0.0D0) THEN
    !
    !       RECALCULATE THE CORRECT VALUE OF SYSTEM(17)
    !       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM(17)
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
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(0)=0,  ALWAYS
                            PXTRAX(1,0)=0.0D0
    !
    !       PUX(0)=SAX/TH(0)
    !
            IF(ALENS(3,0).EQ.0.0D0) THEN
           OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            END IF
                    PXTRAX(2,0)=(SYS13)/ALENS(3,0)
    !
    !       PIX(0) =PUX(0)
                            PXTRAX(3,0)=PXTRAX(2,0)
    !
    !       PIX'(0)=PUX(0)
                            PXTRAX(4,0)=PXTRAX(3,0)
    !
    !       PCX(0) =-SCX
                            PXTRAX(5,0)=(SYSTEM(16))
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
    !
    !       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       CON IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
    !
            IF(ALENS(3,0).EQ.0.0D0) THEN
           OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            END IF
            PXTRAX(6,0)=-((SYSTEM(16))-CON)/ALENS(3,0)
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(6,0)= &
          -(1.0D0-CON)/ALENS(3,0)
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                            PXTRAX(1,1)=(SYS13)
    !
    
    !       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
    !
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
    
    !
    !       PIX(1)=CV(1)*PX(1)+PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
    !
    !       PIX'(1)=(N/N')*PIX(1)
                    PXTRAX(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(3,1)
    !
    !       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                            PXTRAX(5,1)=CON
    !
    !       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
    !
    !       PICY(1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
    !       PICX'(1)
                    PXTRAX(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(7,1)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
           DO 5000 L=2,INT(SYSTEM(26))
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
    !       WHEN ASTOP IS NOT ON SURFACE 1
    !
     5000                     CONTINUE
                    IF(JK.EQ.1) TMP17A=PXTRAX(5,(INT(SYSTEM(26))))
                    IF(JK.EQ.2) TMP17B=PXTRAX(5,(INT(SYSTEM(26))))
     6000                     CONTINUE
            IF(TMP17A.EQ.TMP17B) THEN
            OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
          CALL SHOWIT(1)
            OUTLYNE='APERTURE STOP SURFACE.'
          CALL SHOWIT(1)
            OUTLYNE= &
          'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            ELSE
            SYSTEM(17)=((-.1D0*TMP17A)/(TMP17B-TMP17A))+SYSTEM(17)
                            END IF
    !
    !       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
    !       USING THIS VALUE OF SYSTEM(17)
    !
    !               INITIAL VALUES AT SURFACE 0
    !***************************************************************
    !
    !       CALL PIKRES FOR THE OBJECT SURFACE
                   COMI=0
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(0)=0,  ALWAYS
                            PXTRAX(1,0)=0.0D0
    !
    !       PUX(0)=SAX/TH(0)
                    PXTRAX(2,0)=(SYS13)/ALENS(3,0)
    !
    !       PIX(0) =PUX(0)
                            PXTRAX(3,0)=PXTRAX(2,0)
    !
    !       PIX'(0)=PUX(0)
                            PXTRAX(4,0)=PXTRAX(3,0)
    !
    !       PCX(0) =-SCX
                            PXTRAX(5,0)=(SYSTEM(16))
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
    !
    !       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       SYSTEM(17) IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
            PXTRAX(6,0)=-((SYSTEM(16))-SYSTEM(17))/ALENS(3,0)
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(6,0)= &
          -(1.0D0-SYSTEM(17))/ALENS(3,0)
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                            PXTRAX(1,1)=(SYS13)
    !
    !       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
    !
    !       PIX(1)=CV(1)*PX(1)+PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
    !
    !       PIX'(1)=(N/N')*PIX(1)
                    PXTRAX(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(3,1)
    !
    !       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                            PXTRAX(5,1)=SYSTEM(17)
    !
    !       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
    !
    !       PICX(1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
    !       PICX'(1)
                    PXTRAX(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(7,1)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                            DO 7000 L=2,INT(SYSTEM(26))
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !
     7000                     CONTINUE
    !
    !       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
    !       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
    !       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
    !       HERE. WE HAVE DEFINED . THIS IS THE
    !       APERTURE STOP SURFACE. REDEFINE IT AS L.
                            L=INT(SYSTEM(26))
    !       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
    !       THIS IS DONE BY CALLING SUBROUTINE
    !                               SLVRS
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
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
            DO 9000 L=((INT(SYSTEM(26)))+1),INT(SYSTEM(20))
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !               VALUES AT SURFACE L
    !
    !       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
    !       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       FINISHED WITH PX(L)
    !*******************************************************************************
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
    !       NO CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       FINISHED WITH PCX(L)
    !************************************************************************
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !                       SOLVES ON SURFACE L
    !***************************************************************************
    !       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
    !                       SLVRS
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !
    !               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
    !
     9000                     CONTINUE
    !       TRACE COMPLETED
    !*******************************************************************************
                            ELSE
    !
    !       NO ASTOP ASSIGNED OR TEL ON, USE THE EXISTING VALUE OF SYSTEM(17)
    !
    !*******************************************************************************
    !       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
    !       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
    !       0 OR 1.
    !               INITIAL VALUES AT SURFACE 0
    !       CALL PIKRES FOR THE OBJECT SURFACE
                   COMI=0
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !
    !       PX(0)=0,  ALWAYS
                            PXTRAX(1,0)=0.0D0
    !
    !       PUX(0)=SAX/TH(0)
                    PXTRAX(2,0)=(SYS13)/ALENS(3,0)
    !
    !       PIX(0) =PUX(0)
                            PXTRAX(3,0)=PXTRAX(2,0)
    !
    !       PIX'(0)=PUX(0)
                            PXTRAX(4,0)=PXTRAX(3,0)
    !
    !       PCX(0) =-SCX
                            PXTRAX(5,0)=(SYSTEM(16))
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
    !
    !       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       SYSTEM(17) IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
          IF(SYSTEM(63).EQ.0.0D0) &
          PXTRAX(6,0)=-((SYSTEM(16))-SYSTEM(17))/ALENS(3,0)
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(6,0)= &
          -(1.0D0-SYSTEM(17))/ALENS(3,0)
          IF(SYSTEM(63).EQ.1.0D0) &
          PXTRAX(6,0)=0.0D0
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                            PXTRAX(1,1)=(SYS13)
    !
    !       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
    !
    !       PIX(1)=CV(1)*PX(1)+PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
    !
    !       PIX'(1)=(N/N')*PIX(1)
                    PXTRAX(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(3,1)
    !
    !       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            IF(SYSTEM(63).EQ.0.0D0) PXTRAX(5,1)=SYSTEM(17)
            IF(SYSTEM(63).EQ.1.0D0) PXTRAX(5,1)=PXTRAX(5,0)
    !
    !       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
    !
    !       PICX(1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
    !       PICX'(1)
                    PXTRAX(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(7,1)
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
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
                            DO 8000 L=2,INT(SYSTEM(20))
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !*******************************************************************************
    !       PX(L)=PX(L-1)+CV(L-1)*PUX(L-1)
    !       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       FINISHED WITH PX(L)
    !
    !*******************************************************************************
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
    !*******************************************************************************
    !       NOW CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       FINISHED WITH PCX(L)
    !************************************************************************
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !                       NOW HANDLE SOLVES ON SURFACE L
    !***************************************************************************
    !       DO THIS BY CALLING SLVRS
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !       ALL SOLVES ON SURFACE L HANDLED
    !
    !
     8000                     CONTINUE
    !       PARAXIAL TRACE COMPLETED
                            END IF
    !    Populate data object
    !     Dump data to paraxial ray trace interface
    
          SF = INT(SYSTEM(20))
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

        SUBROUTINE PRTRA_TEMP

            
            use type_utils, only: real2str, bool2str
            use parax_calcs
            use DATLEN
            use mod_lens_data_manager
    !
            use global_widgets
    
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
            INTEGER JK,L,ITYP,SLV1,SLV2,COMI, SF
    !
            COMMON/CSLVRS/SLV1,SLV2
    
            real(kind=real64) :: marPos1, marAng0

            logical :: newWay = .TRUE.
    
    !
            REAL*8 SYS13,TMP15A,TMP15B, &
            CON,CURV,TMP17A,TMP17B,WV
    !
            COMMON/PRCOM/WV,ITYP
    !
            COMMON/PIKCOM/COMI
    !
            INCLUDE 'DATMAI.INC'
            !INCLUDE 'DATLEN.INC'
          INTEGER WWVN
          IF(INT(SYSTEM(11)).EQ.1) WWVN=46
          IF(INT(SYSTEM(11)).EQ.2) WWVN=47
          IF(INT(SYSTEM(11)).EQ.3) WWVN=48
          IF(INT(SYSTEM(11)).EQ.4) WWVN=49
          IF(INT(SYSTEM(11)).EQ.5) WWVN=50
          IF(INT(SYSTEM(11)).EQ.6) WWVN=71
          IF(INT(SYSTEM(11)).EQ.7) WWVN=72
          IF(INT(SYSTEM(11)).EQ.8) WWVN=73
          IF(INT(SYSTEM(11)).EQ.9) WWVN=74
          IF(INT(SYSTEM(11)).EQ.10) WWVN=75
    !
    
          OUTLYNE = "NEW PRTRA ROUTINE STARTED! "
          CALL SHOWIT(19)
    
    
    
    !       Update Lens data class
    !     Dump data to interface modules
          call curr_lens_data%update()
    
          ! To deal with setting this properly with
          ! finite conjugate sytems and support the use case of this
          ! sub to ray trace to compute first order parameters
          call computeMarginalRayPosition(marPos1, marAng0)


    
            CON=SYSTEM(15)
            IF(systemHasYZPlane()) THEN
            !IF(ITYPEP.EQ.1.OR.ITYPEP.EQ.3) THEN
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
    !       7/23/91 SET CON = SYSTEM(15) FOR THE YZ PLANE TRACE
    !
    !       IF AN APERTURE STOP IS DEFINED ON ANY SURFACE
    !       THE VALUE OF SYSTEM(15) NEEDS TO BE REFINED.
    !
    !       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
    !       UP TO THE APERTURE STOP SURFACE (UNLESS THERE IS NO
    !       APERTURE STOP DEFINED) USING TWO DIFFERENT VALUES
    !       OF SYSTEM(15) [HEIGTH OF CHIEF RAY AT SURF 1]
    !
    !       THE TWO VALUES USED ARE 0.0 AND 0.1
    !
    !       THE CORRECET VALUE OF SYSTEM(15) WHICH MAKES PCY ON THE
    !       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
    !
    !       PCY(AT ASTOP FOR SYSTEM(15)=0.0) IS CALLED TMP15A
    !       PCY(AT ASTOP FOR SYSTEM(15)=0.1) IS CALLED TMP15B
    !
    !       SYSTEM(15)=((-.1*TMP15A)/(TMP15B-TMP15A))+SYSTEM(15)
    !
            IF(SYSTEM(26).GT.0.0D0.AND.SYSTEM(63).EQ.0.0D0) THEN
    !
    !       RECALCULATE THE CORRECT VALUE OF SYSTEM(15)
    !       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM(15)
    !
    !                       RAY TARGETING INFORMATION
    !
        DO JK=1,2
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
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
            if (newWay) then
                PXTRAY(1:8,0:1) = setInitialParaxialRays(CON) 
                call LogTermFOR("New way implemented!")
            else
                !PXTRAY(1:8,0:1) = setInitialParaxialRays(CON) 
                !PRINT *, "New way PXTRAY(1:8,0:1) is ", PXTRAY(1:8,0:1)

    !       PY(0)=0,  ALWAYS
            PXTRAY(1,0)=0.0D0
    !
    !       PUY(0)=SAY/TH(0)
    !
            IF(ALENS(3,0).EQ.0.0D0) THEN
           OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            END IF
                    
           if(marAng0.GT.0) then
           PRINT *, "NEW finite marAng0 beginning is ", PXTRAY(1:8,0:1)
           end if

           PXTRAY(2,0) = marAng0

    !       PIY(0) =PUY(0)
            PXTRAY(3,0)=PXTRAY(2,0)
    !
    !       PIY'(0)=PUY(0)
            PXTRAY(4,0)=PXTRAY(3,0)
    !
    !       PCY(0) =-SCY
            PXTRAY(5,0)=(SYSTEM(14))
          IF(SYSTEM(14).EQ.0.0D0) PXTRAY(5,0)=1.0D0
    !
    !       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       CON IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
    !
          ! if (ldm%getSurfaceThickness(0))
          IF(ldm%getSurfThi(0).EQ.0.0D0) THEN  
          !IF(ALENS(3,0).EQ.0.0D0) THEN
            OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
           END IF
            PXTRAY(6,0)=-((SYSTEM(14))-CON)/ALENS(3,0)
          IF(SYSTEM(14).EQ.0.0D0) PXTRAY(6,0)= &
          -(1.0D0-CON)/ALENS(3,0)
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
          IF(ldm%isThiSolveOnSurf(0)) THEN   
          !IF(SOLVE(6,L).NE.0.0D0) THEN
            SLV1 = L
            CALL SLVRS
          END IF
    
          L=1
          SLV1=L
          SLV2=1
          IF(ldm%isThiSolveOnSurf(L).OR. &
          ldm%isYZCurvSolveOnSurf(L)) THEN          
          !IF(SOLVE(6,L).NE.0.0D0.OR. &
          !SOLVE(8,L).NE.0.0D0) THEN
            CALL SLVRS
           END IF
    !       INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    
    
            ! Experimental code !  go back to original and fix it correctly!
            PXTRAY(1,1) = marPos1

    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAY(2,1)=-CURV*PXTRAY(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAY(2,0)


            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
    
    !
    !       PIY(1)=CV(1)*PY(1)+PUY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
                    PRINT *, "Old PXTRAY(3,1) is ", PXTRAY(3,1)         
    !
    !       PIY'(1)=(N/N')*PIY(1)
                    PXTRAY(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAY(3,1)
    !
    !       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            PXTRAY(5,1)=CON
    !
    !       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAY(6,1)=-CURV*PXTRAY(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAY(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
    !
    !       PICY(1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
    !       PICY'(1)
                    PXTRAY(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAY(7,1)
                end if        
    
            PRINT *, "Old way PXTRAY(1:8,1) is ", PXTRAY(1:8,0:1)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
    
            DO L=2,INT(SYSTEM(26))
    !               VALUES AT SURFACE L
                           SLV1=L
                           SLV2=1 !TODO;  Move this to new SOLVRS routine as this is an input

          IF(ldm%isThiSolveOnSurf(L).OR. &
          ldm%isYZCurvSolveOnSurf(L)) CALL SLVRS
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES

            PXTRAY(1:4,L) = traNextSurf(PXTRAY(1:4,L-1),L, INT(SYSTEM(11)))
            PXTRAY(5:8,L) = traNextSurf(PXTRAY(5:8,L-1),L, INT(SYSTEM(11)))

    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
    !       WHEN ASTOP IS NOT ON SURFACE 1
    !
                END DO ! Trace to surface stop
                    IF(JK.EQ.1) TMP15A=PXTRAY(5,(INT(SYSTEM(26))))
                    IF(JK.EQ.2) TMP15B=PXTRAY(5,(INT(SYSTEM(26))))
            END DO ! Trace at two slightly different stop positions
            IF(TMP15A.EQ.TMP15B) THEN
               OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
               CALL SHOWIT(1)
               OUTLYNE='APERTURE STOP SURFACE.'
               CALL SHOWIT(1)
               OUTLYNE= 'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
               CALL SHOWIT(1)
                CALL MACFAL
                RETURN
            ELSE
            SYSTEM(15)=((-.1D0*TMP15A)/(TMP15B-TMP15A))+SYSTEM(15)
                            END IF
    !
    !       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
    !       USING THIS VALUE OF SYSTEM(15)
    !
    !               INITIAL VALUES AT SURFACE 0
    !***************************************************************
    !
    !       CALL PIKRES FOR THE OBJECT SURFACE
                   COMI=0
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PY(0)=0,  ALWAYS
            PXTRAY(1,0)=0.0D0
            PXTRAY(2,0) = marAng0
            ! Experimental code !  go back to original and fix it correctly!
            ! if (ENPUZ == 0) then
            !         PXTRAY(2,0)=(SYSTEM(12))/ALENS(3,0)
            ! else
            !         PXTRAY(2,0)=nao
            ! end if
    
    !
    !       PUY(0)=SAY/TH(0)
            !        PXTRAY(2,0)=(SYSTEM(12))/ALENS(3,0)
    !
    !       PIY(0) =PUY(0)
            PXTRAY(3,0)=PXTRAY(2,0)
    !
    !       PIY'(0)=PUY(0)
            PXTRAY(4,0)=PXTRAY(3,0)
    !
    !       PCY(0) =-SCY
            PXTRAY(5,0)=(SYSTEM(14))
          IF(SYSTEM(14).EQ.0.0D0) PXTRAY(5,0)=1.0D0
    !
    !       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       SYSTEM(15) IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
            PXTRAY(6,0)=-((SYSTEM(14))-SYSTEM(15))/ALENS(3,0)
          IF(SYSTEM(14).EQ.0.0D0) PXTRAY(6,0)= &
          -(1.0D0-SYSTEM(15))/ALENS(3,0)
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
          IF(SOLVE(6,L).NE.0.0D0.OR. &
          SOLVE(8,L).NE.0.0D0) CALL SLVRS
    !
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    
            PXTRAY(1,1) = marPos1

    !       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAY(2,1)=-CURV*PXTRAY(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAY(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
    !
    !       PIY(1)=CV(1)*PY(1)+PUY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
        !PRINT *, "Old PXTRAY(3,1) is ", PXTRAY(3,1)
    !
    !       PIY'(1)=(N/N')*PIY(1)
                    PXTRAY(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAY(3,1)
    !
    !       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                            PXTRAY(5,1)=SYSTEM(15)
    !
    !       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAY(6,1)=-CURV*PXTRAY(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAY(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
    !
    !       PICY(1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
    !       PICY'(1)
                    PXTRAY(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAY(7,1)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                            DO 70 L=2,INT(SYSTEM(26))
                           SLV1=L
                           SLV2=1
          IF(SOLVE(6,L).NE.0.0D0.OR. &
          SOLVE(8,L).NE.0.0D0) CALL SLVRS
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
    !
    !       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
    !
    !       PIY(L)=CV(1)*PY(L)+PUY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
    !
    !       PIY'(L)=(N/N')*PIY(L)
                    PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAY(3,L)
    !
    !       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
    !
    !       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
    !
    !       PICY(L)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
    !       PICY'(L)
                    PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAY(7,L)
    !
    !
     70                     CONTINUE
    !
    !       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
    !       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
    !       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
    !       HERE. WE HAVE DEFINED. THIS IS THE
    !       APERTURE STOP SURFACE. REDEFINE IT AS L.
                            L=INT(SYSTEM(26))
    !       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
    !       THIS IS DONE BY CALLING SUBROUTINE
                           SLV1=L
                           SLV2=1
          IF(SOLVE(6,L).NE.0.0D0.OR. &
          SOLVE(8,L).NE.0.0D0) CALL SLVRS
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
    !       NOW TRACE FROM THE APERTURE STOP SURFACE+1  (SYSTEM(26)+1)
    !       TO THE IMAGE SURFACE  WHERE:
            DO 90 L=((INT(SYSTEM(26)))+1),INT(SYSTEM(20))
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !               VALUES AT SURFACE L
    !
    !       PY(L) = PY(L-1)+TH(L-1)*PUY(L-1) ; THIS IS THE TRANSFER EQUATION
    !       NOW CALCULATE PY VALUE
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
    !
    !       FINISHED WITH PY(L)
    !*******************************************************************************
    !
    !       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
    !
    !       PIY(L)=CV(1)*PY(L)+PUY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
    !
    !       PIY'(L)=(N/N')*PIY(L)
                    PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAY(3,L)
    !
    !       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
    !       NO CALCULATE PCY VALUE
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
    !
    !       FINISHED WITH PCY(L)
    !************************************************************************
    !
    !       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
    !
    !       PICY(L)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
    !       PICY'(L)
                    PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAY(7,L)
    !
    !                       SOLVES ON SURFACE L
    !***************************************************************************
    !       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
                           SLV1=L
                           SLV2=1
          IF(SOLVE(6,L).NE.0.0D0.OR. &
          SOLVE(8,L).NE.0.0D0) CALL SLVRS
    !
    !               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
    !
     90                     CONTINUE
    !       TRACE COMPLETED
    !*******************************************************************************
                            ELSE
    !
    !       NO ASTOP OR TEL SET, USE THE EXISTING VALUE OF SYSTEM(15)
    !
    !*******************************************************************************
    !       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
    !       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
    !       0 OR 1.
    !               INITIAL VALUES AT SURFACE 0
    !       CALL PIKRES FOR THE OBJECT SURFACE
                   COMI=0
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !
    !       PY(0)=0,  ALWAYS
                            PXTRAY(1,0)=0.0D0
    !
            ! Experimental code !  go back to original and fix it correctly!
            PXTRAY(2,0) = marAng0
            ! if (ENPUZ == 0) then
            !         PXTRAY(2,0)=(SYSTEM(12))/ALENS(3,0)
            ! else
            !         PXTRAY(2,0)=nao
            ! end if
    
    !       PUY(0)=SAY/TH(0)
                    !PXTRAY(2,0)=(SYSTEM(12))/ALENS(3,0)
    !
    !       PIY(0) =PUY(0)
                            PXTRAY(3,0)=PXTRAY(2,0)
    !
    !       PIY'(0)=PUY(0)
                            PXTRAY(4,0)=PXTRAY(3,0)
    !
    !       PCY(0) =-SCY
                            PXTRAY(5,0)=(SYSTEM(14))
          IF(SYSTEM(14).EQ.0.0D0) PXTRAY(5,0)=1.0D0
    !
    !       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       SYSTEM(15) IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
          IF(SYSTEM(63).EQ.0.0D0) &
          PXTRAY(6,0)=-((SYSTEM(14))-SYSTEM(15))/ALENS(3,0)
          IF(SYSTEM(14).EQ.0.0D0) PXTRAY(6,0)= &
          -(1.0D0-SYSTEM(15))/ALENS(3,0)
          IF(SYSTEM(63).EQ.1.0D0) &
          PXTRAY(6,0)=0.0D0
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
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    
            ! Experimental code !  go back to original and fix it correctly!
            PXTRAY(1,1) = marPos1
    !         if (ENPUZ == 0) then
    !                 PXTRAY(1,1)=(SYSTEM(12))
    !         else
    !                 PXTRAY(1,1)=tan(thetao)*curr_lens_data%thicknesses(1)
    !         end if
    !         PRINT *, "PXTRAY(1,1) i ", PXTRAY(1,1)
    ! C
    !       PY(1) IS EQUAL TO THE SPECIFIED SAY VALUE IN SYSTEM(12)
                            !PXTRAY(1,1)=(SYSTEM(12))
    !
    !       PUY(1) =-CV(1)*PY(1)*((N'-N)/N')+(N/N')*PUY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAY(2,1)=-CURV*PXTRAY(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAY(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAY(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAY(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(1,1))+PXTRAY(2,1-1)
    !
    !       PIY(1)=CV(1)*PY(1)+PUY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAY(3,1)=CURV*PXTRAY(1,1)+PXTRAY(2,0)
    !
    !       PIY'(1)=(N/N')*PIY(1)
                    PXTRAY(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAY(3,1)
    !
    !       PCY(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            IF(SYSTEM(63).EQ.0.0D0) PXTRAY(5,1)=SYSTEM(15)
            IF(SYSTEM(63).EQ.1.0D0) PXTRAY(5,1)=PXTRAY(5,0)
    !
    !       PUCY(1) =-CV(1)*PCY(1)*((N'-N)/N')+(N/N')*PUCY(0)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAY(6,1)=-CURV*PXTRAY(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAY(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAY(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAY(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAY(5,1))+PXTRAY(6,1-1)
    !
    !       PICY(1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.2.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAY(7,1)=(CURV*PXTRAY(5,1))+PXTRAY(6,0)
    !       PICY'(1)
                    PXTRAY(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAY(7,1)
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
          IF(SOLVE(6,L).NE.0.0D0.OR. &
          SOLVE(8,L).NE.0.0D0) CALL SLVRS
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
                            DO 80 L=2,INT(SYSTEM(20))
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !*******************************************************************************
    !       PY(L)=PY(L-1)+CV(L-1)*PUY(L-1)
    !       NOW CALCULATE PY VALUE
            PXTRAY(1,L)=PXTRAY(1,(L-1))+(ALENS(3,(L-1))*PXTRAY(2,(L-1)))
    !
    !       FINISHED WITH PY(L)
    !
    !*******************************************************************************
    !
    !       PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAY(2,L)=-CURV*PXTRAY(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAY(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAY(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAY(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(1,L))+PXTRAY(2,L-1)
    !
    !       PIY(L)=CV(1)*PY(L)+PUY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAY(3,L)=CURV*PXTRAY(1,L)+PXTRAY(2,(L-1))
    !
    !       PIY'(L)=(N/N')*PIY(L)
                    PXTRAY(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAY(3,L)
    !
    !       PCY(L) = PCY(L-1)+TH(L-1)*PUCY(L-1) ; THIS IS THE TRANSFER EQUATION
    !*******************************************************************************
    !       NO CALCULATE PCY VALUE
            PXTRAY(5,L)=PXTRAY(5,(L-1))+(ALENS(3,(L-1))*PXTRAY(6,(L-1)))
    !
    !       FINISHED WITH PCY(L)
    !************************************************************************
    !
    !       PUCY(L) =-CV(L)*PCY(L)*((N'-N)/N')+(N/N')*PUCY(L-1)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAY(6,L)=-CURV*PXTRAY(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAY(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAY(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAY(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAY(5,L))+PXTRAY(6,L-1)
    !
    !       PICY(L)
    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.2.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAY(7,L)=(CURV*PXTRAY(5,L))+PXTRAY(6,(L-1))
    !       PICY'(L)
                    PXTRAY(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAY(7,L)
    !
    !                       NOW HANDLE SOLVES ON SURFACE L
    !***************************************************************************
    !       DO THIS BY CALLING SLVRS
                           SLV1=L
                           SLV2=1
          IF(SOLVE(6,L).NE.0.0D0.OR. &
          SOLVE(8,L).NE.0.0D0) CALL SLVRS
    !       ALL SOLVES ON SURFACE L HANDLED
    !
     80                     CONTINUE
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
    !       SET CON = SYSTEM(17)
                            CON=SYSTEM(17)
    !
            IF(systemHasXZPlane()) THEN
            !IF(ITYPEP.EQ.2.OR.ITYPEP.EQ.3) THEN
    !
    !       THIS IS SUBROUTINE PRTRA2. THIS IS THE FIRST OF THE
    !       SUBROUTINES WHICH IMPLEMENT THE PARAXIAL RAY TRACE.
    !       IT IS CALLED FROM LNSEOS. THIS SUBROUTINE
    !       TRACES ONLY IN THE XZ PLANE. PIKUPS ARE ALSO RESOLVED BY
    !       CALLS TO PIKRES FOR EACH SURFACE FROM THIS
    !       SUBROUTINE. THE XZ PLANE CROMATIC SURFACE COEFICIENTS
    !       ARE CALCULATED WITH A CALL TO CCOLX.FOR
    !
                            SYS13=SYSTEM(13)
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
    !       THE VALUE OF SYSTEM(17) NEEDS TO BE
    !       REFINED.
    !
    !       THE FIRST STEP IS TO PERFORM THE PARAXIAL RAY TRACE
    !       UP TO THE APERTURE STOP SURFACE (WHEN THE APERTURE STOP
    !       IS NOT ON SURFACE 1) USING TWO DIFFERENT VALUES
    !       OF SYSTEM(17) [HEIGTH OF CHIEF RAY AT SURF 1]
    !
    !       THE TWO VALUES USED ARE 0.0 AND 0.1
    !
    !       THE CORRECET VALUE OF SYSTEM(17) WHICH MAKES PCX ON THE
    !       APERTURE STOP EQUAL TO ZERO IS GIVEN BY:
    !
    !       PCX(AT ASTOP FOR SYSTEM(17)=0.0) IS CALLED TMP17A
    !       PCX(AT ASTOP FOR SYSTEM(17)=0.1) IS CALLED TMP17B
    !
    !       SYSTEM(17)=((-.1D0*TMP17A)/(TMP17B-TMP17A))+SYSTEM(17)
    !
            IF(SYSTEM(26).GT.0.0D0.AND.SYSTEM(63).EQ.0.0D0) THEN
    !
    !       RECALCULATE THE CORRECT VALUE OF SYSTEM(17)
    !       OTHERWISE, USE THE USER PROVIDED VALUE OF SYSTEM(17)
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
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(0)=0,  ALWAYS
                            PXTRAX(1,0)=0.0D0
    !
    !       PUX(0)=SAX/TH(0)
    !
            IF(ALENS(3,0).EQ.0.0D0) THEN
           OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            END IF
                    PXTRAX(2,0)=(SYS13)/ALENS(3,0)
    !
    !       PIX(0) =PUX(0)
                            PXTRAX(3,0)=PXTRAX(2,0)
    !
    !       PIX'(0)=PUX(0)
                            PXTRAX(4,0)=PXTRAX(3,0)
    !
    !       PCX(0) =-SCX
                            PXTRAX(5,0)=(SYSTEM(16))
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
    !
    !       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       CON IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
    !
            IF(ALENS(3,0).EQ.0.0D0) THEN
           OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            END IF
            PXTRAX(6,0)=-((SYSTEM(16))-CON)/ALENS(3,0)
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(6,0)= &
          -(1.0D0-CON)/ALENS(3,0)
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                            PXTRAX(1,1)=(SYS13)
    !
    
    !       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
    !
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
    
    !
    !       PIX(1)=CV(1)*PX(1)+PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
    !
    !       PIX'(1)=(N/N')*PIX(1)
                    PXTRAX(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(3,1)
    !
    !       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                            PXTRAX(5,1)=CON
    !
    !       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
    !
    !       PICY(1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
    !       PICX'(1)
                    PXTRAX(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(7,1)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
           DO 5000 L=2,INT(SYSTEM(26))
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 2 TO ASTOP
    !       WHEN ASTOP IS NOT ON SURFACE 1
    !
     5000                     CONTINUE
                    IF(JK.EQ.1) TMP17A=PXTRAX(5,(INT(SYSTEM(26))))
                    IF(JK.EQ.2) TMP17B=PXTRAX(5,(INT(SYSTEM(26))))
     6000                     CONTINUE
            IF(TMP17A.EQ.TMP17B) THEN
            OUTLYNE='PARAXIAL CHIEF RAY CAN NOT INTERSECT CURRENT'
          CALL SHOWIT(1)
            OUTLYNE='APERTURE STOP SURFACE.'
          CALL SHOWIT(1)
            OUTLYNE= &
          'PARAXIAL RAYS CAN NOT BE TRACED IN THIS SYSTEM'
          CALL SHOWIT(1)
                            CALL MACFAL
                            RETURN
                            ELSE
            SYSTEM(17)=((-.1D0*TMP17A)/(TMP17B-TMP17A))+SYSTEM(17)
                            END IF
    !
    !       NOW TRACE FROM THE OBJECT SURFACE TO THE ASTOP SURFACE
    !       USING THIS VALUE OF SYSTEM(17)
    !
    !               INITIAL VALUES AT SURFACE 0
    !***************************************************************
    !
    !       CALL PIKRES FOR THE OBJECT SURFACE
                   COMI=0
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(0)=0,  ALWAYS
                            PXTRAX(1,0)=0.0D0
    !
    !       PUX(0)=SAX/TH(0)
                    PXTRAX(2,0)=(SYS13)/ALENS(3,0)
    !
    !       PIX(0) =PUX(0)
                            PXTRAX(3,0)=PXTRAX(2,0)
    !
    !       PIX'(0)=PUX(0)
                            PXTRAX(4,0)=PXTRAX(3,0)
    !
    !       PCX(0) =-SCX
                            PXTRAX(5,0)=(SYSTEM(16))
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
    !
    !       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       SYSTEM(17) IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
            PXTRAX(6,0)=-((SYSTEM(16))-SYSTEM(17))/ALENS(3,0)
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(6,0)= &
          -(1.0D0-SYSTEM(17))/ALENS(3,0)
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                            PXTRAX(1,1)=(SYS13)
    !
    !       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
    !
    !       PIX(1)=CV(1)*PX(1)+PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
    !
    !       PIX'(1)=(N/N')*PIX(1)
                    PXTRAX(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(3,1)
    !
    !       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
                            PXTRAX(5,1)=SYSTEM(17)
    !
    !       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
    !
    !       PICX(1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
    !       PICX'(1)
                    PXTRAX(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(7,1)
    !
    !       THIS COMPLETES THE INITIAL VALUE CALCULATIONS FOR SURFACES 0 AND 1
    ! *****************************************************************************
    !       NOW TRACE TO THE APERTURE STOP SURFACE  WHERE:
                            DO 7000 L=2,INT(SYSTEM(26))
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !
     7000                     CONTINUE
    !
    !       HERE ALL PARAXIAL VALUES HAVE BEEN CALCULATED FOR THE
    !       OBJECT THROUGH THE APERTURE STOP IN THE ABSCENSE OF ANY SOLVES.
    !       THERE CAN BE SOLVES ON THE APERTURE STOP SURFACE. HANDLE THESE
    !       HERE. WE HAVE DEFINED . THIS IS THE
    !       APERTURE STOP SURFACE. REDEFINE IT AS L.
                            L=INT(SYSTEM(26))
    !       NOW HANDLE ALL SOLVES ON THE APERTURE STOP SURFACE.
    !       THIS IS DONE BY CALLING SUBROUTINE
    !                               SLVRS
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
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
            DO 9000 L=((INT(SYSTEM(26)))+1),INT(SYSTEM(20))
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !               VALUES AT SURFACE L
    !
    !       PX(L) = PX(L-1)+TH(L-1)*PUX(L-1) ; THIS IS THE TRANSFER EQUATION
    !       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       FINISHED WITH PX(L)
    !*******************************************************************************
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
    !       NO CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       FINISHED WITH PCX(L)
    !************************************************************************
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !                       SOLVES ON SURFACE L
    !***************************************************************************
    !       HANDLE SOLVES ON SURFACE L BY CALLING SUBROUTINE
    !                       SLVRS
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !
    !               NOW ALL SOLVES ON SURFACE L HAVE BEEN HANDLED
    !
     9000                     CONTINUE
    !       TRACE COMPLETED
    !*******************************************************************************
                            ELSE
    !
    !       NO ASTOP ASSIGNED OR TEL ON, USE THE EXISTING VALUE OF SYSTEM(17)
    !
    !*******************************************************************************
    !       TRACE FROM THE OBJECT TO THE IMAGE, PROPERLY HANDLING ALL PIKUPS
    !       AND SOLVES ALONG THE WAY. THEN PROCEED. NO SOLVES OCCUR ON SURFACES
    !       0 OR 1.
    !               INITIAL VALUES AT SURFACE 0
    !       CALL PIKRES FOR THE OBJECT SURFACE
                   COMI=0
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !
    !       PX(0)=0,  ALWAYS
                            PXTRAX(1,0)=0.0D0
    !
    !       PUX(0)=SAX/TH(0)
                    PXTRAX(2,0)=(SYS13)/ALENS(3,0)
    !
    !       PIX(0) =PUX(0)
                            PXTRAX(3,0)=PXTRAX(2,0)
    !
    !       PIX'(0)=PUX(0)
                            PXTRAX(4,0)=PXTRAX(3,0)
    !
    !       PCX(0) =-SCX
                            PXTRAX(5,0)=(SYSTEM(16))
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(5,0)=1.0D0
    !
    !       PUCX(0)=(SCX-ADJUSTMENT ON SURFACE 1)/TH(0)
    !       SYSTEM(17) IS CHIEF RAY POSITION ON SURFACE 1
    !       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
          IF(SYSTEM(63).EQ.0.0D0) &
          PXTRAX(6,0)=-((SYSTEM(16))-SYSTEM(17))/ALENS(3,0)
          IF(SYSTEM(16).EQ.0.0D0) PXTRAX(6,0)= &
          -(1.0D0-SYSTEM(17))/ALENS(3,0)
          IF(SYSTEM(63).EQ.1.0D0) &
          PXTRAX(6,0)=0.0D0
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !               INITIAL VALUES AT SURFACE 1
    !       CALL PIKRES FOR THE SURFACE 1
                   COMI=1
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !
    !       PX(1) IS EQUAL TO THE SPECIFIED SAX VALUE IN SYS13
                            PXTRAX(1,1)=(SYS13)
    !
    !       PUX(1) =-CV(1)*PX(1)*((N'-N)/N')+(N/N')*PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(2,1)=-CURV*PXTRAX(1,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(2,0)
            IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
            IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(2,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(1,1))+PXTRAX(2,1-1)
    !
    !       PIX(1)=CV(1)*PX(1)+PUX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(3,1)=CURV*PXTRAX(1,1)+PXTRAX(2,0)
    !
    !       PIX'(1)=(N/N')*PIX(1)
                    PXTRAX(4,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(3,1)
    !
    !       PCX(1)=(ADJUSTMENT ON SURFACE 1 IF ANY)
            IF(SYSTEM(63).EQ.0.0D0) PXTRAX(5,1)=SYSTEM(17)
            IF(SYSTEM(63).EQ.1.0D0) PXTRAX(5,1)=PXTRAX(5,0)
    !
    !       PUCX(1) =-CV(1)*PCX(1)*((N'-N)/N')+(N/N')*PUCX(0)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
            PXTRAX(6,1)=-CURV*PXTRAX(5,1)* &
            (((ALENS(WWVN,1))- &
            (ALENS(WWVN,0)))/ &
            (ALENS(WWVN,1)))+ &
            ((ALENS(WWVN,0))/ &
            (ALENS(WWVN,1)))*PXTRAX(6,0)
          IF(GLANAM(1,2).EQ.'PERFECT      ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(3,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
          IF(GLANAM(1,2).EQ.'IDEAL        ') &
          PXTRAX(6,1)=(-(1.0D0/ALENS(121,1))*PXTRAX(5,1))+PXTRAX(6,1-1)
    !
    !       PICX(1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,1).EQ.1.0D0) THEN
                    CURV=ALENS(24,1)
                    ELSE
            IF(ALENS(1,1).EQ.0.0D0.AND.ALENS(43,1).NE.0.0D0) THEN
                    CURV=ALENS(43,1)*2.0D0
                    ELSE
                    CURV=ALENS(1,1)
                    END IF
                    END IF
                    PXTRAX(7,1)=(CURV*PXTRAX(5,1))+PXTRAX(6,0)
    !       PICX'(1)
                    PXTRAX(8,1)=((ALENS((WWVN),0))/ &
                    (ALENS((WWVN),1)))*PXTRAX(7,1)
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
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
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
                            DO 8000 L=2,INT(SYSTEM(20))
    !               VALUES AT SURFACE L
    !       CALL PIKRES FOR THE SURFACE L
                   COMI=L
            IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
    !*******************************************************************************
    !       PX(L)=PX(L-1)+CV(L-1)*PUX(L-1)
    !       NOW CALCULATE PX VALUE
            PXTRAX(1,L)=PXTRAX(1,(L-1))+(ALENS(3,(L-1))*PXTRAX(2,(L-1)))
    !
    !       FINISHED WITH PX(L)
    !
    !*******************************************************************************
    !
    !       PUX(L) =-CV(L)*PX(L)*((N'-N)/N')+(N/N')*PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(2,L)=-CURV*PXTRAX(1,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(2,(L-1))
            IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
            IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(2,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(1,L))+PXTRAX(2,L-1)
    !
    !       PIX(L)=CV(1)*PX(L)+PUX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(3,L)=CURV*PXTRAX(1,L)+PXTRAX(2,(L-1))
    !
    !       PIX'(L)=(N/N')*PIX(L)
                    PXTRAX(4,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(3,L)
    !
    !       PCX(L) = PCX(L-1)+TH(L-1)*PUCX(L-1) ; THIS IS THE TRANSFER EQUATION
    !*******************************************************************************
    !       NOW CALCULATE PCX VALUE
            PXTRAX(5,L)=PXTRAX(5,(L-1))+(ALENS(3,(L-1))*PXTRAX(6,(L-1)))
    !
    !       FINISHED WITH PCX(L)
    !************************************************************************
    !
    !       PUCX(L) =-CV(L)*PCX(L)*((N'-N)/N')+(N/N')*PUCX(L-1)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
            PXTRAX(6,L)=-CURV*PXTRAX(5,L)* &
            (((ALENS(WWVN,L))- &
            (ALENS(WWVN,(L-1))))/ &
            (ALENS(WWVN,L)))+ &
            ((ALENS(WWVN,(L-1)))/ &
            (ALENS(WWVN,L)))*PXTRAX(6,(L-1))
          IF(GLANAM(L,2).EQ.'PERFECT      ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(3,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
          IF(GLANAM(L,2).EQ.'IDEAL        ') &
          PXTRAX(6,L)=(-(1.0D0/ALENS(121,L))*PXTRAX(5,L))+PXTRAX(6,L-1)
    !
    !       PICX(L)
    !       CHECK FOR Y-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
                    IF(ALENS(23,L).EQ.1.0D0) THEN
                    CURV=ALENS(24,L)
                    ELSE
            IF(ALENS(1,L).EQ.0.0D0.AND.ALENS(43,L).NE.0.0D0) THEN
                    CURV=ALENS(43,L)*2.0D0
                    ELSE
                    CURV=ALENS(1,L)
                    END IF
                    END IF
                    PXTRAX(7,L)=(CURV*PXTRAX(5,L))+PXTRAX(6,(L-1))
    !       PICX'(L)
                    PXTRAX(8,L)=((ALENS((WWVN),(L-1)))/ &
                    (ALENS((WWVN),L)))*PXTRAX(7,L)
    !
    !                       NOW HANDLE SOLVES ON SURFACE L
    !***************************************************************************
    !       DO THIS BY CALLING SLVRS
                           SLV1=L
                           SLV2=2
          IF(SOLVE(4,L).NE.0.0D0.OR. &
          SOLVE(2,L).NE.0.0D0) CALL SLVRS
    !       ALL SOLVES ON SURFACE L HANDLED
    !
    !
     8000                     CONTINUE
    !       PARAXIAL TRACE COMPLETED
                            END IF
    !    Populate data object
    !     Dump data to paraxial ray trace interface
    
          SF = INT(SYSTEM(20))
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
                            
function systemHasYZPlane() result(boolResult)
    logical :: boolResult
    INTEGER ITYPEP
    !
    COMMON/PTYPER/ITYPEP

    boolResult = .FALSE.

    ! 1 = YZ, 3 = XY + YZ
    IF(ITYPEP.EQ.1.OR.ITYPEP.EQ.3) boolResult = .TRUE.

end function

function systemHasXZPlane() result(boolResult)
    logical :: boolResult
    INTEGER ITYPEP
    !
    COMMON/PTYPER/ITYPEP

    boolResult = .FALSE.

    ! 2 = YZ, 3 = XY + YZ
    IF(ITYPEP.EQ.2.OR.ITYPEP.EQ.3) boolResult = .TRUE.

end function

function traNextSurf(lastSurf, surfIdx, lambdaIdx, useXZPlane, overridePos) result(nextSurf)
    use DATLEN, only: ALENS, GLANAM
    use mod_lens_data_manager
    use type_utils, only: real2str ! DEBUG
! This function will do paraxial ray trace to the next surface
! Inputs:
! lastSurf - array of four numbers for last surface
!  height, slope, incident slope, exit slope
!  surface index - 0 is object up to image surface
!   lambdaIdx - wavelength index (between 1 and 10)
!    useXZPlane - default is YZ plane.  If this is set to true, will 
!   determine curvatures using XZ data    
! Output:
!    nextSurf - same order as last surface but after tracing        
! Ideal lenses will be taken into account here
    !use global_widgets, only: sysConfig

    real(kind=real64), intent(in) :: lastSurf(4)
    integer, intent(in) :: surfIdx
    integer :: lambdaIdx
    logical, optional :: useXZPlane
    real(kind=real64), optional :: overridePos
    logical :: useXZ
    real(kind=real64)  :: nextSurf(4)

    real(kind=real64) :: curv, n, np

    if(present(useXZPlane)) then 
        useXZ = useXZPlane 
    else
        useXZ = .FALSE.
    end if

    if (present(overridePos)) then
        nextSurf(1) = overridePos
    else
        nextSurf(1) = lastSurf(1)+ldm%getSurfThi(surfIdx-1)*lastSurf(2)
    end if
    curv = ldm%getSurfCurv(surfIdx, useXZ)

    np  = ldm%getSurfIndex(surfIdx, lambdaIdx)
    n   = ldm%getSurfIndex(surfIdx-1, lambdaIdx)
    !PUY(L) =-CV(L)*PY(L)*((N'-N)/N')+(N/N')*PUY(L-1)
    nextSurf(2) = -curv*nextSurf(1)*((np-n)/np)+(n/np)*lastSurf(2)

    ! TODO:  figure out how I want to handle non-standard surfaces.
    ! FOr now just copy what is in original code
    !if (ldm%getSurfType .NE. 'Standard') then
    !    nextSurf(2) = ldm%
    IF(GLANAM(surfIdx,2).EQ.'PERFECT      ') &
    nextSurf(2)=(-(1.0D0/ALENS(3,surfIdx))*nextSurf(1))+lastSurf(2)
      IF(GLANAM(surfIdx,2).EQ.'IDEAL        ') &
    nextSurf(2)=(-(1.0D0/ALENS(121,surfIdx))*nextSurf(1))+lastSurf(2)  

    !PIY are incident slope 
    !PIY(L)=CV(1)*PY(L)+PUY(L-1)
    nextSurf(3)=curv*nextSurf(1)+lastSurf(2)


            !PIY' - exit slope
    !       PIY'(L)=(N/N')*PIY(L)
    nextSurf(4) = (n/np)*nextSurf(3)

end function


subroutine resolvePikup(L)
    use DATLEN, only: ALENS
    integer :: L

    integer :: COMI

    COMMON/PIKCOM/COMI

    COMI=L
    IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES

end subroutine

!Not sure how this should go to LDM so for now abstract here
subroutine resolveSolve(L)
    use DATLEN, only: SOLVE
    integer :: L

    INTEGER SLV1,SLV2
    !
    COMMON/CSLVRS/SLV1,SLV2    

    SLV1=L
    SLV2=1
    IF(SOLVE(6,L).NE.0.0D0.OR. &
    SOLVE(8,L).NE.0.0D0) CALL SLVRS

end subroutine

function setInitialParaxialRays(CON) result(initialRays)
    use DATLEN, only: ALENS, SYSTEM
    use DATMAI, only: OUTLYNE
    use mod_lens_data_manager
    use parax_calcs
    use type_utils, only: real2str

    real(kind=real64) :: CON ! Chief ray height at pos 1

    real(kind=real64) :: initialRays(1:8,0:1)
    real(kind=real64) :: marPos1, marAng0
    integer :: COMI
    COMMON/PIKCOM/COMI

    call curr_lens_data%update()
    
    ! To deal with setting this properly with
    ! finite conjugate sytems and support the use case of this
    ! sub to ray trace to compute first order parameters
    call computeMarginalRayPosition(marPos1, marAng0)

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

    ! TODO:  Just send this to PIKRES!!!
    COMI=0
    IF(ALENS(32,0).NE.0.0D0) CALL PIKRES
!
!       PY(0)=0,  ALWAYS
     initialRays(1,0)=0.0D0
!
!       PUY(0)=SAY/TH(0)
!
    IF(ldm%getSurfThi(0).EQ.0.0D0) THEN
        OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
        CALL SHOWIT(1)
        CALL MACFAL
        RETURN
    END IF
    initialRays(2,0) = marAng0

!       PIY(0) =PUY(0)
    initialRays(3,0)=initialRays(2,0)
!
!       PIY'(0)=PUY(0)
    initialRays(4,0)=initialRays(3,0)
!
!       PCY(0) =-SCY
    initialRays(5,0)=(SYSTEM(14))
    IF(SYSTEM(14).EQ.0.0D0) initialRays(5,0)=1.0D0
!
!       PUCY(0)=(SCY-ADJUSTMENT ON SURFACE 1)/TH(0)
!       CON IS CHIEF RAY POSITION ON SURFACE 1
!       ENTERED BY THE DESIGNER IF IT IS NOT TO BE ZERO
!
  ! if (ldm%getSurfaceThickness(0))
  IF(ldm%getSurfThi(0).EQ.0.0D0) THEN  
  !IF(ALENS(3,0).EQ.0.0D0) THEN
    OUTLYNE='OBJECT DISTANCE IS ZERO-PARAXIAL RAY TRACE HALTED'
    CALL SHOWIT(1)
    CALL MACFAL
    RETURN
   END IF
    initialRays(6,0)=-((SYSTEM(14))-CON)/ALENS(3,0)
  IF(SYSTEM(14).EQ.0.0D0) initialRays(6,0)= &
  -(1.0D0-CON)/ALENS(3,0)
!
!       PICY(0) AT OBJECT, PICY = PUCY
  initialRays(7,0)=initialRays(6,0)
!
!       PICY'(0) AT OBJECT PICY'=PICY
  initialRays(8,0)=initialRays(7,0)
!
!       THIS COMPLETES THE VALUES AT THE OBJECT SURFACE
!
    !JN:  If there is a mag solve set thickness here:
    
    IF(ldm%isThiSolveOnSurf(0)) THEN   
        !IF(SOLVE(6,L).NE.0.0D0) THEN
        SLV1 = 0
        CALL SLVRS
    END IF

 
  SLV1=1
  SLV2=1
  IF(ldm%isThiSolveOnSurf(1).OR.ldm%isYZCurvSolveOnSurf(1)) THEN          
    CALL SLVRS
   END IF
!       INITIAL VALUES AT SURFACE 1
!       CALL PIKRES FOR THE SURFACE 1
           COMI=1
    IF(ALENS(32,COMI).NE.0.0D0) CALL PIKRES
!

    initialRays(1:4,1) = traNextSurf(initialRays(1:4,0),1,INT(SYSTEM(11)))
    initialRays(5:8,1) = traNextSurf(initialRays(5:8,0),1,INT(SYSTEM(11)), overridePos=CON)
 


end function


end module