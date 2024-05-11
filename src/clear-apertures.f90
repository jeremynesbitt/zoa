module clear_apertures

    contains

! This is based on SETCLAP, but does not update the ALENS array
! It is to serve as a band-aid until I have time to better abstract
! lens data management
! I want to be able to accurately report to users what the 
! clear apertures are, but not break existing KDP functionality,
! Which assumes that a stored value of 0 means it is automatically calculated
! So the purpose here is to calculate the clear aperture and if ALENS(10,i) is
! 0 then I will report the values calculated here to the user.                            

    SUBROUTINE check_clear_apertures(lData)
        !use global_widgets, only: curr_lens_data
        use kdp_data_types, only: lens_data
        use type_utils ! DEBUG
!
        IMPLICIT NONE
        class(lens_data), intent(inout) :: lData

!
!       THIS IS SUBROUTINE SETCLAP
!
        INTEGER FWARN,RWARN,J,I,K,L,M,N,KK,ALLOERR
!
      LOGICAL OLDLDIF,OLDLDIF2
!
      REAL*8 HY,HX,YF,XF,YR,XR,HXMAX,HXMIN,HYMAX,HYMIN &
      ,XFFIX,YFFIX,XRFIX,YRFIX,XCENPOS,YCENPOS,RMAX,XLO,XHI,YLO,YHI
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'

      REAL*8 VERARRAY,XRAD,YRAD,RRAD
      DIMENSION VERARRAY(:,:)
      ALLOCATABLE :: VERARRAY
      DEALLOCATE(VERARRAY,STAT=ALLOERR)
      ALLOCATE(VERARRAY(1:220,0:MAXSUR),STAT=ALLOERR)

 10   FORMAT('          CLEAR APERTURE ASSIGNED TO SURFACE ',I3)
 15   FORMAT('CLEAR APERTURE DIMENSIONS CHANGED AT SURFACE ',I3)

      ! Here we want to do all surfaces, 
      W1 = 1.0
      W2 = SYSTEM(20)


!       HANDLE NO SURFACES
                IF(SYSTEM(20).EQ.0.0) THEN
        WRITE(OUTLYNE,*)'LENS SYSTEM HAS NO SURFACES'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'NO PARAXIAL DATA EXISTS'
      CALL SHOWIT(1)
                CALL MACFAL
      DEALLOCATE(VERARRAY,STAT=ALLOERR)
                RETURN
                END IF

        IF(SQ.EQ.0) THEN
                   SQ=1
                   WQ='REAL'
                   END IF

      IF(WQ.EQ.'REAL'.OR.WQ.EQ.'VREAL') THEN
      call LogTermFOR("Real SETCLAP Loop")
      RWARN=0
      FWARN=0      

!     EIGHT PLACES AROUND THE FULL FOV
                       KK=0
                       DO K=0,8
      IF(ALENS(9,NEWOBJ).NE.2.0D0.AND.ALENS(9,NEWOBJ).NE.4.0D0 &
      .AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(K.EQ.0) THEN
      YF=0.0D0
      XF=1.0D0
      END IF
      IF(K.EQ.1) THEN
      YF=DSQRT(2.0D0)/2.0D0
      XF=DSQRT(2.0D0)/2.0D0
      END IF
      IF(K.EQ.2) THEN
      YF=1.0D0
      XF=0.0D0
      END IF
      IF(K.EQ.3) THEN
      YF=DSQRT(2.0D0)/2.0D0
      XF=-DSQRT(2.0D0)/2.0D0
      END IF
      IF(K.EQ.4) THEN
      YF=0.0D0
      XF=-1.0D0
      END IF
      IF(K.EQ.5) THEN
      YF=-DSQRT(2.0D0)/2.0D0
      XF=-DSQRT(2.0D0)/2.0D0
      END IF
      IF(K.EQ.6) THEN
      YF=-1.0D0
      XF=0.0D0
      END IF
      IF(K.EQ.7) THEN
      YF=-DSQRT(2.0D0)/2.0D0
      XF=DSQRT(2.0D0)/2.0D0
      END IF
      IF(K.EQ.8) THEN
      YF=0.0D0
      XF=0.0D0
      END IF
                           ELSE
      IF(ALENS(9,NEWOBJ).EQ.2.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      IF(K.EQ.0) THEN
      YF=0.0D0
      XF=1.0D0
               END IF
      IF(K.EQ.1) THEN
      YF=1.0D0
      XF=1.0D0
               END IF
      IF(K.EQ.2) THEN
      YF=1.0D0
      XF=0.0D0
               END IF
      IF(K.EQ.3) THEN
      YF=1.0D0
      XF=-1.0D0
               END IF
      IF(K.EQ.4) THEN
      YF=0.0D0
      XF=-1.0D0
               END IF
      IF(K.EQ.5) THEN
      YF=-1.0D0
      XF=-1.0D0
               END IF
      IF(K.EQ.6) THEN
      YF=-1.0D0
      XF=0.0D0
               END IF
      IF(K.EQ.7) THEN
      YF=-1.0D0
      XF=1.0D0
               END IF
      IF(K.EQ.8) THEN
      YF=0.0D0
      XF=0.0D0
      END IF
               END IF
      IF(ALENS(9,NEWOBJ).EQ.4.0D0.AND.ALENS(127,NEWOBJ).EQ.0.0D0) THEN
      YFFIX=DABS(ALENS(14,NEWOBJ)/ALENS(10,NEWOBJ))
      XFFIX=DABS(ALENS(14,NEWOBJ)/ALENS(11,NEWOBJ))
      IF(K.EQ.0) THEN
      YF=0.0D0
      XF=1.0D0
               END IF
      IF(K.EQ.1) THEN
      YF=1.0D0-(0.02928D0*YFFIX)
      XF=1.0D0-(0.02928D0*XFFIX)
               END IF
      IF(K.EQ.2) THEN
      YF=1.0D0
      XF=0.0D0
               END IF
      IF(K.EQ.3) THEN
      YF=1.0D0-(0.02928D0*YFFIX)
      XF=-1.0D0+(0.02928D0*XFFIX)
               END IF
      IF(K.EQ.4) THEN
      YF=0.0D0
      XF=-1.0D0
               END IF
      IF(K.EQ.5) THEN
      YF=-1.0D0+(0.02928D0*YFFIX)
      XF=-1.0D0+(0.02928D0*XFFIX)
               END IF
      IF(K.EQ.6) THEN
      YF=-1.0D0
      XF=0.0D0
               END IF
      IF(K.EQ.7) THEN
      YF=-1.0D0+(0.02928D0*YFFIX)
      XF=1.0D0-(0.02928D0*XFFIX)
               END IF
      IF(K.EQ.8) THEN
      YF=0.0D0
      XF=0.0D0
      END IF
               END IF
                           END IF
!     EIGHT PLACES AROUND THE APERTURE
!     DO THE FOB
      SAVE_KDP(1)=SAVEINPT(1)
      WC='FOB     '
      WQ='        '
      SQ=0
      SST=0
      STI=0
      DF1=0
      DF2=0
      DF3=0
      DF4=0
      DF5=1
      S1=1
      S2=1
      S3=1
      S4=1
      S5=0
      SN=1
      W1=YF
      W2=XF
      W3=0.0D0
      W4=SYSTEM(11)
!     SET MSG TO FALSE
        MSG=.FALSE.
        CALL FFOB
      IF(.NOT.REFEXT) FWARN=1
      REST_KDP(1)=RESTINPT(1)
!
      IF(WQ.NE.'VREAL') THEN
                       XLO=-1.0D0
                       XHI=1.0D0
                       YLO=-1.0D0
                       YHI=1.0D0
                       END IF
      IF(WQ.EQ.'VREAL') THEN
      CALL VIGCAL(N,XLO,XHI,1)
      CALL VIGCAL(N,YLO,YHI,2)
        MSG=.FALSE.
                       END IF
                       DO L=0,7
                       KK=KK+1
                       M=(3*KK)-2
      IF(ALENS(9,NEWREF).NE.2.0D0.AND.ALENS(9,NEWREF).NE.4.0D0 &
      .AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
      IF(L.EQ.0) THEN
      YR=0.0D0
      XR=XHI
      END IF
      IF(L.EQ.1) THEN
      YR=YHI*DSQRT(2.0D0)/2.0D0
      XR=XHI*DSQRT(2.0D0)/2.0D0
      END IF
      IF(L.EQ.2) THEN
      YR=YHI
      XR=0.0D0
      END IF
      IF(L.EQ.3) THEN
      YR=YHI*DSQRT(2.0D0)/2.0D0
      XR=XLO*DSQRT(2.0D0)/2.0D0
      END IF
      IF(L.EQ.4) THEN
      YR=0.0D0
      XR=XLO
      END IF
      IF(L.EQ.5) THEN
      YR=YLO*DSQRT(2.0D0)/2.0D0
      XR=XLO*DSQRT(2.0D0)/2.0D0
      END IF
      IF(L.EQ.6) THEN
      YR=YLO
      XR=0.0D0
      END IF
      IF(L.EQ.7) THEN
      YR=YLO*DSQRT(2.0D0)/2.0D0
      XR=XHI*DSQRT(2.0D0)/2.0D0
      END IF
                           ELSE
      IF(ALENS(9,NEWREF).EQ.2.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
      IF(L.EQ.0) THEN
      YR=0.0D0
      XR=XHI
               END IF
      IF(L.EQ.1) THEN
      YR=YHI
      XR=XHI
               END IF
      IF(L.EQ.2) THEN
      YR=YHI
      XR=0.0D0
               END IF
      IF(L.EQ.3) THEN
      YR=YHI
      XR=XLO
               END IF
      IF(L.EQ.4) THEN
      YR=0.0D0
      XR=XLO
               END IF
      IF(L.EQ.5) THEN
      YR=YLO
      XR=XLO
               END IF
      IF(L.EQ.6) THEN
      YR=YLO
      XR=0.0D0
               END IF
      IF(L.EQ.7) THEN
      YR=YLO
      XR=XHI
               END IF
               END IF
      IF(ALENS(9,NEWREF).EQ.4.0D0.AND.ALENS(127,NEWREF).EQ.0.0D0) THEN
      YRFIX=DABS(ALENS(14,NEWREF)/ALENS(10,NEWREF))
      XRFIX=DABS(ALENS(14,NEWREF)/ALENS(11,NEWREF))
      IF(L.EQ.0) THEN
      YR=0.0D0
      XR=XHI
               END IF
      IF(L.EQ.1) THEN
      YR=YHI-(0.02928D0*YRFIX*YHI)
      XR=XHI-(0.02928D0*XRFIX*XHI)
               END IF
      IF(L.EQ.2) THEN
      YR=YHI
      XR=0.0D0
               END IF
      IF(L.EQ.3) THEN
      YR=YHI-(0.02928D0*YRFIX*YHI)
      XR=XLO-(0.02928D0*XRFIX*XLO)
               END IF
      IF(L.EQ.4) THEN
      YR=0.0D0
      XR=-XLO
               END IF
      IF(L.EQ.5) THEN
      YR=YLO-(0.02928D0*YRFIX*YLO)
      XR=XLO-(0.02928D0*XRFIX*XLO)
               END IF
      IF(L.EQ.6) THEN
      YR=-YLO
      XR=0.0D0
               END IF
      IF(L.EQ.7) THEN
      YR=YLO-(0.02928D0*YFFIX*YLO)
      XR=XHI-(0.02928D0*XFFIX*XHI)
               END IF
               END IF
               END IF
!     TRACE THE RAY
      SAVE_KDP(1)=SAVEINPT(1)
        WQ='        '
        SQ=0
        SST=0
        STI=0
        DF1=0
        DF2=0
        DF3=0
        DF4=1
        DF5=1
        S1=1
        S2=1
        S3=1
        S4=0
        S5=0
        SN=1
        W1=YR
        W2=XR
        W3=SYSTEM(11)
        WC='RAY     '
        CALL RRAY
      IF(.NOT.RAYEXT) RWARN=1
        REST_KDP(1)=RESTINPT(1)
!     SAVE RAY DATA
                       DO I=0,INT(SYSTEM(20))
                IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
                   VERARRAY(M,I)=RAYRAY(1,I)
                   VERARRAY(M+1,I)=RAYRAY(2,I)
      VERARRAY(M+2,I)=DSQRT((RAYRAY(1,I)**2)+(RAYRAY(2,I)**2))
                       END IF
                       END DO
                       END DO
                       END DO
      LDIF2=OLDLDIF2
      LDIF=OLDLDIF
!
!     PROCESS DATA
      DO I=0,INT(SYSTEM(20))
      IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
                IF(.NOT.DUMMMY(I).OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
      HXMAX=-1.0D10
      HYMAX=-1.0D10
      HXMIN=1.0D10
      HYMIN=1.0D10
      DO J=1,214,3
      IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
      IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
      END DO
      DO J=2,215,3
      IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
      IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
      END DO
                        END IF
                        END IF
               END DO
!
      DO I=0,INT(SYSTEM(20))
        call LogTermFOR("Surf "//trim(int2str(I))// &
          " ALENS(9,1 ) "//trim(real2str(ALENS(9,I))) // &
          " ALENS(127,I) "//trim(real2str(ALENS(127,I))) // &
          " W1 is "//trim(real2str(W1)) // &
          " W2 is "//trim(real2str(W2)))
        PRINT *, "DUMMY(I) is ", DUMMMY(I)
      IF(I.GE.INT(W1).AND.I.LE.INT(W2)) THEN
        ! Comment out this line because it ignores the stop surface
        ! But it may be bad that I am ignoring dummy surfaces
                IF(.NOT.DUMMMY(I)) THEN !.OR.DF1.EQ.0.AND.DF2.EQ.0) THEN
      RMAX=-1.0D10
      HXMAX=-1.0D10
      HYMAX=-1.0D10
      HXMIN=1.0D10
      HYMIN=1.0D10
      DO J=1,214,3
      IF(VERARRAY(J,I).GT.HXMAX) HXMAX=VERARRAY(J,I)
      IF(VERARRAY(J,I).LT.HXMIN) HXMIN=VERARRAY(J,I)
      XCENPOS=(HXMAX+HXMIN)/2.0D0
      END DO
      DO J=2,215,3
      IF(VERARRAY(J,I).GT.HYMAX) HYMAX=VERARRAY(J,I)
      IF(VERARRAY(J,I).LT.HYMIN) HYMIN=VERARRAY(J,I)
      YCENPOS=(HYMAX+HYMIN)/2.0D0
      END DO
      DO J=3,216,3
      IF(VERARRAY(J,I).GT.RMAX) RMAX=VERARRAY(J,I)
      END DO
      YRAD=DABS((HYMAX-HYMIN)/2.0D0)
      XRAD=DABS((HXMAX-HXMIN)/2.0D0)
      IF(ALENS(10,I).EQ.0) THEN
        lData%clearAps(I+1)%userDefined = .FALSE.
        lData%clearAps(I+1)%yRad = YRAD
        lData%clearAps(I+1)%xRad = XRAD
      ELSE
        IF(ALENS(10,I).NE.YRAD) THEN
        lData%clearAps(I+1)%userDefined = .TRUE.               
        lData%clearAps(I+1)%yRad = ALENS(10,I)
        lData%clearAps(I+1)%xRad = ALENS(11,I)        
        END IF
        IF(ALENS(10,I).EQ.YRAD) THEN
            lData%clearAps(I+1)%userDefined = .FALSE.               
            lData%clearAps(I+1)%yRad = YRAD
            lData%clearAps(I+1)%xRad = XRAD                  
        END IF
      END IF

      !call LogTermFOR("YRAD is "//trim(real2str(YRAD)))
      !call LogTermFOR("XRAD is "//trim(real2str(XRAD)))

      RRAD=YRAD

      
       ELSE
        lData%clearAps(I+1)%userDefined = .FALSE.               
        lData%clearAps(I+1)%yRad = ALENS(10,I)
        lData%clearAps(I+1)%xRad = ALENS(11,I)              
        !call LogTermFOR("YRAD is "//trim(real2str(ALENS(10,I))))
        !call LogTermFOR("XRAD is "//trim(real2str(ALENS(11,I))))
                        END IF
                    END IF
        call LogTermFOR("YRAD is "//trim(real2str( lData%clearAps(I+1)%yRad)))
        call LogTermFOR("XRAD is "//trim(real2str( lData%clearAps(I+1)%xRad)))                    
               END DO
      IF(FWARN.NE.0) WRITE(OUTLYNE,2004)
      IF(RWARN.NE.0) WRITE(OUTLYNE,2005)
      CALL SHOWIT(0)
      IF(FWARN.NE.0.OR.RWARN.NE.0) THEN
      WRITE(OUTLYNE,2006)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,2007)
      CALL SHOWIT(0)
                   END IF
 2004 FORMAT('WARNING: SOME CHIEF RAYS COULD NOT BE TRACED')
 2005 FORMAT('WARNING: SOME MARGINAL RAYS COULD NOT BE TRACED')
 2006 FORMAT('LIMIT RAY DATA MAY BE IN ERROR, CLAPS NOT ASSIGNED')
 2007 FORMAT('CHANGE OBJECT HT. OR REF. AP. HT. AND RE-RUN')
      DEALLOCATE(VERARRAY,STAT=ALLOERR)
                        RETURN
                        END IF
                        END


end module