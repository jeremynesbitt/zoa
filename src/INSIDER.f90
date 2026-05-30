! SUB MISSREF.FOR

SUBROUTINE MISSREF(X,Y)
!
   use DATLEN
   use DATMAI
   use mod_surface, only: surf_clap_type, surf_clap_dim, surf_clap_tilt
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   EXTERNAL INSID1,INSID2
!
!       THIS CHECKS IF X AND Y IS INSIDE THE CLAP ON THE REFERENCE SURFACE
!
   INTEGER I,CAFLG,N,III,CACOCA
!
   real(real64) X,Y,Z,ANGLE,JK1,JK2,JK3,&
   &XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,&
   &X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,&
   &YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4,A15,A22
!
   LOGICAL INS,INSID1,INSID2
!
   LOGICAL NOCOBSPSF
   COMMON/PSFCOBS/NOCOBSPSF
!
   INTEGER CAERAS,COERAS
!
   COMMON/CACO/CAERAS,COERAS,LS
!
!     VARIABLES FOR SPOT TRACING
!
   LOGICAL TCLPRF,SPDTRA
!
   COMMON/SPRA1/SPDTRA
   I=NEWREF
   LS=0
!
!       I IS THE REF SURFACE NUMBER
!
!
   IF(surf_clap_type(I) == 0) THEN
!
!       NO CLAPS OR COBS, JUST RETURN
      RETURN
!       THERE ARE CLAPS AND COBS, PROCEED CHECKING
   END IF
!
   IF(surf_clap_type(I) /= 0) THEN
      CAFLG=surf_clap_type(I)
!       CLAP EXISTS
!******************************************************************
!
!       CAFLG=1 CIRCULAR CLAP, DOES IT STOP THE RAY
      IF(CAFLG.EQ.1) THEN
!
         LS=0.0D0
!       CIRCULAR CLAP EQUATION IS:
!
!       (X)**2 + (Y)**2 = R**2
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       GREATER THAN THE RIGHT SIDE, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
!       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE. REMEMBER.

         XR=X-surf_clap_dim(I, 4)-JK1
         YR=Y-surf_clap_dim(I, 3)-JK2
!
         LS=DSQRT((XR**2)+(YR**2))
!
         IF(DABS(surf_clap_dim(I, 1)).LE.DABS(surf_clap_dim(I, 2))) THEN
            RS=DSQRT(surf_clap_dim(I, 1)**2)+AIMTOL
         ELSE
            RS=DSQRT(surf_clap_dim(I, 2)**2)+AIMTOL
         END IF
         IF(REAL(LS).GT.REAL(RS)) THEN
            LS=10.0D0
         ELSE
            LS=0.0D0
         END IF
!
         IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
            REFMISS=.TRUE.
            RETURN
         END IF
      END IF
!       CAFLG NOT 1
   END IF
!
!
!       CAFLG=2 RECTANGULAR CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.2) THEN
!
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
!       COORDINATES ARE:
      X1=-surf_clap_dim(I, 2)-AIMTOL
      Y1=surf_clap_dim(I, 1)+AIMTOL
      X2=-surf_clap_dim(I, 2)-AIMTOL
      Y2=-surf_clap_dim(I, 1)-AIMTOL
      X3=surf_clap_dim(I, 2)+AIMTOL
      Y3=-surf_clap_dim(I, 1)-AIMTOL
      X4=surf_clap_dim(I, 2)+AIMTOL
      Y4=surf_clap_dim(I, 1)+AIMTOL
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE. REMEMBER.

      XRD=XRD-surf_clap_dim(I, 4)-JK1
      YRD=YRD-surf_clap_dim(I, 3)-JK2
!
!       IF A NON-ZERO CLAP TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE. REMEMBER. GAMMA ROTATION OF A CLAP OR
!       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION
!     (THE ROTATION IS ALWAYS ABOUT THE LOCAL Z-AXIS OF THE SURFACE VERTEX)

      A15=(surf_clap_tilt(I)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
!       IF YES, SET LS=10.0D0
!       IF NO SET LS=0.0D0
      XT(1)=X1
      YT(1)=Y1
      XT(2)=X2
      YT(2)=Y2
      XT(3)=X3
      YT(3)=Y3
      XT(4)=X4
      YT(4)=Y4
      NP=4
      X0=XR
      Y0=YR
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS=0.0D0
      ELSE
!       RAY BLOCKED
         LS=10.0D0
      END IF
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         REFMISS=.TRUE.
         RETURN
      END IF
!       CAFLG NOT 2
   END IF
!
!       CAFLG=3 ELLIPTICAL CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.3) THEN
!
      LS=0.0D0
!       ELLIPTICAL CLAP EQUATION IS:
!
!       (X)**2/A**2  + (Y)**2/B**2 = 1
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       GREATER THAN 1.0D0, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE. REMEMBER.

      XRD=XRD-surf_clap_dim(I, 4)-JK1
      YRD=YRD-surf_clap_dim(I, 3)-JK2
!
!       IF A NON-ZERO CLAP TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE. REMEMBER. GAMMA ROTATION OF A CLAP OR
!       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A15=surf_clap_tilt(I)*PII/180.0D0
      A15=(surf_clap_tilt(I)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
      LS=((XR**2)/(surf_clap_dim(I, 2)**2))+&
      &((YR**2)/(surf_clap_dim(I, 1)**2))
!
      IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
!       RAY BLOCKED
         LS=10.0D0
      ELSE
!       NOT BLOCKED
         LS=0.0D0
      END IF
!
      IF(LS.EQ.10.0D0) THEN
         REFMISS=.TRUE.
         RETURN
      END IF
!       CAFLG NOT 3
   END IF
!
!
!       CAFLG=4 RACETRACK CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.4) THEN
!
      LS=0.0D0
!
!       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
!       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
!       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
!       AND THE FOUR CIRCLES NEED TO BE CHECKED.
!
      IF(surf_clap_dim(I, 1).LE.surf_clap_dim(I, 2)) THEN
!       surf_clap_dim(I, 2) = MAXSID
         MAXSID=surf_clap_dim(I, 2)
      ELSE
         MAXSID=surf_clap_dim(I, 1)
      END IF
      IF(surf_clap_dim(I, 5).LT.MAXSID) THEN
!       SETUP THE 8 SIDED BOX
         N=8
         X1=-surf_clap_dim(I, 2)+surf_clap_dim(I, 5)-AIMTOL
         Y1=surf_clap_dim(I, 1)+AIMTOL
         X2=-surf_clap_dim(I, 2)-AIMTOL
         Y2=surf_clap_dim(I, 1)-surf_clap_dim(I, 5)+AIMTOL
         X3=-surf_clap_dim(I, 2)-AIMTOL
         Y3=-surf_clap_dim(I, 1)+surf_clap_dim(I, 5)-AIMTOL
         X4=-surf_clap_dim(I, 2)+surf_clap_dim(I, 5)-AIMTOL
         Y4=-surf_clap_dim(I, 1)-AIMTOL
         X5=surf_clap_dim(I, 2)-surf_clap_dim(I, 5)+AIMTOL
         Y5=-surf_clap_dim(I, 1)-AIMTOL
         X6=surf_clap_dim(I, 2)+AIMTOL
         Y6=-surf_clap_dim(I, 1)+surf_clap_dim(I, 5)-AIMTOL
         X7=surf_clap_dim(I, 2)+AIMTOL
         Y7=surf_clap_dim(I, 1)-surf_clap_dim(I, 5)+AIMTOL
         X8=surf_clap_dim(I, 2)-surf_clap_dim(I, 5)+AIMTOL
         Y8=surf_clap_dim(I, 1)+AIMTOL
!
!
      ELSE
!       SET UP THE FOUR SIDED BOX
         N=4
         X1=-surf_clap_dim(I, 2)-AIMTOL
         Y1=surf_clap_dim(I, 1)+AIMTOL
         X2=-surf_clap_dim(I, 2)-AIMTOL
         Y2=-surf_clap_dim(I, 1)-AIMTOL
         X3=surf_clap_dim(I, 2)+AIMTOL
         Y3=-surf_clap_dim(I, 1)-AIMTOL
         X4=surf_clap_dim(I, 2)+AIMTOL
         Y4=surf_clap_dim(I, 1)+AIMTOL
      END IF
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE. REMEMBER.

      XRD=XRD-surf_clap_dim(I, 4)-JK1
      YRD=YRD-surf_clap_dim(I, 3)-JK2
!
!       IF A NON-ZERO CLAP TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE. REMEMBER. GAMMA ROTATION OF A CLAP OR
!       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A15=surf_clap_tilt(I)*PII/180.0D0
      A15=(surf_clap_tilt(I)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
!       ARE THE POINTS XR AND YR ON THE POLYGON OR OUTSIDE
!       IF YES, SET LS=10.0D0
!       IF NO SET LS=0.0D0
      IF(N.EQ.4) THEN
         XT(1)=X1
         YT(1)=Y1
         XT(2)=X2
         YT(2)=Y2
         XT(3)=X3
         YT(3)=Y3
         XT(4)=X4
         YT(4)=Y4
      ELSE
         XT(1)=X1
         YT(1)=Y1
         XT(2)=X2
         YT(2)=Y2
         XT(3)=X3
         YT(3)=Y3
         XT(4)=X4
         YT(4)=Y4
         XT(5)=X5
         YT(5)=Y5
         XT(6)=X6
         YT(6)=Y6
         XT(7)=X7
         YT(7)=Y7
         XT(8)=X8
         YT(8)=Y8
      END IF
      NP=N
      X0=XR
      Y0=YR
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS=0.0D0
      ELSE
!       RAY BLOCKED
         LS=10.0D0
      END IF
! NOW IS THE POINT INSIDE ANY OF THE FOUR CIRCLES
!       CENTER OF THE FIRST CIRCLE IS AT
      XC1=-surf_clap_dim(I, 2)+surf_clap_dim(I, 5)
      YC1= surf_clap_dim(I, 1)-surf_clap_dim(I, 5)
      CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
!       CENTER OF THE SECOND CIRCLE IS AT
      XC2= -surf_clap_dim(I, 2)+surf_clap_dim(I, 5)
      YC2= -surf_clap_dim(I, 1)+surf_clap_dim(I, 5)
      CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
!       CENTER OF THE THIRD CIRCLE IS AT
      XC3= surf_clap_dim(I, 2)-surf_clap_dim(I, 5)
      YC3=-surf_clap_dim(I, 1)+surf_clap_dim(I, 5)
      CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
!       CENTER OF THE FIRST CIRCLE IS AT
      XC4=surf_clap_dim(I, 2)-surf_clap_dim(I, 5)
      YC4=surf_clap_dim(I, 1)-surf_clap_dim(I, 5)
      CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
!
      RAD2=DSQRT(surf_clap_dim(I, 5)**2)+AIMTOL
!

      IF(.NOT.INS.AND.REAL(CS1).GT.REAL(RAD2)&
      &.AND.REAL(CS2).GT.REAL(RAD2).AND.&
      &REAL(CS3).GT.REAL(RAD2).AND.REAL(CS4).GT.REAL(RAD2)) THEN
!       RAY IS BLOCKED BY ONE OF THE CIRCLES AND THE BOX
         LS=10.0D0
      ELSE
!     RAY PASSES THROUGH A CIRCLE
         LS=0.0D0
      END IF
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         REFMISS=.TRUE.
         RETURN
      END IF
!       CAFLG NOT 4
   END IF
!
!       CAFLG=5 POLY CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.5) THEN
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_clap_dim(I, 2), CENTER TO CORNER DISTANCE
!       IS ALENS(10,II). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_clap_dim(I, 2))
         XT(III)=surf_clap_dim(I, 1)*DCOS(ANGLE+(PII/2.0D0))
         YT(III)=surf_clap_dim(I, 1)*DSIN(ANGLE+(PII/2.0D0))
         ANGLE=ANGLE+((TWOPII)/surf_clap_dim(I, 2))
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_clap_dim(I, 4)-JK1
      YRD=YRD-surf_clap_dim(I, 3)-JK2
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      A15=surf_clap_tilt(I)*PII/180.0D0
      A15=(surf_clap_tilt(I)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_clap_dim(I, 2))
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS=0.0D0
      ELSE
!       RAY BLOCKED
         LS=10.0D0
      END IF
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         REFMISS=.TRUE.
         RETURN
      END IF
!       CAFLG NOT 5
   END IF
!
!       CAFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.6) THEN
      LS=0.0D0
!
!       POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_clap_dim(I, 2))
         XT(III)=IPOLYX(III,I,1)
         YT(III)=IPOLYY(III,I,1)
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_clap_dim(I, 4)-JK1
      YRD=YRD-surf_clap_dim(I, 3)-JK2
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      A15=surf_clap_tilt(I)*PII/180.0D0
      A15=(surf_clap_tilt(I)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_clap_dim(I, 2))
      INS=INSID1()
      IF(INS) THEN
!       RAY NOT BLOCKED
         LS=0.0D0
      ELSE
!       RAY BLOCKED
         LS=10.0D0
      END IF
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         REFMISS=.TRUE.
         RETURN
      END IF
!       CAFLG NOT 6
   END IF
!
   RETURN
END
