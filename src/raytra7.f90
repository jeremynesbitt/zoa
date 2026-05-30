!
!       THIS IS THE SEVENTH FILE OF RAYTRACING ROUTINES

! SUB SAGCACO.FOR

SUBROUTINE SAGCACO(JK_I,JK_X,JK_Y,JK_SAG,ISITIN)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   EXTERNAL INSID1,INSID2
!
!       THIS IS SUBROUTINE SAGCACO.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE CLEAR APERTURE CHECKING AS CALLED BY SSAAGG.FOR
!
   INTEGER I,CAFLG,COFLG,N,JK_I,III
!
   real(real64) X,Y,Z,JK_X,JK_Y,JK_SAG,PRAD,PRAD1,PRAD2 ,XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,ANGLE,X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4,A15,A22
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
   LOGICAL TCLPRF,SPDTRA,ISITIN
!
   COMMON/SPRA1/SPDTRA
   ISITIN=.FALSE.
!
!       THE CALL TO THIS ROUTINE IS:
!       CALL CACHEK
   I=JK_I
   X=JK_X
   Y=JK_Y
   Z=JK_SAG
!
!       I IS THE CURRENT SURFACE NUMBER
!
!       SET FLAGS CAFLG AND COFLG
   IF(surf_clap_type(I).NE.0.0D0) THEN
!       CLAP EXISTS
      CAFLG=INT(ABS(surf_clap_type(I)))
   ELSE
      CAFLG=0
   END IF
   IF(surf_coat_type(I).NE.0.0D0) THEN
!       COBS EXISTS
      COFLG=INT(ABS(surf_coat_type(I)))
   ELSE
      COFLG=0
   END IF
!
!
!       NOW ALL THE CAFLG AND COFLG HAVE BEEN SET
!
!******************************************************************
!       CAFLG NON-ZERO, RESOLVE ALL CLAP BLOCKAGES NOW
!******************************************************************
!
   IF(CAFLG.EQ.1.OR.CAFLG.EQ.0) THEN
!
      LS=0.0D0
!       CIRCULAR CLAP EQUATION IS:
!
!       (X)**2 + (Y)**2 = R**2

      XR=X-surf_clap_dim(I, 4)
      YR=Y-surf_clap_dim(I, 3)
!
      LS=DSQRT((XR**2)+(YR**2))
!
      PRAD1=DABS(PXTRAY(1,I))+DABS(PXTRAY(5,I))
      PRAD2=DABS(PXTRAX(1,I))+DABS(PXTRAX(5,I))
      PRAD=PRAD1
      IF(PRAD2.GT.PRAD1) PRAD=PRAD2
!
      IF(CAFLG.EQ.0) THEN
         RS=DSQRT(PRAD**2)+AIMTOL
      END IF
      IF(CAFLG.EQ.1) THEN
         IF(DABS(surf_clap_dim(I, 1)).LE.DABS(surf_clap_dim(I, 2))) THEN
            RS=DSQRT(surf_clap_dim(I, 1)**2)+AIMTOL
         ELSE
            RS=DSQRT(surf_clap_dim(I, 2)**2)+AIMTOL
         END IF
      END IF
      IF(REAL(LS).GT.REAL(RS)) THEN
         JK_SAG=0.0D0
         ISITIN=.FALSE.
         RETURN
      ELSE
         ISITIN=.TRUE.
      END IF
!       CAFLG NOT 1
   END IF
!
!
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

      XRD=XRD-surf_clap_dim(I, 4)
      YRD=YRD-surf_clap_dim(I, 3)

      A15=surf_clap_tilt(I)*PII/180.0D0
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
         ISITIN=.TRUE.
      ELSE
         JK_SAG=0.0D0
         ISITIN=.FALSE.
         RETURN
      END IF
!
!       CAFLG NOT 2
   END IF
!
   IF(CAFLG.EQ.3) THEN
!
      LS=0.0D0
!       ELLIPTICAL CLAP EQUATION IS:
!
!       (X)**2/A**2  + (Y)**2/B**2 = 1
!
      XRD=X
      YRD=Y

      XRD=XRD-surf_clap_dim(I, 4)
      YRD=YRD-surf_clap_dim(I, 3)
!

      A15=surf_clap_tilt(I)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
      LS=((XR**2)/(surf_clap_dim(I, 2)**2))+((YR**2)/(surf_clap_dim(I, 1)**2))
!
      IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
         JK_SAG=0.0D0
         ISITIN=.FALSE.
         RETURN
      ELSE
         ISITIN=.TRUE.
      END IF
!       CAFLG NOT 3
   END IF
!
!
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

      XRD=XRD-surf_clap_dim(I, 4)
      YRD=YRD-surf_clap_dim(I, 3)
!

      A15=surf_clap_tilt(I)*PII/180.0D0
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
         ISITIN=.TRUE.
      ELSE
         JK_SAG=0.0D0
         ISITIN=.FALSE.
         RETURN
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

      IF(.NOT.INS.AND.REAL(CS1).GT.REAL(RAD2).AND.REAL(CS2).GT.REAL(RAD2).AND.REAL(CS3).GT.REAL(RAD2).AND.REAL(CS4).GT.REAL(RAD2)) THEN
         JK_SAG=0.0D0
         ISITIN=.FALSE.
         RETURN
      ELSE
         ISITIN=.TRUE.
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
!       IS surf_clap_dim(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
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
      XR=XR-surf_clap_dim(I, 4)
      YR=YR-surf_clap_dim(I, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      A15=surf_clap_tilt(I)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_clap_dim(I, 2))
      INS=INSID1()
      IF(INS) THEN
!       RAY BLOCKED
         ISITIN=.FALSE.
         JK_SAG=0.0D0
         RETURN
      ELSE
         ISITIN=.TRUE.
      END IF
   ELSE
!       CAFLG NOT 5
   END IF
!
!       CAFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.6) THEN
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_clap_dim(I, 2), CENTER TO CORNER DISTANCE
!       IS surf_clap_dim(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
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
      XR=XR-surf_clap_dim(I, 4)
      YR=YR-surf_clap_dim(I, 3)
!
!       IF A NON-ZERO CLAP ERASE TILT EXISTS, IT MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       CLEAR APERTURE ERASE. REMEMBER.

      A15=surf_clap_tilt(I)*PII/180.0D0
      XR=(XRD*DCOS(A15))+(YRD*DSIN(A15))
      YR=(YRD*DCOS(A15))-(XRD*DSIN(A15))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_clap_dim(I, 2))
      INS=INSID1()
      IF(INS) THEN
!       RAY BLOCKED
         ISITIN=.FALSE.
         JK_SAG=0.0D0
         RETURN
      ELSE
         ISITIN=.TRUE.
      END IF
   ELSE
!       CAFLG NOT 6
   END IF
!
!******************************************************************
!       COFLG NON-ZERO, RESOLVE ALL COBS BLOCKAGES NOW
!******************************************************************
!
   IF(COFLG.EQ.1) THEN
!
      LS=0.0D0
!       CIRCULAR COBS EQUATION IS:
!
!       (X)**2 + (Y)**2 = R**2
!
      XR=X-surf_cobs_poly(I, 4)
      YR=Y-surf_cobs_poly(I, 3)
!
      LS=DSQRT((XR**2)+(YR**2))
!
      RS=DSQRT(surf_cobs_poly(I, 1)**2)-AIMTOL
      IF(REAL(LS).LT.REAL(RS)) THEN
         JK_SAG=0.0D0
         RETURN
      ELSE
      END IF
!       COFLG NOT 1
   END IF
!
   IF(COFLG.EQ.2) THEN
!
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
!       COORDINATES ARE:
      X1=-surf_cobs_poly(I, 2)-AIMTOL
      Y1=surf_cobs_poly(I, 1)+AIMTOL
      X2=-surf_cobs_poly(I, 2)-AIMTOL
      Y2=-surf_cobs_poly(I, 1)-AIMTOL
      X3=surf_cobs_poly(I, 2)+AIMTOL
      Y3=-surf_cobs_poly(I, 1)-AIMTOL
      X4=surf_cobs_poly(I, 2)+AIMTOL
      Y4=surf_cobs_poly(I, 1)+AIMTOL
      XRD=X
      YRD=Y
!
      XRD=XRD-surf_cobs_poly(I, 4)
      YRD=YRD-surf_cobs_poly(I, 3)
!

      A22=(surf_cobs_poly(I, 6))*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
      INS=INSID2()
      IF(INS) THEN
         JK_SAG=0.0D0
         RETURN
      ELSE
      END IF
!       COFLG NOT 2
   END IF
!
   IF(COFLG.EQ.3) THEN
!
      LS=0.0D0
!       ELLIPTICAL COBS EQUATION IS:
!
!       (X)**2/A**2  + (Y)**2/B**2 = 1
!
!
      XRD=X
      YRD=Y
!
      XRD=XRD-surf_cobs_poly(I, 4)
      YRD=YRD-surf_cobs_poly(I, 3)
!

      A22=(surf_cobs_poly(I, 6))*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
      LS=((XR**2)/(surf_cobs_poly(I, 2)**2))+((YR**2)/(surf_cobs_poly(I, 1)**2))
!
      IF(REAL(LS).LT.(1.0-(AIMTOL**2))) THEN
         JK_SAG=0.0D0
         RETURN
      ELSE
      END IF
!       COFLG NOT 3
   END IF
!
!
   IF(COFLG.EQ.4) THEN
!
      LS=0.0D0
!
!       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
!       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
!       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
!       AND THE FOUR CIRCLES NEED TO BE CHECKED.
!
      IF(surf_cobs_poly(I, 1).LE.surf_cobs_poly(I, 2)) THEN
!       surf_cobs_poly(I, 2) = MAXSID
         MAXSID=surf_cobs_poly(I, 2)
      ELSE
         MAXSID=surf_cobs_poly(I, 1)
      END IF
      IF(surf_cobs_poly(I, 5).LT.MAXSID) THEN
!       SETUP THE 8 SIDED BOX
         N=8
         X1=-surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)+AIMTOL
         Y1=surf_cobs_poly(I, 1)-AIMTOL
         X2=-surf_cobs_poly(I, 2)+AIMTOL
         Y2=surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)-AIMTOL
         X3=-surf_cobs_poly(I, 2)+AIMTOL
         Y3=-surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)+AIMTOL
         X4=-surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)+AIMTOL
         Y4=-surf_cobs_poly(I, 1)+AIMTOL
         X5=surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)-AIMTOL
         Y5=-surf_cobs_poly(I, 1)+AIMTOL
         X6=surf_cobs_poly(I, 2)-AIMTOL
         Y6=-surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)+AIMTOL
         X7=surf_cobs_poly(I, 2)-AIMTOL
         Y7=surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)-AIMTOL
         X8=surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)-AIMTOL
         Y8=surf_cobs_poly(I, 1)-AIMTOL
      ELSE
!       SET UP THE FOUR SIDED BOX
         N=4
         X1=-surf_cobs_poly(I, 2)-AIMTOL
         Y1=surf_cobs_poly(I, 1)+AIMTOL
         X2=-surf_cobs_poly(I, 2)-AIMTOL
         Y2=-surf_cobs_poly(I, 1)-AIMTOL
         X3=surf_cobs_poly(I, 2)+AIMTOL
         Y3=-surf_cobs_poly(I, 1)-AIMTOL
         X4=surf_cobs_poly(I, 2)+AIMTOL
         Y4=surf_cobs_poly(I, 1)+AIMTOL
      END IF
!
      XRD=X
      YRD=Y
!
      XRD=XRD-surf_cobs_poly(I, 4)
      YRD=YRD-surf_cobs_poly(I, 3)
!

      A22=(surf_cobs_poly(I, 6))*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
      INS=INSID2()
      IF(INS) THEN
         JK_SAG=0.0D0
         RETURN
      ELSE
      END IF
! NOW IS THE POINT OUTSIDE OR ON ANY OF THE FOUR CIRCLES
!       CENTER OF THE FIRST CIRCLE IS AT
      XC1=-surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)
      YC1= surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)
      CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
!       CENTER OF THE SECOND CIRCLE IS AT
      XC2= -surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)
      YC2= -surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)
      CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
!       CENTER OF THE THIRD CIRCLE IS AT
      XC3= surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)
      YC3=-surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)
      CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
!       CENTER OF THE FIRST CIRCLE IS AT
      XC4=surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)
      YC4=surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)
      CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
!
      RAD2=DSQRT(surf_cobs_poly(I, 5)**2)-AIMTOL
!
      IF(INS.OR.REAL(CS1).LT.REAL(RAD2).OR.REAL(CS2).LT.REAL(RAD2).OR.REAL(CS3).LT.REAL(RAD2).OR.REAL(CS4).LT.REAL(RAD2))THEN
         JK_SAG=0.0D0
         RETURN
      ELSE
      END IF
!       COFLG NOT 4
   END IF
!
!       COFLG=5 POLY CLAP, DOES IT STOP THE RAY
   IF(COFLG.EQ.5) THEN
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_poly(I, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_poly(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_cobs_poly(I, 2))
         XT(III)=surf_cobs_poly(I, 1)*DCOS(ANGLE+(PII/2.0D0))
         YT(III)=surf_cobs_poly(I, 1)*DSIN(ANGLE+(PII/2.0D0))
         ANGLE=ANGLE+((TWOPII)/surf_cobs_poly(I, 2))
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XR=XR-surf_cobs_poly(I, 4)
      YR=YR-surf_cobs_poly(I, 3)
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

      A22=(surf_cobs_poly(I, 6))*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_poly(I, 2))
      INS=INSID2()
      IF(INS) THEN
!       RAY BLOCKED
         JK_SAG=0.0D0
         RETURN
      ELSE
         RETURN
!       RAY NOT BLOCKED
      END IF
!
   ELSE
!       COFLG NOT 5
   END IF
!
!       COFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
   IF(COFLG.EQ.6) THEN
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_poly(I, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_poly(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      DO III=1,INT(surf_cobs_poly(I, 2))
         XT(III)=IPOLYX(III,I,3)
         YT(III)=IPOLYY(III,I,3)
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XR=XR-surf_cobs_poly(I, 4)
      YR=YR-surf_cobs_poly(I, 3)
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

      A22=(surf_cobs_poly(I, 6))*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_poly(I, 2))
      INS=INSID2()
      IF(INS) THEN
!       RAY BLOCKED
         JK_SAG=0.0D0
         RETURN
      ELSE
         RETURN
!       RAY NOT BLOCKED
      END IF
!
   ELSE
!       COFLG NOT 6
   END IF
   RETURN
END
! SUB FDISTOP.FOR
SUBROUTINE FDISTOP(IW1,ERROR)
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_scy_fang_set, sys_scx_fang_set, sys_mode, &
      & sys_set_scy_fang_set, sys_set_scx_fang_set
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE FDISTOP. THIS SUBROUTINE IMPLEMENTS
!       THE FISHEYE DIST IN OPTIMIZATION
!
!          IW1=FIELD NUMBER
!
   real(real64) PCY,PCX,PUCY,PUCX,V1,VAL1,VAL2,SIG ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP,HPU,HRU,FACTOR,FACTOR2 ,MYW1,MYW2,MYW3,MYW4
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,ERROR,IW1
!
   real(real64) VALUE,HUP,EEFEL
   real(real64) O18,O19
   COMMON/O18O19/O18,O19
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   GRASET=.FALSE.
   O18=sys_scy_fang_set()
   O19=sys_scx_fang_set()
   call sys_set_scy_fang_set(0.0D0)
   call sys_set_scx_fang_set(0.0D0)
!
   ERROR=0
   K=NEWIMG
   MYW1=FIELDY(IW1)
   MYW2=FIELDX(IW1)
   MYW3=FIELDZ(IW1)
   MYW4=0.0D0
   CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR ,0.0D0,0.0D0,0.0D0,MYW4,0)

   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
!     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
!     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
!     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
   SAVE_KDP(15)=SAVEINPT(15)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.FALSE.
   LDIF=.FALSE.
   WQ='        '
   SQ=0
   SST=0
   STI=0
   W1=MYW1
   W2=MYW2
   W3=MYW3
   W4=MYW4
   W5=0.0D0
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
!     SET MSG TO FALSE
   MSG=.FALSE.
   ERROR=0
   WC='FOB     '
   CALL FFOB
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(1)=RESTINPT(1)
   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
   IF(sys_mode().LE.2.0D0) THEN
!     MODE FOCAL
!     PARAX IMAGE POSITION IS JUST
      HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
      HPU=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
!     REAL IMAGE POSITION IS JUST
      XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
      YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
!     REAL ANGLE IS JUST
      XREAL=GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG)
      YREAL=GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG)
      HRU=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HRU.NE.0.0D0) FACTOR=(HR/DTAN(HRU))*HRU
      IF(HRU.EQ.0.0D0) FACTOR=0.0D0
      IF(HPU.NE.0.0D0) FACTOR2=(HP/HPU)*DATAN(HPU)
      IF(HPU.EQ.0.0D0) FACTOR2=0.0D0
      IF(FACTOR2.NE.0.0D0) THEN
         VALUE=((FACTOR-FACTOR2)/FACTOR2)*100.0D0
      END IF
      IF(FACTOR2.EQ.0.0D0) VALUE=0.0D0
   END IF
   IF(sys_mode().GE.3.0D0) THEN
!     PROCEED WITH FISHDIST CALC.
!     PARAX IMAGE SLOPE IS JUST
      HP=DATAN(DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2)))
!     REAL IMAGE SLOPE IS JUST
      XREAL=(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
      YREAL=(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
      IF(DABS(XREAL).GT.PII) XREAL=DABS((TWOPII)-DABS(XREAL))
      IF(DABS(YREAL).GT.PII) YREAL=DABS((TWOPII)-DABS(YREAL))
      IF(DABS(XREAL).GE.(PII/2.0D0))XREAL=DABS((DABS(XREAL))-PII)
      IF(DABS(YREAL).GE.(PII/2.0D0))YREAL=DABS((DABS(YREAL))-PII)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HP.NE.0.0D0) VALUE=((HR-HP)/HP)*100.0D0
      IF(HP.EQ.0.0D0) VALUE=0.0D0
   END IF
   call sys_set_scy_fang_set(O18)
   call sys_set_scx_fang_set(O19)
   RETURN
END
! SUB FDISTOR.FOR
SUBROUTINE FDISTOR(DWORD1,DWORD2,ERROR)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_wl_ref, sys_scy_fang_set, sys_scx_fang_set, sys_mode, &
      & sys_set_scy_fang_set, sys_set_scx_fang_set
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DISTOR. THIS SUBROUTINE IMPLEMENTS
!       THE GET DIST COMMAND
!
!     THE GET DIST COMMAND RESTURNS IN VALUE, THE VALUE OF THE
!     DISTORTION IN PERCENT AT THE FOB POSITION GIVEN BY:
!          YFOB=DWORD1
!          XFOB=DWORD2
!
   real(real64) PCY,PCX,PUCY,PUCX,V1,VAL1,VAL2,SIG ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP,DWORD1,DWORD2,HUP,EEFEL ,MYW1,MYW2,MYW3,MYW4,HPU,HRU,FACTOR,FACTOR2
   real(real64) O18,O19
   COMMON/O18O19/O18,O19
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,ERROR
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   O18=sys_scy_fang_set()
   O19=sys_scx_fang_set()
   call sys_set_scy_fang_set(0.0D0)
   call sys_set_scx_fang_set(0.0D0)
!
   ERROR=0
   K=NEWIMG
!
   IF(DWORD1.EQ.0.0D0) DWORD1=1.0D-10
   IF(DWORD2.EQ.0.0D0) DWORD2=1.0D-10
   MYW1=DWORD1
   MYW2=DWORD2
   MYW3=0.0D0
   MYW4=sys_wl_ref()
   CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR ,0.0D0,0.0D0,0.0D0,MYW4,0)

   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
!     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
!     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
!     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
   SAVE_KDP(15)=SAVEINPT(15)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.FALSE.
   LDIF=.FALSE.
   WQ='        '
   SQ=0
   SST=0
   STI=0
   W1=MYW1
   W2=MYW2
   W3=MYW3
   W4=MYW4
   W5=0.0D0
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
!     SET MSG TO FALSE
   MSG=.FALSE.
   ERROR=0
   WC='FOB     '
   CALL FFOB
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(1)=RESTINPT(1)
   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
   IF(sys_mode().LE.2.0D0) THEN
!     MODE FOCAL
!     PARAX IMAGE POSITION IS JUST
      HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
      HPU=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
!     REAL IMAGE POSITION IS JUST
      XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
      YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      XREAL=GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG)
      YREAL=GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG)
      HRU=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HRU.NE.0.0D0) FACTOR=(HR/DTAN(HRU))*HRU
      IF(HRU.EQ.0.0D0) FACTOR=0.0D0
      IF(HPU.NE.0.0D0) FACTOR2=(HP/HPU)*DATAN(HPU)
      IF(HPU.EQ.0.0D0) FACTOR2=0.0D0
      IF(FACTOR2.NE.0.0D0) THEN
         VALUE=((FACTOR-FACTOR2)/FACTOR2)*100.0D0
      END IF
      IF(FACTOR2.EQ.0.0D0) VALUE=0.0D0
   END IF
   IF(sys_mode().GE.3.0D0) THEN
!     PROCEED WITH FISHDIST CALC.
!     PARAX IMAGE SLOPE IS JUST
      HP=DATAN(DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2)))
!     REAL IMAGE SLOPE IS JUST
      XREAL=(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
      YREAL=(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
      IF(DABS(XREAL).GT.PII) XREAL=DABS((TWOPII)-DABS(XREAL))
      IF(DABS(YREAL).GT.PII) YREAL=DABS((TWOPII)-DABS(YREAL))
      IF(DABS(XREAL).GE.(PII/2.0D0))XREAL=DABS((DABS(XREAL))-PII)
      IF(DABS(YREAL).GE.(PII/2.0D0))YREAL=DABS((DABS(YREAL))-PII)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HP.NE.0.0D0) VALUE=((HR-HP)/HP)*100.0D0
      IF(HP.EQ.0.0D0) VALUE=0.0D0
   END IF
   call sys_set_scy_fang_set(O18)
   call sys_set_scx_fang_set(O19)
   RETURN
END
! SUB DISTOP.FOR
SUBROUTINE DISTOP(IW1,ERROR)
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_scy_fang_set, sys_scx_fang_set, sys_mode, &
      & sys_set_scy_fang_set, sys_set_scx_fang_set
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DISTOP. THIS SUBROUTINE IMPLEMENTS
!       THE DIST IN OPTIMIZATION
!
!          IW1=FIELD NUMBER
!
   real(real64) PCY,PCX,PUCY,PUCX,V1,VAL1,VAL2,SIG ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP ,MYW1,MYW2,MYW3,MYW4
   real(real64) O18,O19
   COMMON/O18O19/O18,O19
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,ERROR,IW1
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   GRASET=.FALSE.
   O18=sys_scy_fang_set()
   O19=sys_scx_fang_set()
   call sys_set_scy_fang_set(0.0D0)
   call sys_set_scx_fang_set(0.0D0)
!
   ERROR=0
   K=NEWIMG
   MYW1=FIELDY(IW1)
   MYW2=FIELDX(IW1)
   MYW3=FIELDZ(IW1)
   MYW4=0.0D0
   CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR ,0.0D0,0.0D0,0.0D0,MYW4,0)

   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
!     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
!     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
!     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
   SAVE_KDP(15)=SAVEINPT(15)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.FALSE.
   LDIF=.FALSE.
   WQ='        '
   SQ=0
   SST=0
   STI=0
   W1=MYW1
   W2=MYW2
   W3=MYW3
   W4=MYW4
   W5=0.0D0
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
!     SET MSG TO FALSE
   MSG=.FALSE.
   ERROR=0
   WC='FOB     '
   CALL FFOB
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(1)=RESTINPT(1)
   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
!     PROCEED WITH DIST CALC.
   IF(sys_mode().LE.2.0D0) THEN
!     MODE FOCAL
!     PARAX IMAGE POSITION IS JUST
      HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
!     REAL IMAGE POSITION IS JUST
      XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
      YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HP.NE.0.0D0) VALUE=((HR-HP)/HP)*100.0D0
      IF(HP.EQ.0.0D0) VALUE=0.0D0
   END IF
   IF(sys_mode().GE.3.0D0) THEN
!     MODE AFOCAL
!     PARAX IMAGE SLOPE IS JUST
      HP=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
!     REAL IMAGE SLOPE IS JUST
      XREAL=DTAN(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
      YREAL=DTAN(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HP.NE.0.0D0) VALUE=((HR-HP)/HP)*100.0D0
      IF(HP.EQ.0.0D0) VALUE=0.0D0
   END IF
   call sys_set_scy_fang_set(O18)
   call sys_set_scx_fang_set(O19)
   RETURN
END
! SUB GNPR1.FOR
SUBROUTINE GNPR1(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PY
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR,GFLAG
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PY
   VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
   VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE=VALUE*SYS12
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR2.FOR
SUBROUTINE GNPR2(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PX
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5,GFLAG
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PX
   VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
   VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE=VALUE*SYS13
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR3.FOR
SUBROUTINE GNPR3(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PUY
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,GFLAG
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PUY
   IF(DABS(DIFF(11,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(11,K),DIFF(12,K))
      END IF
!        IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(12,K))
!      IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELY)*SYS12
   IF(DABS(DIFF(10,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(10,K),DIFF(12,K))
      END IF
!       IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(11,K))
!     IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELY)*SYS12
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   VALUE=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR4.FOR
SUBROUTINE GNPR4(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PUX
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5,GFLAG
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PUX
   IF(DABS(DIFF(4,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(4,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELX)*SYS13
   IF(DABS(DIFF(5,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(5,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELX)*SYS13
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   VALUE=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!     ALL THE PARAXIAL STUFF IS DONE
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR5.FOR
SUBROUTINE GNPR5(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PCY
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,GFLAG
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PCY
   VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
   VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE=VALUE*YHT
!
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR6.FOR
SUBROUTINE GNPR6(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PCX
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,GFLAG
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PCX
   VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
   VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE=VALUE*XHT
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR7.FOR
SUBROUTINE GNPR7(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PUCY
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,GFLAG
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
   WQ='        '
   SQ=0
   SST=0
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PUCY
   IF(DABS(RFDIFF(11,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(12,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELY)*YHT
   IF(DABS(RFDIFF(10,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(11,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELY)*YHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR8.FOR
SUBROUTINE GNPR8(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PUCX
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   INTEGER ERROR
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,GFLAG
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
   SN=1
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PUCX
   IF(DABS(RFDIFF(4,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELX)*XHT
   IF(DABS(RFDIFF(5,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELX)*XHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPRT.FOR
SUBROUTINE GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR,MYW1,MYW2,MYW3,MYW4,GFLAG)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,PX,PY,PUX,PUY,PCX,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,PCY,PUCX,PUCY,MYW1,MYW2,MYW3,MYW4
!
   INTEGER ERROR,GFLAG
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K
!
   PY=0.0D0
   PX=0.0D0
   PCY=0.0D0
   PCX=0.0D0
   PUY=0.0D0
   PUX=0.0D0
   PUCY=0.0D0
   PUCX=0.0D0
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=MYW1
   W2=MYW2
   W3=MYW3
   W4=MYW4
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   W3=MYW4
   SST=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PY
   VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
   VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PY=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PY=PY*SYS12
!
!     PX
   VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
   VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PX=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PX=PX*SYS13
!
!     PUY
   IF(DABS(DIFF(11,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(11,K),DIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(12,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELY)*SYS12
   IF(DABS(DIFF(10,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(10,K),DIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(11,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELY)*SYS12
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PUX
   IF(DABS(DIFF(4,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(4,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELX)*SYS13
   IF(DABS(DIFF(5,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(5,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELX)*SYS13
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PCY
   VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
   VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PCY=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PCY=PCY*YHT
!
!     PCX
   VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
   VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PCX=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PCX=PCX*XHT
!
!     PUCY
   IF(DABS(RFDIFF(11,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(12,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELY)*YHT
   IF(DABS(RFDIFF(10,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(11,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELY)*YHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUCY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PUCX
   IF(DABS(RFDIFF(4,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELX)*XHT
   IF(DABS(RFDIFF(5,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELX)*XHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUCX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))

!     ALL THE PARAXIAL STUFF IS DONE
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPRTGEN.FOR
SUBROUTINE GNPRTGEN(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR ,DWORD1,DWORD2,DWORD3,DWORD4,GFLAG)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,PX,PY,PUX,PUY,PCX,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,PCY,PUCX,PUCY,DWORD1,DWORD2,DWORD3,DWORD4
!
!     DWORD1 IS Y FOB
!     DWORD2 IS X FOB
!     DWORD3 IS Z OBJ. SHIFT
!     DWORD4 IS WAVELENGTH #
!
   INTEGER ERROR,GFLAG
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K
!
!
   ERROR=0
   PX=0.0D0
   PY=0.0D0
   PUX=0.0D0
   PUY=0.0D0
   PCX=0.0D0
   PCY=0.0D0
   PUCX=0.0D0
   PUCY=0.0D0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
   WQ='        '
   SQ=0
   SST=0
   STI=0
   W1=DWORD1
   W2=DWORD2
   W3=DWORD3
   W4=DWORD4
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
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   W3=DWORD4
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PY
   VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
   VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PY=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PY=PY*SYS12
!
!     PX
   VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
   VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PX=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PX=PX*SYS13
!
!     PUY
   IF(DABS(DIFF(11,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(11,K),DIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(12,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELY)*SYS12
   IF(DABS(DIFF(10,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(10,K),DIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(11,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELY)*SYS12
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PUX
   IF(DABS(DIFF(4,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(4,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELX)*SYS13
   IF(DABS(DIFF(5,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(5,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELX)*SYS13
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PCY
   VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
   VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PCY=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PCY=PCY*YHT
!
!     PCX
   VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
   VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PCX=SIG*DSQRT((VAL1**2)+(VAL2**2))
   PCX=PCX*XHT
!
!     PUCY
   IF(DABS(RFDIFF(11,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(12,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELY)*YHT
   IF(DABS(RFDIFF(10,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(11,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELY)*YHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUCY=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PUCX
   IF(DABS(RFDIFF(4,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELX)*XHT
   IF(DABS(RFDIFF(5,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELX)*XHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   PUCX=SIG*(DSQRT((VAL1**2)+(VAL2**2)))

!     ALL THE PARAXIAL STUFF IS DONE
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB GNPR12345678.FOR
SUBROUTINE GNPR12345678(K,NWN1,NWN2,NWN3,NWN4,ERROR,GFLAG)
!
!     PY
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_telecentric, sys_wrx, sys_wry, sys_bdx, sys_bdy
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   real(real64) V1,VAL1,VAL2,SIG,XHT,YHT ,SYS12,SYS13,NWN1,NWN2,NWN3,NWN4
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
   real(real64) VALUE_AUXFOB(1:8)
   COMMON/FOBAUX/VALUE_AUXFOB
!
   INTEGER ERROR,GFLAG
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K
!
!
   ERROR=0
!
   IF(GFLAG.EQ.1) THEN
!     GAUSSIAN CALC
!     SET XHT,YHT,SYS12,SYS13 AND RES SURF TO 1
!
      IF(NEWREF.NE.1) THEN
         OLDREF=NEWREF
         NEWREF=1
      END IF
      XHT=sys_bdx()*0.001D0*surf_thickness(0)
      YHT=sys_bdy()*0.001D0*surf_thickness(0)
      SYS13=sys_wrx()
      SYS12=sys_wry()
   END IF
   IF(GFLAG.EQ.0) THEN
!     NON-GAUSSIAN CALC
!       THE REF OBJECT HEIGHTS ARE:
!       (AT THE OBJECT SURFACE)
      XHT=PXTRAX(5,NEWOBJ)
      YHT=PXTRAY(5,NEWOBJ)
!     SET REF AP HT
!
      IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
         SYS13=PXTRAX(1,NEWREF)
         SYS12=PXTRAY(1,NEWREF)
      ELSE
!     TEL ON
         SYS13=PXTRAX(1,NEWOBJ+1)
         SYS12=PXTRAY(1,NEWOBJ+1)
      END IF
   END IF
!
!     FIRST TRACE THE FOB 0 RAY AND CALCULATE PYPY AND PXPX
!     IF FOCAL AND PYAPY AND PXAPX IF AFOCAL
!     THESE ARE THE EQUIVALENTS OF PCY AND PCX OR PUCY AND PUCX
!     AT THE IMAGE.
!     DO AN FOB 0
   SAVE_KDP(13)=SAVEINPT(13)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.TRUE.
   LDIF=.TRUE.
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
   W1=NWN1
   W2=NWN2
   W3=NWN3
   W4=NWN4
!     SET MSG TO FALSE
   MSG=.FALSE.
   WC='FOB     '
   CALL FFOB
   REST_KDP(13)=RESTINPT(13)
   SAVE_KDP(13)=SAVEINPT(13)
   WQ='        '
   SQ=0
   SST=0
   STI=0
   DF1=1
   DF2=1
   DF3=0
   DF4=1
   DF5=1
   S1=0
   S2=0
   S3=1
   S4=0
   S5=0
   SN=1
   W3=NWN4
   WC='RAY     '
   NOCOAT=.TRUE.
   GRASET=.FALSE.
   DXFSET=.FALSE.
   CALL RRAY
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(13)=RESTINPT(13)
!
   IF(.NOT.REFEXT.OR..NOT.RAYEXT) THEN
      ERROR=1
      RETURN
   END IF
!
!     PY
   VAL1=(DIFF(8,K)-RAYRAY(2,K))/DELY
   VAL2=(DIFF(7,K)-RAYRAY(1,K))/DELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE_AUXFOB(1)=VALUE*SYS12
!
!     PX
   VAL1=(DIFF(1,K)-RAYRAY(1,K))/DELX
   VAL2=(DIFF(2,K)-RAYRAY(2,K))/DELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE_AUXFOB(2)=VALUE*SYS13
!
!     PUY
   IF(DABS(DIFF(11,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(11,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(11,K),DIFF(12,K))
      END IF
!        IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(12,K))
!      IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELY)*SYS12
   IF(DABS(DIFF(10,K)).GE.(1.0D35*DABS(DIFF(12,K)))) THEN
      IF(SNGL(DIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(10,K)).LE.1.0D-15.AND.DABS(DIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(10,K),DIFF(12,K))
      END IF
!       IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(11,K))
!     IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELY)*SYS12
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE_AUXFOB(3)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PUX
   IF(DABS(DIFF(4,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(4,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(4,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-RAYRAY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/DELX)*SYS13
   IF(DABS(DIFF(5,K)).GE.(1.0D35*DABS(DIFF(6,K)))) THEN
      IF(SNGL(DIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(DIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(DIFF(5,K)).LE.1.0D-15.AND.DABS(DIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(DIFF(5,K),DIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-RAYRAY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/DELX)*SYS13
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE_AUXFOB(4)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PCY
   VAL1=(RFDIFF(8,K)-REFRY(2,K))/RFDELY
   VAL2=(RFDIFF(7,K)-REFRY(1,K))/RFDELY
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE_AUXFOB(5)=VALUE*YHT
!
!     PCX
   VAL1=(RFDIFF(1,K)-REFRY(1,INT(K)))/RFDELX
   VAL2=(RFDIFF(2,K)-REFRY(2,K))/RFDELX
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE=SIG*DSQRT((VAL1**2)+(VAL2**2))
   VALUE_AUXFOB(6)=VALUE*XHT
!
!     PUCY
   IF(DABS(RFDIFF(11,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(11,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(11,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(11,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(11,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(12,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELY)*YHT
   IF(DABS(RFDIFF(10,K)).GE.(1.0D35*DABS(RFDIFF(12,K)))) THEN
      IF(SNGL(RFDIFF(10,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(10,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(10,K)).LE.1.0D-15.AND.DABS(RFDIFF(12,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(10,K),RFDIFF(12,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(11,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELY)*YHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE_AUXFOB(7)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
!
!     PUCX
   IF(DABS(RFDIFF(4,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(4,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(4,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(4,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(4,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL1=(V1-REFRY(11,K))
   IF(SNGL(VAL1).GT.RPII) VAL1=VAL1-(TWOPII)
   IF(SNGL(VAL1).EQ.RTWOPII) VAL1=0.0D0
   VAL1=(VAL1/RFDELX)*XHT
   IF(DABS(RFDIFF(5,K)).GE.(1.0D35*DABS(RFDIFF(6,K)))) THEN
      IF(SNGL(RFDIFF(5,K)).GE.0.0) V1=PII/2.0D0
      IF(SNGL(RFDIFF(5,K)).LT.0.0) V1=(3.0D0*PII)/2.0D0
   ELSE
      IF(DABS(RFDIFF(5,K)).LE.1.0D-15.AND.DABS(RFDIFF(6,K)).LE.1.0D-15) THEN
         V1=0.0D0
      ELSE
         V1=DATAN2(RFDIFF(5,K),RFDIFF(6,K))
      END IF
      IF(SNGL(V1).LT.0.0) V1=V1+(TWOPII)
   END IF
   VAL2=(V1-REFRY(12,K))
   IF(SNGL(VAL2).GT.RPII) VAL2=VAL2-(TWOPII)
   IF(SNGL(VAL2).EQ.RTWOPII) VAL2=0.0D0
   VAL2=(VAL2/RFDELX)*XHT
   IF(VAL1.NE.0.0D0) SIG=VAL1/DABS(VAL1)
   IF(VAL1.EQ.0.0D0) SIG=1.0D0
   IF(ISNAN(VAL1))VAL1=0.0D0
   IF(ISNAN(VAL2))VAL2=0.0D0
   VALUE_AUXFOB(8)=SIG*(DSQRT((VAL1**2)+(VAL2**2)))
   IF(GFLAG.EQ.1) NEWREF=OLDREF
   RETURN
END
! SUB DISTOR.FOR
SUBROUTINE DISTOR(DWORD1,DWORD2,ERROR)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_wl_ref, sys_scy_fang_set, sys_scx_fang_set, sys_mode, &
      & sys_set_scy_fang_set, sys_set_scx_fang_set
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE DISTOR. THIS SUBROUTINE IMPLEMENTS
!       THE GET DIST COMMAND
!
!     THE GET DIST COMMAND RESTURNS IN VALUE, THE VALUE OF THE
!     DISTORTION IN PERCENT AT THE FOB POSITION GIVEN BY:
!          YFOB=DWORD1
!          XFOB=DWORD2
!
   real(real64) PCY,PCX,PUCY,PUCX,V1,VAL1,VAL2,SIG ,XREAL,YREAL,PY,PX,PUY,PUX,HR,HP,DWORD1,DWORD2 ,MYW1,MYW2,MYW3,MYW4,HPX,HPY,HRX,HRY,VALUEX,VALUEY
   real(real64) O18,O19
   COMMON/O18O19/O18,O19
!
   LOGICAL OLDLDIF,OLDLDIF2
!
   INTEGER K,ERROR
!
   real(real64) VALUE
   INTEGER NUM5
   COMMON/GV/VALUE,NUM5
!
   O18=sys_scy_fang_set()
   O19=sys_scx_fang_set()
   call sys_set_scy_fang_set(0.0D0)
   call sys_set_scx_fang_set(0.0D0)
!
   ERROR=0
   K=NEWIMG
!
   IF(DWORD1.EQ.0.0D0) DWORD1=1.0D-10
   IF(DWORD2.EQ.0.0D0) DWORD2=1.0D-10
   MYW1=DWORD1
   MYW2=DWORD2
   MYW3=0.0D0
   MYW4=sys_wl_ref()
   CALL GNPRT(K,PY,PX,PUY,PUX,PCY,PCX,PUCY,PUCX,ERROR ,0.0D0,0.0D0,0.0D0,MYW4,0)

   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
!     PROCEED WITH DISTORTION CALCULATION USING PCY AND PCX
!     TRACE THE DESIRED GUT RAY WITHOUT DIFFERENTIAL TRACING
!     AND GET ITS X AND Y POSITIONS AT THE IMAGE PLANE.
   SAVE_KDP(15)=SAVEINPT(15)
   OLDLDIF2=LDIF2
   OLDLDIF=LDIF
   LDIF2=.FALSE.
   LDIF=.FALSE.
   WQ='        '
   SQ=0
   SST=0
   STI=0
   W1=MYW1
   W2=MYW2
   W3=MYW3
   W4=MYW4
   W5=0.0D0
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
!     SET MSG TO FALSE
   MSG=.FALSE.
   ERROR=0
   SQ=0
   WQ='        '
   WC='FOB'
   CALL FFOB
   LDIF2=OLDLDIF2
   LDIF=OLDLDIF
   REST_KDP(1)=RESTINPT(1)
   IF(ERROR.EQ.1) THEN
      call sys_set_scy_fang_set(O18)
      call sys_set_scx_fang_set(O19)
      RETURN
   END IF
!     PROCEED WITH DIST CALC.
   IF(sys_mode().LE.2.0D0) THEN
!     MODE FOCAL
!     PARAX IMAGE POSITION IS JUST
      HPX=DSQRT((PCX*MYW2)**2)
      HPY=DSQRT((MYW1*PCY)**2)
      HP=DSQRT(((MYW1*PCY)**2)+((PCX*MYW2)**2))
!     REAL IMAGE POSITION IS JUST
      XREAL=GUTRAY(1,NEWIMG)-REFRY(1,NEWIMG)
      YREAL=GUTRAY(2,NEWIMG)-REFRY(2,NEWIMG)
      HRX=DSQRT((XREAL)**2)
      HRY=DSQRT((YREAL)**2)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HPX.EQ.0.0D0) VALUEX=0.0D0
      IF(HPX.NE.0.0D0) VALUEX=((HRX-HPX)/HPX)*100.0D0
      IF(HPY.EQ.0.0D0) VALUEY=0.0D0
      IF(HPY.NE.0.0D0) VALUEY=((HRY-HPY)/HPY)*100.0D0
      IF(HP.EQ.0.0D0) VALUE=0.0D0
      IF(HP.NE.0.0D0) VALUE=((HR-HP)/HP)*100.0D0
   END IF
   IF(sys_mode().GE.3.0D0) THEN
!     MODE AFOCAL
!     PARAX IMAGE SLOPE IS JUST
      HP=DSQRT(((MYW1*PUCY)**2)+((PUCX*MYW2)**2))
      HPX=((PUCX*MYW2)**2)
      HPY=((PUCX*MYW2)**2)
!     REAL IMAGE SLOPE IS JUST
      XREAL=DTAN(GUTRAY(11,NEWIMG)-REFRY(11,NEWIMG))
      YREAL=DTAN(GUTRAY(12,NEWIMG)-REFRY(12,NEWIMG))
      HRX=DSQRT((XREAL)**2)
      HRY=DSQRT((YREAL)**2)
      HR=DSQRT(((YREAL)**2)+((XREAL)**2))
      IF(HPX.NE.0.0D0) VALUEX=((HRX-HPX)/HPX)*100.0D0
      IF(HPX.EQ.0.0D0) VALUEX=0.0D0
      IF(HPY.NE.0.0D0) VALUEY=((HRY-HPY)/HPY)*100.0D0
      IF(HPY.EQ.0.0D0) VALUEY=0.0D0
      IF(HP.NE.0.0D0) VALUE=((HR-HP)/HP)*100.0D0
      IF(HP.EQ.0.0D0) VALUE=0.0D0
   END IF
   call sys_set_scy_fang_set(O18)
   call sys_set_scx_fang_set(O19)
   RETURN
END
! SUB CACHEK.FOR

SUBROUTINE CACHEK(JK1,JK2,JK3,CACOCA)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   EXTERNAL INSID1,INSID2
!
!       THIS IS SUBROUTINE CACHEK.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE CLEAR APERTURE CHECKING AS CALLED BY RAYTRA.FOR
!       AND REFRAY.FOR
!
   INTEGER I,CAFLG,COFLG,N,III,CACOCA
!
   real(real64) X,Y,Z,ANGLE,JK1,JK2,JK3,XR,YR,LS,RS,XRD,YRD,X1,X2,X3,X4,Y1,Y2,Y3,Y4,X5,X6,X7,X8,Y5,Y6,Y7,Y8,XC1,XC2,XC3,XC4,YC1,YC2,YC3,YC4,RAD2,MAXSID,CS1,CS2,CS3,CS4,A15,A22
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
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
!
   COMMON/SPRA1/SPDTRA
!
!       THE CALL TO THIS ROUTINE IS:
!       CALL CACHEK
   I=R_I
   X=R_X
   Y=R_Y
   Z=R_Z
   LS=0
   RAYCOD(1)=0
   RAYCOD(2)=I
!       ADDED TO SUPPORT VIIRS FOOTPRINTS ON 10/28/2004
   IF(surf_footblok_flag(I).EQ.1.0D0) THEN
!       FOOTBLOCK WAS ON FOR THIS SURFACE SO SKIP THE CLAP/COBS CHECK
      STOPP=0
      RETURN
   END IF
!
!       I IS THE CURRENT SURFACE NUMBER
!
!       THIS ROUTINE SETS THE FLAGS
!       CAERAS AND COERAS ARE
!       FLAGS SET IF THE NEXT CURRENT SURFACE HAS A COBS OR
!       CLAP ERASE.
!
   IF(surf_clap_type(I).EQ.0.0D0.AND.surf_coat_type(I).EQ.0.0D0) THEN
!
!       NO CLAPS OR COBS, JUST RETURN
      STOPP=0
      RETURN
!       THERE ARE CLAPS AND COBS, PROCEED CHECKING
   END IF
!
!       DO CAERAS,COERAS RESOLUTIONS
!
   IF(surf_cobs_ape_type(I).GT.0.0D0) THEN
!       CLAP ERASE EXISTS
      CAERAS=INT(ABS(surf_cobs_ape_type(I)))
   ELSE
      CAERAS=0
   END IF
   IF(surf_cobs_era_type(I).GT.0.0D0) THEN
!       COBS ERASE EXISTS
      COERAS=INT(ABS(surf_cobs_era_type(I)))
   ELSE
      COERAS=0
   END IF
!
!       NOW ALL THE CAERAS AND COERAS HAVE BEEN SET
!
!       SET FLAGS CAFLG AND COFLG
   IF(surf_clap_type(I).NE.0.0D0.AND.CACOCA.EQ.0.OR.surf_clap_type(I).NE.0.0D0.AND.CACOCA.EQ.1) THEN
!       CLAP EXISTS
      CAFLG=INT(ABS(surf_clap_type(I)))
   ELSE
      CAFLG=0
   END IF
   IF(surf_coat_type(I).NE.0.0D0.AND.CACOCA.EQ.0.OR.surf_coat_type(I).NE.0.0D0.AND.CACOCA.EQ.2) THEN
!       COBS EXISTS
      COFLG=INT(ABS(surf_coat_type(I)))
   ELSE
      COFLG=0
   END IF
!
   IF(NOCOBSPSF) COFLG=0

!
!       NOW ALL THE CAFLG AND COFLG HAVE BEEN SET
!
!******************************************************************
!       CAFLG NON-ZERO, RESOLVE ALL CLAP BLOCKAGES NOW
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
!       RESOLVE CLAP ERASE
      IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)CALL CAERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         RAYCOD(1)=6
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
         IF(MSG) THEN
            IF(surf_clap_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY CIRCULAR CLEAR APERTURE'
               CALL SHOWIT(1)
            END IF
         END IF
         STOPP=1
         RETURN
!       RAY NOT BLOCKED BY CIRCULAR CLAP, CONTINUE
      ELSE
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
!       RESOLVE CLAP ERASE
      IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)CALL CAERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
!
         RAYCOD(1)=6
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
         IF(MSG) THEN
            IF(surf_clap_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY RECTANGULAR CLEAR APERTURE'
               CALL SHOWIT(1)
            END IF
         END IF
!
         STOPP=1
         RETURN
!       RAY NOT BLOCKED BY RECTANGULAR CLAP, CONTINUE
      ELSE
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
      LS=((XR**2)/(surf_clap_dim(I, 2)**2))+((YR**2)/(surf_clap_dim(I, 1)**2))
!
      IF(REAL(LS).GT.(1.0+(AIMTOL**2))) THEN
!       RAY BLOCKED
         LS=10.0D0
      ELSE
!       NOT BLOCKED
         LS=0.0D0
      END IF
!
!       RESOLVE CLAP ERASE
      IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)CALL CAERRS
!
      IF(LS.EQ.10.0D0) THEN
         RAYCOD(1)=6
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!       RAY IS BLOCKED BY CLAP ON SURFACE I
!
         IF(MSG) THEN
            IF(surf_clap_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY ELLIPTICAL CLEAR APERTURE'
               CALL SHOWIT(1)
            END IF
         END IF
!
         STOPP=1
         RETURN
!       RAY NOT BLOCKED BY ELLIPTICAL CLAP, CONTINUE
      ELSE
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

      IF(.NOT.INS.AND.REAL(CS1).GT.REAL(RAD2).AND.REAL(CS2).GT.REAL(RAD2).AND.REAL(CS3).GT.REAL(RAD2).AND.REAL(CS4).GT.REAL(RAD2)) THEN
!       RAY IS BLOCKED BY ONE OF THE CIRCLES AND THE BOX
         LS=10.0D0
      ELSE
!     RAY PASSES THROUGH A CIRCLE
         LS=0.0D0
      END IF
!
!       RESOLVE CLAP ERASE
      IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)CALL CAERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         RAYCOD(1)=6
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_clap_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY RACETRACK CLEAR APERTURE'
               CALL SHOWIT(1)
            END IF
         END IF
         LS=0.0D0
         STOPP=1
         RETURN
      ELSE
!       RAY NOT BLOCKED BY RACETRACK CLAP, CONTINUE
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
!       IS surf_clap_dim(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
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
!       RESOLVE CLAP ERASE
      IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)CALL CAERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         RAYCOD(1)=6
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_clap_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY POLYGON CLEAR APERTURE'
               CALL SHOWIT(1)
            END IF
         END IF
         LS=0.0D0
         STOPP=1
         RETURN
      ELSE
      END IF
!       CAFLG NOT 5
   END IF
!
!       CAFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
   IF(CAFLG.EQ.6) THEN
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_clap_dim(I, 2), CENTER TO CORNER DISTANCE
!       IS surf_clap_dim(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
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
!       RESOLVE CLAP ERASE
      IF(CAERAS.NE.0.AND.LS.EQ.10.0D0)CALL CAERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY CLAP ON SURFACE I
         RAYCOD(1)=6
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_clap_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY IRREGULAR POLYGON CLEAR APERTURE'
               CALL SHOWIT(1)
            END IF
         END IF
         LS=0.0D0
         STOPP=1
         RETURN
      ELSE
      END IF
!       CAFLG NOT 6
   END IF
!
!******************************************************************
!       COFLG NON-ZERO, RESOLVE ALL COBS BLOCKAGES NOW
!******************************************************************
!
!       COFLG=1 CIRCULAR COBS, DOES IT STOP THE RAY
   IF(COFLG.EQ.1) THEN
!
      LS=0.0D0
!       CIRCULAR COBS EQUATION IS:
!
!       (X)**2 + (Y)**2 = R**2
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       LESS THAN OR EQUAL TO THE RIGHT SIDE, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
!       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE DECENTERED
!       OBSCURATION. REMEMBER.
!
      XR=X-surf_cobs_poly(I, 4)-JK1
      YR=Y-surf_cobs_poly(I, 3)-JK2
!
      LS=DSQRT((XR**2)+(YR**2))
!
      RS=DSQRT(surf_cobs_poly(I, 1)**2)-AIMTOL
      IF(REAL(LS).LT.REAL(RS)) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         LS=10.0D0
      ELSE
         LS=0.0D0
      END IF
!
!       RESOLVE COBS ERASE
      IF(COERAS.NE.0.AND.LS.EQ.10.0D0)CALL COERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         RAYCOD(1)=7
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_coat_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY CIRCULAR OBSCURATION'
               CALL SHOWIT(1)
            END IF
         END IF
         STOPP=1
         RETURN
      ELSE
!       RAY NOT BLOCKED BY CIRCULAR OBSCURATION, CONTINUE
      END IF
!       COFLG NOT 1
   END IF
!
!       COFLG=2 RECTANGULAR OBSCURATION, DOES IT STOP THE RAY
   IF(COFLG.EQ.2) THEN
!
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE RECTANGLE, THE CORNER
!       COORDINATES ARE:
      X1=-surf_cobs_poly(I, 2)-AIMTOL
      Y1=surf_cobs_poly(I, 1)+AIMTOL
      X2=-surf_cobs_poly(I, 2)-AIMTOL
      Y2=-surf_cobs_poly(I, 1)-AIMTOL
      X3=surf_cobs_poly(I, 2)+AIMTOL
      Y3=-surf_cobs_poly(I, 1)-AIMTOL
      X4=surf_cobs_poly(I, 2)+AIMTOL
      Y4=surf_cobs_poly(I, 1)+AIMTOL
      XRD=X
      YRD=Y
!
!
!       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       OBSCURATION. REMEMBER.
!
      XRD=XRD-surf_cobs_poly(I, 4)-JK1
      YRD=YRD-surf_cobs_poly(I, 3)-JK2
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
!       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A22=(surf_cobs_poly(I, 6)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
      INS=INSID2()
      IF(INS) THEN
!       RAY  BLOCKED
         LS=10.0D0
      ELSE
         LS=0.0D0
!       RAY NOT BLOCKED
      END IF
!
!       RESOLVE COBS ERASE
      IF(COERAS.NE.0.AND.LS.EQ.10.0D0)CALL COERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         RAYCOD(1)=7
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_coat_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY RECTANGULAR OBSCURATION'
               CALL SHOWIT(1)
            END IF
         END IF
!
         STOPP=1
         RETURN
      ELSE
!       RAY NOT BLOCKED BY RECTANGULAR OBSCURATION, CONTINUE
      END IF
!       COFLG NOT 2
   END IF
!
!       COFLG=3 ELLIPTICAL COBS, DOES IT STOP THE RAY
   IF(COFLG.EQ.3) THEN
!
      LS=0.0D0
!       ELLIPTICAL COBS EQUATION IS:
!
!       (X)**2/A**2  + (Y)**2/B**2 = 1
!
!       SUB X AND Y IN THE LEFT SIDE. IF THE LEFT SIDE IS
!       LESS THAN 1.0D0, THE RAY IS BLOCKED
!       ELSE IT IS NOT BLOCKED.
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       OBSCURATION. REMEMBER.
!
      XRD=XRD-surf_cobs_poly(I, 4)-JK1
      YRD=YRD-surf_cobs_poly(I, 3)-JK2
!
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
!       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A22=(surf_cobs_poly(I, 6)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
      LS=((XR**2)/(surf_cobs_poly(I, 2)**2))+((YR**2)/(surf_cobs_poly(I, 1)**2))
!
      IF(REAL(LS).LT.(1.0-(AIMTOL**2))) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         LS=10.0D0
      ELSE
         LS=0.0D0
      END IF
!
!       RESOLVE COBS ERASE
      IF(COERAS.NE.0.AND.LS.EQ.10.0D0)CALL COERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         RAYCOD(1)=7
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_coat_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY ELLIPTICAL OBSCURATION'
               CALL SHOWIT(1)
            END IF
         END IF
!
         STOPP=1
         RETURN
      ELSE
!       RAY NOT BLOCKED BY ELLIPTICAL COBS, CONTINUE
      END IF
!       COFLG NOT 3
   END IF
!
!
!       COFLG=4 RACETRACK COBS, DOES IT STOP THE RAY
   IF(COFLG.EQ.4) THEN
!
      LS=0.0D0
!
!       IF THE RADIUS OF THE CORNERS IS LESS THAN THE LARGER
!       OF THE TWO SIDE DIMENSIONS, AN 8 SIDED POLYGON NEEDS TO BE
!       CHECKED AS WELL AS THE FOUR CIRCLES. IF NOT, THE BASE RECTANGE
!       AND THE FOUR CIRCLES NEED TO BE CHECKED.
!
      IF(surf_cobs_poly(I, 1).LE.surf_cobs_poly(I, 2)) THEN
!       surf_cobs_poly(I, 2) = MAXSID
         MAXSID=surf_cobs_poly(I, 2)
      ELSE
         MAXSID=surf_cobs_poly(I, 1)
      END IF
      IF(surf_cobs_poly(I, 5).LT.MAXSID) THEN
!       SETUP THE 8 SIDED BOX
         N=8
         X1=-surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)+AIMTOL
         Y1=surf_cobs_poly(I, 1)-AIMTOL
         X2=-surf_cobs_poly(I, 2)+AIMTOL
         Y2=surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)-AIMTOL
         X3=-surf_cobs_poly(I, 2)+AIMTOL
         Y3=-surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)+AIMTOL
         X4=-surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)+AIMTOL
         Y4=-surf_cobs_poly(I, 1)+AIMTOL
         X5=surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)-AIMTOL
         Y5=-surf_cobs_poly(I, 1)+AIMTOL
         X6=surf_cobs_poly(I, 2)-AIMTOL
         Y6=-surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)+AIMTOL
         X7=surf_cobs_poly(I, 2)-AIMTOL
         Y7=surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)-AIMTOL
         X8=surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)-AIMTOL
         Y8=surf_cobs_poly(I, 1)-AIMTOL
      ELSE
!       SET UP THE FOUR SIDED BOX
         N=4
         X1=-surf_cobs_poly(I, 2)-AIMTOL
         Y1=surf_cobs_poly(I, 1)+AIMTOL
         X2=-surf_cobs_poly(I, 2)-AIMTOL
         Y2=-surf_cobs_poly(I, 1)-AIMTOL
         X3=surf_cobs_poly(I, 2)+AIMTOL
         Y3=-surf_cobs_poly(I, 1)-AIMTOL
         X4=surf_cobs_poly(I, 2)+AIMTOL
         Y4=surf_cobs_poly(I, 1)+AIMTOL
      END IF
!
      XRD=X
      YRD=Y
!
!       IF NON-ZERO COBS DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       OBSCURATION. REMEMBER.
!
      XRD=XRD-surf_cobs_poly(I, 4)-JK1
      YRD=YRD-surf_cobs_poly(I, 3)-JK2
!
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED
!       OBSCURATION. REMEMBER. GAMMA ROTATION OF A CLAP OR
!       COBS HAS THE SAME SIGN AS IN A SURFACE ROTATION

      A22=(surf_cobs_poly(I, 6)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
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
      INS=INSID2()
      IF(INS) THEN
!       RAY BLOCKED
         LS=10.0D0
      ELSE
         LS=0.0D0
!       RAY NOT BLOCKED
      END IF
! NOW IS THE POINT OUTSIDE OR ON ANY OF THE FOUR CIRCLES
!       CENTER OF THE FIRST CIRCLE IS AT
      XC1=-surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)
      YC1= surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)
      CS1=DSQRT(((XR-XC1)**2)+((YR-YC1)**2))
!       CENTER OF THE SECOND CIRCLE IS AT
      XC2= -surf_cobs_poly(I, 2)+surf_cobs_poly(I, 5)
      YC2= -surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)
      CS2=DSQRT(((XR-XC2)**2)+((YR-YC2)**2))
!       CENTER OF THE THIRD CIRCLE IS AT
      XC3= surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)
      YC3=-surf_cobs_poly(I, 1)+surf_cobs_poly(I, 5)
      CS3=DSQRT(((XR-XC3)**2)+((YR-YC3)**2))
!       CENTER OF THE FIRST CIRCLE IS AT
      XC4=surf_cobs_poly(I, 2)-surf_cobs_poly(I, 5)
      YC4=surf_cobs_poly(I, 1)-surf_cobs_poly(I, 5)
      CS4=DSQRT(((XR-XC4)**2)+((YR-YC4)**2))
!
      RAD2=DSQRT(surf_cobs_poly(I, 5)**2)-AIMTOL
!
!     RAY BLOCKED BY THE BOX AND CIRCLES
      IF(INS.OR.REAL(CS1).LT.REAL(RAD2).OR.REAL(CS2).LT.REAL(RAD2).OR.REAL(CS3).LT.REAL(RAD2).OR.REAL(CS4).LT.REAL(RAD2))THEN
!       RAY IS BLOCKED BY ONE OF THE CIRCLES
         LS=10.0D0
      ELSE
         LS=0.0D0
      END IF
!
!       RESOLVE COBS ERASE
      IF(COERAS.NE.0.AND.LS.EQ.10.0D0)CALL COERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         RAYCOD(1)=7
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_coat_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY RACETRACK OBSCURATION'
               CALL SHOWIT(1)
            END IF
         END IF
         LS=0.0D0
!
         STOPP=1
         RETURN
      ELSE
!       RAY NOT BLOCKED BY RACETRACK COBS, CONTINUE
      END IF
!       COFLG NOT 4
   END IF
!
!       COFLG=5 POLY CLAP, DOES IT STOP THE RAY
   IF(COFLG.EQ.5) THEN
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_poly(I, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_poly(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      ANGLE=0.0D0
      DO III=1,INT(surf_cobs_poly(I, 2))
         XT(III)=surf_cobs_poly(I, 1)*DCOS(ANGLE+(PII/2.0D0))
         YT(III)=surf_cobs_poly(I, 1)*DSIN(ANGLE+(PII/2.0D0))
         ANGLE=ANGLE+((TWOPII)/surf_cobs_poly(I, 2))
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_poly(I, 4)-JK1
      YRD=YRD-surf_cobs_poly(I, 3)-JK2
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

      A22=(surf_cobs_poly(I, 6)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_poly(I, 2))
      INS=INSID2()
      IF(INS) THEN
!       RAY BLOCKED
         LS=10.0D0
      ELSE
!       RAY NOT BLOCKED
         LS=0.0D0
      END IF
!
!       RESOLVE COBS ERASE
      IF(COERAS.NE.0.AND.LS.EQ.10.0D0)CALL COERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY COBS ON SURFACE I
         RAYCOD(1)=7
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_coat_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY POLYGON OBSCURATION'
               CALL SHOWIT(1)
            END IF
         END IF
         LS=0.0D0
         STOPP=1
         RETURN
      END IF
   ELSE
!       COFLG NOT 5
   END IF
!
!       COFLG=6 IPOLY CLAP, DOES IT STOP THE RAY
   IF(COFLG.EQ.6) THEN
      LS=0.0D0
!
!       IN THE COORDINATE SYSTEM OF THE POLYGON, THE CORNER
!       NUMBER OF POINTS IS surf_cobs_poly(I, 2), CENTER TO CORNER DISTANCE
!       IS surf_cobs_poly(II, 1). POINTS GO COUNTER CLOCKWISE LOOKING
!       TOWARD THE +Z DIRECTION
!       COORDINATES ARE:
      DO III=1,INT(surf_cobs_poly(I, 2))
         XT(III)=IPOLYX(III,I,3)
         YT(III)=IPOLYY(III,I,3)
      END DO
      XRD=X
      YRD=Y
!
!       IF NON-ZERO CLAP ERASE DECENTRATIONS EXISTS, THEY MUST BE CONSIDERED
!       IN CLAP ERASE CHECKING. WE CONVERT THE RAY COORDINATES X AND Y
!       TO XRD AND YRD IN THE COORDINATE SYSTEM OF THE DECENTERED
!       CLEAR APERTURE ERASE. REMEMBER.
!
      XRD=XRD-surf_cobs_poly(I, 4)-JK1
      YRD=YRD-surf_cobs_poly(I, 3)-JK2
!
!       IF A NON-ZERO COBS TILT EXISTS, IT MUST BE CONSIDERED
!       IN COBS CHECKING. WE CONVERT THE RAY COORDINATES XRD AND YRD
!       TO XR AND YR IN THE COORDINATE SYSTEM OF THE ROTATED

      A22=(surf_cobs_poly(I, 6)+JK3)*PII/180.0D0
      XR=(XRD*DCOS(A22))+(YRD*DSIN(A22))
      YR=(YRD*DCOS(A22))-(XRD*DSIN(A22))
!
!       ARE THE POINTS XR AND YR ON THE RECTANGLE OR OUTSIDE
      X0=XR
      Y0=YR
      NP=INT(surf_cobs_poly(I, 2))
      INS=INSID2()
      IF(INS) THEN
!       RAY BLOCKED
         LS=10.0D0
      ELSE
!       RAY NOT BLOCKED
         LS=0.0D0
      END IF
!
!       RESOLVE COBS ERASE
      IF(COERAS.NE.0.AND.LS.EQ.10.0D0)CALL COERRS
!
      IF(LS.EQ.10.0D0) THEN
!       RAY IS BLOCKED BY IRREGULAR POLYGON COBS ON SURFACE I
         RAYCOD(1)=7
         RAYCOD(2)=I
         SPDCD1=RAYCOD(1)
         SPDCD2=I
!
         IF(MSG) THEN
            IF(surf_coat_type(I).GT.0.0D0) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='RAY BLOCKED BY IRREGULAR POLYGON OBSCURATION'
               CALL SHOWIT(1)
            END IF
         END IF
         LS=0.0D0
         STOPP=1
         RETURN
      END IF
   ELSE
!       COFLG NOT 6
   END IF
   RETURN
END
