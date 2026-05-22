!       THIS IS THE FIFTH FILE OF RAYTRACING ROUTINES

! SUB QRRAY.FOR
SUBROUTINE QRRAY
   USE GLOBALS
!
   use mod_lens_data_manager, only: ldm
   use DATLEN
   use mod_surface
   use DATMAI
   use command_utils, only: is_command_query
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE QRRAY.FOR. THIS SUBROUTINE IMPLEMENTS
!       QUICK RAY TRACING AND CALLS QTRA1.FOR
!
   REAL*8 OLDAIM,X,Y,AREA1,AREA2
!
   REAL*8 SYS12,SYS13,SMALLAREA,BIGAREA
!
   INTEGER*1 ACOD
!
   INTEGER KSF,K,I,J,FOTLIM,SFNUMFT,RAYTOT,RAYCD1,RAYCD2,RAYCD3,II,JJ
!
   COMMON/RAYCD/RAYCD1,RAYCD2,RAYCD3
!
   LOGICAL TEMPER,OMSG
!
   COMMON/FOOTNUM/SFNUMFT
!
   COMMON/LIMMER/FOTLIM
!
   REAL RX,RY,RZ,RL,RM,RN,SFL,SFM,SFN,AREA3
!
!
!     CHECK INPUT
!
   IF(is_command_query()) THEN
      OUTLYNE='"FOOT" CAUSES A FOOTPRINT RAY GRID TO BE TRACED'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FOOT" ONLY TAKES NUMERIC WORD #1 AND QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(SQ.EQ.1) THEN
      IF(WQ.NE.'GRID'.AND.WQ.NE.'APE') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FOOT" ONLY TAKES "GRID" AND "APE" AS QUALIFIER WORDS'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FOOT" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(INT(W1).LT.NEWOBJ.OR.INT(W1).GT.NEWIMG) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SURFACE NUMBER BEYOND RANGE SPECIFIED IN "FOB" COMMAND'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       CHECK IF REFEXT=.FALSE. IF SO STOP AND PRINT  MESSAGE
!
   IF(.NOT.REFEXT) THEN
!       NO CHIEF RAY EXISTS, STOP
      CALL REPORT_ERROR_AND_FAIL(&
      & 'AN "FOB" COMMAND MUST BE ISSUED BEFORE RAYS CAN BE TRACED'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   TEMPER=.FALSE.
   IF(surf_clap_type(NEWREF) == 0.OR.surf_multi_clap_flag(NEWREF) /= 0) THEN
!     ASSIGN A TEMPORARY CIRCULAR CLAP
      TEMPER=.TRUE.
      call set_surf_clap_type(NEWREF, 1)
      IF(PXTRAY(1,NEWREF).GT.PXTRAX(1,NEWREF)) THEN
         call set_surf_clap_dim(NEWREF, 1, PXTRAY(1,NEWREF))
         call set_surf_clap_dim(NEWREF, 2, PXTRAY(1,NEWREF))
      ELSE
         call set_surf_clap_dim(NEWREF, 1, PXTRAX(1,NEWREF))
         call set_surf_clap_dim(NEWREF, 2, PXTRAX(1,NEWREF))
      END IF
   END IF
!
!     TOTAL NUMBER OF RECORDS IN FILE IS 1+(((2*FOTLIM)+1)**2)
!
   OPEN(UNIT=94,ACCESS='DIRECT',FORM='UNFORMATTED',FILE='FOOT1.DAT',RECL=(80*NRECL),STATUS='UNKNOWN')
   CALL CLOSE_FILE(94,0)
   OPEN(UNIT=94,ACCESS='DIRECT',FORM='UNFORMATTED',FILE='FOOT1.DAT',RECL=(80*NRECL),STATUS='UNKNOWN')
!
   OUTLYNE='BEAM FOOTPRINT DATA BEING GENERATED'
   CALL SHOWIT(1)
   WRITE(OUTLYNE,*)'CURRENT RAY GRID SIZE IS : ',(2*FOTLIM)+1,' x ',(2*FOTLIM)+1
   CALL SHOWIT(1)
   OUTLYNE='PLEASE WAIT...'
   CALL SHOWIT(1)
!
!
!     AREA OF ONE SQUARE ON THE REFERENCE SURFACE IS EQUAL TO 4 TIMES THE AREA
!     OF A RECTANGLE WHOSE DIMENSIONS ARE THE Y-REF AP HT
!     TIMES THE X-REF AP HT DIVIDED BY THE SQUARE OF THE FOTLIM VALUE
!
   IF(ABS(surf_clap_type(NEWREF)) >= 1.AND.ABS(surf_clap_type(NEWREF)) <= 6.AND.surf_multi_clap_flag(NEWREF) == 0) THEN
!
!       CLAP IS ON REFERENCE SURFACE
!
!       CIRCULAR CLAP
!
      IF(ABS(surf_clap_type(NEWREF)) == 1) THEN
         IF(surf_clap_dim(NEWREF, 1).LE.surf_clap_dim(NEWREF, 2)) THEN
            SYS12=surf_clap_dim(NEWREF, 1)
            SYS13=surf_clap_dim(NEWREF, 1)
         ELSE
            SYS12=surf_clap_dim(NEWREF, 2)
            SYS13=surf_clap_dim(NEWREF, 2)
         END IF
      ELSE
!       NOT CIRCULAR CLAP
      END IF
!        RECT CLAP
!
      IF(ABS(surf_clap_type(NEWREF)) == 2) THEN
         SYS12=surf_clap_dim(NEWREF, 1)
         SYS13=surf_clap_dim(NEWREF, 2)
      ELSE
!       NOT RECT CLAP
      END IF
!        ELIP CLAP
!
      IF(ABS(surf_clap_type(NEWREF)) == 3) THEN
         SYS12=surf_clap_dim(NEWREF, 1)
         SYS13=surf_clap_dim(NEWREF, 2)
      ELSE
!       NOT ELIP CLAP
      END IF
!        RCTK CLAP
!
      IF(ABS(surf_clap_type(NEWREF)) == 4) THEN
         SYS12=surf_clap_dim(NEWREF, 1)
         SYS13=surf_clap_dim(NEWREF, 2)
      ELSE
!       NOT RCTK CLAP
      END IF
!        POLY CLAP
!
      IF(ABS(surf_clap_type(NEWREF)) == 5) THEN
         SYS12=surf_clap_dim(NEWREF, 1)
         SYS13=surf_clap_dim(NEWREF, 1)
      ELSE
!       NOT POLY CLAP
      END IF
!        IPOLY CLAP
!
      IF(ABS(surf_clap_type(NEWREF)) == 6) THEN
         SYS12=surf_clap_dim(NEWREF, 5)
         SYS13=surf_clap_dim(NEWREF, 5)
      ELSE
!       NOT IPOLY CLAP
      END IF
!
   ELSE
!       NO CLAP ON REF SURF.
      SYS13=PXTRAX(1,NEWREF)
      SYS12=PXTRAY(1,NEWREF)
   END IF
   BIGAREA=(SYS12*SYS13)
   SMALLAREA=BIGAREA/DBLE(FOTLIM**2)
!
   RAYTOT=(((2*FOTLIM)+1)**2)+1
   KSF=INT(W1)
   SFNUMFT=INT(W1)
   WRITE(UNIT=94,REC=1) RAYTOT,KSF
!
   K=2
   OLDAIM=AIMTOL
   AIMTOL=1.0D-3
   DO I=-FOTLIM,FOTLIM
      DO J=-FOTLIM,FOTLIM
!       SET RAYCODS
         RAYCOD(1)=0
         Y=DBLE(I)/DBLE(FOTLIM)
         X=DBLE(J)/DBLE(FOTLIM)
         WW1=Y
         WW2=X
         WW3=LFOB(4)
         WVN=LFOB(4)
!       TRACE RAY AND WRITE RAYCOD(1)
         MSG=.FALSE.
         RAYCD1=0
         RAYCD3=0
         RAYCD2=-1
         CALL QTRA1(.TRUE.)
         MSG=.TRUE.
         IF(WQ.EQ.'APE') THEN
!     CONSIDER ALL CLAPS AND COBS AND OTHER BLOCKERS
!     ACOD=1 MEANS RAY SUCCEDDED
!     ACOD=0 MEANS RAY FAILED
            ACOD=1
            IF(RAYCD3.NE.0) ACOD=0
         ELSE
!
!     QUALIFIER WORD WAS NOT 'APE'
!
!     ACOD=1 MEANS RAY SUCCEDDED
!     ACOD=0 MEANS RAY FAILED
            ACOD=1
            IF(RAYCD1.EQ.6.OR.RAYCD1.EQ.7) THEN
               IF(RAYCD2.EQ.NEWREF) ACOD=0
!     IS THE SURFACE WHERE THE CLAP OR COBS BLOCK OCCURRED DEFINED AS
!     A FOOTPRINT BLOCKING SURFACE? IS SO, RAY FAILED
               IF(RAYCD3.NE.0.AND.surf_footblok_flag(RAYCD2) == 1) ACOD=0
            END IF
            IF(RAYCOD(1).NE.0.AND.RAYCOD(1).NE.6.AND.RAYCOD(1).NE.7)ACOD=0
!     SINCE RAY REALLY DID FAIL AND COULD NOT GET THROUGH.
         END IF
!
         KSF=INT(W1)
         SFNUMFT=INT(W1)
         IF(ACOD.EQ.1) THEN
            RX=REAL(QRAY(1,KSF))
            RY=REAL(QRAY(2,KSF))
            RZ=REAL(QRAY(3,KSF))
            RL=REAL(QRAY(4,KSF))
            RM=REAL(QRAY(5,KSF))
            RN=REAL(QRAY(6,KSF))
            SFL=REAL(QRAY(7,KSF))
            SFM=REAL(QRAY(8,KSF))
            SFN=REAL(QRAY(9,KSF))
         END IF
         IF(ACOD.EQ.0) THEN
            RX=0.0
            RY=0.0
            RZ=0.0
         END IF
!
!     TRACE DIFFERENTIAL RAYS
!
!       RAY WAS TRACED, NOW TRACE THE DIFFERENTIAL RAY
!       DATA
!       SAVE RAYRAY DATA
         SAVE=RAYRAY
         OMSG=MSG
         MSG=.FALSE.
         SRAYDIFEXT=.FALSE.
         DIFRAYTRACE=.TRUE.
         CALL DIFRAY
         DIFRAYTRACE=.FALSE.
         MSG=OMSG
         IF(STOPP.EQ.1) THEN
            ACOD=0
            STOPP=0
            RAYRAY=SAVE
         ELSE
            STOPP=0
            RAYEXT=.TRUE.
         END IF
!
         IF(ACOD.NE.0) THEN
!       CALC AN AREA
            AREA1=((DIFF(1,KSF)-RAYRAY(1,KSF))*(DIFF(8,KSF)-RAYRAY(2,KSF)))/(DELX*DELY)
            AREA2=((DIFF(1,NEWREF)-RAYRAY(1,NEWREF))*(DIFF(8,NEWREF)-RAYRAY(2,NEWREF)))/(DELX*DELY)
            AREA3=REAL((AREA1/AREA2)*SMALLAREA)
         END IF
         WRITE(UNIT=94,REC=K) RX,RY,RZ,RL,RM,RN,SFL,SFM,SFN,ACOD,AREA3


         K=K+1
!
!     DO THE NEXT RAY
!
!
      END DO
   END DO
   AIMTOL=OLDAIM
   CALL CLOSE_FILE(94,1)
   IF(TEMPER) THEN
      call ldm%clearApertureTypeAndParams(NEWREF)
   END IF
   RETURN
END
!
SUBROUTINE FTGRID
!
   use DATLEN
   use mod_surface
   use DATMAI
   use command_utils, only: is_command_query
   IMPLICIT NONE
!
   INTEGER FOTLIM
!
   COMMON/LIMMER/FOTLIM
!
!     CHECK INPUT
!
   IF(is_command_query()) THEN
      OUTLYNE='"FOOT GRID" CAUSES A FOOTPRINT GRID SIZE TO BE SET'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'CURRENT FOOT RAY GRID  IS : ',(2*FOTLIM+1),' x ',(2*FOTLIM)+1
      CALL SHOWIT(1)
      RETURN
   END IF
!
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FOOT GRID" ONLY TAKES NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.LT.2.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"FOOT GRID" MINIMUM VALUE FOR NUMERIC WORD #1 IS 2'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(DF1.EQ.1) W1=16.0D0
   FOTLIM=INT(W1)
   RETURN
END
! SUB QTRA1.FOR

SUBROUTINE QTRA1(FOOT_TRACE)
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_aplanatic_aim, sys_fliprefx, sys_fliprefy, &
      & sys_ray_aiming, sys_ref_orient, &
      & sys_scx, sys_scy, sys_telecentric, sys_wavelength
   use mod_lens_data_manager, only: ldm
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE QTRA1.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE TRACING OF A QUICK RAY FOR VIGCAL.
!
   INTEGER JK,I,ISYS20,KKK,J

   INTEGER CAERAS,COERAS
!
   REAL*8 X,Y,Z,L,M,N,XC1,YC1,ZC1,WWW1,WWW2,D21,D22,GAMMA,JKX,JKY,XL,XM,XN,YL,YM,YN,D11,D12,LS,SNINDX,SNIND2,JK1,JK2,JK3,LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,MAG,MF1,MF2,DET,TARX,TARY,XVALUE,YVALUE,VALUE,LOLD,MOLD,NOLD,TARRY,TARRX
!
   COMMON/CACO/CAERAS,COERAS,LS
!
   LOGICAL AIMOK,CLAPT,OLDPASS,MMSG,FOOT_TRACE,DELFAIL
   COMMON/PASSOLD/OLDPASS
   INTEGER SPDCD1,SPDCD2
   COMMON/SPRA2/SPDCD1,SPDCD2
   INTEGER RAYCD1,RAYCD2,RAYCD3
   COMMON/RAYCD/RAYCD1,RAYCD2,RAYCD3
!
!
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
   KKK=0
   DDELX=0.001D0
   DDELY=0.001D0
!       DDELX AND DDELY ARE THE INITIAL INCREMENTORS FOR THE
!       DERIVATIVE CALCULATION IN NEWTON-RAPHSON SEARCH FOR
!       RAY AIMING.
!
   IF(WW3.GE.1.0D0.AND.WW3.LE.5.0D0) THEN
      IF(sys_wavelength(INT(WW3)).EQ.0.0D0) THEN
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
      IF(sys_wavelength(INT(WW3)).EQ.0.0D0) THEN
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
   RAYCOD(1)=0
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
!        IS AIMED.
!     EXCEPT IF THE CLAP VALUE IS SMALLER
   IF(sys_telecentric().EQ.0.0D0) THEN
!       TELECENTRIC AIMING OFF
      JKX=PXTRAX(1,NEWOBJ+1)
      JKY=PXTRAY(1,NEWOBJ+1)
      IF(surf_clap_type(NEWOBJ+1) == 1) THEN
!     CIRCULAR AP
         IF(surf_clap_dim(NEWOBJ+1, 1).LE.surf_clap_dim(NEWOBJ+1, 2)) THEN
            IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 1))
            IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 1))
         ELSE
            IF(DABS(surf_clap_dim(NEWOBJ+1, 2)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 2))
            IF(DABS(surf_clap_dim(NEWOBJ+1, 2)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 2))
         END IF
      END IF
      IF(surf_clap_type(NEWOBJ+1) == 5) THEN
!     CIRCULAR AP
         IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 1))
         IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 1))
      END IF
      IF(surf_clap_type(NEWOBJ+1) == 6) THEN
!     CIRCULAR AP
         IF(DABS(surf_clap_dim(NEWOBJ+1, 5)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 5))
         IF(DABS(surf_clap_dim(NEWOBJ+1, 5)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 5))
      END IF
      IF(surf_clap_type(NEWOBJ+1).GT.1.0D0.AND.surf_clap_type(NEWOBJ+1).LE.4.0D0) THEN
!     OTHER AP
         IF(DABS(surf_clap_dim(NEWOBJ+1, 2)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 2))
         IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 1))
      END IF
   ELSE
!       TELECENTRIC AIMING ON
      JKX=PXTRAX(1,NEWOBJ+1)
      JKY=PXTRAY(1,NEWOBJ+1)
   END IF
!
989 CONTINUE
   IF(.NOT.FOOT_TRACE.AND.KKK.GT.1.AND.RAYCOD(1).EQ.1) THEN
      STOPP=1
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
      RETURN
   END IF
   IF(NULL) THEN
      IF(sys_ray_aiming().EQ.0.0D0) THEN
         IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
            X1AIM=((PXTRAX(5,(NEWOBJ+1)))+(WW2*JKX))
            X1AIM=(X1AIM)-surf_decenter_x(NEWOBJ+1)
            Y1AIM=((PXTRAY(5,(NEWOBJ+1)))+(WW1*JKY))
            Y1AIM=(Y1AIM)-surf_decenter_x(NEWOBJ+1)
            Z1AIM=0.0D0
            XC=X1AIM
            YC=Y1AIM
            ZC=Z1AIM
            XC1=XC
            YC1=YC
            ZC1=ZC
         ELSE
!     TEL ON
            IF(sys_scx().NE.0.0D0)X1AIM=((PXTRAX(5,NEWOBJ))+(WW2*JKX))
            IF(sys_scx().EQ.0.0D0)X1AIM=((WW2*JKX))
            IF(sys_scy().NE.0.0D0)Y1AIM=((PXTRAY(5,NEWOBJ))+(WW1*JKY))
            IF(sys_scy().EQ.0.0D0)Y1AIM=((WW1*JKY))
            Z1AIM=0.0D0
            XC=X1AIM
            YC=Y1AIM
            ZC=Z1AIM
            XC1=XC
            YC1=YC
            ZC1=ZC
         END IF
      ELSE
         X1AIM=((PXTRAX(5,NEWOBJ+1))+(WW2*JKX))
         X1AIM=(X1AIM)-surf_decenter_x(NEWOBJ+1)
         Y1AIM=((PXTRAY(5,NEWOBJ+1))+(WW1*JKY))
         Y1AIM=(Y1AIM)-surf_decenter_x(NEWOBJ+1)
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
   ELSE
!     NOT NULL
      IF(sys_ray_aiming().EQ.0.0D0) THEN
         IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
            X1AIM=((REFRY(1,(NEWOBJ+1)))+(WW2*JKX))
            Y1AIM=((REFRY(2,(NEWOBJ+1)))+(WW1*JKY))
            Z1AIM=REFRY(3,NEWOBJ+1)
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
!
!       NOW THE STARTING DIRECTION COSINES ARE RELATIVELY SIMPLE.
!       THEY ARE CALLED LSTART,MSTART AND NSTART
!
!       FIRST CONVERT THE SURFACE 0 AND 1 COORDINATES INTO YANG AND XANG
!       ANGULAR VALUES AS IN HEXAGON AND CODE V.
!       YANG AND XANG ANGLES IN RADIANS.
   IF(DABS(Z1AIM-ZSTRT).EQ.0.0D0)THEN
      RAYCOD(1)=8
      RAYCOD(2)=NEWOBJ
      STOPP=1
      RETURN
   ELSE
      STOPP=0
!       PROCEED
   END IF
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
!       QRAY(1:9,0:MAXSUR)
!
!
!       KKK COUNTS THE NUMBER OF TRIES TO GET A GOOD REFERENCE
!       SURFACE POINT INTERSECTION
9  CONTINUE
   IF(surf_thickness(NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
   IF(surf_thickness(NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
   RV=.FALSE.
   KKK=KKK+1
!       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
!       AND STOP SEARCHING.
   IF(KKK.GT.NRAITR) THEN
      RAYCOD(1)=3
      RAYCOD(2)=NEWREF
      STOPP=1
      RETURN
   ELSE
      STOPP=0
!        PROCEED
   END IF
!
!       THIS IS THE RE-ENTRY POINT FOR RAY AIMING. RE-ENTRY
!       IS PERFORMED AFTER NEW VALUES OF X1AIM AND Y1AIM WERE
!       CALCULATED JUST BEFORE ENCOUNTERING THE (GO TO 9)
!       STATEMENT NEAR THE END OF THIS ROUTINE.
!
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
!
!       FOR SURFACE NEWOBJ
!       QRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
!       QRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
!       QRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
!       QRAY(4,SURF)=X(LOCAL)- L RAY DIR COS
!       QRAY(5,SURF)=Y(LOCAL)- M RAY DIR COS
!       QRAY(6,SURF)=Z(LOCAL)- N RAY DIR COS
!       QRAY(7,SURF)=X(LOCAL)- L SURFACE NORM
!       QRAY(8,SURF)=Y(LOCAL)- M SURFACE NORM
!       QRAY(9,SURF)=Z(LOCAL)- N SURFACE NORM
   QRAY(1,NEWOBJ)=XSTRT
   QRAY(2,NEWOBJ)=YSTRT
   QRAY(3,NEWOBJ)=ZSTRT
   QRAY(4,NEWOBJ)=LSTART
   QRAY(5,NEWOBJ)=MSTART
   QRAY(6,NEWOBJ)=NSTART
   QRAY(7,NEWOBJ)=0.0D0
   QRAY(8,NEWOBJ)=0.0D0
   QRAY(9,NEWOBJ)=1.0D0
   RAYRAY(1,NEWOBJ)=XSTRT
   RAYRAY(2,NEWOBJ)=YSTRT
   RAYRAY(3,NEWOBJ)=ZSTRT
   RAYRAY(4,NEWOBJ)=LSTART
   RAYRAY(5,NEWOBJ)=MSTART
   RAYRAY(6,NEWOBJ)=NSTART
!
   X=XSTRT
   Y=YSTRT
   Z=ZSTRT
   L=LSTART
   M=MSTART
   N=NSTART
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
      CALL TRNSF2_ARGS(I, X, Y, Z, L, M, N)
      STOPP=0
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
      IF(SNIND2.GT.0.0D0) RAYRAY(24,I)=1.0D0
      IF(SNIND2.LT.0.0D0) RAYRAY(24,I)=-1.0D0
      IF(SNIND2.GT.0.0D0) POSRAY=.TRUE.
      IF(SNIND2.LT.0.0D0) POSRAY=.FALSE.
      CALL HITSUR
      IF(STOPP.EQ.1) THEN
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

      IF(STOPP.EQ.1) THEN
! NEW STUFF 6/2/94
         IF(KKK.EQ.1.AND.RAYCOD(1).EQ.1) THEN
            JKX=0.0D0
            JKY=0.0D0
            KKK=KKK+1
            STOPP=0
            GO TO 989
         END IF
         RETURN
      ELSE
         STOPP=0
      END IF
!       LOAD REF RAY REGISTERS
!       FOR SURFACE NEWOBJ
!       QRAY(1,SURF)=X(LOCAL)- COORDINATE AT SURFACE I
!       QRAY(2,SURF)=Y(LOCAL)- COORDINATE AT SURFACE I
!       QRAY(3,SURF)=Z(LOCAL)- COORDINATE AT SURFACE I
!       QRAY(4,SURF)=X(LOCAL)- L RAY DIR COS
!       QRAY(5,SURF)=Y(LOCAL)- M RAY DIR COS
!       QRAY(6,SURF)=Z(LOCAL)- N RAY DIR COS
!       QRAY(7,SURF)=X(LOCAL)- L SURFACE NORM
!       QRAY(8,SURF)=Y(LOCAL)- M SURFACE NORM
!       QRAY(9,SURF)=Z(LOCAL)- N SURFACE NORM
      QRAY(1,I)=X
      QRAY(2,I)=Y
      QRAY(3,I)=Z
      QRAY(4,I)=L
      QRAY(5,I)=M
      QRAY(6,I)=N
      QRAY(7,I)=LN
      QRAY(8,I)=MN
      QRAY(9,I)=NN
      RAYRAY(1,I)=X
      RAYRAY(2,I)=Y
      RAYRAY(3,I)=Z
      RAYRAY(4,I)=L
      RAYRAY(5,I)=M
      RAYRAY(6,I)=N
!
!     WHAT IS THE SIGN OF THE INDEX IN THE I-1 SPACE
      SNINDX=DABS(ldm%getSurfIndex(I-1,INT(WW3)))/ldm%getSurfIndex(I-1,INT(WW3))
      SNIND2=DABS(ldm%getSurfIndex(I,INT(WW3)))/ldm%getSurfIndex(I,INT(WW3))
!
!     PHYSICAL LENGTH OF THE RAY FROM I-1 TO I IS THE MAG OF THE
!     DIST TRAVELED TIME THE SNINDX TIMES -1 IF RV IN THE SPACE
!     FROM I-1 TO I
!
!       CHECK THE RAY HEIGHT AT
!       NEWR AND ADJUST THE Y1AIM AND X1AIM AT SURFACE
!       NEWOBJ+1 SO THAT THE RAY HEIGHT AT NEWR IS WITHIN
!       AIMTOL . MAXIMUN NUMBER OF ITERRATIONS IS
!       NRAITR. (DEFAULT IS 100).
!
      IF(I.EQ.NEWREF) THEN
!       CALCULATE TARX AND TARY
         IF(ABS(surf_clap_type(I)) >= 1.AND.ABS(surf_clap_type(I)) <= 6.AND.surf_multi_clap_flag(I) == 0) THEN
!     CLAPT=TRUE FOR CLAP TILTS AND DECENTED AND FALSE OTHERWISE
            CLAPT=.FALSE.
            IF(surf_clap_dim(I, 3).NE.0.0D0.OR.surf_clap_dim(I, 4).NE.0.0D0.OR.surf_clap_tilt(I).NE.0.0D0) CLAPT=.TRUE.
!       REF SURF HAS CLAP ON IT
!       THE VALUES OF TARY AND TARX ARE COORDINATES IN THE
!       LOCAL COORDINATE SYSTEM OF THE REFERENCE SYSTEM.
!       IF THE CLAP IS DECENTERED, THE TAR() VALUES ARE
!       MODIFIED BY ADDING THE CLAP DECENTRATIONS SINCE THE RAY
!       IS AIMED TO THE RELATIVE REFERENCE SURFACE COORDINATES.
!
!       SET TARGET TO CENTER OF DECENTERED CLAP, surf_clap_dim(I, 3),
!       AND surf_clap_dim(I, 4) ARE CLAP DECENTRATIONS
!
            WWW1=WW1
            WWW2=WW2
            IF(sys_aplanatic_aim().EQ.1.0D0.AND.surf_curvature(I).NE.0.0D0.AND.surf_clap_type(I) == 1.AND.surf_clap_dim(I, 3).EQ.0.0D0.AND.surf_clap_dim(I, 4).EQ.0.0D0.AND.surf_clap_tilt(I).EQ.0.0D0) THEN
               IF(DABS(1.0D0/surf_curvature(I)).GE.DABS(surf_clap_dim(I, 1)).AND.DABS(1.0D0/surf_curvature(I)).GE.DABS(surf_clap_dim(I, 2)))CALL APLANA(I,WW1,WW2,WWW1,WWW2)
            END IF
!
!       CIRCULAR CLAP
!
            IF(ABS(surf_clap_type(I)) == 1) THEN
               IF(CLAPT) THEN
                  IF(surf_clap_dim(I, 1).LE.surf_clap_dim(I, 2)) THEN
                     TARY=(surf_clap_dim(I, 1)*WWW1)
                     TARX=(surf_clap_dim(I, 1)*WWW2)
                  ELSE
                     TARY=(surf_clap_dim(I, 2)*WWW1)
                     TARX=(surf_clap_dim(I, 2)*WWW2)
                  END IF
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
!     NO CLAP DEC OR TILTS
                  IF(surf_clap_dim(I, 1).LE.surf_clap_dim(I, 2)) THEN
                     TARY=(surf_clap_dim(I, 1)*WWW1)
                     TARX=(surf_clap_dim(I, 1)*WWW2)
                  ELSE
                     TARY=(surf_clap_dim(I, 2)*WWW1)
                     TARX=(surf_clap_dim(I, 2)*WWW2)
                  END IF
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
!       NOW IS THE REF SURF ORIENTATION ANGLE ?
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT CIRCULAR CLAP
            END IF
!        RECT CLAP
!
            IF(ABS(surf_clap_type(I)) == 2) THEN
               IF(CLAPT) THEN
                  TARY=(surf_clap_dim(I, 1)*WW1)
                  TARX=(surf_clap_dim(I, 2)*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  TARY=(surf_clap_dim(I, 1)*WW1)
                  TARX=(surf_clap_dim(I, 2)*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT RECT CLAP
            END IF
!        ELIP CLAP
!
            YVALUE=surf_clap_dim(I, 1)
            XVALUE=surf_clap_dim(I, 2)
!
            IF(ABS(surf_clap_type(I)) == 3) THEN
               IF(CLAPT) THEN
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT ELIP CLAP
            END IF
!        RCTK CLAP
!
            IF(ABS(surf_clap_type(I)) == 4) THEN
               IF(CLAPT) THEN
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 2)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 2)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT RCTK CLAP
            END IF
!        POLY CLAP
!
            IF(ABS(surf_clap_type(I)) == 5) THEN
               IF(CLAPT) THEN
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 1)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 1)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT POLY CLAP
            END IF
            IF(ABS(surf_clap_type(I)) == 6) THEN
               IF(CLAPT) THEN
                  YVALUE=surf_clap_dim(I, 5)
                  XVALUE=surf_clap_dim(I, 5)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  YVALUE=surf_clap_dim(I, 2)
                  XVALUE=surf_clap_dim(I, 2)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT IPOLY CLAP
            END IF
!
         ELSE
!       NO CLAP ON REF SURF.
            TARY=(PXTRAY(1,I)*WW1)
            TARX=(PXTRAX(1,I)*WW2)
            IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
            IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
            GAMMA=(sys_ref_orient()*(PII))/180.0D0
            TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
            TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
            TARY=TARRY
            TARX=TARRX
         END IF
!
         IF(DSQRT(((TARX-X)**2)+((TARY-Y)**2)).LE.AIMTOL.OR.sys_ray_aiming().EQ.0.0D0) THEN
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
            IF(surf_curvature(1).NE.0.0D0)CALL GETZEE1
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
         CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
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
         IF(DELFAIL) THEN
            RETURN
         ELSE
            GO TO 9
         END IF
!       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
      END IF
!
100   CONTINUE
!
10 CONTINUE
   DO R_I=NEWOBJ+1,NEWIMG-1
!       HERE WE CHECK FOR BLOCKAGE BY CLAP OR COBS.
!       AND DO THE APPROPRIATE THINGS.
!       CALL CLAP CHECKING ROUTINE
      IF(ABS(surf_special_type(R_I)) /= 24) THEN
!     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
!
         R_X=QRAY(1,R_I)
         R_Y=QRAY(2,R_I)
         R_Z=QRAY(3,R_I)
         RAYCOD(1)=0
         RAYCOD(2)=-1
         MMSG=MSG
         IF(INT(surf_multi_clap_flag(R_I)).EQ.0.AND.INT(surf_multi_cobs_flag(R_I)).EQ.0) THEN
            CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
         ELSE
            IF(INT(surf_multi_clap_flag(R_I)).NE.0) THEN
               DO JK=1,INT(surf_multi_clap_flag(R_I))
                  IF(MMSG) THEN
                     MSG=.TRUE.
                     IF(JK.LT.INT(surf_multi_clap_flag(R_I))) MSG=.FALSE.
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
25             CONTINUE
            END IF
            IF(INT(surf_multi_cobs_flag(R_I)).NE.0) THEN
               DO JK=1,INT(surf_multi_cobs_flag(R_I))
                  IF(MMSG) THEN
                     MSG=.TRUE.
                  END IF
                  JK1=MULTCOBS(JK,1,R_I)
                  JK2=MULTCOBS(JK,2,R_I)
                  JK3=MULTCOBS(JK,3,R_I)
                  CALL CACHEK(JK1,JK2,JK3,2)
                  IF(RAYCOD(1).NE.0) THEN
                     SPDCD1=RAYCOD(1)
                     SPDCD2=RAYCOD(2)
                     STOPP=1
                     RAYEXT=.FALSE.
                     POLEXT=.FALSE.
                     GO TO 26
                  END IF
               END DO
26             CONTINUE
            END IF
         END IF
      END IF
      IF(.NOT.FOOT_TRACE.AND.RAYCOD(1).NE.0.AND.RAYCOD(1).NE.7) RETURN
      IF(FOOT_TRACE) THEN
         IF(RAYCOD(1).EQ.6.OR.RAYCOD(1).EQ.7) THEN
            IF(RAYCOD(2).EQ.NEWREF) RAYCD1=RAYCOD(1)
            IF(RAYCOD(2).EQ.NEWREF) RAYCD2=RAYCOD(2)
            IF(surf_footblok_flag(RAYCOD(2)).EQ.1.0D0) THEN
               RAYCD1=RAYCOD(1)
               RAYCD2=RAYCOD(2)
            END IF
         END IF
         IF(RAYCOD(1).NE.0) RAYCD3=RAYCOD(1)
      END IF
!       THIS ROUTINE SETS THE FLAGS
!       CAERAS AND COERAS
!       SET IF THE CURRENT SURFACE HAD A COBS OR
!       CLAP ERASE.
      STOPP=0
   END DO
   RETURN
END
! SUB RAYDMP.FOR
SUBROUTINE RAYDMP
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS SUBROUTINE CONTROLS THE OPERATION OF THE "RAYDUMP"
!       COMMAND
!
   INTEGER I
!
   REAL*8 ACLENG
!
   CHARACTER*17 POS,REV
!
   COMMON/ACLEN/ACLENG
!
!
   IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
      WRITE(OUTLYNE,*)'"RAYDUMP" TAKES NO INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETURN
   END IF
!     FIRST PRINT A MESSAGE AS TO WHETHER OR NOT THE LAST RAY
!     FAILED OR NOT OR WAS NEVER TRACED.
!     IF RAYCOD(1)=0 AND RAYEXT FALSE
   IF(RAYCOD(1).EQ.0.AND..NOT.RAYEXT) THEN
!       NO RAY WAS TRACED, NO DATA EXISTS TO DUMP
      WRITE(OUTLYNE,5)'NO CURRENT RAY DATA EXISTS TO DUMP'
      CALL SHOWIT(0)
5     FORMAT(A34)
      RETURN
   END IF
!     IF RAYCOD(1)=0 AND RAYCOD(2).EQ.NEWIMG.AND.RAYEXT.TRUE
!     RAY WAS TRACED SUCCESSFULLY
   IF(RAYCOD(1).EQ.0.AND.RAYEXT) THEN
!       RAY WAS TRACED SUCCESSFULLY
      WRITE(OUTLYNE,10)'CURRENT RAY WAS TRACED SUCCESSFULLY'
      CALL SHOWIT(0)
10    FORMAT(A35)
!
!     HERE IS WHERE OUTPUT STATEMENTS GO FOR RAY DUMP
      DO I=NEWOBJ,NEWIMG

         WRITE(OUTLYNE,100) I,RAYRAY(1,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,101) I,RAYRAY(2,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,102) I,RAYRAY(3,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,103) I,RAYRAY(4,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,104) I,RAYRAY(5,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,105) I,RAYRAY(6,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,106) I,RAYRAY(7,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,107) I,RAYRAY(8,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,108) I,RAYRAY(9,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,109) I,RAYRAY(10,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,110) I,RAYRAY(11,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,111) I,RAYRAY(12,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,112) I,RAYRAY(13,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,113) I,RAYRAY(14,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,114) I,RAYRAY(15,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,115) I,RAYRAY(16,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,116) I,RAYRAY(17,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,117) I,RAYRAY(18,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,118) I,RAYRAY(19,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,119) I,RAYRAY(20,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,120) I,RAYRAY(21,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,121) I,RAYRAY(22,I)
         CALL SHOWIT(0)
         IF(RAYRAY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
         IF(RAYRAY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '

         WRITE(OUTLYNE,122) I,REV
         CALL SHOWIT(0)
         IF(RAYRAY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
         IF(RAYRAY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'

         WRITE(OUTLYNE,122) I,POS
         CALL SHOWIT(0)

         WRITE(OUTLYNE,124) I,RAYRAY(25,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,125) I,RAYRAY(26,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,126) I,RAYRAY(27,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,127) I,RAYRAY(28,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,128) I,RAYRAY(29,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,129) I,RAYRAY(30,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,130) I,RAYRAY(31,I)
         CALL SHOWIT(0)
100      FORMAT('SURF=',I3,1X,'        X-COORDINATE=',D23.15)
101      FORMAT('SURF=',I3,1X,'        Y-COORDINATE=',D23.15)
102      FORMAT('SURF=',I3,1X,'        Z-COORDINATE=',D23.15)
103      FORMAT('SURF=',I3,1X,'       L-DIR. COSINE=',D23.15)
104      FORMAT('SURF=',I3,1X,'       M-DIR. COSINE=',D23.15)
105      FORMAT('SURF=',I3,1X,'       N-DIR. COSINE=',D23.15)
106      FORMAT('SURF=',I3,1X,'     PHYSICAL LENGTH=',D23.15)
107      FORMAT('SURF=',I3,1X,'OPL-(SURF-1 TO SURF)=',D23.15)
108      FORMAT('SURF=',I3,1X,'           COSINE(I)=',D23.15)
109      FORMAT('SURF=',I3,1X,'          COSINE(IP)=',D23.15)
110      FORMAT('SURF=',I3,1X,'   XZ-SLOPE(RADIANS)=',D23.15)
111      FORMAT('SURF=',I3,1X,'   YZ-SLOPE(RADIANS)=',D23.15)
112      FORMAT('SURF=',I3,1X,'  L-(SURFACE NORMAL)=',D23.15)
113      FORMAT('SURF=',I3,1X,'  M-(SURFACE NORMAL)=',D23.15)
114      FORMAT('SURF=',I3,1X,'  N-(SURFACE NORMAL)=',D23.15)
115      FORMAT('SURF=',I3,1X,' X-(PRE-INTERACTION)=',D23.15)
116      FORMAT('SURF=',I3,1X,' Y-(PRE-INTERACTION)=',D23.15)
117      FORMAT('SURF=',I3,1X,' Z-(PRE-INTERACTION)=',D23.15)
118      FORMAT('SURF=',I3,1X,' L-(PRE-INTERACTION)=',D23.15)
119      FORMAT('SURF=',I3,1X,' M-(PRE-INTERACTION)=',D23.15)
120      FORMAT('SURF=',I3,1X,' N-(PRE-INTERACTION)=',D23.15)
121      FORMAT('SURF=',I3,1X,'OPL-(NEWOBJ TO SURF)=',D23.15)
122      FORMAT('SURF=',I3,1X,A17)
124      FORMAT('SURF=',I3,1X,'     RAY ENERGY TERM=',D23.15)
125      FORMAT('SURF=',I3,1X,'       XL DIR COSINE=',D23.15)
126      FORMAT('SURF=',I3,1X,'       XM DIR COSINE=',D23.15)
127      FORMAT('SURF=',I3,1X,'       XN DIR COSINE=',D23.15)
128      FORMAT('SURF=',I3,1X,'       YL DIR COSINE=',D23.15)
129      FORMAT('SURF=',I3,1X,'       YM DIR COSINE=',D23.15)
130      FORMAT('SURF=',I3,1X,'       YN DIR COSINE=',D23.15)
      END DO
!
      RETURN
   END IF
!     IF RAYCOD(1)NOT=0 RAY WAS TRACED UNSUCCESSFULLY TO SURFACE
!     RAYCOD(2)
   IF(RAYCOD(1).NE.0) THEN
!       RAY WAS TRACED UNSUCCESSFULLY TO SURFACE RAYCOD(2)
      WRITE(OUTLYNE,15)'CURRENT RAY WAS TRACED UNSUCCESSFULLY AND FAILED AT SURFACE # ',RAYCOD(2)
      CALL SHOWIT(0)
15    FORMAT(A62,I3)
!
      IF(RAYCOD(1).EQ.1) THEN
         WRITE(OUTLYNE,*)'RAY DID NOT INTERSECT THE SURFACE'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.2) THEN
         WRITE(OUTLYNE,*)'CONVERGENCE FAILURE OCCURRED DURING RAY INTERSECTION'
         CALL SHOWIT(0)
         WRITE(OUTLYNE,*)'WITH ASPHERIC, TORIC OR SPECIAL SURFACE'
         CALL SHOWIT(0)
         WRITE(OUTLYNE,*)'ACCURACY OF INTERSECTION WAS = ',ACLENG
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.3) THEN
         WRITE(OUTLYNE,*)'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.4) THEN
         IF(MSG) THEN
            WRITE(OUTLYNE,*)'TOTAL INTERNAL REFLECTION'
            CALL SHOWIT(0)
         END IF
      END IF
      IF(RAYCOD(1).EQ.5) THEN
         WRITE(OUTLYNE,*)'ANGLE OF DIFFRACTION AT THE GRATING IS PHYSICALLY UNREALIZABLE'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.6) THEN
         WRITE(OUTLYNE,*)'RAY BLOCKED BY CLEAR APERTURE ERASE'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.7) THEN
         WRITE(OUTLYNE,*)'RAY BLOCKED BY OBSCURATION ERASE'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.8) THEN
         WRITE(OUTLYNE,*)'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
         CALL SHOWIT(0)
         WRITE(OUTLYNE,*) '10',RAYCOD(1),RAYCOD(2)
         CALL SHOWIT(1)
      END IF
      IF(RAYCOD(1).EQ.9) THEN
         WRITE(OUTLYNE,*)'ANGLE OF DIFFRACTION AT THE HOE IS PHYSICALLY UNREALIZABLE'
         CALL SHOWIT(0)
      END IF

      IF(RAYCOD(1).EQ.10) THEN
         WRITE(OUTLYNE,*)'HOE CONSTRUCTION POINTS CONFLICT WITH TRANSMISSION/REFLECTION'
         CALL SHOWIT(0)
         WRITE(OUTLYNE,*)'MODE OR CONSTRUCTION POINTS ARE NOT DEFINED'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.11) THEN
         WRITE(OUTLYNE,*)'REFERENCE RAY CAN NOT BE TRACED AT ZERO WAVELENGTH '
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.12) THEN
         WRITE(OUTLYNE,*)'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH '
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.13) THEN
         WRITE(OUTLYNE,*)'RAY MISSED GRAZING INCIDENCE SURFACE SECTION'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.14) THEN
         WRITE(OUTLYNE,*)'ILLUMINATION RAY BLOCKED BY CLEAR APERTURE'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.15) THEN
         WRITE(OUTLYNE,*)'NO GRID FILE EXISTS FOR THIS SURFACE'
         CALL SHOWIT(0)
         WRITE(OUTLYNE,*)'OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
         CALL SHOWIT(0)
      END IF
      IF(RAYCOD(1).EQ.16) THEN
         WRITE(OUTLYNE,*)'SPECIFIED OBJECT POINT DOES NOT EXIST'
         CALL SHOWIT(0)
      END IF
!
!     HERE IS WHERE OUTPUT STATEMENTS GO FOR RAY DUMP
      DO I=NEWOBJ,RAYCOD(2)
         IF(I.EQ.RAYCOD(2))WRITE(OUTLYNE,*)'DATA FOR THIS LAST SURFACE IS SUSPECT'
         IF(I.EQ.RAYCOD(2))CALL SHOWIT(0)

         WRITE(OUTLYNE,100) I,RAYRAY(1,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,101) I,RAYRAY(2,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,102) I,RAYRAY(3,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,103) I,RAYRAY(4,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,104) I,RAYRAY(5,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,105) I,RAYRAY(6,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,106) I,RAYRAY(7,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,107) I,RAYRAY(8,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,108) I,RAYRAY(9,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,109) I,RAYRAY(10,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,110) I,RAYRAY(11,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,111) I,RAYRAY(12,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,112) I,RAYRAY(13,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,113) I,RAYRAY(14,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,114) I,RAYRAY(15,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,115) I,RAYRAY(16,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,116) I,RAYRAY(17,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,117) I,RAYRAY(18,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,118) I,RAYRAY(19,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,119) I,RAYRAY(20,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,120) I,RAYRAY(21,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,121) I,RAYRAY(22,I)
         CALL SHOWIT(0)

         IF(RAYRAY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
         IF(RAYRAY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '

         WRITE(OUTLYNE,122) I,REV
         CALL SHOWIT(0)
         IF(RAYRAY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
         IF(RAYRAY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'

         WRITE(OUTLYNE,122) I,POS
         CALL SHOWIT(0)

         WRITE(OUTLYNE,124) I,RAYRAY(25,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,125) I,RAYRAY(26,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,126) I,RAYRAY(27,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,127) I,RAYRAY(28,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,128) I,RAYRAY(29,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,129) I,RAYRAY(30,I)
         CALL SHOWIT(0)

         WRITE(OUTLYNE,130) I,RAYRAY(31,I)
         CALL SHOWIT(0)
         IF(RAYRAY(23,I).EQ.1.0D0) REV= 'RAY NOT REVERSED '
         IF(RAYRAY(23,I).EQ.-1.0D0) REV='RAY REVERSED     '

         WRITE(OUTLYNE,122) I,REV
         CALL SHOWIT(0)
         IF(RAYRAY(24,I).EQ.1.0D0) POS= 'RAY IS POSRAY    '
         IF(RAYRAY(24,I).EQ.-1.0D0) POS='RAY IS NOT POSRAY'

         WRITE(OUTLYNE,122) I,POS
         CALL SHOWIT(0)
      END DO
!
      RETURN
   END IF
   RETURN
END

SUBROUTINE RAYTRA
   use real_ray_trace

   !CALL RAYTRA_NEW
   CALL RAYTRA_OLD

END SUBROUTINE
! SUB RAYTRA.FOR

SUBROUTINE RAYTRA_OLD
   USE GLOBALS
   use mod_lens_data_manager, only: ldm
   use real_ray_trace, only: adjustLastSurface
   use type_utils, only: real2str, int2str
   use surface_params, only: A_APTYPE, A_CLAP_P1, A_CLAP_P2, A_CLAP_P3, A_CURV, &
      & A_IDEAL_FL, A_MULTICLAP, A_MULTICOBS, A_N2_OFFSET, A_N_OFFSET, &
      & A_SURFTYPE, A_THI, A_XDEC, A_YDEC
   use mod_system, only: sys_aplanatic_aim, sys_ray_aiming, sys_ref_orient, &
      & sys_scx, sys_scy, sys_screen, sys_screen_d, &
      & sys_screen_h, sys_screen_s, sys_screen_surf, sys_telecentric, sys_wavelength
   use mod_system, only: sys_screen_excl_angle
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
   integer :: jj
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
   REAL*8 X,Y,Z,L,M,N,WW1W,WW2W,LER,MER,NER,TANN1,TANN2,IA,IAP,D21,D22,GAMMA,XXX,YYY,TWW1,TWW2,XL,XM,XN,YL,YM,YN,RN1,RN2,WWW1,WWW2,D11,D12,LS,SNINDX,SNIND2,WW1WW,WW2WW,JK1,JK2,JK3,LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,ENERGY_FACTOR,Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,XC1,YC1,ZC1,MF1,MF2,DET,TARX,TARY,XVALUE,YVALUE,TEST,MAG,VALUE,LOLD,MOLD,NOLD,TARRY,TARRX,JKX,JKY,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2,JK_CPL,JK_CPM,JK_CPN,SA_CPL,SA_CPM,SA_CPN,DP,POLANG,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL,STEPL,STEPL1
!
   COMMON/CACO/CAERAS,COERAS,LS
!
!     VARIABLES FOR SPOT TRACING
   LOGICAL TCLPRF,SPDTRA,MMSG
   LOGICAL AIMOK,CLAPT,OLDPASS,GERROR,DELFAIL,ray_blocked
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
      IF(sys_wavelength(INT(WW3)).EQ.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(NEWOBJ)
            OUTLYNE='RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
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
      IF(sys_wavelength(INT(WW3)).EQ.0.0D0) THEN
         IF(MSG) THEN
            CALL RAY_FAILURE(NEWOBJ)
            OUTLYNE='RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
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
   !  IF(ABS(sys_scy()).GT.1E16) THEN
   !
   !   CALL PROCESKDP("SHO ENPOSZ")
   !   PRINT *, "Entrance pupil position ", REG(9)
   !   PRINT *, "Last FOB Y is ", LFOB(1)
   !   PRINT *, "sys_scy() ", sys_scy()
   !   !PRINT *, "TST YSTRT ", LFOB(1)*REG(9)*TAND(SYSTEM(21))
   !   !sys_scy()
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
   IF(sys_telecentric().EQ.0.0D0) THEN
!       TELECENTRIC AIMING IS OFF
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
      IF(ALENS(A_APTYPE,NEWOBJ+1).EQ.1.0D0) THEN
!     CIRCULAR AP
         IF(ALENS(A_CLAP_P1,NEWOBJ+1).LE.ALENS(A_CLAP_P2,NEWOBJ+1)) THEN
            IF(DABS(ALENS(A_CLAP_P1,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(ALENS(A_CLAP_P1,NEWOBJ+1))
            IF(DABS(ALENS(A_CLAP_P1,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(ALENS(A_CLAP_P1,NEWOBJ+1))
         ELSE
            IF(DABS(ALENS(A_CLAP_P2,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(ALENS(A_CLAP_P2,NEWOBJ+1))
            IF(DABS(ALENS(A_CLAP_P2,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(ALENS(A_CLAP_P2,NEWOBJ+1))
         END IF
      END IF
      IF(ALENS(A_APTYPE,NEWOBJ+1).EQ.5.0D0) THEN
!     CIRCULAR AP
         IF(DABS(ALENS(A_CLAP_P1,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(ALENS(A_CLAP_P1,NEWOBJ+1))
         IF(DABS(ALENS(A_CLAP_P1,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(ALENS(A_CLAP_P1,NEWOBJ+1))
      END IF
      IF(ALENS(A_APTYPE,NEWOBJ+1).EQ.6.0D0) THEN
!     CIRCULAR AP
         IF(DABS(ALENS(A_CLAP_P3,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(ALENS(A_CLAP_P3,NEWOBJ+1))
         IF(DABS(ALENS(A_CLAP_P3,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(ALENS(A_CLAP_P3,NEWOBJ+1))
      END IF
      IF(ALENS(A_APTYPE,NEWOBJ+1).GT.1.0D0.AND.ALENS(A_APTYPE,NEWOBJ+1).LE.4.0D0) THEN
!     OTHER AP
         IF(DABS(ALENS(A_CLAP_P2,NEWOBJ+1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(ALENS(A_CLAP_P2,NEWOBJ+1))
         IF(DABS(ALENS(A_CLAP_P1,NEWOBJ+1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(ALENS(A_CLAP_P1,NEWOBJ+1))
      END IF
   ELSE
!       TELECENTRIC AIMING IS ON
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
   END IF
!
   outer_retry: do
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
         IF(sys_ray_aiming().EQ.0.0D0) THEN
!     NO RAY AIMING
            IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
!     RAY AIMING IS OFF, TELECENTRIC RAY AIMING IS OFF
               X1AIM=WW2*JKX
               X1AIM=(X1AIM)-ALENS(A_XDEC,NEWOBJ+1)
               Y1AIM=WW1*JKY
               Y1AIM=(Y1AIM)-ALENS(A_YDEC,NEWOBJ+1)
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
            X1AIM=(X1AIM)-ALENS(A_XDEC,NEWOBJ+1)
            Y1AIM=(LFOB(1)*DABS(PXTRAY(5,NEWOBJ+1)))+(WW1*JKY)
            Y1AIM=(Y1AIM)-ALENS(A_YDEC,NEWOBJ+1)
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
   newton_raphson: do
   IF(ALENS(A_THI,NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
   IF(ALENS(A_THI,NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
   RV=.FALSE.
   KKK=KKK+1
!       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
!       AND STOP SEARCHING.
   IF(KKK.GT.NRAITR) THEN
      IF(MSG) THEN
         CALL RAY_FAILURE(NEWREF)
         OUTLYNE='RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
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
      MAG=DSQRT(((XSTRT-X1AIM)**2)+((YSTRT-Y1AIM)**2)+((ZSTRT-Z1AIM)**2))


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
      IF(WW1.GT.89.9999D0.AND.WW1.LT.90.0001D0)  WW1=89.9999D0
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
      X1AIM=WW2WW*ALENS(A_THI,0)
      Y1AIM=WW1WW*ALENS(A_THI,0)
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
   surface_loop: do I=(NEWOBJ+1),ISYS20

!
!
!       WE ARE CURRENTLY AT SURFACE I-1 AND KNOW WHERE
!       WE ARE AND WHICH DIRECTION WE WANT TO GO.
!       CALLING TRNSF2.FOR TRANSFORMS THE X,Y,Z,L,M AND N INTO
!       THE COORDINATE SYSTEM OF SURFACE I,FOB0 TRACKS IF FOB 0 IS
!       USED OR FOB (SOMETHING NON-ZERO)
      CALL TRNSF2_ARGS(I, X, Y, Z, L, M, N)
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
         RAYRAY(8,I)=DSQRT(((RAYRAY(3,I)-ZOLD)**2)+((RAYRAY(2,I)-YOLD)**2)+((RAYRAY(1,I)-XOLD)**2))
         ! TODO:  This calc is > abs thickness
         ! print out RAYRAY 1..3, I and X/Y/ZOLD
         ! and compare to cangle calcs in adjustlastsurface
!         do jj=1,3
!         call LogTermFOR("RAYRAY "//int2str(jj) //
!      1  " "//real2str(RAYRAY(jj,I)))
!         end do
         ! call LogTermFOR("XOLD is "//real2str(XOLD))
         ! call LogTermFOR("YOLD is "//real2str(YOLD))
         ! call LogTermFOR("ZOLD is "//real2str(ZOLD))

         ! call LogTermFOR('RAYTRA2 dlen is '//real2str(RAYRAY(8,I)))
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
         RAYRAY(8,I)=-(ALENS(A_IDEAL_FL,I-1)-ALENS(A_THI,I-1))*RAYRAY(6,I-1)
      END IF
      IF(INT(WW3).GE.1.AND.INT(WW3).LE.5)RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(A_N_OFFSET+INT(WW3),(I-1)))
      IF(INT(WW3).GE.6.AND.INT(WW3).LE.10)RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(A_N2_OFFSET+INT(WW3),(I-1)))
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
!         if(I==ISYS20) THEN
!         call LogTermFOR("RAYRAY(7,I) is "//real2str(RAYRAY(7,I)))
!         call LogTermFOR("RAYRAY(8,I) is "//real2str(RAYRAY(8,I)))
!         call LogTermFOR("DABS is "//
!      1  real2str(DABS(ALENS(A_N_OFFSET+INT(WW3),(I-1)))))
!         END IF
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
!       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
!       COORDINATES AND LOAD THEM IN ARRAY GLRAY
         IF(GLOBE) THEN
            CALL GLBRAY
         END IF
         IF(GRASET) THEN
!     WE ARE PLANNING TO PLOT THE RAY
!     SHUT OFF GLOBAL IF IT IS ON AND RE-ASSIGN THE GLSURF
            IF(GLOBE) THEN
               OUTLYNE='GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION'
               CALL SHOWIT(1)
               OUTLYNE='FOR RAY PLOTTING'
               CALL SHOWIT(1)
            END IF
            GLSURF=-99
            DO IK=0,NEWIMG
               IF(DABS(ALENS(A_THI,IK)).LE.1.0D10) THEN
                  GLSURF=IK
                  GO TO 8761
               END IF
            END DO
8761        CONTINUE
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
!       CALCULATE TARX AND TARY — the target point on the reference surface
!       that the ray is being aimed at.
         call compute_aim_target(I, WW1, WW2, TARX, TARY)
!
         TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
         !call logger%logTextWithNum("RAYTRA AIMTOL CHECK IS ", TEST)
         IF(TEST.LE.AIMTOL.OR.sys_ray_aiming().EQ.0.0D0.OR.ITRACE) THEN
!       AIM IS GOOD ENOUGH, PROCEED
            AIMOK=.TRUE.
            CYCLE surface_loop
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
            IF(ALENS(A_CURV,1).NE.0.0D0)CALL GETZEE1
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
            CYCLE newton_raphson
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



         CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
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
            CYCLE newton_raphson
         END IF
!       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
      END IF
!
   end do surface_loop
   EXIT outer_retry
   end do newton_raphson
   end do outer_retry
!       CACOCH IS THE FLAG WHICH TELLS WHETHER OR NOT
!       TO CHECK FOR CLAP/COBS INTERFERENCE.
!       IF CACOCH=0 DO NOT CHECK
!       IF CACOCH=1 DO THE CHECK
!       HERE WE CHECK FOR BLOCKAGE BY CLAP OR COBS.
!       AND DO THE APPROPRIATE THINGS.
   IF(CACOCH.EQ.1) THEN
      ray_blocked = .false.
      cacoch_loop: do R_I=NEWOBJ+1,NEWIMG-1
!       CALL CLAP CHECKING ROUTINE
         R_X=RAYRAY(1,R_I)
         R_Y=RAYRAY(2,R_I)
         R_Z=RAYRAY(3,R_I)
!     DON'T CHECK ON OBJECT OR IMAGE SURFACES.
!
         MMSG=MSG
         IF(DABS(ALENS(A_SURFTYPE,R_I)).NE.24.0D0) THEN
!     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
            IF(INT(ALENS(A_MULTICLAP,R_I)).EQ.0.AND.INT(ALENS(A_MULTICOBS,R_I)).EQ.0) THEN
               CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
            ELSE
               IF(INT(ALENS(A_MULTICLAP,R_I)).NE.0) THEN
                  DO JK=1,INT(ALENS(A_MULTICLAP,R_I))
                     IF(MMSG) THEN
                        MSG=.TRUE.
                        IF(JK.LT.INT(ALENS(A_MULTICLAP,R_I))) MSG=.FALSE.
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
               IF(INT(ALENS(A_MULTICOBS,R_I)).NE.0) THEN
                  DO JK=1,INT(ALENS(A_MULTICOBS,R_I))
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
!
!       THIS ROUTINE SETS THE FLAGS
!       CAERAS AND COERAS
!       SET IF THE CURRENT SURFACE HAD A COBS OR
!       CLAP ERASE.
         IF(STOPP.EQ.1) THEN
            ray_blocked = .true.
            EXIT cacoch_loop
         END IF
         STOPP=0
         FAIL=.FALSE.
         RAYEXT=.TRUE.
!       CONTINUE THE RAYTRACE
      end do cacoch_loop

   ELSE
!       NO CHECK TO BE MADE FOR CLAPS/COBS BLOCKAGE
   END IF

   IF(ray_blocked) THEN
   FAIL=.TRUE.
   RAYEXT=.FALSE.
   POLEXT=.FALSE.
   END IF
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
         OUTLYNE='GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION'
         CALL SHOWIT(1)
         OUTLYNE='FOR RAY PLOTTING'
         CALL SHOWIT(1)
      END IF
      GLSURF=-99
      glsurf_search: do I=0,NEWIMG
         IF(DABS(ALENS(A_THI,I)).LE.1.0D10) THEN
            GLSURF=I
            EXIT glsurf_search
         END IF
      end do glsurf_search

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
   call compute_ray_energy
   RETURN
END
! -----------------------------------------------------------------------
! compute_ray_energy — accumulate energy/transmission factors across surfaces
!
! Loops NEWOBJ..NEWIMG and fills RAYRAY(25,:) (energy) and
! RAYRAY(34:40,:) (polarisation factors) for the most recently traced ray.
! Handles: initial energy seed (WW4 or REFEXT), refractive index lookup,
! apodization grid (surface type 19), screen vignetting, polarisation angle,
! coating ENERGY_ADJUST, and grating DIFFRACTION_EFFICIENCY.
!
! All ray/system data are accessed through the DATLEN and DATMAI modules;
! no arguments are needed.  On grid-file error the routine sets STOPP=1
! and returns early — the caller must check STOPP before proceeding.
! -----------------------------------------------------------------------
subroutine compute_ray_energy
   use GLOBALS
   use DATLEN
   use mod_surface
   use DATMAI, only: PII, OUTLYNE
   use surface_params, only: A_COATING, A_GRATING, A_GRAT_SPACE, &
      & A_N2_OFFSET, A_N_OFFSET, A_SURFTYPE
   use mod_system, only: sys_screen, sys_screen_d, sys_screen_excl_angle, &
      sys_screen_h, sys_screen_s, sys_screen_surf
   implicit none
   integer  :: I, J, ISURF, IPASS1, WA3
   real(8)  :: RN1, RN2, POLANG, FACT_PAR, FACT_PER, PHASE_PAR, PHASE_PER
   real(8)  :: IA, IAP, PATHL, ENERGY_FACTOR, AOI, D, H, S, FACTOR, DP, MAG
   real(8)  :: JK_L1, JK_M1, JK_N1, JK_L2, JK_M2, JK_N2
   real(8)  :: JK_CPL, JK_CPM, JK_CPN, SA_CPL, SA_CPM, SA_CPN
   real(8)  :: XPASS, YPASS, ZPASS
   logical  :: GERROR
   COMMON/SAGPAS/XPASS,YPASS,ZPASS

   WA3       = INT(WW3)
   FACT_PAR  = 0.0D0
   FACT_PER  = 0.0D0
   PHASE_PAR = 0.0D0
   PHASE_PER = 0.0D0
   POLANG    = 0.0D0

   RAYRAY(25,NEWOBJ:NEWIMG)    = 0.0D0
   RAYRAY(34:38,NEWOBJ:NEWIMG) = 0.0D0

   DO I=NEWOBJ,NEWIMG
      ! --- seed energy -------------------------------------------------
      IF(I.EQ.NEWOBJ) THEN
         IF(REFEXT) THEN
            RAYRAY(25,I)=WW4*REFRY(9,NEWOBJ)
         ELSE
            RAYRAY(25,I)=WW4
         END IF
      ELSE
         RAYRAY(25,I)=RAYRAY(25,I-1)
      END IF
      ! --- refractive indices either side of this surface --------------
      IF(I.EQ.NEWOBJ) THEN
         IF(WA3.GE.1.AND.WA3.LE.5) THEN
            RN1=(ALENS(A_N_OFFSET+WA3,I))
            RN2=(ALENS(A_N_OFFSET+WA3,I))
         END IF
         IF(WA3.GE.6.AND.WA3.LE.10) THEN
            RN1=(ALENS(A_N2_OFFSET+WA3,I))
            RN2=(ALENS(A_N2_OFFSET+WA3,I))
         END IF
      ELSE
         IF(WA3.GE.1.AND.WA3.LE.5) THEN
            RN1=(ALENS(A_N_OFFSET+WA3,I-1))
            RN2=(ALENS(A_N_OFFSET+WA3,I))
         END IF
         IF(WA3.GE.6.AND.WA3.LE.10) THEN
            RN1=(ALENS(A_N2_OFFSET+WA3,I-1))
            RN2=(ALENS(A_N2_OFFSET+WA3,I))
         END IF
      END IF
      ! --- apodization grid surface (type 19) --------------------------
      IF(ALENS(A_SURFTYPE,I).EQ.19.0D0) THEN
         ISURF=I
         GERROR=.FALSE.
         XPASS=RAYRAY(1,I)
         YPASS=RAYRAY(2,I)
         IPASS1=2
         CALL GRIDS(2,ISURF,GERROR)
         IF(.NOT.GERROR) GRIDSUNLOADED19(I)=.FALSE.
         IF(GERROR) THEN
            IF(MSG) THEN
               CALL RAY_FAILURE(I)
               OUTLYNE='GRID FILE DOES NOT EXIST FOR THIS SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
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
      ! --- screen surface vignetting -----------------------------------
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
      ! --- non-apodization surface: polarisation + coatings + grating --
      IF(ALENS(A_SURFTYPE,I).NE.19.0D0) THEN
         IF(DUM(I).AND.I.GT.0) THEN
            RAYRAY(34:38,I)=0.0D0
         END IF
         ! Compute polarisation angle
         ! JK_CPL/M/N = unit vector normal to plane of incidence
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
         ! SA_CPL/M/N = incident direction unit vector (Z parallel to RYN)
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
         ! Coating transmission
         IF(COATSET) THEN
            J=INT(ALENS(A_COATING,I))
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
         ! Grating diffraction efficiency
         IF(ALENS(A_GRATING,I).EQ.1.0D0.AND.ALENS(A_GRAT_SPACE,I).NE.0.0D0) THEN
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
end subroutine compute_ray_energy

! -----------------------------------------------------------------------
! compute_aim_target — compute the XY target point on the reference surface
!
! Given a reference surface index and fractional field heights (ww1, ww2),
! returns the target coordinates (tarx, tary) in the local coordinate
! system of that surface. The ray aiming iteration drives the ray to hit
! this point within AIMTOL.
!
! Logic:
!   If the surface has a clear aperture (types 1-6) and no multi-clap:
!     - If the aperture is decentered/tilted (clapt=.true.):
!         scale by aperture dims, apply decentration, then rotate by clap tilt
!     - Otherwise:
!         scale by aperture dims, rotate by reference surface orientation angle
!   Otherwise (no aperture, or multi-clap present):
!     use paraxial ray heights (PXTRAX/PXTRAY) scaled by field heights,
!     rotated by reference surface orientation angle
! -----------------------------------------------------------------------
subroutine compute_aim_target(ref_surf, ww1_in, ww2_in, tarx, tary)
   use DATLEN
   use mod_surface
   use DATMAI, only: PII
   use surface_params, only: AP_CIRC, AP_RECT, AP_ELLIP, AP_RCTK, AP_IPOLY, AP_POLY, &
      & A_CLAP_YD, A_CLAP_XD, A_CLAP_TILT, A_CURV, A_APTYPE, A_MULTICLAP, &
      & A_CLAP_P1, A_CLAP_P2, A_CLAP_P3, SYS_FLIPREFX, SYS_FLIPREFY
   use mod_system, only: sys_aplanatic_aim, sys_ref_orient
   implicit none

   integer,  intent(in)  :: ref_surf  ! reference surface index
   real(8),  intent(in)  :: ww1_in    ! fractional Y field height
   real(8),  intent(in)  :: ww2_in    ! fractional X field height
   real(8),  intent(out) :: tarx      ! target X on reference surface
   real(8),  intent(out) :: tary      ! target Y on reference surface

   real(8) :: www1, www2   ! possibly aplanatic-adjusted field heights
   real(8) :: yval, xval   ! aperture dimensions for current type
   real(8) :: gamma        ! aperture or surface rotation angle (radians)
   real(8) :: tarrx, tarry ! rotated temporaries
   logical :: clapt        ! .true. if aperture is decentered or tilted

   www1 = ww1_in
   www2 = ww2_in

   if (dabs(ALENS(A_APTYPE,ref_surf)) >= 1.0d0 .and. dabs(ALENS(A_APTYPE,ref_surf)) <= 6.0d0 .and. ALENS(A_MULTICLAP,ref_surf) == 0.0d0) then

      ! Surface has a single clear aperture — determine if it is decentered/tilted
      clapt = (ALENS(A_CLAP_YD,ref_surf)   /= 0.0d0 .or. ALENS(A_CLAP_XD,ref_surf)   /= 0.0d0 .or. ALENS(A_CLAP_TILT,ref_surf) /= 0.0d0)

      ! Aplanatic aiming adjustment for centred circular apertures
      if (sys_aplanatic_aim() == 1.0d0             .and. ALENS(A_CURV,ref_surf)      /= 0.0d0        .and. ALENS(A_APTYPE,ref_surf)    == 1.0d0        .and. ALENS(A_CLAP_YD,ref_surf)   == 0.0d0        .and. ALENS(A_CLAP_XD,ref_surf)   == 0.0d0        .and. ALENS(A_CLAP_TILT,ref_surf) == 0.0d0) then
         if (dabs(1.0d0/ALENS(A_CURV,ref_surf)) >= dabs(ALENS(A_CLAP_P1,ref_surf)) .and. dabs(1.0d0/ALENS(A_CURV,ref_surf)) >= dabs(ALENS(A_CLAP_P2,ref_surf))) call APLANA(ref_surf, ww1_in, ww2_in, www1, www2)
      end if

      select case (int(dabs(ALENS(A_APTYPE,ref_surf))))

      case (AP_CIRC)  ! circular
         if (clapt) then
            if (ALENS(A_CLAP_P1,ref_surf) <= ALENS(A_CLAP_P2,ref_surf)) then
               tary = ALENS(A_CLAP_P1,ref_surf) * www1
               tarx = ALENS(A_CLAP_P1,ref_surf) * www2
            else
               tary = ALENS(A_CLAP_YD,ref_surf) + ALENS(A_CLAP_P2,ref_surf) * www1
               tarx = ALENS(A_CLAP_XD,ref_surf) + ALENS(A_CLAP_P2,ref_surf) * www2
            end if
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            tarx = tarx + ALENS(A_CLAP_XD,ref_surf)
            tary = tary + ALENS(A_CLAP_YD,ref_surf)
            gamma = (ALENS(A_CLAP_TILT,ref_surf) * PII) / 180.0d0
         else
            if (ALENS(A_CLAP_P1,ref_surf) <= ALENS(A_CLAP_P2,ref_surf)) then
               tary = ALENS(A_CLAP_P1,ref_surf) * www1
               tarx = ALENS(A_CLAP_P1,ref_surf) * www2
            else
               tary = ALENS(A_CLAP_P2,ref_surf) * www1
               tarx = ALENS(A_CLAP_P2,ref_surf) * www2
            end if
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_RECT)  ! rectangular
         if (clapt) then
            if (ANAAIM) then
               tary = ALENS(A_CLAP_P1,ref_surf) * ww1_in
               tarx = ALENS(A_CLAP_P2,ref_surf) * ww2_in
            else if (dabs(ALENS(A_CLAP_P1,ref_surf)) > dabs(ALENS(A_CLAP_P2,ref_surf))) then
               tary = ALENS(A_CLAP_P1,ref_surf) * ww1_in
               tarx = ALENS(A_CLAP_P1,ref_surf) * ww2_in
            else
               tary = ALENS(A_CLAP_YD,ref_surf) + ALENS(A_CLAP_P2,ref_surf) * ww1_in
               tarx = ALENS(A_CLAP_XD,ref_surf) + ALENS(A_CLAP_P2,ref_surf) * ww2_in
            end if
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            tarx = tarx + ALENS(A_CLAP_XD,ref_surf)
            tary = tary + ALENS(A_CLAP_YD,ref_surf)
            gamma = (ALENS(A_CLAP_TILT,ref_surf) * PII) / 180.0d0
         else
            tary = ALENS(A_CLAP_P1,ref_surf) * ww1_in
            tarx = ALENS(A_CLAP_P2,ref_surf) * ww2_in
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_ELLIP)  ! elliptical
         yval = ALENS(A_CLAP_P1,ref_surf)
         xval = ALENS(A_CLAP_P2,ref_surf)
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + ALENS(A_CLAP_XD,ref_surf)
            tary = tary + ALENS(A_CLAP_YD,ref_surf)
            gamma = (ALENS(A_CLAP_TILT,ref_surf) * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_RCTK)  ! racetrack
         yval = ALENS(A_CLAP_P1,ref_surf)
         xval = ALENS(A_CLAP_P2,ref_surf)
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + ALENS(A_CLAP_XD,ref_surf)
            tary = tary + ALENS(A_CLAP_YD,ref_surf)
            gamma = (ALENS(A_CLAP_TILT,ref_surf) * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_POLY)  ! regular polygon — radius to corner is A_CLAP_P1
         yval = ALENS(A_CLAP_P1,ref_surf)
         xval = ALENS(A_CLAP_P1,ref_surf)
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + ALENS(A_CLAP_XD,ref_surf)
            tary = tary + ALENS(A_CLAP_YD,ref_surf)
            gamma = (ALENS(A_CLAP_TILT,ref_surf) * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_IPOLY)  ! irregular polygon — max dim: A_CLAP_P3 (decentered) or A_CLAP_P2 (centred)
         if (clapt) then
            yval = ALENS(A_CLAP_P3,ref_surf)
            xval = ALENS(A_CLAP_P3,ref_surf)
         else
            yval = ALENS(A_CLAP_P2,ref_surf)
            xval = ALENS(A_CLAP_P2,ref_surf)
         end if
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + ALENS(A_CLAP_XD,ref_surf)
            tary = tary + ALENS(A_CLAP_YD,ref_surf)
            gamma = (ALENS(A_CLAP_TILT,ref_surf) * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case default
         tarx = 0.0d0
         tary = 0.0d0
         gamma = 0.0d0
      end select

      ! Apply rotation (either clap tilt or reference surface orientation)
      tarrx = (tarx * dcos(gamma)) - (tary * dsin(gamma))
      tarry = (tarx * dsin(gamma)) + (tary * dcos(gamma))
      tarx = tarrx
      tary = tarry

   else
      ! No clear aperture on reference surface, or multi-clap present —
      ! aim to the paraxial ray height scaled by fractional field
      tary = PXTRAY(1,ref_surf) * ww1_in
      tarx = PXTRAX(1,ref_surf) * ww2_in
      if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
      if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
      gamma = (sys_ref_orient() * PII) / 180.0d0
      tarrx = (tarx * dcos(gamma)) - (tary * dsin(gamma))
      tarry = (tarx * dsin(gamma)) + (tary * dcos(gamma))
      tarx = tarrx
      tary = tarry
   end if

end subroutine compute_aim_target

! SUB RAYTRA2.FOR

SUBROUTINE RAYTRA2
   USE GLOBALS
   use mod_lens_data_manager
   use real_ray_trace
   use mod_system, only: sys_aplanatic_aim, sys_fliprefx, sys_fliprefy, &
      & sys_ray_aiming, sys_ref_orient, &
      & sys_scx, sys_scy, sys_screen, sys_screen_d, sys_screen_excl_angle, &
      & sys_screen_h, sys_screen_s, sys_screen_surf, sys_telecentric
!
   use DATLEN
   use mod_surface
   use DATMAI
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE RAYTRA2.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE TRACING OF A RAY IN OPTIMIZATION AND TOLERANCING
!
   INTEGER IPASS1,JK,I,ISYS20,KKK,J,ISURF,IK,WA3

   INTEGER CAERAS,COERAS,N_HITS
!
   REAL*8 X,Y,Z,L,M,N,WW1W,WW2W,LER,MER,NER,TANN1,TANN2,IA,IAP,D21,D22,GAMMA,XXX,YYY,TWW1,TWW2,XL,XM,XN,YL,YM,YN,RN1,RN2,WWW1,WWW2,D11,D12,LS,SNINDX,SNIND2,WW1WW,WW2WW,JK1,JK2,JK3,LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,ENERGY_FACTOR,Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,XC1,YC1,ZC1,MF1,MF2,DET,TARX,TARY,XVALUE,YVALUE,TEST,MAG,VALUE,LOLD,MOLD,NOLD,TARRY,TARRX,JKX,JKY,STEPL,STEPL1,JK_L1,JK_M1,JK_N1,JK_L2,JK_M2,JK_N2,JK_CPL,JK_CPM,JK_CPN,SA_CPL,SA_CPM,SA_CPN,DP,POLANG,FACT_PAR,FACT_PER,PHASE_PAR,PHASE_PER,PATHL
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
!
   REAL*8 AOI,D,H,S,FACTOR
!

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
   IF(sys_telecentric().EQ.0.0D0) THEN
!       TELECENTRIC AIMING IS OFF
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
      IF(surf_clap_type(NEWOBJ+1) == 1) THEN
!     CIRCULAR AP
         IF(surf_clap_dim(NEWOBJ+1, 1).LE.surf_clap_dim(NEWOBJ+1, 2)) THEN
            IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 1))
            IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 1))
         ELSE
            IF(DABS(surf_clap_dim(NEWOBJ+1, 2)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 2))
            IF(DABS(surf_clap_dim(NEWOBJ+1, 2)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 2))
         END IF
      END IF
      IF(surf_clap_type(NEWOBJ+1) == 5) THEN
!     CIRCULAR AP
         IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 1))
         IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 1))
      END IF
      IF(surf_clap_type(NEWOBJ+1) == 6) THEN
!     CIRCULAR AP
         IF(DABS(surf_clap_dim(NEWOBJ+1, 5)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 5))
         IF(DABS(surf_clap_dim(NEWOBJ+1, 5)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 5))
      END IF
      IF(surf_clap_type(NEWOBJ+1).GT.1.0D0.AND.surf_clap_type(NEWOBJ+1).LE.4.0D0) THEN
!     OTHER AP
         IF(DABS(surf_clap_dim(NEWOBJ+1, 2)).LT.DABS(PXTRAX(1,NEWOBJ+1)))JKX=DABS(surf_clap_dim(NEWOBJ+1, 2))
         IF(DABS(surf_clap_dim(NEWOBJ+1, 1)).LT.DABS(PXTRAY(1,NEWOBJ+1)))JKY=DABS(surf_clap_dim(NEWOBJ+1, 1))
      END IF
   ELSE
!       TELECENTRIC AIMING IS ON
      JKX=(PXTRAX(1,NEWOBJ+1))
      JKY=(PXTRAY(1,NEWOBJ+1))
   END IF
!
989 CONTINUE
   IF(RAYCOD(1).EQ.1.AND.KKK.GT.1) THEN
      STOPP=1
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
      FAIL=.TRUE.
      IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
         CALL MACFAL
      END IF
      RETURN
   END IF
   IF(.NOT.ITRACE) THEN
      IF(NULL.AND..NOT.REFEXT) THEN
!     NULL WITH FAILED CHIEF RAY
         IF(sys_ray_aiming().EQ.0.0D0) THEN
!     NO RAY AIMING
            IF(sys_telecentric().EQ.0.0D0) THEN
!     TEL OFF
!     RAY AIMING IS OFF, TELECENTRIC RAY AIMING IS OFF
               X1AIM=WW2*JKX
               X1AIM=(X1AIM)-surf_decenter_x(NEWOBJ+1)
               Y1AIM=WW1*JKY
               Y1AIM=(Y1AIM)-surf_decenter_y(NEWOBJ+1)
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
            X1AIM=(X1AIM)-surf_decenter_x(NEWOBJ+1)
            Y1AIM=(LFOB(1)*DABS(PXTRAY(5,NEWOBJ+1)))+(WW1*JKY)
            Y1AIM=(Y1AIM)-surf_decenter_y(NEWOBJ+1)
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
9  CONTINUE
   IF(surf_thickness(NEWOBJ).LT.0.0D0) REVSTR=.TRUE.
   IF(surf_thickness(NEWOBJ).GE.0.0D0) REVSTR=.FALSE.
   RV=.FALSE.
   KKK=KKK+1
!       IF KKK EXCEEDS 100 TRIES, PRINT RAY ITERRATION ERROR
!       AND STOP SEARCHING.
   IF(KKK.GT.NRAITR) THEN
      RAYCOD(1)=3
      RAYCOD(2)=NEWREF
      SPDCD1=RAYCOD(1)
      SPDCD2=NEWREF
      STOPP=1
      RAYEXT=.FALSE.
      POLEXT=.FALSE.
      FAIL=.TRUE.
      IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
         CALL MACFAL
      END IF
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
   RAYRAY(36:38,NEWOBJ)=0.0D0
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
      CALL TRNSF2_ARGS(I, X, Y, Z, L, M, N)
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
         RAYRAY(8,I)=-(surf_ideal_efl(I-1)-surf_thickness(I-1))*RAYRAY(6,I-1)
      END IF
      IF(INT(WW3).GE.1.AND.INT(WW3).LE.5)RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(45+INT(WW3),(I-1)))
      IF(INT(WW3).GE.6.AND.INT(WW3).LE.10)RAYRAY(7,I)=RAYRAY(8,I)*DABS(ALENS(65+INT(WW3),(I-1)))
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
         call adjustLastSurface(I,RAYRAY)
      end if
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
         IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
            CALL MACFAL
         END IF
!       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
!       COORDINATES AND LOAD THEM IN ARRAY GLRAY
         IF(GLOBE) THEN
            CALL GLBRAY
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
         IF(ABS(surf_clap_type(I)) >= 1.AND.ABS(surf_clap_type(I)) <= 6.AND.surf_multi_clap_flag(I) == 0) THEN
!     CLAPT=TRUE FOR CLAP TILTS AND DECENTED AND FALSE OTHERWISE
            CLAPT=.FALSE.
            IF(surf_clap_dim(I, 3).NE.0.0D0.OR.surf_clap_dim(I, 4).NE.0.0D0.OR.surf_clap_tilt(I).NE.0.0D0) CLAPT=.TRUE.
!       REF SURF HAS CLAP ON IT
!       THE VALUES OF TARY AND TARX ARE COORDINATES IN THE
!       LOCAL COORDINATE SYSTEM OF THE REFERENCE SYSTEM.
!       IF THE CLAP IS DECENTERED, THE TAR() VALUES ARE
!       MODIFIED BY ADDING THE CLAP DECENTRATIONS SINCE THE RAY
!       IS AIMED TO THE RELATIVE REFERENCE SURFACE COORDINATES.
!
!       SET TARGET TO CENTER OF DECENTERED CLAP, surf_clap_dim(I, 3),
!       AND surf_clap_dim(I, 4) ARE CLAP DECENTRATIONS
!
            WWW1=WW1
            WWW2=WW2
            IF(sys_aplanatic_aim().EQ.1.0D0.AND.surf_curvature(I).NE.0.0D0.AND.surf_clap_type(I) == 1.AND.surf_clap_dim(I, 3).EQ.0.0D0.AND.surf_clap_dim(I, 4).EQ.0.0D0.AND.surf_clap_tilt(I).EQ.0.0D0) THEN
               IF(DABS(1.0D0/surf_curvature(I)).GE.DABS(surf_clap_dim(I, 1)).AND.DABS(1.0D0/surf_curvature(I)).GE.DABS(surf_clap_dim(I, 2)))CALL APLANA(I,WW1,WW2,WWW1,WWW2)
            END IF
!
!       CIRCULAR CLAP
!
            IF(ABS(surf_clap_type(I)) == 1) THEN
               IF(CLAPT) THEN
                  IF(surf_clap_dim(I, 1).LE.surf_clap_dim(I, 2)) THEN
                     TARY=(surf_clap_dim(I, 1)*WWW1)
                     TARX=(surf_clap_dim(I, 1)*WWW2)
                  ELSE
                     TARY=(surf_clap_dim(I, 2)*WWW1)
                     TARX=(surf_clap_dim(I, 2)*WWW2)
                  END IF
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
!     NO CLAP DEC OR TILTS
                  TARY=(surf_clap_dim(I, 1)*WWW1)
                  TARX=(surf_clap_dim(I, 1)*WWW2)
                  IF(surf_clap_dim(I, 1).LE.surf_clap_dim(I, 2)) THEN
                     TARY=(surf_clap_dim(I, 1)*WWW1)
                     TARX=(surf_clap_dim(I, 1)*WWW2)
                  ELSE
                     TARY=(surf_clap_dim(I, 2)*WWW1)
                     TARX=(surf_clap_dim(I, 2)*WWW2)
                  END IF
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
!       NOW IS THE REF SURF ORIENTATION ANGLE ?
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT CIRCULAR CLAP
            END IF
!        RECT CLAP
!
            IF(ABS(surf_clap_type(I)) == 2) THEN
               IF(CLAPT) THEN
                  IF(ANAAIM) THEN
                     TARY=(surf_clap_dim(I, 1)*WW1)
                     TARX=(surf_clap_dim(I, 2)*WW2)
                  ELSE
                     IF(DABS(surf_clap_dim(I, 1)).GT.DABS(surf_clap_dim(I, 2))) THEN
                        TARY=(surf_clap_dim(I, 1)*WW1)
                        TARX=(surf_clap_dim(I, 1)*WW2)
                     ELSE
                        TARY=(surf_clap_dim(I, 2)*WW1)
                        TARX=(surf_clap_dim(I, 2)*WW2)
                     END IF
                  END IF
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  TARY=(surf_clap_dim(I, 1)*WW1)
                  TARX=(surf_clap_dim(I, 2)*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT RECT CLAP
            END IF
!        ELIP CLAP
!
            YVALUE=surf_clap_dim(I, 1)
            XVALUE=surf_clap_dim(I, 2)
!
            IF(ABS(surf_clap_type(I)) == 3) THEN
               IF(CLAPT) THEN
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT ELIP CLAP
            END IF
!        RCTK CLAP
!
            IF(ABS(surf_clap_type(I)) == 4) THEN
               IF(CLAPT) THEN
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 2)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 2)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT RCTK CLAP
            END IF
!        POLY CLAP
!
            IF(ABS(surf_clap_type(I)) == 5) THEN
               IF(CLAPT) THEN
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 1)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  YVALUE=surf_clap_dim(I, 1)
                  XVALUE=surf_clap_dim(I, 1)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               END IF
!       NOT POLY CLAP
            END IF
            IF(ABS(surf_clap_type(I)) == 6) THEN
               IF(CLAPT) THEN
                  YVALUE=surf_clap_dim(I, 5)
                  XVALUE=surf_clap_dim(I, 5)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  TARX=TARX+surf_clap_dim(I, 4)
                  TARY=TARY+surf_clap_dim(I, 3)
!       NOW IS THE CLAP TILTED ?
                  GAMMA=(surf_clap_tilt(I)*(PII))/180.0D0
                  TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
                  TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
                  TARY=TARRY
                  TARX=TARRX
               ELSE
                  YVALUE=surf_clap_dim(I, 2)
                  XVALUE=surf_clap_dim(I, 2)
                  TARY=(YVALUE*WW1)
                  TARX=(XVALUE*WW2)
                  IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
                  IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
                  GAMMA=(sys_ref_orient()*(PII))/180.0D0
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
            IF(sys_fliprefx().NE.0.0D0) TARX=-TARX
            IF(sys_fliprefy().NE.0.0D0) TARY=-TARY
            GAMMA=(sys_ref_orient()*(PII))/180.0D0
            TARRX=((TARX*DCOS(GAMMA))-(TARY*DSIN(GAMMA)))
            TARRY=((TARX*DSIN(GAMMA))+(TARY*DCOS(GAMMA)))
            TARY=TARRY
            TARX=TARRX
         END IF
!
         TEST=DSQRT(((TARX-X)**2)+((TARY-Y)**2))
         IF(TEST.LE.AIMTOL.OR.sys_ray_aiming().EQ.0.0D0.OR.ITRACE) THEN
!       AIM IS GOOD ENOUGH, PROCEED
            AIMOK=.TRUE.
            REFMISS=.FALSE.
            CALL MISSREF(X,Y)
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
            IF(surf_curvature(1).NE.0.0D0)CALL GETZEE1
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
         CALL RAYDERIV(X1LAST,Y1LAST,X1ONE,Y1ONE,RXONE,RYONE,RXLAST,RYLAST,D11,D12,D21,D22)
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
         IF(DELFAIL) THEN
            RETURN
         ELSE
            GO TO 9
         END IF
!       NOT AT THE REFERENCE SURFACE, NO RAY AIMING NEEDED
      END IF
!
100   CONTINUE
!
10 CONTINUE
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
         IF(ABS(surf_special_type(R_I)) /= 24) THEN
!     NO CLAP/COBS CHECK FOR TYPE 24 SPECIAL SURFACE
            IF(INT(surf_multi_clap_flag(R_I)).EQ.0.AND.INT(surf_multi_cobs_flag(R_I)).EQ.0) THEN
               CALL CACHEK(0.0D0,0.0D0,0.0D0,0)
            ELSE
               IF(INT(surf_multi_clap_flag(R_I)).NE.0) THEN
                  DO JK=1,INT(surf_multi_clap_flag(R_I))
                     IF(MMSG) THEN
                        MSG=.TRUE.
                        IF(JK.LT.INT(surf_multi_clap_flag(R_I))) MSG=.FALSE.
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
               IF(INT(surf_multi_cobs_flag(R_I)).NE.0) THEN
                  DO JK=1,INT(surf_multi_cobs_flag(R_I))
                     IF(MMSG) THEN
                        MSG=.TRUE.
                     END IF
                     JK1=MULTCOBS(JK,1,R_I)
                     JK2=MULTCOBS(JK,2,R_I)
                     JK3=MULTCOBS(JK,3,R_I)
                     CALL CACHEK(JK1,JK2,JK3,2)
                     IF(RAYCOD(1).NE.0) THEN
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
90 CONTINUE
   FAIL=.TRUE.
   RAYEXT=.FALSE.
   POLEXT=.FALSE.
   IF(.NOT.SPDTRA.AND.F34.EQ.0.AND.F58.EQ.0) THEN
      CALL MACFAL
   END IF
91 CONTINUE
!
!     FINISHED CLAP/COBS CHEKING
!       IF GLOBAL IS TRUE, CALCULATE THE CURRENT RAY'S GLOBAL
!       COORDINATES AND LOAD THEM IN ARRAY GLRAY
   IF(GLOBE) THEN
      CALL GLBRAY
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
      IF(surf_special_type(I) == 19) THEN
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
               CALL RAY_FAILURE(I)
               OUTLYNE='GRID FILE DOES NOT EXIST FOR THIS SURFACE'
               CALL SHOWIT(1)
               OUTLYNE='OR IT HAS INSUFFICIENT DATA FOR THIS SURFACE'
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
      IF(sys_screen().EQ.1.0D0) THEN
!     SCREEN SURFACE
         IF(I.EQ.INT(sys_screen_surf())) THEN
!       GOT THE SCREEN SURFACE
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
!
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
!       THE UNIT VECTOR IN THE
!       INCIDENT DIRECTION IS:
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
   RETURN
END
