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
   use zoa_output, only: zoa_emit
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   character(len=140) :: emit_tmp
!
!       THIS IS SUBROUTINE QRRAY.FOR. THIS SUBROUTINE IMPLEMENTS
!       QUICK RAY TRACING AND CALLS QTRA1.FOR
!
   real(real64) OLDAIM,X,Y,AREA1,AREA2
!
   real(real64) SYS12,SYS13,SMALLAREA,BIGAREA
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
      call zoa_emit('"FOOT" CAUSES A FOOTPRINT RAY GRID TO BE TRACED', 'black')
      call zoa_emit('RE-ENTER COMMAND', 'black')
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
   call zoa_emit('BEAM FOOTPRINT DATA BEING GENERATED', 'black')
   write(emit_tmp,*)'CURRENT RAY GRID SIZE IS : ',(2*FOTLIM)+1,' x ',(2*FOTLIM)+1
   call zoa_emit(trim(emit_tmp), 'black')
   call zoa_emit('PLEASE WAIT...', 'black')
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
   use zoa_output, only: zoa_emit
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER FOTLIM
   character(len=140) :: emit_tmp
!
   COMMON/LIMMER/FOTLIM
!
!     CHECK INPUT
!
   IF(is_command_query()) THEN
      call zoa_emit('"FOOT GRID" CAUSES A FOOTPRINT GRID SIZE TO BE SET', 'black')
      write(emit_tmp,*)'CURRENT FOOT RAY GRID  IS : ',(2*FOTLIM+1),' x ',(2*FOTLIM)+1
      call zoa_emit(trim(emit_tmp), 'black')
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
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE QTRA1.FOR. THIS SUBROUTINE IMPLEMENTS
!       THE TRACING OF A QUICK RAY FOR VIGCAL.
!
   INTEGER JK,I,ISYS20,KKK,J

   INTEGER CAERAS,COERAS
!
   real(real64) X,Y,Z,L,M,N,XC1,YC1,ZC1,WWW1,WWW2,D21,D22,GAMMA,JKX,JKY,XL,XM,XN,YL,YM,YN,D11,D12,LS,SNINDX,SNIND2,JK1,JK2,JK3,LSTART,MSTART,NSTART,YANG,XANG,X1LAST,LARGE,Y1LAST,X1ONE,Y1ONE,RXLAST,RYLAST,RXONE,RYONE,DDELX,DDELY,MAG,MF1,MF2,DET,TARX,TARY,XVALUE,YVALUE,VALUE,LOLD,MOLD,NOLD,TARRY,TARRX
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

SUBROUTINE RAYTRA
   use real_ray_trace, only: real_ray_trace_core
   call real_ray_trace_core(.false.)
END SUBROUTINE
! SUB RAYTRA.FOR

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
   use DATMAI, only: PII
   use mod_lens_data_manager, only: ldm
   use mod_system, only: sys_screen, sys_screen_d, sys_screen_excl_angle, &
      sys_screen_h, sys_screen_s, sys_screen_surf
   use zoa_output, only: zoa_emit
   use iso_fortran_env, only: real64
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
         IF(WA3.GE.1.AND.WA3.LE.10) THEN
            RN1=ldm%getSurfIndex(I, WA3)
            RN2=ldm%getSurfIndex(I, WA3)
         END IF
      ELSE
         IF(WA3.GE.1.AND.WA3.LE.10) THEN
            RN1=ldm%getSurfIndex(I-1, WA3)
            RN2=ldm%getSurfIndex(I, WA3)
         END IF
      END IF
      ! --- apodization grid surface (type 19) --------------------------
      IF(ldm%getSurfSpecialType(I).EQ.19) THEN
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
      IF(ldm%getSurfSpecialType(I).NE.19) THEN
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
            J=surf_coating_index(I)
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
         IF(surf_diffraction_flag(I).EQ.1.AND.surf_grating_spacing(I).NE.0.0D0) THEN
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
      & SYS_FLIPREFX, SYS_FLIPREFY
   use mod_system, only: sys_aplanatic_aim, sys_ref_orient
   use clear_apertures, only: clear_aperture
   use iso_fortran_env, only: real64
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
   type(clear_aperture) :: cap

   www1 = ww1_in
   www2 = ww2_in

   call cap%from_alens(ref_surf)

   if (cap%shape >= 1 .and. cap%shape <= 6 .and. surf_multi_clap_flag(ref_surf) == 0) then

      ! Surface has a single clear aperture — determine if it is decentered/tilted
      clapt = (cap%decenter_y /= 0.0d0 .or. cap%decenter_x /= 0.0d0 .or. cap%tilt /= 0.0d0)

      ! Aplanatic aiming adjustment for centred circular apertures
      if (sys_aplanatic_aim() == 1.0d0             .and. surf_curvature(ref_surf)      /= 0.0d0        .and. cap%shape == 1        .and. cap%decenter_y == 0.0d0        .and. cap%decenter_x == 0.0d0        .and. cap%tilt == 0.0d0) then
         if (dabs(1.0d0/surf_curvature(ref_surf)) >= dabs(cap%dim1) .and. dabs(1.0d0/surf_curvature(ref_surf)) >= dabs(cap%dim2)) call APLANA(ref_surf, ww1_in, ww2_in, www1, www2)
      end if

      select case (cap%shape)

      case (AP_CIRC)  ! circular
         if (clapt) then
            if (cap%dim1 <= cap%dim2) then
               tary = cap%dim1 * www1
               tarx = cap%dim1 * www2
            else
               tary = cap%decenter_y + cap%dim2 * www1
               tarx = cap%decenter_x + cap%dim2 * www2
            end if
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            tarx = tarx + cap%decenter_x
            tary = tary + cap%decenter_y
            gamma = (cap%tilt * PII) / 180.0d0
         else
            if (cap%dim1 <= cap%dim2) then
               tary = cap%dim1 * www1
               tarx = cap%dim1 * www2
            else
               tary = cap%dim2 * www1
               tarx = cap%dim2 * www2
            end if
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_RECT)  ! rectangular
         if (clapt) then
            if (ANAAIM) then
               tary = cap%dim1 * ww1_in
               tarx = cap%dim2 * ww2_in
            else if (dabs(cap%dim1) > dabs(cap%dim2)) then
               tary = cap%dim1 * ww1_in
               tarx = cap%dim1 * ww2_in
            else
               tary = cap%decenter_y + cap%dim2 * ww1_in
               tarx = cap%decenter_x + cap%dim2 * ww2_in
            end if
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            tarx = tarx + cap%decenter_x
            tary = tary + cap%decenter_y
            gamma = (cap%tilt * PII) / 180.0d0
         else
            tary = cap%dim1 * ww1_in
            tarx = cap%dim2 * ww2_in
            if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
            if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_ELLIP)  ! elliptical
         yval = cap%dim1
         xval = cap%dim2
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + cap%decenter_x
            tary = tary + cap%decenter_y
            gamma = (cap%tilt * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_RCTK)  ! racetrack
         yval = cap%dim1
         xval = cap%dim2
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + cap%decenter_x
            tary = tary + cap%decenter_y
            gamma = (cap%tilt * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_POLY)  ! regular polygon — radius to corner is dim1
         yval = cap%dim1
         xval = cap%dim1
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + cap%decenter_x
            tary = tary + cap%decenter_y
            gamma = (cap%tilt * PII) / 180.0d0
         else
            gamma = (sys_ref_orient() * PII) / 180.0d0
         end if

      case (AP_IPOLY)  ! irregular polygon — max dim: dim5 (decentered) or dim2 (centred)
         if (clapt) then
            yval = cap%dim5
            xval = cap%dim5
         else
            yval = cap%dim2
            xval = cap%dim2
         end if
         tary = yval * ww1_in
         tarx = xval * ww2_in
         if (SYSTEM(SYS_FLIPREFX) /= 0.0d0) tarx = -tarx
         if (SYSTEM(SYS_FLIPREFY) /= 0.0d0) tary = -tary
         if (clapt) then
            tarx = tarx + cap%decenter_x
            tary = tary + cap%decenter_y
            gamma = (cap%tilt * PII) / 180.0d0
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
   use real_ray_trace, only: real_ray_trace_core
   call real_ray_trace_core(.true.)
END SUBROUTINE RAYTRA2

