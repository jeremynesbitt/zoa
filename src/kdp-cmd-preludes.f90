! The CMD-level preludes of FOB, RAY, SPD and CAPFN -- the state each
! command sets up around its legacy handler -- moved out of CMDER so the
! text path (CMDER) and the typed path (mod_kdp_api's kdp_fob / kdp_ray /
! kdp_spd / kdp_capfn) run exactly the same code.  Each reads the parsed
! command from the DATMAI globals, as CMDER did.

! FOB / FOBH: set the field (and trace the chief ray at it).
SUBROUTINE KDP_CMD_FOB
   USE GLOBALS
   use DATLEN
   use DATSP1
   use DATHGR
   use DATSPD
   use DATSUB
   use DATMAI
   use command_utils, only: is_command_query
   use mod_system, only: sys_last_surf, sys_ref_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   LOGICAL ERRFOB,EXIS51,OPEN51

!     NO LINE SPREAD FUNCTIONS MAY EXIST NOW
   RSTREHL_EXIST=.FALSE.
   LSF=.FALSE.
!     NO SPOTS EXIST
   SPDEXT=.FALSE.
   GSPDEXT=.FALSE.
   CPFNEXT=.FALSE.
   CALL DELPSF
!     IF NOT OPTIMIZATION OR TOLERANCING, MESSAGE ON
   IF(F28.EQ.0.AND.F31.EQ.0) MSG=.TRUE.
!     IF VIE, MSG OFF
   IF(F34.EQ.1) MSG=.FALSE.
   IF(F34.EQ.0) MSG=.TRUE.
   NEWOBJ=0
   NEWREF=INT(sys_ref_surf())
   NEWIMG=INT(sys_last_surf())
   IF(WC.EQ.'FOB     ') THEN
      CALL FFOB
   END IF
   IF(WC.EQ.'FOBH    ')CALL FFOBH
   IF(is_command_query()) RETURN
   IF(F34.EQ.1) MSG=.FALSE.
   IF(F34.EQ.0) MSG=.TRUE.
   IF(.NOT.REFEXT) RETURN
   EXIS51=.FALSE.
   INQUIRE(FILE='PSF.DAT',EXIST=EXIS51)
   IF(EXIS51) THEN
      OPEN51=.FALSE.
      INQUIRE(FILE='PSF.DAT',OPENED=OPEN51)
      IF(.NOT.OPEN51) THEN
!     OPEN FILE
         OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'&
         &,FORM='FORMATTED',FILE='PSF.DAT'&
         &,STATUS='UNKNOWN')
      END IF
      CALL CLOSE_FILE(51,0)
   END IF
   EXIS51=.FALSE.
   INQUIRE(FILE='SPSF.DAT',EXIST=EXIS51)
   IF(EXIS51) THEN
      OPEN51=.FALSE.
      INQUIRE(FILE='SPSF.DAT',OPENED=OPEN51)
      IF(.NOT.OPEN51) THEN
!     OPEN FILE
         OPEN(UNIT=51,ACCESS='SEQUENTIAL',BLANK='NULL'&
         &,FORM='FORMATTED',FILE='SPSF.DAT'&
         &,STATUS='UNKNOWN')
      END IF
      CALL CLOSE_FILE(51,0)
   END IF
   OLDIF=LDIF
!     IF DIFFERENTIAL CHIEF RAYS ARE TRACED, TRACE REQULAR DIFFERENTIAL
!     RAYS AS WELL ELSE DON'T
   IF(LDIF2) LDIF=.TRUE.
   SAVE_KDP(3)=SAVEINPT(3)
   SQ=0
   SST=0
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
   W1=0.0D0
   W2=0.0D0
   W3=LFOB(4)
   WC='RAY     '
   NOCOAT=.FALSE.
   IF(.NOT.GLOBE) THEN
      GRASET=.TRUE.
      DXFSET=.TRUE.
   END IF
   CALL RRAY
   NOCOAT=.TRUE.
   LDIF=OLDIF
   REST_KDP(3)=RESTINPT(3)
!     NOW CALL AUXFOB TO CALCULATE DIFFERENTIAL RAY BASED FFL,BFL AND MAGS
!     AND PUPIL DIAMETERS
   CALL LASTRAY(1)
   IF(LDIF2.AND.RAYEXT.AND.REFEXT) THEN
      CALL AUXFOB(ERRFOB)
   END IF
   CALL LASTRAY(2)
END SUBROUTINE

! RAY: trace one real ray at the current field.
SUBROUTINE KDP_CMD_RAY
   USE GLOBALS
   use DATLEN
   use DATSP1
   use DATHGR
   use DATSPD
   use DATSUB
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   IF(WQ.EQ.'CAOB') CACOCH=1
!     IF NOT OPTIMIZATION OR TOLERANCING, MESSAGE ON
   IF(F28.EQ.0.AND.F31.EQ.0) MSG=.TRUE.
!     IF VIE, MSG OFF
   IF(F34.EQ.1) MSG=.FALSE.
   IF(F34.EQ.0) MSG=.TRUE.
   NOCOAT=.FALSE.
   IF(.NOT.GLOBE) THEN
      GRASET=.TRUE.
      DXFSET=.TRUE.
   END IF
   CALL RRAY
!     DONE, MSG BACK ON
   IF(F34.EQ.1) MSG=.FALSE.
   IF(F34.EQ.0) MSG=.TRUE.
END SUBROUTINE

! SPD (and FAIL, FAILACC, SPDADD, SPDSAVE, SPDSTATS): spot diagram.
SUBROUTINE KDP_CMD_SPD
   USE GLOBALS
   use DATLEN
   use DATSP1
   use DATHGR
   use DATSPD
   use DATSUB
   use DATMAI
   IMPLICIT NONE
   GRASET=.FALSE.
   IF(WC.EQ.'SPDSTATS') SPDEXT=.FALSE.
   MSGSPD=.TRUE.
   CALL SPOT
END SUBROUTINE

! CAPFN: the complex aperture function (wavefront) at the current field.
SUBROUTINE KDP_CMD_CAPFN
   USE GLOBALS
   use DATLEN
   use DATSP1
   use DATHGR
   use DATSPD
   use DATSUB
   use DATMAI
   IMPLICIT NONE
   GRASET=.FALSE.
   MSGSPD=.TRUE.
   IF(DOSTREHL) MSGSPD=.FALSE.
   CALL SPOT
END SUBROUTINE
