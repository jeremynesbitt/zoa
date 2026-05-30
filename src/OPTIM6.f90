!       SIXTH SET OF OPTIMIZATION ROUTINES

! ROUTINE DEROFF.FOR
!
SUBROUTINE DEROFF
!
   use DATSUB
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   LOGICAL ITERROR
!
!
   IF(OPTM_INIT.EQ.1) THEN
      DEREXT=.FALSE.
      SOLEXT=.FALSE.
      ITERROR=.FALSE.
      CALL ITER(0,0,ITERROR)
   END IF
!
   RETURN
END
! SUB FMT.FOR
SUBROUTINE FMT
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,TAGER,CFGCHK
!
   real(real64) CFMTFMT,FMTFMT1,DELFMT
!
   LOGICAL NOP,ALLER,CFGER
!
!
   CFGER=.FALSE.
   TAGER=0
   ALLER=.TRUE.
!
!       THIS IS SUBROUTINE FMT. THIS IS THE SUBROUTINE WHICH
!       HANDLES CMD LEVEL COMMAND "FMT"
!
   NOP=.FALSE.
   IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
      NOP=.TRUE.
      SQ=0
      WQ='        '
   END IF
!
!
   IF(OPCNT.EQ.0) THEN
      WRITE(OUTLYNE,*)'THERE ARE NO OPERANDS'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'NO FIGURE OF MERIT CAN EXIST'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       "FMT" TAKES NO INPUT WORDS
   IF(SST.EQ.1) THEN
      WRITE(OUTLYNE,*)'"FMT" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
      WRITE(OUTLYNE,*)'"FMT" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)'"FMT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(S1.EQ.1.AND.SQ.EQ.0) THEN
!     DO FMT FOR A SINGLE OPERAND
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.FALSE.
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.1) THEN
!     DO FMT FOR A SINGLE CFG
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.TRUE.
   END IF
!
!
   IF(.NOT.CFGER.AND..NOT.ALLER) THEN
!     CHECK FOR VALID TAGER
      IF(TAGER.GT.OPCNT) THEN
!     OPERAND DOES NOT EXIST
         WRITE(OUTLYNE,*)'OPERAND NUMBER ',TAGER,'DOES NOT EXIST'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(CFGER) THEN
!     CHECK FOR TAGER GREATER THAN MAXCFG
      IF(TAGER.GT.MAXCFG) THEN
!     CFG NOT EXISTANT
         WRITE(OUTLYNE,*)'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
      CFGCHK=0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
      END DO

      IF(CFGCHK.EQ.0) THEN
!     CFG OP DATA NOT EXISTANT
         WRITE(OUTLYNE,*)'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
   END IF
!     NOW PROCEED WITH THE OUTPUT
!
   OPCALC_TYPE=3
   CALL OPCALC
   IF(F28.EQ.0) RETURN
   CALL OPLOAD
   IF(F28.EQ.0) RETURN
   IF(KILOPT) THEN
      OUTLYNE='SOME OPERANDS ARE NOT CALCULABLE'
      CALL SHOWIT(1)
      OUTLYNE='THE CURRENT FIGURE OF MERIT IS MEANINGLESS.'
      CALL SHOWIT(1)
      RETURN
   END IF
   FMTFLG=.TRUE.
!       PROCEED WITH ACTION FOR COMMAND
   IF(ALLER) THEN
      CFMTFMT=0.0D0
      DO I=1,OPCNT
         CFMTFMT=CFMTFMT+(OPERND(I,14)**2)
      END DO
   END IF
   IF(.NOT.ALLER.AND..NOT.CFGER) THEN
      FMTFMT1=0.0D0
      FMTFMT1=(OPERND(TAGER,14)**2)
   END IF
   IF(.NOT.ALLER.AND.CFGER) THEN
      FMTFMT1=0.0D0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER)FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
      END DO
   END IF
10 FORMAT('CURRENT FMT = ',G23.15)
101 FORMAT('FOR OPERAND NUMBER = ',I3)
102 FORMAT('  FMT CONTRIBUTION = ',G23.15)
201 FORMAT('FOR CONFIG. NUMBER = ',I2)
   IF(.NOT.NOP) THEN
!
      IF(ALLER) THEN
!     PRINT MESSAGE
         WRITE(OUTLYNE,10) CFMTFMT
         CALL SHOWIT(0)
      END IF
      IF(.NOT.ALLER.AND..NOT.CFGER) THEN
!     PRINT MESSAGE
         WRITE(OUTLYNE,101) TAGER
         CALL SHOWIT(0)
         WRITE(OUTLYNE,102) FMTFMT1
         CALL SHOWIT(0)
      END IF
      IF(.NOT.ALLER.AND.CFGER) THEN
!     PRINT MESSAGE
         WRITE(OUTLYNE,201) TAGER
         CALL SHOWIT(0)
         WRITE(OUTLYNE,102) FMTFMT1
         CALL SHOWIT(0)
      END IF
!
   END IF
   RETURN
!       ALL DONE
END
! SUB FMT2.FOR
SUBROUTINE FMT2
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,TAGER,CFGCHK
!
   real(real64) FMTFMT1,DELFMT
!
   LOGICAL NOP,ALLER,CFGER
!
!

   !call LogTermDebug("In loud version FMT2")
   CFGER=.FALSE.
   TAGER=0
   ALLER=.TRUE.
!
   NOP=.FALSE.
   IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
      NOP=.TRUE.
      SQ=0
      WQ='        '
   END IF
!
!
   IF(OPCNT.EQ.0) THEN
      WRITE(OUTLYNE,*)'THERE ARE NO OPERANDS'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'NO FIGURE OF MERIT CAN EXIST'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       "FMT" TAKES NO INPUT WORDS
   IF(SST.EQ.1) THEN
      WRITE(OUTLYNE,*)'"FMT" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
      WRITE(OUTLYNE,*)'"FMT" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)'"FMT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.0) THEN
!     DO FMT FOR A SINGLE OPERAND
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.FALSE.
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.1) THEN
!     DO FMT FOR A SINGLE CFG
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.TRUE.
   END IF
!
!
   IF(.NOT.CFGER.AND..NOT.ALLER) THEN
!     CHECK FOR VALID TAGER
      IF(TAGER.GT.OPCNT) THEN
!     OPERAND DOES NOT EXIST
         WRITE(OUTLYNE,*)'OPERAND NUMBER ',TAGER,'DOES NOT EXIST'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(CFGER) THEN
!     CHECK FOR TAGER GREATER THAN MAXCFG
      IF(TAGER.GT.MAXCFG) THEN
!     CFG NOT EXISTANT
         WRITE(OUTLYNE,*)'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
      CFGCHK=0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
      END DO

      IF(CFGCHK.EQ.0) THEN
!     CFG OP DATA NOT EXISTANT
         WRITE(OUTLYNE,*)'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
   END IF
!     NOW PROCEED WITH THE OUTPUT
!
   IF(ALLER) OLDFMT=FMTFMT
   OPCALC_TYPE=3
   CALL OPCALC
   IF(F28.EQ.0) RETURN
   CALL OPLOAD
   IF(F28.EQ.0) RETURN
   FMTFLG=.TRUE.
!       PROCEED WITH ACTION FOR COMMAND
   IF(ALLER) THEN
      FMTFMT=0.0D0
      DO I=1,OPCNT
         FMTFMT=FMTFMT+(OPERND(I,14)**2)
      END DO
   END IF
   IF(.NOT.ALLER.AND..NOT.CFGER) THEN
      FMTFMT1=0.0D0
      FMTFMT1=(OPERND(TAGER,14)**2)
   END IF
   IF(.NOT.ALLER.AND.CFGER) THEN
      FMTFMT1=0.0D0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER)FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
      END DO
   END IF
   IF(ALLER) DELFMT=FMTFMT-OLDFMT
10 FORMAT('    NEW FMT = ',G23.15,1X,'CHANGE = ',G23.15)
101 FORMAT('FOR OPERAND NUMBER = ',I3)
102 FORMAT('  FMT CONTRIBUTION = ',G23.15)
201 FORMAT('FOR CONFIG. NUMBER = ',I2)
   IF(.NOT.NOP) THEN
!
      IF(ALLER) THEN
!     PRINT MESSAGE
         WRITE(OUTLYNE,10) FMTFMT,(FMTFMT-OLDFMT)
         CALL SHOWIT(0)
         IF(.NOT.FMTEXT) OLDFMT=FMTFMT
         IF(.NOT.FMTEXT) FMTEXT=.TRUE.
      END IF
      IF(.NOT.ALLER.AND..NOT.CFGER) THEN
!     PRINT MESSAGE
         WRITE(OUTLYNE,101) TAGER
         CALL SHOWIT(0)
         WRITE(OUTLYNE,102) FMTFMT1
         CALL SHOWIT(0)
      END IF
      IF(.NOT.ALLER.AND.CFGER) THEN
!     PRINT MESSAGE
         WRITE(OUTLYNE,201) TAGER
         CALL SHOWIT(0)
         WRITE(OUTLYNE,102) FMTFMT1
         CALL SHOWIT(0)
      END IF
!
   END IF
   RETURN
!       ALL DONE
END
! SUB FMT4.FOR
SUBROUTINE FMT4
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,TAGER,CFGCHK
!
   real(real64) FMTFMT1,DELFMT
!
   LOGICAL NOP,ALLER,CFGER
!
!
   CFGER=.FALSE.
   TAGER=0
   ALLER=.TRUE.
!
   NOP=.FALSE.
   IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
      NOP=.TRUE.
      SQ=0
      WQ='        '
   END IF
!
!
   IF(OPCNT.EQ.0) THEN
      WRITE(OUTLYNE,*)'THERE ARE NO OPERANDS'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'NO FIGURE OF MERIT CAN EXIST'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!       "FMT" TAKES NO INPUT WORDS
   IF(SST.EQ.1) THEN
      WRITE(OUTLYNE,*)'"FMT" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'CFG') THEN
      WRITE(OUTLYNE,*)'"FMT" CAN ONLY TAKE "CFG" AS AN OPTIONAL QUALIFIER WORD'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)'"FMT" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.0) THEN
!     DO FMT FOR A SINGLE OPERAND
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.FALSE.
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.1) THEN
!     DO FMT FOR A SINGLE CFG
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.TRUE.
   END IF
!
!
   IF(.NOT.CFGER.AND..NOT.ALLER) THEN
!     CHECK FOR VALID TAGER
      IF(TAGER.GT.OPCNT) THEN
!     OPERAND DOES NOT EXIST
         WRITE(OUTLYNE,*)'OPERAND NUMBER ',TAGER,'DOES NOT EXIST'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(CFGER) THEN
!     CHECK FOR TAGER GREATER THAN MAXCFG
      IF(TAGER.GT.MAXCFG) THEN
!     CFG NOT EXISTANT
         WRITE(OUTLYNE,*)'CONFIG NUMBER BEYOND CURRENT PROGRAM LIMIT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
      CFGCHK=0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
      END DO

      IF(CFGCHK.EQ.0) THEN
!     CFG OP DATA NOT EXISTANT
         WRITE(OUTLYNE,*)'NO OPERANDS ARE ACTIVE IN CONFIG # ',TAGER
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!
   END IF
!     NOW PROCEED WITH THE OUTPUT
!
   IF(ALLER) OLDFMT=FMTFMT
   OPCALC_TYPE=3
   CALL OPCALC
   IF(F28.EQ.0) RETURN
   CALL OPLOAD
   IF(F28.EQ.0) RETURN
   FMTFLG=.TRUE.
!       PROCEED WITH ACTION FOR COMMAND
   IF(ALLER) THEN
      FMTFMT=0.0D0
      DO I=1,OPCNT
         FMTFMT=FMTFMT+(OPERND(I,14)**2)
      END DO
   END IF
   IF(.NOT.ALLER.AND..NOT.CFGER) THEN
      FMTFMT1=0.0D0
      FMTFMT1=(OPERND(TAGER,14)**2)
   END IF
   IF(.NOT.ALLER.AND.CFGER) THEN
      FMTFMT1=0.0D0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER)FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
      END DO
   END IF
   IF(ALLER) DELFMT=FMTFMT-OLDFMT
10 FORMAT('    NEW FMT = ',G23.15,1X,'CHANGE = ',G23.15)
101 FORMAT('FOR OPERAND NUMBER = ',I3)
102 FORMAT('  FMT CONTRIBUTION = ',G23.15)
201 FORMAT('FOR CONFIG. NUMBER = ',I2)
20 FORMAT('FMT(CHANGE) = ',G23.15)
   IF(.NOT.NOP) THEN
!
      IF(ALLER) THEN
!     PRINT MESSAGE
         IF(.NOT.FMTEXT) OLDFMT=FMTFMT
         IF(.NOT.FMTEXT) FMTEXT=.TRUE.
      END IF
!
   END IF
   RETURN
!       ALL DONE
END
! SUB FIXCVAR.FOR
SUBROUTINE FIXCVAR
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER II,I,J,VALT,VBSURF
!
!
   DO I=1,MAXCMP
      IF(ISCOMP(I)) THEN
         II=I
         VBSURF=INT(VARABL(II,3))
         VALT=INT(VARABL(II,1))
         IF(VALT.EQ.2.OR.VALT.EQ.1) THEN
            VARABL(II,4)=surf_curvature(VBSURF)
            VARABL(II,5)=surf_curvature(VBSURF)
            VARABL(II,13)=surf_curvature(VBSURF)
            VARABL(II,6)=0.0D0
         END IF
!
!     DO TH VARIABLE
         IF(VALT.EQ.3) THEN
!
            VARABL(II,4)=surf_thickness(VBSURF)
            VARABL(II,5)=surf_thickness(VBSURF)
            VARABL(II,13)=surf_thickness(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 3, NOT A THICKNESS VARIABLE
         END IF
!     DO CC
         IF(VALT.EQ.4) THEN
!
            VARABL(II,4)=surf_conic(VBSURF)
            VARABL(II,5)=surf_conic(VBSURF)
            VARABL(II,13)=surf_conic(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 4, CC VARIABLE
         END IF
         IF(VALT.EQ.5) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
!
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 4)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 4)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 4)
            VARABL(II,6)=0.0D0
!     VALT NOT 5, AD VARIABLE
         END IF
!
         IF(VALT.EQ.6) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 6)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 6)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 6)
            VARABL(II,6)=0.0D0
!     VALT NOT 6, AE VARIABLE
         END IF
!
         IF(VALT.EQ.7) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 8)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 8)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 8)
            VARABL(II,6)=0.0D0
!     VALT NOT 7, AF VARIABLE
         END IF
!
         IF(VALT.EQ.8) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 10)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 10)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 10)
            VARABL(II,6)=0.0D0
!     VALT NOT 8, AG VARIABLE
         END IF
         IF(VALT.EQ.129) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 12)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 12)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 12)
            VARABL(II,6)=0.0D0
         END IF
         IF(VALT.EQ.130) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 14)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 14)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 14)
            VARABL(II,6)=0.0D0
         END IF
         IF(VALT.EQ.131) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 16)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 16)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 16)
            VARABL(II,6)=0.0D0
         END IF
         IF(VALT.EQ.132) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 18)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 18)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 18)
            VARABL(II,6)=0.0D0
         END IF
         IF(VALT.EQ.133) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 20)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 20)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 20)
            VARABL(II,6)=0.0D0
         END IF
!     DO ZD
         IF(VALT.EQ.134) THEN
            VARABL(II,4)=surf_focus_dz(VBSURF)
            VARABL(II,5)=surf_focus_dz(VBSURF)
            VARABL(II,13)=surf_focus_dz(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 134, ZD VARIABLE
         END IF
!     DO INDEX
         IF(VALT.EQ.135) THEN
            VARABL(II,4)=surf_fict_n(VBSURF)
            VARABL(II,5)=surf_fict_n(VBSURF)
            VARABL(II,13)=surf_fict_n(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 135, INDEX VARIABLE
         END IF
!     DO VNUM
         IF(VALT.EQ.136) THEN
            VARABL(II,4)=surf_fict_v(VBSURF)
            VARABL(II,5)=surf_fict_v(VBSURF)
            VARABL(II,13)=surf_fict_v(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 136, VNUM VARIABLE
         END IF
!     DO PIVX
         IF(VALT.EQ.137) THEN
            VARABL(II,4)=surf_pivot_x(VBSURF)
            VARABL(II,5)=surf_pivot_x(VBSURF)
            VARABL(II,13)=surf_pivot_x(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 137, PIVX VARIABLE
         END IF
!     DO PIVY
         IF(VALT.EQ.138) THEN
            VARABL(II,4)=surf_pivot_y(VBSURF)
            VARABL(II,5)=surf_pivot_y(VBSURF)
            VARABL(II,13)=surf_pivot_y(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 138, PIVY VARIABLE
         END IF
!     DO PIVZ
         IF(VALT.EQ.139) THEN
            VARABL(II,4)=surf_pivot_z(VBSURF)
            VARABL(II,5)=surf_pivot_z(VBSURF)
            VARABL(II,13)=surf_pivot_z(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 139, PIVZ VARIABLE
         END IF
!     DO DPART
         IF(VALT.EQ.140) THEN
            VARABL(II,4)=surf_fict_w(VBSURF)
            VARABL(II,5)=surf_fict_w(VBSURF)
            VARABL(II,13)=surf_fict_w(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 140, DPART VARIABLE
         END IF
!     DO CLPX
         IF(VALT.EQ.141) THEN
            VARABL(II,4)=surf_clap_dim(VBSURF, 2)
            VARABL(II,5)=surf_clap_dim(VBSURF, 2)
            VARABL(II,13)=surf_clap_dim(VBSURF, 2)
            VARABL(II,6)=0.0D0
!     VALT NOT 141, CLPX VARIABLE
         END IF
!     DO CLPY
         IF(VALT.EQ.142) THEN
            VARABL(II,4)=surf_clap_dim(VBSURF, 1)
            VARABL(II,5)=surf_clap_dim(VBSURF, 1)
            VARABL(II,13)=surf_clap_dim(VBSURF, 1)
            VARABL(II,6)=0.0D0
!     VALT NOT 142, CLPY VARIABLE
         END IF
!     DO GDX
         IF(VALT.EQ.143) THEN
            VARABL(II,4)=surf_global_dx(VBSURF)
            VARABL(II,5)=surf_global_dx(VBSURF)
            VARABL(II,13)=surf_global_dx(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 143, GDX VARIABLE
         END IF
!     DO GDY
         IF(VALT.EQ.144) THEN
            VARABL(II,4)=surf_global_dy(VBSURF)
            VARABL(II,5)=surf_global_dy(VBSURF)
            VARABL(II,13)=surf_global_dy(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 144, GDY VARIABLE
         END IF
!     DO GDZ
         IF(VALT.EQ.145) THEN
            VARABL(II,4)=surf_global_dz(VBSURF)
            VARABL(II,5)=surf_global_dz(VBSURF)
            VARABL(II,13)=surf_global_dz(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 145, GDZ VARIABLE
         END IF
!     DO GALPHA
         IF(VALT.EQ.146) THEN
            VARABL(II,4)=surf_global_alpha(VBSURF)
            VARABL(II,5)=surf_global_alpha(VBSURF)
            VARABL(II,13)=surf_global_alpha(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 146, GALPHA VARIABLE
         END IF
!     DO GBETA
         IF(VALT.EQ.147) THEN
            VARABL(II,4)=surf_global_beta(VBSURF)
            VARABL(II,5)=surf_global_beta(VBSURF)
            VARABL(II,13)=surf_global_beta(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 147, GBETA VARIABLE
         END IF
!     DO GGAMMA
         IF(VALT.EQ.148) THEN
            VARABL(II,4)=surf_global_gamma(VBSURF)
            VARABL(II,5)=surf_global_gamma(VBSURF)
            VARABL(II,13)=surf_global_gamma(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 148, GGAMMA VARIABLE
         END IF
!     DO GRS
         IF(VALT.EQ.149) THEN
            VARABL(II,4)=surf_grating_spacing(VBSURF)
            VARABL(II,5)=surf_grating_spacing(VBSURF)
            VARABL(II,13)=surf_grating_spacing(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 149, GRS VARIABLE
         END IF
!
!     DO CVTOR
         IF(VALT.EQ.10.OR.VALT.EQ.9) THEN
            VARABL(II,4)=surf_toric_curvature(VBSURF)
            VARABL(II,5)=surf_toric_curvature(VBSURF)
            VARABL(II,13)=surf_toric_curvature(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 10, NOT A RDTOR OR CVTOR VARIABLE
         END IF
!
!     DO CCTOR
         IF(VALT.EQ.11) THEN
            VARABL(II,4)=surf_anamorphic_conic(VBSURF)
            VARABL(II,5)=surf_anamorphic_conic(VBSURF)
            VARABL(II,13)=surf_anamorphic_conic(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 11, CCTOR VARIABLE
         END IF
!
!     DO ADTOR
         IF(VALT.EQ.12) THEN
            VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 4)
            VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 4)
            VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 4)
            VARABL(II,6)=0.0D0
!     VALT NOT 12, ADTOR VARIABLE
         END IF
!
!     DO AETOR
         IF(VALT.EQ.13) THEN
            VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 6)
            VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 6)
            VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 6)
            VARABL(II,6)=0.0D0
!     VALT NOT 13, AETOR VARIABLE
         END IF
!
!     DO AFTOR
         IF(VALT.EQ.14) THEN
            VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 8)
            VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 8)
            VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 8)
            VARABL(II,6)=0.0D0
!     VALT NOT 14, AFTOR VARIABLE
         END IF
!
!     DO AGTOR
         IF(VALT.EQ.15) THEN
            VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 10)
            VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 10)
            VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 10)
            VARABL(II,6)=0.0D0
!     VALT NOT 15, AGTOR VARIABLE
         END IF
!
!     DO ALPHA
         IF(VALT.EQ.16) THEN
            VARABL(II,4)=surf_alpha_deg(VBSURF)
            VARABL(II,5)=surf_alpha_deg(VBSURF)
            VARABL(II,13)=surf_alpha_deg(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 16, ALPHA VARIABLE
         END IF
!
!     DO BETA
         IF(VALT.EQ.17) THEN
            VARABL(II,4)=surf_beta_deg(VBSURF)
            VARABL(II,5)=surf_beta_deg(VBSURF)
            VARABL(II,13)=surf_beta_deg(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 17, BETA VARIABLE
         END IF
!
!     DO GAMMA
         IF(VALT.EQ.18) THEN
            VARABL(II,4)=surf_gamma_deg(VBSURF)
            VARABL(II,5)=surf_gamma_deg(VBSURF)
            VARABL(II,13)=surf_gamma_deg(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 18, GAMMA VARIABLE
         END IF
!
!     DO YD
         IF(VALT.EQ.20) THEN
            VARABL(II,4)=surf_focus_dy(VBSURF)
            VARABL(II,5)=surf_focus_dy(VBSURF)
            VARABL(II,13)=surf_focus_dy(VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 20, YD VARIABLE
         END IF
!
!     DO N1
         IF(VALT.EQ.21) THEN
            VARABL(II,4)=surf_refractive_index(VBSURF, 1)
            VARABL(II,5)=surf_refractive_index(VBSURF, 1)
            VARABL(II,13)=surf_refractive_index(VBSURF, 1)
            VARABL(II,6)=0.0D0
!     VALT NOT 21, N1 VARIABLE
         END IF
!
!     DO N2
         IF(VALT.EQ.22) THEN
            VARABL(II,4)=surf_refractive_index(VBSURF, 2)
            VARABL(II,5)=surf_refractive_index(VBSURF, 2)
            VARABL(II,13)=surf_refractive_index(VBSURF, 2)
            VARABL(II,6)=0.0D0
!     VALT NOT 22, N2 VARIABLE
         END IF
!
!     DO N3
         IF(VALT.EQ.23) THEN
            VARABL(II,4)=surf_refractive_index(VBSURF, 3)
            VARABL(II,5)=surf_refractive_index(VBSURF, 3)
            VARABL(II,13)=surf_refractive_index(VBSURF, 3)
            VARABL(II,6)=0.0D0
!     VALT NOT 23, N3 VARIABLE
         END IF
!
!     DO N4
         IF(VALT.EQ.24) THEN
            VARABL(II,4)=surf_refractive_index(VBSURF, 4)
            VARABL(II,5)=surf_refractive_index(VBSURF, 4)
            VARABL(II,13)=surf_refractive_index(VBSURF, 4)
            VARABL(II,6)=0.0D0
!     VALT NOT 24, N4 VARIABLE
         END IF
!
!     DO N5
         IF(VALT.EQ.25) THEN
            VARABL(II,4)=surf_refractive_index(VBSURF, 5)
            VARABL(II,5)=surf_refractive_index(VBSURF, 5)
            VARABL(II,13)=surf_refractive_index(VBSURF, 5)
            VARABL(II,6)=0.0D0
!     VALT NOT 25, N5 VARIABLE
         END IF
!
!     NOW DO THE COEFFS
!
!     DO C1 TO C48
         IF(VALT.GE.27.AND.VALT.LE.74) THEN
            VARABL(II,4)=FTFL01((VALT-26),VBSURF)
            VARABL(II,5)=FTFL01((VALT-26),VBSURF)
            VARABL(II,13)=FTFL01((VALT-26),VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 27 TO 74, C1 - C48 VARIABLE
         END IF
!     DO C49 TO C96
         IF(VALT.GE.76.AND.VALT.LE.123) THEN
            VARABL(II,4)=FTFL01((VALT-27),VBSURF)
            VARABL(II,5)=FTFL01((VALT-27),VBSURF)
            VARABL(II,13)=FTFL01((VALT-27),VBSURF)
            VARABL(II,6)=0.0D0
!     VALT NOT 76 TO 123, C49 - C96 VARIABLE
         END IF
!
!     DO AC
         IF(VALT.EQ.75) THEN
            VARABL(II,4)=surf_asphere_coeff(VBSURF, 2)
            VARABL(II,5)=surf_asphere_coeff(VBSURF, 2)
            VARABL(II,13)=surf_asphere_coeff(VBSURF, 2)
            VARABL(II,6)=0.0D0
!     VALT NOT 75, AC VARIABLE
         END IF
!
200      CONTINUE
      END IF
   END DO
!       ALL DONE
   RETURN
END
! SUB FIXTVAR.FOR
SUBROUTINE FIXTVAR
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER II,I,J,VALT,VBSURF
!
!
!     NOW BUILD THE VARIABLE ENTRY
!       J=1  > 1 THROUGH 164, A VARIABLE TYPE DESIGNATOR
!       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
!       J=4  > VARIABLE CURRENT VALUE
!       J=5  > VARIABLE PREVIOUS VALUE
!       J=6  > LAST VARIABLE CHANGE VALUE
!
!
   IF(TVBCNT.EQ.0) RETURN
   DO I=1,TVBCNT
      II=I+MAXCMP
      VBSURF=INT(VARABL(II,3))
      VALT=INT(VARABL(II,1))
      IF(VALT.EQ.1.OR.VALT.EQ.134) THEN
         IF(surf_curvature(VBSURF).EQ.0.0D0) THEN
            VARABL(II,4)=0.0D0
            VARABL(II,5)=0.0D0
            VARABL(II,13)=0.0D0
         ELSE
            VARABL(II,4)=1.0D0/surf_curvature(VBSURF)
            VARABL(II,5)=1.0D0/surf_curvature(VBSURF)
            VARABL(II,13)=1.0D0/surf_curvature(VBSURF)
         END IF
         VARABL(II,6)=0.0D0
      END IF
      IF(VALT.EQ.2.OR.VALT.EQ.135) THEN
         VARABL(II,4)=surf_curvature(VBSURF)
         VARABL(II,5)=surf_curvature(VBSURF)
         VARABL(II,13)=surf_curvature(VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!
!     DO TH VARIABLE
      IF(VALT.EQ.3) THEN
!
         VARABL(II,4)=surf_thickness(VBSURF)
         VARABL(II,5)=surf_thickness(VBSURF)
         VARABL(II,13)=surf_thickness(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 3, NOT A THICKNESS VARIABLE
      END IF
!     DO CC
      IF(VALT.EQ.4) THEN
!
         VARABL(II,4)=surf_conic(VBSURF)
         VARABL(II,5)=surf_conic(VBSURF)
         VARABL(II,13)=surf_conic(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 4, CC VARIABLE
      END IF
      IF(VALT.EQ.5) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
!
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 4)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 4)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 4)
         VARABL(II,6)=0.0D0
!     VALT NOT 5, AD VARIABLE
      END IF
!
      IF(VALT.EQ.6) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 6)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 6)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 6)
         VARABL(II,6)=0.0D0
!     VALT NOT 6, AE VARIABLE
      END IF
!
      IF(VALT.EQ.7) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 8)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 8)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 8)
         VARABL(II,6)=0.0D0
!     VALT NOT 7, AF VARIABLE
      END IF
!
      IF(VALT.EQ.8) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 10)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 10)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 10)
         VARABL(II,6)=0.0D0
!     VALT NOT 8, AG VARIABLE
      END IF
      IF(VALT.EQ.129) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 12)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 12)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 12)
         VARABL(II,6)=0.0D0
      END IF
      IF(VALT.EQ.130) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 14)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 14)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 14)
         VARABL(II,6)=0.0D0
      END IF
      IF(VALT.EQ.131) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 16)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 16)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 16)
         VARABL(II,6)=0.0D0
      END IF
      IF(VALT.EQ.132) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 18)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 18)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 18)
         VARABL(II,6)=0.0D0
      END IF
      IF(VALT.EQ.133) THEN
         IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 20)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 20)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 20)
         VARABL(II,6)=0.0D0
      END IF
!     DO ZD
      IF(VALT.EQ.138) THEN
         VARABL(II,4)=surf_focus_dz(VBSURF)
         VARABL(II,5)=surf_focus_dz(VBSURF)
         VARABL(II,13)=surf_focus_dz(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 138, ZD VARIABLE
      END IF
!     DO INDEX
      IF(VALT.EQ.139) THEN
         VARABL(II,4)=surf_fict_n(VBSURF)
         VARABL(II,5)=surf_fict_n(VBSURF)
         VARABL(II,13)=surf_fict_n(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 139, INDEX VARIABLE
      END IF
!     DO VNUM
      IF(VALT.EQ.140) THEN
         VARABL(II,4)=surf_fict_v(VBSURF)
         VARABL(II,5)=surf_fict_v(VBSURF)
         VARABL(II,13)=surf_fict_v(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 140, VNUM VARIABLE
      END IF
!     DO PIVX
      IF(VALT.EQ.141) THEN
         VARABL(II,4)=surf_pivot_x(VBSURF)
         VARABL(II,5)=surf_pivot_x(VBSURF)
         VARABL(II,13)=surf_pivot_x(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 141, PIVX VARIABLE
      END IF
!     DO PIVY
      IF(VALT.EQ.142) THEN
         VARABL(II,4)=surf_pivot_y(VBSURF)
         VARABL(II,5)=surf_pivot_y(VBSURF)
         VARABL(II,13)=surf_pivot_y(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 142, PIVY VARIABLE
      END IF
!     DO PIVZ
      IF(VALT.EQ.143) THEN
         VARABL(II,4)=surf_pivot_z(VBSURF)
         VARABL(II,5)=surf_pivot_z(VBSURF)
         VARABL(II,13)=surf_pivot_z(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 143, PIVZ VARIABLE
      END IF
!     DO DPART
      IF(VALT.EQ.144) THEN
         VARABL(II,4)=surf_fict_w(VBSURF)
         VARABL(II,5)=surf_fict_w(VBSURF)
         VARABL(II,13)=surf_fict_w(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 144, DPART VARIABLE
      END IF
!     DO CLPX
      IF(VALT.EQ.145) THEN
         VARABL(II,4)=surf_clap_dim(VBSURF, 2)
         VARABL(II,5)=surf_clap_dim(VBSURF, 2)
         VARABL(II,13)=surf_clap_dim(VBSURF, 2)
         VARABL(II,6)=0.0D0
!     VALT NOT 145, CLPX VARIABLE
      END IF
!     DO CLPY
      IF(VALT.EQ.146) THEN
         VARABL(II,4)=surf_clap_dim(VBSURF, 1)
         VARABL(II,5)=surf_clap_dim(VBSURF, 1)
         VARABL(II,13)=surf_clap_dim(VBSURF, 1)
         VARABL(II,6)=0.0D0
!     VALT NOT 146, CLPY VARIABLE
      END IF
!     DO GDX
      IF(VALT.EQ.147) THEN
         VARABL(II,4)=surf_global_dx(VBSURF)
         VARABL(II,5)=surf_global_dx(VBSURF)
         VARABL(II,13)=surf_global_dx(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 147, GDX VARIABLE
      END IF
!     DO GDY
      IF(VALT.EQ.148) THEN
         VARABL(II,4)=surf_global_dy(VBSURF)
         VARABL(II,5)=surf_global_dy(VBSURF)
         VARABL(II,13)=surf_global_dy(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 148, GDY VARIABLE
      END IF
!     DO GDZ
      IF(VALT.EQ.149) THEN
         VARABL(II,4)=surf_global_dz(VBSURF)
         VARABL(II,5)=surf_global_dz(VBSURF)
         VARABL(II,13)=surf_global_dz(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 149, GDZ VARIABLE
      END IF
!     DO GALPHA
      IF(VALT.EQ.150) THEN
         VARABL(II,4)=surf_global_alpha(VBSURF)
         VARABL(II,5)=surf_global_alpha(VBSURF)
         VARABL(II,13)=surf_global_alpha(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 150, GALPHA VARIABLE
      END IF
!     DO GBETA
      IF(VALT.EQ.151) THEN
         VARABL(II,4)=surf_global_beta(VBSURF)
         VARABL(II,5)=surf_global_beta(VBSURF)
         VARABL(II,13)=surf_global_beta(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 151, GBETA VARIABLE
      END IF
!     DO GGAMMA
      IF(VALT.EQ.152) THEN
         VARABL(II,4)=surf_global_gamma(VBSURF)
         VARABL(II,5)=surf_global_gamma(VBSURF)
         VARABL(II,13)=surf_global_gamma(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 152, GGAMMA VARIABLE
      END IF
!     DO GRS
      IF(VALT.EQ.153) THEN
         VARABL(II,4)=surf_grating_spacing(VBSURF)
         VARABL(II,5)=surf_grating_spacing(VBSURF)
         VARABL(II,13)=surf_grating_spacing(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 153, GRS VARIABLE
      END IF
!     DO DISPX
      IF(VALT.EQ.154) THEN
         VARABL(II,4)=TOLER(1,VBSURF)
         VARABL(II,5)=TOLER(1,VBSURF)
         VARABL(II,13)=TOLER(1,VBSURF)
         VARABL(II,7)=TOLER(2,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO DISPY
      IF(VALT.EQ.155) THEN
         VARABL(II,4)=TOLER(3,VBSURF)
         VARABL(II,5)=TOLER(3,VBSURF)
         VARABL(II,13)=TOLER(3,VBSURF)
         VARABL(II,7)=TOLER(4,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO DISPZ
      IF(VALT.EQ.156) THEN
         VARABL(II,4)=TOLER(5,VBSURF)
         VARABL(II,5)=TOLER(5,VBSURF)
         VARABL(II,13)=TOLER(5,VBSURF)
         VARABL(II,7)=TOLER(6,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO STILTA
      IF(VALT.EQ.157) THEN
         VARABL(II,4)=TOLER(7,VBSURF)
         VARABL(II,5)=TOLER(7,VBSURF)
         VARABL(II,13)=TOLER(7,VBSURF)
         VARABL(II,9)=TOLER(8,VBSURF)
         VARABL(II,10)=TOLER(9,VBSURF)
         VARABL(II,11)=TOLER(10,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO STILTB
      IF(VALT.EQ.158) THEN
         VARABL(II,4)=TOLER(11,VBSURF)
         VARABL(II,5)=TOLER(11,VBSURF)
         VARABL(II,13)=TOLER(11,VBSURF)
         VARABL(II,9)=TOLER(12,VBSURF)
         VARABL(II,10)=TOLER(13,VBSURF)
         VARABL(II,11)=TOLER(14,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO STILTG
      IF(VALT.EQ.159) THEN
         VARABL(II,4)=TOLER(15,VBSURF)
         VARABL(II,5)=TOLER(15,VBSURF)
         VARABL(II,13)=TOLER(15,VBSURF)
         VARABL(II,9)=TOLER(16,VBSURF)
         VARABL(II,10)=TOLER(17,VBSURF)
         VARABL(II,11)=TOLER(18,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO BTILTA
      IF(VALT.EQ.160) THEN
         VARABL(II,4)=TOLER(19,VBSURF)
         VARABL(II,5)=TOLER(19,VBSURF)
         VARABL(II,13)=TOLER(19,VBSURF)
         VARABL(II,9)=TOLER(21,VBSURF)
         VARABL(II,10)=TOLER(22,VBSURF)
         VARABL(II,11)=TOLER(23,VBSURF)
         VARABL(II,7)=TOLER(20,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO BTILTB
      IF(VALT.EQ.161) THEN
         VARABL(II,4)=TOLER(24,VBSURF)
         VARABL(II,5)=TOLER(24,VBSURF)
         VARABL(II,13)=TOLER(24,VBSURF)
         VARABL(II,9)=TOLER(26,VBSURF)
         VARABL(II,10)=TOLER(27,VBSURF)
         VARABL(II,11)=TOLER(28,VBSURF)
         VARABL(II,7)=TOLER(25,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO BTILTG
      IF(VALT.EQ.162) THEN
         VARABL(II,4)=TOLER(29,VBSURF)
         VARABL(II,5)=TOLER(29,VBSURF)
         VARABL(II,13)=TOLER(29,VBSURF)
         VARABL(II,9)=TOLER(31,VBSURF)
         VARABL(II,10)=TOLER(32,VBSURF)
         VARABL(II,11)=TOLER(33,VBSURF)
         VARABL(II,7)=TOLER(30,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO ROLLX
      IF(VALT.EQ.163) THEN
         VARABL(II,4)=TOLER(34,VBSURF)
         VARABL(II,5)=TOLER(34,VBSURF)
         VARABL(II,13)=TOLER(34,VBSURF)
         VARABL(II,7)=TOLER(35,VBSURF)
         VARABL(II,12)=TOLER(36,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!     DO ROLLY
      IF(VALT.EQ.164) THEN
         VARABL(II,4)=TOLER(37,VBSURF)
         VARABL(II,5)=TOLER(37,VBSURF)
         VARABL(II,13)=TOLER(37,VBSURF)
         VARABL(II,7)=TOLER(38,VBSURF)
         VARABL(II,12)=TOLER(39,VBSURF)
         VARABL(II,6)=0.0D0
      END IF
!
!     DO RDTOR
      IF(VALT.EQ.9.OR.VALT.EQ.136) THEN
         IF(surf_toric_curvature(VBSURF).NE.0.0D0) THEN
            VARABL(II,4)=1.0D0/surf_toric_curvature(VBSURF)
            VARABL(II,5)=1.0D0/surf_toric_curvature(VBSURF)
            VARABL(II,13)=1.0D0/surf_toric_curvature(VBSURF)
         ELSE
            VARABL(II,4)=0.0D0
            VARABL(II,5)=0.0D0
            VARABL(II,13)=0.0D0
         END IF
         VARABL(II,6)=0.0D0
!     VALT NOT 9 01 136 , NOT A RDTOR VARIABLE
      END IF
!     DO CVTOR
      IF(VALT.EQ.10.OR.VALT.EQ.137) THEN
         VARABL(II,4)=surf_toric_curvature(VBSURF)
         VARABL(II,5)=surf_toric_curvature(VBSURF)
         VARABL(II,13)=surf_toric_curvature(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 10 OR 137, NOT CVTOR VARIABLE
      END IF
!
!     DO CCTOR
      IF(VALT.EQ.11) THEN
         VARABL(II,4)=surf_anamorphic_conic(VBSURF)
         VARABL(II,5)=surf_anamorphic_conic(VBSURF)
         VARABL(II,13)=surf_anamorphic_conic(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 11, CCTOR VARIABLE
      END IF
!
!     DO ADTOR
      IF(VALT.EQ.12) THEN
         VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 4)
         VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 4)
         VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 4)
         VARABL(II,6)=0.0D0
!     VALT NOT 12, ADTOR VARIABLE
      END IF
!
!     DO AETOR
      IF(VALT.EQ.13) THEN
         VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 6)
         VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 6)
         VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 6)
         VARABL(II,6)=0.0D0
!     VALT NOT 13, AETOR VARIABLE
      END IF
!
!     DO AFTOR
      IF(VALT.EQ.14) THEN
         VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 8)
         VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 8)
         VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 8)
         VARABL(II,6)=0.0D0
!     VALT NOT 14, AFTOR VARIABLE
      END IF
!
!     DO AGTOR
      IF(VALT.EQ.15) THEN
         VARABL(II,4)=surf_anamorphic_coeff(VBSURF, 10)
         VARABL(II,5)=surf_anamorphic_coeff(VBSURF, 10)
         VARABL(II,13)=surf_anamorphic_coeff(VBSURF, 10)
         VARABL(II,6)=0.0D0
!     VALT NOT 15, AGTOR VARIABLE
      END IF
!
!     DO ALPHA
      IF(VALT.EQ.16) THEN
         VARABL(II,4)=surf_alpha_deg(VBSURF)
         VARABL(II,5)=surf_alpha_deg(VBSURF)
         VARABL(II,13)=surf_alpha_deg(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 16, ALPHA VARIABLE
      END IF
!
!     DO BETA
      IF(VALT.EQ.17) THEN
         VARABL(II,4)=surf_beta_deg(VBSURF)
         VARABL(II,5)=surf_beta_deg(VBSURF)
         VARABL(II,13)=surf_beta_deg(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 17, BETA VARIABLE
      END IF
!
!     DO GAMMA
      IF(VALT.EQ.18) THEN
         VARABL(II,4)=surf_gamma_deg(VBSURF)
         VARABL(II,5)=surf_gamma_deg(VBSURF)
         VARABL(II,13)=surf_gamma_deg(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 18, GAMMA VARIABLE
      END IF
!
!     DO XD
      IF(VALT.EQ.19) THEN
         VARABL(II,4)=surf_focus_dx(VBSURF)
         VARABL(II,5)=surf_focus_dx(VBSURF)
         VARABL(II,13)=surf_focus_dx(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 19, XD VARIABLE
      END IF
!
!     DO YD
      IF(VALT.EQ.20) THEN
         VARABL(II,4)=surf_focus_dy(VBSURF)
         VARABL(II,5)=surf_focus_dy(VBSURF)
         VARABL(II,13)=surf_focus_dy(VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 20, YD VARIABLE
      END IF
!
!     DO N1
      IF(VALT.EQ.21) THEN
         VARABL(II,4)=surf_refractive_index(VBSURF, 1)
         VARABL(II,5)=surf_refractive_index(VBSURF, 1)
         VARABL(II,13)=surf_refractive_index(VBSURF, 1)
         VARABL(II,6)=0.0D0
!     VALT NOT 21, N1 VARIABLE
      END IF
!
!     DO N2
      IF(VALT.EQ.22) THEN
         VARABL(II,4)=surf_refractive_index(VBSURF, 2)
         VARABL(II,5)=surf_refractive_index(VBSURF, 2)
         VARABL(II,13)=surf_refractive_index(VBSURF, 2)
         VARABL(II,6)=0.0D0
!     VALT NOT 22, N2 VARIABLE
      END IF
!
!     DO N3
      IF(VALT.EQ.23) THEN
         VARABL(II,4)=surf_refractive_index(VBSURF, 3)
         VARABL(II,5)=surf_refractive_index(VBSURF, 3)
         VARABL(II,13)=surf_refractive_index(VBSURF, 3)
         VARABL(II,6)=0.0D0
!     VALT NOT 23, N3 VARIABLE
      END IF
!
!     DO N4
      IF(VALT.EQ.24) THEN
         VARABL(II,4)=surf_refractive_index(VBSURF, 4)
         VARABL(II,5)=surf_refractive_index(VBSURF, 4)
         VARABL(II,13)=surf_refractive_index(VBSURF, 4)
         VARABL(II,6)=0.0D0
!     VALT NOT 24, N4 VARIABLE
      END IF
!
!     DO N5
      IF(VALT.EQ.25) THEN
         VARABL(II,4)=surf_refractive_index(VBSURF, 5)
         VARABL(II,5)=surf_refractive_index(VBSURF, 5)
         VARABL(II,13)=surf_refractive_index(VBSURF, 5)
         VARABL(II,6)=0.0D0
!     VALT NOT 25, N5 VARIABLE
      END IF
!
!     NOW DO THE COEFFS
!
!     DO C1 TO C48
      IF(VALT.GE.27.AND.VALT.LE.74) THEN
         VARABL(II,4)=FTFL01((VALT-26),VBSURF)
         VARABL(II,5)=FTFL01((VALT-26),VBSURF)
         VARABL(II,13)=FTFL01((VALT-26),VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 27 TO 74, C1 - C48 VARIABLE
      END IF
!     DO C49 TO C96
      IF(VALT.GE.76.AND.VALT.LE.123) THEN
         VARABL(II,4)=FTFL01((VALT-27),VBSURF)
         VARABL(II,5)=FTFL01((VALT-27),VBSURF)
         VARABL(II,13)=FTFL01((VALT-27),VBSURF)
         VARABL(II,6)=0.0D0
!     VALT NOT 76 TO 123, C49 - C96 VARIABLE
      END IF
!
!     DO AC
      IF(VALT.EQ.75) THEN
         VARABL(II,4)=surf_asphere_coeff(VBSURF, 2)
         VARABL(II,5)=surf_asphere_coeff(VBSURF, 2)
         VARABL(II,13)=surf_asphere_coeff(VBSURF, 2)
         VARABL(II,6)=0.0D0
!     VALT NOT 75, AC VARIABLE
      END IF
!
200   CONTINUE
   END DO
!       ALL DONE
   RETURN
END
! SUB FIXVAR.FOR
SUBROUTINE FIXVAR
   USE NSSMOD
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,J,VALT,VBSURF,VADD
!
!
!     NOW BUILD THE VARIABLE ENTRY
!       J=1  > 2 THROUGH 133, A VARIABLE TYPE DESIGNATOR
!       J=2  > 1 THROUGH MAXCFG, THE CONFIGURATION DESIGNATOR
!       J=3  > 0 THROUGH MAXSUR, THE SURFACE NUMBER DESIGNATOR
!       J=4  > VARIABLE CURRENT VALUE
!       J=5  > VARIABLE PREVIOUS VALUE
!       J=6  > LAST VARIABLE CHANGE VALUE
!
!
   PRINT *, "FIXVAR ROUTINE STARTED!"

   IF(VBCNT.EQ.0) RETURN
   DO I=1,VBCNT
      VBSURF=INT(VARABL(I,3))
      VBCFG=INT(VARABL(I,2))
      VALT=INT(VARABL(I,1))
      IF(VBCFG.EQ.1) THEN
!     CONFIG 1 VARIABLES
         IF(VALT.EQ.2.OR.VALT.EQ.9) THEN
            VARABL(I,4)=surf_curvature(VBSURF)
            VARABL(I,5)=surf_curvature(VBSURF)
            VARABL(I,13)=surf_curvature(VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!
!     DO TH VARIABLE
         IF(VALT.EQ.3) THEN
!
            VARABL(I,4)=surf_thickness(VBSURF)
            VARABL(I,5)=surf_thickness(VBSURF)
            VARABL(I,13)=surf_thickness(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 3, NOT A THICKNESS VARIABLE
         END IF
!     DO CC
         IF(VALT.EQ.4) THEN
!
            VARABL(I,4)=surf_conic(VBSURF)
            VARABL(I,5)=surf_conic(VBSURF)
            VARABL(I,13)=surf_conic(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 4, CC VARIABLE
         END IF
         IF(VALT.EQ.5) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
!
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 4)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 4)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 4)
            VARABL(I,6)=0.0D0
!     VALT NOT 5, AD VARIABLE
         END IF
!
         IF(VALT.EQ.6) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 6)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 6)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 6)
            VARABL(I,6)=0.0D0
!     VALT NOT 6, AE VARIABLE
         END IF
!
         IF(VALT.EQ.7) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 8)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 8)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 8)
            VARABL(I,6)=0.0D0
!     VALT NOT 7, AF VARIABLE
         END IF
!
         IF(VALT.EQ.8) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 10)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 10)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 10)
            VARABL(I,6)=0.0D0
!     VALT NOT 8, AG VARIABLE
         END IF
         IF(VALT.EQ.129) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 12)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 12)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 12)
            VARABL(I,6)=0.0D0
         END IF
         IF(VALT.EQ.130) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 14)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 14)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 14)
            VARABL(I,6)=0.0D0
         END IF
         IF(VALT.EQ.131) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 16)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 16)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 16)
            VARABL(I,6)=0.0D0
         END IF
         IF(VALT.EQ.132) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 18)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 18)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 18)
            VARABL(I,6)=0.0D0
         END IF
         IF(VALT.EQ.133) THEN
            IF(.NOT.surf_is_asphere(VBSURF)) call set_surf_asphere_flag(VBSURF, .TRUE.)
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 20)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 20)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 20)
            VARABL(I,6)=0.0D0
         END IF
!     DO ZD
         IF(VALT.EQ.134) THEN
            VARABL(I,4)=surf_focus_dz(VBSURF)
            VARABL(I,5)=surf_focus_dz(VBSURF)
            VARABL(I,13)=surf_focus_dz(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 134, ZD VARIABLE
         END IF
!     DO INDEX
         IF(VALT.EQ.135) THEN
            VARABL(I,4)=surf_fict_n(VBSURF)
            VARABL(I,5)=surf_fict_n(VBSURF)
            VARABL(I,13)=surf_fict_n(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 135, INDEX VARIABLE
         END IF
!     DO VNUM
         IF(VALT.EQ.136) THEN
            VARABL(I,4)=surf_fict_v(VBSURF)
            VARABL(I,5)=surf_fict_v(VBSURF)
            VARABL(I,13)=surf_fict_v(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 134, ZD VARIABLE
         END IF
!     DO PIVX
         IF(VALT.EQ.137) THEN
            VARABL(I,4)=surf_pivot_x(VBSURF)
            VARABL(I,5)=surf_pivot_x(VBSURF)
            VARABL(I,13)=surf_pivot_x(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 137, PIVX VARIABLE
         END IF
!     DO PIVY
         IF(VALT.EQ.138) THEN
            VARABL(I,4)=surf_pivot_y(VBSURF)
            VARABL(I,5)=surf_pivot_y(VBSURF)
            VARABL(I,13)=surf_pivot_y(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 138, PIVY VARIABLE
         END IF
!     DO PIVZ
         IF(VALT.EQ.139) THEN
            VARABL(I,4)=surf_pivot_z(VBSURF)
            VARABL(I,5)=surf_pivot_z(VBSURF)
            VARABL(I,13)=surf_pivot_z(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 139, PIVZ VARIABLE
         END IF
!     DO DPART
         IF(VALT.EQ.140) THEN
            VARABL(I,4)=surf_fict_w(VBSURF)
            VARABL(I,5)=surf_fict_w(VBSURF)
            VARABL(I,13)=surf_fict_w(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 140, DPART VARIABLE
         END IF
!     DO CLPX
         IF(VALT.EQ.141) THEN
            VARABL(I,4)=surf_clap_dim(VBSURF, 2)
            VARABL(I,5)=surf_clap_dim(VBSURF, 2)
            VARABL(I,13)=surf_clap_dim(VBSURF, 2)
            VARABL(I,6)=0.0D0
!     VALT NOT 141, CLPX VARIABLE
         END IF
!     DO CLPY
         IF(VALT.EQ.142) THEN
            VARABL(I,4)=surf_clap_dim(VBSURF, 1)
            VARABL(I,5)=surf_clap_dim(VBSURF, 1)
            VARABL(I,13)=surf_clap_dim(VBSURF, 1)
            VARABL(I,6)=0.0D0
!     VALT NOT 142, CLPY VARIABLE
         END IF
!     DO GDX
         IF(VALT.EQ.143) THEN
            VARABL(I,4)=surf_global_dx(VBSURF)
            VARABL(I,5)=surf_global_dx(VBSURF)
            VARABL(I,13)=surf_global_dx(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 143, GDX VARIABLE
         END IF
!     DO GDY
         IF(VALT.EQ.144) THEN
            VARABL(I,4)=surf_global_dy(VBSURF)
            VARABL(I,5)=surf_global_dy(VBSURF)
            VARABL(I,13)=surf_global_dy(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 144, GDY VARIABLE
         END IF
!     DO GDZ
         IF(VALT.EQ.145) THEN
            VARABL(I,4)=surf_global_dz(VBSURF)
            VARABL(I,5)=surf_global_dz(VBSURF)
            VARABL(I,13)=surf_global_dz(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 145, GDZ VARIABLE
         END IF
!     DO GALPHA
         IF(VALT.EQ.146) THEN
            VARABL(I,4)=surf_global_alpha(VBSURF)
            VARABL(I,5)=surf_global_alpha(VBSURF)
            VARABL(I,13)=surf_global_alpha(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 146, GALPHA VARIABLE
         END IF
!     DO GBETA
         IF(VALT.EQ.147) THEN
            VARABL(I,4)=surf_global_beta(VBSURF)
            VARABL(I,5)=surf_global_beta(VBSURF)
            VARABL(I,13)=surf_global_beta(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 147, GBETA VARIABLE
         END IF
!     DO GGAMMA
         IF(VALT.EQ.148) THEN
            VARABL(I,4)=surf_global_gamma(VBSURF)
            VARABL(I,5)=surf_global_gamma(VBSURF)
            VARABL(I,13)=surf_global_gamma(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 148, GGAMMA VARIABLE
         END IF
!     DO GRS
         IF(VALT.EQ.149) THEN
            VARABL(I,4)=surf_grating_spacing(VBSURF)
            VARABL(I,5)=surf_grating_spacing(VBSURF)
            VARABL(I,13)=surf_grating_spacing(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 149, GRS VARIABLE
         END IF
!     DO MACVAR
         IF(VALT.EQ.150) THEN
            VARABL(I,4)=GPREG(VBSURF)
            VARABL(I,5)=GPREG(VBSURF)
            VARABL(I,13)=GPREG(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 150, MACVAR VARIABLE
         END IF
!     DO NSSXPOS
         IF(VALT.EQ.4219) THEN
            VARABL(I,4)=NSSALENS(34,VBSURF)
            VARABL(I,5)=NSSALENS(34,VBSURF)
            VARABL(I,13)=NSSALENS(34,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO NSSYPOS
         IF(VALT.EQ.4220) THEN
            VARABL(I,4)=NSSALENS(35,VBSURF)
            VARABL(I,5)=NSSALENS(35,VBSURF)
            VARABL(I,13)=NSSALENS(35,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO NSSZPOS
         IF(VALT.EQ.4221) THEN
            VARABL(I,4)=NSSALENS(36,VBSURF)
            VARABL(I,5)=NSSALENS(36,VBSURF)
            VARABL(I,13)=NSSALENS(36,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO NSSALPH
         IF(VALT.EQ.4222) THEN
            VARABL(I,4)=NSSALENS(40,VBSURF)
            VARABL(I,5)=NSSALENS(40,VBSURF)
            VARABL(I,13)=NSSALENS(40,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO NSSBETA
         IF(VALT.EQ.4223) THEN
            VARABL(I,4)=NSSALENS(41,VBSURF)
            VARABL(I,5)=NSSALENS(41,VBSURF)
            VARABL(I,13)=NSSALENS(41,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO NSSGAMM
         IF(VALT.EQ.4224) THEN
            VARABL(I,4)=NSSALENS(42,VBSURF)
            VARABL(I,5)=NSSALENS(42,VBSURF)
            VARABL(I,13)=NSSALENS(42,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO V1
         IF(VALT.EQ.4225) THEN
            VARABL(I,4)=NSSALENS(3,VBSURF)
            VARABL(I,5)=NSSALENS(3,VBSURF)
            VARABL(I,13)=NSSALENS(3,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO V2
         IF(VALT.EQ.4226) THEN
            VARABL(I,4)=NSSALENS(4,VBSURF)
            VARABL(I,5)=NSSALENS(4,VBSURF)
            VARABL(I,13)=NSSALENS(4,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO V3
         IF(VALT.EQ.4227) THEN
            VARABL(I,4)=NSSALENS(5,VBSURF)
            VARABL(I,5)=NSSALENS(5,VBSURF)
            VARABL(I,13)=NSSALENS(5,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO V4
         IF(VALT.EQ.4228) THEN
            VARABL(I,4)=NSSALENS(6,VBSURF)
            VARABL(I,5)=NSSALENS(6,VBSURF)
            VARABL(I,13)=NSSALENS(6,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO V5
         IF(VALT.EQ.4229) THEN
            VARABL(I,4)=NSSALENS(7,VBSURF)
            VARABL(I,5)=NSSALENS(7,VBSURF)
            VARABL(I,13)=NSSALENS(7,VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!     DO P001 TO P200
         IF(VALT.GE.4230.AND.VALT.LE.4429) THEN
            VARABL(I,4)=NSSALENS((VALT-4229),VBSURF)
            VARABL(I,5)=NSSALENS((VALT-4229),VBSURF)
            VARABL(I,13)=NSSALENS((VALT-4229),VBSURF)
            VARABL(I,6)=0.0D0
         END IF
!
!     DO CVTOR
         IF(VALT.EQ.10.OR.VALT.EQ.9) THEN
            VARABL(I,4)=surf_toric_curvature(VBSURF)
            VARABL(I,5)=surf_toric_curvature(VBSURF)
            VARABL(I,13)=surf_toric_curvature(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 10, NOT A OR CVTOR VARIABLE
         END IF
!
!     DO CCTOR
         IF(VALT.EQ.11) THEN
            VARABL(I,4)=surf_anamorphic_conic(VBSURF)
            VARABL(I,5)=surf_anamorphic_conic(VBSURF)
            VARABL(I,13)=surf_anamorphic_conic(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 11, CCTOR VARIABLE
         END IF
!
!     DO ADTOR
         IF(VALT.EQ.12) THEN
            VARABL(I,4)=surf_anamorphic_coeff(VBSURF, 4)
            VARABL(I,5)=surf_anamorphic_coeff(VBSURF, 4)
            VARABL(I,13)=surf_anamorphic_coeff(VBSURF, 4)
            VARABL(I,6)=0.0D0
!     VALT NOT 12, ADTOR VARIABLE
         END IF
!
!     DO AETOR
         IF(VALT.EQ.13) THEN
            VARABL(I,4)=surf_anamorphic_coeff(VBSURF, 6)
            VARABL(I,5)=surf_anamorphic_coeff(VBSURF, 6)
            VARABL(I,13)=surf_anamorphic_coeff(VBSURF, 6)
            VARABL(I,6)=0.0D0
!     VALT NOT 13, AETOR VARIABLE
         END IF
!
!     DO AFTOR
         IF(VALT.EQ.14) THEN
            VARABL(I,4)=surf_anamorphic_coeff(VBSURF, 8)
            VARABL(I,5)=surf_anamorphic_coeff(VBSURF, 8)
            VARABL(I,13)=surf_anamorphic_coeff(VBSURF, 8)
            VARABL(I,6)=0.0D0
!     VALT NOT 14, AFTOR VARIABLE
         END IF
!
!     DO AGTOR
         IF(VALT.EQ.15) THEN
            VARABL(I,4)=surf_anamorphic_coeff(VBSURF, 10)
            VARABL(I,5)=surf_anamorphic_coeff(VBSURF, 10)
            VARABL(I,13)=surf_anamorphic_coeff(VBSURF, 10)
            VARABL(I,6)=0.0D0
!     VALT NOT 15, AGTOR VARIABLE
         END IF
!
!     DO ALPHA
         IF(VALT.EQ.16) THEN
            VARABL(I,4)=surf_alpha_deg(VBSURF)
            VARABL(I,5)=surf_alpha_deg(VBSURF)
            VARABL(I,13)=surf_alpha_deg(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 16, ALPHA VARIABLE
         END IF
!
!     DO BETA
         IF(VALT.EQ.17) THEN
            VARABL(I,4)=surf_beta_deg(VBSURF)
            VARABL(I,5)=surf_beta_deg(VBSURF)
            VARABL(I,13)=surf_beta_deg(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 17, BETA VARIABLE
         END IF
!
!     DO GAMMA
         IF(VALT.EQ.18) THEN
            VARABL(I,4)=surf_gamma_deg(VBSURF)
            VARABL(I,5)=surf_gamma_deg(VBSURF)
            VARABL(I,13)=surf_gamma_deg(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 18, GAMMA VARIABLE
         END IF
!
!     DO XD
         IF(VALT.EQ.19) THEN
            VARABL(I,4)=surf_focus_dx(VBSURF)
            VARABL(I,5)=surf_focus_dx(VBSURF)
            VARABL(I,13)=surf_focus_dx(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 19, XD VARIABLE
         END IF
!
!     DO YD
         IF(VALT.EQ.20) THEN
            VARABL(I,4)=surf_focus_dy(VBSURF)
            VARABL(I,5)=surf_focus_dy(VBSURF)
            VARABL(I,13)=surf_focus_dy(VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 20, YD VARIABLE
         END IF
!
!     DO N1
         IF(VALT.EQ.21) THEN
            VARABL(I,4)=surf_refractive_index(VBSURF, 1)
            VARABL(I,5)=surf_refractive_index(VBSURF, 1)
            VARABL(I,13)=surf_refractive_index(VBSURF, 1)
            VARABL(I,6)=0.0D0
!     VALT NOT 21, N1 VARIABLE
         END IF
!
!     DO N2
         IF(VALT.EQ.22) THEN
            VARABL(I,4)=surf_refractive_index(VBSURF, 2)
            VARABL(I,5)=surf_refractive_index(VBSURF, 2)
            VARABL(I,13)=surf_refractive_index(VBSURF, 2)
            VARABL(I,6)=0.0D0
!     VALT NOT 22, N2 VARIABLE
         END IF
!
!     DO N3
         IF(VALT.EQ.23) THEN
            VARABL(I,4)=surf_refractive_index(VBSURF, 3)
            VARABL(I,5)=surf_refractive_index(VBSURF, 3)
            VARABL(I,13)=surf_refractive_index(VBSURF, 3)
            VARABL(I,6)=0.0D0
!     VALT NOT 23, N3 VARIABLE
         END IF
!
!     DO N4
         IF(VALT.EQ.24) THEN
            VARABL(I,4)=surf_refractive_index(VBSURF, 4)
            VARABL(I,5)=surf_refractive_index(VBSURF, 4)
            VARABL(I,13)=surf_refractive_index(VBSURF, 4)
            VARABL(I,6)=0.0D0
!     VALT NOT 24, N4 VARIABLE
         END IF
!
!     DO N5
         IF(VALT.EQ.25) THEN
            VARABL(I,4)=surf_refractive_index(VBSURF, 5)
            VARABL(I,5)=surf_refractive_index(VBSURF, 5)
            VARABL(I,13)=surf_refractive_index(VBSURF, 5)
            VARABL(I,6)=0.0D0
!     VALT NOT 25, N5 VARIABLE
         END IF
!
!     NOW DO THE COEFFS
!
!     DO C1 TO C48
         IF(VALT.GE.27.AND.VALT.LE.74) THEN
            VARABL(I,4)=FTFL01((VALT-26),VBSURF)
            VARABL(I,5)=FTFL01((VALT-26),VBSURF)
            VARABL(I,13)=FTFL01((VALT-26),VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 27 TO 74, C1 - C48 VARIABLE
         END IF
!     DO C49 TO C96
         IF(VALT.GE.76.AND.VALT.LE.123) THEN
            VARABL(I,4)=FTFL01((VALT-27),VBSURF)
            VARABL(I,5)=FTFL01((VALT-27),VBSURF)
            VARABL(I,13)=FTFL01((VALT-27),VBSURF)
            VARABL(I,6)=0.0D0
!     VALT NOT 76 TO 123, C49 - C96 VARIABLE
         END IF
!
!     DO AC
         IF(VALT.EQ.75) THEN
            VARABL(I,4)=surf_asphere_coeff(VBSURF, 2)
            VARABL(I,5)=surf_asphere_coeff(VBSURF, 2)
            VARABL(I,13)=surf_asphere_coeff(VBSURF, 2)
            VARABL(I,6)=0.0D0
!     VALT NOT 75, AC VARIABLE
         END IF
!
!     NOW BOUNDS CHECKER
         IF(VARABL(I,4).LT.VARABL(I,9)) THEN
            VARABL(I,4)=VARABL(I,9)
            WRITE(OUTLYNE,*)'WARNING: '
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
            CALL SHOWIT(1)
         END IF
         IF(VARABL(I,4).GT.VARABL(I,10)) THEN
            VARABL(I,4)=VARABL(I,10)
            WRITE(OUTLYNE,*)'WARNING: '
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
            CALL SHOWIT(1)
         END IF
         VARABL(I,5)=VARABL(I,4)
         VARABL(I,13)=VARABL(I,4)
!
      ELSE
!     NOT CFG 1
         VADD=INT(VARABL(I,14))
         VBSURF=INT(VARABL(I,3))
         VBCFG=INT(VARABL(I,2))
         VALT=INT(VARABL(I,1))
         IF(VBCFG.EQ.0.OR.CFGCNT(VBCFG).EQ.0) THEN
            GO TO 200
         END IF
!     USE AUXCFG ARRAY
!     FIRST WE SEE IF THE CONFIG CALLED FOR IS AN ACTIVE (I.E.
!     SOMETHING IN THAT CONFIG) CONFIGURATION.
!
!     THEN WE CHECK TO SEE IF THE VARIABLE IS IN THE
!     CONFIG SUBFILE DEFINITION FOR THAT CONFIGURATION
!
         J=INT(VARABL(I,11))
!
!     J IS THE LOCATION IN THE AUXCFG ARRAYS WHERE THIS VARIABLE
!     IS LOCATED
!
         IF(VALT.GE.27.AND.VALT.LE.74.OR.VALT.GE.76.AND.VALT.LE.123 .OR.VALT.EQ.141) THEN
            VARABL(I,4) =CFVAL(J,2)
            VARABL(I,5) =CFVAL(J,2)
            VARABL(I,13) =CFVAL(J,2)
            VARABL(I,6) =0.0D0
!
!     BOUNDS CHECKER
            IF(VARABL(I,4).LT.VARABL(I,9)) THEN
               VARABL(I,4)=VARABL(I,9)
               WRITE(OUTLYNE,*)'WARNING: '
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
               CALL SHOWIT(1)
            END IF
            IF(VARABL(I,4).GT.VARABL(I,10)) THEN
               VARABL(I,4)=VARABL(I,10)
               WRITE(OUTLYNE,*)'WARNING: '
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
               CALL SHOWIT(1)
            END IF
            VARABL(I,5)=VARABL(I,4)
            VARABL(I,13)=VARABL(I,4)
!
         ELSE
            IF(CFADD(J,1).EQ.1.AND.CFVAL(J,1).NE.0.0D0.OR.CFADD(J,1).EQ.9.AND.CFVAL(J,1).NE.0.0D0) THEN
               VARABL(I,4) =1.0D0/CFVAL(J,1)
               VARABL(I,5) =1.0D0/CFVAL(J,1)
               VARABL(I,13) =1.0D0/CFVAL(J,1)
               VARABL(I,6) =0.0D0
            ELSE
               VARABL(I,4) =CFVAL(J,1)
               VARABL(I,5) =CFVAL(J,1)
               VARABL(I,13) =CFVAL(J,1)
               VARABL(I,6) =0.0D0
            END IF
!
!     BOUNDS CHECKER
            IF(VARABL(I,4).LT.VARABL(I,9)) THEN
               VARABL(I,4)=VARABL(I,9)
               WRITE(OUTLYNE,*)'WARNING: '
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'VARIABLE # ',I,' HAS REACHED ITS LOWER BOUNDARY VALUE'
               CALL SHOWIT(1)
            END IF
            IF(VARABL(I,4).GT.VARABL(I,10)) THEN
               VARABL(I,4)=VARABL(I,10)
               WRITE(OUTLYNE,*)'WARNING: '
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'VARIABLE # ',I,' HAS REACHED ITS UPPER BOUNDARY VALUE'
               CALL SHOWIT(1)
            END IF
            VARABL(I,5)=VARABL(I,4)
            VARABL(I,13)=VARABL(I,4)
!
         END IF
         VARABL(I,6)=0.0D0
      END IF
200   CONTINUE
   END DO
!       ALL DONE
   PRINT *, "FIXVAR ROUTINE ENDED!"
   RETURN
END
SUBROUTINE FIELDS
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_wl_ref
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER AVAL3*3
   INTEGER I
   AVAL3(1:3)=WC(2:4)
   CALL ATOII(AVAL3,I)
   IF(is_command_query()) THEN
      OUTLYNE='"'//WC//'" SETS A FIELD OF VIEW FOR OPTIMIZATION'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
      OUTLYNE='CURRENT SETTINGS FOR "'//WC//'" ARE:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,110) FIELDY(I)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,111) FIELDX(I)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,112) FIELDZ(I)
      CALL SHOWIT(1)
      WRITE(OUTLYNE,113) INT(FIELDW(I))
      CALL SHOWIT(1)
110   FORMAT(' Y-FOB = ',D23.15)
111   FORMAT(' X-FOB = ',D23.15)
112   FORMAT(' Z-POS = ',D23.15)
113   FORMAT(' WAV#  = ',I2)
      RETURN
   END IF
   IF(SQ.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"'//WC//'" TAKES NO QUALIFIER WORD INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"'//WC//'" TAKES NO NUMERIC WORD #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SST.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"'//WC//'" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF4.EQ.1) THEN
      W4=sys_wl_ref()
      DF4=0
   END IF
   IF(W4.NE.1.0D0.AND.W4.NE.2.0D0.AND.W4.NE.3.0D0.AND.W4.NE.4.0D0 .AND.W4.NE.5.0D0.AND.W4.NE.6.0D0.AND.W4.NE.7.0D0.AND.W4.NE.8.0D0.AND.W4.NE.9.0D0.AND.W4.NE.10.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'WAVELENGTH # MUST BE 1,2,3,4,5,6,7,8,9 OR 10'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1) W1=0.0D0
   IF(DF2.EQ.1) W2=0.0D0
   IF(DF3.EQ.1) W3=0.0D0
   FIELDY(I)=W1
   FIELDX(I)=W2
   FIELDZ(I)=W3
   FIELDW(I)=DBLE(INT(W4))
   RETURN
END
SUBROUTINE MAKE_DEF_AUTO
   use DATSP1
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_mode, sys_wl_ref
   use command_utils, only: is_command_query
   INTEGER IVAL,I,J,K,DFNRD,L,LL,LLL,LLLL,DFNRAYS
   real(real64) JK_WW1,JK_WW2,VL,XPOS,YPOS,STEP,WT1,WT2,WAY,THETA
   real(real64) TESTLENGTH,LAST_TESTLENGTH,YSTART_POS,XSTART_POS
   real(real64) THETA_VAL1
   CHARACTER AV1*23,AV2*23,AVL*23,ACFG*3,AI3*3,AI4*4,AV0*4,AV4*4
   CHARACTER AXPOS*23,AYPOS*23
   COMMON/JK_ATD/AVL,VL
   real(real64) RADIUS_VAL(1:20)
   COMMON/VALRAD/RADIUS_VAL
!
   IF(WC.NE.'DFGRID') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"'//TRIM(WC)//'" TAKES NO STRING OR QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   ELSE
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"'//TRIM(WC)//'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!
!     WC=MONO OR POLY
!
   IF(WC.EQ.'MONO') THEN
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"MONO" SETS AUTOMATIC MERIT FUNCTION BUILDING TO BE RESTRICTED'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'TO THE CONTROL WAVELENGTH ONLY. IT IS THE DEFAULT'
         CALL SHOWIT(1)
         IF(CHROMATIC) WRITE(OUTLYNE,*)'"POLY" IS CURRENTLY IN EFFECT'
         IF(.NOT.CHROMATIC) WRITE(OUTLYNE,*)'"MONO" IS CURRENTLY IN EFFECT'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"MONO" ONLY TAKES NO INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      CHROMATIC=.FALSE.
      RETURN
   END IF
   IF(WC.EQ.'POLY') THEN
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"POLY" SETS AUTOMATIC MERIT FUNCTION BUILDING TO BE RESTRICTED'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'TO POLYCHROMATIC MODE ONLY'
         CALL SHOWIT(1)
         IF(CHROMATIC) WRITE(OUTLYNE,*)'"POLY" IS CURRENTLY IN EFFECT'
         IF(.NOT.CHROMATIC) WRITE(OUTLYNE,*)'"MONO" IS CURRENTLY IN EFFECT'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"POLY" ONLY TAKES NO INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      CHROMATIC=.TRUE.
      RETURN
   END IF
!
!     WC=DFDEL
!
   IF(WC.EQ.'DFDEL') THEN
      IF(DFGRID.EQ.1) THEN
         WRITE(OUTLYNE,*) 'CURRENT GRID SETTING IS "HEX"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'"DFDEL" REQUIRES THE GRID TYPE TO BE SET TO "RECT"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"DFDEL" SETS DEFAULT AUTO MERIT FUNCTION RAY GRID SPACING'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'DFDEL CURRENTLY SET TO ',DFDEL
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"DFDEL" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0) THEN
         WRITE(OUTLYNE,*)'"DFDEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(W1.GT.1.414213D0.OR.W1.LT.0.1054D0) THEN
         WRITE(OUTLYNE,*)'"DFDEL" MAX ALLOWED VALUE IS: 1.414213'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'"DFDEL" MIN ALLOWED VALUE IS: 0.1054'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      DFDEL=W1
      RETURN
   END IF
!
!
!     WC=DFHEX
!
   IF(WC.EQ.'DFHEX') THEN
      IF(DFGRID.EQ.2) THEN
         WRITE(OUTLYNE,*) 'CURRENT GRID SETTING IS "RECT"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'"DFHEX" REQUIRES THE GRID TYPE TO BE SET TO "HEX"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*) 'NO ACTION TAKEN'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
!     SYNTAX CHECK
      IF(is_command_query().OR.SQ.EQ.0.AND.SN.EQ.0.AND.SST.EQ.0) THEN
         WRITE(OUTLYNE,*)      'NUMBER OF RINGS IS SET TO ',DFRIN
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'NUMBER OF PIE SECTORS IS SET TO ',DFSEC
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"DFHEX" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.1) THEN
         IF(W1.LT.1.0D0) THEN
            WRITE(OUTLYNE,*)'MINIMUN NUMBER OF RADIAL RINGS IS 1'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(W1.GT.20.0D0) THEN
            WRITE(OUTLYNE,*)'MAXIMUM NUMBER OF RADIAL RINGS IS 20'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(S2.EQ.1) THEN
         IF(W2.LT.4.0D0) THEN
            WRITE(OUTLYNE,*)'MINIMUN NUMBER OF PIE SECTORS IS 4'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(W2.GT.32.0D0) THEN
            WRITE(OUTLYNE,*)'MAXIMUM NUMBER OF PIE SECTORS IS 32'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(DF1.EQ.0) DFRIN=INT(W1)
      IF(DF2.EQ.0) DFSEC=INT(W2)
      RETURN
   END IF
!
!
!     WC=DFGRID
!
   IF(WC.EQ.'DFGRID') THEN
!     SYNTAX CHECK
      IF(is_command_query().OR.SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)'"DFGRID" SETS DEFAULT AUTO MERIT FUNCTION RAY GRID SHAPE'
         CALL SHOWIT(1)
         IF(DFGRID.EQ.1)WRITE(OUTLYNE,*)'DFGRID CURRENTLY SET TO "HEX"'
         IF(DFGRID.EQ.2)WRITE(OUTLYNE,*)'DFGRID CURRENTLY SET TO "RECT"'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"DFGRID" ONLY TAKES QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'HEX'.AND.WQ.NE.'RECT') THEN
         WRITE(OUTLYNE,*)'"DFGRID" REQUIRES "HEX" OR "RECT"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'AS QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'HEX') DFGRID=1
      IF(WQ.EQ.'RECT') DFGRID=2
      RETURN
   END IF
!
!     WC=DFP
!
   IF(WC.EQ.'DFP') THEN
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"DFP" SETS DEFAULT AUTO MERIT FUNCTION NUMBERS OF FIELD OF VIEW'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'DFP CURRENTLY SET TO ',DFPNUMB
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"DFP" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0) THEN
         WRITE(OUTLYNE,*)'"DFP" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W1).LT.1.OR.INT(W1).GT.25) THEN
         WRITE(OUTLYNE,*)'"DFP" MUST BE AN INTEGER FROM 1 TO 25'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      DFPNUMB=INT(W1)
      RETURN
   END IF
!
!     WC=DFTYPE
!
   IF(WC.EQ.'DFTYPE') THEN
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"DFTYPE" SETS DEFAULT AUTO MERIT FUNCTION OPERAND TYPE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'DFTYPE CURRENTLY SET TO ',DFTYPENUMB
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(S5.EQ.1.OR.S4.EQ.1) THEN
         WRITE(OUTLYNE,*)'"DFTYPE" TAKES NO NUMERIC WORDS #4 OR #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0) THEN
         WRITE(OUTLYNE,*)'"DFTYPE" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W1).LT.1.OR.INT(W1).GT.2) THEN
         WRITE(OUTLYNE,*)'"DFTYPE" MUST BE 1(TRANSVERSE ONLY) OR '
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'2(TRANSVERSE PLUS OPDS)'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(DF2.EQ.1) W2=1.0D0
      IF(DF3.EQ.1) W3=1.0D0
      W2=DABS(W2)
      W3=DABS(W3)
      DFWT1=W2
      DFWT2=W3
      DFTYPENUMB=INT(W1)
      RETURN
   END IF
!
!     WC=FP
!
   IF(WC.EQ.'FP') THEN
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"FP" DEFINES FOBS FOR THE DEFAULT AUTO MERIT FUNCTION'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
         WRITE(OUTLYNE,*)'"FP" REQUIRES EXPLICIT NUMERIC WORDS 1, 2 AND 3'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W1).LT.1.OR.INT(W1).GT.DFPNUMB) THEN
         WRITE(OUTLYNE,*)'FIELD OF VIEW POSITION MUST BE FROM 1 TO ',DFPNUMB
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(DF5.EQ.1) W5=sys_wl_ref()
      IF(INT(W5).LT.1.OR.INT(W5).GT.10) THEN
         WRITE(OUTLYNE,*)'WAVELENGTH NUMBER MUST BE FROM 1 TO 10'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      W4=DABS(W4)
      IF(DF4.EQ.1) W4=1.0D0
      IF(DF5.EQ.1) W5=1.0D0
      DEFAULT_FOB(1,INT(W1))=W2
      DEFAULT_FOB(2,INT(W1))=W3
      DEFAULT_FOB(3,INT(W1))=W4
      DEFAULT_FOB(4,INT(W1))=W5
      RETURN
   END IF
!
!     WC=MAKEAUTO
!
   IF(WC.EQ.'MAKEAUTO') THEN
      IF(DFGRID.EQ.2) THEN
!       GRID SHAPE RECTANGULAR
!     SYNTAX CHECK
         IF(is_command_query()) THEN
            WRITE(OUTLYNE,*)'"MAKEAUTO" CREATES DEFAULT AUTO MERIT FUNCTION'
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)'"MAKEAUTO" ONLY TAKES NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(S1.EQ.0) W1=1.0D0
         IF(INT(W1).LT.1.OR.INT(W1).GT.MAXCFG) THEN
            WRITE(OUTLYNE,*)'CONFIGURATION NUMBER MUST BE FROM 1 TO ',MAXCFG
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         DF_CFG=INT(W1)
!
!     RECTANGULAR GRID OF RAYS
!     CALCULATE DFNRD
         LAST_TESTLENGTH=0.0D0
         TESTLENGTH=0.0D0
         DO I=0,200
            TESTLENGTH=DSQRT(((DFDEL/2.0D0)**2)+(((DFDEL/2.0D0)+((DFDEL)*DBLE(I)))**2))
            IF(LAST_TESTLENGTH.LE.1.0D0.AND.TESTLENGTH.GT.1.0D0) THEN
               DFNRD=((I))*2
               GO TO 10
            ELSE
!     PROCEED WITH NEXT CYCLE
               LAST_TESTLENGTH=TESTLENGTH
            END IF
         END DO
10       CONTINUE
         XSTART_POS=(DFDEL/2.0D0)+(DFDEL*DBLE((DFNRD/2)-1))
         YSTART_POS=(DFDEL/2.0D0)+(DFDEL*DBLE((DFNRD/2)-1))
!     RESET FIELDS AND RAYS TO THE DEFAULT VALUES
         SAVE_KDP(14)=SAVEINPT(14)
         INPUT='FIELDS RESET'
         CALL PROCES
         REST_KDP(14)=RESTINPT(14)
         SAVE_KDP(14)=SAVEINPT(14)
         INPUT='RAYS RESET'
         CALL PROCES
         REST_KDP(14)=RESTINPT(14)
!     REDEFINE THE FIELD OF VIEW POSITIONS
         DO I=1,DFPNUMB
            CALL ITOAAA(I,AI4)
            AV0=AI4
            VL=DEFAULT_FOB(1,I)
            CALL DTOACV
            AV1=AVL
            VL=DEFAULT_FOB(2,I)
            CALL DTOACV
            AV2=AVL
            VL=DEFAULT_FOB(4,I)
            CALL DTOACV
            AV4=AVL
            INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
            CALL PROCES
            INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
         END DO
!     REDEFINE THE RAY POSITIONS
         IF(CHROMATIC) THEN
            LLLL=0
            K=0
            DO L=1,10
               IF(L.EQ.1)  LL=31
               IF(L.EQ.2)  LL=32
               IF(L.EQ.3)  LL=33
               IF(L.EQ.4)  LL=34
               IF(L.EQ.5)  LL=35
               IF(L.EQ.6)  LL=76
               IF(L.EQ.7)  LL=77
               IF(L.EQ.8)  LL=78
               IF(L.EQ.9)  LL=79
               IF(L.EQ.10) LL=80
               IF(SYSTEM(LL).NE.0.0D0) THEN
                  LLL=L
                  LLLL=LLLL+1
!     DO A WAVELENGTH
                  STEP=DFDEL
                  YPOS=-XSTART_POS
                  DO I=1,DFNRD
                     XPOS=-YSTART_POS
                     DO J=1,DFNRD
                        K=K+1
                        CALL ITOAAA(K,AI4)
                        AV0=AI4
                        VL=XPOS
                        CALL DTOACV
                        AXPOS=AVL
                        VL=YPOS
                        CALL DTOACV
                        AYPOS=AVL
                        CALL ITOAAA(LLL,AI4)
                        AV4=AI4
                        INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                        CALL PROCES
                        XPOS=XPOS+STEP
                     END DO
                     YPOS=YPOS+STEP
                  END DO
               END IF
            END DO
         ELSE
!       JUST DO THE CONTROL WAVELENGTH
            LLLL=1
            K=0
            L=INT(sys_wl_ref())
            IF(L.EQ.1)  LL=31
            IF(L.EQ.2)  LL=32
            IF(L.EQ.3)  LL=33
            IF(L.EQ.4)  LL=34
            IF(L.EQ.5)  LL=35
            IF(L.EQ.6)  LL=76
            IF(L.EQ.7)  LL=77
            IF(L.EQ.8)  LL=78
            IF(L.EQ.9)  LL=79
            IF(L.EQ.10) LL=80
            LLL=L
!     DO A WAVELENGTH
            STEP=DFDEL
            YPOS=-XSTART_POS
            DO I=1,DFNRD
               XPOS=-YSTART_POS
               DO J=1,DFNRD
                  K=K+1
                  CALL ITOAAA(K,AI4)
                  AV0=AI4
                  VL=XPOS
                  CALL DTOACV
                  AXPOS=AVL
                  VL=YPOS
                  CALL DTOACV
                  AYPOS=AVL
                  CALL ITOAAA(LLL,AI4)
                  AV4=AI4
                  INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                  CALL PROCES
                  XPOS=XPOS+STEP
               END DO
               YPOS=YPOS+STEP
            END DO
         END IF
!     SET UP THE OPERANDS AND SKIP BLOCKED OPERANDS
         WRITE(OUTLYNE,*) 'SETTING UP OPERAND DEFINITIONS...'
         CALL SHOWIT(1)
!     UPDATE MERIT
         SAVE_KDP(14)=SAVEINPT(14)
         INPUT='UPDATE MERIT'
         CALL PROCES
         REST_KDP(14)=RESTINPT(14)
!     CFG AS NECESSARY
         IF(DF_CFG.NE.1) THEN
            SAVE_KDP(14)=SAVEINPT(14)
            IVAL=DF_CFG
            CALL NTOAN1(IVAL,ACFG)
            INPUT='CFG '//ACFG
            CALL PROCES
            REST_KDP(14)=RESTINPT(14)
         END IF
         DO I=1,DFPNUMB
            DO J=1,(DFNRD**2)*LLLL
               JK_WW1=RAYY(J)
               JK_WW2=RAYX(J)
               WT1=DFWT1*DEFAULT_FOB(3,I)
               WT2=DFWT2*DEFAULT_FOB(3,I)
!     TYPE ONE MERIT FUNCTION
               IF(DFTYPENUMB.EQ.1) THEN
                  IF(surf_clap_type(NEWREF).EQ.2.0D0.AND.surf_multi_clap_flag(NEWREF).EQ.0.0D0) THEN
!     SQUARE OPERAND PATTERN
                     IF(sys_mode().LE.2) THEN
!     FOCAL
                        WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                        CALL PROCES
                     ELSE
!     AFOCAL
                        WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                        CALL PROCES
                     END IF
                  ELSE
!     CIRCULAR OPERAND PATTERN
                     IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
!     ADD OPERAND
                        IF(sys_mode().LE.2) THEN
!     FOCAL
                           WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                           CALL PROCES
                           WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                           CALL PROCES
                        ELSE
!     AFOCAL
                           WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                           CALL PROCES
                           WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                           CALL PROCES
                        END IF
                     ELSE
!     DON'T ADD OPERAND
                     END IF
                  END IF
               END IF
!     TYPE TWO MERIT FUNCTION
               IF(DFTYPENUMB.EQ.2) THEN
                  IF(surf_clap_type(NEWREF).EQ.2.0D0.AND.surf_multi_clap_flag(NEWREF).EQ.0.0D0) THEN
!     SQUARE OPERAND PATTERN
                     IF(sys_mode().LE.2) THEN
!     FOCAL
                        WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                        CALL PROCES
                     ELSE
!     AFOCAL
                        WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                        CALL PROCES
                     END IF
                  ELSE
!     CIRCULAR OPERAND PATTERN
                     IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
!     ADD OPERAND
                        IF(sys_mode().LE.2) THEN
!     FOCAL
                           WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                           CALL PROCES
                           WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                           CALL PROCES
                           WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                           CALL PROCES
                        ELSE
!     AFOCAL
                           WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                           CALL PROCES
                           WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                           CALL PROCES
                           WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                           CALL PROCES
                        END IF
                     ELSE
!     DON'T ADD OPERAND
                     END IF
                  END IF
               END IF
            END DO
         END DO
!
!     EOS
         SAVE_KDP(14)=SAVEINPT(14)
         INPUT='EOS'
         CALL PROCES
         WRITE(OUTLYNE,*) 'MERIT FUNCTION DEFINITION COMPLETED'
         CALL SHOWIT(1)
         REST_KDP(14)=RESTINPT(14)
         RETURN
      END IF
   END IF
!
   IF(DFGRID.EQ.1) THEN
!     HEXAPOLAR GRID
!     SYNTAX CHECK
      IF(is_command_query()) THEN
         WRITE(OUTLYNE,*)'"MAKEAUTO" CREATES DEFAULT AUTO MERIT FUNCTION'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"MAKEAUTO" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0) W1=1.0D0
      IF(INT(W1).LT.1.OR.INT(W1).GT.MAXCFG) THEN
         WRITE(OUTLYNE,*)'CONFIGURATION NUMBER MUST BE FROM 1 TO ',MAXCFG
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      DF_CFG=INT(W1)
!       COMPUTE AND SAVE RAY RING RADII
!
      DO I=1,DFRIN
         RADIUS_VAL(I)=1.0D0/DSQRT(DBLE(I))
      END DO
!       RAYS START AT Y=YMAX, X=0 AND PROCEED CLOCKWISE AROUNT EACH
!       RING
!
      THETA_VAL1=2.0D0*PII/DBLE(DFSEC)
!     RESET FIELDS AND RAYS TO THE DEFAULT VALUES
      SAVE_KDP(14)=SAVEINPT(14)
      INPUT='FIELDS RESET'
      CALL PROCES
      REST_KDP(14)=RESTINPT(14)
      SAVE_KDP(14)=SAVEINPT(14)
      INPUT='RAYS RESET'
      CALL PROCES
      REST_KDP(14)=RESTINPT(14)
!     REDEFINE THE FIELD OF VIEW POSITIONS
      DO I=1,DFPNUMB
         CALL ITOAAA(I,AI4)
         AV0=AI4
         VL=DEFAULT_FOB(1,I)
         CALL DTOACV
         AV1=AVL
         VL=DEFAULT_FOB(2,I)
         CALL DTOACV
         AV2=AVL
         VL=DEFAULT_FOB(4,I)
         CALL DTOACV
         AV4=AVL
         INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
         CALL PROCES
         INPUT ='F'//AV0//','//AV1//','//AV2//','//AV4
      END DO
!     REDEFINE THE RAY POSITIONS
      IF(CHROMATIC) THEN
         LLLL=0
         K=0
         DO L=1,10
            IF(L.EQ.1)  LL=31
            IF(L.EQ.2)  LL=32
            IF(L.EQ.3)  LL=33
            IF(L.EQ.4)  LL=34
            IF(L.EQ.5)  LL=35
            IF(L.EQ.6)  LL=76
            IF(L.EQ.7)  LL=77
            IF(L.EQ.8)  LL=78
            IF(L.EQ.9)  LL=79
            IF(L.EQ.10) LL=80
            IF(SYSTEM(LL).NE.0.0D0) THEN
               LLL=L
               LLLL=LLLL+1
!     DO A WAVELENGTH

!       ZERO ANGLE STARTS AT NOON AND PROCEEDS COUNTER-CLOCKWISE
               DO I=1,DFRIN
                  DO J=1,DFSEC
!       COMPUTE XPOS AND YPOS
                     THETA=DBLE(J-1)*THETA_VAL1
                     XPOS=RADIUS_VAL(I)*DCOS(THETA)
                     YPOS=RADIUS_VAL(I)*DSIN(THETA)
                     K=K+1
                     CALL ITOAAA(K,AI4)
                     AV0=AI4
                     VL=XPOS
                     CALL DTOACV
                     AXPOS=AVL
                     VL=YPOS
                     CALL DTOACV
                     AYPOS=AVL
                     CALL ITOAAA(LLL,AI4)
                     AV4=AI4
                     INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
                     CALL PROCES
                  END DO
               END DO
            END IF
         END DO
      ELSE
!       JUST DO THE CONTROL WAVELENGTH
         LLLL=1
         K=0
         L=INT(sys_wl_ref())
         IF(L.EQ.1)  LL=31
         IF(L.EQ.2)  LL=32
         IF(L.EQ.3)  LL=33
         IF(L.EQ.4)  LL=34
         IF(L.EQ.5)  LL=35
         IF(L.EQ.6)  LL=76
         IF(L.EQ.7)  LL=77
         IF(L.EQ.8)  LL=78
         IF(L.EQ.9)  LL=79
         IF(L.EQ.10) LL=80
         LLL=L

!     DO A WAVELENGTH

!       ZERO ANGLE STARTS AT 3 pm, PROCEEDS COUNTER-CLOCKWISE
         DO I=1,DFRIN
            DO J=1,DFSEC
!       COMPUTE XPOS AND YPOS
               THETA=DBLE(J-1)*THETA_VAL1
               XPOS=RADIUS_VAL(I)*DCOS(THETA)
               YPOS=RADIUS_VAL(I)*DSIN(THETA)
               K=K+1
               CALL ITOAAA(K,AI4)
               AV0=AI4
               VL=XPOS
               CALL DTOACV
               AXPOS=AVL
               VL=YPOS
               CALL DTOACV
               AYPOS=AVL
               CALL ITOAAA(LLL,AI4)
               AV4=AI4
               INPUT ='R'//AV0//','//AYPOS//','//AXPOS//','//AV4
               CALL PROCES
            END DO
         END DO
      END IF

!     SET UP THE OPERANDS AND SKIP BLOCKED OPERANDS
      WRITE(OUTLYNE,*) 'SETTING UP OPERAND DEFINITIONS...'
      CALL SHOWIT(1)
!     UPDATE MERIT
      SAVE_KDP(14)=SAVEINPT(14)
      INPUT='UPDATE MERIT'
      CALL PROCES
      REST_KDP(14)=RESTINPT(14)
!     CFG AS NECESSARY
      IF(DF_CFG.NE.1) THEN
         SAVE_KDP(14)=SAVEINPT(14)
         IVAL=DF_CFG
         CALL NTOAN1(IVAL,ACFG)
         INPUT='CFG '//ACFG
         CALL PROCES
         REST_KDP(14)=RESTINPT(14)
      END IF
      DO I=1,DFPNUMB

         DO J=1,(DFGRID*DFSEC)*LLLL
            JK_WW1=RAYY(J)
            JK_WW2=RAYX(J)
            WT1=DFWT1*DEFAULT_FOB(3,I)
            WT2=DFWT2*DEFAULT_FOB(3,I)
!     TYPE ONE MERIT FUNCTION
            IF(DFTYPENUMB.EQ.1) THEN
               IF(surf_clap_type(NEWREF).EQ.2.0D0.AND.surf_multi_clap_flag(NEWREF).EQ.0.0D0) THEN
!     SQUARE OPERAND PATTERN
                  IF(sys_mode().LE.2) THEN
!     FOCAL
                     WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                     CALL PROCES
                     WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                     CALL PROCES
                  ELSE
!     AFOCAL
                     WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                     CALL PROCES
                     WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                     CALL PROCES
                  END IF
               ELSE
!     CIRCULAR OPERAND PATTERN
                  IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
!     ADD OPERAND
                     IF(sys_mode().LE.2) THEN
!     FOCAL
                        WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                        CALL PROCES
                     ELSE
!     AFOCAL
                        WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                        CALL PROCES
                     END IF
                  ELSE
!     DON'T ADD OPERAND
                  END IF
               END IF
            END IF
!     TYPE TWO MERIT FUNCTION
            IF(DFTYPENUMB.EQ.2) THEN
               IF(surf_clap_type(NEWREF).EQ.2.0D0.AND.surf_multi_clap_flag(NEWREF).EQ.0.0D0) THEN
!     SQUARE OPERAND PATTERN
                  IF(sys_mode().LE.2) THEN
!     FOCAL
                     WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                     CALL PROCES
                     WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                     CALL PROCES
                     WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                     CALL PROCES
                  ELSE
!     AFOCAL
                     WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                     CALL PROCES
                     WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                     CALL PROCES
                     WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                     CALL PROCES
                  END IF
               ELSE
!     CIRCULAR OPERAND PATTERN
                  IF(DSQRT(((JK_WW1)**2)+((JK_WW2)**2)).LE.1.0D0) THEN
!     ADD OPERAND
                     IF(sys_mode().LE.2) THEN
!     FOCAL
                        WRITE(INPUT,*) 'DX,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DY,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                        CALL PROCES
                     ELSE
!     AFOCAL
                        WRITE(INPUT,*) 'DXA,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'DYA,0,',WT1,',,',I,',',J
                        CALL PROCES
                        WRITE(INPUT,*) 'OPDW,0,',WT2,I,',',J
                        CALL PROCES
                     END IF
                  ELSE
!     DON'T ADD OPERAND
                  END IF
               END IF
            END IF
         END DO
      END DO
!
!     EOS
      SAVE_KDP(14)=SAVEINPT(14)
      INPUT='EOS'
      CALL PROCES
      WRITE(OUTLYNE,*) 'MERIT FUNCTION DEFINITION COMPLETED'
      CALL SHOWIT(1)
      REST_KDP(14)=RESTINPT(14)
      RETURN
   END IF
   RETURN
END
! SUB TOPER.FOR
SUBROUTINE TOPER
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!
   INTEGER I,J
!
!       THIS IS SUBROUTINE TOPER. THIS IS THE SUBROUTINE WHICH
!       INITIALLY STARTS THE SETUP OF A NEW TOPER SUBFILE
!       THE CMD LEVEL IS DISABLED AND FLAG F53 IS SET TO 1.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
      WRITE(OUTLYNE,*)'"',WC(1:5),'" TAKES NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   F1=0
   F53=1
!
   DO I=1,MAXTOP
      ISTOP(I)=.FALSE.
      OPERND(MAXFOCRIT+I,1)=0.0D0
      OPERDESC(MAXFOCRIT+I)(1:80)=' '
      OPERND(MAXFOCRIT+I,1:20)=0.0D0
   END DO
   TOPCNT=0
   OPCNT=0
   FMTEXT=.FALSE.
!
   CORMOD=1
!       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
   CURFIG=1
!
!       THE MAXIMUM NUMBER OF OPERANDS ALLOWABLE
!       IS SET IN PROGRAM.FOR WITH THE VALUE MAXTOP WHICH IS
!       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
   RETURN
END
! SUB FOCRIT.FOR
SUBROUTINE FOCRIT
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!
   INTEGER I,J
!
!       THIS IS SUBROUTINE FOCRIT. THIS IS THE SUBROUTINE WHICH
!       INITIALLY STARTS THE SETUP OF A NEW FOCRIT SUBFILE
!       THE CMD LEVEL IS DISABLED AND FLAG F54 IS SET TO 1.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
      WRITE(OUTLYNE,*)'"',WC(1:6),'" TAKES NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   F1=0
   F54=1
!
   DO I=1,MAXFOCRIT
      ISCRIT(I)=.FALSE.
      OPERND(I,1)=0.0D0
      OPERDESC(I)(1:80)=' '
      OPERND(I,1:20)=0.0D0
   END DO
   FCCNT=0
   OPCNT=0
   FMTEXT=.FALSE.
!
!     TARGETS ARE AUTOMATICALLY SET TO THE ORIGINAL VALUES FOR FOCRITS
!
   CORMOD=1
!       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
   CURFIG=1
!
!       THE MAXIMUM NUMBER OF OPERANDS ALLOWABLE
!       IS SET IN PROGRAM.FOR WITH THE VALUE MAXFOCRIT WHICH IS
!       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
   RETURN
END
! SUB MERIT.FOR
SUBROUTINE MERIT
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!
   INTEGER I,J
!
!       THIS IS SUBROUTINE MERIT. THIS IS THE SUBROUTINE WHICH
!       INITIALLY STARTS THE SETUP OF A NEW MERIT SUBFILE
!       THE CMD LEVEL IS DISABLED AND FLAG F27 IS SET TO 1.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
      WRITE(OUTLYNE,*)'"',WC(1:5),'" TAKES NO EXPLICIT INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   F1=0
   F27=1
   OPERND(1:MAXOPT,1)=0.0D0
   OPERDESC(1:MAXOPT)(1:80)=' '
   OPERND(1:MAXOPT,1:20)=0.0D0
!
!       SET THE COUNTER TO THE TOP OF THE MERIT ARRAY STRUCTURE.
   OPCNT=0
   FCCNT=0
   TOPCNT=0
   FMTEXT=.FALSE.
!
   CORMOD=1
!       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
   CURFIG=1
!
!       THE MAXIMUM NUMBER OF OPERANDS ALLOWABLE
!       IS SET IN PROGRAM.FOR WITH THE VALUE MAXOP WHICH IS
!       PASSED IN A COMMON IN THE INCLUDE FILE DATSUB.FOR
   RETURN
END
! SUB MDUMP.FOR
SUBROUTINE MDUMP(IID,JJD,MDERIV)
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,K,J,IID,JJD
!
   real(real64) MDERIV
!
   DIMENSION MDERIV(1:IID,1:JJD)
!
   real(real64) VAL
!
   CHARACTER AI1*3,AI2*3,AI3*3,AI4*3,AVAL1*13,AVAL2*13,AVAL3*13,AVAL4*13
!
!       THIS IS SUBROUTINE MDUMP. THIS IS THE SUBROUTINE WHICH
!       HANDLES AN "ITER MDUMP" COMMAND.
!
!     THE ARRAY MDERIV(I,J) CONTAINS THE PARTIAL DERIVATIVES OF
!     ALL DEFINED OPERANDS WITH RESPECT TO CHANGES IN ALL DEFINED
!     OPERANDS.
!
   IF(.NOT.DEREXT) THEN
      WRITE(OUTLYNE,*)'"ITER (MDUMP, MDP, MDUMPA AND MDPA)"'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'REQUIRE A DERIVATIVE MATRIX TO EXIST'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'BEFORE THEY CAN FUNCTION'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'NO ACTION TAKEN'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(WQ.NE.'MDUMPA'.AND.WQ.NE.'MDPA') THEN
!     ITER MDUMP OR ITER MDP
      DO I=1,VBCNT,4
         CALL I3TOA3(I,AI1)
         CALL I3TOA3(I+1,AI2)
         CALL I3TOA3(I+2,AI3)
         CALL I3TOA3(I+3,AI4)

         IF(I.LE.VBCNT) OUTLYNE='         '//AI1
         IF((I+1).LE.VBCNT)OUTLYNE='         '//AI1//'           '//AI2
         IF((I+2).LE.VBCNT)OUTLYNE='         '//AI1//'           '//AI2//'           '//AI3
         IF((I+3).LE.VBCNT)OUTLYNE='         '//AI1//'           '//AI2//'           '//AI3 //'         '//AI4
         CALL SHOWIT(0)

         DO J=1,OPCNT
            IF(I.LE.VBCNT) THEN
               CALL I3TOA3(J,AI1)
               VAL=MDERIV(J,I)*DINMUL*VARABL(I,8)
               CALL DTOA(VAL,AVAL1)
               OUTLYNE= AI1//' '//AVAL1
            END IF
            IF(I+1.LE.VBCNT) THEN
               VAL=MDERIV(J,I+1)*DINMUL*VARABL(I+1,8)
               CALL DTOA(VAL,AVAL2)
               OUTLYNE=AI1//' '//AVAL1//' '//AVAL2
            END IF
            IF(I+2.LE.VBCNT) THEN
               VAL=MDERIV(J,I+2)*DINMUL*VARABL(I+2,8)
               CALL DTOA(VAL,AVAL3)
               OUTLYNE= AI1//' '//AVAL1//' '//AVAL2//' '//AVAL3
            END IF
            IF(I+3.LE.VBCNT) THEN
               VAL=MDERIV(J,I+3)*DINMUL*VARABL(I+3,8)
               CALL DTOA(VAL,AVAL4)
               OUTLYNE= AI1//' '//AVAL1//' '//AVAL2//' '//AVAL3//' '//AVAL4
            END IF
            CALL SHOWIT(0)
         END DO
      END DO
!     FINISHED DUMPING THE DIFFERENCE MATRIX
   ELSE
!     WQ=MDUMPA OR MDPA
      DO J=1,VBCNT
         DO I=1,OPCNT
            WRITE(OUTLYNE,10)
            CALL SHOWIT(0)
10          FORMAT('**************************************************')
            WRITE(OUTLYNE,20) J,I
            CALL SHOWIT(0)
20          FORMAT('FOR VARIABLE # = ',I3,' AND OPERAND # = ',I3)
            WRITE(OUTLYNE,30) MDERIV(I,J)*DINMUL*VARABL(J,8)
            CALL SHOWIT(0)
30          FORMAT('THE CURRENT DIFFERENCE MATRIX VALUE = ',D23.15)
            WRITE(OUTLYNE,10)
            CALL SHOWIT(0)
         END DO
      END DO
   END IF
   RETURN
END
! SUB FMT3.FOR
SUBROUTINE FMT3
!       SILENT VERSION OF FMT2
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,TAGER,CFGCHK
!
   real(real64) FMTFMT1,DELFMT
!
   LOGICAL NOP,ALLER,CFGER
!
!

   !call LogTermDebug("In Silent version FMT3")
   CFGER=.FALSE.
   TAGER=0
   ALLER=.TRUE.
!
   NOP=.FALSE.
   IF(SQ.EQ.1.AND.WQ.EQ.'NP') THEN
      NOP=.TRUE.
      SQ=0
      WQ='        '
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.0) THEN
!     DO FMT FOR A SINGLE OPERAND
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.FALSE.
   END IF
   IF(S1.EQ.1.AND.SQ.EQ.1) THEN
!     DO FMT FOR A SINGLE CFG
      ALLER=.FALSE.
      TAGER=INT(W1)
      CFGER=.TRUE.
   END IF
!
!
   IF(.NOT.CFGER.AND..NOT.ALLER) THEN
!     CHECK FOR VALID TAGER
      IF(TAGER.GT.OPCNT) THEN
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(CFGER) THEN
!     CHECK FOR TAGER GREATER THAN MAXCFG
      IF(TAGER.GT.MAXCFG) THEN
!     CFG NOT EXISTANT
         CALL MACFAL
         RETURN
      END IF
!     CHECK FOR OPERANDS ACTIVE IN CFG TAGER
      CFGCHK=0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER) CFGCHK=1
      END DO

      IF(CFGCHK.EQ.0) THEN
!     CFG OP DATA NOT EXISTANT
         CALL MACFAL
         RETURN
      END IF
!
   END IF
!     NOW PROCEED WITH THE OUTPUT
!
   IF(ALLER) OLDFMT=FMTFMT
   OPCALC_TYPE=3
   CALL OPCALC
   IF(F28.EQ.0) RETURN
   CALL OPLOAD
   IF(F28.EQ.0) RETURN
   FMTFLG=.TRUE.
!       PROCEED WITH ACTION FOR COMMAND
   IF(ALLER) THEN
      FMTFMT=0.0D0
      DO I=1,OPCNT
         FMTFMT=FMTFMT+(OPERND(I,14)**2)
      END DO
   END IF
   IF(.NOT.ALLER.AND..NOT.CFGER) THEN
      FMTFMT1=0.0D0
      FMTFMT1=(OPERND(TAGER,14)**2)
   END IF
   IF(.NOT.ALLER.AND.CFGER) THEN
      FMTFMT1=0.0D0
      DO I=1,OPCNT
         IF(INT(OPERND(I,16)).EQ.TAGER)FMTFMT1=FMTFMT1+(OPERND(I,14)**2)
      END DO
   END IF
   IF(ALLER) DELFMT=FMTFMT-OLDFMT
   RETURN
!       ALL DONE
END
! SUB ITERADJUST.FOR
SUBROUTINE ITERADJUST(IID,JJD,MDERIV,JA)
!
   use DATCFG
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   INTEGER I,K,J,IID,JJD,SUM1,IIID,JJJD,MAXCNT,ALLOERR
!
   INTEGER JA
!
   real(real64) MDERIV,CHGS1,CHGS2,TESTMAX1,TESTMAX2,VALUE1,VALUE2,VALUE3
!
   DIMENSION MDERIV(1:IID,1:JJD),CHGS1(1:IID)
!
   real(real64) VAL

   real(real64) DERIV
   DIMENSION DERIV(:,:)
   ALLOCATABLE :: DERIV
!
!
!       THIS IS SUBROUTINE MDUMP. THIS IS THE SUBROUTINE WHICH
!       HANDLES AN "ITER ADJUST" COMMAND.
!
!     THE ARRAY MDERIV(I,J) CONTAINS THE PARTIAL DERIVATIVES OF
!     ALL DEFINED OPERANDS WITH RESPECT TO CHANGES IN ALL DEFINED
!     OPERANDS.
!
!
!     CREATE THE CHANGE MATRIX FROM THE DERIVATIVE MATRIX
!
   DO J=1,VBCNT
      DO I=1,OPCNT
         MDERIV(I,J)=MDERIV(I,J)*DINMUL*VARABL(J,8)
      END DO
   END DO
!     CHANGE MATRIX COMPLETE
!
!     FOR EACH VARIABLE, FORM A SUM OF THE SQUARES OF THE CHANGE VALUES FOR ALL THE
!     OPERANDS
   CHGS1(1:IID)=0.0D0
   DO J=1,VBCNT
      DO I=1,OPCNT
         CHGS1(J)=CHGS1(J)+DABS(MDERIV(I,J))
      END DO
   END DO
!     NOW IF THE CHGS1(J) ARE LESS THAN ONTOL, ADD THEM TO THE SUM ACROSS ALL VBS
   CHGS2=0.0D0
   SUM1=0
   DO J=1,VBCNT
      IF(CHGS1(J).GE.ONTOL) THEN
         CHGS2=CHGS2+CHGS1(J)
         SUM1=SUM1+1
      END IF
   END DO
!     NOW DIVIDE BY THE NUMBER OF ENTRIES SUMMED
   CHGS2=CHGS2/DBLE(SUM1)
   ADJUST_VAL1=CHGS2
!     DO THE ADJUST
   DO J=1,VBCNT
      IF(CHGS1(J).NE.0.0D0) THEN
         VARABL(J,8)=VARABL(J,8)*CHGS2/CHGS1(J)
      ELSE
!       NO VARIABLE SCALING
      END IF
   END DO
!     RECOMPUTE DERIVATIVES USING NEW BARABL(J,8) VALUES
   DEALLOCATE(DERIV,STAT=ALLOERR)
   IIID=OPCNT
   JJJD=VBCNT
   IF(IIID.GE.JJJD)MAXCNT=IIID+1
   IF(IIID.LT.JJJD)MAXCNT=JJJD+1
   ALLOCATE(DERIV(1:MAXCNT,1:MAXCNT),STAT=ALLOERR)
   DERIV(1:MAXCNT,1:MAXCNT)=0.0D0
   CALL DERIVATIVES(MAXCNT,DERIV)
!
!
!     IF JA = 1, AUTOSCALE DINMUL
   IF(JA.EQ.1) THEN
      TESTMAX2=0.0D0
      DO I=1,OPCNT
!     VALUE1 = ABSOLUTE VALUE OF CURRENT VALUE OF OPERND I
         VALUE1=DABS(OPERND(I,4))
         TESTMAX1=0.0D0
         DO J=1,VBCNT
!     VALUE2 = ABSOLUTE VALUE OF CURRENT CHANGE VALUE OF OPERAND I
!              WITH RESECT TO VARIABLE J
            VALUE2=DABS(DERIV(I,J))
            IF(VALUE1.NE.0.0D0) THEN
               VALUE3=VALUE2/VALUE1
            ELSE
               VALUE3=0.0D0
            END IF
            IF(VALUE3.GT.LINTOL.AND.VALUE3.GT.TESTMAX1) TESTMAX1=VALUE3
         END DO
         IF(TESTMAX1.GT.TESTMAX2) TESTMAX2=TESTMAX1
      END DO
      TESTMAX2=TESTMAX2/LINTOL
      DINMUL=DINMUL/TESTMAX2
      IF(DINMUL.LT.1.0D-8) DINMUL=1.0D-8
!     RECOMPUTE DERIVATIVES USING NEW BARABL(J,8) VALUES
      DEALLOCATE(DERIV,STAT=ALLOERR)
      IIID=OPCNT
      JJJD=VBCNT
      IF(IIID.GE.JJJD)MAXCNT=IIID+1
      IF(IIID.LT.JJJD)MAXCNT=JJJD+1
      ALLOCATE(DERIV(1:MAXCNT,1:MAXCNT),STAT=ALLOERR)
      DERIV(1:MAXCNT,1:MAXCNT)=0.0D0
      CALL DERIVATIVES(MAXCNT,DERIV)
   END IF
   RETURN
END
