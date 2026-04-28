!       SIXTH FILE FOR LENS DATABASE MANAGER FILES

! SUB SLABEL.FOR
SUBROUTINE SLABEL
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SLABEL WHICH IMPLEMENTS THE LBL/LABEL
!       COMMAND
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
   IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
      LBL(SURF)(1:80)=WS(1:80)
      IF(WS.NE.CNULL) call set_surf_label_flag(SURF, 1)
      IF(WS.EQ.CNULL) call set_surf_label_flag(SURF, 0)
   ELSE
!       NOT LABEL OR LBL
   END IF
   RETURN
END
! SUB SFNO.FOR
SUBROUTINE SFNO
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use command_utils, only: is_command_query
   use mod_system, only: sys_fno_val_set, sys_fno_val_x, sys_fno_val_y, &
      & sys_xz_data_flag, sys_sax, sys_say, &
      & sys_set_fno_val_set, sys_set_fno_val_x, sys_set_fno_val_y, sys_set_na_set, &
      & sys_set_sax_float, sys_set_say_float, sys_set_xz_data_flag
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SFNO WHICH IMPLEMENTS THE FNO(X OR Y) COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE FNO(X OR Y) COMMAND AT
!       THE CMD LEVEL
!
!
   IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'AT THE CMD LEVEL, "FNOY" AND "FNOX"'//'\n'//&
         & 'TAKE NO EXPLICIT INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
      IF(is_command_query().OR..not. is_command_query()) THEN
         IF(WC.EQ.'FNOY') THEN
            IF(sys_fno_val_set().NE.1.0D0.AND.sys_fno_val_set().NE.3.0D0) THEN
               OUTLYNE='NOTE:'
               CALL SHOWIT(1)
               OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
               CALL SHOWIT(1)
               call sys_set_fno_val_y(1.0D0/((2.0D0*sys_say())/surf_thickness(0)))
               call sys_set_say_float(0.0D0)
               call sys_set_sax_float(0.0D0)
            ELSE
            END IF
            WRITE(OUTLYNE,2000) sys_fno_val_y()
            CALL SHOWIT(0)
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            IF(sys_fno_val_set().NE.2.0D0.AND.sys_fno_val_set().NE.3.0D0) THEN
               OUTLYNE='NOTE:'
               CALL SHOWIT(1)
               OUTLYNE='"'//WC(1:4)//'" HAS NOT BEEN EXPLICITLY SET'
               CALL SHOWIT(1)
               call sys_set_fno_val_x(1.0D0/((2.0D0*sys_sax())/surf_thickness(0)))
               call sys_set_say_float(0.0D0)
               call sys_set_sax_float(0.0D0)
            END IF
            WRITE(OUTLYNE,3000) sys_fno_val_x()
            CALL SHOWIT(0)
            RETURN
         END IF
      END IF
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(is_command_query()) THEN
         OUTLYNE='QUERRY OBJECT F-NUMBER VALUES FROM THE CMD LEVEL WITH THE'
         CALL SHOWIT(1)
         OUTLYNE='"FNOY" OR "FNOX" COMMANDS'
         CALL SHOWIT(1)
         RETURN
      END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
      IF(SST.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            CALL REPORT_ERROR_AND_FAIL('"FNOY" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            CALL REPORT_ERROR_AND_FAIL('"FNOX" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1 .OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"FNOY" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"FNOX" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(SQ.EQ.1.AND.F5.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"FNOY" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"FNOX" TAKES NO QUALIFIER WORD IN LENS INPUT MODE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
      IF(F6.EQ.1) THEN
         IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
!
            IF(WC.EQ.'FNOY') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'INVALID QUALIFIER WORD USED WITH "FNOY"'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(WC.EQ.'FNOX') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'INVALID QUALIFIER WORD USED WITH "FNOX"'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
         END IF
      END IF
      IF(DF1.EQ.1) THEN
!
         IF(WC.EQ.'FNOY') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"FNOY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'FNOX') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"FNOX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
      END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
      IF(S1.EQ.0) THEN
         IF(WC.EQ.'FNOY') THEN
            WRITE(OUTLYNE,2000) sys_fno_val_y()
            CALL SHOWIT(0)
         ELSE
         END IF
         IF(WC.EQ.'FNOX') THEN
            WRITE(OUTLYNE,3000) sys_fno_val_x()
            CALL SHOWIT(0)
            IF(sys_xz_data_flag().EQ.0.0D0) call sys_set_xz_data_flag(1.0D0)
            IF(sys_xz_data_flag().EQ.2.0D0) call sys_set_xz_data_flag(3.0D0)
         ELSE
         END IF
         RETURN
      ELSE
         IF(W1.EQ.0.0D0) THEN
            IF(WC.EQ.'FNOY') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"FNOY" MAY NOT BE SET TO ZERO'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(WC.EQ.'FNOX') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"FNOX" MAY NOT BE SET TO ZERO'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            RETURN
         END IF
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'FNOY') THEN
               IF(sys_fno_val_set().NE.3.0D0.AND.sys_fno_val_set().NE.1.0D0) THEN
                  IF(sys_fno_val_set().EQ.0.0D0) call sys_set_fno_val_set(1.0D0)
                  IF(sys_fno_val_set().EQ.2.0D0) call sys_set_fno_val_set(3.0D0)
                  call sys_set_say_float(0.0D0)
                  call sys_set_sax_float(0.0D0)
               END IF
            END IF
            IF(WC.EQ.'FNOX') THEN
               IF(sys_fno_val_set().NE.3.0D0.AND.sys_fno_val_set().NE.2.0D0) THEN
                  IF(sys_fno_val_set().EQ.0.0D0) call sys_set_fno_val_set(2.0D0)
                  IF(sys_fno_val_set().EQ.1.0D0) call sys_set_fno_val_set(3.0D0)
                  call sys_set_say_float(0.0D0)
                  call sys_set_sax_float(0.0D0)
               END IF
            END IF
            call sys_set_na_set(0.0D0)
            IF(WC.EQ.'FNOY') call sys_set_fno_val_y(DABS(W1))
            IF(WC.EQ.'FNOX') call sys_set_fno_val_x(DABS(W1))
         END IF
         IF(WQ.EQ.'DELT') THEN
            call sys_set_na_set(0.0D0)
            IF(WC.EQ.'FNOY') call sys_set_fno_val_y(sys_fno_val_y()+(W1))
            IF(WC.EQ.'FNOX') call sys_set_fno_val_x(sys_fno_val_x()+(W1))
         END IF
         IF(WQ.EQ.'CENT') THEN
            call sys_set_na_set(0.0D0)
            IF(WC.EQ.'FNOY') call sys_set_fno_val_y(sys_fno_val_y()+(W1*0.0D0*sys_fno_val_y()))
            IF(WC.EQ.'FNOX') call sys_set_fno_val_x(sys_fno_val_x()+(W1*0.0D0*sys_fno_val_x()))
         END IF
         RETURN
      END IF
   END IF
2000 FORMAT('FNOY=',1X,D23.15)
3000 FORMAT('FNOX=',1X,D23.15)
   RETURN
END
! SUB SFNB.FOR
SUBROUTINE SFNB
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use command_utils, only: is_command_query
   use mod_system, only: sys_fno_val_set, sys_mode, sys_naox, sys_naoy, sys_telecentric, &
      & sys_fno_flag_x, sys_fno_flag_y, sys_fno_hold_x, sys_fno_hold_y, &
      & sys_fno_val_x, sys_fno_val_y, sys_last_surf, sys_na_set, &
      & sys_sax_float, sys_say_float, &
      & sys_set_fno_flag_x, sys_set_fno_flag_y, sys_set_fno_hold_x, sys_set_fno_hold_y, &
      & sys_set_sax, sys_set_say
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SFNB WHICH IMPLEMENTS THE FNBY AND FNBX
!       COMMANDS AT THE CMD LEVEL. (ALSO FNBY HLD AND FNBX HLD)
!
   REAL*8 FN,ABSSYS
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(is_command_query()) THEN
      OUTLYNE='QUERRY F-NUMBER VALUES FROM THE CMD LEVEL WITH THE'
      CALL SHOWIT(1)
      OUTLYNE='"SHO" OR "GET" AND "WRITE" COMMANDS'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
      IF(WC.EQ.'FNBY') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'FNBX') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
!
      IF(WC.EQ.'FNBY') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'FNBX') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'FNBX'.AND.sys_na_set().NE.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "NAOX"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'FNBY'.AND.sys_na_set().NE.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "NAOY"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'FNBX'.AND.sys_fno_val_set().NE.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "FNOX"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'FNBY'.AND.sys_fno_val_set().NE.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"FNBY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "FNOY"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
   END IF
   IF(sys_say_float().NE.0.0D0.OR.sys_sax_float().NE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SAY OR SAX IS CURRENTLY FLOATING'//'\n'//&
      & '"FNBY/FNBX" ADJUSTMENT NOT ALLOWED', 1)
      RETURN
   END IF
   IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.DF5.EQ.1) THEN
!       FNBY OR FNBX TYPED IN WITH NO INPUT,PRINTS OUT CURRENT VALUES
      IF(WC.EQ.'FNBY'.AND.sys_fno_flag_y().NE.1.0D0.AND.sys_fno_flag_y().NE.-1.0D0) THEN
         IF(DABS(PXTRAY(2,(INT(sys_last_surf())))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,100)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAY(2,(INT(sys_last_surf())))))
         END IF
         WRITE(OUTLYNE,200)FN
         CALL SHOWIT(0)
         RETURN
      END IF
      ABSSYS=DABS(sys_fno_flag_y())
      IF(WC.EQ.'FNBY'.AND.ABSSYS.EQ.1.0D0) THEN
         IF(DABS(PXTRAY(2,(INT(sys_last_surf())))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,100)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAY(2,(INT(sys_last_surf())))))
         END IF
         IF(sys_fno_flag_y().GT.0.0D0) THEN
            WRITE(OUTLYNE,200) FN
            CALL SHOWIT(0)
         END IF
         IF(sys_fno_flag_y().LT.0.0D0) THEN
            WRITE(OUTLYNE,300) sys_fno_hold_y()
            CALL SHOWIT(0)
         END IF
         RETURN
      END IF
      IF(WC.EQ.'FNBX'.AND.sys_fno_flag_x().NE.1.0D0.AND.sys_fno_flag_x().NE.-1.0D0) THEN
         IF(DABS(PXTRAX(2,(INT(sys_last_surf())))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,101)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAX(2,(INT(sys_last_surf())))))
         END IF
         WRITE(OUTLYNE,201) FN
         CALL SHOWIT(0)
         RETURN
      END IF
      ABSSYS=DABS(sys_fno_flag_x())
      IF(WC.EQ.'FNBX'.AND.ABSSYS.EQ.1.0D0) THEN
         IF(DABS(PXTRAX(2,(INT(sys_last_surf())))).LE.1.0D-15) THEN
            WRITE(OUTLYNE,101)
            CALL SHOWIT(0)
            RETURN
         ELSE
            FN=-(1.0D0/(2.0D0*PXTRAX(2,(INT(sys_last_surf())))))
         END IF
         IF(sys_fno_flag_x().GT.0.0D0) THEN
            WRITE(OUTLYNE,201) FN
            CALL SHOWIT(0)
         END IF
         IF(sys_fno_flag_x().LT.0.0D0) THEN
            WRITE(OUTLYNE,301) sys_fno_hold_x()
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
      END IF
!
   ELSE
!       NUMERIC INPUT AND QUALIFIER INPUT NOT DEFAULT
      IF(WQ.NE.'DELK'.AND.WQ.NE.'HLD'.AND.SQ.NE.0) THEN
         CALL REPORT_ERROR_AND_FAIL('INVALID QUALIFIER INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WQ.NE.'DELK') THEN
         IF(WC.EQ.'FNBY') THEN
            IF(W1.EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('F/0.0 IS MEANINGLESS, ADJUSTMENT NOT PERFORMED', 1)
               RETURN
            END IF
!       CHECK LENS MODE, MUST BE FOCAL OR UFOCAL
            IF(sys_mode().EQ.3.0.OR.sys_mode().EQ.4.0) THEN
               IF(WC.EQ.'FNBY') THEN
                  OUTLYNE='"FNBY" REQUIRES THE FOCAL OR UFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(sys_mode().EQ.3.0) THEN
                     OUTLYNE='CURRENT MODE IS AFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(sys_mode().EQ.4.0) THEN
                     OUTLYNE='CURRENT MODE IS UAFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  CALL REPORT_ERROR_AND_FAIL('"FNBY" ADJUSTMENT NOT PERFORMED', 1)
                  RETURN
               END IF
               IF(WC.EQ.'FNBX') THEN
                  OUTLYNE='"FNBX" REQUIRES THE FOCAL OR UFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(sys_mode().EQ.3.0) THEN
                     OUTLYNE='CURRENT MODE IS AFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(sys_mode().EQ.4.0) THEN
                     OUTLYNE='CURRENT MODE IS UAFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  CALL REPORT_ERROR_AND_FAIL('"FNBX" ADJUSTMENT NOT PERFORMED', 1)
                  RETURN
               END IF
            ELSE
            END IF
!
            IF(WQ.NE.'HLD') call sys_set_fno_flag_y(1.0D0)
            IF(WQ.EQ.'HLD') call sys_set_fno_flag_y(-1.0D0)
            call sys_set_fno_hold_y(W1)
!       CALL THE SUBROUTINE FYADJ TO PERFORM THE F NUMBER ADJUSTMENT
!       DO A PARAXIAL TRACE TO GET STARTING VALUES
            ITYPEP=1
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            ITYPEP=1
            CALL FADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            RETURN
         ELSE
!       WC MUST BE 'FNBX'
            IF(W1.EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL('F/0.0 IS MEANINGLESS, ADJUSTMENT NOT PERFORMED', 1)
               RETURN
            END IF
            IF(WQ.NE.'HLD') call sys_set_fno_flag_x(1.0D0)
            IF(WQ.EQ.'HLD') call sys_set_fno_flag_x(-1.0D0)
            call sys_set_fno_hold_x(W1)
!       CALL SUBROUTINE FXADJ TO ADJUST THE F NUMBER IN THE X PLANE
!       DO A PARAXIAL TRACE TO GET STARTING VALUES.
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            ITYPEP=2
            CALL FADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
         END IF
         RETURN
      ELSE
!       WQ IS DEL
         IF(WC.EQ.'FNBY') THEN
            ABSSYS=DABS(sys_fno_flag_y())
            IF(ABSSYS.EQ.1.0D0) THEN
               OUTLYNE='"FNBY" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               call sys_set_fno_flag_y(0.0D0)
               call sys_set_fno_hold_y(0.0D0)
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO FNBY ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
!       WC WAS FNBX
            ABSSYS=DABS(sys_fno_flag_x())
            IF(ABSSYS.EQ.1.0D0) THEN
               OUTLYNE='"FNBX" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               call sys_set_fno_flag_x(0.0D0)
               call sys_set_fno_hold_x(0.0D0)
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO FNBX ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!
100 FORMAT('F-NUMBER (YZ-PLANE) IS INFINITE')
101 FORMAT('F-NUMBER (YX-PLANE) IS INFINITE')
200 FORMAT('F-NUMBER (YZ-PLANE) =',G12.5)
201 FORMAT('F-NUMBER (XZ-PLANE) =',G12.5)
300 FORMAT('F-NUMBER HOLD (YZ-PLANE) =',G12.5)
301 FORMAT('F-NUMBER HOLD (XZ-PLANE) =',G12.5)
!
   RETURN
END
! SUB PIVAXIS.FOR
SUBROUTINE PIVAXIS
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use command_utils, only: is_command_query
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE PIVAXIS WHICH IMPLEMENTS THE
!       "PIVAXIS"
!       COMMAND AT THE CMD LEVEL.
!
   INTEGER AMODE
!
   IF(is_command_query().OR.SQ.EQ.0) THEN
      IF(surf_pivot_axis(SURF) == 0)OUTLYNE='"PIVAXIS" IS SET TO "VERTEX"'
      IF(surf_pivot_axis(SURF) == 1)OUTLYNE='"PIVAXIS" IS SET TO "NORMAL"'
      CALL SHOWIT(0)
      RETURN
   END IF
!
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIVAXIS" TAKES NO STRING OR NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1) THEN
      IF(WQ.NE.'VERTEX'.AND.WQ.NE.'NORMAL') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID QUALIFIER INPUT USED WITH "PIVAXIS"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
!
   IF(WQ.EQ.'VERTEX')THEN
      call set_surf_pivot_axis(SURF, 0)
   END IF
   IF(WQ.EQ.'NORMAL')THEN
      call set_surf_pivot_axis(SURF, 1)
   END IF
   RETURN
END
! SUB SER.FOR
SUBROUTINE SER
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use command_utils, only: is_command_query
   use mod_system, only: sys_fno_val_set, sys_mode, sys_naox, sys_naoy, sys_telecentric, &
      & sys_units, sys_fno_flag_x, sys_fno_flag_y, sys_fno_hold_x, sys_fno_hold_y, &
      & sys_fno_val_x, sys_fno_val_y, sys_last_surf, sys_na_set, &
      & sys_sax_float, sys_say_float, &
      & sys_set_fno_flag_x, sys_set_fno_flag_y, sys_set_fno_hold_x, sys_set_fno_hold_y, &
      & sys_set_sax, sys_set_say
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SER WHICH IMPLEMENTS THE ERY AND ERX
!       COMMANDS AT THE CMD LEVEL.
!
   CHARACTER UNIT*5
!
   REAL*8 ABSSYS,EP
!
   INTEGER ITYPEP
!
   COMMON/PTYPER/ITYPEP
!
!
   IF(sys_units().EQ.1.0) UNIT='IN   '
   IF(sys_units().EQ.2.0) UNIT='CM   '
   IF(sys_units().EQ.3.0) UNIT='MM'
   IF(sys_units().EQ.4.0) UNIT='M    '
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(is_command_query()) THEN
      OUTLYNE='QUERRY EXIT PUPIL VALUES FROM THE CMD LEVEL WITH THE'
      CALL SHOWIT(1)
      OUTLYNE='"SHO" OR "GET" AND "WRITE" COMMANDS'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.S2.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.S3.EQ.1) THEN
!
      IF(WC.EQ.'ERY') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ERY" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'ERX') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ERX" TAKES NO STRING OR NUMERIC WORD #2 THROUGH #5 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
!
   IF(DF1.EQ.1) THEN
!
      IF(WC.EQ.'ERY') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ERY" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'ERX') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"ERX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(WC.EQ.'ERX'.AND.sys_na_set().NE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ERX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "NAOX"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WC.EQ.'ERY'.AND.sys_na_set().NE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ERY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "NAOY"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WC.EQ.'ERX'.AND.sys_fno_val_set().NE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ERX" CAN NOT BE USED WHEN "SAX" IS SPECIFIED BY "FNOX"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WC.EQ.'ERY'.AND.sys_fno_val_set().NE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ERY" CAN NOT BE USED WHEN "SAY" IS SPECIFIED BY "FNOY"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(sys_say_float().NE.0.0D0.OR.sys_sax_float().NE.0.0D0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'SAY OR SAX IS CURRENTLY FLOATING'//'\n'//&
      & '"ERY/ERX" ADJUSTMENT NOT ALLOWED', 1)
      RETURN
   END IF
!
   IF(SQ.EQ.0.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1.AND.DF4.EQ.1.AND.DF5.EQ.1) THEN
!       ERY OR ERX TYPED IN WITH NO INPUT,PRINTS OUT CURRENT VALUES
      IF(WC.EQ.'ERY'.AND.sys_fno_flag_y().NE.2.0.AND.sys_fno_flag_y().NE.-2.0) THEN
!       CALCULATE EXIT PUPIL RADIUS
         EP=PXTRAY(1,(INT(sys_last_surf())))-(PXTRAY(2,(INT(sys_last_surf())))*(PXTRAY(5,(INT(sys_last_surf())))/PXTRAY(6,(INT(sys_last_surf())))))
         WRITE(OUTLYNE,200) EP,UNIT
         CALL SHOWIT(0)
         RETURN
      ELSE
      END IF
      ABSSYS=DABS(sys_fno_flag_y())
      IF(WC.EQ.'ERY'.AND.ABSSYS.EQ.2.0) THEN
         EP=PXTRAY(1,(INT(sys_last_surf())))-(PXTRAY(2,(INT(sys_last_surf())))*(PXTRAY(5,(INT(sys_last_surf())))/PXTRAY(6,(INT(sys_last_surf())))))
         IF(sys_fno_flag_y().GT.0.0D0) THEN
            WRITE(OUTLYNE,200) EP,UNIT
            CALL SHOWIT(0)
         END IF
         IF(sys_fno_flag_y().LT.0.0D0) THEN
            WRITE(OUTLYNE,300) sys_fno_hold_y(),UNIT
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
      END IF
      IF(WC.EQ.'ERX'.AND.sys_fno_flag_x().NE.2.0.AND.sys_fno_flag_x().NE.-2.0) THEN
         EP=PXTRAX(1,(INT(sys_last_surf())))-(PXTRAX(2,(INT(sys_last_surf())))*(PXTRAX(5,(INT(sys_last_surf())))/PXTRAX(6,(INT(sys_last_surf())))))
         WRITE(OUTLYNE,201) EP,UNIT
         CALL SHOWIT(0)
         RETURN
      END IF
      ABSSYS=DABS(sys_fno_flag_x())
      IF(WC.EQ.'ERX'.AND.ABSSYS.EQ.2.0) THEN
         EP=PXTRAX(1,(INT(sys_last_surf())))-(PXTRAX(2,(INT(sys_last_surf())))*(PXTRAX(5,(INT(sys_last_surf())))/PXTRAX(6,(INT(sys_last_surf())))))
         IF(sys_fno_flag_x().GT.0.0D0) THEN
            WRITE(OUTLYNE,201) EP,UNIT
            CALL SHOWIT(0)
         END IF
         IF(sys_fno_flag_x().LT.0.0D0) THEN
            WRITE(OUTLYNE,301) sys_fno_hold_x(),UNIT
            CALL SHOWIT(0)
         END IF
         RETURN
      ELSE
      END IF
!
   ELSE
!       NUMERIC INPUT AND QUALIFIER INPUT NOT DEFAULT
      IF(WQ.NE.'DELK'.AND.WQ.NE.'HLD'.AND.SQ.NE.0) THEN
         CALL REPORT_ERROR_AND_FAIL('INVALID QUALIFIER INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WQ.NE.'DELK') THEN
         IF(WC.EQ.'ERY') THEN
            IF(W1.EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'ZERO DIAMETER EXIT PUPIL RADIUS IS MEANINGLESS'//'\n'//&
               & 'EXIT PUPIL ADJUSTMENT NOT PERFORMED', 1)
               RETURN
            END IF
!       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
            IF(sys_mode().EQ.1.0.OR.sys_mode().EQ.2.0) THEN
               IF(WC.EQ.'ERY') THEN
                  OUTLYNE='"ERY" REQUIRES THE AFOCAL OR UAFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(sys_mode().EQ.1.0) THEN
                     CALL SHOWIT(1)
                     OUTLYNE='CURRENT MODE IS FOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(sys_mode().EQ.2.0) THEN
                     OUTLYNE='CURRENT MODE IS UFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  CALL REPORT_ERROR_AND_FAIL('"ERY" ADJUSTMENT NOT PERFORMED', 1)
                  RETURN
               ELSE
               END IF
               IF(WC.EQ.'ERX') THEN
                  OUTLYNE='"ERX" REQUIRES THE AFOCAL OR UAFOCAL MODES'
                  CALL SHOWIT(1)
                  IF(sys_mode().EQ.1.0) THEN
                     OUTLYNE='CURRENT MODE IS FOCAL'
                     CALL SHOWIT(1)
                  END IF
                  IF(sys_mode().EQ.2.0) THEN
                     OUTLYNE='CURRENT MODE IS UFOCAL'
                     CALL SHOWIT(1)
                  END IF
                  CALL REPORT_ERROR_AND_FAIL('"ERX" ADJUSTMENT NOT PERFORMED', 1)
                  RETURN
               ELSE
               END IF
            ELSE
            END IF
            IF(WQ.NE.'HLD') call sys_set_fno_flag_y(2.0D0)
            IF(WQ.EQ.'HLD') call sys_set_fno_flag_y(-2.0D0)
            call sys_set_fno_hold_y(W1)
!       PERFORM ERY ADJUSTMENT BUT
!       FIRST DO A PARAXIAL TRACE FOR STARTING VALUES
            ITYPEP=1
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            CALL ERADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            RETURN
         ELSE
!       WC MUST BE 'ERX'
            IF(W1.EQ.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'ZERO DIAMETER EXIT PUPIL RADIUS IS MEANINGLESS'//'\n'//&
               & 'ADJUSTMENT NOT PERFORMED', 1)
               RETURN
            END IF
!       CHECK LENS MODE, MUST BE AFOCAL OR UAFOCAL
            IF(WQ.NE.'HLD') call sys_set_fno_flag_x(2.0D0)
            IF(WQ.EQ.'HLD') call sys_set_fno_flag_x(-2.0D0)
            call sys_set_fno_hold_x(W1)
!               PERFORM ERX ADJUSTMENT
!       FIRST DO A PARAXIAL TRACE FOR STARTING VALUES
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
            CALL ERADJ
            ITYPEP=3
!       TELECENTRIC STUFF, 11/12/2000
            IF(sys_telecentric().EQ.1.0D0) THEN
               IF(sys_na_set().EQ.0.0D0.AND.sys_fno_val_set().EQ.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'WHEN "TEL ON" IS SET, NAO OR FNO MUST BE USED'//'\n'//&
                  & 'TO SPECIFY THE MARGINAL PARAXIAL RAY STARTING'//'\n'//&
                  & 'VALUES'//'\n'//&
                  & 'PARAXIAL TRACE STOPPED', 1)
                  RETURN
               ELSE
                  IF(sys_na_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                  END IF
                  IF(sys_na_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_na_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)*sys_naoy())
                     call sys_set_sax(surf_thickness(0)*sys_naox())
                  END IF
                  IF(sys_fno_val_set().EQ.1.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                  END IF
                  IF(sys_fno_val_set().EQ.2.0D0) THEN
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
                  IF(sys_fno_val_set().EQ.3.0D0) THEN
                     call sys_set_say(surf_thickness(0)/(2.0D0*sys_fno_val_y()))
                     call sys_set_sax(surf_thickness(0)/(2.0D0*sys_fno_val_x()))
                  END IF
               END IF
            END IF
!
            CALL PRTRA
         END IF
         RETURN
      ELSE
!       WQ IS DEL
         IF(WC.EQ.'ERY') THEN
            ABSSYS=DABS(sys_fno_flag_y())
            IF(ABSSYS.EQ.2.0) THEN
               OUTLYNE='"ERY" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               call sys_set_fno_flag_y(0.0D0)
               call sys_set_fno_hold_y(0.0D0)
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO ERY ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         ELSE
!       WC WAS ERX
            ABSSYS=DABS(sys_fno_flag_x())
            IF(ABSSYS.EQ.2.0) THEN
               OUTLYNE='"ERX" ADJUSTMENT DELETED'
               CALL SHOWIT(1)
               call sys_set_fno_flag_x(0.0D0)
               call sys_set_fno_hold_x(0.0D0)
               F1=0
               F6=1
               F22=0
               LNSTYP=1
               CALL LNSEOS
               RETURN
            ELSE
               OUTLYNE='NO ERX ADJUSTMENT TO DELETE'
               CALL SHOWIT(1)
            END IF
            CALL MACFAL
            RETURN
         END IF
      END IF
   END IF
!
200 FORMAT('EXIT PUPIL RADIUS (YZ-PLANE) =',G12.5,1X,A5)
201 FORMAT('EXIT PUPIL RADIUS (XZ-PLANE) =',G12.5,1X,A5)
300 FORMAT('EXIT PUPIL RADIUS HOLD (YZ-PLANE) =',G12.5,1X,A5)
301 FORMAT('EXIT PIPIL RADIUS HOLD (XZ-PLANE) =',G12.5,1X,A5)
!
   RETURN
END
! SUB SDEFG.FOR
SUBROUTINE SDEFG
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SDEFG WHICH IMPLEMENTS THE AC,AD,
!       AE,AF,AG,ADTOR,AETOR,AFTOR AND AGTOR
!       COMMANDS AT THE UPDATE LENS LEVEL.
!
   INTEGER CT,PIKCNT,I
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')OUTLYNE='AC,AD,AE,AF AND AG TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')OUTLYNE='AH,AI,AJ,AK AND AL TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR')OUTLYNE='ADTOR, AETOR, AFTOR AND AGTOR '//'TAKE NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SST.EQ.1) THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')OUTLYNE='AC,AD,AE,AF AND AG TAKE NO STRING INPUT'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')OUTLYNE='AH,AI,AJ,AK AND AL TAKE NO STRING INPUT'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR')OUTLYNE='ADTOR, AETOR, AFTOR AND AGTOR '//'TAKE NO STRING INPUT'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')OUTLYNE='AC,AD,AE,AF AND AG TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')OUTLYNE='AH,AI,AJ,AK AND AL TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR')OUTLYNE='ADTOR, AETOR, AFTOR AND AGTOR '//'TAKE NO QUALIFIER WORD IN LENS INPUT MODE'
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')OUTLYNE='INVALID QUALIFIER WORD USED WITH AC,AD,AE,AF OR AG'
         IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')OUTLYNE='INVALID QUALIFIER WORD USED WITH AH,AI,AJ,AK OR AL'
         IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR') THEN
            OUTLYNE='INVALID QUALIFIER WORD USED WITH'
            OUTLYNE='ADTOR, AETOR, AFTOR OR AGTOR'
         END IF
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1)THEN
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC')OUTLYNE='AC,AD,AE,AF OR AG REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')OUTLYNE='AH,AI,AJ,AK OR AL REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR') THEN
         OUTLYNE='ADTOR,AETOR,AFTOR OR AGTOR REQUIRES EXPLICIT'
         OUTLYNE='NUMERIC WORD #1 INPUT'
      END IF
      CALL SHOWIT(1)
      CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!
!               WE ARE AT LENS UPDATE LEVEL
   IF(WC.EQ.'AC') CT=26
   IF(WC.EQ.'AD') CT=5
   IF(WC.EQ.'AE') CT=6
   IF(WC.EQ.'AF') CT=7
   IF(WC.EQ.'AG') CT=8
   IF(WC.EQ.'AH') CT=27
   IF(WC.EQ.'AI') CT=28
   IF(WC.EQ.'AJ') CT=29
   IF(WC.EQ.'AK') CT=30
   IF(WC.EQ.'AL') CT=31
   IF(WC.EQ.'ADTOR') CT=22
   IF(WC.EQ.'AETOR') CT=23
   IF(WC.EQ.'AFTOR') CT=24
   IF(WC.EQ.'AGTOR') CT=25
!
   IF(WC.EQ.'AD'.OR.WC.EQ.'AF'.OR.WC.EQ.'AE'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
      IF(.NOT.surf_is_asphere(SURF)) THEN
!       SURFACE NOT ASPHERIC, SET IT AS SUCH.
         IF(WC.EQ.'AC') THEN
            IF(surf_curvature(SURF).NE.0.0D0) THEN
               OUTLYNE='WARNING:'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
               CALL SHOWIT(1)
               OUTLYNE='THE "AC" TERM WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
               CALL SHOWIT(1)
            END IF
            W5=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=1
            DF5=0
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AD') THEN
            W1=W1
            DF1=0
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AE') THEN
            W2=W1
            DF1=1
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            W1=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AF') THEN
            W3=W1
            DF1=1
            DF2=1
            DF3=0
            DF4=1
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AG') THEN
            W4=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=0
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AH') THEN
            W1=W1
            DF1=0
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AI') THEN
            W2=W1
            DF1=1
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            W1=0.0D0
            W3=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AJ') THEN
            W3=W1
            DF1=1
            DF2=1
            DF3=0
            DF4=1
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W4=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AK') THEN
            W4=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=0
            DF5=1
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W5=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AL') THEN
            W5=W1
            DF1=1
            DF2=1
            DF3=1
            DF4=1
            DF5=0
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            SQ=0
            SN=1
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AC'.OR.WC.EQ.'AD'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AE')CALL SASPH
         IF(WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')CALL SASPH
         RETURN
      ELSE
      END IF
      IF(surf_is_asphere(SURF)) THEN
         IF(WC.EQ.'AC') THEN
            IF(SQ.EQ.0) call set_surf_asphere_coeff(SURF, 2, W1)
            IF(WQ.EQ.'DELT') call set_surf_asphere_coeff(SURF, 2, surf_asphere_coeff(SURF, 2)+W1)
            IF(WQ.EQ.'CENT')call set_surf_asphere_coeff(SURF, 2, surf_asphere_coeff(SURF, 2)+(W1*0.01D0*surf_asphere_coeff(SURF, 2)))
            IF(surf_curvature(SURF).NE.0.0D0) THEN
               OUTLYNE='WARNING:'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
               CALL SHOWIT(1)
               OUTLYNE='THE "AC" TERM WILL BE IGNORED FOR THIS NON-PLANO SURFACE'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'AD') call set_surf_asphere_coeff(SURF, 4, W1)
            IF(WC.EQ.'AE') call set_surf_asphere_coeff(SURF, 6, W1)
            IF(WC.EQ.'AF') call set_surf_asphere_coeff(SURF, 8, W1)
            IF(WC.EQ.'AG') call set_surf_asphere_coeff(SURF, 10, W1)
            IF(WC.EQ.'AH') call set_surf_asphere_coeff(SURF, 12, W1)
            IF(WC.EQ.'AI') call set_surf_asphere_coeff(SURF, 14, W1)
            IF(WC.EQ.'AJ') call set_surf_asphere_coeff(SURF, 16, W1)
            IF(WC.EQ.'AK') call set_surf_asphere_coeff(SURF, 18, W1)
            IF(WC.EQ.'AL') call set_surf_asphere_coeff(SURF, 20, W1)
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'AD') call set_surf_asphere_coeff(SURF, 4, surf_asphere_coeff(SURF, 4)+W1)
            IF(WC.EQ.'AE') call set_surf_asphere_coeff(SURF, 6, surf_asphere_coeff(SURF, 6)+W1)
            IF(WC.EQ.'AF') call set_surf_asphere_coeff(SURF, 8, surf_asphere_coeff(SURF, 8)+W1)
            IF(WC.EQ.'AG') call set_surf_asphere_coeff(SURF, 10, surf_asphere_coeff(SURF, 10)+W1)
            IF(WC.EQ.'AH') call set_surf_asphere_coeff(SURF, 12, surf_asphere_coeff(SURF, 12)+W1)
            IF(WC.EQ.'AI') call set_surf_asphere_coeff(SURF, 14, surf_asphere_coeff(SURF, 14)+W1)
            IF(WC.EQ.'AJ') call set_surf_asphere_coeff(SURF, 16, surf_asphere_coeff(SURF, 16)+W1)
            IF(WC.EQ.'AK') call set_surf_asphere_coeff(SURF, 18, surf_asphere_coeff(SURF, 18)+W1)
            IF(WC.EQ.'AL') call set_surf_asphere_coeff(SURF, 20, surf_asphere_coeff(SURF, 20)+W1)
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'AD')call set_surf_asphere_coeff(SURF, 4, surf_asphere_coeff(SURF, 4)+(W1*0.01D0*surf_asphere_coeff(SURF, 4)))
            IF(WC.EQ.'AE')call set_surf_asphere_coeff(SURF, 6, surf_asphere_coeff(SURF, 6)+(W1*0.01D0*surf_asphere_coeff(SURF, 6)))
            IF(WC.EQ.'AF')call set_surf_asphere_coeff(SURF, 8, surf_asphere_coeff(SURF, 8)+(W1*0.01D0*surf_asphere_coeff(SURF, 8)))
            IF(WC.EQ.'AG')call set_surf_asphere_coeff(SURF, 10, surf_asphere_coeff(SURF, 10)+(W1*0.01D0*surf_asphere_coeff(SURF, 10)))
            IF(WC.EQ.'AH')call set_surf_asphere_coeff(SURF, 12, surf_asphere_coeff(SURF, 12)+(W1*0.01D0*surf_asphere_coeff(SURF, 12)))
            IF(WC.EQ.'AI')call set_surf_asphere_coeff(SURF, 14, surf_asphere_coeff(SURF, 14)+(W1*0.01D0*surf_asphere_coeff(SURF, 14)))
            IF(WC.EQ.'AJ')call set_surf_asphere_coeff(SURF, 16, surf_asphere_coeff(SURF, 16)+(W1*0.01D0*surf_asphere_coeff(SURF, 16)))
            IF(WC.EQ.'AK')call set_surf_asphere_coeff(SURF, 18, surf_asphere_coeff(SURF, 18)+(W1*0.01D0*surf_asphere_coeff(SURF, 18)))
            IF(WC.EQ.'AL')call set_surf_asphere_coeff(SURF, 20, surf_asphere_coeff(SURF, 20)+(W1*0.01D0*surf_asphere_coeff(SURF, 20)))
         END IF
      ELSE
      END IF
   ELSE
!       NOT AC,AD,AE,AF,AG,AH,AI,AJ,AK OR AL
   END IF
   IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR') THEN
      IF(surf_toric_flag(SURF) == 0) THEN
         WRITE(OUTLYNE,*)'SURFACE',SURF,' NOT A TORIC'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL(&
         & 'RE-DEFINE SURFACE TYPE AS X OR Y-TORIC AND THEN'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(surf_anamorphic_flag(SURF) /= 1) THEN
         IF(WC.EQ.'ADTOR') THEN
            W1=W1
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            DF1=0
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AETOR') THEN
            W2=W1
            W1=0.0D0
            W3=0.0D0
            W4=0.0D0
            DF1=1
            DF2=0
            DF3=1
            DF4=1
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AFTOR') THEN
            W3=W1
            W1=0.0D0
            W2=0.0D0
            W4=0.0D0
            DF1=1
            DF2=1
            DF3=0
            DF4=1
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         IF(WC.EQ.'AGTOR') THEN
            W4=W1
            W1=0.0D0
            W2=0.0D0
            W3=0.0D0
            W4=0.0D0
            DF1=1
            DF2=1
            DF3=1
            DF4=0
            DF5=1
            SQ=0
            SST=0
         ELSE
         END IF
         CALL STASPH
      ELSE
      END IF
      IF(surf_anamorphic_flag(SURF) == 1) THEN
         IF(SQ.EQ.0) THEN
            IF(WC.EQ.'ADTOR') call set_surf_anamorphic_coeff(SURF, 4, W1)
            IF(WC.EQ.'AETOR') call set_surf_anamorphic_coeff(SURF, 6, W1)
            IF(WC.EQ.'AFTOR') call set_surf_anamorphic_coeff(SURF, 8, W1)
            IF(WC.EQ.'AGTOR') call set_surf_anamorphic_coeff(SURF, 10, W1)
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'ADTOR') call set_surf_anamorphic_coeff(SURF, 4, surf_anamorphic_coeff(SURF, 4)+W1)
            IF(WC.EQ.'AETOR') call set_surf_anamorphic_coeff(SURF, 6, surf_anamorphic_coeff(SURF, 6)+W1)
            IF(WC.EQ.'AFTOR') call set_surf_anamorphic_coeff(SURF, 8, surf_anamorphic_coeff(SURF, 8)+W1)
            IF(WC.EQ.'AGTOR') call set_surf_anamorphic_coeff(SURF, 10, surf_anamorphic_coeff(SURF, 10)+W1)
         END IF
         IF(WQ.EQ.'DELT') THEN
            IF(WC.EQ.'ADTOR')call set_surf_anamorphic_coeff(SURF, 4, surf_anamorphic_coeff(SURF, 4)+(W1*0.01D0*surf_anamorphic_coeff(SURF, 4)))
            IF(WC.EQ.'AETOR')call set_surf_anamorphic_coeff(SURF, 6, surf_anamorphic_coeff(SURF, 6)+(W1*0.01D0*surf_anamorphic_coeff(SURF, 6)))
            IF(WC.EQ.'AFTOR')call set_surf_anamorphic_coeff(SURF, 8, surf_anamorphic_coeff(SURF, 8)+(W1*0.01D0*surf_anamorphic_coeff(SURF, 8)))
            IF(WC.EQ.'AGTOR')call set_surf_anamorphic_coeff(SURF, 10, surf_anamorphic_coeff(SURF, 10)+(W1*0.01D0*surf_anamorphic_coeff(SURF, 10)))
         END IF
      ELSE
      END IF
   ELSE
!       NOT ADTOR,AETOR,AFTOR OR AGTOR
   END IF
!
!       IF THERE ARE PIKUPS FOR AD,AE,AF,AG,AH,AI,AJ,AK OR AL
!       THEN DELETE THEM.
!
!       CHECK FOR ANY PIKUPS
!
!       CHECK FOR PIKUP
   IF(PIKUP(1,SURF,CT).EQ.1.0D0)THEN
!       DELETE PIKUP
      PIKUP(1:6,SURF,CT)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      IF(WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'AC'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL')WRITE(OUTLYNE,*)'SURFACE',SURF,' : PIKUP (',WC(1:2),') DELETED'
      IF(WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR')WRITE(OUTLYNE,*)'SURFACE',SURF,' : PIKUP (',WC(1:5),') DELETED'
      CALL SHOWIT(1)
   END IF
!       DUMP PIKUP PRO AND NPRO IF FOUND
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1:6,SURF,11)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1:6,SURF,12)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
!       NOW SEE IF THERE ARE AND REMAINING PIKUPS. IF NOT, SET
!       surf_pickup_count(SURF) TO ZERO
   END IF
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0) call set_surf_pickup_count(SURF, 0)
!
   RETURN
END
! SUB SPIVOT.FOR
SUBROUTINE SPIVOT
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIVOT WHICH IMPLEMENTS THE PIVOT
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!       THE DEC COMMAND IS:
!
!               PIVOT X Y Z
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIVOT" TAKES NO QUALIFIER OR STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIVOT" TAKES NO NUMERIC WORD #4 OR #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIVOT" REQUIRES SOME EXPLICIT NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               PROCEED
!
!       CHECK FOR PIKUP PIVX IF FOUND REMOVE
!
      IF(PIKUP(1,SURF,34).EQ.1.0D0) THEN
!       DELETE PIKUP PIVX
         PIKUP(1:6,SURF,34)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVX DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,35).EQ.1.0D0) THEN
!       DELETE PIKUP PIVY
         PIKUP(1:6,SURF,35)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVY DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,36).EQ.1.0D0) THEN
!       DELETE PIKUP PIVZ
         PIKUP(1:6,SURF,36)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVZ DELETED'
         CALL SHOWIT(1)
      END IF
!
!       SET surf_pickup_count(SURF) IF NO OTHER PIKUPS EXIST
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) call set_surf_pickup_count(SURF, 0)
!
!       PROCEED WITH PIVOT ASSIGNMENT
!
      IF(W1.EQ.0.0D0.AND.W2.EQ.0.0D0.AND.W3.EQ.0.0D0) THEN
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         call set_surf_pivot_flag(SURF, 1)
         call set_surf_pivot_x(SURF, W1)
         call set_surf_pivot_y(SURF, W2)
         call set_surf_pivot_z(SURF, W3)
         call set_surf_decenter_y(SURF, 0.0D0)
         call set_surf_decenter_x(SURF, 0.0D0)
         call set_surf_decenter_z(SURF, 0.0D0)
         call set_surf_focus_dx(SURF, 0.0D0)
         call set_surf_focus_dy(SURF, 0.0D0)
         call set_surf_focus_dz(SURF, 0.0D0)
      ELSE
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         call set_surf_pivot_flag(SURF, 1)
         call set_surf_pivot_x(SURF, W1)
         call set_surf_pivot_y(SURF, W2)
         call set_surf_pivot_z(SURF, W3)
         call set_surf_decenter_y(SURF, 0.0D0)
         call set_surf_decenter_x(SURF, 0.0D0)
         call set_surf_decenter_z(SURF, 0.0D0)
         call set_surf_focus_dx(SURF, 0.0D0)
         call set_surf_focus_dy(SURF, 0.0D0)
         call set_surf_focus_dz(SURF, 0.0D0)
      END IF
!
!       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
!       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
!       IF THE CURRENT WORK HERE CHANGED THE STATUS OF surf_decenter_flag(SURF)
!       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
!       RESOLVE THIS ISSUE.
!
      DO 100 I=0,INT(sys_last_surf())
         IF(PIKUP(1,I,34).EQ.1.0D0) THEN
            IF(INT(PIKUP(2,I,34)).EQ.SURF) THEN
!       UPDATE THE ALENS(59) ON THE PIKING SURFACE
               call set_surf_pivot_flag(I, surf_pivot_flag(SURF))
            ELSE
!       DONT DO ANYTHING
            END IF
         ELSE
!       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
         END IF
100   CONTINUE
!
   END IF
   RETURN
END
! SUB SPIVOTD.FOR
SUBROUTINE SPIVOTD
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIVOTD WHICH IMPLEMENTS THE PIVOTD
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!               PIVOT X Y Z
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"PIVOTD" TAKES NO ADDITIONAL INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               PROCEED
!
!       CHECK FOR PIKUP PIVX IF FOUND REMOVE
!
      IF(PIKUP(1,SURF,34).EQ.1.0D0) THEN
!       DELETE PIKUP PIVX
         PIKUP(1:6,SURF,34)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVX DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,35).EQ.1.0D0) THEN
!       DELETE PIKUP PIVY
         PIKUP(1:6,SURF,35)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVY DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,36).EQ.1.0D0) THEN
!       DELETE PIKUP PIVZ
         PIKUP(1:6,SURF,36)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP PIVZ DELETED'
         CALL SHOWIT(1)
      END IF
!
!       SET surf_pickup_count(SURF) IF NO OTHER PIKUPS EXIST
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) call set_surf_pickup_count(SURF, 0)
!
!       PROCEED WITH PIVOTD
!
      call set_surf_pivot_flag(SURF, 0)
      ALENS(78:79,SURF)=0.0D0
      call set_surf_pivot_z(SURF, 0.0D0)
      ALENS(30:31,SURF)=0.0D0
      call set_surf_decenter_z(SURF, 0.0D0)
      ALENS(114:116,SURF)=0.0D0
!
!       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
!       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
!       IF THE CURRENT WORK HERE CHANGED THE STATUS OF surf_decenter_flag(SURF)
!       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
!       RESOLVE THIS ISSUE.
!
      DO 100 I=0,INT(sys_last_surf())
         IF(PIKUP(1,I,34).EQ.1.0D0) THEN
            IF(INT(PIKUP(2,I,34)).EQ.SURF) THEN
!       UPDATE THE ALENS(59) ON THE PIKING SURFACE
               call set_surf_pivot_flag(I, surf_pivot_flag(SURF))
            ELSE
!       DONT DO ANYTHING
            END IF
         ELSE
!       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
         END IF
100   CONTINUE
!
   END IF
   RETURN
END
! SUB SPIVAX.FOR
SUBROUTINE SPIVAX
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SPIVAX WHICH IMPLEMENTS THE PIVAXIS
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!       THE COMMAND IS:
!
!               PIVAXIS (LOCAL OR NORMAL)
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIVAXIS" TAKES NO NUMERIC OR STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.1.AND.WQ.NE.'LOCAL'.AND.WQ.NE.'NORMAL') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"PIVAXIS" TAKES QUALIFIER WORDS "LOCAL" AND "NORMAL" ONLY'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(SURF.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'LOCAL') call set_surf_pivot_axis(SURF, 0)
   IF(WQ.EQ.'NORMAL') call set_surf_pivot_axis(SURF, 1)
   RETURN
END
! SUB SCW.FOR
SUBROUTINE SCW
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_wl_pri1, sys_wl_pri2, sys_wl_ref, sys_wl_sec1, sys_wl_sec2, &
      & sys_set_wl_pri1, sys_set_wl_pri2, sys_set_wl_ref, sys_set_wl_sec1, sys_set_wl_sec2
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SCW WHICH IMPLEMENTS THE CW COMMAND
!       AT THE LENS OF UPDATE LENS LEVEL OR THE CW COMMAND AT
!       THE CMD LEVEL
!
!
   IF(WC.EQ.'CW') THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
      ELSE
!       AT CMD
      END IF
!
      IF(F1.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'AT THE CMD LEVEL, "CW" TAKES NO EXPLICIT INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         WRITE(OUTLYNE,1000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,2000) INT(sys_wl_ref())
         CALL SHOWIT(0)
         RETURN
      ELSE
!               NOT AT CMD LEVEL
!
         IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
            IF(SQ.EQ.1.OR.SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1 .OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
               CALL REPORT_ERROR_AND_FAIL(&
               & '"CW" ONLY ACCEPTS NUMERIC WORD #1 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
!
            IF(DF1.EQ.1) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"CW" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
            IF(S1.EQ.0) THEN
               WRITE(OUTLYNE,1000)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,2000) INT(sys_wl_ref())
               CALL SHOWIT(0)
               RETURN
            ELSE
               IF(W1.LT.1.0.OR.W1.GT.10.0D0)THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'NUMERIC INPUT TO "CW" OUTSIDE ALLOWED BOUNDS'//'\n'//&
                  & 'RE-ENTER COMMAND', 1)
                  RETURN
               ELSE
                  call sys_set_wl_ref(W1)
                  RETURN
               END IF
            END IF
         END IF
      END IF
   END IF
   IF(WC.EQ.'PCW') THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
      ELSE
!       AT CMD
      END IF
      IF(F1.EQ.1) THEN
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
         IF(SN.EQ.1.OR.SQ.EQ.1.OR.SST.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'AT THE CMD LEVEL, "PCW" TAKES NO INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         WRITE(OUTLYNE,3000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,4000) INT(sys_wl_pri1()),INT(sys_wl_pri2())
         CALL SHOWIT(0)
         RETURN
      ELSE
!               NOT AT CMD LEVEL
!
         IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
            IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1)THEN
!
               CALL REPORT_ERROR_AND_FAIL(&
               & '"PCW" ONLY ACCEPTS NUMERIC WORD #1 AND #2 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"PCW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
            IF(S1.EQ.0.AND.S2.EQ.0) THEN
               WRITE(OUTLYNE,3000)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,4000) INT(sys_wl_pri1()),INT(sys_wl_pri2())
               CALL SHOWIT(0)
               RETURN
            ELSE
               IF(W1.LT.1.0.OR.W1.GT.10.0.OR.W2.LT.1.0.OR.W2.GT.10.0D0)THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'NUMERIC INPUT TO "PCW" OUTSIDE ALLOWED BOUNDS'//'\n'//&
                  & 'RE-ENTER COMMAND', 1)
                  RETURN
               ELSE
                  call sys_set_wl_pri1(W1)
                  call sys_set_wl_pri2(W2)
                  F22=1
                  RETURN
               END IF
            END IF
         END IF
      END IF
   END IF
   IF(WC.EQ.'SCW') THEN
      IF(F5.EQ.1.OR.F6.EQ.1) THEN
      END IF
      IF(F1.EQ.1) THEN
!               CHECK FOR PRESENCE OF ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
         IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'AT THE CMD LEVEL, "SCW" TAKES NO INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         WRITE(OUTLYNE,5000)
         CALL SHOWIT(0)
         WRITE(OUTLYNE,6000) INT(sys_wl_sec1()),INT(sys_wl_sec2())
         CALL SHOWIT(0)
         RETURN
      ELSE
!               NOT AT CMD LEVEL
!
         IF(F5.EQ.1.OR.F6.EQ.1) THEN
!
!               CHECK FOR PRESENCE OF QUALIFIER OR NUMERIC WORDS
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
            IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
!
               CALL REPORT_ERROR_AND_FAIL(&
               & '"SCW" ONLY ACCEPTS NUMERIC WORD #1 AND #2 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
!
            IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"SCW" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
!
!               WE ARE AT LENS INPUT OR LENS UPDATE LEVEL
!
            IF(S1.EQ.0.AND.S2.EQ.0) THEN
               WRITE(OUTLYNE,5000)
               CALL SHOWIT(0)
               WRITE(OUTLYNE,6000) INT(sys_wl_sec1()),INT(sys_wl_sec2())
               CALL SHOWIT(0)
               RETURN
            ELSE
               IF(W1.LT.1.0.OR.W1.GT.10.0.OR.W2.LT.1.0.OR.W2.GT.10.0D0)THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'NUMERIC INPUT TO "SCW" OUTSIDE ALLOWED BOUNDS'//'\n'//&
                  & 'RE-ENTER COMMAND', 1)
                  RETURN
               ELSE
                  call sys_set_wl_sec1(W1)
                  call sys_set_wl_sec2(W2)
                  F22=1
                  RETURN
               END IF
            END IF
         END IF
      END IF
   END IF
!
1000 FORMAT('CONTROL WAVELENGTH IS WAVLENGTH')
2000 FORMAT(10X,I2)
3000 FORMAT('PRIMARY WAVELENGTH PAIRS ARE WAVELENGTHS')
4000 FORMAT(5X,I2,2X,'AND',2X,I2)
5000 FORMAT('SECONDARY WAVELENGTH PAIRS ARE WAVELENGTHS')
6000 FORMAT(5X,I2,2X,'AND',2X,I2)
   RETURN
END
! SUB SCVR.FOR
SUBROUTINE SCVR
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use mod_system, only: sys_units
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SCVR WHICH IMPLEMENTS THE CVTOR AND RDTOR
!       COMMANDS AT THE UPDATE LENS LEVEL.
!       A YTORIC OR AN XTORIC COMMAND MUST BE ISSUED BEFORE TORIC DATA
!       CAN BE ENTERD FOR A SURFACE.
!
   INTEGER I
!
   REAL*8 DR,NEWRAD,ARG1,RADIUS,APER,WAVE,WAVER,VAL1,RAD
!
!
   IF(SURF.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL('OBJECT SURFACE MAY NOT BE A TORIC'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
   IF(WQ.NE.'DELTFR') THEN
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'CVTOR') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"CVTOR" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'RDTOR') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"RDTOR" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         RETURN
      END IF
   ELSE
! DELTFR
      IF(S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'CVTOR') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"CVTOR DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'RDTOR') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"RDTOR DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         RETURN
      END IF
      IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0 .AND.DF3.EQ.0) THEN
!
         IF(WC.EQ.'CVTOR') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"CVTOR DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'RDTOR') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"RDTOR DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         RETURN
      END IF
   END IF
   IF(SST.EQ.1) THEN
!
      IF(WC.EQ.'CVTOR') THEN
         CALL REPORT_ERROR_AND_FAIL('"CVTOR" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'RDTOR') THEN
         CALL REPORT_ERROR_AND_FAIL('"RDTOR" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      RETURN
   END IF
   IF(SQ.EQ.1.AND.F5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"'//WC(1:5)//'" TAKES NO QUALIFIER INPUT DURING LENS INPUT MODE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELTFR') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID QUALIFIER USED WITH "'//WC(1:5)//'"'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"'//WC(1:5)//'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF

   IF(WC.EQ.'RDTOR'.OR.WC.EQ.'CVTOR') THEN
!
!       IF THE SURFACE WAS NOT A TORIC THEN SAY SO AND RETURN
!
      IF(surf_toric_flag(SURF) == 0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'A "YTORIC" OR "XTORIC" COMMAND MUST BE ENTERED'//'\n'//&
         & 'PRIOR TO TORIC DATA BEING ENTERED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!       surf_toric_flag(SURF) NOT ZERO, MUST BE 1.0 (Y-TORIC)
!       OR 2.0 (X-TORIC).
!
      IF(WC.EQ.'CVTOR') THEN
         IF(SQ.EQ.0) call set_surf_toric_curvature(SURF, W1)
         IF(WQ.EQ.'DELT') call set_surf_toric_curvature(SURF, surf_toric_curvature(SURF)+W1)
         IF(WQ.EQ.'CENT')call set_surf_toric_curvature(SURF, surf_toric_curvature(SURF)+(W1*0.01D0*surf_toric_curvature(SURF)))
         IF(WQ.EQ.'DELTFR') THEN
            IF(DF2.EQ.0) WAVER=W2
            IF(DF2.EQ.1) WAVER=0.5461D0
            IF(DF3.EQ.0) APER=W3
!
            IF(DF3.EQ.1) THEN
               IF(surf_clap_type(SURF) == 0.OR.surf_ccr_flag(SURF) /= 0) THEN
!     USE LAST PARAXIAL DATA
                  APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
               ELSE
                  IF(surf_clap_type(SURF) == 1) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                     IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                     IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                  END IF
                  IF(surf_clap_type(SURF) >= 2.AND.surf_clap_type(SURF) <= 4) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                     IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                     IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                  END IF
               END IF
            END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
            IF(sys_units().EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
            IF(sys_units().EQ.2.0D0) WAVE=WAVER*1.0D-4
            IF(sys_units().EQ.3.0D0) WAVE=WAVER*1.0D-3
            IF(sys_units().EQ.4.0D0) WAVE=WAVER*1.0D-6
!
            IF(surf_toric_curvature(SURF).NE.0.0D0) THEN
!     CURVED SURFACE
               RAD=1.0D0/surf_toric_curvature(SURF)
               RADIUS=DABS(1.0D0/surf_toric_curvature(SURF))
               ARG1=(RADIUS**2)-((APER/2.0D0)**2)
               IF(ARG1.LT.0.0D0) THEN
                  CALL REPORT_ERROR_AND_FAIL(&
                  & 'RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'//'\n'//&
                  & 'RE-ENTER COMMAND', 1)
                  RETURN
               END IF
               IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
               IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)DR=W1*(DABS((WAVE*DSQRT(ARG1))/(2.0D0*(DSQRT(ARG1)-RADIUS))))
               IF(DR.EQ.0.0D0) call set_surf_toric_curvature(SURF, surf_toric_curvature(SURF))
               IF(DR.NE.0.0D0) THEN
                  NEWRAD=RAD+DR
                  IF(NEWRAD.EQ.0.0D0) call set_surf_toric_curvature(SURF, 0.0D0)
                  IF(NEWRAD.NE.0.0D0) call set_surf_toric_curvature(SURF, 1.0D0/NEWRAD)
               END IF
            ELSE
!     SURFACE WAS FLAT
               NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))+(W1*WAVE/4.0D0)
               IF(NEWRAD.EQ.0.0D0) call set_surf_toric_curvature(SURF, 0.0D0)
               IF(NEWRAD.NE.0.0D0) call set_surf_toric_curvature(SURF, 1.0D0/NEWRAD)
!
            END IF
!     DONE WITH DELTFR
!
         END IF
         RETURN
      ELSE
      END IF
!     WC MUST BE RDTOR
      IF(WQ.EQ.'DELTFR') THEN
         IF(DF2.EQ.0) WAVER=W2
         IF(DF2.EQ.1) WAVER=0.5461D0
         IF(DF3.EQ.0) APER=W3
         IF(DF3.EQ.1) THEN
            IF(surf_clap_type(SURF) == 0.OR.surf_ccr_flag(SURF) /= 0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(surf_clap_type(SURF) == 1) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
               END IF
               IF(surf_clap_type(SURF) >= 2.AND.surf_clap_type(SURF) <= 4) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
         IF(sys_units().EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
         IF(sys_units().EQ.2.0D0) WAVE=WAVER*1.0D-4
         IF(sys_units().EQ.3.0D0) WAVE=WAVER*1.0D-3
         IF(sys_units().EQ.4.0D0) WAVE=WAVER*1.0D-6
         IF(surf_toric_curvature(SURF).NE.0.0D0) RADIUS=DABS(1.0D0/surf_toric_curvature(SURF))
         IF(surf_toric_curvature(SURF).NE.0.0D0) RAD=1.0D0/surf_toric_curvature(SURF)
         IF(surf_toric_curvature(SURF).EQ.0.0D0) RADIUS=0.0D0
!
         IF(RADIUS.NE.0.0D0) THEN
!     CURVED SURFACE
            ARG1=(RADIUS**2)-((APER/2.0D0)**2)
            IF(ARG1.LT.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
            IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)DR=W1*(DABS((WAVE*DSQRT(ARG1))/(2.0D0*(DSQRT(ARG1)-RADIUS))))
            IF(DR.EQ.0.0D0) call set_surf_toric_curvature(SURF, surf_toric_curvature(SURF))
            IF(DR.NE.0.0D0) THEN
               NEWRAD=RAD+DR
               IF(NEWRAD.EQ.0.0D0) call set_surf_toric_curvature(SURF, 0.0D0)
               IF(NEWRAD.NE.0.0D0) call set_surf_toric_curvature(SURF, 1.0D0/NEWRAD)
            END IF
         ELSE
!     SURFACE WAS FLAT
            NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))+(W1*WAVE/4.0D0)
            IF(NEWRAD.EQ.0.0D0) call set_surf_toric_curvature(SURF, 0.0D0)
            IF(NEWRAD.NE.0.0D0) call set_surf_toric_curvature(SURF, 1.0D0/NEWRAD)
!
         END IF
!     DONE WITH DELTFR
      END IF
      IF(W1.EQ.0.0D0) THEN
         IF(SQ.EQ.0) call set_surf_toric_curvature(SURF, 0.0D0)
         IF(WQ.EQ.'DELT') call set_surf_toric_curvature(SURF, surf_toric_curvature(SURF))
         IF(WQ.EQ.'CENT') call set_surf_toric_curvature(SURF, surf_toric_curvature(SURF))
      ELSE
         IF(SQ.EQ.0) call set_surf_toric_curvature(SURF, 1.0D0/(W1))
         IF(WQ.EQ.'DELT') THEN
            IF(surf_toric_curvature(SURF).EQ.0.0D0) VAL1=0.0D0
            IF(surf_toric_curvature(SURF).NE.0.0D0) VAL1=1.0D0/surf_toric_curvature(SURF)
            VAL1=VAL1+W1
            IF(VAL1.EQ.0.0D0) call set_surf_toric_curvature(SURF, 0.0D0)
            IF(VAL1.NE.0.0D0) call set_surf_toric_curvature(SURF, 1.0D0/VAL1)
         END IF
         IF(WQ.EQ.'CENT') THEN
            IF(surf_toric_curvature(SURF).EQ.0.0D0) VAL1=0.0D0
            IF(surf_toric_curvature(SURF).NE.0.0D0) VAL1=1.0D0/surf_toric_curvature(SURF)
            VAL1=VAL1+(W1*0.01D0*VAL1)
            IF(VAL1.EQ.0.0D0) call set_surf_toric_curvature(SURF, 0.0D0)
            IF(VAL1.NE.0.0D0) call set_surf_toric_curvature(SURF, 1.0D0/VAL1)
         END IF
      END IF
!
!       IF AN RDTOR OR CVTOR PIKUP EXISTS, GET RID OF IT
!
      IF(PIKUP(1,SURF,9).EQ.1.0D0) THEN
         call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :(CVTOR) PIKUP DELETED'
         CALL SHOWIT(1)
         PIKUP(1:6,SURF,9)=0.0D0
      END IF
      IF(PIKUP(1,SURF,10).EQ.1.0D0) THEN
         call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :(RDTOR) PIKUP DELETED'
         CALL SHOWIT(1)
         PIKUP(1:6,SURF,10)=0.0D0
      END IF
!       NOW CHECK FOR CURVATURE SOLVES AND DELETE THE APPROPRIATE
!       ONES. IF THE SURFACE IS A Y-TORIC, DELETE XZ CURVATURE
!       SOLVES. IF THE SURFACE IS AN X-TORIC DELETE YZ CURVATURE SOLVES
!
      IF(surf_toric_flag(SURF) == 1) THEN
!       Y-TORIC, ARE THERE XZ CURVATURE SOLVES
         IF(SOLVE(2,SURF).GT.0.0D0) THEN
!       SOLVE TO DELETE IS FOUND
            SOLVE(2,SURF)=0.0D0
            SOLVE(1,SURF)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVE DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(surf_toric_flag(SURF) == 2) THEN
!       X-TORIC, ARE THERE YZ CURVATURE SOLVES
         IF(SOLVE(8,SURF).GT.0.0D0) THEN
!       SOLVE TO DELETE IS FOUND
            SOLVE(8,SURF)=0.0D0
            SOLVE(9,SURF)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVE DELETED'
            CALL SHOWIT(1)
         END IF
      END IF
!       NOW RE-CALCULATE surf_solve_flag(SURF) THE SOLVE COUNTER
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   ELSE
!       NOT RDTOR OR CVTOR
   END IF
   RETURN
END
! SUB SCV.FOR
SUBROUTINE SCV
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use mod_system, only: sys_units
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SCV WHICH IMPLEMENTS THE CV AND RD
!       COMMANDS AT THE LENS OF UPDATE LENS LEVEL.
!       THE SECOND NUMERIC WORD IS THE CONIC CONSTANT
!       ASSOCIATED WITH THIS SURFACE.
!
   INTEGER PIKCNT,I
!
   REAL*8 VAL1,WAVE,WAVER,APER,RADIUS,DR,NR,RAD ,NEWRAD,ARG1
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
   IF(WQ.NE.'DELTFR'.AND.WQ.NE.'SAG') THEN
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
         IF(WC.EQ.'CV') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"CV" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'RD') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"RD" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         RETURN
      END IF
   ELSE
! DELTFR OR SAG
      IF(WQ.EQ.'DELTFR') THEN
         IF(S4.EQ.1.OR.S5.EQ.1) THEN
!
            IF(WC.EQ.'CV') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"CV DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"RD DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            RETURN
         END IF
         IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0 .AND.DF3.EQ.0) THEN
!
            IF(WC.EQ.'CV') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"CV DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"RD DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AND #3 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'SAG') THEN
         IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
!
            IF(WC.EQ.'CV') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"CV SAG" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"RD SAG" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            RETURN
         END IF
         IF(W2.LE.0.0D0 .AND.DF2.EQ.0) THEN
!
            IF(WC.EQ.'CV') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"CV SAG" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF(WC.EQ.'RD') THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"RD SAG" REQUIRES POSITIVE NUMERIC WORD #2 INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            RETURN
         END IF
      END IF
!     NOT DELTFR OR SAG
   END IF
   IF(SST.EQ.1) THEN
!
      IF(WC.EQ.'CV') THEN
         CALL REPORT_ERROR_AND_FAIL('"CV" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'RD') THEN
         CALL REPORT_ERROR_AND_FAIL('"RD" TAKES NO STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      RETURN
   END IF
   IF(F6.EQ.1) THEN
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT'.AND.WQ.NE.'DELTFR'.AND.WQ.NE.'SAG') THEN
!
         IF(WC.EQ.'CV') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'INVALID QUALIFIER USED WITH "CV"'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'RD') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'INVALID QUALIFIER USED WITH "RD"'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         RETURN
      END IF
   END IF
   IF(F5.EQ.1) THEN
      IF(SQ.EQ.1) THEN
!
         IF(WC.EQ.'CV') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"CV" TAKES NO QUALIFIER WORDS IN LENS INPUT MODE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         IF(WC.EQ.'RD') THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"RD" TAKES NO QUALIFIER WORDS IN LENS INPUT MODE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         RETURN
      END IF
   END IF
   IF(DF1.EQ.1) THEN
!
      IF(WC.EQ.'CV') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"CV" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(WC.EQ.'RD') THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & '"RD" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      RETURN
   END IF
!
   IF(WC.EQ.'RD'.AND.SQ.EQ.0.AND.DF2.EQ.0) call set_surf_conic(SURF, W2)
!
   IF(WC.EQ.'CV') THEN
      IF(SQ.EQ.0) call set_surf_curvature(SURF, W1)
      IF(SQ.EQ.0.AND.DF2.EQ.0) call set_surf_conic(SURF, W2)
      IF(WQ.EQ.'DELTFR') THEN
         IF(DF2.EQ.0) WAVER=W2
         IF(DF2.EQ.1) WAVER=0.5461D0
         IF(DF3.EQ.0) APER=2.0D0*W3
!
         IF(DF3.EQ.1) THEN
            IF(surf_clap_type(SURF) == 0.OR.surf_array_parity(SURF) /= 0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(surf_clap_type(SURF) == 1) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
               END IF
               IF(surf_clap_type(SURF) >= 2.AND.surf_clap_type(SURF) <= 4) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
         IF(sys_units().EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
         IF(sys_units().EQ.2.0D0) WAVE=WAVER*1.0D-4
         IF(sys_units().EQ.3.0D0) WAVE=WAVER*1.0D-3
         IF(sys_units().EQ.4.0D0) WAVE=WAVER*1.0D-6
!
         IF(surf_curvature(SURF).NE.0.0D0) THEN
!     CURVED SURFACE
            RADIUS=DABS(1.0D0/surf_curvature(SURF))
            RAD=1.0D0/surf_curvature(SURF)
            ARG1=(RADIUS**2)-((APER/2.0D0)**2)
            IF(ARG1.LT.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
            IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)DR=W1*(DABS((WAVE*DSQRT(ARG1))/(2.0D0*(DSQRT(ARG1)-RADIUS))))
            IF(DR.EQ.0.0D0) call set_surf_curvature(SURF, surf_curvature(SURF))
            IF(DR.NE.0.0D0) THEN
               NEWRAD=RAD+DR
               IF(NEWRAD.EQ.0.0D0) call set_surf_curvature(SURF, 0.0D0)
               IF(NEWRAD.NE.0.0D0) call set_surf_curvature(SURF, 1.0D0/NEWRAD)
            END IF
         ELSE
!     SURFACE WAS FLAT
            NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))+(W1*WAVE/4.0D0)
            IF(NEWRAD.EQ.0.0D0) call set_surf_curvature(SURF, 0.0D0)
            IF(NEWRAD.NE.0.0D0) call set_surf_curvature(SURF, 1.0D0/NEWRAD)
!
         END IF
!     DONE WITH DELTFR
!
      END IF
      IF(WQ.EQ.'SAG') THEN
         IF(DF2.EQ.0) APER=2.0D0*W2
!
         IF(DF2.EQ.1) THEN
            IF(surf_clap_type(SURF) == 0.AND.surf_array_parity(SURF) /= 0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(surf_clap_type(SURF) == 1) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
               END IF
               IF(surf_clap_type(SURF) >= 2.AND.surf_clap_type(SURF) <= 4) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
         ARG1=(((W1**2)*(surf_conic(SURF)+1.0D0))+((APER/2.0D0)**2))
         IF(ARG1.LT.0.0D0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         call set_surf_curvature(SURF, (2.0D0*W1)/(((W1**2)*(surf_conic(SURF)+1.0D0))+((APER/2.0D0)**2)))
!     DONE WITH SAG
!
      END IF
      IF(WQ.EQ.'DELT') call set_surf_curvature(SURF, surf_curvature(SURF)+W1)
      IF(WQ.EQ.'CENT')call set_surf_curvature(SURF, surf_curvature(SURF)+(W1*0.01D0*surf_curvature(SURF)))
   ELSE
   END IF
!     WC MUST BE RD
   IF(WC.EQ.'RD') THEN
      IF(WQ.EQ.'DELTFR') THEN
         IF(DF2.EQ.0) WAVER=W2
         IF(DF2.EQ.1) WAVER=0.5461D0
         IF(DF3.EQ.0) APER=2.0D0*W3
         IF(DF3.EQ.1) THEN
            IF(surf_clap_type(SURF) == 0.AND.surf_array_parity(SURF) /= 0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(surf_clap_type(SURF) == 1) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
               END IF
               IF(surf_clap_type(SURF) >= 2.AND.surf_clap_type(SURF) <= 4) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!       WAVER=WAVELENGTH IN MICRONS (DEFAULT = 0.5461)
         IF(sys_units().EQ.1.0D0) WAVE=(WAVER*1.0D-3)/(25.4D0)
         IF(sys_units().EQ.2.0D0) WAVE=WAVER*1.0D-4
         IF(sys_units().EQ.3.0D0) WAVE=WAVER*1.0D-3
         IF(sys_units().EQ.4.0D0) WAVE=WAVER*1.0D-6
         IF(surf_curvature(SURF).NE.0.0D0) RADIUS=DABS(1.0D0/surf_curvature(SURF))
         IF(surf_curvature(SURF).NE.0.0D0) RAD=1.0D0/surf_curvature(SURF)
         IF(surf_curvature(SURF).EQ.0.0D0) RADIUS=0.0D0
!
         IF(RADIUS.NE.0.0D0) THEN
!     CURVED SURFACE
            ARG1=(RADIUS**2)-((APER/2.0D0)**2)
            IF(ARG1.LT.0.0D0) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & 'RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            IF((DSQRT(ARG1)-RADIUS).EQ.0.0D0) DR=0.0D0
            IF((DSQRT(ARG1)-RADIUS).NE.0.0D0)DR=W1*(DABS((WAVE*DSQRT(ARG1))/(2.0D0*(DSQRT(ARG1)-RADIUS))))
            IF(DR.EQ.0.0D0) call set_surf_curvature(SURF, surf_curvature(SURF))
            IF(DR.NE.0.0D0) THEN
               NEWRAD=RAD+DR
               IF(NEWRAD.EQ.0.0D0) call set_surf_curvature(SURF, 0.0D0)
               IF(NEWRAD.NE.0.0D0) call set_surf_curvature(SURF, 1.0D0/NEWRAD)
            END IF
         ELSE
!     SURFACE WAS FLAT
            NEWRAD=((1.0D0/(W1*WAVE))*((APER/2.0D0)**2))+(W1*WAVE/4.0D0)
            IF(NEWRAD.EQ.0.0D0) call set_surf_curvature(SURF, 0.0D0)
            IF(NEWRAD.NE.0.0D0) call set_surf_curvature(SURF, 1.0D0/NEWRAD)
!
         END IF
!     DONE WITH DELTFR
      END IF
      IF(WQ.EQ.'SAG') THEN
         IF(DF2.EQ.0) APER=2.0D0*W2
         IF(DF2.EQ.1) THEN
            IF(surf_clap_type(SURF) == 0.OR.surf_array_parity(SURF) /= 0) THEN
!     USE LAST PARAXIAL DATA
               APER=2.0D0*(DABS(PXTRAY(1,SURF))+DABS(PXTRAY(5,SURF)))
            ELSE
               IF(surf_clap_type(SURF) == 1) THEN
!     USE MIN CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
               END IF
               IF(surf_clap_type(SURF) >= 2.AND.surf_clap_type(SURF) <= 4) THEN
!     USE MAX CLAP DATA WITHOUT DEC OR TILTS
                  IF(surf_clap_dim(SURF, 1).GT.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 1))
                  IF(surf_clap_dim(SURF, 1).LE.surf_clap_dim(SURF, 2))APER=2.0D0*DABS(surf_clap_dim(SURF, 2))
               END IF
            END IF
         END IF
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
!
!       APER=PART DIAMETER IN CURRENT LENS UNITS (REQUIRED)
         ARG1=(((W1**2)*(surf_conic(SURF)+1.0D0))+((APER/2.0D0)**2))
         IF(ARG1.LT.0.0D0) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & 'RADIUS OR CURVATURE CHANGE CAN NOT BE DETERMINED'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         call set_surf_curvature(SURF, (2.0D0*W1)/(((W1**2)*(surf_conic(SURF)+1.0D0))+((APER/2.0D0)**2)))
!     DONE WITH SAG
!
      END IF
      IF(W1.EQ.0.0D0) THEN
         IF(SQ.EQ.0) call set_surf_curvature(SURF, 0.0D0)
         IF(WQ.EQ.'DELT') call set_surf_curvature(SURF, surf_curvature(SURF))
         IF(WQ.EQ.'CENT') call set_surf_curvature(SURF, surf_curvature(SURF))
      ELSE
         IF(SQ.EQ.0) call set_surf_curvature(SURF, 1.0D0/(W1))
         IF(WQ.EQ.'DELT') THEN
            IF(surf_curvature(SURF).EQ.0.0D0) VAL1=0.0D0
            IF(surf_curvature(SURF).NE.0.0D0) VAL1=1.0D0/surf_curvature(SURF)
            VAL1=VAL1+W1
            IF(VAL1.EQ.0.0D0) call set_surf_curvature(SURF, 0.0D0)
            IF(VAL1.NE.0.0D0) call set_surf_curvature(SURF, 1.0D0/VAL1)
         END IF
         IF(WQ.EQ.'CENT') THEN
            IF(surf_curvature(SURF).EQ.0.0D0) VAL1=0.0D0
            IF(surf_curvature(SURF).NE.0.0D0) VAL1=1.0D0/surf_curvature(SURF)
            VAL1=VAL1+(W1*0.01D0*VAL1)
            IF(VAL1.EQ.0.0D0) call set_surf_curvature(SURF, 0.0D0)
            IF(VAL1.NE.0.0D0) call set_surf_curvature(SURF, 1.0D0/VAL1)
         END IF
      END IF
   END IF
!
   IF(surf_curvature(SURF).EQ.0.0D0.AND.surf_conic(SURF).NE.0.0D0) THEN
      OUTLYNE='WARNING:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
      CALL SHOWIT(1)
      OUTLYNE='THE CONIC CONSTANT WILL BE IGNORED FOR THIS PLANO SURFACE'
      CALL SHOWIT(1)
   END IF
   IF(surf_curvature(SURF).NE.0.0D0.AND.surf_asphere_coeff(SURF, 2).NE.0.0D0) THEN
      call set_surf_asphere_coeff(SURF, 2, 0.0D0)
      OUTLYNE='WARNING:'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'FOR SURFACE ',SURF
      CALL SHOWIT(1)
      OUTLYNE='THE "AC" TERM RESET TO 0.0 FOR THIS NON-PLANO SURFACE'
      CALL SHOWIT(1)
   END IF
!
!       CHECK CURVATURE SOLVES
! IF THE SURFACE IS NOT A TORIC surf_toric_flag(SURF)=0 THEN DELETE ALL CURVATUVE SOLVES
!       AND RE-CALCULATE surf_solve_flag(SURF) THE SOLVE TRACKER
!       IF THE SURFACE IS AN X TORIC THEN DELETE ONLY THE XZ PLANE CURVATURE SOLVES
!       IF THE SURFACE IS AN Y TORIC THEN DELETE ONLY THE YZ PLANE CURVATURE SOLVES
!
!       CHECK FOR A TORIC
   IF(surf_toric_flag(SURF) == 0) THEN
!       NO TORIC, DELETE ALL CURVATURE SOLVE
      IF(SOLVE(8,SURF).NE.0.0D0) THEN
         SOLVE(8,SURF)=0.0D0
         SOLVE(9,SURF)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :ALL CURVATURE SOLVES DELETED'
         CALL SHOWIT(1)
         SOLVE(2,SURF)=0.0D0
         SOLVE(1,SURF)=0.0D0
      END IF
!       RE-CALCULATE surf_solve_flag(SURF)
      call set_surf_solve_flag(SURF, 0.0D0)
      IF(SOLVE(6,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
      IF(SOLVE(4,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
      IF(SOLVE(8,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
      IF(SOLVE(2,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
   ELSE
!       TORIC
!               CHECK FOR Y-TORIC
      IF(surf_toric_flag(SURF) == 1) THEN
!       Y-TORIC, DELETE ALL YZ PLANE CURVATURE SOLVES
         IF(SOLVE(8,SURF).NE.0.0D0) THEN
            SOLVE(8,SURF)=0.0D0
            SOLVE(9,SURF)=0.0D0
            WRITE(OUTLYNE,*)'SURFACE',SURF,' :YZ PLANE CURVATURE SOLVES DELETED'
            CALL SHOWIT(1)
!       RE-CALCULATE surf_solve_flag(SURF)
            call set_surf_solve_flag(SURF, 0.0D0)
            IF(SOLVE(6,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
            IF(SOLVE(4,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
            IF(SOLVE(8,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
            IF(SOLVE(2,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
         ELSE
!       NO SOLVES
         END IF
      ELSE
         IF(surf_toric_flag(SURF) == 2) THEN
!       X-TORIC, DELETE ALL XZ PLANE CURVATURE SOLVES
            IF(SOLVE(2,SURF).NE.0.0D0) THEN
               SOLVE(2,SURF)=0.0D0
               SOLVE(1,SURF)=0.0D0
               WRITE(OUTLYNE,*)'SURFACE',SURF,' :XZ PLANE CURVATURE SOLVES DELETED'
               CALL SHOWIT(1)
!       RE-CALCULATE surf_solve_flag(SURF)
               call set_surf_solve_flag(SURF, 0.0D0)
               IF(SOLVE(6,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+1.0D0)
               IF(SOLVE(4,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.1D0)
               IF(SOLVE(8,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+2.0D0)
               IF(SOLVE(2,SURF).GT.0.0D0) call set_surf_solve_flag(SURF, surf_solve_flag(SURF)+0.2D0)
            END IF
         ELSE
            CALL REPORT_ERROR_AND_FAIL('SERIOUS ERROR IN ASSIGNMENT OF surf_toric_flag()', 1)
            RETURN
         END IF
      END IF
   END IF
!
!       CHECK FOR RD OR CV PIKUPS AND DELETE IF FOUND
!
   IF(surf_pickup_count(SURF) == 0) THEN
!       NO PIKUPS DON'T DO ANYTHING
!
      RETURN
   ELSE
   END IF
!
!       CHECK FOR CV OR RD OR PRO OR NPRO
!       PIKUPS AND DELETE IF FOUND
!
   IF(PIKUP(1,SURF,1).EQ.0.0D0.AND.PIKUP(1,SURF,2).EQ.0.0D0.AND.PIKUP(1,SURF,11).EQ.0.0D0.AND.PIKUP(1,SURF,12).EQ.0.0D0) THEN
!
!       NO CV RD PRO OR NPRO PIKUPS, JUST RETURN
!
      RETURN
   ELSE
   END IF
!
!       DELETE THE PIKUP
   IF(PIKUP(1,SURF,1).GT.0.0D0) THEN
      PIKUP(1:6,SURF,1)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (RD) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,2).GT.0.0D0) THEN
      PIKUP(1:6,SURF,2)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (CV) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,11).GT.0.0D0) THEN
      PIKUP(1:6,SURF,11)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (PRO) DELETED'
      CALL SHOWIT(1)
   END IF
   IF(PIKUP(1,SURF,12).GT.0.0D0) THEN
      PIKUP(1:6,SURF,12)=0.0D0
      call set_surf_pickup_count(SURF, surf_pickup_count(SURF)-1)
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (NPRO) DELETED'
      CALL SHOWIT(1)
   END IF
!
!       ARE THERE MORE PIKUPS? IF NOT SET surf_pickup_count(SURF) TO ZERO.
!
   PIKCNT=0
   DO 10 I=1,PSIZ
      IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
         PIKCNT=PIKCNT+1
      ELSE
      END IF
10 CONTINUE
!
   IF(PIKCNT.EQ.0) call set_surf_pickup_count(SURF, 0)
   RETURN
END
! SUB SINS.FOR
SUBROUTINE SINS
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use mod_system, only: sys_astop, sys_last_surf, sys_ref_surf, &
      & sys_set_astop, sys_set_last_surf, sys_set_ref_surf
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SINS WHICH IMPLEMENTS THE INS
!       COMMANDS AT THE UPDATE LENS LEVEL.
!       THE FIRST NUMERIC WORD IS THE SURFACE WHICH WE WISH TO
!       ADD TO THE LENS. VALID VALUES ARE FROM 1 TO THE INTEGER VALUE
!       STORED IN SYSTEM(20). FOR EXAMPLE, IF THE COMMAND
!
!                       INS,15 IS ISSUED AND SYSTEM(20)=25
!       THEN A DUMMY BLANK DEFAULT SURFACE IS ADDED TO ALENS
!       BETWEEN EXISTING SURFACES 14 AND 15. OLD SURFACE
!       15 BECOMES SURFACE 16 AND SYSTEM(20) IS INCREMENTED BY 1.
!       THE CURRENT SURFACE BECOMES THE SURFACE INSERTER. IF INS
!       IS ISSUED WITH NO NUMERIC INPUT, A DUMMY SURFACE IS ENTERED
!       AHEAD OF THE CURRENT, CURRENT SURFACE. AFTER THE INSERTION
!       THE NEWLY INSERTED SURFACE BECOMES THE NEW CURRENT SURFACE.
!       SOLVE ARRAY AND PIKUPS ALSO UPDATED
!
   INTEGER K,L,I,J,JK,II,III,IV
!
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"INS" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1) THEN
      W1=DBLE(SURF)
      DF1=0
   ELSE
   END IF
   IF(DF2.EQ.1) THEN
      W2=1.0D0
      DF2=0
   ELSE
   END IF
   IF(GLANAM(INT(W1)-1,2).EQ.'PERFECT      ') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE MAY NOT BE INSERTED BEHIND A "PERFECT" SURFACE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(GLANAM(INT(W1)-1,2).EQ.'IDEAL        ') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'A SURFACE MAY NOT BE INSERTED BEHIND A "IDEAL" SURFACE'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!       IS DEFAULT SURFACE ENTRY IN EFFECT?
!
   DO II=1,INT(W2)
!
!       USE THE NUMERIC
!       VALUE OF NUMERIC WORD 1 AS THE LOCATION OF THE SURFACE
!       INSERTION.
      IF(W1.EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'SURFACE INSERTION INFRONT OF OBJECT SURF. NOT ALLOWED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(W1.LT.1.0D0.OR.W1.GT.sys_last_surf()) THEN
!
!       TRYING TO INSERT A SURFACE NUMBER INFRONT OF THE
!       OBJECT SURFACE OR AFTER THE IMAGE SURFACE. THIS OPERATION
!       IS NOT ALLOWED.
!       PRINT ERROR AND RETURN.
         CALL REPORT_ERROR_AND_FAIL(&
         & 'INVALID LOCATION FOR INSERTED SURFACE'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      ELSE
!       SURFACE NUMBER WITHIN VALID RANGE.
!
!
!       IS SURFACE INSERTED, INFRONT OF REFERENCE SURFACE ?
         IF(INT(W1).LE.INT(sys_ref_surf())) THEN
            call sys_set_ref_surf(sys_ref_surf()+1.0D0)
            NEWREF=INT(sys_ref_surf())
         ELSE
!       NOT IN FRONT OF REF SURF
         END IF
!       IS SURFACE INSERTED, INFRONT OF ASTOP SURFACE ?
         IF(INT(W1).LE.INT(sys_astop()).AND.sys_astop().NE.-99.0D0) THEN
            call sys_set_astop(sys_astop()+1.0D0)
         ELSE
!       NOT IN FRONT OF ASTOP SURFOR THERE IS NO ASTOP SURFACE
         END IF
!
         call sys_set_last_surf(sys_last_surf()+1.0D0)
         K=INT(W1)
         L=INT(sys_last_surf())
         DO J=(L-1),K,-1
            ALENS(1:LSIZ,(J+1))=ALENS(1:LSIZ,J)
         END DO
         DO J=(L-1),K,-1
            LBL(J+1)(1:80)=LBL(J)(1:80)
         END DO
!     NOW SPSRF DATA
         DO  J=(L-1),K,-1
            FTFL01(1:96,J+1)=FTFL01(1:96,J)
         END DO
         DO J=(L-1),K,-1
            GLANAM((J+1),1)=GLANAM(J,1)
            GLANAM((J+1),2)=GLANAM(J,2)
            SOLVE(0:9,J+1)=SOLVE(0:9,J)
         END DO
!
!     L IS THE NEW FINAL SURFACE OF THE LENS
         DO J=0,L
            DO I=1,PSIZ
               IF(I.NE.32) THEN
!     NOT THOAL
!       IF THE SURFACE NUMBER OF THE SURFACE BEING INSERTED IS
!       NUMERICALLY LESS THAN OR EQUAL TO SURFACE NUMBER REFERENCED
!       BY THE PIKUP (PIKUP(2,J,I) THEN
                  IF(surf_pickup_count(J) /= 0) THEN
!     THERE IS A PIKUP ON SURFACE J
                     IF(K.LE.INT(PIKUP(2,J-1,I))) THEN
                        PIKUP(2,J-1,I)=PIKUP(2,J-1,I)+1.0D0
                     ELSE
!       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
!       THE PIKUP
                     END IF
                  END IF
               ELSE
!     THOAL
                  IF(surf_pickup_count(J) /= 0) THEN
!     THERE IS A PIKUP ON SURFACE J
                     IF(K.LE.INT(PIKUP(2,J-1,I))) THEN
                        PIKUP(2,J-1,I)=PIKUP(2,J-1,I)+1.0D0
                     ELSE
!       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
!       THE PIKUP
                     END IF
                     IF(K.LE.INT(PIKUP(3,J-1,I))) THEN
                        PIKUP(3,J-1,I)=PIKUP(3,J-1,I)+1.0D0
                     ELSE
!       SURFACE BEING INSERTED AFTER THE SURFACE REFERENCED BY
!       THE PIKUP
                     END IF
                  END IF

               END IF
            END DO
         END DO
!
!     K IS THE SURFACE BEING INSERTED
         DO J=(L-1),K,-1
            PIKUP(1:6,J+1,1:PSIZ)=PIKUP(1:6,J,1:PSIZ)
         END DO
!
         SURF=INT(W1)
         CALL FLUSHNEXT
         IF(VBCNT.NE.0) THEN
            DO III=1,VBCNT
               IF(VARABL(III,3).GE.W1) VARABL(III,3)=VARABL(III,3)+1.0D0
            END DO
         END IF
         IF(TVBCNT.NE.0) THEN
            DO III=1,TVBCNT
               IV=III+MAXCMP
               IF(VARABL(IV,3).GE.W1) VARABL(IV,3)=VARABL(IV,3)+1.0D0
            END DO
         END IF
         IF(CMPCNT.NE.0) THEN
            DO III=1,CMPCNT
               IF(VARABL(III,3).GE.W1) VARABL(III,3)=VARABL(III,3)+1.0D0
            END DO
         END IF
!     HERE IS WHERE WE CALL CFGFX1.FOR TO FIX POSSIBLE
!     CONFIGS DATA IMPACTED BY THE INSERTION
         CALL CFGFX1(INT(W1))
!
      END IF
!
!       PRINT INSERTION MESSAGE
      WRITE(OUTLYNE,*)'SURFACE',SURF,' :SURFACE INSERTED'
      CALL SHOWIT(1)
      F22=1
!       NOW SURFACE NUMBER SURF HAS BEEN INSERTED
      DO I=0,INT(sys_last_surf())
         IF(surf_tilt_flag(I) == 6.OR.surf_tilt_flag(I) == 1.AND.surf_tilt_return_flag(I).EQ.1) THEN
!       FOUND A TILT RET
            IF((surf_ret_surf_num(I)).GE.SURF) call set_surf_ret_surf_num(I, surf_ret_surf_num(I) + 1)
         END IF
      END DO
   END DO

   RETURN
END
! SUB SDEL.FOR
SUBROUTINE SDEL
!
   use DATSUB
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use mod_system, only: sys_astop, sys_last_surf, sys_ref_surf, &
      & sys_set_astop, sys_set_astop_adj, sys_set_last_surf, sys_set_ref_surf
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SDEL WHICH IMPLEMENTS THE DEL
!       COMMANDS AT THE UPDATE LENS LEVEL.
!       THE FIRST NUMERIC WORD IS THE SURFACE WHICH WE WISH TO
!       DELETE FROM THE LENS. VALID VALUES ARE FROM 1 TO THE INTEGER VALUE
!       STORED IN (SYSTEM(20) MINUS 1).
!
!       IT IS NOT PERMITTED TO DELETE THE OBJECT OR IMAGE SURFACES.
!
!        FOR EXAMPLE, IF THE COMMAND
!
!                       DEL,15 IS ISSUED AND SYSTEM(20)=25
!       THEN SURFACE 15 IS DELETE. OLD SURFACE 16 BECOMES NEW 15
!       AND SO FORTH AND SYSTEM(20) IS DECREMENTED BY 1.0D0. THE CURRENT
!       SURFACE BECOMES THE NEW SURFACE 15.
!       ISSUING DEL WITH NO NUMERIC WORD CAUSES THE CURRENT SURFACE
!       TO BE DELETED IF THE CURRENT SURFACE IS NOT THE OBJECT OR
!       IMMAGE SURFACE. IF THE CURRENT SURFACE IS THE OBJECT SURFACE,
!       AN MESSAGE IS PRINTED STATING THAT THE OBJECT SURFACE MAY NOT
!       BE DELETED. IF THE CURRENT SURFACE IS THE IMAGE SURFACE THEN
!       THE CURRENT SURFACE IS DECREMENTED BY ONE AND THEN THE DELETION
!       IS PERFORMED. THIS MAY BE REPEATED UNTIL ONLY THE OBJECT AND
!       IMAGE SURFACES REMAIN. SOLVE DATA ALSO FIXED.
!
   INTEGER PIKCNT,J,I,K,III,II,SNUMBER,IIII,IV,JJJJ,V
!
!
!               CHECK FOR PRESENCE OF QUALIFIER OR STRING INPUT
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 .OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DEL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
!
!               WE ARE AT LENS UPDATE LEVEL
!
!       IS DEFAULT SURFACE ENTRY IN EFFECT?
!
   IF(DF1.EQ.1) THEN
      W1=DBLE(SURF)
      DF1=0
   ELSE
   END IF
   IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
!
   IF(DF2.EQ.1) THEN
      W2=W1
      DF2=0
   ELSE
   END IF
!
   IF(W2.LT.W1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'STARTING SURFACE # MAY NOT BE GREATER THAN ENDING SURFACE #'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(W1.LE.0.0D0.OR.W1.GE.sys_last_surf().OR.W2.LE.0.0D0.OR.W2.GE.sys_last_surf()) THEN
!
!       TRYING TO DELETE A SURFACE NUMBER LESS THAN OR EQUAL TO THE
!       OBJECT SURFACE OR GREATER THAN OR EQUAL TO THE IMAGE SURFACE
!       IS NOT ALLOWED.
!       PRINT ERROR AND RETURN.
!
      IF(W1.EQ.0.0.OR.W2.EQ.0.0D0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'OBJECT SURFACE DELETION NOT ALLOWED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(W1.EQ.sys_last_surf().OR.W2.EQ.sys_last_surf()) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'IMAGE SURFACE DELETION NOT ALLOWED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
   END IF
!
   SNUMBER=INT(W1)
   SURF=INT(W1)
   DO II=1,INT(W2)-INT(W1)+1
!
!       DELETION MESSAGE
      IF(VBCNT.NE.0) THEN
         DO III=1,VBCNT
            IF(INT(VARABL(III,3)).EQ.SNUMBER) THEN
!     DELETE AN ENTRY
               DO IV=III,VBCNT-1
                  VARABL(IV,1:17)=VARABL(IV+1,1:17)
               END DO
               VBCNT=VBCNT-1
               GO TO 20
            END IF
            IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
!     MOVE ALL SURF NUMBERS UP
               DO IIII=1,VBCNT
                  VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
               END DO
            END IF
20          CONTINUE
         END DO
      END IF
      IF(TVBCNT.NE.0) THEN
         DO III=1,TVBCNT
            V=III+MAXCMP
            IF(INT(VARABL(V,3)).EQ.SNUMBER) THEN
!     DELETE AN ENTRY
               DO IV=III,TVBCNT-1
                  V=IV+MAXCMP
                  VARABL(IV,1:17)=VARABL(IV+1,1:17)
               END DO
               TVBCNT=TVBCNT-1
               GO TO 30
            END IF
            IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
!     MOVE ALL SURF NUMBERS UP
               DO IIII=1,TVBCNT
                  VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
               END DO
            END IF
30          CONTINUE
         END DO
      END IF
      IF(CMPCNT.NE.0) THEN
         DO III=1,CMPCNT
            IF(INT(VARABL(III,3)).EQ.SNUMBER) THEN
!     DELETE AN ENTRY
               DO IV=III,CMPCNT-1
                  VARABL(IV,1:17)=VARABL(IV+1,1:17)
               END DO
               CMPCNT=CMPCNT-1
               GO TO 40
            END IF
            IF(INT(VARABL(III,3)).GT.SNUMBER) THEN
!     MOVE ALL SURF NUMBERS UP
               DO IIII=1,CMPCNT
                  VARABL(IIII,3)=VARABL(IIII,3)-1.0D0
               END DO
            END IF
40          CONTINUE
         END DO
      END IF
      WRITE(OUTLYNE,*)'DELETING SURFACE ',SNUMBER
      CALL SHOWIT(1)
!
!       IS THE SURFACE THE REFERENCE SURFACE ?
      IF(SURF.EQ.INT(sys_ref_surf())) THEN
         OUTLYNE='CURRENT REFERENCE SURFACE BEING DELETED'
         CALL SHOWIT(1)
         OUTLYNE='REFERENCE SURFACE SHIFTED TO SURFACE 1'
         CALL SHOWIT(1)
         call sys_set_ref_surf(1.0D0)
      END IF
!       IS SURFACE DELETED, INFRONT OF REFERENCE SURFACE ?
      IF(SURF.LT.INT(sys_ref_surf())) THEN
         call sys_set_ref_surf(sys_ref_surf()-1.0D0)
         NEWREF=INT(sys_ref_surf())
      ELSE
!       NOT IN FRONT OF REF SURF
      END IF
!       IS THE SURFACE THE ASTOP SURFACE ?
      IF(SURF.EQ.INT(sys_astop())) THEN
         OUTLYNE='CURRENT APERTURE STOP SURFACE BEING DELETED'
         CALL SHOWIT(1)
         call sys_set_astop(-99.0D0)
         call sys_set_astop_adj(0.0D0)
      END IF
!       IS SURFACE DELETED, INFRONT OF ASTOP SURFACE ?
      IF(SURF.LT.INT(sys_astop()).AND.sys_astop().NE.-99.0D0) THEN
         call sys_set_astop(sys_astop()-1.0D0)
      ELSE
!       NOT IN FRONT OF ASTOP SURF OR NO ASTOP DEFINED.
      END IF
!
!       DELETE SURFACE
!       REPRESENTED BY SURF,SURF EITHER INPUT ON COMMAND LINE
!       OR SET BY ABOVE DEFAULT PROCEEDURE.
!
!       BEFORE DELETING THE SURFACE,CHECK TO SEE IF THERE ARE
!       ANY SOLVES OR PIKUPS ON THAT SURFACE AND IF THERE
!       ARE ANY, PRINT A MESSAGE THAT EITHER SOLVES OR PIKUPS
!       ARE BEING DELETED WITH THE SURFACE DELETION.
!
!       FIRST SOLVES
!
      IF(surf_pickup_count(SURF) /= 0) THEN
         OUTLYNE='PIKUP DATA BEING DELETED WITH SURFACE DELETION'
         CALL SHOWIT(1)
      END IF
!
!       THEN PIKUPS
!
      IF(surf_solve_flag(SURF) /= 0) THEN
         OUTLYNE='SOLVE DATA BEING DELETED WITH SURFACE DELETION'
         CALL SHOWIT(1)
      END IF
!
!       THEN SPECIAL SURFACE DATA
!
!       NOW WHAT IF THE SURFACE TO BE DELETED IS THE SURFACE
!       FROM WHICH SOMETHING IS BEING PIKED UP FROM. IN THAT CASE
!       THE PIKUP SHOULD BE DELETED WITH THE ASSOCIATED PARAMETER
!       LEFT AT ITS CURRENT VALUE.
!
!       THE ENTIRE LENS MUST BE CHECKED TO SEE IF A PIKUP
!       REFERS TO THE SURFACE TO BE DELETED. THAT PIKUP MUST BE
!       DELETED. IF THAT WAS THE LAST PIKUP ON THAT SURFACE THEN
!       surf_pickup_count(FOR THE SURFACE WITH PIKUPS) IS SET TO ZERO
!
      DO J=0,INT(sys_last_surf())
         IF(surf_pickup_count(J).NE.0D0) THEN
!       FOUND A SURFACE WITH PIKUPS
            DO I=1,PSIZ
               IF(PIKUP(1,J,I).EQ.1) THEN
                  IF(INT(PIKUP(2,J,I)).EQ.SURF) THEN
!       FOUND A PIKUP WHICH REFERENCES A SURFACE TO BE DELETED
!       DELETE THE PIKUP
!       AND PRINT MESSAGE
!
                     PIKUP(1:6,J,I)=0.0D0
                     OUTLYNE='PIKUP REFERENCED BY DELETED SURFACE HAS BEEN DELETED'
                     CALL SHOWIT(1)
                     OUTLYNE='ASSOCIATED PARAMETER FROZEN AT CURRENT VALUE'
                     CALL SHOWIT(1)
                  END IF
               END IF
            END DO
         END IF
!       CHECK THE NEXT SURFACE
      END DO
!
!               NOW FIX THE STATUS OF surf_pickup_count(K) FOR ALL
!       SURFACES K=0 TO INT(SYSTEM(20))
!
      DO 405 K=0,INT(sys_last_surf())
         IF(surf_pickup_count(K) /= 0) THEN
!       THE MIGHT BE PIKUPS
!       SHOULD THE STATUS BE CHANGED
            PIKCNT=0
            DO 406 I=1,PSIZ
               IF(PIKUP(1,K,I).NE.0.0D0) THEN
                  PIKCNT=PIKCNT+1
               ELSE
!       PROCEED
               END IF
406         CONTINUE
!       IF NO PIKUPS FOUND, CHANGE STATUS
            IF(PIKCNT.EQ.0) call set_surf_pickup_count(K, 0)
!       PROCEED TO CHECK THE NEXT SURFACE FOR PIKUPS TO BE
!       STATUS RESOLVED
         ELSE
         END IF
405   CONTINUE
!
      IF(surf_special_type(SURF) /= 0) THEN
         OUTLYNE='SPECIAL SURFACE DATA BEING DELETED WITH SURFACE DELETION'
         CALL SHOWIT(1)
      END IF
!
      K=INT(sys_last_surf())-1
      DO J=SURF,K
         ALENS(1:LSIZ,J)=ALENS(1:LSIZ,(J+1))
      END DO
!
      DO I=SURF,K
         LBL(I)(1:80)=LBL(I+1)(1:80)
      END DO
!
      DO J=SURF,K
         FTFL01(1:96,J)=FTFL01(1:96,(J+1))
      END DO
      DO J=SURF,K
         PRINT *, "GLANAM(J,1) is ", GLANAM(J,1)
         PRINT *, "GLANAM(J,2) is ", GLANAM(J,2)
         GLANAM(J,1)=GLANAM((J+1),1)
         GLANAM(J,2)=GLANAM((J+1),2)
         SOLVE(0:9,J) = SOLVE(0:9,(J+1))
      END DO
!     THE FIRST SURFACE IN THE LOOP IS THE SURFACE 0
!     SNUMBER IS THE CURRENT SURFACE
      DO J=0,K
         DO I=1,PSIZ
            IF(I.NE.32) THEN
!     NOT THOAL
!       IF THE SURFACE NUMBER OF THE SURFACE BEING DELETED IS
!       NUMERICALLY LESS THAN OR EQUAL TO SURFACE NUMBER REFERENCED
!       BY THE PIKUP (PIKUP(2,J+1,I)
               IF(SURF.LE.INT(PIKUP(2,J+1,I)))PIKUP(2,J+1,I)=PIKUP(2,J+1,I)-1.0D0
            ELSE
!     THOAL
               IF(SURF.LE.INT(PIKUP(2,J+1,I)))PIKUP(2,J+1,I)=PIKUP(2,J+1,I)-1.0D0
               IF(SURF.LE.INT(PIKUP(3,J+1,I)))PIKUP(3,J+1,I)=PIKUP(3,J+1,I)-1.0D0
            END IF
         END DO
      END DO
      DO J=SURF,K
         PIKUP(1:6,J,1:PSIZ)=PIKUP(1:6,J+1,1:PSIZ)
      END DO
!
!       DECREMENT THE IMAGE SURFACE NUMBER COUNTER SYSTEM(20) BY 1.0D0
      IF(sys_last_surf().GT.0.0D0) THEN
         call sys_set_last_surf((sys_last_surf()-1.0D0))
      ELSE
         call sys_set_last_surf(0.0D0)
      END IF
!
!       IF THE CURRENT SURFACE IS THE IMAGE SURFACE,
!       PRINT WARNING MESSAGE
!
      IF(SURF.EQ.INT(sys_last_surf()))THEN
         OUTLYNE='CURRENT SURFACE IS NOW THE IMAGE SURFACE'
         CALL SHOWIT(1)
      END IF
      F22=1
!     HERE IS WHERE WE CALL CFGFX2.FOR  AND CFGFX3.FOR TO FIX POSSIBLE
!     CONFIGS DATA IMPACTED BY THE DELETION
      CALL CFGFX2(SURF)
      CALL CFGFX3(SURF)
!     SURF WAS DELETED
      DO I=0,INT(sys_last_surf())
         IF(surf_tilt_flag(I) == 6.OR.surf_tilt_flag(I) == 1.AND.surf_tilt_return_flag(I).EQ.1) THEN
!       FOUND A TILT RET
            IF((surf_ret_surf_num(I)).GE.SURF) call set_surf_ret_surf_num(I, surf_ret_surf_num(I) - 1)
         END IF
      END DO
   END DO
   RETURN
END
! SUB SDEC.FOR
SUBROUTINE SDEC
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_system, only: sys_last_surf
   use mod_surface
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE SDEC WHICH IMPLEMENTS THE DEC
!       COMMAND AT THE LENS INPUT LEVEL AND LENS UPDATE LEVEL.
!       THE DEC COMMAND IS:
!
!               DEC, YD XD ZD
!
   INTEGER PIKCNT,I
!
!
!               CHECK FOR ADDITIONAL INPUT AND
!               PRINT ERROR AND RETURN IF DISCOVERED.
!
   IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DEC" TAKES NO QUALIFIER OR STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(S4.EQ.1.OR.S5.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DEC" TAKES NO NUMERIC WORD #4 OR #5 INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DEC" REQUIRES SOME EXPLICIT NUMERIC INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!
   IF(F5.EQ.1.OR.F6.EQ.1) THEN
      IF(SURF.EQ.0) THEN
         CALL REPORT_ERROR_AND_FAIL(&
         & 'OBJECT SURFACE MAY NOT BE TILTED OR DECENTERED'//'\n'//&
         & 'RE-ENTER COMMAND', 1)
         RETURN
      END IF
!
!       WE ARE AT LENS INPUT LEVEL OR LENS UPDATE LEVEL
!
!               PROCEED
!
!       DON'T SET DEC FLAG (surf_decenter_flag(SURF) IF YD AND XD ARE BOTH 0.0D0
!       BUT CLEAR IT.
!
!       CHECK FOR PIKUP YD OR PIKUP XD OR ZD AND IF FOUND REMOVE
!
      IF(PIKUP(1,SURF,13).EQ.1.0D0) THEN
!       DELETE PIKUP YD
         PIKUP(1:6,SURF,13)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (YD) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,33).EQ.1.0D0) THEN
!       DELETE PIKUP YD
         PIKUP(1:6,SURF,33)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (ZD) DELETED'
         CALL SHOWIT(1)
      END IF
      IF(PIKUP(1,SURF,14).EQ.1.0D0) THEN
!       DELETE PIKUP XD
         PIKUP(1:6,SURF,14)=0.0D0
         WRITE(OUTLYNE,*)'SURFACE',SURF,' :PIKUP (XD) DELETED'
         CALL SHOWIT(1)
      END IF
!
!       SET surf_pickup_count(SURF) IF NO OTHER PIKUPS EXIST
      PIKCNT=0
      DO 10 I=1,PSIZ
         IF(PIKUP(1,SURF,I).NE.0.0D0) THEN
            PIKCNT=PIKCNT+1
         ELSE
         END IF
10    CONTINUE
      IF(PIKCNT.EQ.0) call set_surf_pickup_count(SURF, 0)
!
!       PROCEED WITH DEC ASSIGNMENT
!
      IF(W1.EQ.0.0D0.AND.W2.EQ.0.0D0.AND.W3.EQ.0.0D0) THEN
         IF(DF1.EQ.1) W1=0.0D0
         IF(DF2.EQ.1) W2=0.0D0
         IF(DF3.EQ.1) W3=0.0D0
         call set_surf_decenter_flag(SURF, 0)
         call set_surf_decenter_y(SURF, W1)
         call set_surf_decenter_x(SURF, W2)
         call set_surf_decenter_z(SURF, W3)
         call set_surf_focus_dy(SURF, W1)
         call set_surf_focus_dx(SURF, W2)
         call set_surf_focus_dz(SURF, W3)
      ELSE
!       NOT BOTH W1 AND W2 ARE ZERO, PROCEED
!       SET DEC FLAG AND VALUES.
         IF(DF1.EQ.1) W1=surf_decenter_y(SURF)
         IF(DF2.EQ.1) W2=surf_decenter_x(SURF)
         IF(DF3.EQ.1) W3=surf_decenter_z(SURF)
         call set_surf_decenter_flag(SURF, 1)
         call set_surf_decenter_y(SURF, W1)
         call set_surf_decenter_x(SURF, W2)
         call set_surf_decenter_z(SURF, W3)
         call set_surf_focus_dy(SURF, W1)
         call set_surf_focus_dx(SURF, W2)
         call set_surf_focus_dz(SURF, W3)
      END IF
!
!       NOW WHAT IF THIS CURRENT SURFACE IS BEING PIKUP UP WITH A
!       PIKUP YD OR PIKUP XD FROM ANOTHER SURFACE. IF IT IS THEN
!       IF THE CURRENT WORK HERE CHANGED THE STATUS OF surf_decenter_flag(SURF)
!       IT WILL CHANGE THE STATUS ON THE PIKING SURFACE.
!       RESOLVE THIS ISSUE.
!
      DO 100 I=0,INT(sys_last_surf())
         IF(PIKUP(1,I,13).EQ.1.0D0.OR.PIKUP(1,I,14).EQ.1.0D0.OR.PIKUP(1,I,33).EQ.1.0D0.OR.PIKUP(1,I,13).EQ.1.0D0.AND.PIKUP(1,I,14).EQ.1.0D0 .AND.PIKUP(1,I,33).EQ.1.0D0) THEN
            IF(INT(PIKUP(2,I,13)).EQ.SURF.OR.INT(PIKUP(2,I,14)).EQ.SURF.OR.INT(PIKUP(2,I,33)).EQ.SURF) THEN
!       UPDATE THE ALENS(29) ON THE PIKING SURFACE
               call set_surf_decenter_flag(I, surf_decenter_flag(SURF))
            ELSE
!       DONT DO ANYTHING
            END IF
         ELSE
!       NOT RIGHT KIND OF PIKUP,DO NOTING,PROCEED TO NEXT SURFACE
         END IF
100   CONTINUE
!
!       IF THE SURFACE HAS A TILT AUTO THEN REMOVE THE
!       AUTO AND PRINT MESSAGE
!
      IF(surf_tilt_flag(SURF) == 2.OR.surf_tilt_flag(SURF).EQ.3.0D0) THEN
         call set_surf_tilt_flag(SURF, 1)
         IF(surf_tilt_flag(SURF) == 2) OUTLYNE='"AUTO" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
         IF(surf_tilt_flag(SURF) == 3) OUTLYNE='"AUTOM" ADJUST REMOVED FROM SURFACE TILT DEFINITION'
         CALL SHOWIT(1)
      END IF
   END IF
   RETURN
END
