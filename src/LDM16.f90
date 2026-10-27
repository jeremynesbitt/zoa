! SUB OOPDIF.FOR
SUBROUTINE OOPDIF
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DOES THE OOPDIF COMMAND
!
!
   IF(is_command_query()) THEN
      IF(OPDIF) WRITE(OUTLYNE,100)
      IF(.NOT.OPDIF) WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
100   FORMAT(&
      &'DIFFERNTIAL RAY TRACING "ON" IN PRE-DEFINED OPERANDS')
200   FORMAT(&
      &'DIFFERNTIAL RAY TRACING "OFF" IN PRE-DEFINED OPERANDS')
      RETURN
   END IF
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"OPDIF" TAKES NO STRING OR NUMERICAL INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!       QUALIFIERS
   IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'//'\n'//&
      & 'USED WITH "OPDIF"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"OPDIF" REQUIRES EXPLICIT QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'ON') OPDIF=.TRUE.
   IF(WQ.EQ.'OFF') OPDIF=.FALSE.
   RETURN
END
! SUB LLDDFF.FOR
SUBROUTINE LLDDFF
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DOES THE LDIF SETTING VIA THE DIFRAY COMMAND
!
!
   IF(is_command_query()) THEN
      IF(LDIF) WRITE(OUTLYNE,100)
      IF(.NOT.LDIF) WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
100   FORMAT(&
      &'DIFFERNTIAL RAY TRACING IS CURRENTLY ACTIVE')
200   FORMAT(&
      &'DIFFERNTIAL RAY TRACING IS CURRENTLY "NOT" ACTIVE')
      RETURN
   END IF
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFRAY" TAKES NO STRING OR NUMERICAL INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!       QUALIFIERS
   IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'//'\n'//&
      & 'USED WITH "DIFRAY"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFRAY" REQUIRES EXPLICIT QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'ON') LDIF=.TRUE.
   IF(WQ.EQ.'OFF') LDIF=.FALSE.
   RETURN
END
! SUB LLDDF2.FOR
SUBROUTINE LLDDF2
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DOES THE LDIF2 SETTING VIA THE DIFFOB COMMAND
!
!
   IF(is_command_query()) THEN
      IF(LDIF2) WRITE(OUTLYNE,100)
      IF(.NOT.LDIF2) WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
100   FORMAT(&
      &'DIFFERNTIAL REFERENCE RAY TRACING IS CURRENTLY ACTIVE')
200   FORMAT(&
      &'DIFFERNTIAL REFERENCE RAY TRACING IS CURRENTLY "NOT" ACTIVE')
      RETURN
   END IF
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFFOB" TAKES NO STRING OR NUMERICAL INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!       QUALIFIERS
   IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'//'\n'//&
      & 'USED WITH "DIFFOB"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"DIFFOB" REQUIRES EXPLICIT QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'ON') LDIF2=.TRUE.
   IF(WQ.EQ.'OFF') LDIF2=.FALSE.
   RETURN
END
! SUB LLDDF3.FOR
SUBROUTINE LLDDF3
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS ROUTINE DOES THE SETTING OR NOVIRT VIA THE VIRTRAY COMMAND
!
!
   IF(is_command_query()) THEN
      IF(NOVIRT) WRITE(OUTLYNE,100)
      IF(.NOT.NOVIRT) WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
100   FORMAT(&
      &'VIRTUAL RAYS WILL NOT BE PLOTTED (PROGRAM DEFAULT)')
200   FORMAT(&
      &'VIRTUAL RAYS WILL BE PLOTTED')
      RETURN
   END IF
!       CHECK FOR STRING INPUT
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"VIRTRAY" TAKES NO STRING OR NUMERICAL INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
!       QUALIFIERS
   IF(SQ.EQ.1.AND.WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ON" AND "OFF" ARE THE ONLY VALID QUALIFIERS'//'\n'//&
      & 'USED WITH "VIRTRAY"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"VIRTRAY" REQUIRES EXPLICIT QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'ON') NOVIRT=.FALSE.
   IF(WQ.EQ.'OFF') NOVIRT=.TRUE.
   RETURN
END
! SUB LEPRT.FOR
SUBROUTINE LEPRT
!
   use DATSUB
   use DATLEN
   use mod_system, only: sys_last_surf
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE LEPRT. THIS SUBROUTINE IMPLEMENTS
!       THE CMD COMMAND "LEPRT" OR "LIS"
!
   INTEGER IW3
!
   LOGICAL OLDHEAD
!
   CHARACTER IAW3*3
!
!
   IF(S2.EQ.1.OR.SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.&
   &S4.EQ.1.OR.S5.EQ.1) THEN
      IF(WC.EQ.'LEPRT') WRITE(OUTLYNE,*)&
      &'"LEPRT" TAKES NO QUALIFIER, STRING OR NUMERIC WORD'
      IF(WC.EQ.'LIS') WRITE(OUTLYNE,*)&
      &'"LIS" TAKES NO QUALIFIER, STRING OR NUMERIC WORD'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'"#2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(is_command_query()) THEN
      WRITE(OUTLYNE,*)&
      &'"LEPRT/LIS" LISTS ALL LENS DATA'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.0) THEN
      IF(W1.LT.0.0D0.OR.W1.GT.sys_last_surf()) THEN
         WRITE(OUTLYNE,*)&
         &'"SURFACE NUMBER BEYOND LEGAL BOUNDS"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*) 'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!
   OLDHEAD=HEADIN
!
   IF(DF1.EQ.0) THEN
      IW3=INT(W1)
      CALL ITOAA(IW3,IAW3)
      IF(.NOT.HEADIN) HEADIN=.TRUE.
   END IF
!
   IF(DF1.EQ.1) THEN
!
!     OUTPUT NON-SURFACE STUFF
!
!     PROCEED
      WRITE(OUTLYNE,10)
      CALL SHOWIT(0)
10    FORMAT('CURRENT LENS DATA LISTING')
      INPUT='LI'
      CALL PROCES
      INPUT='LIC'
      CALL PROCES
      INPUT='INI'
      CALL PROCES
      INPUT='LTYPE'
      CALL PROCES
      INPUT='UNITS ?'
      CALL PROCES
      INPUT='REFK'
      CALL PROCES
      INPUT='FLDSARE'
      CALL PROCES
      INPUT='OCDY'
      CALL PROCES
      INPUT='OCDX'
      CALL PROCES
      INPUT='SAY'
      CALL PROCES
      INPUT='SAX'
      CALL PROCES
      INPUT='AUTOFUNC'
      CALL PROCES
      INPUT='SPTWT'
      CALL PROCES
      INPUT='MODE'
      CALL PROCES
      INPUT='ASTOP'
      CALL PROCES
      INPUT='SPC'
      CALL PROCES
      INPUT='PIVAXIS'
      CALL PROCES
   END IF
!
   IF(DF1.EQ.1) INPUT='RTGLBL ALL'
   IF(DF1.EQ.0) INPUT='RTGLBL '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='ASPH ALL'
   IF(DF1.EQ.0) INPUT='ASPH '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='TASPH ALL'
   IF(DF1.EQ.0) INPUT='TASPH '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='DEFORM ALL'
   IF(DF1.EQ.0) INPUT='DEFORM '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='TAD ALL'
   IF(DF1.EQ.0) INPUT='TAD '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='TR ALL'
   IF(DF1.EQ.0) INPUT='TR '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='CAOB ALL'
   IF(DF1.EQ.0) INPUT='CAOB '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='RIN ALL'
   IF(DF1.EQ.0) INPUT='RIN '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='RIN2 ALL'
   IF(DF1.EQ.0) INPUT='RIN2 '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='SPGR ALL'
   IF(DF1.EQ.0) INPUT='SPGR '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='GRT ALL'
   IF(DF1.EQ.0) INPUT='GRT '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='SLV ALL'
   IF(DF1.EQ.0) INPUT='SLV '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='PIKK ALL'
   IF(DF1.EQ.0) INPUT='PIKK '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='SURTYPE ALL'
   IF(DF1.EQ.0) INPUT='SURTYPE '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='FOOTBLOK ALL'
   IF(DF1.EQ.0) INPUT='FOOTBLOK '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='PIVAXIS ALL'
   IF(DF1.EQ.0) INPUT='PIVAXIS '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='DUMOUT ALL'
   IF(DF1.EQ.0) INPUT='DUMOUT '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='INR ALL'
   IF(DF1.EQ.0) INPUT='INR '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='THM ALL'
   IF(DF1.EQ.0) INPUT='THM '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='PRICE ALL'
   IF(DF1.EQ.0) INPUT='PRICE '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='COATING ALL'
   IF(DF1.EQ.0) INPUT='COATING '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) INPUT='PRSPR ALL'
   IF(DF1.EQ.0) INPUT='PRSPR '//IAW3
   CALL PROCES
   IF(DF1.EQ.1) THEN
      INPUT='CF'
      CALL PROCES
      INPUT='CFG'
      CALL PROCES
   END IF
   HEADIN=OLDHEAD
   RETURN
END
! SUB LENUP.FOR
SUBROUTINE LENUP
   use zoa_output, only: zoa_emit
   use type_utils, only: real2str
!
   use DATLEN
   use mod_system, only: sys_last_surf, sys_set_telecentric, sys_telecentric
   use DATMAI
   use mod_surface
   use command_utils, only: is_command_query
   use mod_parsed_command, only: parsed_command, capture_command
   use iso_fortran_env, only: real64
   use glass_manager, only: gdb
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE LENUP. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE CHANGES AN EXISTING LENS.
!
   real(real64) RAL,RBE,CGAM,SGAM,RGAM
!
   !PRINT *, "IN LENUP ROUTINE"
   !call updateTerminalLog(INPUT, "red")

   IF(WC.EQ.'M'.OR.WC.EQ.'CK') THEN
      CALL MESCOM
      RETURN
   END IF
!       HERE IS WERE THE ? FOLLOWING A LENS ULPATE COMMAND
!       IS HANDLED
   IF(is_command_query()) THEN
      CALL ULQUER
      RETURN
   END IF
!       CERTAIN COMMANDS ARE NOT ALLOWED IF SURF=SYSTEM(20)
!       THEY ARE TH,PY,PCY,PX,PCX,CAY,CAX,PIKUP TH,
   IF(SURF.EQ.INT(sys_last_surf())) THEN
      IF(WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.&
      &WC.EQ.'PX'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'&
      &.OR.WC.EQ.'PIKUP'.AND.WQ.EQ.'TH') THEN
         !     IF(WC.EQ.'TH'.OR.WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.
         !  1  WC.EQ.'PX'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'
         !  2  .OR.WC.EQ.'PIKUP'.AND.WQ.EQ.'TH') THEN
!       THESE ARE NOT VALID AT THE IMAGE SURFACE
!
         IF(WC.EQ.'TH') THEN
            OUTLYNE=&
            &'THE "TH" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'PY') THEN
            OUTLYNE=&
            &'THE "PY" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'PCY') THEN
            OUTLYNE=&
            &'THE "PCY" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'CAY') THEN
            OUTLYNE=&
            &'THE "CAY" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'PX') THEN
            OUTLYNE=&
            &'THE "PX" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'PCX') THEN
            OUTLYNE=&
            &'THE "PCX" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'CAX') THEN
            OUTLYNE=&
            &'THE "CAX" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
!
         IF(WC.EQ.'PIKUP'.AND.WQ.EQ.'TH') THEN
            OUTLYNE=&
            &'THE "PIKUP TH" COMMAND IS NOT VALID AT THE IMAGE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('IN THE UPDATE LENS MODE'//'\n'//'NO ACTION TAKEN', 1)
            RETURN
         END IF
!
         RETURN
!       NOT A COMMAND THAT WAS DISALLOWED
      END IF
!       NOT IMAGE SURF, PROCEED
   END IF
!
!       FIRST HANDLE EOS (END OF SUBFILE) IN THIS CASE THE LENS
!       SUBFILE.
!
   IF(WC.EQ.'EOS') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
         CALL REPORT_ERROR_AND_FAIL('"EOS" TAKES NO EXPLICIT INPUT'//'\n'//'RE-ENTER COMMAND', 1)
         RETURN
      END IF
      LNSTYP=1
      CALL LNSEOS
      RETURN
   ELSE
!       DON'T TERMINATE THE INPUT ROUTINE. ACCEPT INPUT DATA STARTING
!       AT THE CURRENT SURFACE OR ACCEPT SYSTEM AND TILE DATA OR CHANGE
!       CURRENT SURFACE. DEFAULT CURRENT SURFACE IS SURF=0
!       HERE IS WHERE THE BRANCHING TO SUBROUTINES WHICH HANDLE
!       CHANGES TO THE LENS MATRICES ARE HANDELED.
!
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA ITEMS.
!
!       COMMAND (LI) OR (LIC) AT 'U L' LEVEL
      IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
         CALL SLI
         RETURN
      END IF
!       COMMAND (INI) AT 'U L' LEVEL
      IF(WC.EQ.'INI') THEN
         CALL SINI
         RETURN
      END IF
!       COMMAND (LTYPE) AT 'U L' LEVEL
      IF(WC.EQ.'LTYPE') THEN
         CALL SLTYPE
         RETURN
      END IF
!       COMMAND (LABEL) OR (LBL) AT 'U L' LEVEL
      IF(WC.EQ.'LBL'.OR.WC.EQ.'LABEL') THEN
         CALL SLABEL
         RETURN
      END IF
!       COMMAND (WV,W1 W2 W3 W4 W5)
      IF(WC.EQ.'WV') THEN
         CALL SWV
         RETURN
      END IF
!       COMMAND (WV2,W1 W2 W3 W4 W5)
      IF(WC.EQ.'WV2') THEN
         CALL SWV2
         RETURN
      END IF
!       COMMAND (UNITS)
      IF(WC.EQ.'UNITS') THEN
         CALL SUNITS
         RETURN
      END IF
!       COMMAND (CW,PCW OR SCW)
      IF(WC.EQ.'CW'.OR.WC.EQ.'PCW'.OR.&
      &WC.EQ.'SCW') THEN
         CALL SCW
         RETURN
      END IF
!       COMMAND (SAY OR SAX)
      IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
         CALL SSA
         RETURN
      END IF
!       COMMAND (WRY OR WRX)
      IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX') THEN
         CALL SWR
         RETURN
      END IF
!       COMMAND (BDY OR BDX)
      IF(WC.EQ.'BDY'.OR.WC.EQ.'BDX') THEN
         CALL SBD
         RETURN
      END IF
!       COMMAND (NAOY OR NAOX)
      IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
         CALL SNAO
         RETURN
      END IF
!       COMMAND (FNOY OR FNOX)
      IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
         CALL SFNO
         RETURN
      END IF
!       COMMAND (SCY,Y0 Y1 AND SCY FANG,U0 U1)
      IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
         CALL SSC
         RETURN
      END IF
!       COMMAND (PXIM AND PYIM)
      IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
         CALL PXYIM
         RETURN
      END IF
!       COMMAND (RXIM AND RYIM)
      IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
         CALL RXYIM
         RETURN
      END IF
!       COMMAND (RD) OR (CV)
      IF(WC.EQ.'RD'.OR.WC.EQ.'CV') THEN
         CALL SCV
         RETURN
      END IF
!       COMMAND (INR)
      IF(WC.EQ.'INR') THEN
         CALL INRINR
         RETURN
      END IF
!       COMMAND (MFG)
      IF(WC.EQ.'MFG') THEN
         MFG=WS
         RETURN
      END IF
!       COMMAND (CATNUM)
      IF(WC.EQ.'CATNUM') THEN
         CATNUM=WS
         RETURN
      END IF
!       COMMAND (INRD)
      IF(WC.EQ.'INRD') THEN
         CALL INRINRD
         RETURN
      END IF
!       COMMAND (ZERO)
      IF(WC.EQ.'ZERO') THEN
         CALL ZERO
         RETURN
      END IF
!       COMMAND (TH)
      IF(WC.EQ.'TH') THEN
         CALL STH
         IF(sys_telecentric().NE.0.0D0.AND.DABS(surf_thickness(NEWOBJ)).GE.1.0D10) THEN
            call sys_set_telecentric(0.0D0)
            OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
            CALL SHOWIT(1)
            OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
            CALL SHOWIT(1)
         END IF
         RETURN
      END IF
      IF(WC.EQ.'THM') THEN
         CALL STHM
         RETURN
      END IF
      IF(WC.EQ.'PRICE') THEN
         CALL SPRICE
         RETURN
      END IF
      IF(WC.EQ.'AUTOFUNC') THEN
         CALL SAUTOFUNC
         RETURN
      END IF
!       COMMAND (CC OR CCTOR)
      IF(WC.EQ.'CCK'.OR.WC.EQ.'CCTOR') THEN
         CALL SCC
         RETURN
      END IF
!
!       COMMAND (DEFORM)
      IF(WC.EQ.'DEFORM') THEN
         IF(surf_special_type(SURF) /= 0) THEN
            OUTLYNE=&
            &'A SPECIAL SURFACE MAY NOT BE DEFINED AS A DEFORMABLE SURFACE'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
         CALL DEFIT
         RETURN
      END IF
!
!       COMMAND (DELDEFOR)
      IF(WC.EQ.'DELDEFOR') THEN
         IF(surf_default_flag(SURF) == 0) THEN
            OUTLYNE=&
            &'NO DEFORMABLE SURFACE DEFINITION EXISTS TO BE DELETED'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
         CALL DELDEFIT
         RETURN
      END IF
!       COMMAND (AIR)
      IF(WC.EQ.'AIR') THEN
         CALL SAIR
         RETURN
      END IF
!       COMMAND (GRT,GRO,GRS,GRX,GRY,GRZ)
      IF(WC.EQ.'GRT'.OR.WC.EQ.'GRO'.OR.WC.EQ.'GRS'.OR.&
      &WC.EQ.'GRX'.OR.WC.EQ.'GRY'.OR.WC.EQ.'GRZ') THEN
         CALL SGRAT
         RETURN
      END IF
!       COMMAND (NODUM)
      IF(WC.EQ.'NODUM') THEN
         CALL SNODUM
         RETURN
      END IF
!       COMMAND (MODEL [INDENTIFICATION],N V])
      IF(WC.EQ.'MODEL') THEN
         IF(is_command_query()) THEN
            WRITE(OUTLYNE,100) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
            RETURN
         END IF
         CALL SGLASS
         RETURN
      END IF
!       COMMAND (GLASS [INDENTIFICATION],N1 N2 N3 N4 N5])
      IF(WC.EQ.'GLASS') THEN
         IF(is_command_query()) THEN
            WRITE(OUTLYNE,100) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
100         FORMAT('GLASS TYPE FOR SURFACE # ',I3,' IS: ',&
            &A8,1X,A8)
            RETURN
         END IF
         CALL SGLASS
         RETURN
      END IF
!       COMMAND ((GLASS CAT NAME) [INDENTIFICATION])
      IF(gdb%isNameInCatalog(trim(WC)).OR.&
      &WC.EQ.'MATL'.OR.WC.EQ.'GLAK'.OR.&
      &WC.EQ.'USER'.OR.WC.EQ.'RUSSIAN'.OR.&
      &WC.EQ.'GLCAT') THEN
         CALL GLSCAT
         RETURN
      END IF
!       COMMAND (MULTCLAP)
      IF(WC.EQ.'MULTCLAP') THEN
         CALL MULT_CLAP
         RETURN
      END IF
!       COMMAND (MULTCOBS)
      IF(WC.EQ.'MULTCOBS') THEN
         CALL MULT_COBS
         RETURN
      END IF
!       COMMAND (SPIDER)
      IF(WC.EQ.'SPIDER') THEN
         CALL SSPIDER
         RETURN
      END IF
!       COMMAND (N1,W1, TO N10,W1)
      IF(WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.&
      &WC.EQ.'N4'.OR.WC.EQ.'N5'.OR.WC.EQ.'N6'.OR.WC.EQ.'N7'&
      &.OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10') THEN
         CALL RNCHG
         RETURN
      END IF
!       COMMAND (INDEX OR VNUM OR DPART)
      IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
         CALL FICTCHG
         RETURN
      END IF
!       COMMAND (ASPH,D E F G AC)
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'ASPH2') THEN
         CALL SASPH
         RETURN
      END IF
!       COMMAND (ASPHD OR TASPHD)
      IF(WC.EQ.'ASPHD'.OR.WC.EQ.'TASPHD') THEN
         CALL SASPHD
         RETURN
      END IF
!       COMMAND (ARRAYD)
      IF(WC.EQ.'ARRAYD') THEN
         CALL SARRAYD
         RETURN
      END IF
!       COMMAND (ARRAY)
      IF(WC.EQ.'ARRAY') THEN
         CALL SARRAY(.FALSE.)
         RETURN
      END IF
!       COMMAND (TASPH,D E F G)
      IF(WC.EQ.'TASPH') THEN
         CALL STASPH
         RETURN
      END IF
!       COMMAND (REFS)
      IF(WC.EQ.'REFS') THEN
         CALL SREFS
         RETURN
      END IF
!       COMMAND (ASTOP)
      IF(WC.EQ.'ASTOP') THEN
         CALL SASTOP
         RETURN
      END IF
!       COMMAND (REFL)
      IF(WC.EQ.'REFL') THEN
         CALL SREFL
         RETURN
      END IF
!       COMMAND (REFLTIRO)
      IF(WC.EQ.'REFLTIRO') THEN
         CALL SREFL
         RETURN
      END IF
!       COMMAND (REFLTIR)
      IF(WC.EQ.'REFLTIR') THEN
         CALL SREFL
         RETURN
      END IF
!       COMMAND (PERFECT)
      IF(WC.EQ.'PERFECT') THEN
         CALL SPERFECT
         RETURN
      END IF
!       COMMAND (REAL OR PARAX)
      IF(WC.EQ.'REAL'.OR.WC.EQ.'PARAX') THEN
         CALL SURF_TYPE
         RETURN
      END IF
!       COMMAND (IDEAL)
      IF(WC.EQ.'IDEAL') THEN
         CALL SIDEAL
         RETURN
      END IF
!       COMMAND (CHG,W1)
      IF(WC.EQ.'CHG') THEN
         CALL SCHG
         RETURN
      END IF
!       COMMAND (INS,W1)
      IF(WC.EQ.'INSK') THEN
         IF(F12.NE.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"INS" ONLY WORKS IN CONFIGURATION #1 (MAIN CONFIG)'//'\n'//&
            & 'NO ACTION TAKEN', 1)
            RETURN
         END IF
         CALL SINS
         RETURN
      END IF
!       COMMAND (DEL,W1 W2)
      IF(WC.EQ.'DELK') THEN
         IF(F12.NE.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"DEL" ONLY WORKS IN CONFIGURATION #1 (MAIN CONFIG)'//'\n'//&
            & 'NO ACTION TAKEN', 1)
            RETURN
         END IF
         CALL SDEL
         RETURN
      END IF
!       COMMAND (MODE)
      IF(WC.EQ.'MODE') THEN
         CALL SMODE
         RETURN
      END IF
!       COMMAND (PIVAXIS)
      IF(WC.EQ.'PIVAXIS') THEN
         CALL PIVAXIS
         RETURN
      END IF
!       COMMAND (SPTWT)
      IF(WC.EQ.'SPTWT'.OR.WC.EQ.'SPTWT2') THEN
         CALL SSPTWT
         RETURN
      END IF
!       COMMAND (CLAP.OR.COBS)
      IF(WC.EQ.'CLAP'.OR.WC.EQ.'COBS') THEN
         CALL SAPE
!       CLAP
         IF(surf_clap_type(SURF) == 6.0)  CALL LOADIPOLY(1,SURF)
!       CLAP ERASE
         IF(surf_cobs_ape_type(SURF) == 6.0) CALL LOADIPOLY(2,SURF)
!       COBS
         IF(surf_coat_type(SURF) == 6.0) CALL LOADIPOLY(3,SURF)
!       COBS ERASE
         IF(surf_cobs_era_type(SURF) == 6.0) CALL LOADIPOLY(4,SURF)
         RETURN
      END IF
!       COMMAND (CLAPD OR COBSD)
      IF(WC.EQ.'CLAPD'.OR.WC.EQ.'COBSD') THEN
         CALL SAPED
         RETURN
      END IF
!       COMMAND (AC,AD,AE,AF,AG,ADTOR,AETOR,AFTOR,AGTOR)
!       COMMAND (AH,AI,AJ,AK,AL)
      IF(WC.EQ.'ACK'.OR.WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.&
      &'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'&
      &.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'&
      &.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
         CALL SDEFG
         RETURN
      END IF
!       COMMAND (PIVX.OR.PIVY.OR.PIVZ)
      IF(WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'.OR.WC.EQ.'PIVZ') THEN
         CALL SPIVXYD
         RETURN
      END IF
!       COMMAND (XD.OR.YD.OR.ZD)
      IF(WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ZD') THEN
         CALL SXYD
         RETURN
      END IF
!       COMMAND (ALPHA, BETA OR GAMMA)
      IF(WC.EQ.'ALPHA'.OR.WC.EQ.'BETA'.OR.WC.EQ.&
      &'GAMMA') THEN
         CALL SANGLE
!     USE THE CODE-V DEFAULT GAMMA ONLY IF IT IS NOT EXPLICITLY INPUT
         IF(surf_tilt_flag(SURF) == 4.0) THEN
!       TILT BEN
            RAL=(PII/180.0D0)*surf_alpha(SURF)
            RBE=(PII/180.0D0)*surf_beta(SURF)
            CGAM=(DCOS(RAL)+DCOS(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
            SGAM=-(DSIN(RAL)*DSIN(RBE))/(1.0D0+(DCOS(RAL)*DCOS(RBE)))
            IF(CGAM.LT.-1.0D0) CGAM=-1.0D0
            IF(CGAM.GT.+1.0D0) CGAM=+1.0D0
            IF(SGAM.GE.0.0D0) RGAM=DABS(DACOS(CGAM))
            IF(SGAM.LT.0.0D0) RGAM=-DABS(DACOS(CGAM))
            call set_surf_gamma(SURF, RGAM*180.0D0/PII)
            call set_surf_gamma_deg(SURF, RGAM*180.0D0/PII)
         END IF
         RETURN
      END IF
!       COMMAND (GDX.OR.GDX.OR.GDZ)
      IF(WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ') THEN
         CALL SGXYD
         RETURN
      END IF
!       COMMAND (GALPHA, GBETA OR GGAMMA)
      IF(WC.EQ.'GALPHA'.OR.WC.EQ.'GBETA'.OR.WC.EQ.&
      &'GGAMMA') THEN
         CALL SGANGLE
         RETURN
      END IF
!       COMMAND (DEC)
      IF(WC.EQ.'DEC') THEN
         CALL SDEC
         RETURN
      END IF
!       COMMAND (PIVOT)
      IF(WC.EQ.'PIVOT') THEN
         CALL SPIVOT
         RETURN
      END IF
!       COMMAND (PIVOTD)
      IF(WC.EQ.'PIVOTD') THEN
         CALL SPIVOTD
         RETURN
      END IF
!       COMMAND (PIVAXIS)
      IF(WC.EQ.'PIVAXIS') THEN
         CALL SPIVAX
         RETURN
      END IF
!       COMMAND (TILT,ALPHA BETA GAMMA OR RTILT,ALPHA BETA GAMMA
!               OR TILT AUTO,ALPHA BETA GAMMA OR TILT AUTOM,ALPHA
!               BETA GAMMA)
      IF(WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'.OR.&
      &WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'.OR.&
      &WC.EQ.'RTILT') THEN
         CALL STILT
         RETURN
      END IF
!       COMMAND (TILT AUTOD)
      IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTOD') THEN
         CALL STILTAD
         RETURN
      END IF
!       COMMAND (TILTD)
      IF(WC.EQ.'TILTD') THEN
         CALL STILTD
         RETURN
      END IF
!       COMMAND (YTORIC OR XTORIC)
      IF(WC.EQ.'YTORIC'.OR.WC.EQ.'XTORIC') THEN
         CALL STORIC
         RETURN
      END IF
!       COMMAND (RDTOR,CVTOR)
      IF(WC.EQ.'RDTOR'.OR.WC.EQ.'CVTOR') THEN
         CALL SCVR
         RETURN
      END IF
!       COMMAND (RAYERROR) THEN
      IF(WC.EQ.'RAYERROR') THEN
         CALL RAYERROR
         RETURN
      END IF
!       COMMAND (ROO) THEN
      IF(WC.EQ.'ROO') THEN
         CALL ROO
         RETURN
      END IF
!       COMMAND (CCR) THEN
      IF(WC.EQ.'CCR') THEN
         CALL CCR
         RETURN
      END IF
!       COMMAND (COATING)
      IF(WC.EQ.'COATING') THEN
         CALL SCOATING
         RETURN
      END IF
!       COMMAND (SPGR)
      IF(WC.EQ.'SPGR') THEN
         CALL SPGR
         RETURN
      END IF
!       COMMAND (FOOTBLOK)
      IF(WC.EQ.'FOOTBLOK') THEN
         CALL BLKHEAD
         RETURN
      END IF
!       COMMAND (TORD)
      IF(WC.EQ.'TORD') THEN
         CALL STORD
         RETURN
      END IF
!       COMMAND (GRTD)
      IF(WC.EQ.'GRTD') THEN
         CALL SGRTD
         RETURN
      END IF
!       COMMANDS (PY,PX,PCY,PCX,CAY AND CAX)
!       AND DELETIONS TSD
      IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.&
      &'PCY'.OR.WC.EQ.'TSD'.OR.WC.EQ.'REDSLV'.OR.&
      &WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX') THEN

         CALL THSOLV


         IF(sys_telecentric().NE.0.0D0.AND.DABS(surf_thickness(NEWOBJ)).GE.1.0D10) THEN
            call sys_set_telecentric(0.0D0)
            OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
            CALL SHOWIT(1)
            OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
            CALL SHOWIT(1)
         END IF
         RETURN
      END IF
!       COMMANDS (APY,PIY,PUY,APCY,PICY,PUCY,COCY,
!                 APX,PIX,PUX,APCX,PICX,PUCX, AND COCX)
!       AND DELETIONS CSD,CSDX,CSDY
      IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.&
      &WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.WC.EQ.&
      &'COCY'.OR.WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'&
      &.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.WC.EQ.&
      &'COCX'.OR.WC.EQ.'CSD'.OR.WC.EQ.'CSDX'.OR.&
      &WC.EQ.'CSDY') THEN
         CALL CVSOLV
         RETURN
      END IF
!       COMMANDS (PIKUP)
      IF(WC.EQ.'PIKUP') THEN
         CALL SPIKUP
         IF(sys_telecentric().NE.0.0D0.AND.DABS(surf_thickness(NEWOBJ)).GE.1.0D10) THEN
            call sys_set_telecentric(0.0D0)
            OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
            CALL SHOWIT(1)
            OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
            CALL SHOWIT(1)
         END IF
         RETURN
      END IF
!       COMMANDS (PIKD)
      IF(WC.EQ.'PIKD') THEN
         ! SPIKD takes the parsed command explicitly (refactor #9 pilot)
         block
           type(parsed_command) :: cmd
           cmd = capture_command()
           CALL SPIKD(cmd)
         end block
         RETURN
      END IF
   END IF
   RETURN
END
SUBROUTINE RE_DISPLAY_LENS(YESEOS)
   USE GLOBALS
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   INTEGER SAVESURF
   LOGICAL YESEOS
   SAVE SAVESURF
   SAVESURF=SURF
   SAVE_KDP(21)=SAVEINPT(21)
   LNSTYP=1
   CALL LNSEOS
   REST_KDP(21)=RESTINPT(21)
   SAVE_KDP(21)=SAVEINPT(21)
! WINTER
!        CALL MY_CLS
   REST_KDP(21)=RESTINPT(21)
   IF(.NOT.YESEOS) THEN
            CALL KDP_EXEC('RTG ALL')
            CALL KDP_EXEC('U L')
      SURF=SAVESURF
   END IF
   RETURN
END
