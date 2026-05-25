! SUB CMDER.FOR
SUBROUTINE CMDER
   USE GLOBALS
   use zoa_ui
   use zoa_output, only: zoa_emit
   use glass_manager
   use kdp_interfaces , only: EDITOR, POWSYM, FIR,&
   &SYSCONFIGUI, SPR, plot_seidel, MACROUI, PLT3DTST,&
   &PLTIMTST, PTSTUFF, Sandbox
   use codeV_commands, only: startCodeVLensUpdateCmd,&
   &newLens, setLens, execSav
   use global_widgets, only: ioConfig, ID_TERMINAL_DEFAULT,&
   &ID_TERMINAL_KDPDUMP, sysConfig
   use DATLEN

!
   use DATSP1
   use DATHGR
   use DATSPD
   use DATSUB
   use DATMAI
   use command_utils, only: is_command_query
   use mod_system, only: sys_last_surf, sys_ref_surf, sys_astop, sys_astop_adj, &
      &sys_ray_aiming, sys_telecentric, sys_verbose_optim, &
      &sys_set_ref_surf, sys_set_astop, sys_set_astop_adj, &
      &sys_set_ray_aiming, sys_set_telecentric
   IMPLICIT NONE
   LOGICAL ITERROR

!
   CHARACTER DDATE*10,TTIME*8,FILNM*10,NM*8,TTTIM*8,DDDAT*10 &
   &,AI*3,OWC*8,OWQ*8,HNAM*8,AI4*4
!
   LOGICAL LPASS1,LPASS2,EXTDMTF1,EXTDMTF2,PERF
!
   REAL*8 X1,Y1,Z1,XL1,XM1,XN1,YL1,YM1,YN1,ZL1,ZM1,ZN1
   REAL*8 X2,Y2,Z2,XL2,XM2,XN2,YL2,YM2,YN2,ZL2,ZM2,ZN2
   REAL*8 X3,Y3,Z3,XL3,XM3,XN3,YL3,YM3,YN3,ZL3,ZM3,ZN3
   REAL*8 ALPHA,BETA,GAMMA
!
   COMMON/DMTFEXT/EXTDMTF1,EXTDMTF2,PERF
!
   LOGICAL EXTGMTF1,EXTGMTF2,RTGERROR
!
   COMMON/GMTFEXT/EXTGMTF1,EXTGMTF2
!
   CHARACTER LUNI*3
!
   REAL*8 XPFOB,YPFOB
   COMMON/PFOB/XPFOB,YPFOB,LUNI
!
   REAL*8 FLDCODE(1:2,0:10)
   CHARACTER FLDUNIT(0:10)*3
   COMMON/FLDLAB/FLDCODE,FLDUNIT
!
   COMMON/MANH/HNAM
!
   LOGICAL ERRFOB,FIELDER,RAYER,IS &
   &,LBT,EXIS51,OPEN51,LERROR,ERRORPSF,OLDLDIF2,OLDLDIF &
   &,REFERR,PDOK
!
   LOGICAL MULTIOTF
   COMMON/OTFMULTI/MULTIOTF
!
   REAL*8 DIAX,DIAY,ENDIST,OS62,OS63


!     COMMON/WAITEROLD/OLDWAITER
!
   INTEGER II,I,ITNUM,ITCT,OLDSTOP,OLDSTOP2,KKKEY,CCODE,ERROR &
   &,OREFLOC,IIG,OCOLDEF,OCOLBAC,IU,IPASS2,CACOCHVIE &
   &,MMM,N,ALLOERR
   COMMON/CCPAC/OCOLBAC,OCOLDEF
   COMMON/GII/IIG
!
   COMMON/CODECC/CCODE,KKKEY
!
   COMMON/STOPPER_2/OLDSTOP,OLDSTOP2
!
   COMMON/STRNGR/DDDAT,TTTIM,NM,FILNM
!
!     THESE LINES SUPPORT THE 50 LEVELS OF INPUT SAVE AND RECALL IN KDP
   !LOGICAL REST_KDP(1:200)
   !COMMON/KDPREST/REST_KDP
   !LOGICAL SAVE_KDP(1:200)
   !COMMON/KDPSAVE/SAVE_KDP
   !LOGICAL RESTINPT
   !LOGICAL SAVEINPT
   !EXTERNAL RESTINPT
   !EXTERNAL SAVEINPT

   !INCLUDE 'DATLEN.INC'
!        INCLUDE 'DATMAC.INC' - binary macro system removed
   REAL*8 TESTA,TESTB,TESTC,TESTD,TESTE
!
!     REMEMBER ORIGINAL REFLOC SETTING FOR MULTIPLE FOV DIFFRACTION
!     CALCULATIONS
   OREFLOC=REFLOC
   LASTCOMWRD=WC


   !if(INPUT.EQ.'MREFRESH') call updateTerminalLog(INPUT, "blue")
   !if (.NOT.isCodeVCommand(WC)) then

   !call updateTerminalLog(INPUT, "black")

   !end if
   !call sleep(1)
   !call pending_events()
   !PRINT *, "CMDER INPUT = ", INPUT

!
! ******************************************************************************
!
!       CMD LEVEL COMMANDS F1=1
!
!*******************************************************************************
!
   IF(F1.EQ.1) THEN
      IF(WC.EQ.'?') THEN
         CALL QUERRYY
         RETURN
      END IF
      IF(WC.EQ.' '.AND.F50.EQ.1) THEN
         CALL BLANK
         F50=0
         RETURN
      END IF
      IF(WC.EQ.' '.AND.F50.NE.1) THEN
         RETURN
      END IF
!       CHECK FOR SPECT SUB-LEVEL
!
      IF(F17.EQ.0.AND.WC.EQ.'ITF'.OR.F17.EQ.0.AND.WC.EQ.'ILF'&
      &.OR.F17.EQ.0.AND.WC.EQ.'PROCEED'&
      &.OR.F17.EQ.0.AND.WC.EQ.'IPF') THEN
!       ILF
         IF(WC.EQ.'ILF') THEN
            IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
!
               CALL REPORT_ERROR_AND_FAIL(&
               & '"ILF" TAKES NO EXPLICIT INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            F48=1
            OUTLYNE='WARNING:'
            CALL SHOWIT(1)
            OUTLYNE='IF THE NEXT COMMAND IS "PROCEED" YOUR LENS LIBRARY'
            CALL SHOWIT(1)
            OUTLYNE='WILL BE ERASED'
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(WC.EQ.'PROCEED'.AND.F48.EQ.1) THEN
            IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
               OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               F48=0
               CALL MACFAL
               RETURN
            END IF
            CALL ILF
            F48=0
            RETURN
         END IF
         IF(WC.NE.'PROCEED'.AND.F48.EQ.1) THEN
            OUTLYNE='ILF CANCELLED'
            CALL SHOWIT(1)
            F48=0
            RETURN
         END IF
!       ITF
         IF(WC.EQ.'ITF') THEN
            IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"ITF" TAKES NO EXPLICIT INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            OUTLYNE='WARNING:'
            CALL SHOWIT(1)
            OUTLYNE='IF THE NEXT COMMAND IS "PROCEED" YOUR TRANSMISSION'
            CALL SHOWIT(1)
            OUTLYNE='LIBRARY WILL BE ERASED'
            CALL SHOWIT(1)
            F45=1
            RETURN
         END IF
         IF(WC.EQ.'PROCEED'.AND.F45.EQ.1) THEN
            IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
               OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               F45=0
               CALL MACFAL
               RETURN
            END IF
            CALL ITF
            F45=0
            RETURN
         END IF
         IF(WC.NE.'PROCEED'.AND.F45.EQ.1) THEN
            OUTLYNE='ITF CANCELLED'
            CALL SHOWIT(1)
            F45=0
            RETURN
         END IF
!       IPF
         IF(WC.EQ.'IPF') THEN
            IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
               CALL REPORT_ERROR_AND_FAIL(&
               & '"IPF" TAKES NO EXPLICIT INPUT'//'\n'//&
               & 'RE-ENTER COMMAND', 1)
               RETURN
            END IF
            OUTLYNE='WARNING:'
            CALL SHOWIT(1)
            OUTLYNE='IF THE NEXT COMMAND IS "PROCEED" YOUR PLOT'
            CALL SHOWIT(1)
            OUTLYNE='LIBRARY WILL BE ERASED'
            CALL SHOWIT(1)
            F33=1
            RETURN
         END IF
         IF(WC.EQ.'PROCEED'.AND.F33.EQ.1) THEN
            IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
               OUTLYNE='"PROCEED" TAKES NO EXPLICIT INPUT'
               CALL SHOWIT(1)
               OUTLYNE='RE-ENTER COMMAND'
               CALL SHOWIT(1)
               F33=0
               CALL MACFAL
               RETURN
            END IF
            CALL IPF
            F33=0
            RETURN
         END IF
         IF(WC.NE.'PROCEED'.AND.F33.EQ.1) THEN
            OUTLYNE='IPF CANCELLED'
            CALL SHOWIT(1)
            F33=0
            RETURN
         END IF
         IF(WC.EQ.'PROCEED'.AND.F45.EQ.0) THEN
            OUTLYNE='NO ITF ACTION TAKEN'
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(WC.EQ.'PROCEED'.AND.F33.EQ.0) THEN
            OUTLYNE='NO IPF ACTION TAKEN'
            CALL SHOWIT(1)
            RETURN
         END IF
         IF(WC.EQ.'PROCEED'.AND.F48.EQ.0) THEN
            OUTLYNE='NO ILF ACTION TAKEN'
            CALL SHOWIT(1)
            RETURN
         END IF
      END IF
      IF(F17.EQ.1.AND.WC.EQ.'ITF'.OR.F17.EQ.1.AND.WC.EQ.'ILF'&
      &.OR.F17.EQ.1.AND.WC.EQ.'PROCEED'&
      &.OR.F17.EQ.1.AND.WC.EQ.'IPF') THEN
!       WE ARE AT SPECT
         OUTLYNE=&
         &'"ILF", "ITF" AND "IPF" NOT VALID AT "SPECT" LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(F17.EQ.1) THEN
!               WE ARE IN THE SPECT PROGRAM LEVEL
!
!       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
!       ENTERED AT THE SPECT LEVEL.
!               WE ARE IN SPECT MODE. THE SUBROUTINE
!               SPECIN CONTAINS RULES FOR RESPONDING TO SPECIAL
!       SPECT SPECIFIC INPUT. MANY CMD LEVEL COMMANDS ARE ALSO ALLOWED.
!       DURING THIS MODE SPECIFIC SPECT COMMANDS ARE PERFORMED
!       BY SPECIN.FOR
!
         IS=.FALSE.
         IF(WC.EQ.'START')       IS=.TRUE.
         IF(WC.EQ.'WAVLN')       IS=.TRUE.
         IF(WC.EQ.'INT')         IS=.TRUE.
         IF(WC.EQ.'TABLE')       IS=.TRUE.
!               IF(WC.EQ.'LOADPHOT')    IS=.TRUE.
!               IF(WC.EQ.'LOADSCOT')    IS=.TRUE.
         IF(WC.EQ.'DIRECT')      IS=.TRUE.
         IF(WC.EQ.'INSERT')      IS=.TRUE.
         IF(WC.EQ.'DROP')        IS=.TRUE.
         IF(WC.EQ.'DELETE')      IS=.TRUE.
         IF(WC.EQ.'GETFILE')     IS=.TRUE.
         IF(WC.EQ.'BLACKBDY')    IS=.TRUE.
         IF(WC.EQ.'PHOTOPIC')    IS=.TRUE.
         IF(WC.EQ.'SCOTOPIC')    IS=.TRUE.
         IF(WC.EQ.'PUT')         IS=.TRUE.
         IF(WC.EQ.'LIST')        IS=.TRUE.
         IF(WC.EQ.'RENAME')      IS=.TRUE.
         IF(WC.EQ.'SPRINT')      IS=.TRUE.
         IF(WC.EQ.'PUNCH')       IS=.TRUE.
         IF(WC.EQ.'INTER')       IS=.TRUE.
         IF(WC.EQ.'NARCIN')      IS=.FALSE.
         IF(WC.EQ.'FLNAME')      IS=.FALSE.
         IF(WC.EQ.'FILE')        IS=.TRUE.
         IF(WC.EQ.'ENDTABLE')    IS=.TRUE.
         IF(WC.EQ.'NARC')        IS=.FALSE.
         IF(WC.EQ.'EOS')         IS=.TRUE.
         IF(WC.EQ.'DATA')        IS=.TRUE.
         IF(WC.EQ.'CUME')        IS=.TRUE.
         IF(WC.EQ.'WFACTOR')     IS=.TRUE.
         IF(WC.EQ.'WORK')        IS=.TRUE.
         IF(WC.EQ.'PTABLE')      IS=.TRUE.
         IF(WC.EQ.'DIR')         IS=.TRUE.
         IF(WC.EQ.'CK')           IS=.TRUE.
         IF(WC.EQ.'M')           IS=.TRUE.
         IF(WC.EQ.'NAME')        IS=.TRUE.
         IF(WC.EQ.'PLOTR')       IS=.TRUE.
         IF(WC.EQ.'PLOTT')       IS=.TRUE.
         IF(IS) THEN
            CALL SPECIN
            RETURN
!       PROCEED AS A CHECK FOR INVALIDS WAS ALREADY MADE.
         END IF
!       NOT AT SPECT LEVEL, PROCEED
      END IF
!************************************************************
      PDOK=.FALSE.
      IF(WC.EQ.'PARTDRAW') PDOK=.TRUE.
      IF(WC.EQ.'OD      ') PDOK=.TRUE.
      IF(WC.EQ.'DIATOL  ') PDOK=.TRUE.
      IF(WC.EQ.'RADTOL  ') PDOK=.TRUE.
      IF(WC.EQ.'RADTLF  ') PDOK=.TRUE.
      IF(WC.EQ.'FRNG    ') PDOK=.TRUE.
      IF(WC.EQ.'THITOL  ') PDOK=.TRUE.
      IF(WC.EQ.'CLERAP  ') PDOK=.TRUE.
      IF(WC.EQ.'SURFQUAL') PDOK=.TRUE.
      IF(WC.EQ.'FNGDIA  ') PDOK=.TRUE.
      IF(WC.EQ.'CENTIR  ') PDOK=.TRUE.
      IF(WC.EQ.'BRKEDG  ') PDOK=.TRUE.
      IF(WC.EQ.'SURFCOAT') PDOK=.TRUE.
      IF(WC.EQ.'SAGTOL  ') PDOK=.TRUE.
      IF(WC.EQ.'PRPNTL  ') PDOK=.TRUE.
      IF(WC.EQ.'TITLE   ') PDOK=.TRUE.
      IF(WC.EQ.'DWGNO   ') PDOK=.TRUE.
      IF(WC.EQ.'SURFMATL') PDOK=.TRUE.
      IF(WC.EQ.'GLSCD   ') PDOK=.TRUE.
      IF(WC.EQ.'CONAME  ') PDOK=.TRUE.
      IF(WC.EQ.'WAVEL   ') PDOK=.TRUE.
      IF(WC.EQ.'PARTGO  ') PDOK=.TRUE.
      IF(WC.EQ.'PARTQUIT') PDOK=.TRUE.
      IF(PDOK) THEN
!       PART DRAWINGS
         CALL PART_DRAW
         PDOK=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'LENO')THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='CFG 1'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         CALL LENOUT
         RETURN
      END IF
      IF(WC.EQ.'LENOCSV')THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='CFG 1'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         CALL LENOCSV
         RETURN
      END IF
      IF(WC.EQ.'CV2PRG')THEN
         CALL CV2PRG
         ! Save lens in temp folder (skip in headless/test mode to avoid polluting test state)
         IF(.NOT.HEADLESS_MODE) CALL execSav('SAV')
         RETURN
      END IF
      ! ZMX2PRG, ECHO migrated to codeV-commands.f90
      IF(WC.EQ.'POWSYM') THEN
         CALL POWSYM
         RETURN
      END IF
      IF(WC.EQ.'SYSCON') THEN
         IF(HEADLESS_MODE) THEN
            OUTLYNE='Command requires GUI'
            CALL SHOWIT(1)
         ELSE
            CALL SYSCONFIGUI
         END IF
         RETURN
      END IF
      IF(WC.EQ.'SPR') THEN
         CALL SPR
         RETURN
      END IF
      IF(WC.EQ.'PLT3DTST') THEN
         CALL PLT3DTST
         return
      END IF
      IF(WC.EQ.'PTSTUFF') THEN
         CALL PTSTUFF
         return
      END IF
      IF(WC.EQ.'PLTIMTST') THEN
         CALL PLTIMTST
         return
      END IF
      ! Support for CodeV 'LEN NEW' Command
      IF(WC.EQ.'LEN') THEN
         IF(WQ.EQ.'NEW') THEN
            CALL NEWLENS
            return
         END IF
         call setLens()
         RETURN
      END IF
      ! Check if we have a CodeV command to execute
      If (startCodeVLensUpdateCmd(WC)) then
         return
      end if

      IF(WC.EQ.'MACROUI') THEN
         IF(HEADLESS_MODE) THEN
            OUTLYNE='Command requires GUI'
            CALL SHOWIT(1)
         ELSE
            CALL MACROUI
         END IF
         RETURN
      END IF

      ! TSTCMDS migrated to codeV-commands.f90
      IF(WC.EQ.'SANDBOX') THEN
         CALL Sandbox
      END IF
      IF(WC.EQ.'FIR') THEN
         CALL FIR
         RETURN
      END IF
      IF(WC.EQ.'SPSRF'.AND.WQ.NE.'ON'.AND.&
      &WC.EQ.'SPSRF'.AND.WQ.NE.'OFF')THEN
         IF(RAYCLEAR) THEN
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            REFEXT=.FALSE.
            SPDEXT=.FALSE.
            GSPDEXT=.FALSE.
            CPFNEXT=.FALSE.
            CALL DELPSF
         END IF
         II=INT(sys_last_surf())
         ALENS(88,1:II)=0.0D0
         LPASS1=.FALSE.
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL GRIDS(1,0,LPASS1)
!       CALL DEROFF
!       CALL AUTOFF
         CALL SPSIN
         RETURN
      END IF
      IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON'.OR.&
      &WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')THEN
         IF(RAYCLEAR) THEN
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            REFEXT=.FALSE.
            SPDEXT=.FALSE.
            GSPDEXT=.FALSE.
            CPFNEXT=.FALSE.
            CALL DELPSF
         END IF
         II=INT(sys_last_surf())
         ALENS(88,1:II)=0.0D0
         LPASS1=.FALSE.
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL GRIDS(1,0,LPASS1)
         CALL SPONOF
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'SPSRF'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'SP'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'SPSRF'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'SP') THEN
         IF(RAYCLEAR) THEN
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            REFEXT=.FALSE.
            SPDEXT=.FALSE.
            GSPDEXT=.FALSE.
            CPFNEXT=.FALSE.
            CALL DELPSF
         END IF
         II=INT(sys_last_surf())
         ALENS(88,1:II)=0.0D0
         LPASS1=.FALSE.
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL GRIDS(1,0,LPASS1)
         CALL SPSUP
         RETURN
      END IF
      IF(WC.EQ.'SPFIT')THEN
         CALL SPFIT
         RETURN
      END IF
      IF(WC.EQ.'CONFIGS'.OR.WC.EQ.'CONFIG') THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL DEROFF
         CALL AUTOFF
         CALL CFGIN
         RETURN
      END IF
      IF(WC.EQ.'SPECT')THEN
         CALL SPECTR
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'CONFIGS'.OR.&
      &WQ.EQ.'UPDATE'.AND.WQ.EQ.'CONFIG'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'CF'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'CONFIGS'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'CONFIG'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'CF') THEN
!       DO A FORCED RETURN TO CFG1
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL FRCCF1(1)
         CALL CFGUP
         RETURN
      END IF
      IF(WC.EQ.'CFG') THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL CFGCHG
         RETURN
      END IF
      IF(WC.EQ.'DELCFG') THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL DELCFG
         RETURN
      END IF
      IF(WC.EQ.'REMOVE') THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL REMOVE
         RETURN
      END IF
      IF(WC.EQ.'DEZOOM') THEN
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL DEROFF
         CALL AUTOFF
         CALL DEZOOM
         RETURN
      END IF
!       CALL TO LSTAT( LENS LIBRARY STATUS)
      IF(WC.EQ.'LSTAT')  THEN
         IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
            CALL REPORT_ERROR_AND_FAIL(&
            & '"LSTAT" TAKES NO EXPLICIT INPUT'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF

         CALL myLSTAT
         RETURN
      END IF
!               LENS LIBRARY COMMANDS
      IF(WC.EQ.'LIB') THEN
         OPTMES=.FALSE.
         CALL LLIB
         OPTMES=.TRUE.
         RETURN
      END IF
!
!       CALL TO PSTAT( PLOT LIBRARY STATUS)
      IF(WC.EQ.'PSTAT')  THEN
         IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
            OUTLYNE=&
            &'"PSTAT" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
            RETURN
         END IF
         CALL PLSTAT
         RETURN
      END IF
!                PLOT LIBRARY COMMANDS (PLIB/PLLIB removed)
!
      IF(WC.EQ.'PRSPR') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PPRSPR
         RETURN
      END IF
      IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
         CALL SLI
         RETURN
      END IF
      IF(WC.EQ.'WV2') THEN
         CALL SWV2
         RETURN
      END IF
      IF(WC.EQ.'CW'.OR.WC.EQ.'PCW'.OR.&
      &WC.EQ.'SCW') THEN
         CALL SCW
         RETURN
      END IF
      IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX') THEN
         CALL SSA
         RETURN
      END IF
      IF(WC.EQ.'WRY'.OR.WC.EQ.'WRX') THEN
         CALL SWR
         RETURN
      END IF
      IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
         CALL SNAO
         RETURN
      END IF
      IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
         CALL SFNO
         RETURN
      END IF
      IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
         CALL SSC
         RETURN
      END IF
      IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM') THEN
         CALL PXYIM
         RETURN
      END IF
      IF(WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
         CALL RXYIM
         RETURN
      END IF
      IF(WC.EQ.'BDX'.OR.WC.EQ.'BDY') THEN
         CALL SBD
         RETURN
      END IF
      IF(WC.EQ.'TESTRD'.OR.WC.EQ.'TESTCYL') THEN
         CALL TEST_PLATE_IT
         RETURN
      END IF
      IF(WC.EQ.'CTG'.OR.WC.EQ.'RTG') THEN
         testa=3.0d0
         testb=0.0d0
         testc=1.0d-200
         testd=1.0d200
         TESTE=-4.0D0
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         LBT=.FALSE.
         RTGERROR=.FALSE.
         CALL SRTG(LBT,RTGERROR)
         OWC=WC
         WC='ARRAY'
         IF(.NOT.RTGERROR) CALL SARRAY(.FALSE.)
         WC=OWC
         RETURN
      END IF
      IF(WC.EQ.'CTGLBL'.OR.WC.EQ.'RTGLBL') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         LBT=.TRUE.
         CALL SRTG(LBT,RTGERROR)
         OWC=WC
         WC='ARRAY'
         IF(.NOT.RTGERROR) CALL SARRAY(.FALSE.)
         WC=OWC
         RETURN
         LBT=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'LENADD') THEN
         CALL DEROFF
         CALL AUTOFF
         CALL LENADD
         RETURN
      END IF
      !IF(WC.EQ.'LOADPROF') THEN
      !CALL LOADPROF
      !RETURN
      !END IF
      !IF(WC.EQ.'LISTPROF') THEN
      !CALL LISTPROF
      !RETURN
      !END IF
      !IF(WC.EQ.'LOADISSU') THEN
      !CALL LOADISSU
      !RETURN
      !END IF
      !IF(WC.EQ.'PLOTISSU') THEN
      !CALL PLOTISSU
      !RETURN
      !END IF
      IF(WC.EQ.'GLASSP') THEN
         IF(WQ.EQ.'GLAK'.AND.SST.EQ.0.OR.&
         &WQ.EQ.'GLCAT'.AND.SST.EQ.0) THEN
            ! TODO:  Put all this in glass manager
            do I=1,size(gdb%catalogs)
               WQ = trim(gdb%catalogs(I))
               CALL GLASSP
            end do
            WQ='GLAK'

         ELSE
            IF(WS(1:1).EQ.':') WS(1:80)=WS(2:80)
            CALL GLASSP
         END IF
         RETURN
      END IF
      IF(WC.EQ.'DEG'.OR.WC.EQ.'RAD'.OR.WC.EQ.'TANGENT'&
      &.OR.WC.EQ.'ANGMODE') THEN
         CALL DEGRAD
         RETURN
      END IF
      IF(WC.EQ.'ASPH') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SASPH
         WC='ASPH2'
         CALL SASPH
         WC='ASPH'
         RETURN
      END IF
      IF(WC.EQ.'ASPH2') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SASPH
         RETURN
      END IF
      IF(WC.EQ.'SURTYPE') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SURFTYPE
         RETURN
      END IF
      IF(WC.EQ.'FOOTBLOK') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL BLKOUT
         RETURN
      END IF
      IF(WC.EQ.'DUMOUT') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL DUMOUT
         RETURN
      END IF
      IF(WC.EQ.'TASPH') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL STASPH
         RETURN
      END IF
      IF(WC.EQ.'REFS') THEN
         WRITE(OUTLYNE,100) INT(sys_ref_surf())
100      FORMAT('THE CURRENT REFERENCE SURFACE I SURFACE # ',I3)
         CALL SHOWIT(0)
         RETURN
      END IF
      IF(WC.EQ.'CAOB') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SCAOB
         RETURN
      END IF
      IF(WC.EQ.'SPIDER') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SPIDEROUT
         RETURN
      END IF
      IF(WC.EQ.'ARRAY') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SARRAY(.TRUE.)
         RETURN
      END IF
      IF(WC.EQ.'TAD') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL STAD
         RETURN
      END IF
      IF(WC.EQ.'SPIDER') THEN
!        CALL SPIDER
         RETURN
      END IF
      IF(WC.EQ.'COATING') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL COATING
         RETURN
      END IF
      IF(WC.EQ.'PIVOT') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SPIV
         RETURN
      END IF
      IF(WC.EQ.'RIN'.OR.WC.EQ.'RIN2') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SRIN
         RETURN
      END IF
      IF(WC.EQ.'GRT') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SGRATT
         RETURN
      END IF
      IF(WC.EQ.'NDEX'.OR.WC.EQ.'NDEX2') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL SNDEX
         RETURN
      END IF
      IF(WC.EQ.'TORIC'.OR.WC.EQ.'RTORIC'.OR.&
      &WC.EQ.'TR'.OR.WC.EQ.'TC'.OR.WC.EQ.'CTORIC') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PTORIC
         RETURN
      END IF
      IF(WC.EQ.'SPTWT'.OR.WC.EQ.'SPTWT2') THEN
         CALL SSPTWT
         RETURN
      END IF
      IF(WC.EQ.'MAGY'.OR.WC.EQ.'MAGX') THEN
         CALL SMAG
         RETURN
      END IF
      IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX') THEN
         CALL SFNB
         RETURN
      END IF
      IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX') THEN
         CALL SER
         RETURN
      END IF
      IF(WC.EQ.'SLV') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRSLV
         RETURN
      END IF
      IF(WC.EQ.'PIKK') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRPIK
         RETURN
      END IF
      IF(WC.EQ.'PXTX'.OR.WC.EQ.'PXTY'.OR.WC.EQ.'PITX'&
      &.OR.WC.EQ.'PITY'.OR.WC.EQ.'PRTX'.OR.WC.EQ.'PRTY') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PAROUT
         RETURN
      END IF
      IF(WC.EQ.'PRREF') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRREF
         RETURN
      END IF
      IF(WC.EQ.'PRDIFFXM'.OR.WC.EQ.'PRDIFFYM'.OR.WC.EQ.'PRDIFFXR'&
      &.OR.WC.EQ.'PRDIFFYR') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRDIFF
         RETURN
      END IF
      IF(WC.EQ.'MAB3'.OR.WC.EQ.'XMAB3') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL MMAB3
         !IF(WC.EQ.'MAB3') CALL MMAB3_NEW(1)
         !IF(WC.EQ.'XMAB3') CALL MMAB3_NEW(0)
         RETURN
      END IF
      IF(WC.EQ.'MAB5'.OR.WC.EQ.'XMAB5') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL MMAB5
         RETURN
      END IF
      IF(WC.EQ.'MABX5'.OR.WC.EQ.'XMABX5') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL MMABX5
         RETURN
      END IF
      IF(WC.EQ.'SA357'.OR.WC.EQ.'XSA357') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL SA357
         RETURN
      END IF
      IF(WC.EQ.'MABP3'.OR.WC.EQ.'XMABP3') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL MMABP3
         RETURN
      END IF
      IF(WC.EQ.'SA357I'.OR.WC.EQ.'XSA357I') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL A357I
         RETURN
      END IF
      IF(WC.EQ.'MAB5I'.OR.WC.EQ.'XMAB5I') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL AB5I
         RETURN
      END IF
      IF(WC.EQ.'MABX5I'.OR.WC.EQ.'XMABX5I') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRC
         CALL ABX5I
         RETURN
      END IF
      IF(WC.EQ.'PCD3'.OR.WC.EQ.'XPCD3'.OR.WC.EQ.&
      &'SCD3'.OR.WC.EQ.'XSCD3') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRD
         CALL PCD3
         RETURN
      END IF
      IF(WC.EQ.'PCD5'.OR.WC.EQ.'XPCD5'.OR.WC.EQ.&
      &'SCD5'.OR.WC.EQ.'XSCD5') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRD
         CALL PCD5
         RETURN
      END IF
      IF(WC.EQ.'PCDX5'.OR.WC.EQ.'XPCDX5'.OR.WC.EQ.&
      &'SCDX5'.OR.WC.EQ.'XSCDX5') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRD
         CALL PCDX5
         RETURN
      END IF
      IF(WC.EQ.'PCDP3'.OR.WC.EQ.'XPCDP3'.OR.WC.EQ.&
      &'SCDP3'.OR.WC.EQ.'XSCDP3') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRD
         CALL PCDP3
         RETURN
      END IF
      IF(WC.EQ.'PCDSA'.OR.WC.EQ.'XPCDSA'.OR.WC.EQ.&
      &'SCDSA'.OR.WC.EQ.'XSCDSA') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRD
         CALL PCDSA
         RETURN
      END IF
      IF(WC.EQ.'FCHY'.OR.WC.EQ.'FCHX') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRTRB
         CALL FCH
         RETURN
      END IF
      IF(WC.EQ.'LOADMAC') THEN
         OUTLYNE = "LOADMAC COMMAND DISABLED"
         CALL SHOWIT(1)
         !CALL MACARRAY_LOAD(NUMINLIST)
         RETURN
      END IF
      IF(WC.EQ.'OCDY'.OR.WC.EQ.'OCDX') THEN
         CALL OCD
         RETURN
      END IF
      IF(WC.EQ.'SC'.OR.WC.EQ.'WSC') THEN
         CALL SCALLE
         RETURN
      END IF
      IF(WC.EQ.'CK'.OR.WC.EQ.'M') THEN
         CALL MESCOM
         RETURN
      END IF
      IF(WC.EQ.'EXI'.OR.WC.EQ.'EXIT') THEN
         CALL EXITT(0)
         RETURN
      END IF
!       THREEDEESET (PLOTCAD2) removed
      IF(WC.EQ.'X1Y1='.OR.WC.EQ.'X2Y2='.OR.&
      &WC.EQ.'X3Y3='.OR.WC.EQ.'X4Y4='.OR.&
      &WC.EQ.'INTERP') THEN
         CALL INTRPP
         RETURN
      END IF
      IF(WC.EQ.'TABLE'.OR.WC.EQ.'COLHD'.OR.&
      &WC.EQ.'COLHD2'.OR.WC.EQ.'ROWHD'.OR.&
      &WC.EQ.'ROWHD2') THEN
         CALL TABLE
         RETURN
      END IF
      IF(WC.EQ.'NEWCMD'.OR.WC.EQ.'STWORD'.OR.WC.EQ.&
      &'CWORD'.OR.WC.EQ.'QWORD'.OR.WC.EQ.'N1WORD'.OR.WC.EQ.&
      &'N2WORD'.OR.WC.EQ.'N3WORD'.OR.WC.EQ.'N4WORD'.OR.WC &
      &.EQ.'N5WORD') THEN
         CALL CWRITE
         RETURN
      END IF
      IF(WC.EQ.'ATAN2') THEN
         CALL ATANN2
         RETURN
      END IF
      IF(WC.EQ.'LENS') THEN
         PFAC=1.0D0
         IF(RAYCLEAR) THEN
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            REFEXT=.FALSE.
            SPDEXT=.FALSE.
            GSPDEXT=.FALSE.
            CPFNEXT=.FALSE.
            CALL DELPSF
         END IF
         II=INT(sys_last_surf())
         ALENS(88,1:II)=0.0D0
         ALENS(109,1:II)=0.0D0
         LPASS1=.FALSE.
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL GRIDS(1,0,LPASS1)
         LPASS1=.FALSE.
         LPASS2=.FALSE.
         CALL DEFGRIDS(1,0,LPASS1,LPASS2)
         CALL DEROFF
         CALL AUTOFF
         CALL LENNS
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'LENS'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'L'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'LENS'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'L') THEN
         CALL ULENNS
         RETURN
      END IF
! WINTER
!        IF(WC.EQ.'CLS') THEN
!        CALL MY_CLS
!        RETURN
!        END IF
      IF(WC.EQ.'EJECT') THEN
         CALL EJECT
         RETURN
      END IF
      IF(WC.EQ.'FLAG') THEN
         CALL FLAG
         RETURN
      END IF
      IF(WC.EQ.'OUTPUT'.OR.WC.EQ.'OUT') THEN
!       OUT T IS THE SAME AS OUT TP
         IF(WQ.EQ.'T       '.AND.SST.EQ.0) WQ='TP      '
      END IF
!
      IF(WC.EQ.'OUTPUT'.OR.WC.EQ.'OUT') THEN
         IF(WQ(1:8).EQ.'T       '.OR.WQ.EQ.'FILE    ') THEN
            CALL OUTPUT2
            RETURN
         ELSE
            CALL OUTPUT
            RETURN
         END IF
      END IF
      IF(WC.EQ.'INPUT'.OR.WC.EQ.'IN') THEN
         CALL INPUTT
         RETURN
      END IF
      IF(WC.EQ.'ENT'.OR.&
      &WC.EQ.'ENTI'.OR.&
      &WC.EQ.'ENTC'.OR.WC.EQ.'PULL'.OR.&
      &WC.EQ.'IPULL'.OR.WC.EQ.'CPULL'.OR.&
      &WC.EQ.'RUP'.OR.WC.EQ.'IRUP'.OR.WC &
      &.EQ.'CRUP'.OR.WC.EQ.'RDN'.OR.WC.EQ.&
      &'IRDN'.OR.WC.EQ.'CRDN'.OR.WC.EQ.'LASTX'.OR.WC.EQ.&
      &'X-Y'.OR.&
      &WC.EQ.'LASTIX'.OR.WC.EQ.'IX-IY'.OR.WC.EQ.'RE-IM'.OR.&
      &WC.EQ.'CLX'.OR.WC.EQ.'CLIX'.OR.WC.EQ.&
      &'CLSTK'.OR.WC.EQ.'CLSTKI'.OR.WC.EQ.'CLSTKC'.OR.WC.EQ.&
      &'+'.OR.WC.EQ.'-'.OR.WC.EQ.'*'.OR.WC.EQ.'/'.OR.WC.EQ.&
      &'C+'.OR.WC.EQ.'C-'.OR.WC.EQ.'C*'.OR.WC.EQ.'C/'.OR.WC.EQ.&
      &'I+'.OR.WC.EQ.'I-'.OR.WC.EQ.'I*'.OR.WC.EQ.'I/'.OR.&
      &WC.EQ.'Y**X'.OR.WC.EQ.'CY**CX'.OR.&
      &WC.EQ.'IY**IX') THEN
         CALL STACK
         RETURN
      END IF
      IF(WC.EQ.'PRSTK'.OR.WC.EQ.'PRSTKC'.OR.&
      &WC.EQ.'PRSTKI'.OR.WC.EQ.'PRLSTX'.OR.WC.EQ.'PRLSTIX') THEN
         CALL STACK
         RETURN
      END IF
      IF(WC.EQ.'R-P'.OR.WC.EQ.'P-R'.OR.WC.EQ.'R-SP'.OR.&
      &WC.EQ.'SP-R'.OR.WC.EQ.'R-CYL'.OR.WC.EQ.'CYL-R'.OR.&
      &WC.EQ.'H-HMS'.OR.WC.EQ.'HMS-H') THEN
         CALL COORD
         RETURN
      END IF
      IF(WC.EQ.'IN-MM'.OR.WC.EQ.'IN-CM'.OR.WC.EQ.'IN-M'.OR.&
      &WC.EQ.'MM-IN'.OR.WC.EQ.'CM-IN'.OR.WC.EQ.'M-IN') THEN
         CALL COORD
         RETURN
      END IF
      IF(WC.EQ.'CLGREG'.OR.WC.EQ.'CLSTREG'&
      &.OR.WC.EQ.'STADD'.OR.WC.EQ.'STSUB'.OR.&
      &WC.EQ.'STDEV'.OR.WC.EQ.'MEAN'&
      &) THEN
         CALL GGPREG
         RETURN
      END IF
!     THESE ARE 400 GENERAL PURPOSE CHARACTER*80 REGISTERS ADDED
!     IN 3/93
      IF(WC.EQ.'CLASTO'.OR.WC.EQ.'ASTO'.OR.WC.EQ.'ARCL'.OR.&
      &WC.EQ.'AWRITE') THEN
         CALL GPRGA
         RETURN
      END IF
      IF(WC.EQ.'MOD') THEN
         CALL MMOD
         RETURN
      END IF
      IF(WC.EQ.'INTGR'.OR.WC.EQ.'FRAC'.OR.WC.EQ.'FACT'&
      &.OR.WC.EQ.'CHS'.OR.WC.EQ.'RTD'.OR.WC.EQ.'DTR'&
      &.OR.WC.EQ.'ASIN'.OR.WC.EQ.'ACOS'.OR.WC.EQ.'PLUS'&
      &.OR.WC.EQ.'MINUS'.OR.WC.EQ.'DIV'.OR.WC.EQ.'MPY'&
      &.OR.WC.EQ.'MOVE'.OR.WC.EQ.'ATAN'.OR.WC.EQ.'PI'&
      &.OR.WC.EQ.'RAND'.OR.WC.EQ.'SIN'.OR.WC.EQ.'COS'&
      &.OR.WC.EQ.'TAN'.OR.WC.EQ.'TANH'.OR.WC.EQ.'SINH'&
      &.OR.WC.EQ.'COSH'.OR.WC.EQ.'SQRT'.OR.WC.EQ.'ABS'&
      &.OR.WC.EQ.'EXP'.OR.WC.EQ.'RECIP'.OR.WC.EQ.'POW'&
      &.OR.WC.EQ.'LOG10'.OR.WC.EQ.'LN'.OR.WC.EQ.'STORE'&
      &.OR.WC.EQ.'SGN'.OR.WC.EQ.'CLREG'.OR.WC.EQ.'PRIREG'.OR.WC.EQ.&
      &'WRITE'.OR.WC.EQ.'MAXVAL'.OR.WC.EQ.'MINVAL')&
      &THEN
         CALL RGMATH
         RETURN
      END IF
      IF(WC.EQ.'STOREMIN'.OR.WC.EQ.'STOREMAX'.OR.&
      &WC.EQ.'RESETMIN'.OR.WC.EQ.'RESETMAX') THEN
         CALL MINMAXREG
         RETURN
      END IF
      IF(WC.EQ.'LFORMAT')  THEN
         CALL LFORMER
         RETURN
      END IF
      IF(WC.EQ.'LWRITE')  THEN
         CALL LWRITE
         RETURN
      END IF
      IF(WC.EQ.'FORMAT')  THEN
         CALL FORMER
         RETURN
      END IF
!     SPECIAL PFAC OPERATION
      IF(WC.EQ.'PFAC')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='PFAC    '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='PFAC    '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL DAYS OPERATION
      IF(WC.EQ.'DAYS')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='DAYS    '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='DAYS    '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL SHORT OPERATION
      IF(WC.EQ.'SHORT')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SHORT   '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SHORT   '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL MEDIUM OPERATION
      IF(WC.EQ.'MEDIUM')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='MEDIUM  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='MEDIUM  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL LONG OPERATION
      IF(WC.EQ.'LONG')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='LONG    '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='LONG    '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL BUYDELAY OPERATION
      IF(WC.EQ.'BUYDELAY')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='BUYDELAY'
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='BUYDELAY'
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL SELDEALY OPERATION
      IF(WC.EQ.'SELDELAY')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SELDELAY'
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SELDELAY'
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL DINMUL OPERATION
      IF(WC.EQ.'DINMUL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='DINMUL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='DINMUL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL LINTOL OPERATION
      IF(WC.EQ.'LINTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='LINTOL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='LINTOL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL ONTOL OPERATION
      IF(WC.EQ.'ONTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='ONTOL   '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='ONTOL   '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL SINGTOL OPERATION
      IF(WC.EQ.'SINGTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SINGTOL '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SINGTOL '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL SURTOL OPERATION
      IF(WC.EQ.'SURTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SURTOL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SURTOL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL SAGDEL OPERATION
      IF(WC.EQ.'SAGDEL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SAGDEL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SAGDEL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL AIMTOL OPERATION
      IF(WC.EQ.'AIMTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='AIMTOL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='AIMTOL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL CAIMTOL OPERATION
      IF(WC.EQ.'CAIMTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='CAIMTOL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='CAIMTOL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL MAXOPT OPERATION
      IF(WC.EQ.'MAXOPT')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='MAXOPT  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='MAXOPT  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL MAXREG OPERATION
      IF(WC.EQ.'MAXREG')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='MAXREG  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='MAXREG  '
               SQ=1
            END IF
         END IF
      END IF
      IF(WC.EQ.'SERINC')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SERINC  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SERINC  '
               SQ=1
            END IF
         END IF
      END IF
      IF(WC.EQ.'SERLIM')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='SERLIM  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='SERLIM  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL NRAITR OPERATION
      IF(WC.EQ.'NRAITR')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='NRAITR  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='NRAITR  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL DFTOL OPERATION
      IF(WC.EQ.'DIFTOL')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='DIFTOL  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='DIFTOL  '
               SQ=1
            END IF
         END IF
      END IF
!     SPECIAL DELSUR OPERATION
      IF(WC.EQ.'DELSUR')  THEN
         IF(SQ.EQ.0.AND.SN.EQ.0.OR.is_command_query()) THEN
            WC='PMP     '
            WQ='DELSUR  '
            SQ=1
         ELSE
            IF(S1.EQ.1) THEN
               WC='PM      '
               WQ='DELSUR  '
               SQ=1
            END IF
         END IF
      END IF
      IF(WC.EQ.'PM'.OR.WC.EQ.'PMP'.OR.WC.EQ.'OPCON')  THEN
         CALL PM
         RETURN
      END IF
      IF(WC.EQ.'WSYS'.OR.WC.EQ.'WSYSTEM')  THEN
         CALL MYSYS
         RETURN
      END IF
      IF(WC.EQ.'SYS'.OR.WC.EQ.'SYSTEM')  THEN
         CALL MYSYS
         RETURN
      END IF
      IF(WC.EQ.'COFACTOR')  THEN
         CALL COFACTOR
         RETURN
      END IF
      IF(WC.EQ.'DATE')  THEN
         CALL MYDATE(DDATE)
         DDDAT=DDATE
         WRITE(OUTLYNE,1919) DDATE
         CALL SHOWIT(0)
1919     FORMAT(A10)
         RETURN
      END IF
      IF(WC.EQ.'TIME')  THEN
         CALL MYTIME(TTIME)
         TTTIM=TTIME
         WRITE(OUTLYNE,1918) TTIME
         CALL SHOWIT(0)
1918     FORMAT(A8)
         RETURN
      END IF
      IF(WC.EQ.'SETTIMER')  THEN
         CALL SETTIM
         RETURN
      END IF
      IF(WC.EQ.'SEETIMER')  THEN
         CALL SEETIM
         RETURN
      END IF
      IF(WC.EQ.'RAY')  THEN
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
         RETURN
      END IF
      IF(WC.EQ.'MRAYS')  THEN
         IF(WQ.EQ.'CAOB') CACOCH=1
         CALL MRRAYS
         RETURN
      END IF
      IF(WC.EQ.'MFOBS')  THEN
         CALL MFFOBS
         RETURN
      END IF
      IF(WC.EQ.'MTRACE')  THEN
         MSG=.FALSE.
         CALL MTRACER
!     DONE, MSG BACK ON
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'MTRACEI1') THEN
         MSG=.FALSE.
!       SHUT OFF DIFFERENTIAL TRACING
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFFOB OFF'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFRAY OFF'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         CALL MTRACERI_GRID1
!       TURN ON DIFFERENTIAL TRACING
!     DONE, MSG BACK ON
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFFOB ON'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFRAY ON'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'MTRACEI2') THEN
         MSG=.FALSE.
!       SHUT OFF DIFFERENTIAL TRACING
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFFOB OFF'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFRAY OFF'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         CALL MTRACERI_GRID2
!       TURN ON DIFFERENTIAL TRACING
!     DONE, MSG BACK ON
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFFOB ON'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='DIFRAY ON'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'INTEN')  THEN
!       RHIST_INTENSITY CALCULATION FROM SHORT RAYHIST.DAT
         CALL RHIST_INTENSITY
         RETURN
      END IF
      IF(WC.EQ.'IRAY')  THEN
         IF(WQ.EQ.'CAOB') CACOCH=1
         OLDIF=LDIF
         OLDIF2=LDIF2
         LDIF=.FALSE.
         LDIF2=.TRUE.
         OLDREF=NEWREF
         NEWREF=1
         OLDSTOP=INT(sys_astop())
         OLDSTOP2=INT(sys_astop_adj())
         call sys_set_ref_surf(1.0D0)
         call sys_set_astop(1.0D0)
         call sys_set_astop_adj(0.0D0)
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
         NEWREF=OLDREF
         call sys_set_ref_surf(DBLE(NEWREF))
         call sys_set_astop(DBLE(OLDSTOP))
         call sys_set_astop_adj(DBLE(OLDSTOP2))
         LDIF=OLDIF
         LDIF2=OLDIF2
         RETURN
      END IF
      IF(WC.EQ.'IRAYA')  THEN
         OS62=sys_ray_aiming()
         OS63=sys_telecentric()
         call sys_set_ray_aiming(0.0D0)
         call sys_set_telecentric(0.0D0)
         OLDIF=LDIF
         OLDIF2=LDIF2
         LDIF=.FALSE.
         LDIF2=.TRUE.
         OLDREF=NEWREF
         NEWREF=1
         OLDSTOP=INT(sys_astop())
         OLDSTOP2=INT(sys_astop_adj())
         call sys_set_ref_surf(1.0D0)
         call sys_set_astop(1.0D0)
         call sys_set_astop_adj(0.0D0)
         ITRACE=.TRUE.
!     IF VIE, MSG OFF
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         CALL IRAY
         LDIF=OLDIF
         LDIF2=OLDIF2
!     DONE, MSG BACK ON
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         NEWREF=OLDREF
         call sys_set_ref_surf(DBLE(NEWREF))
         call sys_set_astop(DBLE(OLDSTOP))
         call sys_set_astop_adj(DBLE(OLDSTOP2))
         LDIF=OLDIF
         LDIF2=OLDIF2
         call sys_set_ray_aiming(OS62)
         call sys_set_telecentric(OS63)
         ITRACE=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'FOBA') THEN
!     FOBA CREATES THE INPUT FOR A REGULAR FOB COMMAND
!     AND IF SUCCESSFUL, IT EXECUTES THE FOB, ELSE IT DOES NOT
         CALL FOBA
         NEWOBJ=0
         NEWREF=INT(sys_ref_surf())
         NEWIMG=INT(sys_last_surf())
         RETURN
      END IF
      IF(WC.EQ.'FOB'.OR.WC.EQ.'FOBH')  THEN
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
         RETURN
      END IF
      IF(WC.EQ.'IFOB')  THEN
         OLDIF=LDIF
         OLDIF2=LDIF2
         LDIF=.FALSE.
         LDIF2=.TRUE.
         OLDREF=NEWREF
         NEWREF=1
         OLDSTOP=INT(sys_astop())
         OLDSTOP2=INT(sys_astop_adj())
         call sys_set_ref_surf(1.0D0)
         call sys_set_astop(1.0D0)
         call sys_set_astop_adj(0.0D0)
!     NO LINE SPREAD FUNCTIONS MAY EXIST NOW
         LSF=.FALSE.
!     NO SPOTS EXIST
         SPDEXT=.FALSE.
         GSPDEXT=.FALSE.
         CPFNEXT=.FALSE.
         CALL DELPSF
!     IF VIE, MSG OFF
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         CALL FFOB
         IF(is_command_query()) RETURN
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         NEWREF=OLDREF
         call sys_set_ref_surf(DBLE(NEWREF))
         call sys_set_astop(DBLE(OLDSTOP))
         call sys_set_astop_adj(DBLE(OLDSTOP2))
         LDIF=OLDIF
         LDIF2=OLDIF2
         RETURN
      END IF
      IF(WC.EQ.'VB'.OR.WC.EQ.'VBA')  THEN
         OPTMES=.FALSE.
         CALL VARBLL
         OPTMES=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'TVB')  THEN
         IF(F12.NE.1) THEN
            OUTLYNE=&
            &'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'THE "TVB" COMMAND MAY BE ISSUED'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NO ACTION TAKEN'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         OPTMES=.FALSE.
         CALL TVARBLL
         OPTMES=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'COMPS')  THEN
         IF(F12.NE.1) THEN
            OUTLYNE=&
            &'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'THE "COMPS" COMMAND MAY BE ISSUED'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NO ACTION TAKEN'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         OPTMES=.FALSE.
         CALL CVARBLL
         OPTMES=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'MR'.OR.WC.EQ.'MRA'&
      &.OR.WC.EQ.'OP'.OR.WC.EQ.'OPA')  THEN
!       DO A FORCED RETURN TO CFG1
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL FRCCF1(1)
         OPTMES=.FALSE.
         WQ='CFG' ! Hack
         CALL MAROUT
         OPTMES=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'TOPS') THEN
         OPTMES=.FALSE.
         CALL TOPOUT
         OPTMES=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'CRITS') THEN
         OPTMES=.FALSE.
         CALL CRITOUT
         OPTMES=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'AIMRAY'.OR.WC.EQ.'RAYAIM')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL NOAIMM
         RETURN
      END IF
      IF(WC.EQ.'FLIPREFX'.OR.WC.EQ.'FLIPREFY')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL FLIPREF
         RETURN
      END IF
      IF(WC.EQ.'SCREEN')  THEN
         IF(SQ.EQ.0) WQ='ON'
         IF(SQ.EQ.0) SQ=1
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL SCREENIT
         RETURN
      END IF
      IF(WC.EQ.'RHIST')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL RHISTORY
         RETURN
      END IF
      IF(WC.EQ.'FRAME')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL FFRAME
         RETURN
      END IF
      IF(WC.EQ.'NEUTFILE')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         IF(WQ.EQ.'ON') NEUTFILE=.TRUE.
         IF(WQ.EQ.'OFF') NEUTFILE=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'MACFAIL')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL MACFAIL
         RETURN
      END IF
      IF(WC.EQ.'USEOLREF')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL OLDREFDAT
         RETURN
      END IF
      IF(WC.EQ.'SAVEREF')  THEN
         CALL SAVEREFDATA
         RETURN
      END IF
      IF(WC.EQ.'SAVERAY')  THEN
         CALL SAVE_RAY_DATA
         RETURN
      END IF
      IF(WC.EQ.'RESTRAY')  THEN
         CALL REST_RAY_DATA
         RETURN
      END IF
      IF(WC.EQ.'CLEARRAY')  THEN
         CALL CLEAR_RAY_DATA
         RETURN
      END IF
      IF(WC.EQ.'CARTMAN')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO')  WQ='OFF'
         IF(WQ.EQ.'ON')  CARTMAN=.TRUE.
         IF(WQ.EQ.'OFF') CARTMAN=.FALSE.
         IF(is_command_query().AND.CARTMAN) OUTLYNE='CARTMAN IS TRUE'
         IF(is_command_query().AND..NOT.CARTMAN) OUTLYNE='CARTMAN IS FALSE'
         IF(is_command_query()) CALL SHOWIT(1)
         RETURN
      END IF
      IF(WC.EQ.'REVRAY')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL SET_REVRAY
         RETURN
      END IF
      IF(WC.EQ.'AIMAPL')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL NOAIMAPL
         RETURN
      END IF
      IF(WC.EQ.'ALL')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL ALLDEF
         RETURN
      END IF
      IF(WC.EQ.'NODRAW'.OR.WC.EQ.'YESDRAW')  THEN
         CALL NODRAWW
         RETURN
      END IF
      IF(WC.EQ.'NOWMF'.OR.WC.EQ.'YESWMF')  THEN
         CALL NOWMFF
         RETURN
      END IF
      IF(WC.EQ.'PSFLIN'.OR.WC.EQ.'PSFLOG')  THEN
         CALL PSFLINLOG
         RETURN
      END IF
      IF(WC.EQ.'PSFTAG'.OR.WC.EQ.'PSFLI')  THEN
         CALL PSFTAGG
         RETURN
      END IF
      IF(WC.EQ.'CAPFNROT')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL CAPFNROT
         RETURN
      END IF
!       PLTCAPFN/PLTCAPCO (PLOTCAD4) removed
      IF(WC.EQ.'USERCONT') THEN
         OCOLBAC=COLBAC
         COLBAC=0
         OCOLDEF=COLDEF
         COLBAC=0
         COLDEF=14
         IU=0
         CALL USER_CONTOUR(IU)
         REST_KDP(1)=RESTINPT(1)
         COLBAC=OCOLBAC
         COLDEF=OCOLDEF
         RETURN
      END IF
      IF(WC.EQ.'IOBJECT'.OR.WC.EQ.'IOBJECTN'.OR.WC.EQ.'COLOR'&
      &.OR.WC.EQ.'OBJVAL'.OR.WC.EQ.'OTOBMP'.OR.WC.EQ.'ITOBMP'&
      &.OR.WC.EQ.'IIMAGE'.OR.WC.EQ.'IIMAGEN'.OR.WC.EQ.'IMTRACE1'&
      &.OR.WC.EQ.'IMTRACE2'.OR.WC.EQ.'IMTRACE3'.OR.WC.EQ.'BUILDIMG'&
      &.OR.WC.EQ.'PLTOBJ'.OR.WC.EQ.'OBJVAL'.OR.WC.EQ.'OFROMBMP'&
      &.OR.WC.EQ.'IFROMBMP'.OR.WC.EQ.'PLTIMG'.OR.WC.EQ.'BMPREADR'&
      &.OR.WC.EQ.'PSFTOIMG'.OR.WC.EQ.'IIMAGED'.OR.WC.EQ.'IMGSLICE'&
      &.OR.WC.EQ.'LMINUSR'.OR.WC.EQ.'IOBJECTD'.OR.WC.EQ.'IMTRACE4'&
      &.OR.WC.EQ.'IMTRACE5'.OR.WC.EQ.'INTTOPSF') THEN
! JRN comment out since FULLIMAGING needs WINTERACTER
         !CALL FULLIMAGING
         RETURN
      END IF
      IF(WC.EQ.'PLOT'.AND.WQ.EQ.'SAGFILE') THEN
         PLOTCAPCON=.FALSE.
         CALL PLT_SAGFILE
         RETURN
      END IF
      IF(WC.EQ.'SAGFLROT')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL SSAGFLROT
         RETURN
      END IF
      IF(WC.EQ.'RMSMAP'.OR.WC.EQ.'PTVMAP'.OR.WC.EQ.'STRLMAP') THEN
         CALL MAPFIELDOPD
         RETURN
      END IF
      IF(WC.EQ.'SAGFLROT')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL SSAGFLROT
         RETURN
      END IF
      IF(WC.EQ.'CAPFNTAG'.OR.WC.EQ.'CAPFNLI')  THEN
         CALL CAPFNTAGG
         RETURN
      END IF
      IF(WC.EQ.'TEL')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL TELAIM
         RETURN
      END IF
      IF(WC.EQ.'GEOLEICA')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL GEOLEICA
         RETURN
      END IF
      IF(WC.EQ.'DIFLEICA')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL DIFLEICA
         RETURN
      END IF
      IF(WC.EQ.'OVERBOSE')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL OVERBOSE
         RETURN
      END IF
      IF(WC.EQ.'OPTMINIT')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL OPTMINIT
         RETURN
      END IF
      IF(WC.EQ.'NEAR'.OR.WC.EQ.'FAR')  THEN
         CALL NEARFARNEAR
         RETURN
      END IF
      IF(WC.EQ.'LENSREST')  THEN
         CALL LENSREST
         RETURN
      END IF
      IF(WC.EQ.'LSAVE')  THEN
         CALL LENSSAVE_NOOPT
         RETURN
      END IF
      IF(WC.EQ.'LENSSAVE')  THEN
         CALL LENSSAVE
         RETURN
      END IF
      IF(WC.EQ.'LENSLOC')  THEN
         CALL LENSLOC
         RETURN
      END IF
      IF(WC.EQ.'LENSDIR')  THEN
         CALL LENSDIR
         RETURN
      END IF
      IF(WC.EQ.'DIFRAY')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL LLDDFF
         RETURN
      END IF
      IF(WC.EQ.'LINK'.OR.WC.EQ.'LINKD'.OR.WC.EQ.'TLINK') THEN
         IF(WC.EQ.'LINK'.OR.WC.EQ.'LINKD') THEN
            LERROR=.FALSE.
            CALL LINKIT(LERROR)
            RETURN
         END IF
         IF(WC.EQ.'TLINK') THEN
            LERROR=.FALSE.
            SAVE_KDP(3)=SAVEINPT(3)
            WC='LINK'
            CALL LINKIT(LERROR)
            SAVE_KDP(3)=SAVEINPT(3)
            IF(.NOT.LERROR) THEN
               SAVE_KDP(3)=SAVEINPT(3)
               WC='LINKD'
               S2=0
               S3=0
               S4=0
               S5=0
               SN=1
               DF2=1
               DF3=1
               DF4=1
               DF5=1
               W2=0.0D0
               W3=0.0D0
               W4=0.0D0
               W5=0.0D0
               CALL LINKIT(LERROR)
               REST_KDP(3)=RESTINPT(3)
            END IF
            RETURN
         END IF
      END IF
      IF(WC.EQ.'DIFFOB')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL LLDDF2
         RETURN
      END IF
      IF(WC.EQ.'OPDIF')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL OOPDIF
         RETURN
      END IF
      IF(WC.EQ.'VIRTRAY')  THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL LLDDF3
         RETURN
      END IF
      IF(WC.EQ.'XFAN'.OR.WC.EQ.'YFAN'.OR.WC.EQ.&
      &'NFAN'.OR.WC.EQ.'PFAN')  THEN
         CACOCH=1
!       SET SO NO DIFFERENTIAL RAYS ARE TRACED
!       AND NO INDEPENDENT ERROR MESSAAGES ARE PRINTED
         GRASET=.FALSE.
         OLDIF=LDIF
         LDIF=.FALSE.
         MSG=.FALSE.
         CALL FANS
         LDIF=OLDIF
         MSG=.TRUE.
         CACOCH=0
         RETURN
      END IF
      IF(WC.EQ.'VERTS') THEN
         X1=GPREG(1)
         Y1=GPREG(2)
         Z1=GPREG(3)
         XL1=GPREG(4)
         XM1=GPREG(5)
         XN1=GPREG(6)
         YL1=GPREG(7)
         YM1=GPREG(8)
         YN1=GPREG(9)
         ZL1=GPREG(10)
         ZM1=GPREG(11)
         ZN1=GPREG(12)
         X2=GPREG(13)
         Y2=GPREG(14)
         Z2=GPREG(15)
         XL2=GPREG(16)
         XM2=GPREG(17)
         XN2=GPREG(18)
         YL2=GPREG(19)
         YM2=GPREG(20)
         YN2=GPREG(21)
         ZL2=GPREG(22)
         ZM2=GPREG(23)
         ZN2=GPREG(24)
         CALL VERT123(&
         &X1,Y1,Z1,XL1,XM1,XN1,YL1,YM1,YN1,ZL1,ZM1,ZN1,&
         &X2,Y2,Z2,XL2,XM2,XN2,YL2,YM2,YN2,ZL2,ZM2,ZN2,&
         &X3,Y3,Z3,XL3,XM3,XN3,YL3,YM3,YN3,ZL3,ZM3,ZN3,&
         &ALPHA,BETA,GAMMA)
         RETURN
      END IF
      IF(WC.EQ.'GLOBAL'.OR.WC.EQ.'OFFSET'.OR.WC.EQ.'VERTEX')&
      &THEN
         IF(WC.EQ.'GLOBAL') GRASET=.FALSE.
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL GLOBAL
         RETURN
      END IF
      IF(WC.EQ.'PRGLOBAL')&
      &THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRGLBL
         RETURN
      END IF
      IF(WC.EQ.'PRXYZ'.OR.WC.EQ.'PRXYI'.OR.&
      &WC.EQ.'PRXYIP') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRRAY
         RETURN
      END IF
      IF(WC.EQ.'PRNSS') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRNSS
         RETURN
      END IF
      IF(WC.EQ.'PRFLUX') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRFLUX
         RETURN
      END IF
      IF(WC.EQ.'PRPOL') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRPOL
         RETURN
      END IF
      IF(WC.EQ.'PRLMN') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRLMN
         RETURN
      END IF
      IF(WC.EQ.'PRX'.OR.WC.EQ.'PRY') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRXY
         RETURN
      END IF
      IF(WC.EQ.'PRZ') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRZ
         RETURN
      END IF
      IF(WC.EQ.'PRR') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRR
         RETURN
      END IF
      IF(WC.EQ.'PRXYD') THEN
         IF(SQ.EQ.0.AND.DF1.EQ.0.AND.W1.LT.0.0D0) W1=sys_last_surf()+W1
         CALL PRXYD
         RETURN
      END IF
      IF(WC.EQ.'VARIABLE'.OR.WC.EQ.'VARI') THEN
         TVBCNT=0
         PFAC=1.0D0
         ISCOMP(1:MAXCMP)=.FALSE.
         CALL VRBL1
         RETURN
      END IF
      IF(WC.EQ.'TVAR') THEN
         IF(F12.NE.1) THEN
            OUTLYNE=&
            &'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'THE "TVAR" COMMAND MAY BE ISSUED'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NO ACTION TAKEN'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         CALL TVRBL1
         RETURN
      END IF
      IF(WC.EQ.'COMPVAR') THEN
         IF(F12.NE.1) THEN
            OUTLYNE=&
            &'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'THE "COMPVAR" COMMAND MAY BE ISSUED'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NO ACTION TAKEN'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         CALL CVRBL1
         RETURN
      END IF
      IF(WC.EQ.'MERIT') THEN
         PFAC=1.0D0
!
         FCCNT=0
         TOPCNT=0
         ISCRIT(1:MAXFOCRIT)=.FALSE.
         ISTOP(1:MAXTOP)=.FALSE.
         CALL MERIT
         JK_CHMODE=.FALSE.
         CORMOD=1
         CURFIG=1
         RETURN
      END IF
      IF(WC.EQ.'TOPER') THEN
         CALL TOPER
         JK_CHMODE=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'FOCRIT') THEN
         CALL FOCRIT
         JK_CHMODE=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'VARIABLE'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'VB'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'VARIABLE'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'VB') THEN
         CALL DEROFF
         CALL UVRBL1
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'TVAR'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'TVB'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'TVAR'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'TVB') THEN
         IF(F12.NE.1) THEN
            OUTLYNE=&
            &'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'THE "UPDATE TVAR" COMMAND MAY BE ISSUED'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NO ACTION TAKEN'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         CALL DEROFF
         CALL TUVRBL1
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'COMPVAR'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'CMP'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'COMPVAR'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'CMP') THEN
         IF(F12.NE.1) THEN
            OUTLYNE=&
            &'CONFIGURATION #1 MUST BE THE CURRENT CONFIGURATION BEFORE'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'THE "UPDATE COMPVAR" COMMAND MAY BE ISSUED'
            CALL SHOWIT(1)
            OUTLYNE=&
            &'NO ACTION TAKEN'
            CALL MACFAL
            RETURN
         END IF
         CALL DEROFF
         CALL UCVRBL1
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'MERIT'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'M'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'MERIT'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'M') THEN
         JK_CHMODE=.FALSE.
         CORMOD=1
         CURFIG=1
         CALL DEROFF
         CALL UMERIT
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'TOPER'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'TOP'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'TOPER'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'TOP') THEN
         CALL DEROFF
         CALL UTOPER
         RETURN
      END IF
      IF(WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'FOCRIT'.OR.&
      &WC.EQ.'UPDATE'.AND.&
      &WQ.EQ.'FC'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'FOCRIT'.OR.&
      &WC.EQ.'U'.AND.&
      &WQ.EQ.'FC') THEN
         JK_CHMODE=.FALSE.
         CALL DEROFF
         CALL UFOCRIT
         RETURN
      END IF
      IF(WC.EQ.'PLOT'.OR.WC.EQ.'PNOTE') THEN
         IF(DEVTYP.NE.1) CALL PLTDEV
         PLOTCAPCON=.FALSE.
         CALL PPLOTT
         RETURN
      END IF
      IF(WC.EQ.'VIEVIG') THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL VIEVIG
         RETURN
      END IF
      IF(WC.EQ.'COATINGS') THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL SETCOAT
         RETURN
      END IF
      IF(WC.EQ.'VIESYM') THEN
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO') WQ='OFF'
         CALL VIESYM
         RETURN
      END IF
      !IF(WC.EQ.'VIE'.OR.WC.EQ.'VIECO') THEN
      IF(WC.EQ.'VIECO') THEN
         F34 = 1
         PRINT *, "ABout to call VIE from CMDER"
         call LogTermFOR("ABout to call VIE from CMDER")
         call PROCESKDP('VIE')
         F34=0
         MSG=.TRUE.
         !CACOCH=0
         !CACOCHVIE=0
         !CACOCHVIE=0
         !active_plot = ID_NEWPLOT_LENSDRAW
         !PRINT *, "ACTIVE PLOT CHANGED DUE TO VIECO CALL!"
         !IF(WC.EQ.'VIECO') CACOCHVIE=1
         !F34=1
         !MSG=.FALSE.
         !CALL VIE(CACOCHVIE)
         !call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)
         !CALL VIE_NEW(CACOCHVIE)
         !call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
         !F34=0
         !MSG=.TRUE.
         !CACOCH=0
         !CACOCHVIE=0
         RETURN
      END IF
      IF(WC.EQ.'ORIENT'.OR.WC.EQ.'NORIENT') THEN
         PLOTCAPCON=.FALSE.
         CALL PPLOTT
         RETURN
      END IF
      IF(WC.EQ.'LISTDRAW') THEN
         !active_plot = ID_NEWPLOT_LENSDRAW
         CALL PPLOTT
         RETURN
      END IF
! WINTER
!        IF(WC.EQ.'REPLAY') THEN
!        CALL REPLAYFILE
!        RETURN
!        END IF
      IF(WC.EQ.'PLOT'.AND.WQ.NE.'DEV'.AND.F17.EQ.1) THEN
         OUTLYNE=&
         &'THIS PLOT COMMAND NOT OPERATIONAL AT THE "SPECT" LEVEL'
         CALL SHOWIT(1)
         OUTLYNE='RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(WC.EQ.'SAG') THEN
         SAGCODE=0
         CALL SSAAGG
         RETURN
      END IF
      IF(WC.EQ.'SPD'.OR.WC.EQ.'FAIL'.OR.WC.EQ.'FAILACC'&
      &.OR.WC.EQ.'SPDADD'.OR.WC.EQ.'SPDSAVE'.OR.WC.EQ.'SPDSTATS') THEN
         GRASET=.FALSE.
         IF(WC.EQ.'SPDSTATS') SPDEXT=.FALSE.
         MSGSPD=.TRUE.
         CALL SPOT
         RETURN
      END IF
      IF(WC.EQ.'ISPD')THEN
         OLDIF=LDIF
         OLDIF2=LDIF2
         LDIF=.FALSE.
         LDIF2=.TRUE.
         OLDREF=NEWREF
         NEWREF=1
         OLDSTOP=INT(sys_astop())
         OLDSTOP2=INT(sys_astop_adj())
         call sys_set_ref_surf(1.0D0)
         call sys_set_astop(1.0D0)
         call sys_set_astop_adj(0.0D0)
         GRASET=.FALSE.
         SPDEXT=.FALSE.
         MSGSPD=.TRUE.
!     IF VIE, MSG OFF
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         CALL SPOT
!     DONE, MSG BACK ON
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         NEWREF=OLDREF
         call sys_set_ref_surf(DBLE(NEWREF))
         call sys_set_astop(DBLE(OLDSTOP))
         call sys_set_astop_adj(DBLE(OLDSTOP2))
         LDIF=OLDIF
         LDIF2=OLDIF2
         RETURN
      END IF
      IF(WC.EQ.'ISPDA')THEN
         GRASET=.FALSE.
         SPDEXT=.FALSE.
         MSGSPD=.TRUE.
         OS62=sys_ray_aiming()
         OS63=sys_telecentric()
         call sys_set_ray_aiming(0.0D0)
         call sys_set_telecentric(0.0D0)
         OLDIF=LDIF
         OLDIF2=LDIF2
         LDIF=.FALSE.
         LDIF2=.TRUE.
         OLDREF=NEWREF
         NEWREF=1
         OLDSTOP=INT(sys_astop())
         OLDSTOP2=INT(sys_astop_adj())
         call sys_set_ref_surf(1.0D0)
         call sys_set_astop(1.0D0)
         call sys_set_astop_adj(0.0D0)
         ITRACE=.TRUE.
!     IF VIE, MSG OFF
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         CALL SPOT
         LDIF=OLDIF
         LDIF2=OLDIF2
!     DONE, MSG BACK ON
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         NEWREF=OLDREF
         call sys_set_ref_surf(DBLE(NEWREF))
         call sys_set_astop(DBLE(OLDSTOP))
         call sys_set_astop_adj(DBLE(OLDSTOP2))
         LDIF=OLDIF
         LDIF2=OLDIF2
         call sys_set_ray_aiming(OS62)
         call sys_set_telecentric(OS63)
         ITRACE=.FALSE.
!     DONE, MSG BACK ON
         IF(F34.EQ.1) MSG=.FALSE.
         IF(F34.EQ.0) MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'RING'.OR.WC.EQ.'RINGS'.OR.WC.EQ.'RANNUM'&
      &.OR.WC.EQ.'SPDRESET'.OR.WC.EQ.'APOD'.OR.&
      &WC.EQ.'SPOT'.OR.WC.EQ.'RECT') THEN
         CALL SPOTSET
         RETURN
      END IF
      IF(WC.EQ.'OPRING'.OR.WC.EQ.'OPRINGS'.OR.WC.EQ.'OPRANNUM'&
      &.OR.WC.EQ.'OPSPDRST'.OR.&
      &WC.EQ.'OPSPOT'.OR.WC.EQ.'OPRECT') THEN
         GRSPT=0
         CALL OPSPOTSET
         RETURN
      END IF
      IF(WC.EQ.'OPNRD'&
      &.OR.WC.EQ.'TOLNRD'.OR.WC.EQ.'NRD'.OR.WC.EQ.'TGR'&
      &.OR.WC.EQ.'PGR'.OR.WC.EQ.'GRI'.OR.WC.EQ.'CAPFNNRD'.OR.&
      &WC.EQ.'EXTENT') THEN
         CALL MERIT2
         RETURN
      END IF
      IF(WC.EQ.'REDSQ'.OR.WC.EQ.'REDSUMSQ'.OR.WC.EQ.'SREDSQ') THEN
         IF(WC.EQ.'REDSUMSQ') THEN
            GRASET=.FALSE.
            CALL SUMSPREDSQ
         ELSE
            CALL SPREDSQ
         END IF
         RETURN
      END IF
      IF(WC.EQ.'DRED') THEN
         ERRORPSF=.FALSE.
         CALL PSFSPOT(ERRORPSF,MMM)
         IF(ERRORPSF) THEN
            OUTLYNE=&
            &'NO DIFFRACTION PSF EXISTS, NO PSF SPOT FILE WAS CREATED'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('NO ACTION TAKEN', 1)
            RETURN
         END IF
         CALL DSPRED(MMM)
         RETURN
      END IF
      IF(WC.EQ.'DREDSQ') THEN
         ERRORPSF=.FALSE.
         CALL PSFSPOT(ERRORPSF,MMM)
         IF(ERRORPSF) THEN
            OUTLYNE=&
            &'NO DIFFRACTION PSF EXISTS, "DREDSQ" CAN NOT PROCEED'
            CALL SHOWIT(1)
            CALL REPORT_ERROR_AND_FAIL('NO ACTION TAKEN', 1)
            RETURN
         END IF
         CALL DSPREDSQ(MMM)
         RETURN
      END IF
      IF(WC.EQ.'PSFSPOT') THEN
         ERRORPSF=.FALSE.
         CALL PSFSPOT(ERRORPSF,MMM)
         RETURN
      END IF
      IF(WC.EQ.'REDK'.OR.WC.EQ.'ESEDX'.OR.WC.EQ.'ESEDY'&
      &.OR.WC.EQ.'ESED'.OR.WC.EQ.'REDSUM'.OR.WC.EQ.'SRED'.OR.&
      &WC.EQ.'SESED'.OR.WC.EQ.'SESEDX'.OR.WC.EQ.'SESEDY') THEN
         IF(WC.EQ.'REDSUM') THEN
            GRASET=.FALSE.
            CALL SUMSPRED
         ELSE
            CALL SPRED
         END IF
         RETURN
      END IF
      IF(WC.EQ.'LSF') THEN
         GRASET=.FALSE.
         CALL LSFLSF
         RETURN
      END IF
      IF(WC.EQ.'OLSF') THEN
         GRASET=.FALSE.
         CALL OLDLSF
         RETURN
      END IF
      IF(WC.EQ.'EDIT') THEN
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='REPLACE'
         CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         IF(HEADLESS_MODE) THEN
            OUTLYNE='Command requires GUI'
            CALL SHOWIT(1)
         ELSE
            CALL EDITOR
         END IF
         RETURN
      END IF
      IF(WC.EQ.'RESTORE') THEN
         GRASET=.FALSE.
         F28=1
         MSG=.FALSE.
         OPTMES=.FALSE.
         CALL RESTOR(.TRUE.)
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'ROBB') THEN
         GRASET=.FALSE.
         F28=1
         MSG=.FALSE.
         OPTMES=.FALSE.
         CALL ROBB
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'ITER'.OR.WC.EQ.'IT') THEN
         OWC=WC
         IF(SQ.EQ.1) OWQ=WQ
         IF(SQ.EQ.0) OWQ='        '
         GRASET=.FALSE.
         F28=1
         KILOPT=.FALSE.
         MSG=.FALSE.
         OPTMES=.FALSE.
         IF(SQ.EQ.0.OR.WQ.EQ.'POWL'.OR.WQ.EQ.'P'.OR.WQ.EQ.'FULL'&
         &.OR.WQ.EQ.'F'.OR.WQ.EQ.'DIR'.OR.WQ.EQ.'D'.OR.WQ.EQ.'POWELL') THEN
            ITCT=INT(W1)
            IF(ITCT.EQ.0) ITCT=1
            DO ITNUM=1,ITCT
               DEREXT=.FALSE.
               WC=OWC
               WQ=OWQ
               SQ=0
               IF(WQ.NE.'        ') SQ=1
               DF1=1
               S1=0
               SN=0
               ITERROR=.FALSE.
               CALL ITER(1,1,ITERROR)
               IF(ITERROR) THEN
                  ITERROR=.FALSE.
                  EXIT
               END IF
               IF(sys_verbose_optim().NE.0.0D0) THEN
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='VB'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='VBA'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='OP'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='OPA'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  INPUT='OPRD'
                  CALL PROCES
                  REST_KDP(1)=RESTINPT(1)
               END IF
               IF(F28.EQ.0.OR.KILOPT) GO TO 8769
            END DO
8769        CONTINUE
         ELSE
            WC=OWC
            WQ=OWQ
            SQ=0
            IF(WQ.NE.'        ') SQ=1
            CALL ITER(1,1,ITERROR)
            IF(ITERROR) THEN
               ITERROR=.FALSE.
            END IF
            IF(sys_verbose_optim().NE.0.0D0) THEN
               SAVE_KDP(1)=SAVEINPT(1)
               INPUT='VB'
               CALL PROCES
               REST_KDP(1)=RESTINPT(1)
               SAVE_KDP(1)=SAVEINPT(1)
               INPUT='VBA'
               CALL PROCES
               REST_KDP(1)=RESTINPT(1)
               SAVE_KDP(1)=SAVEINPT(1)
               INPUT='OP'
               CALL PROCES
               REST_KDP(1)=RESTINPT(1)
               SAVE_KDP(1)=SAVEINPT(1)
               INPUT='OPA'
               CALL PROCES
               REST_KDP(1)=RESTINPT(1)
               SAVE_KDP(1)=SAVEINPT(1)
               INPUT='OPRD'
               CALL PROCES
               REST_KDP(1)=RESTINPT(1)
            END IF
         END IF
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'RSV') THEN
!       RESOLVE THE EXISTING MATRIX BUT FIRST DO A RESTORE
         GRASET=.FALSE.
         F28=1
         KILOPT=.FALSE.
         MSG=.FALSE.
         OPTMES=.FALSE.
         WC='RSV'
         SQ=0
         CALL ITER(1,1,ITERROR)
         IF(ITERROR) THEN
            ITERROR=.FALSE.
         END IF
         IF(sys_verbose_optim().NE.0.0D0) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='VB'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='VBA'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='OP'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='OPA'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='OPRD'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
         END IF
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'SV') THEN
!       RESOLVE THE EXISTING MATRIX WITHOUT FIRST DOING A RESTORE
         GRASET=.FALSE.
         F28=1
         KILOPT=.FALSE.
         MSG=.FALSE.
         OPTMES=.FALSE.
         WC='SV'
         SQ=0
         CALL ITER(1,1,ITERROR)
         IF(ITERROR) THEN
            ITERROR=.FALSE.
         END IF
         IF(sys_verbose_optim().NE.0.0D0) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='VB'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='VBA'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='OP'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='OPA'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='OPRD'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
         END IF
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'OPRD') THEN
         IF(RAYCLEAR) THEN
            RAYEXT=.FALSE.
            POLEXT=.FALSE.
            REFEXT=.FALSE.
         END IF
!       DO A FORCED RETURN TO CFG1
         GRIDSUNLOADED19(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED20(0:MAXSUR)=.TRUE.
         GRIDSUNLOADED22(0:MAXSUR)=.TRUE.
         CALL FRCCF1(1)
         GRASET=.FALSE.
         F28=1
         KILOPT=.FALSE.
         MSG=.FALSE.
         OPTMES=.FALSE.
         CALL OPRD
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'FMT') THEN
         GRASET=.FALSE.
         F28=1
         MSG=.FALSE.
         OPTMES=.FALSE.
         CALL FMT
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'J1') THEN
         CALL BESS
         RETURN
      END IF
      IF(WC.EQ.'GOTF') THEN
         GRASET=.FALSE.
         IF(LASTWASFOB) THEN
            MULTIOTF=.FALSE.
            GRASET=.FALSE.
            OTFPAIR=1
            SAVE_KDP(1)=SAVEINPT(1)
            OLDLDIF2=LDIF2
            OLDLDIF=LDIF
            LDIF2=.TRUE.
            LDIF=.TRUE.
            WQ='        '
            SQ=0
            SST=0
            STI=0
            DF1=1
            DF2=1
            DF3=1
            DF4=1
            DF5=1
            S1=0
            S2=0
            S3=0
            S4=0
            S5=0
            SN=0
!     SET MSG TO FALSE
            MSG=.FALSE.
            MSGSPD=.FALSE.
            ERROR=0
            WC='SPD     '
            CALL SPOT
            IF(.NOT.SPDEXT) EXTGMTF1=.FALSE.
            IF(.NOT.SPDEXT) EXTGMTF2=.FALSE.
            IF(.NOT.SPDEXT) RETURN
            REST_KDP(1)=RESTINPT(1)
            IF(SQ.EQ.0) THEN
               IIG=2
               CALL GOTF
               IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF1=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF2=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.SPDEXT) RETURN
            ELSE
               CALL GOTF
               IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF1=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.SPDEXT) EXTGMTF2=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.SPDEXT) RETURN
               IIG=1
            END IF
            RETURN
         ELSE
!     DO MULTIPLE GOTF FOV'S
            IF(CFLDCNT.EQ.0) THEN
               OUTLYNE=&
               &'MULTIPLE FIELDS OF VIEW ARE NOT DEFINED'
               CALL SHOWIT(1)
               OUTLYNE=&
               &'NO ACTION TAKEN'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            MULTIOTF=.TRUE.
            IIG=2
            IF(SQ.EQ.0) THEN
               IF(CFLDCNT.GT.0) THEN
                  GRASET=.FALSE.
                  OTFPAIR=0
                  DO I=1,CFLDCNT
                     OTFPAIR=OTFPAIR+1
                     SAVE_KDP(1)=SAVEINPT(1)
                     OLDLDIF2=LDIF2
                     OLDLDIF=LDIF
                     LDIF2=.TRUE.
                     LDIF=.TRUE.
                     WQ='P       '
                     SQ=1
                     SST=0
                     STI=0
                     W1=CFLDS(2,I)
                     W2=CFLDS(1,I)
                     DF1=0
                     DF2=0
                     DF3=1
                     DF4=1
                     DF5=1
                     S1=1
                     S2=1
                     S3=0
                     S4=0
                     S5=0
                     SN=1
!     SET MSG TO FALSE
                     MSG=.FALSE.
                     ERROR=0
                     WC='FOB     '
                     CALL FFOB
                     IF(.NOT.REFEXT) EXTGMTF1=.FALSE.
                     IF(.NOT.REFEXT) EXTGMTF2=.FALSE.
                     IF(.NOT.REFEXT) RETURN
                     FLDCODE(1,I)=XPFOB
                     FLDCODE(2,I)=YPFOB
                     FLDUNIT(I)=LUNI
                     REST_KDP(1)=RESTINPT(1)
                     SAVE_KDP(1)=SAVEINPT(1)
                     OLDLDIF2=LDIF2
                     OLDLDIF=LDIF
                     LDIF2=.TRUE.
                     LDIF=.TRUE.
                     WQ='        '
                     SQ=0
                     SST=0
                     STI=0
                     DF1=1
                     DF2=1
                     DF3=1
                     DF4=1
                     DF5=1
                     S1=0
                     S2=0
                     S3=0
                     S4=0
                     S5=0
                     SN=0
!     SET MSG TO FALSE
                     MSG=.FALSE.
                     MSGSPD=.FALSE.
                     ERROR=0
                     WC='SPD     '
                     CALL SPOT
                     IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF1=.FALSE.
                     IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF2=.FALSE.
                     IF(.NOT.SPDEXT.OR..NOT.REFEXT) RETURN
                     REST_KDP(1)=RESTINPT(1)
                     CALL GOTF
                     IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF1=.FALSE.
                     IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF2=.FALSE.
                     IF(.NOT.SPDEXT.OR..NOT.REFEXT) RETURN
                  END DO
                  RETURN
               END IF
            ELSE
!     SQ=1
            END IF
            OTFPAIR=1
            CALL GOTF
            IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF1=.FALSE.
            IF(.NOT.SPDEXT.OR..NOT.REFEXT) EXTGMTF2=.FALSE.
            IF(.NOT.SPDEXT.OR..NOT.REFEXT) RETURN
         END IF
         RETURN
      END IF
      IF(WC.EQ.'DOTF') THEN
         IF(LASTWASFOB.OR.CPFNEXT) THEN
            MULTIOTF=.FALSE.
            GRASET=.FALSE.
            OTFPAIR=0
            IF(SQ.EQ.0) THEN
               IIG=2
               CALL DOTF
               IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
            ELSE
               IIG=1
               CALL DOTF
               IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
               IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
            END IF
            CPFNEXT=.FALSE.
            LASTWASFOB=.FALSE.
            RETURN
         ELSE
            IF(CFLDCNT.EQ.0) THEN
               OUTLYNE=&
               &'MULTIPLE FIELDS OF VIEW ARE NOT DEFINED'
               CALL SHOWIT(1)
               OUTLYNE=&
               &'NO ACTION TAKEN'
               CALL SHOWIT(1)
               CALL MACFAL
               RETURN
            END IF
            MULTIOTF=.TRUE.
!     DO MULTIPLE DOTF FOV'S
            IIG=2
            IF(SQ.EQ.0) THEN
               IF(REFLOC.EQ.4) THEN
                  OUTLYNE='"RSPH" TEMPORARILY CHANGED FROM "BEST" TO "NOTILT"'
                  CALL SHOWIT(1)
                  OREFLOC=4
                  REFLOC=3
               END IF
               IF(CFLDCNT.GT.0) THEN
                  GRASET=.FALSE.
                  OTFPAIR=0
                  SAVE_KDP(1)=SAVEINPT(1)
                  OLDLDIF2=LDIF2
                  OLDLDIF=LDIF
                  LDIF2=.TRUE.
                  LDIF=.TRUE.
                  WQ='P       '
                  SQ=1
                  SST=0
                  STI=0
                  W1=0.0D0
                  W2=0.0D0
                  DF1=0
                  DF2=0
                  DF3=1
                  DF4=1
                  DF5=1
                  S1=1
                  S2=1
                  S3=0
                  S4=0
                  S5=0
                  SN=1
!     SET MSG TO FALSE
                  MSG=.FALSE.
                  ERROR=0
                  WC='FOBA    '
                  CALL FOBA
                  IF(.NOT.REFEXT) EXTDMTF1=.FALSE.
                  IF(.NOT.REFEXT) EXTDMTF2=.FALSE.
                  IF(.NOT.REFEXT) RETURN
                  FLDCODE(1,OTFPAIR)=XPFOB
                  FLDCODE(2,OTFPAIR)=YPFOB
                  FLDUNIT(OTFPAIR)=LUNI
                  REST_KDP(1)=RESTINPT(1)
                  SAVE_KDP(1)=SAVEINPT(1)
                  WC='CAPFN   '
                  WQ='PERFECT '
                  DF1=0
                  DF2=1
                  DF3=1
                  DF4=1
                  DF5=1
                  S1=1
                  S2=0
                  S3=0
                  S4=0
                  S5=0
                  SN=1
                  W1=CAPDEF
                  REFERR=.FALSE.
                  NRDFACTOR=1.0D0
                  PERFECT=.TRUE.
                  CALL COMPAP(REFERR,1)
                  PERFECT=.FALSE.
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                  REST_KDP(1)=RESTINPT(1)
                  CALL DOTF
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                  IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                  CPFNEXT=.FALSE.
                  DO I=1,CFLDCNT
                     OTFPAIR=OTFPAIR+1
                     SAVE_KDP(1)=SAVEINPT(1)
                     OLDLDIF2=LDIF2
                     OLDLDIF=LDIF
                     LDIF2=.TRUE.
                     LDIF=.TRUE.
                     WQ='P       '
                     SQ=1
                     SST=0
                     STI=0
                     W1=CFLDS(2,I)
                     W2=CFLDS(1,I)
                     DF1=0
                     DF2=0
                     DF3=1
                     DF4=1
                     DF5=1
                     S1=1
                     S2=1
                     S3=0
                     S4=0
                     S5=0
                     SN=1
!     SET MSG TO FALSE
                     MSG=.FALSE.
                     ERROR=0
                     WC='FOB     '
                     CALL FFOB
                     IF(.NOT.REFEXT) EXTDMTF1=.FALSE.
                     IF(.NOT.REFEXT) EXTDMTF2=.FALSE.
                     IF(.NOT.REFEXT) RETURN
                     FLDCODE(1,OTFPAIR)=XPFOB
                     FLDCODE(2,OTFPAIR)=YPFOB
                     FLDUNIT(OTFPAIR)=LUNI
                     REST_KDP(1)=RESTINPT(1)
                     CALL DOTF
                     IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
                     IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
                     IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
                  END DO
                  IF(REFLOC.EQ.3.AND.OREFLOC.EQ.4) THEN
                     OUTLYNE='"RSPH" RESET FROM "NOTILT" TO "BEST"'
                     CALL SHOWIT(1)
                     REFLOC=3
                  END IF
                  CPFNEXT=.FALSE.
                  LASTWASFOB=.FALSE.
                  RETURN
               END IF
            ELSE
!     SQ=1
            END IF
            OTFPAIR=0
            CALL DOTF
            IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF1=.FALSE.
            IF(.NOT.REFEXT.OR..NOT.CPFNEXT) EXTDMTF2=.FALSE.
            IF(.NOT.REFEXT.OR..NOT.CPFNEXT) RETURN
         END IF
         IF(REFLOC.EQ.3.AND.OREFLOC.EQ.4) THEN
            OUTLYNE='"RSPH" RESET FROM "NOTILT" TO "BEST"'
            CALL SHOWIT(1)
            REFLOC=3
         END IF
         CPFNEXT=.FALSE.
         LASTWASFOB=.FALSE.
         RETURN
      END IF
!
      IF(WC.EQ.'TFDOTF') THEN
         GRASET=.FALSE.
         CALL TFDOTF
         RETURN
      END IF
!       CAPFIX (PLOTCAD4) removed
      IF(WC.EQ.'VAR'.OR.WC.EQ.'APSTREHL'.OR.WC.EQ.'STREHL') THEN
         CALL APSTREHL
         LSF=.FALSE.
         SPDEXT=.FALSE.
         GSPDEXT=.FALSE.
         CPFNEXT=.FALSE.
         LDIF=.TRUE.
         LDIF2=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'DR/FR') THEN
         CALL DLRPFR
         RETURN
      END IF
      IF(WC.EQ.'USERFUNC') THEN
         CALL CALL_USERFUNC
         RETURN
      END IF
      IF(WC.EQ.'USERSUBR') THEN
         CALL CALL_USERSUBR
         RETURN
      END IF
      IF(WC.EQ.'CAPFN') THEN
         GRASET=.FALSE.
         MSGSPD=.TRUE.
         IF(DOSTREHL) MSGSPD=.FALSE.
         CALL SPOT
         RETURN
      END IF
      IF(WC.EQ.'CAPFNIN'.OR.WC.EQ.'CAPFNADD'.OR.WC.EQ.&
      &'CAPFNCLR') THEN
         CALL OPDIN
         RETURN
      END IF
      IF(WC.EQ.'LEPRT'.OR.WC.EQ.'LIS') THEN
         CALL LEPRT
         RETURN
      END IF
      IF(WC.EQ.'THERM') THEN
         IF(WQ.NE.'AIR'.AND.WQ.NE.'OXYGEN'.AND.WQ.NE.'NITROGEN'&
         &.AND.WQ.NE.'ETHANE'.AND.WQ.NE.'METHANE'.AND.WQ.NE.'HYDROGEN'&
         &.AND.WQ.NE.'ARGON'.AND.WQ.NE.'HELIUM'.AND.WQ.NE.'GAS') THEN
            CALL THERM
         ELSE
            CALL THERMGAS
         END IF
         RETURN
      END IF
      IF(WC.EQ.'FOOT'.AND.WQ.NE.'GRID') THEN
         GRASET=.FALSE.
         MSG=.FALSE.
         CALL QRRAY
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'FOOTAREA') THEN
         GRASET=.FALSE.
         MSG=.FALSE.
         CALL FOOTAREA
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'FOOTSANG') THEN
         GRASET=.FALSE.
         MSG=.FALSE.
         CALL FOOTSANG
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'FOOT'.AND.WQ.EQ.'GRID') THEN
         GRASET=.FALSE.
         MSG=.FALSE.
         CALL FTGRID
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'RADUNITS'.OR.WC.EQ.'PLANK'.OR.WC.EQ.'WIEN'&
      &.OR.WC.EQ.'STEFBOLT') THEN
         CALL SCHWARTZ
         RETURN
      END IF
      IF(WC.EQ.'K0'.OR.WC.EQ.'CVG') THEN
         CALL APOVERT
         RETURN
      END IF
      IF(gdb%isNameInCatalog(trim(WC)).OR.WC.EQ.'GLCAT'.OR.WC.EQ.&
      &'MATL'.OR.WC.EQ.'USER'.OR.WC.EQ.'GLAK'.OR.WC.EQ.'AIR') THEN

         !   IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'OHARA'.OR.WC.EQ.'HOYA'.OR.
         !    1WC.EQ.'CORNIN'.OR.WC.EQ.'CHANCE'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.
         !    2'MATL'.OR.WC.EQ.'RADHARD'.OR.WC.EQ.'USER'.OR.WC.EQ.'HIKARI'
         !    3.OR.WC.EQ.'RUSSIAN'.OR.WC.EQ.'GLAK'.OR.WC.EQ.'AIR'.OR.WC.EQ.
         !   4'SCH2000') THEN
         IF(WS(1:1).EQ.':') WS(1:80)=WS(2:80)
         PRINT *, "Calling GLSRIN"
         CALL GLSRIN
         RETURN
      END IF
      IF(WC.EQ.'MAKEAUTO'.OR.WC.EQ.'DFTYPE'.OR.WC.EQ.'DFDEL'&
      &.OR.WC.EQ.'FP'.OR.WC.EQ.'DFP'.OR.WC.EQ.'MONO'.OR.&
      &WC.EQ.'POLY'.OR.WC.EQ.'DFGRID'.OR.WC.EQ.'DFHEX') THEN
         CALL MAKE_DEF_AUTO
         RETURN
      END IF
      IF(WC.EQ.'LENDIR'.OR.WC.EQ.'MACDIR'.OR.WC.EQ.'TRADIR'&
      &.OR.WC.EQ.'PLTDIR'.OR.WC.EQ.'CHGMAC') THEN
         CALL CHADIR
         RETURN
      END IF
      IF(WC.EQ.'PFIND') THEN
         GRASET=.FALSE.
         F28=1
         KILOPT=.FALSE.
         MSG=.FALSE.
         OPTMES=.FALSE.
         CALL PFIND
         OPTMES=.TRUE.
         F28=0
         MSG=.TRUE.
         RETURN
      END IF
      IF(WC.EQ.'MFG') THEN
         WRITE(OUTLYNE,6661) MFG(1:20)
         CALL SHOWIT(1)
         RETURN
      END IF
      IF(WC.EQ.'CATNUM') THEN
         WRITE(OUTLYNE,6662) CATNUM(1:20)
         CALL SHOWIT(1)
         RETURN
      END IF
6661  FORMAT('THE CURRENT MFG IS: ',A20)
6662  FORMAT('THE CURRENT CATNUM IS: ',A20)
      IF(WC.EQ.'AST'.OR.WC.EQ.'DIST'.OR.WC.EQ.'FLDCV'&
      &.OR.WC.EQ.'FISHDIST') THEN
         CALL FIELDABS
         RETURN
      END IF
      IF(WC.EQ.'PLTGOTF')  THEN
         CALL PLTGOTF
         RETURN
      END IF
      IF(WC.EQ.'PLTDOTF')  THEN
         CALL PLTDOTF
         RETURN
      END IF
      IF(WC.EQ.'PLTLSF')  THEN
         CALL PLTLSF
         RETURN
      END IF
!       PLTCHRSH (PLOTCAD4) removed
!       PLTRED/PLTESED (PLOTCAD2) removed
      IF(WC.EQ.'GPXTX'.OR.WC.EQ.'GPXTY') THEN
         CALL GPXT
         RETURN
      END IF
      IF(WC.EQ.'BEA'.OR.WC.EQ.'BEAM') THEN
         CALL GBEAM
         RETURN
      END IF
      IF(WC.EQ.'RAYS'.OR.WC.EQ.'FIELDS') THEN
         CALL QRRYFL
         RETURN
      END IF
      IF(WC.EQ.'FLDS') THEN
         CALL FLDS
         ! This is hopefully a band aid.  If I don't do this, during init the loaded lens
         ! can get out of sync with sysConfig in some situations.
         ! once I clean up the loading lens part to update sysConfig at the proper end
         ! this will no longer be needed
         call sysConfig%setNumFields(CFLDCNT)
         RETURN
      END IF
!     MACDMP/TOLDMP removed with binary macro system
      IF(WC.EQ.'FOCI') THEN
!       CALCULATES FOCI POSITIONS FOR CONICS
         CALL FOCII
         RETURN
      END IF
      IF(WC.EQ.'RTOD') THEN
!       CALCULATES R TO D CONVERSION FOR GRAZING SUPPORT
         CALL RTOD
         RETURN
      END IF
      IF(WC.EQ.'DTOR') THEN
!       CALCULATES D TO R CONVERSION FOR GRAZING SUPPORT
         CALL DTOR
         RETURN
      END IF
      IF(WC.EQ.'ETOCC') THEN
!       CALCULATES E TO CC CONVERSION FOR GRAZING SUPPORT
         CALL ETOCC
         RETURN
      END IF
      IF(WC.EQ.'CCTOE') THEN
!       CALCULATES CC TO E CONVERSION FOR GRAZING SUPPORT
         CALL CCTOE
         RETURN
      END IF
      IF(WC.EQ.'RHO') THEN
!       CALCULATES RHO POSITIONS FOR CONICS
         CALL RRHHOO
         RETURN
      END IF
      IF(WC.EQ.'STAMPD'.OR.WC.EQ.'STAMPT') THEN
!       SETS TIME AND DATE STAMPING FOR LI
         CALL STAMPER
         RETURN
      END IF
      IF(WC.EQ.'DINCR') THEN
!       SETS DEFAULT DINCRS FOR CLASSES OF VARIABLES
         CALL DINCIT
         RETURN
      END IF
      IF(WC.EQ.'NEWSEED') THEN
!       RESETS THE RANDOM NUMBER SEED
         CALL NEWSEED
         RETURN
      END IF
      IF(WC.EQ.'Z0') THEN
!       CALCULATES Z POSITIONS FOR CONICS
         CALL ZZEEOO
         RETURN
      END IF
      IF(WC.EQ.'PROMPT') THEN
!       SETS PROMPT FOR PROMPTED INPUT
         CALL SETPMT
         RETURN
      END IF
      IF(WC.EQ.'PREAD') THEN
!       DOES PROMPTED INPUT
         CALL PREAD
         RETURN
      END IF
      IF(WC.EQ.'CLEARREG') THEN
!       CLEARS GENERAL PURPOSE STORAGE REGISTERS (SETS THEM TO 0.0)
         CALL CLEARREG
         RETURN
      END IF
      IF(WC.EQ.'STOAX') THEN
!       STORE PROMPT READ VALUE INTO AN ALPH STORAGE REGISTER
         CALL STOAX
         RETURN
      END IF
      IF(WC.EQ.'ATON') THEN
!       CONVERTS STRING TO NUMBER
         CALL MACATON
         RETURN
      END IF
      IF(WC.EQ.'ID1')  RETURN
      IF(WC.EQ.'ID2')  RETURN
      IF(WC.EQ.'ID3')  RETURN
      IF(WC.EQ.'ID4')  RETURN
      IF(WC.EQ.'ID5')  RETURN
      IF(WC.EQ.'ID6')  RETURN
      IF(WC.EQ.'ID7')  RETURN
      IF(WC.EQ.'ID8')  RETURN
      IF(WC.EQ.'ID9')  RETURN
      IF(WC.EQ.'ID10') RETURN
      IF(WC.EQ.'IDTAG') THEN
!       CREATES THE LENS IDTAG
         IF(IN.NE.5) CALL DOGTAG
         RETURN
      END IF
      IF(WC.EQ.'APPEND'.OR.WC.EQ.'REPLACE') THEN
!       SETS APPEND/REPLACE FOR EDITTEXT, CARDTEXT AND PUNCHFILE.DAT
         CALL APPREP
         RETURN
      END IF
      IF(WC.EQ.'LIMRAYS') THEN
!       CALCULATES YZ AND XZ PLANE RAY EXTENTS
         CALL DEROFF
         CALL SIZES
         RETURN
      END IF
      IF(WC.EQ.'BLKRAYS') THEN
!       CALCULATES YZ AND XZ PLANE RAY BLOCKAGES
         CALL DEROFF
         CALL SIZES2
         REFEXT=.FALSE.
         RETURN
      END IF
      IF(WC.EQ.'PSFK') THEN
         GRASET=.FALSE.
         MSGSPD=.TRUE.
         IF(DOSTREHL) MSGSPD=.FALSE.
         CALL SPOT
         RETURN
      END IF
      IF(WC.EQ.'PUPIL') THEN
         GRASET=.FALSE.
         MSGSPD=.TRUE.
         IF(DOSTREHL) MSGSPD=.FALSE.
         CALL SPOT
         RETURN
      END IF
      IF(WC.EQ.'PIXEL'.OR.WC.EQ.'CENTROID') THEN
         CALL PIXAR
         RETURN
      END IF
      IF(WC.EQ.'STREAK') THEN
         IF(WS(1:3).EQ.'YES') WS(1:3)='ON '
         IF(WS(1:2).EQ.'NO')  WQ(1:3)='OFF'
         CALL STREAK
         RETURN
      END IF
      IF(WC.EQ.'PSFINT'.OR.WC.EQ.'PSFINTS') THEN
         CALL PSFINT
         RETURN
      END IF
      IF(WC.EQ.'GAUSS'.OR.WC.EQ.'NOGAUSS'.OR.&
      &WC.EQ.'OPWTA'.OR.WC.EQ.'OPWTAIM') THEN
         RETURN
      END IF
      IF(WC.EQ.'SETCLAP') THEN
!       CALCULATES YZ AND XZ PLANE RAY EXTENTS AND SETS CLAPS
         CALL DEROFF
         CALL SETCLAP
         RETURN
      END IF
      IF(WC.EQ.'BWRTSPOT') THEN
!       WRITES THE SPOT DIAGRAM TO THE FILE NAMED BY THE
!       QUALIFIER WORD.SPD
         CALL BWRTSPOT
         RETURN
      END IF
      IF(WC.EQ.'AWRTSPOT') THEN
!       WRITES THE SPOT DIAGRAM TO THE FILE NAMED BY THE
!       QUALIFIER WORD.SPD
         CALL AWRTSPOT
         RETURN
      END IF
      IF(WC.EQ.'BWRTSUM') THEN
!       WRITES THE SUMMED SPOT DIAGRAM TO THE FILE NAMED BY THE
!       QUALIFIER WORD.SPD
         CALL BWRTSUM
         RETURN
      END IF
      IF(WC.EQ.'AWRTSUM') THEN
!       WRITES THE SUMMED SPOT DIAGRAM TO THE FILE NAMED BY THE
!       QUALIFIER WORD.SPD
         CALL AWRTSUM
         RETURN
      END IF
      IF(WC.EQ.'TOMODEL') THEN
!       CONVERTS A CATALOG GLASS TO A MODEL GLASS
         I=INT(W1)
         CALL MODEL(I)
         RETURN
      END IF
      IF(WC.EQ.'TOMYGLAS') THEN
!       CONVERTS A CATALOG GLASS TO A MYGLASS
         I=INT(W1)
         CALL MYGLASS(I)
         RETURN
      END IF
      IF(WC.EQ.'VIG') THEN
!       TURN VIG SWITCH ON,OFF,YES NO
         IF(WQ.EQ.'YES') WQ='ON'
         IF(WQ.EQ.'NO')  WQ='OFF'
         CALL VIGGER
         RETURN
      END IF
      IF(WC.EQ.'SENSI') THEN
!       INITIATE SENSITIVITY ANALYSIS
         F31=1
         CALL SENSI(0)
         F31=0
         RETURN
      END IF
      IF(WC.EQ.'MXT'.OR.WC.EQ.'MNT'&
      &.OR.WC.EQ.'MPR'.OR.WC.EQ.'MNR') THEN
!       SET MIN AND MAX THICKNESS/RADIUS LIMITS FOR VARIABLES
         CALL THRDLIM
         RETURN
      END IF
      IF(WC.EQ.'INVSENSI') THEN
!       INITIATE INVERSE SENSITIVITY ANALYSIS
         F31=1
         CALL SENSI(1)
         F31=0
         RETURN
      END IF
      IF(WC.EQ.'MONTE') THEN
!       INITIATE MONTE-CARLO ANALYSIS
         F31=1
         CALL MONTE
         F31=0
         RETURN
      END IF
      IF(WC.EQ.'AVEC'.OR.WC.EQ.'BVEC'.OR.WC.EQ.'DOT'&
      &.OR.WC.EQ.'CROSS') THEN
         CALL VECTOROP
         RETURN
      END IF
      IF(WC.EQ.'SHUFFLE') THEN
         IF(W1.LE.0.0D0) W1=20.0D0
         N=INT(W1)
         ALLOCATE (DARR(1:N,1:2),STAT=ALLOERR)
         CALL SHUFFLE(N,DARR)
         DEALLOCATE (DARR)
         RETURN
      END IF
      FIELDER=.FALSE.
      DO II=1,200
         CALL ITOAAA(II,AI4)
         IF(II.LE.9.AND.WC(1:2).EQ.'F'//AI4(1:1).OR.&
         &II.GE.10.AND.I.LE.99.AND.WC(1:3).EQ.'F'//AI4(1:2).OR.&
         &II.GE.100.AND.I.LE.999.AND.WC(1:4).EQ.'F'//AI4(1:3)) THEN
            FIELDER =.TRUE.
            GO TO 945
         END IF
      END DO
945   CONTINUE
      IF(FIELDER) THEN
         CALL FIELDS
         RETURN
      END IF
      RAYER=.FALSE.
      DO II=1,5000
         CALL ITOAAA(II,AI4)
         IF(II.LE.9.AND.WC(1:8).EQ.'R'//AI4(1:1)//'      '.OR.&
         &II.GE.10.AND.II.LE.99.AND.WC(1:8).EQ.'R'//AI4(1:2)//'     '.OR.&
         &II.GE.100.AND.II.LE.999.AND.WC(1:8).EQ.'R'//AI4(1:3)//'    '.OR.&
         &II.GE.1000.AND.II.LE.5000.AND.WC(1:8).EQ.'R'//AI4(1:4)//&
         &'   ') THEN
            RAYER=.TRUE.
            GO TO 946
         END IF
      END DO
946   CONTINUE
      IF(RAYER) THEN
         CALL RAYS
         RETURN
      END IF
      IF(WC.EQ.'NSSDEL'.OR.WC.EQ.'NSSNEW'.OR.WC.EQ.'NSSUNITS'.OR.&
      &WC.EQ.'NSSWV'.OR.WC.EQ.'NSSWT'.OR.WC.EQ.'UNIVERSE'.OR.WC.EQ.&
      &'OBJECT'.OR.WC.EQ.'ONAME'.OR.WC.EQ.'NSSVERT'.OR.WC.EQ.'NSSDET'&
      &.OR.WC.EQ.'SNAME'.OR.WC.EQ.'NSSCOAT1'.OR.WC.EQ.'SGRT'.OR.WC.EQ.&
      &'SURFACE'.OR.WC.EQ.'SPROFILE'.OR.WC.EQ.'SGRTD'.OR.WC.EQ.&
      &'SPARAM'.OR.WC.EQ.'SPOS'.OR.WC.EQ.'SROT'.OR.WC.EQ.'NSSCOAT2'.OR.&
      &WC.EQ.'MEDIA1'.OR.WC.EQ.'MEDIA2'.OR.WC.EQ.'NSSN'.OR.WC.EQ.&
      &'NSSSAVE'.OR.WC.EQ.'NSSREST'.OR.WC.EQ.'NSSMINE'.OR.WC.EQ.&
      &'NSSMHIT'.OR.WC.EQ.'NSSSPLIT'.OR.WC.EQ.'NSSOBJ'.OR.WC.EQ.&
      &'NSSGRIDS'.OR.WC.EQ.'NSSGRIDR'.OR.WC.EQ.&
      &'NSSTRACE'.OR.WC.EQ.'SCLAP'.OR.WC.EQ.&
      &'SHOLE'.OR.WC.EQ.'NSSLIST'.OR.WC.EQ.'OBJMEDIA'.OR.WC.EQ.&
      &'NSSLENO'.OR.WC.EQ.'SCLEAR'.OR.WC.EQ.'SUNCLEAR'.OR.WC.EQ.&
      &'SBOUNDX'.OR.WC.EQ.'SBOUNDY'.OR.WC.EQ.'SBOUNDZ'.OR.WC.EQ.&
      &'NSSPOL'.OR.WC.EQ.'NSSINTER'.OR.WC.EQ.'NSSAPODR'.OR.WC.EQ.&
      &'NSSSPOT'.OR.WC.EQ.'NSSORINT'.OR.WC.EQ.'NSSLINK'.OR.&
      &WC.EQ.'NSSEOS'.OR.WC.EQ.'NSSREF') THEN
         CALL NSSCALL
         RETURN
      END IF
      CALL SHOW_INVALID_CMD('INVALID CMD LEVEL COMMAND')
      CALL MACFAL
   END IF
   RETURN
END
SUBROUTINE DELPSF
   USE GLOBALS
   use DATSP1
   use DATSPD
   use DATSUB
   use DATLEN
   use DATMAI
   INTEGER ALLOERR
   PSFEXT=.FALSE.
! WINTER
!        CALL IOSDELETEFILE('PSF.DAT')
!        CALL IOSDELETEFILE('SPDPSF.DAT')
   DEALLOCATE(SPDPSF1,SPDPSF2,SPDPSF3,STAT=ALLOERR)
   DEALLOCATE(IPSF1,IPSF2,IPSF3,STAT=ALLOERR)
   RETURN
END
