!       EIGHTEENTH FILE FOR LENS DATABASE MANAGER FILES

SUBROUTINE FLUSHNEXT
   USE GLOBALS
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_lens_data_manager, only: ldm
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!     ALL THE ARRAY VALUES FOR THE NEXT SURFACE ARE SET TO THEIR
!     PROPER STARTING DEFAULT VALUES
   INTEGER I,J,K
!
!       INITIALIZE THE ARRAYS
   NUMHITS(SURF)=1
   SOLVE(1:9,SURF)=0.0D0
   PIKUP(1:6,SURF,1:PSIZ)=0.0D0
   call ldm%removeAllSurfaceData(SURF)
   call ldm%initRefractiveIndices(SURF)
   call ldm%clearPivotAndHigherAspherics(SURF)
   call ldm%clearApertureRegion(SURF)
   call ldm%clearMultiApertureData(SURF)
   call ldm%clearSpiderData(SURF)
   MULTCLAP(1:1000,1:3,SURF)=0.0D0
   MULTCOBS(1:1000,1:3,SURF)=0.0D0
   GLANAM(SURF,1)='             '
   GLANAM(SURF,2)='AIR          '
   LBL(SURF)(1:80)=' '
   RETURN
END
! SUB LENIN.FOR
SUBROUTINE LENIN
   use glass_manager
!
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use command_utils, only: is_command_query
   use mod_system, only: sys_telecentric, sys_set_telecentric
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE LENIN. THIS IS THE SUBROUTINE WHICH
!       HANDLES THE SETUP OF A NEW LENS AFTER INITIALIZATION BY
!       LENNS.
!
   CHARACTER WCOLD*8
!
   real(real64) RAL,RBE,CGAM,SGAM,RGAM
!
   IF(WC.EQ.'M'.OR.WC.EQ.'CK') THEN
      CALL MESCOM
      RETURN
   END IF
!       HERE IS WERE THE ? FOLLOWING A LENS ULPATE COMMAND
!       IS HANDLED
   IF(is_command_query()) THEN
      CALL LQUERY
      RETURN
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
!
      LNSTYP=1
      CALL LNSEOS
      RETURN
   ELSE
!       DON'T TERMINATE THE INPUT ROUTINE. ACCEPT INPUT DATA STARTING
!       AT THE CURRENT SURFACE OR ACCEPT SYSTEM AND TILE DATA.
!       HERE IS WHERE THE BRANCHING TO SUBROUTINES WHICH HANDLE
!       INPUT TO THE LENS MATRICES IS HANDELED.
!       FIRST THE NON-SURFACE DEPENDENT DATA IS STORED.
!
!       FILE LENSSTORE.DOC CONTAINS INFORMATION ON THE
!       SPECIFIC STORAGE LOCATIONS FOR LENS DATA ITEMS.
!
!       COMMAND (LI) OR (LIC) AT 'LENS' LEVEL
      IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
         CALL SLI
         RETURN
      END IF
!       COMMAND (INI) AT 'LENS' LEVEL
      IF(WC.EQ.'INI') THEN
         CALL SINI
         RETURN
      END IF
!       COMMAND (LTYPE) AT 'LENS' LEVEL
      IF(WC.EQ.'LTYPE') THEN
         CALL SLTYPE
         RETURN
      END IF
!       COMMAND (LABEL) OR (LBL) AT 'LENS' LEVEL
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
      IF(WC.EQ.'CW'.OR.WC.EQ.'PCW'.OR.WC.EQ.'SCW') THEN
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
            CALL REPORT_ERROR_AND_FAIL(&
            & 'A SPECIAL SURFACE MAY NOT BE DEFINED AS A DEFORMABLE SURFACE'//'\n'//&
            & 'RE-ENTER COMMAND', 1)
            RETURN
         END IF
         CALL DEFIT
         RETURN
      END IF
!
!       COMMAND (AIR)
      IF(WC.EQ.'AIR') THEN
         CALL SAIR
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (GRT,GRO,GRS,GRX,GRY,GRZ)
      IF(WC.EQ.'GRT'.OR.WC.EQ.'GRO'.OR.WC.EQ.'GRS'.OR.WC.EQ.'GRX'.OR.WC.EQ.'GRY'.OR.WC.EQ.'GRZ') THEN
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
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (GLASS [INDENTIFICATION],N1 N2 N3 N4 N5])
      IF(WC.EQ.'GLASS') THEN
         IF(is_command_query()) THEN
            WRITE(OUTLYNE,100) SURF,GLANAM(SURF,1),GLANAM(SURF,2)
            CALL SHOWIT(0)
100         FORMAT('GLASS TYPE FOR SURFACE # ',I3,' IS: ',A8,1X,A8)
            RETURN
         END IF
         CALL SGLASS
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (N6,W1, TO N10,W1)
      IF(WC.EQ.'N6'.OR.WC.EQ.'N7'.OR.WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10'.OR.WC .EQ.'N4'.OR.WC.EQ.'N5') THEN
         CALL RNCHG
         RETURN
      END IF
!       COMMAND ((GLASS CAT NAME) [INDENTIFICATION])
      IF(gdb%isNameInCatalog(WC).OR.WC.EQ.'GLCAT'.OR.WC.EQ.'GLAK')THEN
         !call LogTermFOR("About to call GLSCAT for "//WC)
!                 IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'SCH2000'.OR.
!      1          WC.EQ.'OHARA'.OR.WC.EQ.'CHANCE'.OR.
!      2          WC.EQ.'CORNIN'.OR.WC.EQ.'MATL'.OR.
!      6          WC.EQ.'HOYA'.OR.WC.EQ.'GLCAT'.OR.WC.EQ.'RADHARD'
!      7          .OR.WC.EQ.'USER'.OR.WC.EQ.'RUSSIAN'.OR.WC.EQ.'HIKARI'
!      7          .OR.WC.EQ.'GLAK')THEN
         CALL GLSCAT
         CALL FLUSHNEXT
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
!       COMMAND (INDEX OR VNUM OR DPART)
      IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
         CALL FICTCHG
         RETURN
      END IF
!       COMMAND ((GLASS CAT NAME) [INDENTIFICATION])
      IF(gdb%isNameInCatalog(trim(WC)).OR.WC.EQ.'GLCAT'.OR.WC.EQ.'GLAK')THEN
         CALL GLSCAT
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (ARRAY)
      IF(WC.EQ.'ARRAY') THEN
         CALL SARRAY(.FALSE.)
         RETURN
      END IF
!       COMMAND (ASPH,D E F G AC)
      IF(WC.EQ.'ASPH'.OR.WC.EQ.'ASPH2') THEN
         CALL SASPH
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
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (REFLTIRO)
      IF(WC.EQ.'REFLTIRO') THEN
         CALL SREFL
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (REFLTIR)
      IF(WC.EQ.'REFLTIR') THEN
         CALL SREFL
         CALL FLUSHNEXT
         RETURN
      END IF
!       COMMAND (PERFECT)
      IF(WC.EQ.'PERFECT') THEN
         CALL SPERFECT
         CALL FLUSHNEXT
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
         CALL FLUSHNEXT
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
!       COMMAND (CLAP OR COBS)
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
!       COMMAND (AC,AD,AE,AF,AG,ADTOR,AETOR,AFTOR,AGTOR)
!       COMMAND (AH,AI,AJ,AK,AL)
      IF(WC.EQ.'ACK'.OR.WC.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AG'.OR.WC.EQ.'ADTOR'.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'.OR.WC.EQ.'AH'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AI'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL') THEN
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
      IF(WC.EQ.'ALPHA'.OR.WC.EQ.'BETA'.OR.WC.EQ.'GAMMA') THEN
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
!       COMMAND (GDX.OR.GDY.OR.GDZ)
      IF(WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ') THEN
         CALL SGXYD
         RETURN
      END IF
!       COMMAND (GALPHA, GBETA OR GGAMMA)
      IF(WC.EQ.'GALPHA'.OR.WC.EQ.'GBETA'.OR.WC.EQ.'GGAMMA') THEN
         CALL SGANGLE
         RETURN
      END IF
!       COMMAND (DEC,YD XD ZD)
      IF(WC.EQ.'DEC') THEN
         CALL SDEC
         RETURN
      END IF
!       COMMAND (PIVOT,X Y Z)
      IF(WC.EQ.'PIVOT') THEN
         CALL SPIVOT
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
      IF(WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'.OR.WC.EQ.'TILT'.AND.WQ.NE.'AUTOD'.OR.WC.EQ.'RTILT') THEN
         CALL STILT
         RETURN
      END IF
!       COMMAND (YTORIC OR XTORIC)
      IF(WC.EQ.'YTORIC'.OR.WC.EQ.'XTORIC') THEN
         CALL STORIC
         RETURN
      END IF
!       COMMAND (CVTOR OR RDTOR)
      IF(WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR') THEN
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
!       COMMANDS (PY,PX,PCY,PCX,CAY,CAX)
      IF(WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCY'.OR.WC.EQ.'PCX'.OR.WC.EQ.'CAY'.OR.WC.EQ.'CAX'.OR.WC.EQ.'REDSLV') THEN
         !call LogTermFOR("Got to THSOLV call from LENIN!")
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
!       COMMANDS (APY,PIY,PUY,APCY,PICY,PUCY,COCY,OR
!                 APX,PIX,PUX,APCX,PICX,PUCX AND COCX)
      IF(WC.EQ.'APY'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PUY'.OR.WC.EQ.'APCY'.OR.WC.EQ.'PICY'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'COCY'.OR.WC.EQ.'APX'.OR.WC.EQ.'PIX'.OR.WC.EQ.'PUX'.OR.WC.EQ.'APCX'.OR.WC.EQ.'PICX'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'COCX') THEN
         CALL CVSOLV
         RETURN
      END IF
!       COMMAND (PIKUP)
      IF(WC.EQ.'PIKUP') THEN
         CALL SPIKUP
         IF(sys_telecentric().NE.0.0D0.AND.DABS(surf_thickness(NEWOBJ)).GE.1.0D10) THEN
            call sys_set_telecentric(0.0D0)
            OUTLYNE='OBJECT THICKNESS EQUALS OR EXCEEDS 1.0D+10 LENS UNITS'
            CALL SHOWIT(1)
            OUTLYNE='TELECENTRIC AIMING HAS BEEN SHUT OFF'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'PIKUP'.AND.WQ.EQ.'GLASS') CALL FLUSHNEXT
         RETURN
      END IF
   END IF
!
   RETURN
END
SUBROUTINE LOADIPOLY(J,SUR)
   use DATLEN
   use mod_surface
   use DATMAI
   use mod_surface
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   INTEGER J,SUR,N,I,M
   real(real64) X,Y
   CHARACTER AN*2,FILE_NAME*11
   LOGICAL FEXIST,FOPEN
!       J=1 = CLAP
!       J=2 = CLAP ERASE
!       J=3 = COBS
!       J=4 = COBS ERASE
!       open appropriate file, read data and load appropriate IPOLYX and IPOLYY arrays.
!
   IF(J.EQ.1) THEN
      N=INT(surf_clap_dim(SURF, 1))
      CALL ITOA2(N,AN)
      FILE_NAME='IPOLY'//AN//'.DAT'
!
!       CHECK FOR FILE EXISTENCE
      FEXIST=.FALSE.
      CALL EXIST_FILE(FILE_NAME,FEXIST)
      IF(FEXIST) THEN
!
!       CHECK FOR FILE ALREADY OPENED
         FOPEN=.FALSE.
         CALL OPEN_FILE(FILE_NAME,FOPEN)
         IF(FOPEN) THEN
            CALL CLOSE_FILE(67,1)
         END IF
      ELSE
!       FILE DOES NOT EXIST,QUIT
         CALL MACFAL
         RETURN
      END IF
!       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
      OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED',FILE=FILE_NAME ,STATUS='UNKNOWN')
      REWIND(UNIT=67)
      DO I=1,INT(surf_clap_dim(SURF, 2))
         READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
         IPOLYX(I,SUR,J)=X
         IPOLYY(I,SUR,J)=Y
      END DO
   END IF
!
   IF(J.EQ.2) THEN
      N=INT(surf_cobs_ape_data(SURF, 1))
      CALL ITOA2(N,AN)
      FILE_NAME='IPOLY'//AN//'.DAT'
!
!       CHECK FOR FILE EXISTENCE
      FEXIST=.FALSE.
      CALL EXIST_FILE(FILE_NAME,FEXIST)
      IF(FEXIST) THEN
!
!       CHECK FOR FILE ALREADY OPENED
         FOPEN=.FALSE.
         CALL OPEN_FILE(FILE_NAME,FOPEN)
         IF(FOPEN) THEN
            CALL CLOSE_FILE(67,1)
         END IF
      ELSE
!       FILE DOES NOT EXIST,QUIT
         CALL MACFAL
         RETURN
      END IF
!       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
      OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED',FILE=FILE_NAME ,STATUS='UNKNOWN')
      REWIND(UNIT=67)
      DO I=1,INT(surf_cobs_ape_data(SURF, 2))
         READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
         IPOLYX(I,SUR,J)=X
         IPOLYY(I,SUR,J)=Y
      END DO
   END IF
!
!
   IF(J.EQ.3) THEN
      N=INT(surf_cobs_poly(SURF, 1))
      CALL ITOA2(N,AN)
      FILE_NAME='IPOLY'//AN//'.DAT'
!
!       CHECK FOR FILE EXISTENCE
      FEXIST=.FALSE.
      CALL EXIST_FILE(FILE_NAME,FEXIST)
      IF(FEXIST) THEN
!
!       CHECK FOR FILE ALREADY OPENED
         FOPEN=.FALSE.
         CALL OPEN_FILE(FILE_NAME,FOPEN)
         IF(FOPEN) THEN
            CALL CLOSE_FILE(67,1)
         END IF
      ELSE
!       FILE DOES NOT EXIST,QUIT
         CALL MACFAL
         RETURN
      END IF
!       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
      OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED',FILE=FILE_NAME ,STATUS='UNKNOWN')
      REWIND(UNIT=67)
      DO I=1,INT(surf_cobs_poly(SURF, 2))
         READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
         IPOLYX(I,SUR,J)=X
         IPOLYY(I,SUR,J)=Y
      END DO
   END IF
!
!
   IF(J.EQ.4) THEN
      N=INT(surf_cobs_era_data(SURF, 1))
      CALL ITOA2(N,AN)
      FILE_NAME='IPOLY'//AN//'.DAT'
!
!       CHECK FOR FILE EXISTENCE
      FEXIST=.FALSE.
      CALL EXIST_FILE(FILE_NAME,FEXIST)
      IF(FEXIST) THEN
!
!       CHECK FOR FILE ALREADY OPENED
         FOPEN=.FALSE.
         CALL OPEN_FILE(FILE_NAME,FOPEN)
         IF(FOPEN) THEN
            CALL CLOSE_FILE(67,1)
         END IF
      ELSE
!       FILE DOES NOT EXIST,QUIT
         CALL MACFAL
         RETURN
      END IF
!       NOW OPEN FILE AND LOAD APPROPRIATE ARRAY
      OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED',FILE=FILE_NAME ,STATUS='UNKNOWN')
      REWIND(UNIT=67)
      DO I=1,INT(surf_cobs_era_data(SURF, 2))
         READ(UNIT=67,FMT=*,END=9991,ERR=9991) M,X,Y
         IPOLYX(I,SUR,J)=X
         IPOLYY(I,SUR,J)=Y
      END DO
   END IF
!
9991 CONTINUE
   CALL CLOSE_FILE(67,1)
   RETURN
END
