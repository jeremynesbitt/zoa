        SUBROUTINE USERINPT
C
C  Use of the WINTERACTER module is compulsory
C
      USE WINTERACTER
      USE GLOBALS
      IMPLICIT NONE
C
      CHARACTER KKDP*3,RES3*3,RESPONSE*80,AJX*6,AJY*6
c
      INCLUDE 'RESOURCE.INC'
C
C  Parameters for new line and tab codes in editor
C
      REAL*8 XFOB,YFOB,ZFOB,RAYXR,RAYYR,INTENSITY,RD(0:499,1:2)
      REAL*8 TH(0:499,1:2),TEMPVAL,OLDCTOF,OLD59,DALPHA,DBETA,DGAMMA
      REAL*8 DDECX,DDECY,DDECZ,DPIVX,DPIVY,DPIVZ
      REAL*8 TEMPCV,TEMPTH
      REAL*8 DW1,DW2,DW3,DW4,DW5,DW6,DOUBARRAY(1:96)
      INTEGER IW1
      INTEGER FWNUM,ONUM,RNUM,RWNUM,ROWS_TO_ADD,CURROW,CURCOL
      INTEGER PREVROW,PREVCOL,N,APETYPE,OLD_APETYPE,OLD26,OLD27,OLD25
      INTEGER COBSTYPE,OLD_COBSTYPE,EAPETYPE,OLD_EAPETYPE,ECOBSTYPE
      INTEGER OLD_ECOBSTYPE,NROWS,FIRSTJ,LASTJ
      INTEGER PTYPER,K,L,M,FNDLIST,CATLIST,BFORCE,LIBLIST
      COMMON/TYPERP/PTYPER
      COMMON/ROWCOL/CURROW,CURCOL,PREVROW,PREVCOL
      CHARACTER(LEN=3) AONUM,ARNUM,AINUM,AJ,AK,AL,AM,AN
      CHARACTER (LEN=9) BIGWORD
      CHARACTER(LEN=2),   PARAMETER :: NEWLIN = CHAR(13)//CHAR(10)
      CHARACTER(LEN=1),   PARAMETER :: TAB    = CHAR(9)
      CHARACTER*255 FNAME,TNAME,ONAME
      CHARACTER AI*3,AI4*4,CVALUE*80,SPECNAME*13,MNAME1*8,MNAME2*8,
     1GL*27,GL2(0:499,1:2)*29,ATEMPVAL*23,TEMPGL*27,AW6*23,MNAME27*27
     2,ONAME27*27
      CHARACTER AW1*23,AW2*23,AW3*23,AW4*23,AW5*23,STRINGER*80,ASSI*23
      CHARACTER CURNAME*13
      COMMON/NAMECUR/CURNAME
      COMMON/SPREADLENS/RD,TH,GL2
      CHARACTER NAME13*13,TEMP27*27,FILENAME*70,PART1*5,PART2*4,A6*6
C
C Declare window-type and message variables
C
      TYPE(WIN_STYLE)     :: WINDOW
      TYPE(WIN_MESSAGE)   :: MESSAGE,MESSAGE1

      INTEGER             :: ITYPE,IREND,I,N3,NUMINLIST,J,IW2,ITYPE1
C
      INTEGER             :: ISTARTSURF,ISTOPSURF,JX,JY
C
C Entered commands and displayed output
C
      CHARACTER(LEN=1024) :: COMMAND,OUTPUT
C
      REAL*8              :: IMAX
      REAL*8              :: IMIN
      INTEGER             :: INUM
      INTEGER             :: IWAV
      INTEGER             :: ICOL
      INTEGER             :: IROW
      INTEGER             :: IPOS
      INTEGER             :: IEDITWIN
      INTEGER             :: ISET
      INTEGER             :: JSET
      INTEGER             :: KSET
      INTEGER             :: LSET
      INTEGER             :: MSET
      INTEGER             :: NSET
      INTEGER             :: OSET
      INTEGER             :: PSET
      INTEGER             :: QSET
      INTEGER             :: RSET
      INTEGER             :: ISTATE
      INTEGER             :: JSTATE
      INTEGER             :: KSTATE
      INTEGER             :: LSTATE
      INTEGER             :: MSTATE
      INTEGER             :: NSTATE
      INTEGER             :: IOPTION
      REAL*8 WAVE_LENGTH
C
      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATMAC.INC'
      INCLUDE 'DATLEN.INC'
      INCLUDE 'DATSUB.INC'
      INCLUDE 'DATSPD.INC'
      INCLUDE 'DATSP1.INC'
C
      INTEGER ITEM
C
      LOGICAL EXIS88,EXIS89
C
      FNAME(1:255)=' '
      TNAME(1:255)=' '
      ONAME(1:255)=' '
      STRINGER(1:80)=' '

      CALL WMessage(ITYPE, MESSAGE)
C
      CALL WMESSAGEENABLE(FIELDCHANGED,1)
C
C       TURN ON FIELD CHANGE DETECTION
C
        SELECT CASE (ITYPE)

        CASE (FieldChanged)
C
        INCLUDE 'CHANGE_FIELDS_HANDLER.INC'
C
        CASE (PushButton)
C
        INCLUDE 'GLASSP.INC'
C
        INCLUDE 'MACRO1_DIALOG_HANDLER.INC'
C
        INCLUDE 'PARAX1_DIALOG_HANDLER.INC'
C
        INCLUDE 'PARAX2_DIALOG_HANDLER.INC'
C
        INCLUDE 'PARAX3_DIALOG_HANDLER.INC'
C
        INCLUDE 'PARAX4_DIALOG_HANDLER.INC'
C
        INCLUDE 'RAYS.INC'
C
        INCLUDE 'RAYSETTINGS.INC'
C
        INCLUDE 'LENSED.INC'
C
        INCLUDE 'LENO.INC'
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE LENS LIBRARY GUI
C
        IF(MESSAGE%WIN.EQ.IDD_LLIB) THEN
C
        CALL WDIALOGSELECT(IDD_LLIB)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDF_MA)
        CALL WDIALOGPUTRADIOBUTTON(IDF_MA1)
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLEN'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
C
        CASE (IDF_ED)
        CALL WDIALOGPUTRADIOBUTTON(IDF_ED1)
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLES'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
C
        CASE (IDF_MG)
        CALL WDIALOGPUTRADIOBUTTON(IDF_MG1)
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLMG'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
C
        CASE (IDF_NC)
        CALL WDIALOGPUTRADIOBUTTON(IDF_NC1)
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLNC'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
C
        CASE (IDF_RO)
        CALL WDIALOGPUTRADIOBUTTON(IDF_RO1)
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLRO'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
C
        CASE (IDF_SH)
        CALL WDIALOGPUTRADIOBUTTON(IDF_SH1)
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLSH'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
C
        CASE (IDF_LIB_GET)
        CALL WDIALOGGETMENU(IDF_LIBCONTENTS,IOPTION,CVALUE)
         IF(IOPTION.NE.0) THEN
         WRITE(INPUT,*) 'LIB GET,',CVALUE(1:3)
         CALL PROCES
                         END IF
C
        CASE (IDF_LIB_DEL)
         CALL WDIALOGGETRADIOBUTTON(IDF_MA1,ISET)
        IF(ISET.EQ.1) THEN
         CALL WDIALOGGETMENU(IDF_LIBCONTENTS,IOPTION,CVALUE)
         IF(IOPTION.NE.0) THEN
         WRITE(INPUT,*) 'LIB DEL,',CVALUE(1:3)
         CALL PROCES
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLEN'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
                         END IF
                        ELSE
        WRITE(OUTLYNE,*)
     1  'DELETION OF MANUFACTURERS LENSES NOT ALLOWED'
        CALL SHOWIT(1)
                        END IF
C
        CASE (IDF_LIB_PUT)
         CALL WDIALOGGETRADIOBUTTON(IDF_MA1,ISET)
        IF(ISET.EQ.1) THEN
         WRITE(INPUT,*) 'LIB PUT'
         CALL PROCES
C       INPUT='OUT NULL'
C       CALL PROCES
        INPUT='LENDIR LIBLEN'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
                        ELSE
        WRITE(OUTLYNE,*)
     1  'LENS STORAGE INTO MANUFACTURERS LIBRARIES NOT ALLOWED'
        CALL SHOWIT(1)
                        END IF
C
        CASE (IDF_LENADD)
         CALL WDIALOGGETRADIOBUTTON(IDF_LENADD1,ISET)
         CALL WDIALOGGETMENU(IDF_LIBCONTENTS,IOPTION,CVALUE)
         IF(IOPTION.NE.0) THEN
         IF(ISET.EQ.1) THEN
         WRITE(INPUT,*) 'LENADD,',CVALUE(1:3)
         CALL PROCES
                        ELSE
         WRITE(INPUT,*) 'LENADD,',CVALUE(1:3),',1,1'
         CALL PROCES
                        END IF
                         END IF
        WRITE(OUTLYNE,*)
     1 'LIBRARY FILE # ',CVALUE(1:3),' ADDED TO CURRENT LENS'
        CALL SHOWIT(1)
C
        CASE (IDF_LIBSAVE)
         WRITE(INPUT,*) 'LIBSAVE'
         CALL PROCES
C
        CASE (IDF_LIBREST)
         WRITE(INPUT,*) 'LIBREST'
         CALL PROCES

        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                END IF
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE BLACK BODY GUI
C
        IF(MESSAGE%WIN.EQ.IDD_BB) THEN
C
        CALL WDIALOGSELECT(IDD_BB)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDF_WIEN)
        CALL WDIALOGGETRADIOBUTTON(IDF_UN1,ISET)
        CALL WDIALOGGETDOUBLE(IDF_T1,DW1)
        IF(ISET.EQ.1) THEN
        INPUT='RADUNITS WATTS'
        CALL PROCES
                ELSE
        INPUT='RADUNITS PHOTONS'
        CALL PROCES
                END IF
        IF(DW1.LT.0.0D0) DW1=0.0D0
        WRITE(INPUT,*)'WIEN P,',DW1
        CALL PROCES

        CASE (IDF_STEFBOLT)
        CALL WDIALOGGETRADIOBUTTON(IDF_UN1,ISET)
        CALL WDIALOGGETDOUBLE(IDF_T2,DW1)
        CALL WDIALOGGETDOUBLE(IDF_W1,DW2)
        CALL WDIALOGGETDOUBLE(IDF_W2,DW3)
        IF(DW1.LT.0.0D0) DW1=0.0D0
        IF(DW2.LT.0.0D0) DW2=0.0D0
        IF(DW3.LT.0.0D0) DW3=0.0D0
        IF(ISET.EQ.1) THEN
        INPUT='RADUNITS WATTS'
        CALL PROCES
                ELSE
        INPUT='RADUNITS PHOTONS'
        CALL PROCES
                END IF
        WRITE(INPUT,*)'STEFBOLT P,',DW1,',',DW3,',',DW2
        CALL PROCES

        CASE (IDF_PLANK)
        CALL WDIALOGGETRADIOBUTTON(IDF_UN1,ISET)
        CALL WDIALOGGETDOUBLE(IDF_T3,DW1)
        CALL WDIALOGGETDOUBLE(IDF_W3,DW2)
        IF(DW1.LT.0.0D0) DW1=0.0D0
        IF(DW2.LT.0.0D0) DW2=0.0D0
        IF(ISET.EQ.1) THEN
        INPUT='RADUNITS WATTS'
        CALL PROCES
                ELSE
        INPUT='RADUNITS PHOTONS'
        CALL PROCES
                END IF
        WRITE(INPUT,*)'PLANK P,',DW1,',',DW2
        CALL PROCES
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                END IF
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF FONT SELECTION MENU ITEM
C
        IF(MESSAGE%WIN.EQ.IDD_EDITFONT) THEN
C
        CALL WDIALOGSELECT(IDD_EDITFONT)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        CALL WDIALOGGETINTEGER(IDF_INT1,IW1)
        CALL WEDITFONT(4,IW1)
        CALL SAVEFONT(IW1)
        CALL WDialogHide()
        CALL WDialogUnload()
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                END IF
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE VARIABLES GUI
C
C       VARIABLES
C
        IF(MESSAGE%WIN.EQ.IDD_VARED) THEN
C
                        CALL VARIABLES(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE APERTURE APODIZATION GUI
C
C       APOD
C
        IF(MESSAGE%WIN.EQ.IDD_APOD) THEN
C
                        CALL APODGUI(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE OPTIMIZE GUI
C
C       OPTIMIZE
C
        IF(MESSAGE%WIN.EQ.IDD_OPTIM) THEN
C
                        CALL OPTIMIZE(MESSAGE)

C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       DOTF GUI
C
        IF(MESSAGE%WIN.EQ.IDD_DOTF) THEN
C
                        CALL DOTFGUI(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE DOES PSF GUI
C
        IF(MESSAGE%WIN.EQ.IDD_PSF) THEN
C
                        CALL PSFGUI(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE DOES PSF STREAKING GUI
C
        IF(MESSAGE%WIN.EQ.IDD_PSFSTREAK) THEN
C
                        CALL PSFSTREAK(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE DOES PSF STREHL RATIO GUI
C
        IF(MESSAGE%WIN.EQ.IDD_PSFSTREHL) THEN
C
                        CALL PSFSTREHL(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE DOES PSF ENERGY DISTRIBUTION GUI
C
        IF(MESSAGE%WIN.EQ.IDD_PSFENERGY) THEN
C
                        CALL PSFENERGY(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES GEOMETRICAL SPOT DIAGRAM GUI
C
        IF(MESSAGE%WIN.EQ.IDD_SPOT) THEN
C
                        CALL SPOTGUI(MESSAGE)
C
                        END IF
C
C       THIS CODE TAKES GEOMETRICAL SPOT DIAGRAM RAY FAILURE GUI
C
        IF(MESSAGE%WIN.EQ.IDD_FAIL) THEN
C
                        CALL FAILGUI(MESSAGE)
C
                        END IF
C
C       THIS CODE TAKES GEOMETRICAL SPOT DIAGRAM RAY ANGLE STATISTICS
C
        IF(MESSAGE%WIN.EQ.IDD_ISTAT) THEN
C
                        CALL ISTATGUI(MESSAGE)
C
                        END IF
C
C       THIS CODE TAKES GEOMETRICAL SPOT DIAGRAM WRITING GUI
C
        IF(MESSAGE%WIN.EQ.IDD_SPOTWRITE) THEN
C
                        CALL SPOTWRITEGUI(MESSAGE)
C
                        END IF
C
C       THIS CODE TAKES GEOMETRICAL SPOT DIAGRAM PLOTTING
C
        IF(MESSAGE%WIN.EQ.IDD_SPOTPLOT) THEN
C
                        CALL SPOTPLOT(MESSAGE)
C
                        END IF
C
C       GOTF GUI
C
        IF(MESSAGE%WIN.EQ.IDD_GOTF) THEN
C
                        CALL GOTFGUI(MESSAGE)
C
                        END IF
C
C       GEOMETRICAL ENERGY DISTRIBUTION GUI
C
        IF(MESSAGE%WIN.EQ.IDD_SPOTENERGY) THEN
C
                        CALL SPOTENERGY(MESSAGE)
C
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C       GLASS
C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       GLASS DATA
C
C
        IF(MESSAGE%WIN.EQ.IDD_EDGLASS) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_EDGLASS)
C
        SELECT CASE(MESSAGE%VALUE1)
C
C
C       PRESS BUTTONS
C
        CASE (IDF_ED9)
        IF(GLANAM(J,1).EQ.'SCHOTT'.OR.GLANAM(J,1).EQ.'OHARA'
     1  .OR.GLANAM(J,1).EQ.'HOYA'.OR.GLANAM(J,1).EQ.'CHANCE'
     1  .OR.GLANAM(J,1).EQ.'CORNIN'.OR.GLANAM(J,1).EQ.'RUSSIAN'
     1  .OR.GLANAM(J,1).EQ.'HIKARI'.OR.GLANAM(J,1).EQ.'RADHARD'
     1  .OR.GLANAM(J,1).EQ.'MATL'.OR.GLANAM(J,1).EQ.'SCH2000') THEN
        MNAME27='JUNK                '
                        ELSE
        MNAME27='JUNK                '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='SCHOTT BK7'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMYGLASS.INC'
        CALL GLASSFRESH(J)
        CALL WDIALOGCLEARFIELD(IDF_CURGLASS)
                        END IF
C
        CASE (IDF_ED10)
        IF(GLANAM(1,J).EQ.'GLASS') THEN
        MNAME27='GLASS                    '
                        ELSE
        MNAME27='GLASS                    '
        CALL WDIALOGPUTSTRING(IDF_MYNAME,'DUMMY')
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='GLASS DUMMY'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDIALOGCLEARFIELD(IDF_MYNAME)
        CALL WDIALOGFIELDSTATE(IDF_MYNAME,1)
        CALL WDIALOGCLEARFIELD(IDF_N1)
        CALL WDIALOGCLEARFIELD(IDF_N2)
        CALL WDIALOGCLEARFIELD(IDF_N3)
        CALL WDIALOGCLEARFIELD(IDF_N4)
        CALL WDIALOGCLEARFIELD(IDF_N5)
        CALL WDIALOGCLEARFIELD(IDF_N6)
        CALL WDIALOGCLEARFIELD(IDF_N7)
        CALL WDIALOGCLEARFIELD(IDF_N8)
        CALL WDIALOGCLEARFIELD(IDF_N9)
        CALL WDIALOGCLEARFIELD(IDF_N10)
        CALL WDIALOGFIELDSTATE(IDF_N1,2)
        CALL WDIALOGFIELDSTATE(IDF_N2,2)
        CALL WDIALOGFIELDSTATE(IDF_N3,2)
        CALL WDIALOGFIELDSTATE(IDF_N4,2)
        CALL WDIALOGFIELDSTATE(IDF_N5,2)
        CALL WDIALOGFIELDSTATE(IDF_N6,2)
        CALL WDIALOGFIELDSTATE(IDF_N7,2)
        CALL WDIALOGFIELDSTATE(IDF_N8,2)
        CALL WDIALOGFIELDSTATE(IDF_N9,2)
        CALL WDIALOGFIELDSTATE(IDF_N10,2)
        IF(SYSTEM(1).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N1,1)
        IF(SYSTEM(2).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N2,1)
        IF(SYSTEM(3).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N3,1)
        IF(SYSTEM(4).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N4,1)
        IF(SYSTEM(5).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N5,1)
        IF(SYSTEM(71).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N6,1)
        IF(SYSTEM(72).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N7,1)
        IF(SYSTEM(73).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N8,1)
        IF(SYSTEM(73).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N9,1)
        IF(SYSTEM(75).NE.0.0D0)
     1  CALL WDIALOGFIELDSTATE(IDF_N10,1)
        CALL WDIALOGFIELDSTATE(IDF_MYGLASSOK,1)
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTCAT.INC'
        CALL GLASSFRESH(J)
                        END IF

        CASE (IDF_ED11)
        IF(GLANAM(1,J).EQ.'MODEL') THEN
        MNAME27='MODEL               '
                        ELSE
        CALL WDIALOGPUTSTRING(IDF_MYNAME,'DUMMY')
        MNAME27='MODEL               '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='MODEL DUMMY'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDIALOGCLEARFIELD(IDF_MODNAME)
        CALL WDIALOGFIELDSTATE(IDF_MODNAME,1)
        CALL WDIALOGCLEARFIELD(IDF_ND)
        CALL WDIALOGCLEARFIELD(IDF_VD)
        CALL WDIALOGCLEARFIELD(IDF_DPART)
        CALL WDIALOGFIELDSTATE(IDF_ND,1)
        CALL WDIALOGFIELDSTATE(IDF_VD,1)
        CALL WDIALOGFIELDSTATE(IDF_DPART,1)
        CALL WDIALOGFIELDSTATE(IDF_MODELOK,1)
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMYGLASS.INC'
        INCLUDE 'NOTCAT.INC'
        CALL GLASSFRESH(J)
                        END IF

        CASE (IDF_ED12)
        IF(GLANAM(1,J).EQ.'USER') THEN
        MNAME27='USER                       '
                        ELSE
        CALL WDIALOGPUTSTRING(IDF_USERNAME,'DUMMY')
        MNAME27='USER                       '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='USER DUMMY'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDIALOGCLEARFIELD(IDF_USERNAME)
        CALL WDIALOGFIELDSTATE(IDF_USERNAME,1)
        CALL WDIALOGPUTCHECKBOX(IDF_USER,1)
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTCAT.INC'
        INCLUDE 'NOTMYGLASS.INC'
        CALL GLASSFRESH(J)
                        END IF
C
        CASE (IDF_ED13)
        MNAME27='AIR                        '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='AIR'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTCAT.INC'
        INCLUDE 'NOTMYGLASS.INC'
        CALL GLASSFRESH(J)
C
        CASE (IDF_ED14)
        MNAME27='REFL                       '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='REFL'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTCAT.INC'
        INCLUDE 'NOTMYGLASS.INC'
        CALL GLASSFRESH(J)
C
        CASE (IDF_ED15)
        MNAME27='REFLTIRO                   '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='REFLTIRO'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTCAT.INC'
        INCLUDE 'NOTMYGLASS.INC'
        CALL GLASSFRESH(J)
C
        CASE (IDF_ED16)
        MNAME27='REFLTIR                    '
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='REFLTIR'
        INPUT='EOS'
        CALL PROCES
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'NOTUSER.INC'
        INCLUDE 'NOTMODEL.INC'
        INCLUDE 'NOTCAT.INC'
        INCLUDE 'NOTMYGLASS.INC'
        CALL GLASSFRESH(J)
C
        CASE (IDCANCEL)
C
        CALL GLASSFRESH(J)
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_EDGLASS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_MODTOCAT)
C
C       GATHER UP DATA AND DO STUFF
        CALL WDialogGetMenu(IDF_FINDGLASS,IOPTION,CVALUE)
        IF(IOPTION.NE.0) THEN
        CVALUE=FNDARRAY(IOPTION)
        CVALUE=TRIM(CVALUE)
        MNAME27=CVALUE(1:27)
        MNAME27=TRIM(MNAME27)
        CALL TOUPPER(MNAME27,27)
C
C       DO THE CONVERTION
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT=MNAME27
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL GLASSFRESH(J)
                        END IF
C
        CASE (IDF_MYGLASSOK)
C
C       GATHER UP DATA AND DO STUFF
        CALL WDIALOGGETSTRING(IDF_MYNAME,NAME13)
        CALL WDIALOGGETDOUBLE(IDF_N1,DW1)
        CALL WDIALOGGETDOUBLE(IDF_N2,DW2)
        CALL WDIALOGGETDOUBLE(IDF_N3,DW3)
        CALL WDIALOGGETDOUBLE(IDF_N4,DW4)
        CALL WDIALOGGETDOUBLE(IDF_N5,DW5)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        CALL DTOA23(DW3,AW3)
        CALL DTOA23(DW4,AW4)
        CALL DTOA23(DW5,AW5)
C
C       DO THE CONVERTION
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='GLASS '//NAME13//','//AW1//','//AW2//','//AW3
     1//','//AW4//','//AW5
        CALL PROCES
        CALL WDIALOGGETDOUBLE(IDF_N6,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='N6,'//AW1
        CALL PROCES
        CALL WDIALOGGETDOUBLE(IDF_N7,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='N7,'//AW1
        CALL PROCES
        CALL WDIALOGGETDOUBLE(IDF_N8,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='N8,'//AW1
        CALL PROCES
        CALL WDIALOGGETDOUBLE(IDF_N9,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='N9,'//AW1
        CALL PROCES
        CALL WDIALOGGETDOUBLE(IDF_N10,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='N10,'//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL GLASSFRESH(J)
C
        CASE (IDF_MODELOK)
C
C       GATHER UP DATA AND DO STUFF
        CALL WDIALOGGETSTRING(IDF_MODNAME,NAME13)
        CALL WDIALOGGETDOUBLE(IDF_ND,DW1)
        CALL WDIALOGGETDOUBLE(IDF_VD,DW2)
        CALL WDIALOGGETDOUBLE(IDF_DPART,DW3)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        CALL DTOA23(DW3,AW3)
C
C       DO THE CONVERTION
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='MODEL '//NAME13//','//AW1//','//AW2//','//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL GLASSFRESH(J)
C
        CASE (IDF_CATTOMOD)
C
C       GATHER UP DATA AND DO STUFF
        CURNAME=GLANAM(J,2)
        CALL ITOAA(J,AJ)
        INPUT='TOMODEL,'//AJ
        CALL PROCES
        CALL GLASSFRESH(J)
C
        CASE (IDF_CHGGLASS)
C
C       GATHER UP DATA AND DO STUFF
        CALL WDialogGetMenu(IDF_CATMENU,IOPTION,CVALUE)
        IF(IOPTION.NE.0) THEN
        CVALUE=CATARRAY(IOPTION)
        CVALUE=TRIM(CVALUE)
        MNAME27=CVALUE(1:27)
        MNAME27=TRIM(MNAME27)
        CALL TOUPPER(MNAME27,27)
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT=MNAME27
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL GLASSFRESH(J)
                END IF

        CASE (IDF_USEROK)
C
C       GATHER UP DATA AND DO STUFF
        CALL WDIALOGGETSTRING(IDF_USERNAME,NAME13)
C
C       DO THE CONVERTION
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='USER '//NAME13
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL GLASSFRESH(J)
C
        END SELECT
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       RADIUS
C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       RADIUS
C
C
        IF(MESSAGE%WIN.EQ.IDD_SRADIUS) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_SRADIUS)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDCANCEL)
C
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_SRADIUS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDOK)
C
        CALL WDIALOGGETDOUBLE(IDF_VAL,TEMPCV)
        INPUT='U L'
        CALL PROCES
        WRITE(INPUT,*) 'CHG ',J
        CALL PROCES
        WRITE(INPUT,*) 'RD ',TEMPCV
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_SRADIUS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       CURVATURE
C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       CURVATURE INPUT
C
C
        IF(MESSAGE%WIN.EQ.IDD_SCURVATURE) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_SCURVATURE)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDCANCEL)
C
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_SCURVATURE)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDOK)
C
        CALL WDIALOGGETDOUBLE(IDF_VAL,TEMPCV)
        INPUT='U L'
        CALL PROCES
        WRITE(INPUT,*) 'CHG ',J
        CALL PROCES
        WRITE(INPUT,*) 'CV ',TEMPCV
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_SCURVATURE)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       THICKNESS
C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       THICKNESS INPUT
C
C
        IF(MESSAGE%WIN.EQ.IDD_STHICKNESS) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_STHICKNESS)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDCANCEL)
C
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_STHICKNESS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDOK)
C
        CALL WDIALOGGETDOUBLE(IDF_VAL,TEMPTH)
        INPUT='U L'
        CALL PROCES
        WRITE(INPUT,*) 'CHG ',J
        CALL PROCES
        WRITE(INPUT,*) 'TH ',TEMPTH
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_STHICKNESS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_SURFACEEDITOR_RD)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       APERTURE STOP HANDLING
C       THIS CODE TAKES CARE OF THE OPERATION OF THE STOP
C       SURFACE DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_STOPSURF) THEN
        CALL WDIALOGSELECT(IDD_STOPSURF)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       ASSIGN STOP SURFACE
        CALL WDIALOGGETRADIOBUTTON(IDF_ASTOP1,ISET)
        SELECT CASE (ISET)
C
        CASE (1)
C       USE NO PUPIL ADJUSTMENT.
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASTOP'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
        CASE (2)
C       ENTRANCE PUPIL ADJUSTMENT.
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASTOP EN'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
        CASE (3)
C       EXIT PUPIL ADJUSTMENT.
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASTOP EX'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
        CASE (4)
C       USE NO PUPIL ADJUSTMENT.
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASTOP ENEX'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        END SELECT
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'


        CASE (IDCANCEL)
C       ASSIGN STOP SURFACE TO OLD STOP SURF
        J=OLD26
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(OLD27.EQ.0) INPUT='ASTOP'
        IF(OLD27.EQ.-1) INPUT='ASTOP EN'
        IF(OLD27.EQ.1) INPUT='ASTOP EX'
        IF(OLD27.EQ.2) INPUT='ASTOP ENEX'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        END SELECT
                           END IF

C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       REFERENCE SURFACE HANDLING
C       THIS CODE TAKES CARE OF THE OPERATION OF THE REFERENCE
C       SURFACE DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_REFSSURF) THEN
        OLD59=SYSTEM(59)
        CALL WDIALOGSELECT(IDD_REFSSURF)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       ASSIGN REFS SURFACE
C
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        CALL WDIALOGGETDOUBLE(IDF_REFROT,TEMPVAL)
        CALL DTOA23(TEMPVAL,ATEMPVAL)
        INPUT='REFS '//ATEMPVAL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'


        CASE (IDCANCEL)
C       ASSIGN REFS SURFACE TO OLD STOP SURF
        J=OLD25
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        TEMPVAL=OLD59
        CALL DTOA23(TEMPVAL,ATEMPVAL)
        INPUT='REFS '//ATEMPVAL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        END SELECT
                           END IF

C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       CLEAR APERTURE HANDLING
C       THIS CODE TAKES CARE OF THE OPERATION OF THE CLEAR
C       APERTURE MAIN EDITOR WINDOW
C
        IF(MESSAGE%WIN.EQ.IDD_CLAPS) THEN
        CALL WDIALOGSELECT(IDD_CLAPS)
        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDF_MULTICLAP)
C       SET MULTIPLE CLAPS
        CALL WDIALOGGETINTEGER(IDF_NUMAPE,ISET)
        CALL WDIALOGLOAD(IDD_MCLAP)
        CALL WDIALOGPUTINTEGER(IDF_MAX,INT(ALENS(127,EDIT_SURFACE)))
        CALL WDIALOGPUTINTEGER(IDF_NUM,ISET)
        CALL WDIALOGPUTDOUBLE(IDF_X,MULTCLAP(ISET,1,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_Y,MULTCLAP(ISET,2,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,MULTCLAP(ISET,3,EDIT_SURFACE))
        CALL WDIALOGSELECT(IDD_MCLAP)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_MULTICOBS)
C       SET MULTIPLE COBS
        CALL WDIALOGGETINTEGER(IDF_NUMOBS,ISET)
        CALL WDIALOGLOAD(IDD_MCOBS)
        CALL WDIALOGPUTINTEGER(IDF_MAX,INT(ALENS(128,EDIT_SURFACE)))
        CALL WDIALOGPUTINTEGER(IDF_NUM,ISET)
        CALL WDIALOGPUTDOUBLE(IDF_X,MULTCOBS(ISET,1,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_Y,MULTCOBS(ISET,2,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,MULTCOBS(ISET,3,EDIT_SURFACE))
        CALL WDIALOGSELECT(IDD_MCOBS)
        CALL WDialogShow(-1,-1,0,Modeless)


        CASE (IDF_SPIDERD)
C       DELETE ANY EXISTING SPIDER
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='SPIDER DELETE'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C       DELETE ANY EXISTING SPIDER
        OLD_COBSTYPE=0
        OLD_ECOBSTYPE=0
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_CLAPD)
C       DELETE ANY EXISTING CLAP
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAPD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C       DELETE ANY EXISTING CLAP
        OLD_APETYPE=0
        OLD_EAPETYPE=0
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_COBSD)
C       DELETE ANY EXISTING COBS
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBSD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C       DELETE ANY EXISTING CLAP
        OLD_COBSTYPE=0
        OLD_ECOBSTYPE=0
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        CASE (ID_EDCLAP)
C       EDIT STUFF
        CALL WDIALOGGETRADIOBUTTON(IDF_APE1,ISET)
        SELECT CASE (ISET)
C
        CASE (1)
C       IF NO CLAP SET, MAKE IT CIRCULAR
                        ALENS(9,EDIT_SURFACE)=1.0D0
                        ALENS(10,EDIT_SURFACE)=0.0D0
                        ALENS(11,EDIT_SURFACE)=0.0D0
                        ALENS(12,EDIT_SURFACE)=0.0D0
                        ALENS(13,EDIT_SURFACE)=0.0D0
                        ALENS(14,EDIT_SURFACE)=0.0D0
                        ALENS(15,EDIT_SURFACE)=0.0D0
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR CLAP DATA
        CALL WDialogLoad(IDD_APECIRC)
C       LOAD EXISTING NUMERIC WORDS
        APETYPE=1
        OLD_APETYPE=APETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(10,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(13,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(12,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(11,EDIT_SURFACE))
        OLDCTOF=ALENS(11,EDIT_SURFACE)
        Call WDialogPutDouble(IDF_NW5,ALENS(14,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (2)
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR CLAP DATA
        CALL WDialogLoad(IDD_APECIRC)
C       LOAD EXISTING NUMERIC WORDS
        APETYPE=1
        IF(APETYPE.NE.OLD_APETYPE) THEN
                ALENS(10,EDIT_SURFACE)=0.0D0
                ALENS(11,EDIT_SURFACE)=0.0D0
                ALENS(12,EDIT_SURFACE)=0.0D0
                ALENS(13,EDIT_SURFACE)=0.0D0
                ALENS(14,EDIT_SURFACE)=0.0D0
                ALENS(15,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_APETYPE=APETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(10,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(13,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(12,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(11,EDIT_SURFACE))
        OLDCTOF=ALENS(11,EDIT_SURFACE)
        Call WDialogPutDouble(IDF_NW5,ALENS(14,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (3)
C       OPEN DIALOG WINDOW FOR EDITTING RECT CLAP DATA
        CALL WDialogLoad(IDD_APERECT)
C       LOAD EXISTING NUMERIC WORDS
        APETYPE=2
        IF(APETYPE.NE.OLD_APETYPE) THEN
                ALENS(10,EDIT_SURFACE)=0.0D0
                ALENS(11,EDIT_SURFACE)=0.0D0
                ALENS(12,EDIT_SURFACE)=0.0D0
                ALENS(13,EDIT_SURFACE)=0.0D0
                ALENS(14,EDIT_SURFACE)=0.0D0
                ALENS(15,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_APETYPE=APETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(11,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(10,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(13,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(12,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(15,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (4)
C       OPEN DIALOG WINDOW FOR EDITTING ELIP CLAP DATA
        CALL WDialogLoad(IDD_APEELIP)
C       LOAD EXISTING NUMERIC WORDS
        APETYPE=3
        IF(APETYPE.NE.OLD_APETYPE) THEN
                ALENS(10,EDIT_SURFACE)=0.0D0
                ALENS(11,EDIT_SURFACE)=0.0D0
                ALENS(12,EDIT_SURFACE)=0.0D0
                ALENS(13,EDIT_SURFACE)=0.0D0
                ALENS(14,EDIT_SURFACE)=0.0D0
                ALENS(15,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_APETYPE=APETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(11,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(10,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(13,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(12,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(15,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (5)
C       OPEN DIALOG WINDOW FOR EDITTING RCTK CLAP DATA
        CALL WDialogLoad(IDD_APERCTK)
C       LOAD EXISTING NUMERIC WORDS
        APETYPE=4
        IF(APETYPE.NE.OLD_APETYPE) THEN
                ALENS(10,EDIT_SURFACE)=0.0D0
                ALENS(11,EDIT_SURFACE)=0.0D0
                ALENS(12,EDIT_SURFACE)=0.0D0
                ALENS(13,EDIT_SURFACE)=0.0D0
                ALENS(14,EDIT_SURFACE)=0.0D0
                ALENS(15,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_APETYPE=APETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(11,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(10,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(13,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(12,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(14,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW6,ALENS(15,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (6)
C       OPEN DIALOG WINDOW FOR EDITTING POLY CLAP DATA
        CALL WDialogLoad(IDD_APEPOLY)
C       LOAD EXISTING NUMERIC WORDS
        APETYPE=5
        IF(APETYPE.NE.OLD_APETYPE) THEN
                ALENS(10,EDIT_SURFACE)=0.0D0
                ALENS(11,EDIT_SURFACE)=3.0D0
                ALENS(12,EDIT_SURFACE)=0.0D0
                ALENS(13,EDIT_SURFACE)=0.0D0
                ALENS(14,EDIT_SURFACE)=0.0D0
                ALENS(15,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_APETYPE=APETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(10,EDIT_SURFACE))
        Call WDialogPutInteger(IDF_NW2,INT(ALENS(11,EDIT_SURFACE)))
        Call WDialogPutDouble(IDF_NW3,ALENS(13,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(12,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(15,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        END SELECT
C
        CASE (ID_EDCOBS)
C       EDIT STUFF
        CALL WDIALOGGETRADIOBUTTON(IDF_COBS1,JSET)
        SELECT CASE (JSET)
C
        CASE (1)
                        ALENS(16,EDIT_SURFACE)=1.0D0
                        ALENS(17,EDIT_SURFACE)=0.0D0
                        ALENS(18,EDIT_SURFACE)=0.0D0
                        ALENS(19,EDIT_SURFACE)=0.0D0
                        ALENS(20,EDIT_SURFACE)=0.0D0
                        ALENS(21,EDIT_SURFACE)=0.0D0
                        ALENS(22,EDIT_SURFACE)=0.0D0
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR COBS DATA
        CALL WDialogLoad(IDD_APECIRC2)
C       LOAD EXISTING NUMERIC WORDS
        COBSTYPE=1
        OLD_COBSTYPE=COBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(17,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(20,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(19,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (2)
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR OBS DATA
        CALL WDialogLoad(IDD_APECIRC2)
C       LOAD EXISTING NUMERIC WORDS
        COBSTYPE=1
        IF(COBSTYPE.NE.OLD_COBSTYPE) THEN
                ALENS(17,EDIT_SURFACE)=0.0D0
                ALENS(18,EDIT_SURFACE)=0.0D0
                ALENS(19,EDIT_SURFACE)=0.0D0
                ALENS(20,EDIT_SURFACE)=0.0D0
                ALENS(21,EDIT_SURFACE)=0.0D0
                ALENS(22,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_COBSTYPE=COBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(17,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(20,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(19,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (3)
C       OPEN DIALOG WINDOW FOR EDITTING RECT COBS DATA
        CALL WDialogLoad(IDD_APERECT2)
C       LOAD EXISTING NUMERIC WORDS
        COBSTYPE=2
        IF(COBSTYPE.NE.OLD_COBSTYPE) THEN
                ALENS(17,EDIT_SURFACE)=0.0D0
                ALENS(18,EDIT_SURFACE)=0.0D0
                ALENS(19,EDIT_SURFACE)=0.0D0
                ALENS(20,EDIT_SURFACE)=0.0D0
                ALENS(21,EDIT_SURFACE)=0.0D0
                ALENS(22,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_COBSTYPE=COBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(18,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(17,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(20,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(19,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(22,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (4)
C       OPEN DIALOG WINDOW FOR EDITTING ELIP COBS DATA
        CALL WDialogLoad(IDD_APEELIP2)
C       LOAD EXISTING NUMERIC WORDS
        COBSTYPE=3
        IF(COBSTYPE.NE.OLD_COBSTYPE) THEN
                ALENS(17,EDIT_SURFACE)=0.0D0
                ALENS(18,EDIT_SURFACE)=0.0D0
                ALENS(19,EDIT_SURFACE)=0.0D0
                ALENS(20,EDIT_SURFACE)=0.0D0
                ALENS(21,EDIT_SURFACE)=0.0D0
                ALENS(22,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_COBSTYPE=COBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(18,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(17,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(20,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(19,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(22,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (5)
C       OPEN DIALOG WINDOW FOR EDITTING RCTK COBS DATA
        CALL WDialogLoad(IDD_APERCTK2)
C       LOAD EXISTING NUMERIC WORDS
        COBSTYPE=4
        IF(COBSTYPE.NE.OLD_COBSTYPE) THEN
                ALENS(17,EDIT_SURFACE)=0.0D0
                ALENS(18,EDIT_SURFACE)=0.0D0
                ALENS(19,EDIT_SURFACE)=0.0D0
                ALENS(20,EDIT_SURFACE)=0.0D0
                ALENS(21,EDIT_SURFACE)=0.0D0
                ALENS(22,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_COBSTYPE=COBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(18,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(17,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(20,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(19,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(21,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW6,ALENS(22,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (6)
C       OPEN DIALOG WINDOW FOR EDITTING POLY COBS DATA
        CALL WDialogLoad(IDD_APEPOLY2)
C       LOAD EXISTING NUMERIC WORDS
        COBSTYPE=5
        IF(COBSTYPE.NE.OLD_COBSTYPE) THEN
                ALENS(17,EDIT_SURFACE)=0.0D0
                ALENS(18,EDIT_SURFACE)=3.0D0
                ALENS(19,EDIT_SURFACE)=0.0D0
                ALENS(20,EDIT_SURFACE)=0.0D0
                ALENS(21,EDIT_SURFACE)=0.0D0
                ALENS(22,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_COBSTYPE=COBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(17,EDIT_SURFACE))
        Call WDialogPutInteger(IDF_NW2,INT(ALENS(18,EDIT_SURFACE)))
        Call WDialogPutDouble(IDF_NW3,ALENS(20,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(19,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(22,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        END SELECT
C
        CASE (ID_EDCLAPE)
C       EDIT STUFF
        CALL WDIALOGGETRADIOBUTTON(IDF_EAPE1,KSET)
        SELECT CASE (KSET)
C
        CASE (1)
                        ALENS(51,EDIT_SURFACE)=1.0D0
                        ALENS(52,EDIT_SURFACE)=0.0D0
                        ALENS(53,EDIT_SURFACE)=0.0D0
                        ALENS(54,EDIT_SURFACE)=0.0D0
                        ALENS(55,EDIT_SURFACE)=0.0D0
                        ALENS(56,EDIT_SURFACE)=0.0D0
                        ALENS(57,EDIT_SURFACE)=0.0D0
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR CLAPE DATA
        CALL WDialogLoad(IDD_APECIRC3)
C       LOAD EXISTING NUMERIC WORDS
        EAPETYPE=1
        OLD_EAPETYPE=EAPETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(52,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(55,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(54,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (2)
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR CLAPE DATA
        CALL WDialogLoad(IDD_APECIRC3)
C       LOAD EXISTING NUMERIC WORDS
        EAPETYPE=1
        IF(EAPETYPE.NE.OLD_EAPETYPE) THEN
                ALENS(52,EDIT_SURFACE)=0.0D0
                ALENS(53,EDIT_SURFACE)=0.0D0
                ALENS(54,EDIT_SURFACE)=0.0D0
                ALENS(55,EDIT_SURFACE)=0.0D0
                ALENS(56,EDIT_SURFACE)=0.0D0
                ALENS(57,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_EAPETYPE=EAPETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(52,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(55,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(54,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (3)
C       OPEN DIALOG WINDOW FOR EDITTING RECT CLAPE DATA
        CALL WDialogLoad(IDD_APERECT3)
C       LOAD EXISTING NUMERIC WORDS
        EAPETYPE=2
        IF(EAPETYPE.NE.OLD_EAPETYPE) THEN
                ALENS(52,EDIT_SURFACE)=0.0D0
                ALENS(53,EDIT_SURFACE)=0.0D0
                ALENS(54,EDIT_SURFACE)=0.0D0
                ALENS(55,EDIT_SURFACE)=0.0D0
                ALENS(56,EDIT_SURFACE)=0.0D0
                ALENS(57,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_EAPETYPE=EAPETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(53,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(52,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(55,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(54,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(57,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (4)
C       OPEN DIALOG WINDOW FOR EDITTING ELIP CLAPE DATA
        CALL WDialogLoad(IDD_APEELIP3)
C       LOAD EXISTING NUMERIC WORDS
        EAPETYPE=3
        IF(EAPETYPE.NE.OLD_EAPETYPE) THEN
                ALENS(52,EDIT_SURFACE)=0.0D0
                ALENS(53,EDIT_SURFACE)=0.0D0
                ALENS(54,EDIT_SURFACE)=0.0D0
                ALENS(55,EDIT_SURFACE)=0.0D0
                ALENS(56,EDIT_SURFACE)=0.0D0
                ALENS(57,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_EAPETYPE=EAPETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(53,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(52,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(55,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(54,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(57,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (5)
C       OPEN DIALOG WINDOW FOR EDITTING RCTK CLAPE DATA
        CALL WDialogLoad(IDD_APERCTK3)
C       LOAD EXISTING NUMERIC WORDS
        EAPETYPE=4
        IF(EAPETYPE.NE.OLD_EAPETYPE) THEN
                ALENS(52,EDIT_SURFACE)=0.0D0
                ALENS(53,EDIT_SURFACE)=0.0D0
                ALENS(54,EDIT_SURFACE)=0.0D0
                ALENS(55,EDIT_SURFACE)=0.0D0
                ALENS(56,EDIT_SURFACE)=0.0D0
                ALENS(57,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_EAPETYPE=EAPETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(53,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(52,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(55,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(54,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(56,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW6,ALENS(57,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (6)
C       OPEN DIALOG WINDOW FOR EDITTING POLY CLAPE DATA
        CALL WDialogLoad(IDD_APEPOLY3)
C       LOAD EXISTING NUMERIC WORDS
        EAPETYPE=5
        IF(EAPETYPE.NE.OLD_EAPETYPE) THEN
                ALENS(52,EDIT_SURFACE)=0.0D0
                ALENS(53,EDIT_SURFACE)=3.0D0
                ALENS(54,EDIT_SURFACE)=0.0D0
                ALENS(55,EDIT_SURFACE)=0.0D0
                ALENS(56,EDIT_SURFACE)=0.0D0
                ALENS(57,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_EAPETYPE=EAPETYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(52,EDIT_SURFACE))
        Call WDialogPutInteger(IDF_NW2,INT(ALENS(53,EDIT_SURFACE)))
        Call WDialogPutDouble(IDF_NW3,ALENS(55,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(54,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(57,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        END SELECT
C
        CASE (ID_EDCOBSE)
C       EDIT STUFF
        CALL WDIALOGGETRADIOBUTTON(IDF_ECOBS1,LSET)
        SELECT CASE (LSET)
C
        CASE (1)
                        ALENS(61,EDIT_SURFACE)=1.0D0
                        ALENS(62,EDIT_SURFACE)=0.0D0
                        ALENS(63,EDIT_SURFACE)=0.0D0
                        ALENS(64,EDIT_SURFACE)=0.0D0
                        ALENS(65,EDIT_SURFACE)=0.0D0
                        ALENS(66,EDIT_SURFACE)=0.0D0
                        ALENS(67,EDIT_SURFACE)=0.0D0
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR COBSE DATA
        CALL WDialogLoad(IDD_APECIRC4)
C       LOAD EXISTING NUMERIC WORDS
        ECOBSTYPE=1
        OLD_ECOBSTYPE=ECOBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(62,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(65,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(64,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (2)
C       OPEN DIALOG WINDOW FOR EDITTING CIRCULAR OBS DATA
        CALL WDialogLoad(IDD_APECIRC4)
C       LOAD EXISTING NUMERIC WORDS
        ECOBSTYPE=1
        IF(ECOBSTYPE.NE.OLD_ECOBSTYPE) THEN
                ALENS(62,EDIT_SURFACE)=0.0D0
                ALENS(63,EDIT_SURFACE)=0.0D0
                ALENS(64,EDIT_SURFACE)=0.0D0
                ALENS(65,EDIT_SURFACE)=0.0D0
                ALENS(66,EDIT_SURFACE)=0.0D0
                ALENS(67,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_ECOBSTYPE=ECOBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(62,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(65,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(64,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (3)
C       OPEN DIALOG WINDOW FOR EDITTING RECT COBSE DATA
        CALL WDialogLoad(IDD_APERECT4)
C       LOAD EXISTING NUMERIC WORDS
        ECOBSTYPE=2
        IF(ECOBSTYPE.NE.OLD_ECOBSTYPE) THEN
                ALENS(62,EDIT_SURFACE)=0.0D0
                ALENS(63,EDIT_SURFACE)=0.0D0
                ALENS(64,EDIT_SURFACE)=0.0D0
                ALENS(65,EDIT_SURFACE)=0.0D0
                ALENS(66,EDIT_SURFACE)=0.0D0
                ALENS(67,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_ECOBSTYPE=ECOBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(63,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(62,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(65,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(64,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(67,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (4)
C       OPEN DIALOG WINDOW FOR EDITTING ELIP COBSE DATA
        CALL WDialogLoad(IDD_APEELIP4)
C       LOAD EXISTING NUMERIC WORDS
        ECOBSTYPE=3
        IF(ECOBSTYPE.NE.OLD_ECOBSTYPE) THEN
                ALENS(62,EDIT_SURFACE)=0.0D0
                ALENS(63,EDIT_SURFACE)=0.0D0
                ALENS(64,EDIT_SURFACE)=0.0D0
                ALENS(65,EDIT_SURFACE)=0.0D0
                ALENS(66,EDIT_SURFACE)=0.0D0
                ALENS(67,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_ECOBSTYPE=ECOBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(63,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(62,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(65,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(64,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(67,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (5)
C       OPEN DIALOG WINDOW FOR EDITTING RCTK COBSE DATA
        CALL WDialogLoad(IDD_APERCTK4)
C       LOAD EXISTING NUMERIC WORDS
        ECOBSTYPE=4
        IF(ECOBSTYPE.NE.OLD_ECOBSTYPE) THEN
                ALENS(62,EDIT_SURFACE)=0.0D0
                ALENS(63,EDIT_SURFACE)=0.0D0
                ALENS(64,EDIT_SURFACE)=0.0D0
                ALENS(65,EDIT_SURFACE)=0.0D0
                ALENS(66,EDIT_SURFACE)=0.0D0
                ALENS(67,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_ECOBSTYPE=ECOBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(63,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW2,ALENS(62,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW3,ALENS(65,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(64,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(66,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW6,ALENS(67,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (6)
C       OPEN DIALOG WINDOW FOR EDITTING POLY COBSE DATA
        CALL WDialogLoad(IDD_APEPOLY4)
C       LOAD EXISTING NUMERIC WORDS
        ECOBSTYPE=5
        IF(ECOBSTYPE.NE.OLD_ECOBSTYPE) THEN
                ALENS(62,EDIT_SURFACE)=0.0D0
                ALENS(63,EDIT_SURFACE)=3.0D0
                ALENS(64,EDIT_SURFACE)=0.0D0
                ALENS(65,EDIT_SURFACE)=0.0D0
                ALENS(66,EDIT_SURFACE)=0.0D0
                ALENS(67,EDIT_SURFACE)=0.0D0
                        END IF
        OLD_ECOBSTYPE=ECOBSTYPE
        Call WDialogPutDouble(IDF_NW1,ALENS(62,EDIT_SURFACE))
        Call WDialogPutInteger(IDF_NW2,INT(ALENS(63,EDIT_SURFACE)))
        Call WDialogPutDouble(IDF_NW3,ALENS(65,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW4,ALENS(64,EDIT_SURFACE))
        Call WDialogPutDouble(IDF_NW5,ALENS(67,EDIT_SURFACE))

        CALL WDialogShow(-1,-1,0,Modeless)

        END SELECT
C
        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        END SELECT
                        END IF
C
C
C
C       THIS CODE TAKES CARE OF CHANGING CLAP VALUES
C
        IF(MESSAGE%WIN.EQ.IDD_APECIRC) THEN
C       CIRCULAR
        CALL WDIALOGSELECT(IDD_APECIRC)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP '//AW1//' '//AW3//' '//AW2//' '//AW4//' '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERECT) THEN
C       RECTANGULAR
        CALL WDIALOGSELECT(IDD_APERECT)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP RECT '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='CLAP TILT '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEELIP) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEELIP)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP ELIP '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='CLAP TILT '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERCTK) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APERCTK)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        Call WDialogGetDouble(IDF_NW6,DW6)
        CALL DTOA23(DW6,AW6)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP RCTK '//AW2//' '//AW1//' '//AW4//' '//AW3//' '//AW5
        CALL PROCES
        INPUT='CLAP TILT '//AW6
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEPOLY) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEPOLY)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetInteger(IDF_NW2,IW2)
        CALL ITOAA(IW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP POLY '//AW1//' '//AW2//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='CLAP TILT '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
C
C       THIS CODE TAKES CARE OF CHANGING COBS VALUES
C
        IF(MESSAGE%WIN.EQ.IDD_APECIRC2) THEN
C       CIRCULAR
        CALL WDIALOGSELECT(IDD_APECIRC2)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS '//AW1//' '//AW3//' '//AW2
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERECT2) THEN
C       RECTANGULAR
        CALL WDIALOGSELECT(IDD_APERECT2)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS RECT '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='COBS TILT '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEELIP2) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEELIP2)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS ELIP '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='COBS TILT '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERCTK2) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APERCTK2)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        Call WDialogGetDouble(IDF_NW6,DW6)
        CALL DTOA23(DW6,AW6)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS RCTK '//AW2//' '//AW1//' '//AW4//' '//AW3//' '//AW5
        CALL PROCES
        INPUT='COBS TILT '//AW6
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEPOLY2) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEPOLY2)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetInteger(IDF_NW2,IW2)
        CALL ITOAA(IW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS POLY '//AW1//' '//AW2//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='COBS TILT '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
C
C       THIS CODE TAKES CARE OF CHANGING CLAPE VALUES
C
        IF(MESSAGE%WIN.EQ.IDD_APECIRC3) THEN
C       CIRCULAR
        CALL WDIALOGSELECT(IDD_APECIRC3)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP ERASE '//AW1//' '//AW3//' '//AW2
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERECT3) THEN
C       RECTANGULAR
        CALL WDIALOGSELECT(IDD_APERECT3)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP RECTE '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='CLAP TILTE '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEELIP3) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEELIP3)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP ELIPE '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='CLAP TILTE '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERCTK3) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APERCTK3)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        Call WDialogGetDouble(IDF_NW6,DW6)
        CALL DTOA23(DW6,AW6)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP RCTKE '//AW2//' '//AW1//' '//AW4//' '//AW3//' '//AW5
        CALL PROCES
        INPUT='CLAP TILTE '//AW6
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEPOLY3) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEPOLY3)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetInteger(IDF_NW2,IW2)
        CALL ITOAA(IW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        Call WDialogGetDouble(IDF_NW6,DW6)
        CALL DTOA23(DW6,AW6)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CLAP POLYE '//AW1//' '//AW2//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='CLAP TILTE '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
C
C       THIS CODE TAKES CARE OF CHANGING COBSE VALUES
C
        IF(MESSAGE%WIN.EQ.IDD_APECIRC4) THEN
C       CIRCULAR
        CALL WDIALOGSELECT(IDD_APECIRC4)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS ERASE '//AW1//' '//AW3//' '//AW2
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERECT4) THEN
C       RECTANGULAR
        CALL WDIALOGSELECT(IDD_APERECT4)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS RECTE '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='COBS TILTE '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEELIP4) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEELIP4)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS ELIPE '//AW2//' '//AW1//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='COBS TILTE '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APERCTK4) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APERCTK4)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_NW2,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        Call WDialogGetDouble(IDF_NW6,DW6)
        CALL DTOA23(DW6,AW6)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS RCTKE '//AW2//' '//AW1//' '//AW4//' '//AW3//' '//AW5
        CALL PROCES
        INPUT='COBS TILTE '//AW6
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
        IF(MESSAGE%WIN.EQ.IDD_APEPOLY4) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_APEPOLY4)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_NW1,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetInteger(IDF_NW2,IW2)
        CALL ITOAA(IW2,AW2)
        Call WDialogGetDouble(IDF_NW3,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_NW4,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_NW5,DW5)
        CALL DTOA23(DW5,AW5)
        Call WDialogGetDouble(IDF_NW6,DW6)
        CALL DTOA23(DW6,AW6)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='COBS POLYE '//AW1//' '//AW2//' '//AW4//' '//AW3
        CALL PROCES
        INPUT='COBS TILTE '//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
C
        IF(MESSAGE%WIN.EQ.IDD_MCLAP) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_MCLAP)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
C       DO NEEDED STUFF
        CALL WDIALOGGETDOUBLE(IDF_X,DW1)
        CALL WDIALOGGETDOUBLE(IDF_Y,DW2)
        CALL WDIALOGGETDOUBLE(IDF_GAMMA,DW3)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        CALL DTOA23(DW3,AW3)
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        J=ISET
        CALL ITOAA(J,AJ)
        INPUT='MULTCLAP '//AJ//','//AW1//','//AW2//','//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
C
        IF(MESSAGE%WIN.EQ.IDD_MCOBS) THEN
C       ELLIPTICAL
        CALL WDIALOGSELECT(IDD_MCOBS)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
C       DO NEEDED STUFF
        CALL WDIALOGGETDOUBLE(IDF_X,DW1)
        CALL WDIALOGGETDOUBLE(IDF_Y,DW2)
        CALL WDIALOGGETDOUBLE(IDF_GAMMA,DW3)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        CALL DTOA23(DW3,AW3)
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG,'//AJ
        CALL PROCES
        J=ISET
        CALL ITOAA(J,AJ)
        INPUT='MULTCOBS '//AJ//','//AW1//','//AW2//','//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        CALL WDIALOGSELECT(IDD_CLAPS)
        INCLUDE 'FRESHCLAP.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                           END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       TILT AND DECENTER HANDLING
C       THIS CODE TAKES CARE OF THE OPERATION OF THE TILTS
C       AND DECENTERS
C
        IF(MESSAGE%WIN.EQ.IDD_TILTS) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILTS)
        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDOK)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (IDCANCEL)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)


        CASE (IDF_PIV)
C       OPEN THE PIVAX DIALOG
        CALL WDIALOGLOAD(IDD_PIVAX)
        CALL WDIALOGSELECT(IDD_PIVAX)
        J=EDIT_SURFACE
        IF(ALENS(59,J).EQ.0.0D0)
     1  CALL WDIALOGPUTRADIOBUTTON(IDF_PIVAX1)
        IF(ALENS(59,J).NE.0.0D0)
     1  CALL WDIALOGPUTRADIOBUTTON(IDF_PIVAX2)
        CALL WDIALOGPUTDOUBLE(IDF_PIVX,ALENS(78,J))
        CALL WDIALOGPUTDOUBLE(IDF_PIVY,ALENS(79,J))
        CALL WDIALOGPUTDOUBLE(IDF_PIVZ,ALENS(80,J))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_TILTD)
C       DELETE ANY EXISTING TILT AND DECENTERS
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILTD'
        CALL PROCES
        INPUT='DEC 0.0 0.0 0.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT0)
        CALL WDIALOGCLEARFIELD(IDF_TILT7)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_TILTAUTOD)
C       DELETE ANY EXISTING TILT AUTOS
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT AUTOD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_DARD)
C       SIMPLIFY TILT DAR
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT DARD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BEND)
C       SIMPLIFY TILT BEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT BEND'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_EDTILT)
C       EDIT TILT VALUES
        ISET=0
        CALL WDIALOGGETRADIOBUTTON(IDF_TILT0,ISET)

        SELECT CASE (ISET)
        CASE (1)
C       NO TILT
C       DO NOTHING
C
        CASE (2)
C       TILT
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT1)
C       LOAD AND SHOW THE TILT DIALOG
        CALL WDIALOGLOAD(IDD_TILT)
        CALL WDIALOGPUTDOUBLE(IDF_ALPHA,ALENS(26,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_BETA,ALENS(27,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,ALENS(28,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (3)
C       RTILT
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT2)
C       LOAD AND SHOW THE RTILT DIALOG
        CALL WDIALOGLOAD(IDD_RTILT)
        CALL WDIALOGPUTDOUBLE(IDF_ALPHA,ALENS(26,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_BETA,ALENS(27,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,ALENS(28,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (4)
C       TILT AUTO
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT3)
C       LOAD AND SHOW THE TILT AUTO DIALOG
        CALL WDIALOGLOAD(IDD_TILTAUTO)
        CALL WDIALOGPUTDOUBLE(IDF_ALPHA,ALENS(26,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_BETA,ALENS(27,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,ALENS(28,EDIT_SURFACE))
        IF(ALENS(25,EDIT_SURFACE).EQ.2.0D0)
     1  CALL WDIALOGPUTRADIOBUTTON(IDF_AUTO1)
        IF(ALENS(25,EDIT_SURFACE).EQ.3.0D0)
     1  CALL WDIALOGPUTRADIOBUTTON(IDF_AUTO2)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (5)
C       TILT BEN
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT4)
C       LOAD AND SHOW THE TILT BEN DIALOG
        CALL WDIALOGLOAD(IDD_TILTBEN)
        CALL WDIALOGPUTDOUBLE(IDF_ALPHA,ALENS(26,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_BETA,ALENS(27,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (6)
C       TILT DAR
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT5)
C       LOAD AND SHOW THE TILT DAR DIALOG
        CALL WDIALOGLOAD(IDD_TILTDAR)
        CALL WDIALOGPUTDOUBLE(IDF_ALPHA,ALENS(26,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_BETA,ALENS(27,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,ALENS(28,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (7)
C       TILT RET
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT6)
C       LOAD AND SHOW THE TILT RET DIALOG
        CALL WDIALOGLOAD(IDD_TILTRET)
        CALL WDIALOGPUTINTEGER(IDF_RETSURF,INT(ALENS(70,EDIT_SURFACE)))
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (8)
C       TILT REV
        CALL WDIALOGPUTRADIOBUTTON(IDF_TILT8)
C       LOAD AND SHOW THE TILT REV DIALOG
        CALL WDIALOGLOAD(IDD_TILTREV)
        CALL WDIALOGPUTDOUBLE(IDF_ALPHA,ALENS(26,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_BETA,ALENS(27,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_GAMMA,ALENS(28,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
C
        CASE (IDF_EDDEC)
C       EDIT DEC VALUES
C
C       DECENTER
C       LOAD AND SHOW THE DECENTER DIALOG
        CALL WDIALOGLOAD(IDD_DEC)
        CALL WDIALOGPUTDOUBLE(IDF_XDEC,ALENS(31,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_YDEC,ALENS(30,EDIT_SURFACE))
        CALL WDIALOGPUTDOUBLE(IDF_ZDEC,ALENS(69,EDIT_SURFACE))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
                           END IF

C
C       ACTIONS INSIDE THE TILT DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_TILT) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILT)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_ALPHA,DALPHA)
        CALL DTOA23(DALPHA,AW1)
        Call WDialogGetDouble(IDF_BETA,DBETA)
        CALL DTOA23(DBETA,AW2)
        Call WDialogGetDouble(IDF_GAMMA,DGAMMA)
        CALL DTOA23(DGAMMA,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT '//AW1//' '//AW2//' '//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C       ACTIONS INSIDE THE RTILT DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_RTILT) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_RTILT)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_ALPHA,DALPHA)
        CALL DTOA23(DALPHA,AW1)
        Call WDialogGetDouble(IDF_BETA,DBETA)
        CALL DTOA23(DBETA,AW2)
        Call WDialogGetDouble(IDF_GAMMA,DGAMMA)
        CALL DTOA23(DGAMMA,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='RTILT '//AW1//' '//AW2//' '//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C       ACTIONS INSIDE THE TILT AUTO DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_TILTAUTO) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILTAUTO)
        ISET=0
        CALL WDIALOGGETRADIOBUTTON(IDF_AUTO1,ISET)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_ALPHA,DALPHA)
        CALL DTOA23(DALPHA,AW1)
        Call WDialogGetDouble(IDF_BETA,DBETA)
        CALL DTOA23(DBETA,AW2)
        Call WDialogGetDouble(IDF_GAMMA,DGAMMA)
        CALL DTOA23(DGAMMA,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1)
     1  INPUT='TILT AUTO '//AW1//' '//AW2//' '//AW3
        IF(ISET.EQ.2)
     1  INPUT='TILT AUTOM '//AW1//' '//AW2//' '//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C       ACTIONS INSIDE THE TILT BEN DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_TILTBEN) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILTBEN)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_ALPHA,DALPHA)
        CALL DTOA23(DALPHA,AW1)
        Call WDialogGetDouble(IDF_BETA,DBETA)
        CALL DTOA23(DBETA,AW2)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT BEN '//AW1//' '//AW2
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C       ACTIONS INSIDE THE TILT REV DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_TILTREV) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILTREV)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_ALPHA,DALPHA)
        CALL DTOA23(DALPHA,AW1)
        Call WDialogGetDouble(IDF_BETA,DBETA)
        CALL DTOA23(DBETA,AW2)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT REV '//AW1//' '//AW2
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C       ACTIONS INSIDE THE TILT DAR DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_TILTDAR) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILTDAR)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_ALPHA,DALPHA)
        CALL DTOA23(DALPHA,AW1)
        Call WDialogGetDouble(IDF_BETA,DBETA)
        CALL DTOA23(DBETA,AW2)
        Call WDialogGetDouble(IDF_GAMMA,DGAMMA)
        CALL DTOA23(DGAMMA,AW3)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT DAR '//AW1//' '//AW2//' '//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C
C       ACTIONS INSIDE THE TILT RET DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_TILTRET) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_TILTRET)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetInteger(IDF_RETSURF,IW2)
        CALL ITOAA(IW2,AW1)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TILT RET '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
C
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF

C
C       ACTIONS INSIDE THE DEC DATA DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_DEC) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_DEC)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_XDEC,DDECX)
        Call WDialogGetDouble(IDF_YDEC,DDECY)
        Call WDialogGetDouble(IDF_ZDEC,DDECZ)
        INPUT='U L'
        CALL PROCES
        WRITE(INPUT,*) 'CHG ',EDIT_SURFACE
        CALL PROCES
        WRITE(INPUT,*) 'DEC,',DDECY,',',DDECX,',',DDECZ
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        IF(DDECX.NE.0.0D0.OR.DDECY.NE.0.0D0.OR.DDECZ
     1  .NE.0.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_TILT7)
        IF(DDECX.EQ.0.0D0.AND.DDECY.EQ.0.0D0.AND.DDECZ
     1  .EQ.0.0D0) CALL WDIALOGCLEARFIELD(IDF_TILT7)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C       ACTIONS INSIDE THE PIVAX DIALOG
        IF(MESSAGE%WIN.EQ.IDD_PIVAX) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_PIVAX)
        SELECT CASE(MESSAGE%VALUE1)
        CASE (IDF_PIVOTD)
C       SET EXISTING NUMERIC WORDS
        CALL WDIALOGSELECT(IDD_PIVAX)
        Call WDialogPutDouble(IDF_PIVX,0.0D0)
        Call WDialogPutDouble(IDF_PIVY,0.0D0)
        Call WDialogPutDouble(IDF_PIVZ,0.0D0)
        Call WDialogPutRadioButton(IDF_PIVAX1)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='PIVOTD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDIALOGSHOW(-1,-1,0,MODELESS)
C
        CASE (IDOK)
C       GET EXISTING NUMERIC WORDS
        Call WDialogGetDouble(IDF_PIVX,DW1)
        Call WDialogGetDouble(IDF_PIVY,DW2)
        Call WDialogGetDouble(IDF_PIVZ,DW3)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        CALL DTOA23(DW3,AW3)
        CALL WDIALOGGETRADIOBUTTON(IDF_PIVAX1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='PIVAXIS NORMAL'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                      END IF
        IF(ISET.EQ.2) THEN
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='PIVAXIS VERTEX'
        CALL PROCES
        INPUT='PIVOT,'//AW1//','//AW2//','//AW3
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                      END IF
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE(IDCANCEL)
C       DON'T DO ANYTHING
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_TILTS)
        CALL WDialogShow(-1,-1,0,Modeless)
        END SELECT
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       ASPHERIC AND TORIC HANDLING
        INPUT='OUT NULL'
        CALL PROCES
        INCLUDE 'ASPH.INC'
        INPUT='OUT TP'
        CALL PROCES

C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       PICKUPS AND SOLVES
C       THIS CODE TAKES CARE OF THE OPERATION OF
C       PIKUPS AND SOLVES
C
        IF(MESSAGE%WIN.EQ.IDD_PIKSLV) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_PIKSLV)

        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDF_PIKDALL)
C       DELETE ALL PIKUPS ON THIS SURFACE
C
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='PIKD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL PIKFRESH(J)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_SLVD)
C       DELETE ALL SOLVES ON THIS SURFACE
C
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CSD'
        CALL PROCES
        INPUT='TSD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL PIKFRESH(J)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_CSDX)
C       DELETE ALL XZ-PLANE CURVATURE SOLVES FOR THIS SURFACE
C
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CSDX'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL PIKFRESH(J)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_CSDY)
C       DELETE ALL YZ-PLANE CURVATURE SOLVES FOR THIS SURFACE
C
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CSDY'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL PIKFRESH(J)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_TSD)
C       DELETE ALL THICKNESS SOLVES FOR THIS SURFACE
C
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TSD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL PIKFRESH(J)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_B1)
C
        CALL WDIALOGLOAD(IDD_SLVED)
C
C       SOLVE EDITOR FOR THIS SURFACE
C       PY
        IF(SOLVE(6,J).EQ.1.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV1)
        IF(SOLVE(6,J).EQ.1.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(6,J).EQ.1.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(7,J))
C
C       PX
        IF(SOLVE(4,J).EQ.4.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV1)
        IF(SOLVE(4,J).EQ.4.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(4,J).EQ.1.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(3,J))
C
C       PCY
        IF(SOLVE(6,J).EQ.2.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV2)
        IF(SOLVE(6,J).EQ.2.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(6,J).EQ.2.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(7,J))
C
C       PCX
        IF(SOLVE(4,J).EQ.5.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV2)
        IF(SOLVE(4,J).EQ.5.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(4,J).EQ.5.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(3,J))
C
C       CAY
        IF(SOLVE(6,J).EQ.3.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV8)
        IF(SOLVE(6,J).EQ.3.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(6,J).EQ.3.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(7,J))
C
C       CAX
        IF(SOLVE(4,J).EQ.6.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV8)
        IF(SOLVE(4,J).EQ.6.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(4,J).EQ.6.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(3,J))
C
C       APY
        IF(SOLVE(8,J).EQ.1.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV9)
        IF(SOLVE(8,J).EQ.1.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.1.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       APX
        IF(SOLVE(2,J).EQ.8.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV9)
        IF(SOLVE(2,J).EQ.8.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.8.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))
C
C       PIY
        IF(SOLVE(8,J).EQ.2.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV4)
        IF(SOLVE(8,J).EQ.2.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.2.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       PIX
        IF(SOLVE(2,J).EQ.9.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV4)
        IF(SOLVE(2,J).EQ.9.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.9.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))
C
C       PUY
        IF(SOLVE(8,J).EQ.3.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV3)
        IF(SOLVE(8,J).EQ.3.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.3.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       PUX
        IF(SOLVE(2,J).EQ.10.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV3)
        IF(SOLVE(2,J).EQ.10.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.10.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))
C
C       APCY
        IF(SOLVE(8,J).EQ.4.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV10)
        IF(SOLVE(8,J).EQ.4.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.4.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       APCX
        IF(SOLVE(2,J).EQ.11.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV10)
        IF(SOLVE(2,J).EQ.11.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.11.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))
C
C       PICY
        IF(SOLVE(8,J).EQ.5.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV6)
        IF(SOLVE(8,J).EQ.5.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.5.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       PICX
        IF(SOLVE(2,J).EQ.12.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV6)
        IF(SOLVE(2,J).EQ.12.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.12.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))
C
C       PUCY
        IF(SOLVE(8,J).EQ.6.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV5)
        IF(SOLVE(8,J).EQ.6.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.6.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       PUCX
        IF(SOLVE(2,J).EQ.13.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV5)
        IF(SOLVE(2,J).EQ.13.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.13.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))
C
C       COCY
        IF(SOLVE(8,J).EQ.7.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV7)
        IF(SOLVE(8,J).EQ.7.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE1)
        IF(SOLVE(8,J).EQ.7.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(9,J))
C
C       COCX
        IF(SOLVE(2,J).EQ.14.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_SLV7)
        IF(SOLVE(2,J).EQ.14.0D0) CALL WDIALOGPUTRADIOBUTTON(IDF_PLANE2)
        IF(SOLVE(2,J).EQ.14.0D0)
     1  CALL WDIALOGPUTDOUBLE(IDF_TARVAL,SOLVE(1,J))

        CALL WDIALOGSELECT(IDD_SLVED)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDCANCEL)
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

C
        CASE (IDF_BP1)
        PTYPER=1
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Radius of Curvature Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP2)
        PTYPER=2
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Curvature Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP3)
        PTYPER=3
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Thickness Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP4)
        PTYPER=4
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Conic Constant Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP5)
        PTYPER=5
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'4th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP6)
        PTYPER=6
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'6th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP7)
        PTYPER=7
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'8th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP8)
        PTYPER=8
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'10th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP9)
        PTYPER=9
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Toric Curvature Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP10)
        PTYPER=10
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Toric Radius Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP11)
        PTYPER=11
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Surface Profile Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
C
        CASE (IDF_BP12)
        PTYPER=12
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,
     1  'Negated Surface Profile Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP13)
        PTYPER=13
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Y-Decenter Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP14)
        PTYPER=14
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'X-Decenter Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP15)
        PTYPER=15
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Alpha Angle Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP16)
        PTYPER=16
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Beta Angle Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP17)
        PTYPER=17
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Gamma Angle Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP18)
        PTYPER=18
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Clear Aperture Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP19)
        PTYPER=19
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Obscuration Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP20)
        PTYPER=20
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Glass Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP21)
        PTYPER=21
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Toric Conic Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP22)
        PTYPER=22
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,
     1  'Toric 4th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP23)
        PTYPER=23
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,
     1  'Toric 6th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP24)
        PTYPER=24
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,
     1  'Toric 8th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP25)
        PTYPER=25
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,
     1  'Toric 10th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP26)
        PTYPER=26
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'2nd Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP27)
        PTYPER=27
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'12th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP28)
        PTYPER=28
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'14th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP29)
        PTYPER=29
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'16th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP30)
        PTYPER=30
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'18th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP31)
        PTYPER=31
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'20th Order Aspheric Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP32)
        PTYPER=32
        CALL WDIALOGLOAD(IDD_PIKED3)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Overall Length Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTINTEGER(IDF_STOSURF,INT(PIKUP(3,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(4,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(5,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_STOSURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP33)
        PTYPER=33
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Z-Decenter Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP34)
        PTYPER=34
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'X-Pivot Point Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP35)
        PTYPER=35
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Y-Pivot Point Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP36)
        PTYPER=36
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Z-Pivot Point Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP37)
        PTYPER=37
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Global X-Position Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP38)
        PTYPER=38
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Global Y-Position Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP39)
        PTYPER=39
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Global Z-Position Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP40)
        PTYPER=40
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Global Alpha Tilt Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP41)
        PTYPER=41
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Global Beta Tilt Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP42)
        PTYPER=42
        CALL WDIALOGLOAD(IDD_PIKED1)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Global Gamma Tilt Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        CALL WDIALOGPUTDOUBLE(IDF_MULT,PIKUP(3,J,PTYPER))
        CALL WDIALOGPUTDOUBLE(IDF_ADD,PIKUP(4,J,PTYPER))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTDOUBLE(IDF_MULT,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ADD,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP43)
        PTYPER=43
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Grating Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_BP44)
        PTYPER=44
        CALL WDIALOGLOAD(IDD_PIKED2)
        CALL WDIALOGPUTINTEGER(IDF_SURFNUM,EDIT_SURFACE)
        CALL WDIALOGPUTSTRING(IDF_PTYPE,'Coating Pickup')
        IF(PIKUP(1,J,PTYPER).EQ.1.0D0) THEN
        CALL WDIALOGPUTINTEGER(IDF_STASURF,INT(PIKUP(2,J,PTYPER)))
        IF(PIKUP(6,J,PTYPER).EQ.0.0D0) PIKUP(6,J,PTYPER)=1.0D0
        CALL WDIALOGPUTINTEGER(IDF_CFG,INT(PIKUP(6,J,PTYPER)))
                                ELSE
        CALL WDIALOGPUTINTEGER(IDF_STASURF,J)
        CALL WDIALOGPUTINTEGER(IDF_CFG,0)
                                END IF
        CALL WDIALOGPUTCHECKBOX(IDF_SPECOPT,0)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                                END IF
C
C       HANDLE OPERATIONS INSIDE THE SOLVE EDITOR DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_SLVED) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_SLVED)

        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDCANCEL)
        CALL WDIALOGSELECT(IDD_SLVED)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDOK)
C
        CALL WDIALOGGETRADIOBUTTON(IDF_PLANE1,ISET)
        CALL WDIALOGGETRADIOBUTTON(IDF_SLV1,JSET)
        CALL WDIALOGGETDOUBLE(IDF_TARVAL,DW1)
        CALL DTOA23(DW1,AW1)
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        SELECT CASE (JSET)
C
                CASE (1)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='PY '//AW1
        IF(ISET.EQ.2) INPUT='PX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (2)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='PCY '//AW1
        IF(ISET.EQ.2) INPUT='PCX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (3)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='PUY '//AW1
        IF(ISET.EQ.2) INPUT='PUX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (4)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='PIY '//AW1
        IF(ISET.EQ.2) INPUT='PIX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (5)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='PUCY '//AW1
        IF(ISET.EQ.2) INPUT='PUCX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (6)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='PICY '//AW1
        IF(ISET.EQ.2) INPUT='PICX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (7)
        CALL WDIALOGGETDOUBLE(IDF_TARVAL,DW1)
        DW1=DBLE(INT(DW1))
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='COCY '//AW1
        IF(ISET.EQ.2) INPUT='COCX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (8)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='CAY '//AW1
        IF(ISET.EQ.2) INPUT='CAX '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (9)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='APY '
        IF(ISET.EQ.2) INPUT='APX '
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
                CASE (10)
        INPUT='U L'
        CALL PROCES
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='APCY '
        IF(ISET.EQ.2) INPUT='APCX '
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
        END SELECT
C       NOW GET OUT OF THE SOLVE EDITOR DIALOG
C
        CALL WDIALOGSELECT(IDD_SLVED)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
                        END IF
C
C       HANDLE OPERATIONS INSIDE THE PICKUP EDITOR #1 DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_PIKED1) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_PIKED1)

        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        IF(PTYPER.EQ.1) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP RD,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP RD,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.2) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP CV,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP CV,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.3) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP TH,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP TH,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.4) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP CC,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP CC,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.5) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AD,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AD,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.6) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AE,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AE,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.7) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AF,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AF,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.8) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AG,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AG,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.9) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP CVTOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP CVTOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.10) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP RDTOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP RDTOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.13) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP YD,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP YD,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.14) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP XD,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP XD,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.15) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP ALPHA,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP ALPHA,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.16) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP BETA,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP BETA,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.17) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GAMMA,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GAMMA,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.21) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP CCTOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP CCTOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.22) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP ADTOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP ADTOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.23) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AETOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AETOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.24) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AFTOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AFTOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.25) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AGTOR,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AGTOR,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.26) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AC,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AC,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.27) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AH,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AH,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.28) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AI,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AI,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.29) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AJ,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AJ,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.30) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AK,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AK,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.31) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP AL,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP AL,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.33) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP ZD,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP ZD,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.34) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP PIVX,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP PIVX,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.35) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP PIVY,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP PIVY,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.36) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP PIVZ,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP PIVZ,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.37) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GDX,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GDX,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.38) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GDY,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GDY,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.39) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GDZ,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GDZ,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.40) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GALPHA,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GALPHA,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.41) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GBETA,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GBETA,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.42) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        CALL WDIALOGGETCHECKBOX(IDF_SPECOPT,M)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        IF(L.EQ.1) L=0
        IF(L.GT.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP GGAMMA,'//AK//','//AW1//','//AW2//',,'//AL
        IF(M.EQ.1)
     1  INPUT='PIKUP GGAMMA,'//AK//','//AW1//','//','//'1.0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
C
C       NOW GET OUT
C
        PTYPER=0
        CALL WDIALOGSELECT(IDD_PIKED1)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        INCLUDE 'LOADSHEET.INC'
        CALL PIKFRESH(J)
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDCANCEL)
        PTYPER=0
        CALL WDIALOGSELECT(IDD_PIKED1)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                        END IF
C
        IF(MESSAGE%WIN.EQ.IDD_PIKED2) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_PIKED2)

        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        IF(PTYPER.EQ.11) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP PRO,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP PRO,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.12) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP NPRO,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP NPRO,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.18) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP CLAP,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP CLAP,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.19) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP COBS,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP COBS,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.20) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP GLASS,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP GLASS,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.43) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP GRT,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP GRT,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
        IF(PTYPER.EQ.44) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_CFG,L)
        IF(L.EQ.1) L=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(L.EQ.0)
     1  INPUT='PIKUP COATING,'//AK
        IF(L.EQ.1)
     1  INPUT='PIKUP COATING,'//AK//',,,,'//AL
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
C
C       NOW GET OUT
C
        PTYPER=0
        CALL WDIALOGSELECT(IDD_PIKED2)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        INCLUDE 'LOADSHEET.INC'
        CALL PIKFRESH(J)
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDCANCEL)
        PTYPER=0
        CALL WDIALOGSELECT(IDD_PIKED2)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                        END IF
        IF(MESSAGE%WIN.EQ.IDD_PIKED3) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_PIKED3)

        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        IF(PTYPER.EQ.32) THEN
        CALL WDIALOGGETINTEGER(IDF_STASURF,K)
        CALL WDIALOGGETINTEGER(IDF_STOSURF,L)
        CALL WDIALOGGETINTEGER(IDF_CFG,M)
        CALL WDIALOGGETDOUBLE(IDF_MULT,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ADD,DW2)
        IF(M.EQ.1) M=0
        CALL ITOAA(J,AJ)
        CALL ITOAA(K,AK)
        CALL ITOAA(L,AL)
        CALL ITOAA(M,AM)
        CALL DTOA23(DW1,AW1)
        CALL DTOA23(DW2,AW2)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        INPUT='CHG '//AJ
        CALL PROCES
        IF(M.EQ.0)
     1  INPUT='PIKUP THOAL,'//AK//','//AL//','//AW1//','//AW2
        IF(M.EQ.1)
     1  INPUT='PIKUP THOAL,'//AK//','//AL//','//AW1//','//AW2//','//AM
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        END IF
C
C       NOW GET OUT
C
        PTYPER=0
        CALL WDIALOGSELECT(IDD_PIKED3)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        INCLUDE 'LOADSHEET.INC'
        CALL PIKFRESH(J)
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDCANCEL)
        PTYPER=0
        CALL WDIALOGSELECT(IDD_PIKED3)
        CALL WDIALOGHIDE()
        CALL WDIALOGUNLOAD()
        CALL WDIALOGSELECT(IDD_PIKSLV)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        END SELECT
                        END IF
C
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       SPECIAL SURFACES
        INPUT='OUT NULL'
        CALL PROCES
        INCLUDE 'SPSRF.INC'
        INPUT='OUT TP'
        CALL PROCES
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       OTHER
        INCLUDE 'OTHER.INC'
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       ZERO
        INCLUDE 'ZERO.INC'
C*****************************************************************************************
C*****************************************************************************************
C
C*****************************************************************************************
C*****************************************************************************************
C       ARRAY SURFACES AND DIFFRACTION GRATINGS
        INCLUDE 'ARRAYGRT.INC'
C*****************************************************************************************
C*****************************************************************************************
C
C       MENU SELECTIONS

          CASE (MenuSelect)

C
C  Selection from menu, display about box if About option chosen.
C
C
C     ***********************************************************
C                      HELP MENU ITEMS GO HERE
C     ***********************************************************
C
              IF (MESSAGE%VALUE1 == ID_HELP_ABOUT) THEN
      CALL WDialogLoad(IDD_ABOUT)
      CALL WDialogShow(-1,-1,0,Modal)
      CALL WDialogUnload()
                   END IF
C       SHOW THE REFERENCE MANUAL
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_MANUAL) THEN
      CALL whelpfile('MANUAL.PDF')
                       END IF
C       SHOW THE TUTORIAL #1 MANUAL
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_TUT1) THEN
      CALL whelpfile('TUT1.PDF')
                       END IF
C       SHOW THE TUTORIAL #2 MANUAL
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_TUT2) THEN
      CALL whelpfile('TUT2.PDF')
                       END IF
C       SHOW THE ABERRATION PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_ABRPRMPT) THEN
      CALL whelpfile('ABRPRMPT.PDF')
      ITEM=ID_SYSTEM
                       END IF
C       SHOW THE CAD PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_CADPRMPT) THEN
      CALL whelpfile('CADPRMPT.PDF')
      ITEM=ID_SYSTEM
                       END IF
C       SHOW THE GENERAL COMMAND PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_CMDPRMPT) THEN
      CALL whelpfile('CMDPRMPT.PDF')
                       END IF
C       SHOW THE GRAPHICS PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_GRAPRMPT) THEN
      CALL whelpfile('GRAPRMPT.PDF')
                       END IF
C       SHOW THE LENS DATABASE MANAGER PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_LENPRMPT) THEN
      CALL whelpfile('LENPRMPT.PDF')
                       END IF
C       SHOW THE MACRO PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_MACPRMPT) THEN
      CALL whelpfile('MACPRMPT.PDF')
                       END IF
C       SHOW THE NON-SEQUENTIAL DATABASE PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_NSSPRMPT) THEN
      CALL whelpfile('NSSPRMPT.PDF')
                       END IF
C       SHOW THE OPTIMIZATION PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_OPTPRMPT) THEN
      CALL whelpfile('OPTPRMPT.PDF')
                       END IF
C       SHOW THE SPECTRAL ANALYSIS PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_SPEPRMPT) THEN
      CALL whelpfile('SPEPRMPT.PDF')
                       END IF
C       SHOW THE FINANCIAL ANALYSIS PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_PROFPRMPT) THEN
      CALL whelpfile('PROFPRMPT.PDF')
                       END IF
C       SHOW THE TOLERANCING PROMPTING GUIDE
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_TOLPRMPT) THEN
      CALL whelpfile('TOLPRMPT.PDF')
                       END IF
C       SHOW WHAT'S NEW
C*****************************************************************************************
              IF (MESSAGE%VALUE1 == ID_WHATSNEW) THEN
      CALL whelpfile('WHATISNEW.PDF')
                       END IF
C     ***********************************************************
C                      END OF HELP MENU
C     ***********************************************************
C
C     MACRO MENU
C
C     ***********************************************************
C                      MACRO MENU ITEMS GO HERE
C     ***********************************************************
C
C*****************************************************************************************
C       MACRO MANIPULATION DIALOG BOX
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_MACRO_MANIPULATE) THEN
        CALL MACARRAY_LOAD(NUMINLIST)
        CALL WDialogLoad(IDD_MACRO)
        CALL WDialogPutMenu(IDF_MENU1,MACARRAY,NUMINLIST,1)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C
C       LIST HOW TO SAVE THE MACRO DIRECTORY TO DISK
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_MACROS_SAVE) THEN
       OUTLYNE='>MACSAVE'
       CALL SHOWIT(1)
                           INPUT='MACSAVE'
                           CALL PROCES
                       END IF
C       LIST HOW TO RESTORE THE MACRO DIRECTORY FROM DISK
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_MACROS_RESTORE) THEN
       OUTLYNE='>MACREST'
       CALL SHOWIT(1)
                           INPUT='MACREST'
                           CALL PROCES
                       END IF
C       SHOW THE MACRO PROMPTING GUIDE
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_MACRO_HELP_PRMPT) THEN
      CALL whelpfile('MACPRMPT.PDF')
                       END IF
C
C     ***********************************************************
C                      END OF MACRO MENU
C     ***********************************************************
C
C
C     ***********************************************************
C                      LENS MENU ITEMS GO HERE
C     ***********************************************************
C
C       RESTORE CURRENT LENS
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_LREST) THEN
       CALL IOsDirName(ONAME)
       IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
C      WINDOWS
       FNAME=TRIM(ONAME)//'\LENSES\'
                        ELSE
C      LINUX
       FNAME=TRIM(ONAME)//'/LENSES/'
                        END IF
       CALL WSELECTFILE('Lens File (*.prg)|*.prg|',
     1 Must Exist, FNAME, 'Restore Lens')
       IF(WINFODIALOG(EXITBUTTONCOMMON).EQ.COMMONOPEN) THEN
       IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
       TNAME=TRIM(ONAME)//'\CURLENS\LENSTEXT.DAT'
                        ELSE
       TNAME=TRIM(ONAME)//'/CURLENS/LENSTEXT.DAT'
                        END IF
       CALL IOsDeleteFile(TNAME)
       CALL IOsCopyFile(FNAME,TNAME)
       CALL IOsDirChange(ONAME)
       WRITE(OUTLYNE,*)'LENS FROM ',FNAME(1:69)
       CALL SHOWIT(1)
       WRITE(OUTLYNE,*)'IS NOW THE CURRENT LENS'
       CALL SHOWIT(1)
C*******************************************************************
C       LOAD INITIAL LENS DATA
        EXIS89=.FALSE.
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
        INQUIRE(FILE='CURLENS\LENSTEXT.DAT',EXIST=EXIS89)
                        ELSE
        INQUIRE(FILE='CURLENS/LENSTEXT.DAT',EXIST=EXIS89)
                        END IF
                IF(EXIS89) THEN
C       LENSTEXT EXISTS, READ IT
C
C               THIS IS THE LENSTEXT.DAT INPUT SECTION
C               THAT IS USED TO RESTORE THE LAST CURRENT LENS
C               FROM UNIT 89
C
C
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
         OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='CURLENS\LENSTEXT.DAT'
     2  ,STATUS='UNKNOWN')
                        ELSE
         OPEN(UNIT=89,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='CURLENS/LENSTEXT.DAT'
     2  ,STATUS='UNKNOWN')
                        END IF
        REWIND(UNIT=89)
 3141   READ(UNIT=89,FMT=100,END=8888,ERR=8887) INPUT(1:140)
        IF(INPUT(1:3).EQ.'OUT') GO TO 3141
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
      IF(INPUT(1:1).EQ.' ') INPUT(1:140)=INPUT(2:140)//' '
        IF(INPUT(1:8).EQ.'FLDSRAYS') THEN
                        IREND=500
                IF(INPUT(10:13).EQ.' 500') IREND=500
                IF(INPUT(10:13).EQ.'1000') IREND=1000
                IF(INPUT(10:13).EQ.'1500') IREND=1500
                IF(INPUT(10:13).EQ.'2000') IREND=2000
                IF(INPUT(10:13).EQ.'2500') IREND=2500
                IF(INPUT(10:13).EQ.'3000') IREND=3000
                IF(INPUT(10:13).EQ.'3500') IREND=3500
                IF(INPUT(10:13).EQ.'4000') IREND=4000
                IF(INPUT(10:13).EQ.'4500') IREND=4500
                IF(INPUT(10:13).EQ.'5000') IREND=5000
                DO I=1,200
        READ(89,*) AI,FIELDY(I),FIELDX(I),
     1FIELDZ(I),N3
        FIELDW(I)=DBLE(N3)
        IF(FIELDW(I).EQ.0.0D0) THEN
                FIELDW(I)=SYSTEM(11)
                END IF
                END DO
                DO I=1,IREND
        READ(89,*,ERR=8887,END=8887) AI4,RAYY(I),RAYX(I),N3
                RAYW(I)=DBLE(N3)
        IF(RAYW(I).EQ.0.0D0) THEN
        IF(I.GE.1.AND.I.LE.41) RAYW(I)=SYSTEM(11)
        IF(I.GE.42.AND.I.LE.82) RAYW(I)=SYSTEM(7)
        IF(I.GE.83.AND.I.LE.123) RAYW(I)=SYSTEM(8)
                END IF
                END DO
                ELSE
      IF(INPUT(1:1).EQ.'.'.OR.
     1INPUT(1:1).EQ.'1'.OR.
     1INPUT(1:1).EQ.'2'.OR.
     1INPUT(1:1).EQ.'3'.OR.
     1INPUT(1:1).EQ.'4'.OR.
     1INPUT(1:1).EQ.'5'.OR.
     1INPUT(1:1).EQ.'6'.OR.
     1INPUT(1:1).EQ.'7'.OR.
     1INPUT(1:1).EQ.'8'.OR.
     1INPUT(1:1).EQ.'9'.OR.
     1INPUT(1:1).EQ.'0'
     1.OR.INPUT(1:8).EQ.'FLDSRAYS') THEN
                ELSE
                CALL PROCES
                END IF
                END IF
                        GO TO 3141
C     LENSTEXT.DAT HAS BEEN LOADED
 8888   BACKSPACE(UNIT=89)
        REWIND (UNIT=89)
        CALL CLOSE_FILE(89,1)
                       GO TO 8889
 8887   BACKSPACE(UNIT=89)
        BACKSPACE(UNIT=89)
                        GO TO 3141
 8889                   CONTINUE
                        ELSE
C     NO LENSTEXT EXISTS,OR AN ERROR EXITSED
                        END IF
                       END IF
        INCLUDE 'LOADSHEET.INC'
                       END IF
C     ***********************************************************
C       SAVE CURRENT LENS
C*****************************************************************************************

C*******************************************************************
C*******************************************************************
                               IF (MESSAGE%VALUE1 == ID_LSAVE) THEN
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
                INPUT='OUT FILE CURLENS\LENSTEXT.DAT'
                        ELSE
                INPUT='OUT FILE CURLENS/LENSTEXT.DAT'
                        END IF
                CALL PROCES
                INPUT='LENO'
                CALL PROCES
                INPUT='OUT TP'
                CALL PROCES
       CALL IOsDirName(ONAME)
                IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
       TNAME=TRIM(ONAME)//'\LENSES\'
       FNAME=TRIM(ONAME)//'\CURLENS\LENSTEXT.DAT'
                        ELSE
       TNAME=TRIM(ONAME)//'/LENSES/'
       FNAME=TRIM(ONAME)//'/CURLENS/LENSTEXT.DAT'
                        END IF
       CALL WSELECTFILE('Lens File (*.prg)|*.prg|',
     1 45,TNAME, 'Save Lens')
       IF(WINFODIALOG(EXITBUTTONCOMMON).EQ.COMMONOPEN) THEN
       CALL IOsDeleteFile(TNAME)
       CALL IOsCopyFile(FNAME,TNAME)
       WRITE(OUTLYNE,*)'CURRENT LENS SAVED TO ',TNAME(1:69)
       CALL SHOWIT(1)
       CALL IOsDirChange(ONAME)
                                END IF
                                END IF
C     ***********************************************************
C       EDIT LENS
C*****************************************************************************************

C*******************************************************************
C*******************************************************************
       IF (MESSAGE%VALUE1 == ID_LNEWLENS_RD) THEN
        RDM=.TRUE.
C       SAVE CURRENT LENS TO \CURLENS\LASTCURLENS.DAT
C       INITIALIZE A THREE SURFACE NEW LENS
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
                INPUT='OUT FILE CURLENS\LASTCURLENS.DAT'
                        ELSE
                INPUT='OUT FILE CURLENS/LASTCURLENS.DAT'
                        END IF
                CALL PROCES
                INPUT='LENO'
                CALL PROCES
                INPUT='OUT TP'
                CALL PROCES
                INPUT='LENS'
                CALL PROCES
                INPUT='TH 1.0D30'
                CALL PROCES
                INPUT='AIR'
                CALL PROCES
                INPUT='AIR'
                CALL PROCES
                INPUT='AIR'
                CALL PROCES
                INPUT='EOS'
                CALL PROCES
c       CALL WDialogUnLoad()
        CALL WDialogLoad(IDD_SURFACEEDITOR_RD)
        EDITOROPEN=.TRUE.
C
C       MAKE A BACKUP FOR UNDO
C
C       DUMP CURRENT LENS TO LENSBAK.DAT SO WE CAN UNDO
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
        INPUT='OUT FILE CURLENS\LENSBACK.DAT'
                        ELSE
        INPUT='OUT FILE CURLENS/LENSBACK.DAT'
                        END IF
        CALL PROCES
        INPUT='LENO'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INPUT='IN TP'
        CALL PROCES
C
        INCLUDE 'LOADSHEET.INC'
C
C       PROCEED OPENING THE LENS EDITOR
                                END IF
       IF (MESSAGE%VALUE1 == ID_LNEWLENS_CV) THEN
        RDM=.FALSE.
C       SAVE CURRENT LENS TO \CURLENS\LASTCURLENS.DAT
C       INITIALIZE A THREE SURFACE NEW LENS
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
                INPUT='OUT FILE CURLENS\LASTCURLENS.DAT'
                        ELSE
                INPUT='OUT FILE CURLENS/LASTCURLENS.DAT'
                        END IF
                CALL PROCES
                INPUT='LENO'
                CALL PROCES
                INPUT='OUT TP'
                CALL PROCES
                INPUT='LENS'
                CALL PROCES
                INPUT='TH 1.0D30'
                CALL PROCES
                INPUT='AIR'
                CALL PROCES
                INPUT='AIR'
                CALL PROCES
                INPUT='AIR'
                CALL PROCES
                INPUT='EOS'
                CALL PROCES
c       CALL WDialogUnLoad()
        CALL WDialogLoad(IDD_SURFACEEDITOR_RD)
        EDITOROPEN=.TRUE.
C
C       MAKE A BACKUP FOR UNDO
C
C       DUMP CURRENT LENS TO LENSBAK.DAT SO WE CAN UNDO
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
        INPUT='OUT FILE CURLENS\LENSBACK.DAT'
                        ELSE
        INPUT='OUT FILE CURLENS/LENSBACK.DAT'
                        END IF
        CALL PROCES
        INPUT='LENO'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INPUT='IN TP'
        CALL PROCES
C
        INCLUDE 'LOADSHEET.INC'
C
C       PROCEED OPENING THE LENS EDITOR
                                END IF
C
       IF (MESSAGE%VALUE1 == ID_LCURLENS_RD) THEN
        RDM=.TRUE.
        CALL WDialogLoad(IDD_SURFACEEDITOR_RD)
        EDITOROPEN=.TRUE.
C
C       MAKE A BACKUP FOR UNDO
C
C       DUMP CURRENT LENS TO LENSBAK.DAT SO WE CAN UNDO
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
        INPUT='OUT FILE CURLENS\LENSBACK.DAT'
                        ELSE
        INPUT='OUT FILE CURLENS/LENSBACK.DAT'
                        END IF
        CALL PROCES
        INPUT='LENO'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INPUT='IN TP'
        CALL PROCES
C
        INCLUDE 'LOADSHEET.INC'
C
C       PROCEED OPENING THE LENS EDITOR
                                END IF
       IF (MESSAGE%VALUE1 == ID_LCURLENS_CV) THEN
        RDM=.FALSE.
        CALL WDialogLoad(IDD_SURFACEEDITOR_RD)
        EDITOROPEN=.TRUE.
C
C       MAKE A BACKUP FOR UNDO
C
C       DUMP CURRENT LENS TO LENSBAK.DAT SO WE CAN UNDO
        IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
        INPUT='OUT FILE CURLENS\LENSBACK.DAT'
                        ELSE
        INPUT='OUT FILE CURLENS/LENSBACK.DAT'
                        END IF
        CALL PROCES
        INPUT='LENO'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        INPUT='IN TP'
        CALL PROCES
C
        INCLUDE 'LOADSHEET.INC'
C
C       PROCEED OPENING THE LENS EDITOR
                                END IF
       IF (MESSAGE%VALUE1 == ID_VIE) THEN
        CALL WDIALOGLOAD(IDD_VIE)
        CALL WDIALOGSELECT(IDD_VIE)
        CALL WDIALOGPUTINTEGER(IDF_STARTSURF,1)
        CALL WDIALOGPUTINTEGER(IDF_STOPSURF,INT(SYSTEM(20)))
        CALL WDIALOGPUTDOUBLE(IDF_FACTOR,0.10D0)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF


C       LENOCV
C*******************************************************************
       IF (MESSAGE%VALUE1 == ID_LENOCV) THEN
        CALL WDialogLoad(IDD_LENOCV)
        CALL WDialogSelect(IDD_LENOCV)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C
C       LENOACC
C*******************************************************************
       IF (MESSAGE%VALUE1 == ID_LENOACC) THEN
        CALL WDialogLoad(IDD_LENOACC)
        CALL WDialogSelect(IDD_LENOACC)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C       IMPORT ZEMAX LENS
C*******************************************************************
       IF (MESSAGE%VALUE1 == ID_IMPORT_ZMX) THEN
        CALL WDialogLoad(IDD_IMPORT_ZMX)
        CALL WDialogSelect(IDD_IMPORT_ZMX)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C
C       IMPORT CODE-V LENS
C*******************************************************************
       IF (MESSAGE%VALUE1 == ID_IMPORT_CV) THEN
        CALL WDialogLoad(IDD_IMPORT_CV)
        CALL WDialogSelect(IDD_IMPORT_CV)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C
C     ***********************************************************
C                      LENS LIBRARAY MENU ITEMS GO HERE
C     ***********************************************************
       IF (MESSAGE%VALUE1 == ID_LLIB) THEN
        CALL WDialogLoad(IDD_LLIB)
        CALL WDialogSelect(IDD_LLIB)
        CALL WDIALOGPUTRADIOBUTTON(IDF_MA1)
        INPUT='OUT NULL'
        CALL PROCES
        INPUT='LENDIR LIBLEN'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL LIBLOAD(LIBLIST)
        CALL WDialogPutMenu(IDF_LIBCONTENTS,LIBARRAY,LIBLIST,1)
        CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C
C     ***********************************************************
C                      GLASS MENU
C     ***********************************************************
       IF (MESSAGE%VALUE1 == ID_GLASSP) THEN
       CALL WDialogLoad(IDD_GLASSP)
        CALL GCATLOAD(CATLIST)
        CALL WDIALOGPUTDOUBLE(IDF_WV1,GLSWV(1))
        CALL WDIALOGPUTDOUBLE(IDF_WV2,GLSWV(2))
        CALL WDIALOGPUTDOUBLE(IDF_WV3,GLSWV(3))
        CALL WDIALOGPUTDOUBLE(IDF_WV4,GLSWV(4))
        CALL WDIALOGPUTDOUBLE(IDF_WV5,GLSWV(5))
        CALL WDIALOGCLEARFIELD(IDF_N1)
        CALL WDIALOGCLEARFIELD(IDF_N2)
        CALL WDIALOGCLEARFIELD(IDF_N3)
        CALL WDIALOGCLEARFIELD(IDF_N4)
        CALL WDIALOGCLEARFIELD(IDF_N5)
        CALL WDialogPutMenu(IDF_GLASSP,CATARRAY,CATLIST,1)
        CALL WDIALOGFIELDSTATE(IDF_GLASSP,1)
        INCLUDE 'GLASSPFRESH.INC'
       CALL WDialogShow(-1,-1,0,Modeless)
                                END IF
C     ***********************************************************
C                      END GLASS MENU
C     ***********************************************************
C       LENS DATA DISPLAY
C
C       LENS SURFACE DATA DISPLAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_RTG) THEN
                        INPUT='RTG ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_RTGLBL) THEN
                        INPUT='RTGLBL ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_CTG) THEN
                        INPUT='CTG ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_CTGLBL) THEN
                        INPUT='CTGLBL ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_DUMOUT) THEN
                        INPUT='DUMOUT ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_CAOB) THEN
                        INPUT='CAOB ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_SPIDER) THEN
                        INPUT='SPIDER ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_TAD) THEN
                        INPUT='TAD ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_PIVOT) THEN
                        INPUT='PIVOT ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_ASPH) THEN
                        INPUT='ASPH ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='ASPH2 ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_ARRAY) THEN
                        INPUT='ARRAY ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_DEFORM) THEN
                        INPUT='DEFORM ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_THM) THEN
                        INPUT='THM ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_TR) THEN
                        INPUT='TR ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_TC) THEN
                        INPUT='TC ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_TASPH) THEN
                        INPUT='TASPH ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_SLV) THEN
                        INPUT='SLV ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_PIK) THEN
                        INPUT='PIK ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_RIN) THEN
                        INPUT='RIN ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='RIN2 ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_NDEX) THEN
                        INPUT='NDEX ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='NDEX2 ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_FOOTBLOK) THEN
                        INPUT='FOOTBLOK ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_SPGR) THEN
                        INPUT='SPGR ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_PRICE) THEN
                        INPUT='PRICE ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_GRT) THEN
                        INPUT='GRT ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_PRSPR) THEN
                        INPUT='PRSPR ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_CONFIGS) THEN
                        INPUT='CONFIGS ALL'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
C       LENS NON-SURFACE DATA DISPLAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_LI) THEN
                        INPUT='LI'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='LIC'
                        CALL PROCES
                        END IF
       IF (MESSAGE%VALUE1 == ID_INI) THEN
                        INPUT='INI'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_LTYPE) THEN
                        INPUT='LTYPE'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_SPTWT) THEN
                        INPUT='SPTWT'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='CW'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='PCW'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='SCW'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_FIELD) THEN
C       REFERENCE OBJECT SPECIFICATION
        OUTLYNE='REFERENCE OBJECT SPECIFIED BY:'
        CALL SHOWIT(1)
        OUTLYNE=' '
        CALL SHOWIT(1)
        IF(SYSTEM(51).EQ.1.0D0) INPUT='SCY'
        IF(SYSTEM(51).EQ.1.0D0) CALL PROCES
        IF(SYSTEM(52).EQ.1.0D0) INPUT='SCY FANG'
        IF(SYSTEM(52).EQ.1.0D0) CALL PROCES
        IF(SYSTEM(53).EQ.1.0D0) INPUT='SCX'
        IF(SYSTEM(53).EQ.1.0D0) CALL PROCES
        IF(SYSTEM(54).EQ.1.0D0) INPUT='SCX FANG'
        IF(SYSTEM(54).EQ.1.0D0) CALL PROCES
C
        IF(SYSTEM(95).EQ.-1.0D0) INPUT='PYIM'
        IF(SYSTEM(95).EQ.-1.0D0) CALL PROCES
        IF(SYSTEM(95).EQ.1.0D0) INPUT='PYIM FANG'
        IF(SYSTEM(95).EQ.1.0D0) CALL PROCES
        IF(SYSTEM(94).EQ.-1.0D0) INPUT='PXIM'
        IF(SYSTEM(94).EQ.-1.0D0) CALL PROCES
        IF(SYSTEM(94).EQ.1.0D0) INPUT='PXIM FANG'
        IF(SYSTEM(94).EQ.1.0D0) CALL PROCES
C
        IF(SYSTEM(99).EQ.-1.0D0) INPUT='PYIM'
        IF(SYSTEM(99).EQ.-1.0D0) CALL PROCES
        IF(SYSTEM(99).EQ.1.0D0) INPUT='PYIM FANG'
        IF(SYSTEM(99).EQ.1.0D0) CALL PROCES
        IF(SYSTEM(98).EQ.-1.0D0) INPUT='PXIM'
        IF(SYSTEM(98).EQ.-1.0D0) CALL PROCES
        IF(SYSTEM(98).EQ.1.0D0) INPUT='PXIM FANG'
        IF(SYSTEM(98).EQ.1.0D0) CALL PROCES
        OUTLYNE='  '
        CALL SHOWIT(1)
        IF(CFLDCNT.EQ.0) THEN
        OUTLYNE='THERE ARE CURRENTLY NO MULTIPLE FIELD OF VIEW'
        CALL SHOWIT(1)
        OUTLYNE='POSITIONS DEFINED FOR USE IN MULTIPLE FIELD'
        CALL SHOWIT(1)
        OUTLYNE='OF VIEW ANALYSIS OPTIONS'
        CALL SHOWIT(1)
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
        IF(CFLDCNT.NE.0) THEN
                        INPUT='FLDSARE'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
                        END IF
       IF (MESSAGE%VALUE1 == ID_MODE) THEN
                        INPUT='MODE'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_UNITS) THEN
                        INPUT='UNITS'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
       IF (MESSAGE%VALUE1 == ID_STOP) THEN
                        INPUT='ASTOP'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        INPUT='REFS'
                        CALL PROCES
        OUTLYNE=' '
        CALL SHOWIT(1)
                        END IF
C
C     ***********************************************************
C       PARAXIAL MENU
C
C       MONOCHROMATIC PARAXIAL DATA DISPLAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_PAR1) THEN
        CALL WDialogLoad(IDD_PARAX1)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       CHROMATIC PARAXIAL DATA DISPLAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_PAR2) THEN
        CALL WDialogLoad(IDD_PARAX2)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       FIRST ORDER OPERATING CONDITIONS DISPLAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_PAR3) THEN
        CALL WDialogLoad(IDD_PARAX3)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF

C       EFL, BFL, FFL AND PRINCIPAL PLANE DISPLAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_PAR41) THEN
        CALL WDialogLoad(IDD_FIRD)
        CALL WDIALOGPUTINTEGER(IDF_FIRD1,0)
        CALL WDIALOGPUTINTEGER(IDF_FIRD2,INT(SYSTEM(20)))
        IF(SYSTEM(11).LT.6.0D0) WAVE_LENGTH=SYSTEM(INT(SYSTEM(11)))
        IF(SYSTEM(11).GT.5.0D0) WAVE_LENGTH=SYSTEM(INT(SYSTEM(11))+65)
        CALL WDIALOGPUTDOUBLE(IDF_FIRD3,WAVE_LENGTH)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C     ***********************************************************
C                      END OF PARAXIAL MENU
C     ***********************************************************
C       RAY MENU
C
C       SINGLE RAY
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_RAY1) THEN
        CALL WDialogLoad(IDD_RAY)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_STASURF,0)
        CALL WDIALOGPUTINTEGER(IDF_STOSURF,INT(SYSTEM(20)))
        CALL WDIALOGPUTDOUBLE(IDF_RAYXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_RAYYVALUE,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_RAYWAVE,INT(SYSTEM(11)))
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       SIMPLE FANS
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_FAN1) THEN
        CALL WDialogLoad(IDD_FAN1)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        CALL WDIALOGPUTDOUBLE(IDF_MAX,1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_MIN,-1.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_SSI,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_NUM,11)
        CALL WDIALOGPUTINTEGER(IDF_WAV,INT(SYSTEM(11)))
        CALL WDIALOGPUTRADIOBUTTON(IDF_GRA1)
        CALL WDIALOGPUTRADIOBUTTON(IDF_FT1)
        CALL WDIALOGPUTRADIOBUTTON(IDF_Q1)
        CALL WDIALOGPUTRADIOBUTTON(IDF_COL1)
        CALL WDIALOGPUTCHECKBOX(IDF_WMF,0)
        CALL WDIALOGPUTCHECKBOX(IDF_PLOT,1)
        CALL WDIALOGPUTCHECKBOX(IDF_AUTOSSI,1)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       ADVANCED RAY SETTINGS
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_RAY7) THEN
        CALL WDialogLoad(IDD_RAYSETTINGS)
        CALL WDIALOGPUTDOUBLE(IDF_SURTOL,SURTOL)
        CALL WDIALOGPUTDOUBLE(IDF_AIMTOL,AIMTOL)
        CALL WDIALOGPUTDOUBLE(IDF_CAIMTOL,CAIMTOL)
        CALL WDIALOGPUTINTEGER(IDF_NRAITR,NRAITR)
        CALL WDIALOGPUTINTEGER(IDF_NEWOBJ,NEWOBJ)
        CALL WDIALOGPUTINTEGER(IDF_NEWREF,NEWREF)
        CALL WDIALOGPUTINTEGER(IDF_NEWOBJ,NEWIMG)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C     ***********************************************************
C                      END OF RAY MENU
C     ***********************************************************
C       IMAGE EVALUATION MENU
C
C       APODIZATION SETTINGS
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_APOD) THEN
        CALL WDialogLoad(IDD_APOD)
        IF(APODGAUSS) THEN
        CALL WDIALOGPUTRADIOBUTTON(IDF_GAU1)
        CALL WDIALOGPUTDOUBLE(IDF_APODVAL,APODDBLOSS)
                ELSE
        CALL WDIALOGPUTRADIOBUTTON(IDF_UNI1)
        CALL WDIALOGPUTDOUBLE(IDF_APODVAL,0.0D0)
                END IF
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       DIFFRACTION SETTINGS
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_DIFSET) THEN
        CALL WDialogLoad(IDD_DIFSET)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       CAPFN
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_CAPFN) THEN
        CALL WDialogLoad(IDD_CAPFN)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVNUM,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVFIT,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_PLTWAV,INT(SYSTEM(11)))
        CALL WDIALOGPUTRADIOBUTTON(IDF_GRA1)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       DOTF
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_DOTF) THEN
        CALL WDialogLoad(IDD_DOTF)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVNUM,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVFIT,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_PLTWAV,INT(SYSTEM(11)))
        CALL WDIALOGPUTRADIOBUTTON(IDF_GRA1)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       PSF
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_PSF) THEN
        CALL WDialogLoad(IDD_PSF)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVNUM,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVFIT,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_PLTWAV,INT(SYSTEM(11)))
        CALL WDIALOGPUTRADIOBUTTON(IDF_GRA1)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_GRI,GRI)
        CALL WDIALOGPUTINTEGER(IDF_NRD,NRD)
        CALL WDIALOGPUTINTEGER(IDF_TGR,TGR)
        CALL WDIALOGPUTINTEGER(IDF_PGR,PGR)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       GEOMETRICAL SPOT DIAGRAMS
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_SPOT) THEN
        CALL WDialogLoad(IDD_SPOT)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        IF(SPDTYPE.EQ.1) THEN
C       RECT
        CALL WDIALOGPUTRADIOBUTTON(IDF_SPO2)
        CALL WDIALOGPUTINTEGER(IDF_RECT,NRECT)
                        END IF
        IF(SPDTYPE.EQ.2) THEN
C       RING
        CALL WDIALOGPUTRADIOBUTTON(IDF_SPO1)
        CALL WDIALOGPUTINTEGER(IDF_RINGS,INT(RINGTOT))
        CALL WDIALOGPUTINTEGER(IDF_RINGNUM,1)
        CALL WDIALOGPUTDOUBLE(IDF_RAD,RINGRAD(1))
        CALL WDIALOGPUTINTEGER(IDF_RAYS,RINGPNT(1))
        CALL WDIALOGPUTDOUBLE(IDF_THETA,RINGANG(1))

                        END IF
        IF(SPDTYPE.EQ.3) THEN
C       RAND
        CALL WDIALOGPUTRADIOBUTTON(IDF_SPO3)
        CALL WDIALOGPUTINTEGER(IDF_RECT,INT(RNUMBR))
                        END IF
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       DIST, AST, FLDCV
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_DISAST) THEN
        CALL WDialogLoad(IDD_DISAST)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       GOTF
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_GOTF) THEN
        CALL WDialogLoad(IDD_GOTF)
        CALL WDIALOGPUTINTEGER(IDF_FWAVE,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVNUM,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_WAVFIT,INT(SYSTEM(11)))
        CALL WDIALOGPUTINTEGER(IDF_PLTWAV,INT(SYSTEM(11)))
        CALL WDIALOGPUTRADIOBUTTON(IDF_GRA1)
        CALL WDIALOGPUTDOUBLE(IDF_FXVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FYVALUE,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_FZVALUE,0.0D0)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C     ***********************************************************
C                      END OF IMAGE EVALUATION MENU
C     ***********************************************************
C     ***********************************************************
C*****************************************************************************************
C*****************************************************************************************
C
C       CLS MENU ITEM
C
       IF (MESSAGE%VALUE1 == ID_CLS) THEN
        INPUT='CLS'
        CALL PROCES
                END IF

C*****************************************************************************************
C*****************************************************************************************
C       BLACK BODY MENU
C
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_BB) THEN
        CALL WDialogLoad(IDD_BB)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C     ***********************************************************
C                      END OF BLACK BODY MENU
C*****************************************************************************************
C       FONT MENU
C
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_FONT) THEN
        CALL WDialogLoad(IDD_EDITFONT)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C     ***********************************************************
C                      END OF FONT MENU
C     ***********************************************************
C     ***********************************************************
C     ***********************************************************
C     ***********************************************************
C       OPTIMIZATION MENU
C
C       VARIABLES
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_VARED) THEN
        CALL WDialogLoad(IDD_VARED)
C       LOAD THE SHEET
        CALL VARIFRESH
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C       OPTIMIZE
C*****************************************************************************************
       IF (MESSAGE%VALUE1 == ID_OPTIM) THEN
        CALL WDialogLoad(IDD_OPTIM)
        CALL WDIALOGPUTDOUBLE(IDF_PFAC,PFAC)
        CALL WDialogShow(-1,-1,0,Modeless)
                        END IF
C
C     ***********************************************************
C                      END OF OPTIM MENU
C     ***********************************************************
C     ***********************************************************
C              EXIT PROGRAM
C*****************************************************************************************
              CASE (CloseRequest)
C
C  Request to close window. This message will be reported when the user
C  attempts to close the editor or selects Exit from the File menu.
C  Since we are using a Modeless editor we must close the window ourselves,
C  this would be done automatically for a Modal editor.
C
              CALL WindowCloseChild(IEDITWIN)
              SQ=0
              SN=0
              SST=0
              CALL EXITT(0)

C             THIS RUNS THE EDITOR BASES SCROLLING INPUT/OUTPUT WINDOW
C             THIS IS THE MAIN PROGRAM ENVIRONMENT
C*****************************************************************************************
              CASE (EditorCommand)
C
C  Command entered into editor's command line, i.e. RETURN key pressed.
C  Echo entered command in editor's main window.
C  Display output of command in editor's main window.
C  Clear command line ready for the next command.
C
C  A real program should check for a full editor buffer here. This would be
C  done using WInfoEditor before WEditPutTextPart. If necessary text would
C  be deleted from the start of the buffer by calling WEditPutTextPart with
C  a zero length string.
C
C  Note: other actions could be taken when a command is entered,
C        e.g. displaying a dialog. Likewise the editor's main window can
C        be updated at any time, not just in response to command entry.
C
              CALL WEditGetCommand(COMMAND)
              HT=.FALSE.
              CALL IUpperCase(COMMAND)
C
C       HANDLE CASE OF ECHO ON
C*****************************************************************************************
               IF(ECH.EQ.1) THEN
               IPOS = WInfoEditor(1,2) + 1
        CALL WEditPutTextPart('>'//TRIM(COMMAND)//NEWLIN,IPOS,IPOS)
               END IF
                  SELECT CASE (TRIM(COMMAND))
                      CASE ('CLS')
C       HANDLE CLEAR SCREEN COMMAND TO EMPTY SCREEN BUFFER
C*****************************************************************************************
                      CALL WEditPutText('')
C             Add latest prompt to command line
              CALL SELECTKDP(KKDP)
              CALL WEditPrompt(KKDP)
                   CALL WEditPutCommand('')
                          OUTPUT = ''
                      CASE DEFAULT
              IF (LEN_TRIM(COMMAND) /= 0) THEN
C             Add latest prompt to command line
              CALL SELECTKDP(KKDP)
              CALL WEditPrompt(KKDP)
C             BLANK OUT THE COMMAND LINE
              CALL WEditPutCommand('')
C
C       AUTOMATIC REFRESH OF EDIITING MACRO
C*****************************************************************************************
        EXIS88=.FALSE.
        INQUIRE(FILE='RESOLVE.DAT',EXIST=EXIS88)
        IF(RESOLVEIT.AND.EXIS88) THEN
        EXIS88=.FALSE.
        SAVE_KDP(16)=SAVEINPT(16)
        WC='MREFRESH'
        SQ=0
        STI=0
        SN=0
        SST=0
        CALL MREFRESH
        REST_KDP(16)=RESTINPT(16)
        CALL MY_DELETE_FILE('RESOLVE.DAT')
        RESOLVEIT=.FALSE.
               END IF
C     THIS IS THE MAIN PROGRAM COMMAND TO COMMAND PROCESSOR CALL
C     WHEN INPUT COMES FRON THE COMMAND LINE OF THE SCREEN
C*****************************************************************************************
C       LOCKOUT COMMAND MODE WHILE SPREADSHEET EDITOR IS OPEN
        IF(EDITOROPEN) THEN
        OUTLYNE='WHILE THE SPREADSHEET LENS EDITOR IS OPEN, NO COMMAND'
        CALL SHOWIT(0)
        OUTLYNE='LINE INPUT IS ALLOWED.'
        CALL SHOWIT(0)
        OUTLYNE='TO USE THE COMMAND LINE INPUT MODE, FIRST CLOSE THE'
        CALL SHOWIT(0)
        OUTLYNE='SPREADSHEET LENS EDITOR.'
        CALL SHOWIT(0)
                ELSE
              INPUT=COMMAND(1:132)
              MULTICOM=.TRUE.
              CALL PROCES
              MULTICOM=.FALSE.
                END IF
              CALL SELECTKDP(KKDP)
              CALL WEditPrompt(KKDP)
                        ELSE
C             Add latest prompt to command line
              CALL SELECTKDP(KKDP)
              CALL WEditPrompt(KKDP)
C             BLANK OUT THE COMMAND LINE
              CALL WEditPutCommand('')
        EXIS88=.FALSE.
        INQUIRE(FILE='RESOLVE.DAT',EXIST=EXIS88)
        IF(RESOLVEIT.AND.EXIS88) THEN
        EXIS88=.FALSE.
        SAVE_KDP(16)=SAVEINPT(16)
        WC='MREFRESH'
        SQ=0
        STI=0
        SN=0
        SST=0
        CALL MREFRESH
        REST_KDP(16)=RESTINPT(16)
        CALL MY_DELETE_FILE('RESOLVE.DAT')
        RESOLVEIT=.FALSE.
               END IF
C       THIS IS A BLANK LINE WHICH AUTOMATICALLY CAUSES
C       THE MACRO REFRESH TO BE EXECUTED AFTER A MACRO IS READ IN
C       AFTER FULL SCREEN EDITTING.
               INPUT='                                              '
               CALL PROCES
               CALL SELECTKDP(KKDP)
               CALL WEditPrompt(KKDP)
                        END IF
                  END SELECT
C              END IF
          END SELECT
                                RETURN
 100    FORMAT(A140)
                                END
C SUB MAC_EDITOR.INC
        SUBROUTINE MAC_EDITOR
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ITEM
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATMAC.INC'
C
      RESOLVEIT=.TRUE.
      ITEM=ID_SYSTEM
      IF(ITEM.EQ.3)
     1CALL IOsCommand('START MEDFILE.EXE 1MAC_EDIT.DAT',1)
      IF(ITEM.EQ.4)
     1CALL IOsCommand('CMD.EXE /c MEDFILE.EXE 1MAC_EDIT.DAT',1)
                        RETURN
                        END
