        SUBROUTINE SPOTGUI(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        REAL*8 DW1,DW2,DW3,DW4,DW5
        INTEGER ISET,JSET,KSET,LSET,MSET,NSET
        LOGICAL FAILACC
        COMMON/ACCFAIL/FAILACC
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE SPOT DIAGRAM MAIN GUI
C
        CALL WDIALOGSELECT(IDD_SPOT)
C
        SELECT CASE(MESSAGE%VALUE1)
C
C       SETTING SPOT TO RING
        CASE(IDF_SP1)
        CALL WDIALOGPUTRADIOBUTTON(IDF_SPO1)
        INPUT='SPOT RING'
        CALL PROCES
C       LEAVE THE RINGS THE DEFAULT
C
C       SETTING SPOT TO RECT
        CASE(IDF_SP2)
        CALL WDIALOGPUTRADIOBUTTON(IDF_SPO2)
        INPUT='SPOT RECT'
        CALL PROCES
        CALL WDIALOGGETINTEGER(IDF_RECT,ISET)
        WRITE(INPUT,*) 'RECT,',ISET
        CALL PROCES
C
C       SETTING SPOT TO RANDOM
        CASE(IDF_SP3)
        CALL WDIALOGPUTRADIOBUTTON(IDF_SPO3)
        INPUT='SPOT RAND'
        CALL PROCES
        CALL WDIALOGGETINTEGER(IDF_RANNUM,ISET)
        WRITE(INPUT,*) 'RANNUM,',ISET
        CALL PROCES
C
C       SETTING RING VALUES
        CASE(IDF_ASSIGN)
        CALL WDIALOGGETRADIOBUTTON(IDF_SPO1,LSET)
        IF(LSET.EQ.1) THEN
C       RINGS
        CALL WDIALOGGETINTEGER(IDF_RINGS,ISET)
        CALL WDIALOGGETINTEGER(IDF_RINGNUM,JSET)
        CALL WDIALOGGETDOUBLE(IDF_RAD,DW1)
        CALL WDIALOGGETINTEGER(IDF_RAYS,KSET)
        CALL WDIALOGGETDOUBLE(IDF_THETA,DW2)
        WRITE(INPUT,*) 'RINGS,',ISET
        CALL PROCES
        WRITE(INPUT,*) 'RING,',JSET,',',DW1,',',KSET,',',DW2
        CALL PROCES
        ISET=INT(RINGTOT)
        CALL WDIALOGGETINTEGER(IDF_RINGS,ISET)
        CALL WDIALOGGETINTEGER(IDF_RINGNUM,JSET)
        CALL WDIALOGPUTDOUBLE(IDF_RAD,RINGRAD(JSET))
        CALL WDIALOGPUTINTEGER(IDF_RAYS,RINGPNT(JSET))
        CALL WDIALOGPUTDOUBLE(IDF_THETA,RINGANG(JSET))


                        END IF
        IF(LSET.EQ.2) THEN
C       RECT
        CALL WDIALOGGETINTEGER(IDF_RECT,ISET)
        WRITE(INPUT,*) 'RECT,',ISET
        CALL PROCES
                        END IF
        IF(LSET.EQ.3) THEN
C       RAND
        CALL WDIALOGGETINTEGER(IDF_RANNUM,ISET)
        WRITE(INPUT,*) 'RANNUM,',ISET
        CALL PROCES
                        END IF
C
C       SHOWING VALUES
        CASE(IDF_SHOW)
        CALL WDIALOGGETRADIOBUTTON(IDF_SPO1,LSET)
        IF(LSET.EQ.1) THEN
C       RINGS
        WRITE(INPUT,*) 'RINGS ?'
        CALL PROCES
        WRITE(INPUT,*) 'RING'
        CALL PROCES
        ISET=INT(RINGTOT)
        CALL WDIALOGGETINTEGER(IDF_RINGS,ISET)
        CALL WDIALOGGETINTEGER(IDF_RINGNUM,ISET)
        CALL WDIALOGPUTDOUBLE(IDF_RAD,RINGRAD(ISET))
        CALL WDIALOGPUTINTEGER(IDF_RAYS,RINGPNT(ISET))
        CALL WDIALOGPUTDOUBLE(IDF_THETA,RINGANG(ISET))

                        END IF
        IF(LSET.EQ.2) THEN
C       RECT
        WRITE(INPUT,*) 'RECT ?'
        CALL PROCES
                        END IF
        IF(LSET.EQ.3) THEN
C       RAND
        WRITE(INPUT,*) 'RANNUM ?'
        CALL PROCES
                        END IF
C
C       SINGLE WAVELENGTH SPOT
        CASE(IDF_SPD1)
        CALL WDIALOGGETINTEGER(IDF_SPDLAM,ISET)
        CALL WDIALOGGETCHECKBOX(IDF_ACC,KSET)
        IF(JSET.EQ.0.AND.KSET.EQ.0) THEN
        CALL WDIALOGGETRADIOBUTTON(IDF_STATS1,MSET)
        IF(MSET.EQ.1) THEN
        INPUT='STATS FULL'
        CALL PROCES
                        ELSE
        INPUT='STATS MIN'
        CALL PROCES
                        END IF
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD,',ISET
        CALL PROCES
                        END IF
        IF(JSET.EQ.0.AND.KSET.EQ.1) THEN
        CALL WDIALOGGETRADIOBUTTON(IDF_STATS1,MSET)
        IF(MSET.EQ.1) THEN
        INPUT='STATS FULL'
        CALL PROCES
                        ELSE
        INPUT='STATS MIN'
        CALL PROCES
                        END IF
        INPUT='OUT NULL'
        CALL PROCES
        CALL CHIEFTRACE
        INPUT='OUT TP'
        CALL PROCES
        WRITE(INPUT,*) 'SPD ACC,',ISET
        CALL PROCES
                        END IF
C
C
C       MULTI WAVELENGTH SPOT
        CASE(IDF_SPD)
        CALL WDIALOGGETCHECKBOX(IDF_ACC,KSET)
        IF(JSET.EQ.0.AND.KSET.EQ.0) THEN
        CALL WDIALOGGETRADIOBUTTON(IDF_STATS1,MSET)
        IF(MSET.EQ.1) THEN
        INPUT='STATS FULL'
        CALL PROCES
                        ELSE
        INPUT='STATS MIN'
        CALL PROCES
                        END IF
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
                        END IF
        IF(JSET.EQ.0.AND.KSET.EQ.1) THEN
        CALL WDIALOGGETRADIOBUTTON(IDF_STATS1,MSET)
        IF(MSET.EQ.1) THEN
        INPUT='STATS FULL'
        CALL PROCES
                        ELSE
        INPUT='STATS MIN'
        CALL PROCES
                        END IF
        INPUT='OUT NULL'
        CALL PROCES
        CALL CHIEFTRACE
        INPUT='OUT TP'
        CALL PROCES
        WRITE(INPUT,*) 'SPD ACC'
        CALL PROCES
                        END IF
C
C       MOVE SPOT
        CASE(IDF_MOVE)
        IF(.NOT.SPDEXT) THEN
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
        END IF
        CALL WDIALOGGETDOUBLE(IDF_DELTA,DW1)
        CALL WDIALOGGETCHECKBOX(IDF_ACC,ISET)
        IF(ISET.EQ.0) THEN
        CALL WDIALOGGETRADIOBUTTON(IDF_STATS1,MSET)
        IF(MSET.EQ.1) THEN
        INPUT='STATS FULL'
        CALL PROCES
                        ELSE
        INPUT='STATS MIN'
        CALL PROCES
                        END IF
        WRITE(INPUT,*) 'SPD MOVE,',DW1
        CALL PROCES
                        END IF
        IF(ISET.EQ.1) THEN
        CALL WDIALOGGETRADIOBUTTON(IDF_STATS1,MSET)
        IF(MSET.EQ.1) THEN
        INPUT='STATS FULL'
        CALL PROCES
                        ELSE
        INPUT='STATS MIN'
        CALL PROCES
                        END IF
        WRITE(INPUT,*) 'SPD MOVEACC,',DW1
        CALL PROCES
                        END IF
C
C       SPDSAVE
        CASE(IDF_SPDSAVE)
        WRITE(INPUT,*) 'SPDSAVE'
        CALL PROCES
C
C       SPDADD
        CASE(IDF_SPDADD)
        WRITE(INPUT,*) 'SPDADD'
        CALL PROCES
C
C       SPDSTATS
        CASE(IDF_SUMSTATS)
        WRITE(INPUT,*) 'SPDSTATS'
        CALL PROCES
C
        CASE(IDF_FAIL)
        IF(.NOT.SPDEXT) THEN
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
        END IF
        CALL WDIALOGGETCHECKBOX(IDF_ACC,ISET)
        IF(ISET.EQ.0) FAILACC=.FALSE.
        IF(ISET.EQ.1) FAILACC=.TRUE.
        CALL WDialogLoad(IDD_FAIL)
        CALL WDIALOGPUTINTEGER(IDF_STA,0)
        CALL WDIALOGPUTINTEGER(IDF_STO,INT(SYSTEM(20)))
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE(IDF_ISTAT)
        IF(.NOT.SPDEXT) THEN
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
        END IF
        CALL WDialogLoad(IDD_ISTAT)
        CALL WDIALOGPUTINTEGER(IDF_SURF,INT(SYSTEM(20)))
        CALL WDIALOGPUTDOUBLE(IDF_ANGLE1,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_ANGLE2,90.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_INCREMENT,10.0D0)
        CALL WDIALOGPUTRADIOBUTTON(IDF_ANG1)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE(IDF_SPOTWRITE)
        IF(.NOT.SPDEXT) THEN
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
        END IF
        CALL WDialogLoad(IDD_SPOTWRITE)
        CALL WDIALOGPUTSTRING(IDF_FILENAME,'SPOTFILE')
        CALL WDIALOGPUTRADIOBUTTON(IDF_WR2)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE(IDF_SPOTPLOT)
        IF(.NOT.SPDEXT) THEN
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
        END IF
        CALL WDialogLoad(IDD_SPOTPLOT)
        CALL WDIALOGPUTSTRING(IDF_FILENAME,'SPOTFILE')
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE(IDF_SPOTENERGY)
        IF(.NOT.SPDEXT) THEN
        CALL CHIEFTRACE
        WRITE(INPUT,*) 'SPD'
        CALL PROCES
        END IF
        CALL WDialogLoad(IDD_SPOTENERGY)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
        SUBROUTINE FAILGUI(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET,JSET
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        LOGICAL FAILACC
        COMMON/ACCFAIL/FAILACC
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE SPOT DIAGRAM RAY FAILURE GUI
C
        CALL WDIALOGSELECT(IDD_FAIL)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDOK)
        CALL WDIALOGGETINTEGER(IDF_STA,ISET)
        CALL WDIALOGGETINTEGER(IDF_STO,JSET)
        IF(.NOT.FAILACC) THEN
        WRITE(INPUT,*) 'FAIL,',ISET,',',JSET
        CALL PROCES
                        ELSE
        WRITE(INPUT,*) 'FAIL ACC,',ISET,',',JSET
        CALL PROCES
                        END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
        SUBROUTINE ISTATGUI(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET
        REAL*8 DW1,DW2,DW3
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE SPOT DIAGRAM ANGLE STAT GUI
C
        CALL WDIALOGSELECT(IDD_ISTAT)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDOK)
        CALL WDIALOGGETINTEGER(IDF_SURF,ISET)
        CALL WDIALOGGETDOUBLE(IDF_ANGLE1,DW1)
        CALL WDIALOGGETDOUBLE(IDF_ANGLE2,DW2)
        CALL WDIALOGGETDOUBLE(IDF_INCREMENT,DW3)
        CALL WDIALOGGETRADIOBUTTON(IDF_ANG1,ISET)
        IF(ISET.EQ.1) THEN
        WRITE(INPUT,*) 'SPD ISTAT,',ISET,',',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
        WRITE(INPUT,*) 'SPD IPSTAT,',ISET,',',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
        SUBROUTINE SPOTWRITEGUI(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET
        CHARACTER*8 NAME8
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE SPOT DIAGRAM ANGLE STAT GUI
C
        CALL WDIALOGSELECT(IDD_SPOTWRITE)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDOK)
        CALL WDIALOGGETSTRING(IDF_FILENAME,NAME8)
        CALL WDIALOGGETRADIOBUTTON(IDF_WR1,ISET)
        IF(ISET.EQ.1) THEN
        WRITE(INPUT,*) 'BWRTSPOT ',TRIM(NAME8)
        CALL PROCES
        WRITE(OUTLYNE,*) 'SPOT DIAGRAM WRITTEN TO :',TRIM(NAME8)//'.SPD'
        CALL SHOWIT(1)
                        END IF
        IF(ISET.EQ.2) THEN
        WRITE(INPUT,*) 'AWRTSPOT ',TRIM(NAME8)
        CALL PROCES
        WRITE(OUTLYNE,*) 'SPOT DIAGRAM WRITTEN TO :',TRIM(NAME8)//'.ASC'
        CALL SHOWIT(1)
                        END IF
        IF(ISET.EQ.3) THEN
        WRITE(INPUT,*) 'BWRTSUM ',TRIM(NAME8)
        CALL PROCES
        WRITE(OUTLYNE,*)
     1  'SUMMED SPOT DIAGRAM WRITTEN TO :',TRIM(NAME8)//'.SPD'
        CALL SHOWIT(1)
                        END IF
        IF(ISET.EQ.4) THEN
        WRITE(INPUT,*) 'AWRTSUM ',TRIM(NAME8)
        CALL PROCES
        WRITE(OUTLYNE,*)
     1  'SUMMED SPOT DIAGRAM WRITTEN TO :',TRIM(NAME8)//'.ASC'
        CALL SHOWIT(1)
                        END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
        SUBROUTINE SPOTPLOT(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET,JSET,KSET,LSET
        REAL*8 DW1,DW2,DW3,DW4,DW5
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE SPOT PLOTTING GUI
C
        CALL WDIALOGSELECT(IDD_SPOTPLOT)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDOK)
C
C       SCALE FACTOR
        CALL WDIALOGGETRADIOBUTTON(IDF_SC1,ISET)
        IF(ISET.EQ.2) THEN
        CALL WDIALOGGETDOUBLE(IDF_SSI,DW1)
        WRITE(INPUT,*) 'SPDSSI,',DW1
        CALL PROCES
                        END IF
C
C       DETECTOR ON PLOT
        JSET=0
        CALL WDIALOGGETCHECKBOX(IDF_DET,ISET)
        CALL WDIALOGGETRADIOBUTTON(IDF_SH1,JSET)
        IF(ISET.EQ.1.AND.JSET.EQ.1) THEN
C       CIRCULAR
        CALL WDIALOGGETDOUBLE(IDF_RAD,DW1)
        CALL WDIALOGGETDOUBLE(IDF_CDX,DW2)
        CALL WDIALOGGETDOUBLE(IDF_CDY,DW3)
        INPUT='DET ON'
        CALL PROCES
        WRITE(INPUT,*)'DET CIRC,',2.0D0*DW1,',',DW2,',',DW3
        CALL PROCES
                                END IF
        IF(ISET.EQ.1.AND.JSET.EQ.2) THEN
C       RECTANGULAR
        CALL WDIALOGGETDOUBLE(IDF_RX,DW1)
        CALL WDIALOGGETDOUBLE(IDF_RY,DW2)
        CALL WDIALOGGETDOUBLE(IDF_RDX,DW3)
        CALL WDIALOGGETDOUBLE(IDF_RDY,DW4)
        CALL WDIALOGGETDOUBLE(IDF_RROT,DW5)
        INPUT='DET ON'
        CALL PROCES
        WRITE(INPUT,*)'DET RECT,',DW1,',',DW2,',',DW3,',',DW4,',',DW5
        CALL PROCES
                                END IF
        IF(ISET.EQ.0.OR.JSET.EQ.0) THEN
        INPUT='DET ON'
        CALL PROCES
                                END IF
C
        CALL WDIALOGGETRADIOBUTTON(IDF_CENT1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='PLTSPD CHIEF,1'
        CALL PROCES
                        END IF
        IF(ISET.EQ.2) THEN
        INPUT='PLTSPD CENT,1'
        CALL PROCES
                        END IF
        IF(ISET.EQ.3) THEN
        INPUT='PLTSPD SUM,1'
        CALL PROCES
                        END IF
        IF(ISET.EQ.4) THEN
        INPUT='PLTSPD ZERO,1'
        CALL PROCES
                        END IF
C
        CALL GRAPHOUTPUT
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
C
C       THIS CODE TAKES CARE OF SPOT ENERGY DISTRIBUTION GUI
C
        SUBROUTINE SPOTENERGY(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET,JSET,KSET,LSET,MSET
        REAL*8 DW1,DW2,DW3
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE SPOT DIAGRAM ENERGY DISTRIBUTIONS
C
        CALL WDIALOGSELECT(IDD_SPOTENERGY)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDF_SPOTRED)
C       TYPE OF SPOT TO USE
        CALL WDIALOGGETRADIOBUTTON(IDF_SH1,ISET)
C       DELTAE OR E
        CALL WDIALOGGETRADIOBUTTON(IDF_TYPE1,JSET)
C       CHIEF OR CENTROID
        CALL WDIALOGGETRADIOBUTTON(IDF_CEN1,MSET)
C       SILENT
        CALL WDIALOGGETCHECKBOX(IDF_ACC,KSET)
C       PLOT
        CALL WDIALOGGETCHECKBOX(IDF_PLOT,LSET)
        CALL WDIALOGGETDOUBLE(IDF_XOFF,DW2)
        CALL WDIALOGGETDOUBLE(IDF_YOFF,DW3)
C
        IF(MSET.EQ.1) THEN
C       CENTER ON CHIEF RAY
C       SINGLE VALUE STUFF
        IF(JSET.EQ.2) THEN
C       SINGLE VALUE ONLY
        CALL WDIALOGGETDOUBLE(IDF_E,DW1)
        IF(ISET.EQ.1) THEN
C       CURRENT SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'RED ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'RED ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
        INPUT='WRITE X'
        CALL PROCES
                        END IF
                        ELSE
C       SUMMED SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'SRED ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'SRED ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
        INPUT='WRITE X'
        CALL PROCES
                        END IF
                        END IF
                        ELSE
C       MULTIPLE TABULAR LISTINGS AND MAYBE PLOTS
        CALL WDIALOGGETDOUBLE(IDF_DELTAE,DW1)
        IF(ISET.EQ.1) THEN
C       CURRENT SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'RED N,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'RED,',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
                        ELSE
C       SUMMED SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'SRED N,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'SRED,',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
                        END IF
        IF(LSET.EQ.1) THEN
        INPUT='PLTRED,1'
        CALL PROCES
        CALL GRAPHOUTPUT
                        END IF
                        END IF
                        ELSE
C       CENTER ON CENTROID
C       SINGLE VALUE STUFF
        IF(JSET.EQ.2) THEN
C       SINGLE VALUE ONLY
        CALL WDIALOGGETDOUBLE(IDF_E,DW1)
        IF(ISET.EQ.1) THEN
C       CURRENT SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'RED CACC,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'RED CACC,',DW1,',',DW2,',',DW3
        CALL PROCES
        INPUT='WRITE X'
        CALL PROCES
                        END IF
                        ELSE
C       SUMMED SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'SRED CACC,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'SRED CACC,',DW1,',',DW2,',',DW3
        CALL PROCES
        INPUT='WRITE X'
        CALL PROCES
                        END IF
                        END IF
                        ELSE
C       MULTIPLE TABULAR LISTINGS AND MAYBE PLOTS
        CALL WDIALOGGETDOUBLE(IDF_DELTAE,DW1)
        IF(ISET.EQ.1) THEN
C       CURRENT SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'RED NCENT,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'RED CENT,',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
                        ELSE
C       SUMMED SPOT
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'SRED NCENT,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'SRED CENT,',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
                        END IF
        IF(LSET.EQ.1) THEN
        INPUT='PLTRED,1'
        CALL PROCES
        CALL GRAPHOUTPUT
                        END IF
                        END IF
                        END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
