        SUBROUTINE PSFGUI(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        REAL*8 DW1,DW2,DW3,DW4,DW5
        INTEGER ISET,JSET,KSET,LSET,MSET,NSET
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATSPD.INC'
        INCLUDE 'DATSP1.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       PSF
C
        CALL WDIALOGSELECT(IDD_PSF)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDF_PSF)
        CALL CHIEFTRACE
        CALL WDIALOGGETRADIOBUTTON(IDF_PSF1,ISET)
        IF(ISET.EQ.1) THEN
C       DETERMINE GRI
        CALL WDIALOGGETINTEGER(IDF_NRD,JSET)
        CALL WDIALOGGETINTEGER(IDF_TGR,KSET)
        CALL WDIALOGGETINTEGER(IDF_PGR,LSET)
        IF(LSET.GE.KSET) LSET=KSET-1
        INPUT='OUT NULL'
        CALL PROCES
        WRITE(INPUT,*) 'NRD,',JSET
        CALL PROCES
        WRITE(INPUT,*) 'TGR,',KSET
        CALL PROCES
        WRITE(INPUT,*) 'PGR,',LSET
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.2) THEN
C       DETERMINE NRD
        CALL WDIALOGGETINTEGER(IDF_TGR,KSET)
        CALL WDIALOGGETDOUBLE(IDF_GRI,DW1)
        CALL WDIALOGGETINTEGER(IDF_PGR,LSET)
        IF(LSET.GE.KSET) LSET=KSET-1
C       INPUT='OUT NULL'
C       CALL PROCES
        WRITE(INPUT,*) 'TGR,',KSET
        CALL PROCES
        WRITE(INPUT,*) 'GRI,',DW1
        CALL PROCES
        WRITE(INPUT,*) 'PGR,',LSET
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.3) THEN
C       DETERMINE TGR
        CALL WDIALOGGETINTEGER(IDF_NRD,JSET)
        CALL WDIALOGGETDOUBLE(IDF_GRI,DW1)
C       INPUT='OUT NULL'
C       CALL PROCES
        WRITE(INPUT,*) 'NRD,',JSET
        CALL PROCES
        WRITE(INPUT,*) 'GRI,',DW1
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
C
C       WRITE PSF
        CALL WDIALOGGETCHECKBOX(IDF_WRITE,ISET)
        IF(ISET.EQ.0) THEN
        INPUT='PSFWRITE NO'
        CALL PROCES
                END IF
        IF(ISET.EQ.1) THEN
        INPUT='PSFWRITE YES'
        CALL PROCES
                END IF
C
C       LINEAR OR LOG PLOT
        CALL WDIALOGGETRADIOBUTTON(IDF_PL1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='PSFLIN'
        CALL PROCES
                END IF
        IF(ISET.EQ.2) THEN
        CALL WDIALOGGETINTEGER(IDF_DECADES,JSET)
        WRITE(INPUT,*) 'PSFLOG,',JSET
        CALL PROCES
                END IF
C
C       PLOT PSF ?
        CALL WDIALOGGETCHECKBOX(IDF_PSFPLOT,ISET)
        IF(ISET.EQ.0) THEN
        INPUT='PSFPLOT NO'
        CALL PROCES
                END IF
        IF(ISET.EQ.1) THEN
        INPUT='PSFPLOT YES'
        CALL PROCES
                END IF
C
C       ROTATE PLOT
        CALL WDIALOGGETCHECKBOX(IDF_ROT,ISET)
        IF(ISET.EQ.0) THEN
        INPUT='PSFROT NO'
        CALL PROCES
                END IF
        IF(ISET.EQ.1) THEN
        INPUT='PSFROT YES'
        CALL PROCES
                END IF
C
C       TYPE OF PSF COMMAND
C
        CALL WDIALOGGETRADIOBUTTON(IDF_P1,ISET)
        IF(ISET.EQ.1) INPUT='PSF,1'
        IF(ISET.EQ.2) INPUT='PSF PERFECT,1'
        IF(ISET.EQ.3) INPUT='PSF PERFNOOB,1'
        CALL PROCES
C
C       WRITE CAPFN
        CALL WDIALOGGETCHECKBOX(IDF_CAPWRITE,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='CAPFNOUT'
        CALL PROCES
C
C       GRAPHICS AND RELOAD SETTINGS
        CALL WDIALOGGETCHECKBOX(IDF_PSFPLOT,ISET)
        IF(ISET.EQ.1) CALL GRAPHOUTPUT
        CALL WDIALOGPUTDOUBLE(IDF_GRI,GRI)
        CALL WDIALOGPUTINTEGER(IDF_TGR,TGR)
        CALL WDIALOGPUTINTEGER(IDF_NRD,NRD)
        CALL WDIALOGPUTINTEGER(IDF_PGR,PGR)

                        END IF
C
C       PSF STREAKING
        CASE(IDF_PSFSTREAK)
        CALL CHIEFTRACE
        CALL WDIALOGGETRADIOBUTTON(IDF_PSF1,ISET)
        IF(ISET.EQ.1) THEN
C       DETERMINE GRI
        CALL WDIALOGGETINTEGER(IDF_NRD,JSET)
        CALL WDIALOGGETINTEGER(IDF_TGR,KSET)
        CALL WDIALOGGETINTEGER(IDF_PGR,LSET)
        IF(LSET.GE.KSET) LSET=KSET-1
        INPUT='OUT NULL'
        CALL PROCES
        WRITE(INPUT,*) 'NRD,',JSET
        CALL PROCES
        WRITE(INPUT,*) 'TGR,',KSET
        CALL PROCES
        WRITE(INPUT,*) 'PGR,',LSET
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.2) THEN
C       DETERMINE NRD
        CALL WDIALOGGETINTEGER(IDF_TGR,KSET)
        CALL WDIALOGGETDOUBLE(IDF_GRI,DW1)
        CALL WDIALOGGETINTEGER(IDF_PGR,LSET)
        IF(LSET.GE.KSET) LSET=KSET-1
C       INPUT='OUT NULL'
C       CALL PROCES
        WRITE(INPUT,*) 'TGR,',KSET
        CALL PROCES
        WRITE(INPUT,*) 'GRI,',DW1
        CALL PROCES
        WRITE(INPUT,*) 'PGR,',LSET
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.3) THEN
C       DETERMINE TGR
        CALL WDIALOGGETINTEGER(IDF_NRD,JSET)
        CALL WDIALOGGETDOUBLE(IDF_GRI,DW1)
C       INPUT='OUT NULL'
C       CALL PROCES
        WRITE(INPUT,*) 'NRD,',JSET
        CALL PROCES
        WRITE(INPUT,*) 'GRI,',DW1
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
C
C       WRITE PSF
        CALL WDIALOGGETCHECKBOX(IDF_WRITE,ISET)
        IF(ISET.EQ.0) THEN
        INPUT='PSFWRITE NO'
        CALL PROCES
                END IF
        IF(ISET.EQ.1) THEN
        INPUT='PSFWRITE YES'
        CALL PROCES
                END IF
C
C       LINEAR OR LOG PLOT
        CALL WDIALOGGETRADIOBUTTON(IDF_PL1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='PSFLIN'
        CALL PROCES
                END IF
        IF(ISET.EQ.2) THEN
        CALL WDIALOGGETINTEGER(IDF_DECADES,JSET)
        WRITE(INPUT,*) 'PSFLOG,',JSET
        CALL PROCES
                END IF
C
C       PLOT PSF ?
        CALL WDIALOGGETCHECKBOX(IDF_PSFPLOT,ISET)
        INPUT='PSFPLOT NO'
        CALL PROCES
C
C       TYPE OF PSF COMMAND
C
        CALL WDIALOGGETRADIOBUTTON(IDF_P1,ISET)
        IF(ISET.EQ.1) INPUT='PSF,1'
        IF(ISET.EQ.2) INPUT='PSF PERFECT,1'
        IF(ISET.EQ.3) INPUT='PSF PERFNOOB,1'
        CALL PROCES
C
C       WRITE CAPFN
        CALL WDIALOGGETCHECKBOX(IDF_CAPWRITE,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='CAPFNOUT'
        CALL PROCES
C
                        END IF
        CALL WDIALOGLOAD(IDD_PSFSTREAK)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C       PSF STREHL
        CASE(IDF_PSFSTREHL)
        CALL WDIALOGLOAD(IDD_PSFSTREHL)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C       PSF ENERGY DISTRIBUTIONS
        CASE(IDF_PSFENERGY)
        CALL CHIEFTRACE
        CALL WDIALOGGETRADIOBUTTON(IDF_PSF1,ISET)
        IF(ISET.EQ.1) THEN
C       DETERMINE GRI
        CALL WDIALOGGETINTEGER(IDF_NRD,JSET)
        CALL WDIALOGGETINTEGER(IDF_TGR,KSET)
        CALL WDIALOGGETINTEGER(IDF_PGR,LSET)
        IF(LSET.GE.KSET) LSET=KSET-1
        INPUT='OUT NULL'
        CALL PROCES
        WRITE(INPUT,*) 'NRD,',JSET
        CALL PROCES
        WRITE(INPUT,*) 'TGR,',KSET
        CALL PROCES
        WRITE(INPUT,*) 'PGR,',LSET
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.2) THEN
C       DETERMINE NRD
        CALL WDIALOGGETINTEGER(IDF_TGR,KSET)
        CALL WDIALOGGETDOUBLE(IDF_GRI,DW1)
        CALL WDIALOGGETINTEGER(IDF_PGR,LSET)
        IF(LSET.GE.KSET) LSET=KSET-1
C       INPUT='OUT NULL'
C       CALL PROCES
        WRITE(INPUT,*) 'TGR,',KSET
        CALL PROCES
        WRITE(INPUT,*) 'GRI,',DW1
        CALL PROCES
        WRITE(INPUT,*) 'PGR,',LSET
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.3) THEN
C       DETERMINE TGR
        CALL WDIALOGGETINTEGER(IDF_NRD,JSET)
        CALL WDIALOGGETDOUBLE(IDF_GRI,DW1)
C       INPUT='OUT NULL'
C       CALL PROCES
        WRITE(INPUT,*) 'NRD,',JSET
        CALL PROCES
        WRITE(INPUT,*) 'GRI,',DW1
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
C
C       WRITE PSF
        CALL WDIALOGGETCHECKBOX(IDF_WRITE,ISET)
        IF(ISET.EQ.0) THEN
        INPUT='PSFWRITE NO'
        CALL PROCES
                END IF
        IF(ISET.EQ.1) THEN
        INPUT='PSFWRITE YES'
        CALL PROCES
                END IF
C
C       LINEAR OR LOG PLOT
        CALL WDIALOGGETRADIOBUTTON(IDF_PL1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='PSFLIN'
        CALL PROCES
                END IF
        IF(ISET.EQ.2) THEN
        CALL WDIALOGGETINTEGER(IDF_DECADES,JSET)
        WRITE(INPUT,*) 'PSFLOG,',JSET
        CALL PROCES
                END IF
C
C       PLOT PSF ?
        CALL WDIALOGGETCHECKBOX(IDF_PSFPLOT,ISET)
        INPUT='PSFPLOT NO'
        CALL PROCES
C
C       TYPE OF PSF COMMAND
C
        CALL WDIALOGGETRADIOBUTTON(IDF_P1,ISET)
        IF(ISET.EQ.1) INPUT='PSF,1'
        IF(ISET.EQ.2) INPUT='PSF PERFECT,1'
        IF(ISET.EQ.3) INPUT='PSF PERFNOOB,1'
        CALL PROCES
C
C       WRITE CAPFN
        CALL WDIALOGGETCHECKBOX(IDF_CAPWRITE,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='CAPFNOUT'
        CALL PROCES
C
                        END IF
        CALL WDIALOGLOAD(IDD_PSFENERGY)
        CALL WDialogShow(-1,-1,0,Modeless)
C
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
C       THIS CODE TAKES CARE OF PSF STREAKING GUI
C
        SUBROUTINE PSFSTREAK(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET,JSET,KSET
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
C       THIS CODE TAKES CARE OF THE PDFSTREAKING
C
        CALL WDIALOGSELECT(IDD_PSFSTREAK)
C
        SELECT CASE(MESSAGE%VALUE1)
C
C       X - STREAK
        CASE(IDF_STREAKX)
C
        CALL WDIALOGGETCHECKBOX(IDF_STREAKPLOT,ISET)
        CALL WDIALOGGETCHECKBOX(IDF_STREAKWRITE,JSET)
        IF(ISET.EQ.1) THEN
        INPUT='STREAK PLOT YES'
        CALL PROCES
                      ELSE
        INPUT='STREAK PLOT NO'
        CALL PROCES
                      END IF
        IF(JSET.EQ.1) THEN
        INPUT='STREAK WRITE YES'
        CALL PROCES
                      ELSE
        INPUT='STREAK WRITE NO'
        CALL PROCES
                      END IF
        CALL WDIALOGGETINTEGER(IDF_STREAKM,KSET)
        WRITE(INPUT,*) 'STREAK X,',KSET
        CALL PROCES
C
C       Y - STREAK
        CASE(IDF_STREAKY)
C
        CALL WDIALOGGETCHECKBOX(IDF_STREAKPLOT,ISET)
        CALL WDIALOGGETCHECKBOX(IDF_STREAKWRITE,JSET)
        IF(ISET.EQ.1) THEN
        INPUT='STREAK PLOT YES'
        CALL PROCES
                      ELSE
        INPUT='STREAK PLOT NO'
        CALL PROCES
                      END IF
        IF(JSET.EQ.1) THEN
        INPUT='STREAK WRITE YES'
        CALL PROCES
                      ELSE
        INPUT='STREAK WRITE NO'
        CALL PROCES
                      END IF
        CALL WDIALOGGETINTEGER(IDF_STREAKM,KSET)
        WRITE(INPUT,*) 'STREAK Y,',KSET
        CALL PROCES
C
C       XY - STREAK
        CASE(IDF_STREAKXY)
C
        CALL WDIALOGGETCHECKBOX(IDF_STREAKPLOT,ISET)
        CALL WDIALOGGETCHECKBOX(IDF_STREAKWRITE,JSET)
        IF(ISET.EQ.1) THEN
        INPUT='STREAK PLOT YES'
        CALL PROCES
                      ELSE
        INPUT='STREAK PLOT NO'
        CALL PROCES
                      END IF
        IF(JSET.EQ.1) THEN
        INPUT='STREAK WRITE YES'
        CALL PROCES
                      ELSE
        INPUT='STREAK WRITE NO'
        CALL PROCES
                      END IF
        CALL WDIALOGGETINTEGER(IDF_STREAKM,KSET)
        WRITE(INPUT,*) 'STREAK XY,',KSET
        CALL PROCES
C
        IF(ISET.EQ.1) CALL GRAPHOUTPUT
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
C
        SUBROUTINE PSFSTREHL(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET,JSET
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
C       THIS CODE TAKES CARE OF THE PDFSTREAKING
C
        CALL WDIALOGSELECT(IDD_PSFSTREHL)
C
        SELECT CASE(MESSAGE%VALUE1)
C
C
        CASE(IDF_STREHL)
        CALL WDIALOGGETCHECKBOX(IDF_ACC,ISET)
        CALL WDIALOGGETCHECKBOX(IDF_NOOB,JSET)
        CALL CHIEFTRACE
        IF(ISET.EQ.0.AND.JSET.EQ.0) THEN
        INPUT='STREHL'
        CALL PROCES
                        END IF
        IF(ISET.EQ.1.AND.JSET.EQ.0) THEN
        INPUT='STREHL ACC'
        CALL PROCES
                        END IF
        IF(ISET.EQ.0.AND.JSET.EQ.1) THEN
        INPUT='STREHL NOOB'
        CALL PROCES
                        END IF
        IF(ISET.EQ.1.AND.JSET.EQ.1) THEN
        INPUT='STREHL ACCNOOB'
        CALL PROCES
                        END IF
C
        CASE(IDF_APSTREHL)
        CALL WDIALOGGETCHECKBOX(IDF_ACC,ISET)
        CALL CHIEFTRACE
        IF(ISET.EQ.1) THEN
        INPUT='OUT NULL'
        CALL PROCES
        INPUT='CAPFN'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                        ELSE
        INPUT='CAPFN'
        CALL PROCES
                        END IF
        IF(ISET.EQ.1.) THEN
        INPUT='APSTREHL ACC'
        CALL PROCES
                        ELSE
        INPUT='APSTREHL'
        CALL PROCES
                        END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
C
C       THIS CODE TAKES CARE OF PSF ENERGY DISTRIBUTION GUI
C
        SUBROUTINE PSFENERGY(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        INTEGER ISET,JSET,KSET,LSET
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
C       THIS CODE TAKES CARE OF THE PSF ENERGY DISTRIBUTIONS
C
        CALL WDIALOGSELECT(IDD_PSFENERGY)
C
        SELECT CASE(MESSAGE%VALUE1)
C
C
        CASE(IDF_DRED)
        CALL WDIALOGGETRADIOBUTTON(IDF_SH1,ISET)
        CALL WDIALOGGETRADIOBUTTON(IDF_TYPE1,JSET)
        CALL WDIALOGGETCHECKBOX(IDF_ACC,KSET)
        CALL WDIALOGGETCHECKBOX(IDF_PLOT,LSET)
        CALL WDIALOGGETDOUBLE(IDF_XOFF,DW2)
        CALL WDIALOGGETDOUBLE(IDF_YOFF,DW3)
C
C       SINGLE VALUE STUFF
        IF(JSET.EQ.2) THEN
C       SINGLE VALUE ONLY
        CALL WDIALOGGETDOUBLE(IDF_E,DW1)
        IF(ISET.EQ.1) THEN
C       CIRCULAR
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'DRED ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'DRED ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
        INPUT='WRITE X'
        CALL PROCES
                        END IF
                        ELSE
C       SQUARE
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'DREDSQ ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'DREDSQ ACC,',DW1,',',DW2,',',DW3
        CALL PROCES
        INPUT='WRITE X'
        CALL PROCES
                        END IF
                        END IF
                        ELSE
C       MULTIPLE TABULAR LISTINGS AND MAYBE PLOTS
        CALL WDIALOGGETDOUBLE(IDF_DELTAE,DW1)
        IF(ISET.EQ.1) THEN
C       CIRCULAR
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'DRED N,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'DRED,',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
                        ELSE
C       SQUARE
        IF(KSET.EQ.1) THEN
C       SILENT
        WRITE(INPUT,*)'DREDSQ N,',DW1,',',DW2,',',DW3
        CALL PROCES
                        ELSE
C       NOT SILENT
        WRITE(INPUT,*)'DREDSQ,',DW1,',',DW2,',',DW3
        CALL PROCES
                        END IF
                        END IF
        IF(LSET.EQ.1) THEN
        INPUT='PLTRED,1'
        CALL PROCES
        CALL GRAPHOUTPUT
                        END IF
                        END IF
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
