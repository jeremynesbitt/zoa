        SUBROUTINE DOTFGUI(MESSAGE)
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
C       DOTF
C
        CALL WDIALOGSELECT(IDD_DOTF)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDF_DOTF)
C
C       SPACE O OR I
        CALL WDIALOGGETRADIOBUTTON(IDF_SP1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='SPACE I'
        CALL PROCES
                        END IF
        IF(ISET.EQ.2) THEN
        INPUT='SPACE O'
        CALL PROCES
                        END IF
C
C       O NEAR OR FAR
        CALL WDIALOGGETRADIOBUTTON(IDF_O1,ISET)
        IF(ISET.EQ.1) THEN
        INPUT='FAR'
        CALL PROCES
                        END IF
        IF(ISET.EQ.2) THEN
        INPUT='NEAR'
        CALL PROCES
                        END IF
C
        CALL WDIALOGGETRADIOBUTTON(IDF_F1,ISET)
        IF(ISET.EQ.1) THEN
C       USE CURRENT MULTI-FOV IN LDM
                        ELSE
        CALL CHIEFTRACE
                        END IF
C       MAKE THE DOTF
        INPUT='DOTF'
        CALL PROCES
C
C       DIFLEACA
C
        CALL WDIALOGGETRADIOBUTTON(IDF_A1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 1'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_B1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 2'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_C1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 3'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_D1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 4'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_E1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 5'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_G1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 6'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_H1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 7'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_I1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 8'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_J1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 9'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_K1,ISET)
        IF(ISET.EQ.2) THEN
                INPUT='DIFLEICA NO, 10'
                CALL PROCES
                END IF
C
C       PLOT AND TYPE
        CALL WDIALOGGETCHECKBOX(IDF_NOPLOT,ISET)
                IF(ISET.EQ.0) THEN
C       PLOT
C
        CALL WDIALOGGETDOUBLE(IDF_MAX,DW1)
C
        CALL WDIALOGGETRADIOBUTTON(IDF_T1,JSET)
        IF(JSET.EQ.1) THEN
C       REGULAR PLOT
        IF(DW1.LE.0.0D0) THEN
        INPUT='PLTDOTF,,1'
        CALL PROCES
                CALL GRAPHOUTPUT
                ELSE
        WRITE(INPUT,*) 'PLTDOTF,',DW1,',1'
        CALL PROCES
                CALL GRAPHOUTPUT
                END IF
                        END IF
        IF(JSET.EQ.2) THEN
C       LEICA PLOT
        INPUT='PLTDOTF LEICA,,1'
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
