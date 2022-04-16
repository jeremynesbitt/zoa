        SUBROUTINE GOTFGUI(MESSAGE)
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
C       GOTF
C
        CALL WDIALOGSELECT(IDD_GOTF)
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDF_GOTF)
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
C
        CALL WDIALOGGETDOUBLE(IDF_MAX,DW1)
C       MAKE THE GOTF
        IF(DW1.NE.0.0D0) THEN
        WRITE(INPUT,*)'GOTF',DW1
        CALL PROCES
                        ELSE
        WRITE(INPUT,*)'GOTF'
        CALL PROCES
                        END IF
C
C       DIFLEACA
C
        CALL WDIALOGGETRADIOBUTTON(IDF_A1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 1'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_B1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 2'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_C1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 3'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_D1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 4'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_E1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 5'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_G1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 6'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_H1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 7'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_I1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 8'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_J1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 9'
                CALL PROCES
                END IF
        CALL WDIALOGGETRADIOBUTTON(IDF_K1,ISET)
        IF(ISET.EQ.2) THEN
                WRITE(INPUT,*) 'GEOLEICA NO, 10'
                CALL PROCES
                END IF
C
C       PLOT AND TYPE
        CALL WDIALOGGETCHECKBOX(IDF_NOPLOT,ISET)
                IF(ISET.EQ.0) THEN
C       PLOT
C
C
        CALL WDIALOGGETRADIOBUTTON(IDF_T1,JSET)
        IF(JSET.EQ.1) THEN
C       REGULAR PLOT
        INPUT='PLTGOTF,1'
        CALL PROCES
                CALL GRAPHOUTPUT
                        END IF
        IF(JSET.EQ.2) THEN
C       LEICA PLOT
        INPUT='PLTGOTF LEICA,1'
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
