        SUBROUTINE APODGUI(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        REAL*8 DW1
        INTEGER ISET
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE APOD GUI
C
        CALL WDIALOGSELECT(IDD_APOD)
C
        CALL WDIALOGGETRADIOBUTTON(IDF_GAU1,ISET)
C
        CALL WDIALOGGETDOUBLE(IDF_APODVAL,DW1)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDOK)
        IF(ISET.EQ.1) THEN
        WRITE(INPUT,*) 'APOD GAUSS,',DW1
        CALL PROCES
                ELSE
        WRITE(INPUT,*) 'APOD NONE'
        CALL PROCES
        CALL WDIALOGPUTDOUBLE(IDF_APODVAL,0.0D0)
                END IF
        CALL WDialogHide()
        CALL WDialogUnload()
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
