C       THIS CODE TAKES CARE OF THE OPERATION OF ZEROING
C       LENS SURFACE DATA
C
        IF(MESSAGE%WIN.EQ.IDD_ZERO) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_ZERO)
        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDOK)
C
C       MAKE LENS CHANGES HERE
C
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ZERO'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C

        CASE (IDCANCEL)
        CALL WDIALOGSELECT(IDD_ZERO)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
                        END IF
