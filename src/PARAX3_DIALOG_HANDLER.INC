C       FIRST ORDER OPERATING CONDITIONS DIALOG HANDLER
C
        IF(MESSAGE%WIN.EQ.IDD_PARAX3) THEN
        CALL WDIALOGSELECT(IDD_PARAX3)
           SELECT CASE(MESSAGE%VALUE1)
            CASE (IDOK)
        CALL WDIALOGGETRADIOBUTTON(IDF_OCD1,ISET)
        IF(ISET.EQ.1) THEN
C       YZ-PLANE DATA
        INPUT='OCDY'
                ELSE
C       XZ-PLANE DATA
        INPUT='OCDX'
                END IF
        CALL PROCES
        WRITE(OUTLYNE,*) '   '
        CALL SHOWIT(0)
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                                END IF
