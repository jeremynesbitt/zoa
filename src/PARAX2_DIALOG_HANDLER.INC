C       HANDLES DIALOG FOR 1,3,5,7 ORDER CHROMATIC DIFFERENCES
C
        IF(MESSAGE%WIN.EQ.IDD_PARAX2) THEN
        CALL WDialogSelect(IDD_PARAX2)
           SELECT CASE(MESSAGE%VALUE1)
            CASE (IDOK)
        CALL WDIALOGGETRADIOBUTTON(IDF_PLANE3,ISET)
        CALL WDIALOGGETRADIOBUTTON(IDF_PRI,JSET)
        CALL WDIALOGGETRADIOBUTTON(IDF_CHRO1,KSET)
        IF(ISET.EQ.1) THEN
C       YZ-PLANE DATA
        SELECT CASE (KSET)
        CASE (1)
        INPUT='FCHY ALL'
        CASE (2)
        IF(JSET.EQ.1) INPUT='PCD3 ALL'
        IF(JSET.EQ.2) INPUT='SCD3 ALL'
        CASE (3)
        IF(JSET.EQ.1) INPUT='PCD5 ALL'
        IF(JSET.EQ.2) INPUT='SCD5 ALL'
        CASE (4)
        IF(JSET.EQ.1) INPUT='PCDX5 ALL'
        IF(JSET.EQ.2) INPUT='SCDX5 ALL'
        CASE (5)
        IF(JSET.EQ.1) INPUT='PCDP3 ALL'
        IF(JSET.EQ.2) INPUT='SCDP3 ALL'
        CASE (6)
        IF(JSET.EQ.1) INPUT='PCDSA ALL'
        IF(JSET.EQ.2) INPUT='SCDSA ALL'
        END SELECT
                        ELSE
C       XZ-PLANE DATA
        SELECT CASE (KSET)
        CASE (1)
        INPUT='FCHX ALL'
        CASE (2)
        IF(JSET.EQ.1) INPUT='XPCD3 ALL'
        IF(JSET.EQ.2) INPUT='XSCD3 ALL'
        CASE (3)
        IF(JSET.EQ.1) INPUT='XPCD5 ALL'
        IF(JSET.EQ.2) INPUT='XSCD5 ALL'
        CASE (4)
        IF(JSET.EQ.1) INPUT='XPCDX5 ALL'
        IF(JSET.EQ.2) INPUT='XSCDX5 ALL'
        CASE (5)
        IF(JSET.EQ.1) INPUT='XPCDP3 ALL'
        IF(JSET.EQ.2) INPUT='XSCDP3 ALL'
        CASE (6)
        IF(JSET.EQ.1) INPUT='XPCDSA ALL'
        IF(JSET.EQ.2) INPUT='XSCDSA ALL'
        END SELECT
                        END IF
        CALL PROCES
        WRITE(OUTLYNE,*) '   '
        CALL SHOWIT(0)
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                END IF
