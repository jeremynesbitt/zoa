C       DOES THE FIRST,THIRD,FIFTH,SEVENTH MONOCHROMATIC ABERRATION DIALOG
C
        IF(MESSAGE%WIN.EQ.IDD_PARAX1) THEN
        CALL WDialogSelect(IDD_PARAX1)
C
           SELECT CASE(MESSAGE%VALUE1)
            CASE (IDOK)
        CALL WDIALOGGETRADIOBUTTON(IDF_PLANE1,ISET)
        CALL WDIALOGGETRADIOBUTTON(IDF_PAR1,JSET)
        IF(ISET.EQ.1) THEN
C       YZ-PLANE DATA
        SELECT CASE (JSET)
        CASE (1)
        INPUT='PXTY ALL'
        CASE (2)
        INPUT='PITY ALL'
        CASE (3)
        INPUT='PRTY ALL'
        CASE (4)
        INPUT='MAB3 ALL'
        CASE (5)
        INPUT='MAB5 ALL'
        CASE (6)
        INPUT='MABX5 ALL'
        CASE (7)
        INPUT='SA357 ALL'
        CASE (8)
        INPUT='MABP3 ALL'
        END SELECT
                        ELSE
C       ISET=2, XZ-PLANE DATA
        SELECT CASE (JSET)
        CASE (1)
        INPUT='PXTX ALL'
        CASE (2)
        INPUT='PITX ALL'
        CASE (3)
        INPUT='PRTX ALL'
        CASE (4)
        INPUT='XMAB3 ALL'
        CASE (5)
        INPUT='XMAB5 ALL'
        CASE (6)
        INPUT='XMABX5 ALL'
        CASE (7)
        INPUT='XSA357 ALL'
        CASE (8)
        INPUT='XMABP3 ALL'
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
