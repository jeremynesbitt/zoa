C       ZERO
C
        CALL WDialogSelect(IDD_ZERO)
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        Call WDialogPutInteger(IDF_SURFNUM,J)
        CALL WDialogShow(-1,-1,0,Modeless)
