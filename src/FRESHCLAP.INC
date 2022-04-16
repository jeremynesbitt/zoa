        Call WDialogPutInteger(IDF_MAXAPE,INT(ALENS(127,EDIT_SURFACE)))
        Call WDialogPutInteger(IDF_NUMAPE,0)
        Call WDialogPutInteger(IDF_MAXOBS,INT(ALENS(128,EDIT_SURFACE)))
        Call WDialogPutInteger(IDF_NUMOBS,0)
        IF(ALENS(9,EDIT_SURFACE).EQ.0.0D0) THEN
        OLD_APETYPE=0
        Call WDialogPutRadioButton(IDF_APE1)
                END IF
        IF(ALENS(9,EDIT_SURFACE).EQ.1.0D0) THEN
        OLD_APETYPE=1
        Call WDialogPutRadioButton(IDF_APE2)
                END IF
        IF(ALENS(9,EDIT_SURFACE).EQ.2.0D0) THEN
        OLD_APETYPE=2
        Call WDialogPutRadioButton(IDF_APE3)
                END IF
        IF(ALENS(9,EDIT_SURFACE).EQ.3.0D0) THEN
        OLD_APETYPE=3
        Call WDialogPutRadioButton(IDF_APE4)
                END IF
        IF(ALENS(9,EDIT_SURFACE).EQ.4.0D0) THEN
        OLD_APETYPE=4
        Call WDialogPutRadioButton(IDF_APE5)
                END IF
        IF(ALENS(9,EDIT_SURFACE).EQ.5.0D0) THEN
        OLD_APETYPE=5
        Call WDialogPutRadioButton(IDF_APE6)
                END IF
C
        IF(ALENS(16,EDIT_SURFACE).EQ.0.0D0) THEN
        OLD_COBSTYPE=0
        Call WDialogPutRadioButton(IDF_COBS1)
                END IF
        IF(ALENS(16,EDIT_SURFACE).EQ.1.0D0) THEN
        OLD_COBSTYPE=1
        Call WDialogPutRadioButton(IDF_COBS2)
                END IF
        IF(ALENS(16,EDIT_SURFACE).EQ.2.0D0) THEN
        OLD_COBSTYPE=2
        Call WDialogPutRadioButton(IDF_COBS3)
                END IF
        IF(ALENS(16,EDIT_SURFACE).EQ.3.0D0) THEN
        OLD_COBSTYPE=3
        Call WDialogPutRadioButton(IDF_COBS4)
                END IF
        IF(ALENS(16,EDIT_SURFACE).EQ.4.0D0) THEN
        OLD_COBSTYPE=4
        Call WDialogPutRadioButton(IDF_COBS5)
                END IF
        IF(ALENS(16,EDIT_SURFACE).EQ.5.0D0) THEN
        OLD_COBSTYPE=5
        Call WDialogPutRadioButton(IDF_COBS6)
                END IF
C
        IF(ALENS(51,EDIT_SURFACE).EQ.0.0D0) THEN
        OLD_EAPETYPE=0
        Call WDialogPutRadioButton(IDF_EAPE1)
                END IF
        IF(ALENS(51,EDIT_SURFACE).EQ.1.0D0) THEN
        OLD_EAPETYPE=1
        Call WDialogPutRadioButton(IDF_EAPE2)
                END IF
        IF(ALENS(51,EDIT_SURFACE).EQ.2.0D0) THEN
        OLD_EAPETYPE=2
        Call WDialogPutRadioButton(IDF_EAPE3)
                END IF
        IF(ALENS(51,EDIT_SURFACE).EQ.3.0D0) THEN
        OLD_EAPETYPE=3
        Call WDialogPutRadioButton(IDF_EAPE4)
                END IF
        IF(ALENS(51,EDIT_SURFACE).EQ.4.0D0) THEN
        OLD_EAPETYPE=4
        Call WDialogPutRadioButton(IDF_EAPE5)
                END IF
        IF(ALENS(51,EDIT_SURFACE).EQ.5.0D0) THEN
        OLD_EAPETYPE=5
        Call WDialogPutRadioButton(IDF_EAPE6)
                END IF
C
        IF(ALENS(61,EDIT_SURFACE).EQ.0.0D0) THEN
        OLD_ ECOBSTYPE=0
        Call WDialogPutRadioButton(IDF_ECOBS1)
                END IF
        IF(ALENS(61,EDIT_SURFACE).EQ.1.0D0) THEN
        OLD_ ECOBSTYPE=1
        Call WDialogPutRadioButton(IDF_ECOBS2)
                END IF
        IF(ALENS(61,EDIT_SURFACE).EQ.2.0D0) THEN
        OLD_ ECOBSTYPE=2
        Call WDialogPutRadioButton(IDF_ECOBS3)
                END IF
        IF(ALENS(61,EDIT_SURFACE).EQ.3.0D0) THEN
        OLD_ ECOBSTYPE=3
        Call WDialogPutRadioButton(IDF_ECOBS4)
                END IF
        IF(ALENS(61,EDIT_SURFACE).EQ.4.0D0) THEN
        OLD_ ECOBSTYPE=4
        Call WDialogPutRadioButton(IDF_ECOBS5)
                END IF
        IF(ALENS(61,EDIT_SURFACE).EQ.5.0D0) THEN
        OLD_ ECOBSTYPE=5
        Call WDialogPutRadioButton(IDF_ECOBS6)
                END IF
