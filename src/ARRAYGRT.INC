C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       ARRAY LENS AND DIFFRACTION GRATING PARAMETERS
C
        IF(MESSAGE%WIN.EQ.IDD_GRTARRAY) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_GRTARRAY)
        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDOK)
C
C       MAKE LENS CHANGES HERE
C
        Call WDialogGetRadioButton(IDF_GRT1,ISET)
        Call WDialogGetRadioButton(IDF_ARRAY1,JSET)
        Call WDialogGetRadioButton(IDF_EVENODD1,KSET)
C
        IF(ISET.EQ.1) THEN
C       NO GRATING
        IF(ALENS(96,J).NE.0.0D0) THEN
C       DELETE THE GRATING DEFINITION
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='GRTD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
                END IF
        IF(ISET.EQ.2) THEN
C       REQUEST GRATING ASSIGNMENT
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        Call WDialogGetDouble(IDF_GRO,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_GRS,DW2)
        CALL DTOA23(DW2,AW2)
        Call WDialogGetDouble(IDF_GRX,DW3)
        CALL DTOA23(DW3,AW3)
        Call WDialogGetDouble(IDF_GRY,DW4)
        CALL DTOA23(DW4,AW4)
        Call WDialogGetDouble(IDF_GRZ,DW5)
        CALL DTOA23(DW5,AW5)
        INPUT='GRT'
        CALL PROCES
        INPUT='GRO,'//AW1
        CALL PROCES
        INPUT='GRS,'//AW2
        CALL PROCES
        INPUT='GRX,'//AW3
        CALL PROCES
        INPUT='GRY,'//AW4
        CALL PROCES
        INPUT='GRZ,'//AW5
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(ISET.EQ.3) THEN
C       REQUEST GRATING DELETTION
        IF(ALENS(96,J).NE.0.0D0) THEN
C       DELETE THE GRATING DEFINITION
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='GRTD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
                END IF
C
C       ARRAY LENS
C
        IF(JSET.EQ.1) THEN
C       NO ARRAY DEFINITION
        IF(ALENS(133,J).NE.0.0D0) THEN
C       DELETE ARRAY LENS DEFINITION
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='ARRAYD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
                END IF
        IF(JSET.EQ.2) THEN
C       REQUEST ARRAY LENS ASSIGNMENT
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        Call WDialogGetDouble(IDF_DX,DW1)
        CALL DTOA23(DW1,AW1)
        Call WDialogGetDouble(IDF_DY,DW2)
        CALL DTOA23(DW2,AW2)
        IF(KSET.EQ.1) THEN
C       ODD ARRAY
        INPUT='ARRAY ODD,'//AW1//','//AW2
        CALL PROCES
                END IF
        IF(KSET.EQ.2) THEN
C       ODD ARRAY
        INPUT='ARRAY EVEN,'//AW1//','//AW2
        CALL PROCES
                END IF
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
        IF(JSET.EQ.3) THEN
C       REQUEST ARRAY DEFINITION DELETTION
        IF(ALENS(133,J).NE.0.0D0) THEN
C       DELETE THE ARRAY LENS DEFINITION
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='CHG,'//AJ
        CALL PROCES
        INPUT='ARRAYD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
                END IF
                END IF
C
        INCLUDE 'LOADSHEET.INC'
        INCLUDE 'ARRAYGRTFRESH.INC'
        CALL WDIALOGSELECT(IDD_GRTARRAY)
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (IDCANCEL)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDIALOGSELECT(IDD_GRTARRAY)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
                        END IF
C
