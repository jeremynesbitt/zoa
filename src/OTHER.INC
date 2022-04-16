C       THIS CODE TAKES CARE OF THE OPERATION OF THE OTHER
C       LENS PARAMETERS
C
        IF(MESSAGE%WIN.EQ.IDD_OTHER) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_OTHER)
        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDOK)
C
C       MAKE LENS CHANGES HERE
C
        Call WDialogGetDouble(IDF_INR,DW1)
        Call WDialogGetInteger(IDF_COATFILENUM,MSET)
        Call WDialogGetRadioButton(IDF_REAL1,ISET)
        Call WDialogGetString(IDF_LBL,STRINGER)
        Call WDialogGetRadioButton(IDF_FP1,JSET)
        Call WDialogGetRadioButton(IDF_DUM1,KSET)
        Call WDialogGetRadioButton(IDF_INR1,LSET)
        Call WDialogGetDouble(IDF_RAYERR,DW2)
        INPUT='U L'
        CALL PROCES
        CALL ITOAA(EDIT_SURFACE,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.1) INPUT='REAL'
        IF(ISET.EQ.2) INPUT='PARAX'
        CALL PROCES
        IF(JSET.EQ.1) INPUT='FOOTBLOK NO'
        IF(JSET.EQ.2) INPUT='FOOTBLOK YES'
        CALL PROCES
        IF(KSET.EQ.1) INPUT='NODUM NO'
        IF(KSET.EQ.2) INPUT='NODUM YES'
        CALL PROCES
        IF(GLANAM(EDIT_SURFACE,2).EQ.'REFL') THEN
        Call WDialogGetDouble(IDF_THM,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='THM,'//AW1
        CALL PROCES
                        END IF
        Call WDialogGetDouble(IDF_SPGR,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='SPGR,'//AW1
        CALL PROCES
        Call WDialogGetDouble(IDF_PRICE,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='PRICE,'//AW1
        CALL PROCES
C
        IF(LSET.EQ.2) THEN
C       EXPLICIT CHANGE
        Call WDialogGetDouble(IDF_INR,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='INR,'//AW1
        CALL PROCES
        END IF
        IF(LSET.EQ.1.AND.ALENS(143,EDIT_SURFACE).EQ.1.0D0) THEN
C       WAS EXPLICIT BUT NOW MAKE IT IMPLICIT
        INPUT='INRD'
        CALL PROCES
        END IF
C     RANDOM RAY ERROR
        CALL DTOA23(DW2,AW2)
        INPUT='RAYERROR,'//AW2
        CALL PROCES

C       LBL
        INPUT='LBL,'//STRINGER
        CALL PROCES
C
C       COATING FILE NUMBER
        WRITE(INPUT,*) 'COATING,',MSET
        CALL PROCES
C
        INPUT='EOS'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
C
        INCLUDE 'LOADSHEET.INC'
        INCLUDE 'OTHERFRESH.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDIALOGSELECT(IDD_OTHER)
        CALL WDialogShow(-1,-1,0,Modeless)
C       CALL WDialogHide()
C       CALL WDialogUnload()
C       INCLUDE 'LOADSHEET.INC'
C       CALL WDialogShow(-1,-1,0,Modeless)

        CASE (IDCANCEL)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDIALOGSELECT(IDD_OTHER)
        CALL WDialogShow(-1,-1,0,Modeless)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
                        END IF
C
