C
        IF(MESSAGE%WIN.EQ.IDD_LENOCV) THEN
        CALL WDIALOGSELECT(IDD_LENOCV)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        CALL WDIALOGGETSTRING(IDF_FILE,FILENAME)
        FILENAME=TRIM(FILENAME)
        INPUT='OUT FILE '//FILENAME
        CALL PROCES
        INPUT='LENO CV'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        WRITE(OUTLYNE,*)
     1  'CURRENT LENS OUTPUT IN CODE-V FORMAT'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'TO FILE NAMED : '//FILENAME
        CALL SHOWIT(1)
        CALL WDialogHide()
        CALL WDialogUnload()
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        END IF
C
        IF(MESSAGE%WIN.EQ.IDD_LENOACC) THEN
        CALL WDIALOGSELECT(IDD_LENOACC)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        CALL WDIALOGGETSTRING(IDF_FILE,FILENAME)
        FILENAME=TRIM(FILENAME)
        INPUT='OUT FILE '//FILENAME
        CALL PROCES
        INPUT='LENO AC'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        WRITE(OUTLYNE,*)
     1  'CURRENT LENS OUTPUT IN ACCOS-V FORMAT'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'TO FILE NAMED : '//FILENAME
        CALL SHOWIT(1)
        CALL WDialogHide()
        CALL WDialogUnload()
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        END IF
C
        IF(MESSAGE%WIN.EQ.IDD_IMPORT_CV) THEN
        CALL WDIALOGSELECT(IDD_IMPORT_CV)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        CALL WDIALOGGETSTRING(IDF_FILE,FILENAME)
        FILENAME=TRIM(FILENAME)
        INPUT='CV2PRG '//FILENAME
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        WRITE(OUTLYNE,*)
     1  TRIM(FILENAME)//' IMPORTED AS NEW CURRENT LENS'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'CHECK CONVERT.ERR FOR ITEMS NOT IMPORTED'
        CALL SHOWIT(1)
        CALL WDialogHide()
        CALL WDialogUnload()
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        END IF
C
        IF(MESSAGE%WIN.EQ.IDD_IMPORT_ZMX) THEN
        CALL WDIALOGSELECT(IDD_IMPORT_ZMX)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE (IDOK)
        CALL WDIALOGGETSTRING(IDF_FILE,FILENAME)
        FILENAME=TRIM(FILENAME)
        INPUT='ZMX2PRG '//FILENAME
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        WRITE(OUTLYNE,*)
     1  TRIM(FILENAME)//' IMPORTED AS NEW CURRENT LENS'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'CHECK CONVERT.ERR FOR ITEMS NOT IMPORTED'
        CALL SHOWIT(1)
        CALL WDialogHide()
        CALL WDialogUnload()
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        END IF
