C       THIS CODE TAKES CARE OF THE OPERATION OF
C       TORICS AND ASPHERICS
C
        IF(MESSAGE%WIN.EQ.IDD_ASPH) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_ASPH)

        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDOK)
C
C       GET VALUES AND MAKE APPROPRIATE CHANGES
C
C       TORIC PRESENCE
        CALL WDIALOGGETRADIOBUTTON(IDF_TR1,ISET)
C       1 NO TORIC
C       2 YTORIC
C       3 XTORIC

C       TORIC VALUE MODE
        CALL WDIALOGGETRADIOBUTTON(IDF_T1,JSET)
C       1 CURVATURE
C       2 RADIUS
C
        IF(ISET.EQ.1) THEN
C       NONTORIC

C       CONICS
        CALL WDIALOGGETDOUBLE(IDF_CC,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(1,J).NE.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CC '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
C
C       ASPHERICS
C       2ND
        CALL WDIALOGGETDOUBLE(IDF_2ND,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(1,J).EQ.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AC '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
C
C       4TH
        CALL WDIALOGGETDOUBLE(IDF_4TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AD '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       6TH
        CALL WDIALOGGETDOUBLE(IDF_6TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AE '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       8TH
        CALL WDIALOGGETDOUBLE(IDF_8TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AF '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       10TH
        CALL WDIALOGGETDOUBLE(IDF_10TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AG '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       12TH
        CALL WDIALOGGETDOUBLE(IDF_12TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AH '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       14TH
        CALL WDIALOGGETDOUBLE(IDF_14TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AI '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       16TH
        CALL WDIALOGGETDOUBLE(IDF_16TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AJ '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       18TH
        CALL WDIALOGGETDOUBLE(IDF_18TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AK '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       20TH
        CALL WDIALOGGETDOUBLE(IDF_20TH,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(8,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AL '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
C
        IF(ISET.EQ.2.OR.ISET.EQ.3) THEN
C       YTORIC
        CALL WDIALOGGETDOUBLE(IDF_TORVAL,DW1)
        CALL DTOA23(DW1,AW1)
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        IF(ISET.EQ.2) INPUT='YTORIC'
        IF(ISET.EQ.3) INPUT='XTORIC'
        CALL PROCES
        IF(JSET.EQ.1) INPUT='RDTOR '//AW1
        IF(JSET.EQ.2) INPUT='CVTOR '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
C
C       TORIC CONICS
        CALL WDIALOGGETDOUBLE(IDF_TCC,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(24,J).NE.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='CCTOR '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
C
C       4THA
        CALL WDIALOGGETDOUBLE(IDF_4THA,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(4,J).NE.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(36,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ADTOR '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
C
C       6THA
        CALL WDIALOGGETDOUBLE(IDF_6THA,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(5,J).NE.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(36,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AETOR '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
C
C       8THA
        CALL WDIALOGGETDOUBLE(IDF_8THA,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(6,J).NE.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(36,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AFTOR '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
C
C       10THA
        CALL WDIALOGGETDOUBLE(IDF_10THA,DW1)
        CALL DTOA23(DW1,AW1)
        IF(ALENS(7,J).NE.0.0D0) THEN
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        IF(ALENS(36,J).EQ.0.0D0) THEN
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TASPH'
        CALL PROCES
        END IF
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='AGTOR '//AW1
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                ELSE
C       NO CHANGE
                END IF
                        END IF
C
        CALL WDIALOGSELECT(IDD_ASPH)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (IDCANCEL)
        CALL WDIALOGSELECT(IDD_ASPH)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)


        CASE (IDF_TORD)
C       DELETE ANY EXISTING TORICS
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TORD'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDIALOGPUTRADIOBUTTON(IDF_TR1)
        CALL WDIALOGPUTDOUBLE(IDF_TORVAL,0.0D0)
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_ASPH)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_ASPHD)
C       DELETE ANY ASPHERICS AND CONICS
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='ASPHD'
        CALL PROCES
        INPUT='CC 0.0D0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDIALOGPUTDOUBLE(IDF_2ND,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_4TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_6TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_8TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_10TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_12TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_14TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_16TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_18TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_20TH,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_CC,0.0D0)
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_ASPH)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE (IDF_TASPHD)
C       DELETE ANAMORPHIC ASPHERICS AND TORIC CONIC
        INPUT='U L'
        CALL PROCES
        J=EDIT_SURFACE
        CALL ITOAA(J,AJ)
        INPUT='CHG '//AJ
        CALL PROCES
        INPUT='TASPHD'
        CALL PROCES
        INPUT='CCTOR 0.0D0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDIALOGPUTDOUBLE(IDF_4THA,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_6THA,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_8THA,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_10THA,0.0D0)
        CALL WDIALOGPUTDOUBLE(IDF_TCC,0.0D0)
        INCLUDE 'LOADSHEET.INC'
        CALL WDIALOGSELECT(IDD_ASPH)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        END SELECT
C
                END IF
