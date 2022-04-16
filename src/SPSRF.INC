C       THIS CODE TAKES CARE OF THE OPERATION OF THE
C       SPECIAL SURFACE DATA
C
        IF(MESSAGE%WIN.EQ.IDD_SPSRF) THEN
        J=EDIT_SURFACE
        CALL WDIALOGSELECT(IDD_SPSRF)
        SELECT CASE(MESSAGE%VALUE1)

        CASE (IDOK)
        CALL WDIALOGGETRADIOBUTTON(IDF_B1,ISET)
        CALL ITOAA(J,AJ)
        SELECT CASE (ISET)

        CASE (1)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.1.0D0) THEN
        INPUT='SPECIAL,'//AJ//',1'
        CALL PROCES
                        END IF
        CALL DOCOEFFS39(J)
        INPUT='EOS'
        CALL PROCES

        CASE (2)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.2.0D0) THEN
        INPUT='SPECIAL,'//AJ//',2'
        CALL PROCES
                        END IF
        CALL DOCOEFFS66(J)
        INPUT='EOS'
        CALL PROCES

        CASE (3)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.3.0D0) THEN
        INPUT='SPECIAL,'//AJ//',3'
        CALL PROCES
                        END IF
        CALL DOCOEFFS37(J)
        INPUT='EOS'
        CALL PROCES

        CASE (4)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.4.0D0) THEN
        INPUT='SPECIAL,'//AJ//',4'
        CALL PROCES
                        END IF
        CALL DOCOEFFS3(J)
        INPUT='EOS'
        CALL PROCES

        CASE (5)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.5.0D0) THEN
        INPUT='SPECIAL,'//AJ//',5'
        CALL PROCES
                        END IF
        CALL DOCOEFFS96(J)
        INPUT='EOS'
        CALL PROCES

        CASE (6)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.6.0D0) THEN
        INPUT='SPECIAL,'//AJ//',6'
        CALL PROCES
                        END IF
        CALL DOCOEFFS39(J)
        INPUT='EOS'
        CALL PROCES

        CASE (7)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.7.0D0) THEN
        INPUT='SPECIAL,'//AJ//',7'
        CALL PROCES
                        END IF
        CALL DOCOEFFS91(J)
        INPUT='EOS'
        CALL PROCES

        CASE (8)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.8.0D0) THEN
        INPUT='SPECIAL,'//AJ//',8'
        CALL PROCES
                        END IF
        CALL DOCOEFFS91(J)
        INPUT='EOS'
        CALL PROCES

        CASE (9)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.9.0D0) THEN
        INPUT='SPECIAL,'//AJ//',9'
        CALL PROCES
                        END IF
        CALL DOCOEFFS66(J)
        INPUT='EOS'
        CALL PROCES

        CASE (10)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.10.0D0) THEN
        INPUT='SPECIAL,'//AJ//',10'
        CALL PROCES
                        END IF
        CALL DOCOEFFS37(J)
        INPUT='EOS'
        CALL PROCES

        CASE (11)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.11.0D0) THEN
        INPUT='SPECIAL,'//AJ//',11'
        CALL PROCES
                        END IF
        CALL DOCOEFFS96(J)
        INPUT='EOS'
        CALL PROCES

        CASE (12)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 0.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS11(J)
        INPUT='EOS'
        CALL PROCES

        CASE (13)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 1.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS31(J)
        INPUT='EOS'
        CALL PROCES

        CASE (14)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 2.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS89(J)
        INPUT='EOS'
        CALL PROCES

        CASE (15)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 3.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS89(J)
        INPUT='EOS'
        CALL PROCES

        CASE (16)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 4.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS89(J)
        INPUT='EOS'
        CALL PROCES

        CASE (17)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 5.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS89(J)
        INPUT='EOS'
        CALL PROCES

        CASE (18)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 6.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS31(J)
        INPUT='EOS'
        CALL PROCES

        CASE (19)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11 7.0D0'
        CALL PROCES
                        END IF
        CALL DOCOEFFS96(J)
        INPUT='EOS'
        CALL PROCES

        CASE (20)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.13.0D0) THEN
        INPUT='SPECIAL,'//AJ//',13'
        CALL PROCES
                        END IF
        CALL DOCOEFFS14(J)
        INPUT='EOS'
        CALL PROCES

        CASE (21)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.14.0D0) THEN
        INPUT='SPECIAL,'//AJ//',14'
        CALL PROCES
                        END IF
        CALL DOCOEFFS48(J)
        INPUT='EOS'
        CALL PROCES

        CASE (22)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.15.0D0) THEN
        INPUT='SPECIAL,'//AJ//',15'
        CALL PROCES
                        END IF
        CALL DOCOEFFS48(J)
        INPUT='EOS'
        CALL PROCES

        CASE (23)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.16.0D0) THEN
        INPUT='SPECIAL,'//AJ//',16'
        CALL PROCES
                        END IF
        CALL DOCOEFFS11(J)
        INPUT='EOS'
        CALL PROCES

        CASE (24)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.17.0D0) THEN
        INPUT='SPECIAL,'//AJ//',17'
        CALL PROCES
                        END IF
        CALL DOCOEFFS96(J)
        INPUT='EOS'
        CALL PROCES

        CASE (25)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.18.0D0) THEN
        INPUT='SPECIAL,'//AJ//',18'
        CALL PROCES
                        END IF
        CALL DOCOEFFS18(J)
        INPUT='EOS'
        CALL PROCES

        CASE (26)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.19.0D0) THEN
        INPUT='SPECIAL,'//AJ//',19'
        CALL PROCES
                        END IF
        CALL DOCOEFFS4(J)
        INPUT='EOS'
        CALL PROCES

        CASE (27)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.20.0D0) THEN
        INPUT='SPECIAL,'//AJ//',20'
        CALL PROCES
                        END IF
        CALL DOCOEFFS5(J)
        INPUT='EOS'
        CALL PROCES

        CASE (28)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.21.0D0) THEN
        INPUT='SPECIAL,'//AJ//',21'
        CALL PROCES
                        END IF
        CALL DOCOEFFS96(J)
        INPUT='EOS'
        CALL PROCES

        CASE (29)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.22.0D0) THEN
        INPUT='SPECIAL,'//AJ//',22'
        CALL PROCES
                        END IF
        CALL DOCOEFFS4(J)
        INPUT='EOS'
        CALL PROCES

        CASE (30)

        INPUT='U SP'
        CALL PROCES
        IF(DABS(ALENS(34,J)).NE.23.0D0) THEN
        INPUT='SPECIAL,'//AJ//',23'
        CALL PROCES
                        END IF
        CALL DOCOEFFS96(J)
        INPUT='EOS'
        CALL PROCES

        END SELECT
        INCLUDE 'LOADSHEET.INC'
        CALL SPSRFFRESH(J)
        CALL WDIALOGSELECT(IDD_SPSRF)
        CALL WDialogHide()
        CALL WDialogUnload()
        INCLUDE 'LOADSHEET.INC'
        CALL WDialogShow(-1,-1,0,Modeless)

        CASE (IDF_SPSRFON)
        IF(ALENS(34,J).GE.0.0D0) THEN
                ELSE
        CALL ITOAA(J,AJ)
        INPUT='SPSRF ON,'//AJ
        CALL PROCES
        CALL SPSRFFRESH(J)
                END IF
C
        CASE (IDF_SPSRFOFF)
        IF(ALENS(34,J).LE.0.0D0) THEN
                ELSE
        CALL ITOAA(J,AJ)
        INPUT='SPSRF OFF,'//AJ
        CALL PROCES
        CALL SPSRFFRESH(J)
                END IF

        CASE (IDF_SPDEL)
        INPUT='U SP'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='SPDEL,'//AJ
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
        CALL WDIALOGCLEARFIELD(IDF_B1)
        CALL WDIALOGCLEARFIELD(IDF_B2)
        CALL WDIALOGCLEARFIELD(IDF_B3)
        CALL WDIALOGCLEARFIELD(IDF_B4)
        CALL WDIALOGCLEARFIELD(IDF_B5)
        CALL WDIALOGCLEARFIELD(IDF_B6)
        CALL WDIALOGCLEARFIELD(IDF_B7)
        CALL WDIALOGCLEARFIELD(IDF_B8)
        CALL WDIALOGCLEARFIELD(IDF_B9)
        CALL WDIALOGCLEARFIELD(IDF_B10)
        CALL WDIALOGCLEARFIELD(IDF_B11)
        CALL WDIALOGCLEARFIELD(IDF_B12a)
        CALL WDIALOGCLEARFIELD(IDF_B12b)
        CALL WDIALOGCLEARFIELD(IDF_B12c)
        CALL WDIALOGCLEARFIELD(IDF_B12d)
        CALL WDIALOGCLEARFIELD(IDF_B12e)
        CALL WDIALOGCLEARFIELD(IDF_B12f)
        CALL WDIALOGCLEARFIELD(IDF_B12g)
        CALL WDIALOGCLEARFIELD(IDF_B12h)
        CALL WDIALOGCLEARFIELD(IDF_B13)
        CALL WDIALOGCLEARFIELD(IDF_B14)
        CALL WDIALOGCLEARFIELD(IDF_B15)
        CALL WDIALOGCLEARFIELD(IDF_B16)
        CALL WDIALOGCLEARFIELD(IDF_B17)
        CALL WDIALOGCLEARFIELD(IDF_B18)
        CALL WDIALOGCLEARFIELD(IDF_B19)
        CALL WDIALOGCLEARFIELD(IDF_B20)
        CALL WDIALOGCLEARFIELD(IDF_B21)
        CALL WDIALOGCLEARFIELD(IDF_B22)
        CALL WDIALOGCLEARFIELD(IDF_B23)
                DO I=1,96
        CALL WGRIDCLEARCELL(IDF_GRID1,1,I)
        CALL WGRIDCLEARCELL(IDF_GRID1,2,I)
        CALL WGRIDSTATE(IDF_GRID1,1,I)
        CALL WGRIDSTATE(IDF_GRID1,2,I)
                END DO
        CALL SPSRFFRESH(J)

        CASE (IDF_S1)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.1.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',1'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB1(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B1)
        ELSE
        GUIERROR=.FALSE.
        END IF

        CASE (IDF_S2)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.2.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',2'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB2(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B2)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S3)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.3.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',3'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB3(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B3)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S4)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.4.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',4'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB4(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B4)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S5)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.5.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',5'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB5(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B5)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S6)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.6.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',6'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB6(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B6)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S7)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.7.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',7'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB7(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B7)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S8)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.8.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',8'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB8(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B8)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S9)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.9.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',9'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB9(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B9)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S10)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.10.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',10'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB10(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B10)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S11)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.11.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',11'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB11(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B11)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12a)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',0'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12a(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12a)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12b)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',1'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                                END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12b(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12b)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12c)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',2'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12c(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12c)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12d)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',3'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12d(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12d)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12e)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',4'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12e(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12e)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12f)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',5'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12f(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12f)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12g)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',6'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12g(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12g)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S12h)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.12.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',12'
        CALL PROCES
        INPUT='C11,'//AJ//',7'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB12h(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B12h)
        ELSE
        GUIERROR=.FALSE.
                        END IF


        CASE (IDF_S13)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.13.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',13'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB13(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B13)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S14)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.14.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',14'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB14(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B14)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S15)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.15.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',15'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB15(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B15)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S16)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.16.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',16'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB16(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B16)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S17)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.17.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',17'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB17(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B17)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S18)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.18.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',18'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB18(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B18)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S19)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.19.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',19'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB19(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B19)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S20)

C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.20.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',20'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB20(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B20)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S21)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.21.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',21'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB21(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B21)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S22)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.22.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',22'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB22(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B22)
        ELSE
        GUIERROR=.FALSE.
                        END IF

        CASE (IDF_S23)
C       LOAD THE APPROPRIATE COEF DESCRIPTIONS AND VALUES
        IF(DABS(ALENS(34,J)).NE.23.0D0) THEN
        CALL ITOAA(J,AJ)
        INPUT='U SP'
        CALL PROCES
        INPUT='SPECIAL,'//AJ//',23'
        CALL PROCES
        INPUT='EOS'
        CALL PROCES
                        END IF
        IF(.NOT.GUIERROR) THEN
        CALL LOADGRIDA
        CALL LOADGRIDB23(J)
        CALL WDIALOGPUTRADIOBUTTON(IDF_B23)
        ELSE
        GUIERROR=.FALSE.
                        END IF
C
        END SELECT
        CALL SPSRFFRESH(J)
                        END IF
C
