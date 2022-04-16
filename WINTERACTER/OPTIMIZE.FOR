        SUBROUTINE OPTIMIZE(MESSAGE)
        USE WINTERACTER
        IMPLICIT NONE
        REAL*8 DW1,DW2,DW3,DW4,DW5
        INTEGER ISET,JSET,KSET
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        INCLUDE 'RESOURCE.INC'
        TYPE(WIN_MESSAGE)   :: MESSAGE
C*****************************************************************************************
C*****************************************************************************************
C
C       THIS CODE TAKES CARE OF THE OPERATION OF THE OPTIMIZE GUI
C
C       OPTIMIZE
C
C
        CALL WDIALOGSELECT(IDD_OPTIM)
        CALL WDIALOGFIELDSTATE(IDF_PFAC,1)
        CALL WDIALOGFIELDSTATE(IDF_CY,1)
        CALL WDIALOGFIELDSTATE(IDF_CF,1)
C
        SELECT CASE(MESSAGE%VALUE1)
C
        CASE(IDF_MEIRON)
        CALL WDIALOGFIELDSTATE(IDF_CY,1)
        CALL WDIALOGFIELDSTATE(IDF_CF,1)
        CALL WDIALOGFIELDSTATE(IDF_PFAC,1)
        CALL WDIALOGGETDOUBLE(IDF_PFAC,DW1)
        WRITE(OUTLYNE,*) 'PFAC RESET TO: ',DW1
        CALL SHOWIT(1)
        PFAC=DW1
        CALL WDIALOGPUTDOUBLE(IDF_PFAC,DW1)
        CALL WDialogShow(-1,-1,0,Modeless)
C
        CASE(IDF_PFIND)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
C
        CALL WDIALOGGETINTEGER(IDF_CY,ISET)
        CALL WDIALOGGETDOUBLE(IDF_CF,DW1)
        WRITE(INPUT,*) 'PFIND,',ISET,',',DW1
        CALL PROCES
        CALL WDIALOGPUTDOUBLE(IDF_PFAC,PFAC)
C
        CASE(IDF_REST1)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        INPUT='RESTORE'
        CALL PROCES
C
        CASE(IDF_REST2)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        INPUT='RESTORE MIN'
        CALL PROCES
C
        CASE(IDF_REST3)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        INPUT='RESTORE ORIG'
        CALL PROCES
C
        CASE(IDF_ITER)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        CALL WDIALOGGETINTEGER(IDF_NITER,ISET)
        WRITE(INPUT,*) 'ITER,',ISET
        CALL PROCES
C
        CASE(IDF_ITERFULL)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        CALL WDIALOGGETINTEGER(IDF_NITERFULL,ISET)
        WRITE(INPUT,*) 'ITER FULL,',ISET
        CALL PROCES
C
        CASE(IDF_ITERP)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        CALL WDIALOGGETINTEGER(IDF_NITERP,ISET)
        WRITE(INPUT,*) 'IT P,',ISET
        CALL PROCES
C
        CASE(IDF_ROBB)
C       BOOK KEEPING
        CALL WDIALOGGETCHECKBOX(IDF_VERB,JSET)
        IF(JSET.EQ.0) THEN
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,0)
                INPUT='OVERBOSE NO'
                CALL PROCES
                ELSE
        CALL WDIALOGPUTCHECKBOX(IDF_VERB,1)
                INPUT='OVERBOSE YES'
                CALL PROCES
                END IF
        CALL WDIALOGGETDOUBLE(IDF_BETA,DW1)
        CALL WDIALOGGETDOUBLE(IDF_DELTA,DW2)
        CALL WDIALOGGETINTEGER(IDF_NROBB,ISET)
        WRITE(INPUT,*) 'ROBB,',DW1,',',DW2,',',ISET
        CALL PROCES
C
        CASE(IDCANCEL)
        CALL WDialogHide()
        CALL WDialogUnload()
        END SELECT
                        RETURN
                        END
