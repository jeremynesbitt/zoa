C       HERE WE PLACE THE CODE THAT UPDATES THE LENS AND RELOADES THE LENS
C       GET CELL VALUE AND PUT IN TEMPVAL
C
C
C       RADIUS OF CURVATURE CHANGES
        IF(PREVCOL.EQ.1) THEN
        CALL WGRIDGETCELLDOUBLE(IDF_LENGRID1,PREVCOL,PREVROW,TEMPVAL)
        IF(TEMPVAL.NE.RD((PREVROW-1),2)) THEN
                INPUT='U L'
                CALL PROCES
                WRITE(INPUT,*) 'CHG,',PREVROW-1
                CALL PROCES
        IF(RDM) THEN
                WRITE(INPUT,*) 'RD,',TEMPVAL
                CALL PROCES
        ELSE
                WRITE(INPUT,*) 'CV,',TEMPVAL
                CALL PROCES
        END IF
                INPUT='EOS'
                CALL PROCES
                INPUT='OUT TP'
                CALL PROCES
                INCLUDE 'LOADSHEET.INC'
                END IF
                END IF
C       THICKNESS CHANGES
        IF(PREVCOL.EQ.2) THEN
        CALL WGRIDGETCELLDOUBLE(IDF_LENGRID1,PREVCOL,PREVROW,TEMPVAL)
        IF(TEMPVAL.NE.TH((PREVROW-1),2)) THEN
                INPUT='U L'
                CALL PROCES
                CALL ITOAA((PREVROW-1),AJ)
                INPUT='CHG '//AJ
                CALL PROCES
                CALL DTOA23(TEMPVAL,ATEMPVAL)
                INPUT='TH '//ATEMPVAL
                CALL PROCES
                INPUT='EOS'
                CALL PROCES
                INPUT='OUT TP'
                CALL PROCES
                INCLUDE 'LOADSHEET.INC'
                END IF
                END IF
