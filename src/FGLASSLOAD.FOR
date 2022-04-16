        SUBROUTINE FGLASSLOAD(FNDLIST,J)
        IMPLICIT NONE
        INTEGER JJ
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        CHARACTER AJ*3
        INTEGER I,J,FNDLIST,NUMINLIST,FIRSTJ,LASTJ
        CHARACTER*132 LINE,TEMP27*27
        INPUT='OUT ED'
        CALL PROCES
        CALL ITOAA(J,AJ)
        INPUT='FINDGLASS ,'//AJ//','//'9999'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='EDITTEXT.DAT'
     2  ,STATUS='UNKNOWN')
        REWIND(9)
        I=1
 1      READ(9,25,ERR=90,END=90) LINE
        IF(I.GT.4) THEN
        FNDARRAY(I-4)(1:27)=LINE(6:32)
                        DO JJ=1,27
        IF(FNDARRAY(I-4)(JJ:JJ).EQ.' ') THEN
                FIRSTJ=JJ
                GO TO 10
                        END IF
                        END DO
 10             CONTINUE
                DO JJ=FIRSTJ,27
        IF(FNDARRAY(I-4)(JJ:JJ).NE.' ') THEN
                LASTJ=JJ
                GO TO 20
                        END IF
                        END DO
 20             CONTINUE

        TEMP27='                           '
        TEMP27=FNDARRAY(I-4)(1:FIRSTJ)//FNDARRAY(I-4)(LASTJ:27)
        FNDARRAY(I-4)(1:27)=TEMP27(1:27)
                END IF
        I=I+1
        GO TO 1
 90     CONTINUE
        CALL CLOSE_FILE(9,0)
 25     FORMAT(A132)
        FNDLIST=I-5
        RETURN
        END
