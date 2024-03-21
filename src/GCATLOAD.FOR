        SUBROUTINE GCATLOAD(CATLIST, CATARRAY)
        USE GLOBALS
        IMPLICIT NONE
        INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
        INTEGER OLDOUT,OLDIN,I,JJ,CATLIST,FIRSTJ,LASTJ
        CHARACTER*132 LINE,TEMP27
        CHARACTER CATARRAY(1:9999)*50
        PRINT *, "Glass CAT Load Module"
        INPUT='OUT ED'
        CALL PROCES
        INPUT='GLASSP GLAK'
        CALL PROCES
        INPUT='OUT TP'
        CALL PROCES
        CALL CLOSE_FILE(9,1)
        OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=trim(basePath)//'EDITTEXT.DAT'
     2  ,STATUS='UNKNOWN')
        REWIND(9)
        I=1
 1      READ(9,25,ERR=90,END=90) LINE
        CATARRAY(I)=LINE(1:27)
                        DO JJ=1,27
        IF(CATARRAY(I)(JJ:JJ).EQ.' ') THEN
                FIRSTJ=JJ
                GO TO 10
                        END IF
                        END DO
 10             CONTINUE
                DO JJ=FIRSTJ,27
        IF(CATARRAY(I)(JJ:JJ).NE.' ') THEN
                LASTJ=JJ
                GO TO 20
                        END IF
                        END DO
 20             CONTINUE

        TEMP27='                           '
        TEMP27=CATARRAY(I)(1:FIRSTJ)//CATARRAY(I)(LASTJ:27)
        CATARRAY(I)(1:27)=TEMP27

        I=I+1
        GO TO 1
 90     CONTINUE
        CALL CLOSE_FILE(9,0)
 25     FORMAT(A132)
        CATLIST=I-1
        RETURN
        END
