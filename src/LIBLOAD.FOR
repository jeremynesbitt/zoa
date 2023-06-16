        SUBROUTINE LIBLOAD(LIBLIST, LIBARRAY)
        USE GLOBALS
        IMPLICIT NONE
        !INCLUDE 'DATMAI.INC'
        !INCLUDE 'DATLEN.INC'
        INTEGER I,LIBLIST
        CHARACTER*132 LINE,TEMP80
        CHARACTER(len=80), dimension(999) :: LIBARRAY
        WRITE ( *, '(a)' ) '  Hello from LIBLOAD!'
        CALL PROCESKDP('OUT ED')
        CALL PROCESKDP('LIB P')
        CALL PROCESKDP('OUT TP')
        !INPUT='OUT ED'
        !CALL PROCES
        !INPUT='LIB P'
        !CALL PROCES
        !INPUT='OUT TP'
        !CALL PROCES
        CALL CLOSE_FILE(9,1)
        WRITE ( *, '(a)' ) '  Hello from LIBLOAD!'
        OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=trim(basePath)//'EDITTEXT.DAT'
     2  ,STATUS='UNKNOWN')
        WRITE ( *, '(a)' ) ' LIBLOAD AFTER LOADING FILE!'
        REWIND(9)
        READ(9,25,ERR=90,END=90) LINE
        WRITE ( *, '(a)' ) '  Hello from LIBLOAD after reading LINE!'
        I=1
 1      READ(9,25,ERR=90,END=90) LINE
        LIBARRAY(I)=LINE(1:4)//' --- '//LINE(25:80)
        I=I+1
        GO TO 1
 90     CONTINUE
        CALL CLOSE_FILE(9,0)
 25     FORMAT(A132)
        LIBLIST=I-1
        RETURN
        END
