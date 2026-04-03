C macro-stubs.FOR - Replaces MACRO1-4.FOR
C
C Contains the subroutines from the old binary macro system that are still
C called by non-macro code.  The binary macro execution engine (MACRUN,
C MACIN, MACEXC, MACCOM, etc.) has been removed; only the following survive:
C
C   MACFAL          - error handler (minimal: binary-macro path is dead)
C   LTH80/LTH140    - string-length utilities used by CONFIG3
C   SORT_JK1        - heapsort used by PLOTCAD4 and WAVSPOT7
C   LENSSAVE_NOOPT  - LSAVE command
C   LENSSAVE        - LENSSAVE command
C   LENSREST        - LENSREST command
C
C ---------------------------------------------------------------------------
C MACFAL - called on input errors to abort in-progress macro/optimization.
C Without the binary macro system MACFAILURE is always .FALSE., so only the
C four flag assignments at the top run; the rest is dead code.
C ---------------------------------------------------------------------------
        SUBROUTINE MACFAL
        USE GLOBALS
        use DATSUB
        IMPLICIT NONE
        LOGICAL ITERROR
        INCLUDE 'DATMAI.INC'
C
                        FOBRUN=.FALSE.
                        ITERROR=.TRUE.
                        GUIERROR=.TRUE.
                        ALLSTOP=.TRUE.
C     With the binary macro system removed, MACFAILURE is always .FALSE.
C     so we return immediately without resetting any F-flags.
        IF(.NOT.MACFAILURE) RETURN
C
C     Guard: if somehow MACFAILURE is true (shouldn't happen), reset to
C     a safe CMD-level state.
                        BADOPS=.TRUE.
                        F1=1
                        F2=0
                        F3=0
                        F4=0
                        F5=0
                        F6=0
                        F7=0
                        F8=0
                        F9=0
                        F10=0
                        F11=0
                        F12=1
                        F13=0
                        F14=0
                        F15=0
                        F16=0
                        F17=0
                        F18=0
                        F26=0
                        F27=0
                        F29=0
                        F30=0
                        F31=0
                        F32=0
                        F35=0
                        F36=0
                        F37=0
                        F38=0
                        F39=0
                        F40=0
                        F41=0
                        F42=0
                        F43=0
                        F44=0
                        F45=0
                        F46=0
                        F47=0
                        F48=0
                        F49=0
                        F50=0
                        F51=0
                        F52=0
                        F53=0
                        F54=0
                        F55=0
                        RETURN
        END SUBROUTINE MACFAL
C
C ---------------------------------------------------------------------------
C LTH80 - last non-blank position in an 80-character string
C ---------------------------------------------------------------------------
        SUBROUTINE LTH80(IPT80,LENG)
        IMPLICIT NONE
        CHARACTER IPT80*80
        INTEGER LENG,QBVAL,I
        LENG=80
        DO 10 I=80,1,-1
        QBVAL=ICHAR(IPT80(I:I))
        IF(QBVAL.EQ.32) THEN
        LENG=LENG-1
        GO TO 10
        ELSE
        RETURN
        END IF
 10     CONTINUE
        RETURN
        END SUBROUTINE LTH80
C
C ---------------------------------------------------------------------------
C LTH140 - last non-blank position in a 140-character string
C ---------------------------------------------------------------------------
        SUBROUTINE LTH140(IPT140,LENG)
        IMPLICIT NONE
        CHARACTER IPT140*140
        INTEGER LENG,QBVAL,I
        LENG=140
        DO 10 I=140,1,-1
        QBVAL=ICHAR(IPT140(I:I))
        IF(QBVAL.EQ.32) THEN
        LENG=LENG-1
        GO TO 10
        ELSE
        RETURN
        END IF
 10     CONTINUE
        RETURN
        END SUBROUTINE LTH140
C
C ---------------------------------------------------------------------------
C SORT_JK1 - heapsort: sorts RA ascending; RA1 and RA2 follow the permutation.
C Used for field sorting in PLOTCAD4 and WAVSPOT7.
C ---------------------------------------------------------------------------
      SUBROUTINE SORT_JK1(N,RA,RA1,RA2)
C     USED IN FIELD SORTING OR MULTIPLE FIELDS OF VIEW
      IMPLICIT NONE
      REAL*8 RA,RRA,RRA1,RRA2,RA1,RA2
      INTEGER N,L,IR,J,I
      DIMENSION RA(N),RA1(N),RA2(N)
      INCLUDE 'DATMAI.INC'
      L=N/2+1
      IR=N
 10   CONTINUE
      IF(L.GT.1) THEN
      L=L-1
      RRA=RA(L)
      RRA1=RA1(L)
      RRA2=RA2(L)
          ELSE
      RRA=RA(IR)
      RRA1=RA1(IR)
      RRA2=RA2(IR)
      RA(IR)=RA(1)
      RA1(IR)=RA1(1)
      RA2(IR)=RA2(1)
      IR=IR-1
      IF(IR.EQ.1) THEN
      RA(1)=RRA
      RA1(1)=RRA1
      RA2(1)=RRA2
          RETURN
               END IF
               END IF
      I=L
      J=L+L
 20   IF(J.LE.IR) THEN
          IF(J.LT.IR) THEN
               IF(RA(J).LT.RA(J+1)) J=J+1
          END IF
      IF(RRA.LT.RA(J)) THEN
          RA(I)=RA(J)
          RA1(I)=RA1(J)
          RA2(I)=RA2(J)
               I=J
               J=J+1
                   ELSE
               J=IR+1
                   END IF
                   GO TO 20
                   END IF
                   RA(I)=RRA
                   RA1(I)=RRA1
                   RA2(I)=RRA2
                       GO TO 10
        END SUBROUTINE SORT_JK1
C
C ---------------------------------------------------------------------------
C LENSSAVE_NOOPT - LSAVE command: save lens without optimization data
C ---------------------------------------------------------------------------
        SUBROUTINE LENSSAVE_NOOPT
        USE GLOBALS
        IMPLICIT NONE
        CHARACTER LFILENAME*80
        INTEGER WSCNT
        INCLUDE 'DATMAI.INC'
C
        IF(STI.EQ.1) THEN
        OUTLYNE=
     1  '"LSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
      CALL SHOWIT(1)
        OUTLYNE=
     1  'WITHOUT SAVING OPTIMIZATION OR RAY DATA'
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(SN.EQ.1.OR.SQ.EQ.1) THEN
        OUTLYNE=
     1  '"LSAVE" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
      IF(SST.EQ.0) WS='LENS'
               WSCNT=LEN(TRIM(WS))
        WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
        CALL SHOWIT(1)
               LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
               WSCNT=LEN(TRIM(LFILENAME))
C
C     DELETE CURRENT FILE
      OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=TRIM(DIRLEN)//TRIM(LFILENAME)
     2  ,STATUS='UNKNOWN')
      CALL CLOSE_FILE(97,0)
C
C     SAVE I/O
          SAVE_KDP(1)=SAVEINPT(1)
      INPUT='OUT FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL PROCES
      INPUT='LENO NOOPT'
          CALL PROCES
      INPUT='IN TP'
          CALL PROCES
      INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/O
      REST_KDP(1)=RESTINPT(1)
        OUTLYNE=
     1  'CURRENT LENS SAVED AS:'//TRIM(DIRLEN)//TRIM(LFILENAME)
      CALL SHOWIT(1)
                        RETURN
        END SUBROUTINE LENSSAVE_NOOPT
C
C ---------------------------------------------------------------------------
C LENSSAVE - LENSSAVE command: save lens with all data
C ---------------------------------------------------------------------------
        SUBROUTINE LENSSAVE
        USE GLOBALS
        IMPLICIT NONE
        CHARACTER LFILENAME*80
        INTEGER WSCNT
        INCLUDE 'DATMAI.INC'
C
        IF(STI.EQ.1) THEN
        OUTLYNE=
     1  '"LENSSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(SN.EQ.1.OR.SQ.EQ.1) THEN
        OUTLYNE=
     1  '"LENSSAVE" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
      IF(SST.EQ.0) WS='LENS'
               WSCNT=LEN(TRIM(WS))
        WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
        CALL SHOWIT(1)
               LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
               WSCNT=LEN(TRIM(LFILENAME))
C
C     DELETE CURRENT FILE
      OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=TRIM(DIRLEN)//TRIM(LFILENAME)
     2  ,STATUS='UNKNOWN')
      CALL CLOSE_FILE(97,0)
C
C     SAVE I/O AND WRITE LENS
          SAVE_KDP(1)=SAVEINPT(1)
      INPUT='OUT FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL PROCES
        WRITE(OUTLYNE,*) INPUT(1:79)
        CALL SHOWIT(1)
      INPUT='LENO'
          CALL PROCES
      INPUT='IN TP'
          CALL PROCES
      INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/O
      REST_KDP(1)=RESTINPT(1)
        OUTLYNE=
     1  'CURRENT LENS SAVED AS:'//TRIM(DIRLEN)//TRIM(LFILENAME)
      CALL SHOWIT(1)
                        RETURN
        END SUBROUTINE LENSSAVE
C
C ---------------------------------------------------------------------------
C LENSREST - LENSREST command: restore lens from file
C ---------------------------------------------------------------------------
        SUBROUTINE LENSREST
        IMPLICIT NONE
        CHARACTER LFILENAME*80
        INTEGER WSCNT
        LOGICAL EXISJK
        INCLUDE 'DATMAI.INC'
C
        IF(STI.EQ.1) THEN
        OUTLYNE=
     1  '"LENSREST" RESTORES THE CURRENT LENS FROM AN ASCII FILE'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SN.EQ.1.OR.SQ.EQ.1) THEN
        OUTLYNE=
     1  '"LENSREST" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SST.EQ.0) THEN
        OUTLYNE=
     1  '"LENSREST" REQUIRES A FILE NAME'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
               WSCNT=LEN(TRIM(WS))
        WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
        CALL SHOWIT(1)
               LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
               WSCNT=LEN(TRIM(LFILENAME))
C
C     DOES THE FILE EXIST?
        EXISJK=.FALSE.
        INQUIRE(FILE=TRIM(DIRLEN)//TRIM(LFILENAME),EXIST=EXISJK)
      IF(.NOT.EXISJK) THEN
        OUTLYNE=
     1  'LENS FILE NAMED '//TRIM(LFILENAME)//' DOES NOT EXIST'
      CALL SHOWIT(1)
        OUTLYNE='NO LENS RESTORATION WAS PERFORMED'
      CALL SHOWIT(1)
                        CALL MACFAL
                       RETURN
                       END IF
C
C     SAVE I/O AND RESTORE LENS
          SAVE_KDP(1)=SAVEINPT(1)
      INPUT='IN TP'
          CALL PROCES
      INPUT='OUT TP'
          CALL PROCES
      INPUT='IN FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
          CALL PROCES
      INPUT='IN TP'
          CALL PROCES
      INPUT='OUT TP'
          CALL PROCES
C     RESTORE I/O
      REST_KDP(1)=RESTINPT(1)
        OUTLYNE=
     1  'LENS SAVED AS: '//TRIM(LFILENAME)//' HAS BEEN RESTORED'
      CALL SHOWIT(1)
                        RETURN
        END SUBROUTINE LENSREST
C
C ---------------------------------------------------------------------------
C FUNNAME - define alternate macro function names; no-op (binary macros removed).
C ---------------------------------------------------------------------------
        SUBROUTINE FUNNAME
        IMPLICIT NONE
        RETURN
        END SUBROUTINE FUNNAME
C
C ---------------------------------------------------------------------------
C MMOD - MOD command: compute W1 modulo W2, result in accumulator REG(9).
C ---------------------------------------------------------------------------
        SUBROUTINE MMOD
        IMPLICIT NONE
        INCLUDE 'DATMAI.INC'
        IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1
     1  .OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)'"MOD" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
        WRITE(OUTLYNE,*)
     1  '"MOD" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
          IF(W2.EQ.0.0D0) THEN
          REG(40)=REG(9)
          REG(9)=0.0D0
                   ELSE
          REG(40)=REG(9)
          REG(9)=DMOD(W1,W2)
                   END IF
                        RETURN
        END SUBROUTINE MMOD
C
C ---------------------------------------------------------------------------
C NO_ZEROS - strip redundant zeros from numeric string output.
C ---------------------------------------------------------------------------
      SUBROUTINE NO_ZEROS(STRING,N)
      INTEGER N,I,J
      CHARACTER STRING*(*)
      INCLUDE 'DATMAI.INC'
 20                     CONTINUE
                        J=1
                        DO I=1,N
      IF(N.EQ.79.AND.J.GE.77) GO TO 30
      IF(N.EQ.139.AND.J.GE.137) GO TO 30
      IF(STRING(J:J+2).EQ.'00D'.OR.STRING(J:J+2).EQ.'00E'.OR.
     1STRING(J:J+2).EQ.'00,'.OR.STRING(J:J+2).EQ.'00 ') THEN
      STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
      IF(STRING(J:J+1).EQ.' 0')
     1STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
                        GO TO 20
                        ELSE
                        J=J+1
      IF(N.EQ.79.AND.J.GE.77) GO TO 30
      IF(N.EQ.139.AND.J.GE.137) GO TO 30
                        END IF
                        END DO
 30                     CONTINUE
                       RETURN
        END SUBROUTINE NO_ZEROS
C
C ---------------------------------------------------------------------------
C ONE_BLANK - reduce multiple consecutive blanks to one.
C ---------------------------------------------------------------------------
      SUBROUTINE ONE_BLANK(STRING,N)
      INTEGER N,I,J
      CHARACTER STRING*(*)
                        DO I=1,N
      IF(STRING(1:1).EQ.' ')
     1STRING(1:N)=STRING(2:140)//' '
                        END DO
      IF(STRING(1:1).EQ.' ') RETURN
                         J=2
                        DO I=1,N
      IF(STRING(J:J+1).EQ.'  ') THEN
      STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
                        ELSE
                        J=J+1
                        END IF
                        END DO
                       RETURN
        END SUBROUTINE ONE_BLANK
C
