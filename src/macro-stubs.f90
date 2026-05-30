! macro-stubs.FOR - Replaces MACRO1-4.FOR
!
! Contains the subroutines from the old binary macro system that are still
! called by non-macro code.  The binary macro execution engine (MACRUN,
! MACIN, MACEXC, MACCOM, etc.) has been removed; only the following survive:
!
!   MACFAL          - error handler (minimal: binary-macro path is dead)
!   LTH80/LTH140    - string-length utilities used by CONFIG3
!   SORT_JK1        - heapsort used by PLOTCAD4 and WAVSPOT7
!   LENSSAVE_NOOPT  - LSAVE command
!   LENSSAVE        - LENSSAVE command
!   LENSREST        - LENSREST command
!
! ---------------------------------------------------------------------------
! MACFAL - called on input errors to abort in-progress macro/optimization.
! Without the binary macro system MACFAILURE is always .FALSE., so only the
! four flag assignments at the top run; the rest is dead code.
! ---------------------------------------------------------------------------
SUBROUTINE MACFAL
   USE GLOBALS
   use DATSUB
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   LOGICAL ITERROR
!
   FOBRUN=.FALSE.
   ITERROR=.TRUE.
   GUIERROR=.TRUE.
   ALLSTOP=.TRUE.
!     With the binary macro system removed, MACFAILURE is always .FALSE.
!     so we return immediately without resetting any F-flags.
   IF(.NOT.MACFAILURE) RETURN
!
!     Guard: if somehow MACFAILURE is true (shouldn't happen), reset to
!     a safe CMD-level state.
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
!
! ---------------------------------------------------------------------------
! LTH80 - last non-blank position in an 80-character string
! ---------------------------------------------------------------------------
SUBROUTINE LTH80(IPT80,LENG)
   use iso_fortran_env, only: real64
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
10 CONTINUE
   RETURN
END SUBROUTINE LTH80
!
! ---------------------------------------------------------------------------
! LTH140 - last non-blank position in a 140-character string
! ---------------------------------------------------------------------------
SUBROUTINE LTH140(IPT140,LENG)
   use iso_fortran_env, only: real64
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
10 CONTINUE
   RETURN
END SUBROUTINE LTH140
!
! ---------------------------------------------------------------------------
! SORT_JK1 - heapsort: sorts RA ascending; RA1 and RA2 follow the permutation.
! Used for field sorting in PLOTCAD4 and WAVSPOT7.
! ---------------------------------------------------------------------------
SUBROUTINE SORT_JK1(N,RA,RA1,RA2)
!     USED IN FIELD SORTING OR MULTIPLE FIELDS OF VIEW
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   real(real64) RA,RRA,RRA1,RRA2,RA1,RA2
   INTEGER N,L,IR,J,I
   DIMENSION RA(N),RA1(N),RA2(N)
   L=N/2+1
   IR=N
10 CONTINUE
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
20 IF(J.LE.IR) THEN
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
!
! ---------------------------------------------------------------------------
! LENSSAVE_NOOPT - LSAVE command: save lens without optimization data
! ---------------------------------------------------------------------------
SUBROUTINE LENSSAVE_NOOPT
   USE GLOBALS
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER LFILENAME*80
   INTEGER WSCNT
!
   IF(is_command_query()) THEN
      OUTLYNE=&
      &'"LSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
      CALL SHOWIT(1)
      OUTLYNE=&
      &'WITHOUT SAVING OPTIMIZATION OR RAY DATA'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SN.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE=&
      &'"LSAVE" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SST.EQ.0) WS='LENS'
   WSCNT=LEN(TRIM(WS))
   WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
   CALL SHOWIT(1)
   LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
   WSCNT=LEN(TRIM(LFILENAME))
!
!     DELETE CURRENT FILE
   OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'&
   &,FORM='FORMATTED',FILE=TRIM(DIRLEN)//TRIM(LFILENAME)&
   &,STATUS='UNKNOWN')
   CALL CLOSE_FILE(97,0)
!
!     SAVE I/O
   SAVE_KDP(1)=SAVEINPT(1)
   INPUT='OUT FILE '//TRIM(DIRLEN)//TRIM(LFILENAME)
   CALL PROCES
   INPUT='LENO NOOPT'
   CALL PROCES
   INPUT='IN TP'
   CALL PROCES
   INPUT='OUT TP'
   CALL PROCES
!     RESTORE I/O
   REST_KDP(1)=RESTINPT(1)
   OUTLYNE=&
   &'CURRENT LENS SAVED AS:'//TRIM(DIRLEN)//TRIM(LFILENAME)
   CALL SHOWIT(1)
   RETURN
END SUBROUTINE LENSSAVE_NOOPT
!
! ---------------------------------------------------------------------------
! LENSSAVE - LENSSAVE command: save lens with all data
! ---------------------------------------------------------------------------
SUBROUTINE LENSSAVE
   USE GLOBALS
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER LFILENAME*80
   INTEGER WSCNT
!
   IF(is_command_query()) THEN
      OUTLYNE=&
      &'"LENSSAVE" SAVES THE CURRENT LENS IN AN ASCII FILE'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SN.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE=&
      &'"LENSSAVE" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(SST.EQ.0) WS='LENS'
   WSCNT=LEN(TRIM(WS))
   WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
   CALL SHOWIT(1)
   LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
   WSCNT=LEN(TRIM(LFILENAME))
!
!     DELETE CURRENT FILE
   OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'&
   &,FORM='FORMATTED',FILE=TRIM(DIRLEN)//TRIM(LFILENAME)&
   &,STATUS='UNKNOWN')
   CALL CLOSE_FILE(97,0)
!
!     SAVE I/O AND WRITE LENS
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
!     RESTORE I/O
   REST_KDP(1)=RESTINPT(1)
   OUTLYNE=&
   &'CURRENT LENS SAVED AS:'//TRIM(DIRLEN)//TRIM(LFILENAME)
   CALL SHOWIT(1)
   RETURN
END SUBROUTINE LENSSAVE
!
! ---------------------------------------------------------------------------
! LENSREST - LENSREST command: restore lens from file
! ---------------------------------------------------------------------------
SUBROUTINE LENSREST
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   CHARACTER LFILENAME*80
   INTEGER WSCNT
   LOGICAL EXISJK
!
   IF(is_command_query()) THEN
      OUTLYNE=&
      &'"LENSREST" RESTORES THE CURRENT LENS FROM AN ASCII FILE'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SN.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE=&
      &'"LENSREST" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(SST.EQ.0) THEN
      OUTLYNE=&
      &'"LENSREST" REQUIRES A FILE NAME'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   WSCNT=LEN(TRIM(WS))
   WRITE(OUTLYNE,*) WSCNT,TRIM(WS)
   CALL SHOWIT(1)
   LFILENAME(1:LEN(TRIM(WS))+4)=TRIM(WS)//'.PRG'
   WSCNT=LEN(TRIM(LFILENAME))
!
!     DOES THE FILE EXIST?
   EXISJK=.FALSE.
   INQUIRE(FILE=TRIM(DIRLEN)//TRIM(LFILENAME),EXIST=EXISJK)
   IF(.NOT.EXISJK) THEN
      OUTLYNE=&
      &'LENS FILE NAMED '//TRIM(LFILENAME)//' DOES NOT EXIST'
      CALL SHOWIT(1)
      OUTLYNE='NO LENS RESTORATION WAS PERFORMED'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
!     SAVE I/O AND RESTORE LENS
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
!     RESTORE I/O
   REST_KDP(1)=RESTINPT(1)
   OUTLYNE=&
   &'LENS SAVED AS: '//TRIM(LFILENAME)//' HAS BEEN RESTORED'
   CALL SHOWIT(1)
   RETURN
END SUBROUTINE LENSREST
!
! ---------------------------------------------------------------------------
! FUNNAME - define alternate macro function names; no-op (binary macros removed).
! ---------------------------------------------------------------------------
SUBROUTINE FUNNAME
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   RETURN
END SUBROUTINE FUNNAME
!
! ---------------------------------------------------------------------------
! MMOD - MOD command: compute W1 modulo W2, result in accumulator REG(9).
! ---------------------------------------------------------------------------
SUBROUTINE MMOD
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
   IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
   &.OR.S5.EQ.1) THEN
      WRITE(OUTLYNE,*)'"MOD" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
   IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"MOD" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
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
!
! ---------------------------------------------------------------------------
! NO_ZEROS - strip redundant zeros from numeric string output.
! ---------------------------------------------------------------------------
SUBROUTINE NO_ZEROS(STRING,N)
   use DATMAI
   INTEGER N,I,J
   CHARACTER STRING*(*)
20 CONTINUE
   J=1
   DO I=1,N
      IF(N.EQ.79.AND.J.GE.77) GO TO 30
      IF(N.EQ.139.AND.J.GE.137) GO TO 30
      IF(STRING(J:J+2).EQ.'00D'.OR.STRING(J:J+2).EQ.'00E'.OR.&
      &STRING(J:J+2).EQ.'00,'.OR.STRING(J:J+2).EQ.'00 ') THEN
         STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
         IF(STRING(J:J+1).EQ.' 0')&
         &STRING(1:N)=STRING(1:J-1)//STRING(J+1:N)//' '
         GO TO 20
      ELSE
         J=J+1
         IF(N.EQ.79.AND.J.GE.77) GO TO 30
         IF(N.EQ.139.AND.J.GE.137) GO TO 30
      END IF
   END DO
30 CONTINUE
   RETURN
END SUBROUTINE NO_ZEROS
!
! ---------------------------------------------------------------------------
! ONE_BLANK - reduce multiple consecutive blanks to one.
! ---------------------------------------------------------------------------
SUBROUTINE ONE_BLANK(STRING,N)
   INTEGER N,I,J
   CHARACTER STRING*(*)
   DO I=1,N
      IF(STRING(1:1).EQ.' ')&
      &STRING(1:N)=STRING(2:140)//' '
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
!
