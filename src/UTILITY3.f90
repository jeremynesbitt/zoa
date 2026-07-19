!       THIRD SET OF UTILTIY ROUTINES GO HERE

! SUB FORMER.FOR
SUBROUTINE FORMER
!
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!
   INTEGER I,LL
!
   IF(is_command_query()) THEN
      LL=0
      DO I=80,1,-1
         IF(WFORM(I:I).NE.' ') THEN
            LL=I
            GO TO 986
         END IF
      END DO
986   CONTINUE
      WRITE(OUTLYNE,*) 'THE CURRENT WRITE FORMAT IS : ',WFORM(2:LL-1)
      CALL SHOWIT(0)
      RETURN
   END IF
   IF(SQ.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(WC//' ONLY TAKES STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SST.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(WC//' REQUIRES EXPLICIT STRING INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   DO I=1,80
      WFORM(I:I)=' '
   END DO
   LL=0
   DO I=80,1,-1
      IF(WS(I:I).NE.' ') THEN
         LL=I
         GO TO 987
      END IF
   END DO
987 CONTINUE
   WFORM(1:LL+2)='('//WS(1:LL)//')'
   RETURN
END
! SUB REWIND.FOR
SUBROUTINE REWIND
!
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS SUBROUTINE IS CALLED TO REWIND THE CARDTEXT.DAT FILE
!       UNIT = 8.
!
!       THE ONLY VALID QUALIFIER WORD IS:
!               CP = CARDTEXT.DATA = (UNIT=8)
!
   LOGICAL OPEN8,EXIST8,OPEN67,EXIST67
!
!
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL('"REWIND" ONLY TAKES QUALIFIER INPUT'//'\n'//'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SQ.EQ.0) WQ='FOE.DAT'
   IF(WQ.NE.'CP'.AND.WQ.NE.'FOE') THEN
      CALL REPORT_ERROR_AND_FAIL('ONLY CP (CARDTEXT DATA) AND FOE.DAT CAN BE REWOUND.', 1)
      RETURN
   END IF
   IF(WQ.EQ.'CP') THEN
!       WQ MUST BE 'CP'
      EXIST8=.FALSE.
      OPEN8=.FALSE.
      INQUIRE(FILE='CARDTEXT.DAT',EXIST=EXIST8)
      INQUIRE(FILE='CARDTEXT.DAT',OPENED=OPEN8)
      IF(EXIST8.OR.OPEN8) THEN
         REWIND(UNIT=8)
      ELSE
         IF(APPEND) OPEN(UNIT=8,ACCESS='APPEND',BLANK='NULL'&
         &,FORM='FORMATTED',FILE='CARDTEXT.DAT'&
         &,STATUS='UNKNOWN')
         IF(.NOT.APPEND) OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'&
         &,FORM='FORMATTED',FILE='CARDTEXT.DAT'&
         &,STATUS='UNKNOWN')
      END IF
   END IF
   IF(WQ.EQ.'FOE') THEN
!       WQ MUST BE 'FOE'
      EXIST8=.FALSE.
      OPEN8=.FALSE.
      INQUIRE(FILE='FOE.DAT',EXIST=EXIST67)
      INQUIRE(FILE='FOE.DAT',OPENED=OPEN67)
      IF(EXIST8.OR.OPEN67) THEN
         REWIND(UNIT=67)
      ELSE
         IF(APPEND) OPEN(UNIT=67,ACCESS='APPEND',BLANK='NULL'&
         &,FORM='FORMATTED',FILE='FOE.DAT'&
         &,STATUS='UNKNOWN')
         IF(.NOT.APPEND) OPEN(UNIT=67,ACCESS='SEQUENTIAL',BLANK='NULL'&
         &,FORM='FORMATTED',FILE='FOE.DAT'&
         &,STATUS='UNKNOWN')
      END IF
   END IF
   RETURN
END
! SUB ALLDEF.FOR
SUBROUTINE ALLDEF
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!     THIS DOES THE "ALL" COMMAND
!
!
!       CHECK FOR STRING INPUT
   IF(is_command_query()) THEN
      IF(ALLSET) OUTLYNE='"ALL" IS SET TO "ON"'
      IF(.NOT.ALLSET) OUTLYNE='"ALL" IS SET TO "OFF"'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"ALL" TAKES NO NUMERIC OR STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE ONLY VALID QUALIFIERS ARE "ON" AND "OFF"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'ON') ALLSET=.TRUE.
   IF(WQ.EQ.'OFF') ALLSET=.FALSE.
   RETURN
END
! SUB SETCOAT.FOR
SUBROUTINE SETCOAT
!
   use DATLEN
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!     THIS DOES THE "COATINGS" COMMAND
!
!
!       CHECK FOR STRING INPUT
   IF(is_command_query()) THEN
      IF(COATSET) OUTLYNE='"COATINGS" IS SET TO "ON"'
      IF(.NOT.COATSET) OUTLYNE='"COATINGS" IS SET TO "OFF"'
      CALL SHOWIT(1)
      RETURN
   END IF
   IF(SST.EQ.1.OR.SN.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"COATINGS" TAKES NO NUMERIC OR STRING INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & 'THE ONLY VALID QUALIFIERS ARE "ON" AND "OFF"'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(WQ.EQ.'ON') COATSET=.TRUE.
   IF(WQ.EQ.'OFF') COATSET=.FALSE.
   RETURN
END
! SUB WRITE.FOR
SUBROUTINE WRITE
!
   use DATMAI
   use command_utils, only: is_command_query
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
!       THIS SUBROUTINE IS CALLED TO WRITE OUT THE CONTENTS
!       OF A NAMED STORAGE REGISTER OR THE ACCUMULATOR.
!       OUTPUT CAN BE LABELD OR NOT LABELD. THE LABEL IS
!       TYPED IN AS AN ALPHANUMERIC STRING.
!
   CHARACTER ACCWRD*8,CSTRING*23
!
   real(real64) RGVAL
!
   INTEGER ACCSUB,ACCCNT,N,i
!
   COMMON/ACCSB/ACCWRD
!
   COMMON/ACCSB2/ACCSUB,ACCCNT
!
!
!               TEST TO SEE IF LABEL IS BLANK. IF YES, PRINT
!               REGISTER NAME FOLLOWED BY EQUAL SIGN IF
!               OUTPUT DEVICE IS THE SCREEN (OUT=6).
!               IF OUT NOT EQUAL TO 6 THEN JUST PRINT
!               THE REGISTER VALUE. IF LABEL IS NOT BLANK
!               PRINT FIRST 40 CHARACTERS IN THE
!               STRING FOLLOWED BY AN EQUAL SIGN.
!
   IF(is_command_query()) THEN
      OUTLYNE= 'NO ADDITIONAL INFORMATION AVAILABLE'
      CALL SHOWIT(1)
      RETURN
   END IF
!
   IF(WQ.NE.'A'.AND.WQ.NE.'B'.AND.WQ.NE.'C'&
   &.AND.WQ.NE.'D'.AND.WQ.NE.'E'.AND.WQ.NE.'F'&
   &.AND.WQ.NE.'G'.AND.WQ.NE.'H'.AND.WQ.NE.'ACC'&
   &.AND.WQ.NE.'        '.AND.WQ.NE.'ALL'.AND.&
   &WQ.NE.'X'.AND.WQ.NE.'Y'.AND.WQ.NE.'Z'.AND.&
   &WQ.NE.'T'.AND.WQ.NE.'IX'.AND.WQ.NE.'IY'&
   &.AND.WQ.NE.'IZ'.AND.WQ.NE.'IT'.AND.WQ.NE.&
   &'I'.AND.WQ.NE.'ITEST'.AND.WQ.NE.'J'.AND.&
   &WQ.NE.'JTEST'.AND.WQ.NE.'K'.AND.WQ.NE.'L'.AND.WQ.NE.'M'&
   &.AND.WQ.NE.'N'.AND.WQ.NE.'KTEST'.AND.WQ.NE.'LTEST'.AND.WQ.NE.&
   &'MTEST'.AND.WQ.NE.'NTEST') THEN
      CALL REPORT_ERROR_AND_FAIL('INVALID REGISTER NAME'//'\n'//'RE-ENTER COMMAND', 1)
      GO TO 20
   END IF
   IF(ACCSUB.EQ.1) THEN
      IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X'.OR.WQ.EQ.' ') THEN
         WQ=ACCWRD
         ACCCNT=ACCCNT-1
         IF(ACCCNT.EQ.0) ACCSUB=0
      END IF
   END IF
10 IF(SST.EQ.0) THEN
      IF(WQ.EQ.'A') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(1)
            WS='A ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(1)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'B') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(2)
            WS='B ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(2)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'C') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(3)
            WS='C ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(3)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'D') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(4)
            WS='D ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(4)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'E') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(5)
            WS='E ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(5)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'F') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(6)
            WS='F ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(6)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'G') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(7)
            WS='G ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(7)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'H') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(8)
            WS='H ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(8)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'        '.OR.WQ.EQ.'ACC'&
      &.OR.WQ.EQ.'X') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(9)
            WS='X ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(9)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'Y') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(10)
            WS='Y ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(10)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'Z') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(11)
            WS='Z ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(11)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'T') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(12)
            WS='T ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(12)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'IX') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(13)
            WS='IX ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(13)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'IY') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(14)
            WS='IY ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(14)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'IZ') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(15)
            WS='IZ ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(15)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'IT') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(16)
            WS='IT ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(16)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'I') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(17)
            WS='I ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(17)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'ITEST') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(18)
            WS='ITEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(18)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'J') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(19)
            WS='J ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(19)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'JTEST') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(20)
            WS='JTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(20)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'LASTX') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(40)
            WS='LASTX ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(40)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'LASTIX') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(30)
            WS='LASTIX ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(30)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'K') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(21)
            WS='K ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(21)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'L') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(22)
            WS='L ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(22)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'M') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(23)
            WS='M ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(23)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'N') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(24)
            WS='N ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(24)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'KTEST') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(25)
            WS='KTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(25)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'LTEST') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(26)
            WS='LTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(26)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'MTEST') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(27)
            WS='MTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(27)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'NTEST') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(28)
            WS='NTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            WRITE(OUTLYNE,1001) REG(28)
            CALL SHOWIT(0)
         END IF
      END IF
      IF(WQ.EQ.'ALL'.OR.WC.EQ.'PRIREG') THEN
         IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(1)
            WS='A ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(2)
            WS='B ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(3)
            WS='C ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(4)
            WS='D ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(5)
            WS='E ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(6)
            WS='F ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(7)
            WS='G ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(8)
            WS='H ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(9)
            WS='X ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(10)
            WS='Y ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(11)
            WS='Z ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(12)
            WS='T ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(13)
            WS='IX ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(14)
            WS='IY ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(15)
            WS='IZ ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(16)
            WS='IT ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(17)
            WS='I ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(18)
            WS='ITEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(19)
            WS='J ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(20)
            WS='JTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(21)
            WS='K ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(25)
            WS='KTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(22)
            WS='L ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(26)
            WS='LTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(23)
            WS='M ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(27)
            WS='MTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(24)
            WS='N ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(28)
            WS='NTEST ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(40)
            WS='LASTX ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
            WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(30)
            WS='LASTIX ='//' '//CSTRING(1:23)
            WRITE(OUTLYNE,1100) WS
            CALL SHOWIT(0)
         ELSE
            CALL REPORT_ERROR_AND_FAIL(' WRITE ALL SUPPORTED FOR SCREEN AND PRINTER ONLY', 1)
         END IF
      END IF
!
   ELSE
!
!               THE LABEL IS NOT BLANK
!               IF THE LABEL IS NOT BLANK BUT OUT IS NOT
!               EQUAL TO 6 OR 7, THEN SET THE LABEL TO BLANK
!               AND GO TO 10 AND REPROCESS.
      IF(OUT.NE.6.AND.OUT.NE.7) THEN
         SST=0
         GO TO 10
      END IF
!
      IF(WS(1:1).EQ.':') WS(1:80)=WS(2:80)
      DO I=40,1,-1
         IF(WS(I:I).NE.' ') THEN
            N=I
            GO TO 987
         END IF
      END DO
987   WS(1:(N+2))=WS(1:N)//' ='
      N=N+2
      IF(WQ.EQ.'A') RGVAL=REG(1)
      IF(WQ.EQ.'B') RGVAL=REG(2)
      IF(WQ.EQ.'C') RGVAL=REG(3)
      IF(WQ.EQ.'D') RGVAL=REG(4)
      IF(WQ.EQ.'E') RGVAL=REG(5)
      IF(WQ.EQ.'F') RGVAL=REG(6)
      IF(WQ.EQ.'G') RGVAL=REG(7)
      IF(WQ.EQ.'H') RGVAL=REG(8)
      IF(WQ.EQ.'        '.OR.WQ.EQ.'ACC'.OR.WQ.EQ.'X')&
      &RGVAL=REG(9)
      IF(WQ.EQ.'Y') RGVAL=REG(10)
      IF(WQ.EQ.'Z') RGVAL=REG(11)
      IF(WQ.EQ.'T') RGVAL=REG(12)
      IF(WQ.EQ.'IX') RGVAL=REG(13)
      IF(WQ.EQ.'IY') RGVAL=REG(14)
      IF(WQ.EQ.'IZ') RGVAL=REG(15)
      IF(WQ.EQ.'IT') RGVAL=REG(16)
      IF(WQ.EQ.'I') RGVAL=REG(17)
      IF(WQ.EQ.'ITEST') RGVAL=REG(18)
      IF(WQ.EQ.'J') RGVAL=REG(19)
      IF(WQ.EQ.'JTEST') RGVAL=REG(20)
      IF(WQ.EQ.'K') RGVAL=REG(21)
      IF(WQ.EQ.'L') RGVAL=REG(22)
      IF(WQ.EQ.'M') RGVAL=REG(23)
      IF(WQ.EQ.'N') RGVAL=REG(24)
      IF(WQ.EQ.'KTEST') RGVAL=REG(25)
      IF(WQ.EQ.'LTEST') RGVAL=REG(26)
      IF(WQ.EQ.'MTEST') RGVAL=REG(27)
      IF(WQ.EQ.'NTEST') RGVAL=REG(28)
      IF(WQ.EQ.'LASTX') RGVAL=REG(40)
      IF(WQ.EQ.'LASTIX') RGVAL=REG(30)
      IF(WQ.EQ.'ALL'.OR.WC.EQ.'PRIREG')THEN
         CALL REPORT_ERROR_AND_FAIL('WRITE ALL NOT FUNCTIONAL WITH LABEL STRING', 1)
      END IF
      WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) RGVAL
      WS=WS(1:N)//' '//CSTRING(1:23)
      WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
   END IF
20 CONTINUE
!
!       THE FOLLOWING ARE THE WRITE FORMAT STATEMENTS
!
1001 FORMAT(D23.15)
1100 FORMAT(A79)
   RETURN
69 CONTINUE
   OUTLYNE=&
   &'INVALID FORMAT SPECIFICATION EXISTS'
   CALL SHOWIT(1)
   OUTLYNE=&
   &'RE-ISSUE THE "FORMAT" COMMAND'
   CALL SHOWIT(1)
   CALL MACFAL
   RETURN
END
