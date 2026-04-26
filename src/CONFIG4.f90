!       FOURTH FILE OF CONFIGS FILES

! SUB CFDFLT.FOR
SUBROUTINE CFDFLT
!
   use DATCFG
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf, sys_high_cfg
   IMPLICIT NONE
!
!       THIS SUBROUTINE IS CALLED BY CFGOUT AND
!       CAUSES THE CURRENT PROGRAM INSTRUCTION TO CHECK
!       FOR CORRECT PROGRAMATICAL SYNTAX AND TO CORRECT IT
!
   LOGICAL CEE
!
   INTEGER RETRET,TAG,II,RET,LEOS,SPEOS,I
!
   COMMON/RETIT/RET,RETRET,LEOS,SPEOS
!
!
   IF(WQ.EQ.'CENT'.OR.WQ.EQ.'DELT') THEN
      WRITE(OUTLYNE,*)&
      &'QUALIFIERS "CENT" AND "DELT" ARE NOT OPERATIONAL IN'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)&
      &'ALTERNATE LENS CONFIGURATIONS'
      CALL SHOWIT(1)
      CALL MACFAL
      RETURN
   END IF
!
   IF(WC.EQ.'ZERO') THEN
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"ZERO" TAKES NO INPUT'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'YTORIC') THEN
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"YTORIC" TAKES NO INPUT'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'XTORIC') THEN
      IF(SQ.EQ.1.OR.SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"XTORIC" TAKES NO INPUT'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
!     MULTCLAP STUFF
   IF(WC.EQ.'MULTCLAP') THEN
      IF(SST.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"MULTCLAP" ONLY TAKES QUALIFIER'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'"OR NUMERIC WORD #1 THROUGH #4 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
         OUTLYNE=&
         &'"MULTCLAP" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SN.EQ.1.AND.SQ.EQ.1) THEN
         OUTLYNE=&
         &'"MULTCLAP" REQUIRES EXPLICIT NUMERIC OR QUALIFIER INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'BUT NOT BOTH'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'DELETE') THEN
         OUTLYNE=&
         &'"MULTCLAP" ONLY TAKES "DELETE" AS A QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'BUT NOT BOTH'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(INT(W1).LE.0.OR.INT(W1).GT.1000) THEN
         WRITE(OUTLYNE,*)&
         &'"MULTCLAP" REQUIRES NUMERIC WORD #1 TO BE FROM 1 TO 1000'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
!     MULTCOBS STUFF
   IF(WC.EQ.'MULTCOBS') THEN
      IF(SST.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"MULTCOBS" ONLY TAKES QUALIFIER'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'"OR NUMERIC WORD #1 THROUGH #4 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
         OUTLYNE=&
         &'"MULTCOBS" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SN.EQ.1.AND.SQ.EQ.1) THEN
         OUTLYNE=&
         &'"MULTCOBS" REQUIRES EXPLICIT NUMERIC OR QUALIFIER INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'BUT NOT BOTH'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'DELETE') THEN
         OUTLYNE=&
         &'"MULTCOBS" ONLY TAKES "DELETE" AS A QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'BUT NOT BOTH'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(INT(W1).LE.0.OR.INT(W1).GT.1000) THEN
         WRITE(OUTLYNE,*)&
         &'"MULTCOBS" REQUIRES NUMERIC WORD #1 TO BE FROM 1 TO 1000'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
!     SPIDER STUFF
   IF(WC.EQ.'SPIDER') THEN
      IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"SPIDER" ONLY TAKES QUALIFIER'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'"OR NUMERIC WORD #1 THROUGH #3 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
         OUTLYNE=&
         &'"SPIDER" REQUIRES EXPLICIT NUMERIC WORD #1 THROUGH #3 INPUT'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SN.EQ.1.AND.SQ.EQ.1) THEN
         OUTLYNE=&
         &'"SPIDER" REQUIRES EXPLICIT NUMERIC OR QUALIFIER INPUT'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'BUT NOT BOTH'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'DELETE') THEN
         OUTLYNE=&
         &'"SPIDER" ONLY TAKES "DELETE" AS A QUALIFIER WORD'
         CALL SHOWIT(1)
         OUTLYNE=&
         &'BUT NOT BOTH'
         CALL SHOWIT(1)
         CALL REPORT_ERROR_AND_FAIL('RE-ENTER COMMAND', 1)
         RETURN
      END IF
      IF(INT(W1).LT.1) THEN
         WRITE(OUTLYNE,*)&
         &'"SPIDER" REQUIRES NUMERIC WORD #1 TO BE GREATER THAN OR'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'EQUAL TO 1'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
!     NSS STUFF
   IF(WC.EQ.'ROO') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.&
      &S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"ROO" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0.OR.W1.LE.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"ROO" REQUIRES EXPLICIT, POSITIVE NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'CCR') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"CCR" TAKES NO QUALIFIER OR STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"CCR" TAKES NO NUMERIC WORD #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0.OR.W1.LE.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"CCR" REQUIRES EXPLICIT, POSITIVE NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
!
!     UPDATE LENS DELETETION COMMANDS
   IF(WC.EQ.'CSD'.OR.WC.EQ.'CSDX'.OR.WC.EQ.'CSDY'.OR.WC.EQ.'TSD'&
   &.OR.WC.EQ.'ASPHD'.OR.WC.EQ.'TASPHD'.OR.WC.EQ.'TILTD'.OR.&
   &WC.EQ.'CLAPD'.OR.WC.EQ.'COBSD'.OR.WC.EQ.'TORD'&
   &.OR.WC.EQ.'DELDEFOR'.OR.WC.EQ.'ARRAYD') THEN
      DO II=8,1,-1
         IF(WC(II:II).NE.' ') THEN
            TAG=II
         END IF
      END DO
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"',WC(1:TAG),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'TAKES NO STRING OR QUALIFIER'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'OR NUMERIC WORDS #3 THROUGH #5'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)'"',WC(1:TAG),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'USES EITHER TWO OR ZERO NUMERIC WORDS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W1).LT.0) THEN
         WRITE(OUTLYNE,*)&
         &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W2).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)&
         &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
         &INT(sys_last_surf())
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(W1.GT.W2) THEN
         WRITE(OUTLYNE,*)&
         &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'THE STARTING SURFACE #'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
!
   IF(WC.EQ.'SAY'.OR.WC.EQ.'SAX'.OR.WC.EQ.'WRY'.OR.WC.EQ.'WRX'&
   &.OR.WQ.EQ.'BDX'.OR.WC.EQ.'BDY') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         IF(SQ.EQ.0) THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:3),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT'.AND.&
      &WQ.NE.'FLOAT'.AND.WQ.NE.'NOFLOAT') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER USED WITH "',WC(1:3),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.AND.SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'COATING') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:7),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:7),'" TAKES NO QUALIFIER OR STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:7),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LT.0.0D0.OR.W1.GT.1000.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:7),'" REQUIRES NUMBERIC WORD #1 TO BE IN THE RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'0 TO 1000'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'AUTOFUNC') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:8),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:8),'" TAKES NO STRING OR QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:8),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W1).NE.0.AND.INT(W1).NE.1.AND.INT(W1).NE.2 &
      &.AND.INT(W1).NE.3.AND.INT(W1).NE.4.AND.INT(W1).NE.5 &
      &.AND.INT(W1).NE.6.AND.INT(W1).NE.7.AND.INT(W1).NE.8 &
      &.AND.INT(W1).NE.9.AND.INT(W1).NE.10) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:8),'" REQUIRES EXPLICIT INTEGER NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'BETWEEN 0 AND 10'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'NAOY'.OR.WC.EQ.'NAOX') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER USED WITH "',WC(1:4),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'FNOY'.OR.WC.EQ.'FNOX') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER USED WITH "',WC(1:4),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'SCY'.OR.WC.EQ.'SCX') THEN
      IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" ONLY TAKES QUALIFIER AND NUMERIC WORD #1'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=1.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W2=0.0
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'FANG    ') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND '
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'PXIM'.OR.WC.EQ.'PYIM'.OR.&
   &WC.EQ.'RXIM'.OR.WC.EQ.'RYIM') THEN
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" ONLY TAKES QUALIFIER AND NUMERIC WORD #1'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=1.0
      END IF
      IF(SQ.EQ.1.AND.WQ.NE.'FANG    ') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND '
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'SPTWT') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1)THEN
         WRITE(OUTLYNE,*)'"',WC(1:5),'" ONLY TAKES NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LT.0.0D0.OR.W2.LT.0.0D0.OR.W3.LT.0.0D0 &
      &.OR.W4.LT.0.0D0.OR.W5.LT.0.0D0) THEN
         WRITE(OUTLYNE,*)'"',WC(1:5),&
         &'" REQUIRES NON-NEGATIVE NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SN.NE.0) THEN
         IF(DF1.EQ.1)THEN
            W1=0.0
            DF1=0
         END IF
         IF(DF2.EQ.1) THEN
            W2=0.0
            DF2=0
         END IF
         IF(DF3.EQ.1) THEN
            W3=0.0
            DF3=0
         END IF
         IF(DF4.EQ.1) THEN
            W4=0.0
            DF4=0
         END IF
         IF(DF5.EQ.1) THEN
            W5=0.0
            DF5=0
         END IF
         RETURN
      END IF
   END IF
   IF(WC.EQ.'SPTWT2') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1)THEN
         WRITE(OUTLYNE,*)'"',WC(1:6),'" ONLY TAKES NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LT.0.0D0.OR.W2.LT.0.0D0.OR.W3.LT.0.0D0 &
      &.OR.W4.LT.0.0D0.OR.W5.LT.0.0D0) THEN
         WRITE(OUTLYNE,*)'"',WC(1:6),&
         &'" REQUIRES NON-NEGATIVE NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SN.NE.0) THEN
         IF(DF1.EQ.1)THEN
            W1=0.0
            DF1=0
         END IF
         IF(DF2.EQ.1) THEN
            W2=0.0
            DF2=0
         END IF
         IF(DF3.EQ.1) THEN
            W3=0.0
            DF3=0
         END IF
         IF(DF4.EQ.1) THEN
            W4=0.0
            DF4=0
         END IF
         IF(DF5.EQ.1) THEN
            W5=0.0
            DF5=0
         END IF
         RETURN
      END IF
   END IF
   IF(WC.EQ.'SPTWT'.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1 &
   &.AND.DF4.EQ.1.AND.DF5.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"SPTWT" REQUIRES SOME EXPLICIT NUMERIC INPUT IN THIS CONTEXT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETRET=1
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'SPTWT2'.AND.DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1 &
   &.AND.DF4.EQ.1.AND.DF5.EQ.1) THEN
      WRITE(OUTLYNE,*)&
      &'"SPTWT2" REQUIRES SOME EXPLICIT NUMERIC INPUT IN THIS CONTEXT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETRET=1
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'THERM') THEN
      IF(SST.EQ.1)THEN
         WRITE(OUTLYNE,*)'"THERM" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"THERM" TAKES NO NUMERIC WORD #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1)THEN
         W1=0.0
         DF1=0
      END IF
      IF(DF2.EQ.1) THEN
         W2=sys_last_surf()
         DF2=0
      END IF
      IF(INT(W1).LT.0.OR.INT(W1).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)&
         &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W2).LT.0.OR.INT(W2).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)&
         &'ENDING SURFACE NUMBER MUST BE LESS THAN ',INT(sys_last_surf())
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W2).LT.INT(W1)) THEN
         WRITE(OUTLYNE,*)&
         &'ERROR:'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'ENDING SURFACE LESS THAN STARTING SURFACE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF3.EQ.1) THEN
         WRITE(OUTLYNE,*)'"THERM" REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'GLASS'.OR.WQ.EQ.'SPACE'.OR.WQ.EQ.'THICK'.OR.WQ.EQ.&
      &'SHAPE'.OR.WQ.EQ.'GAS') THEN
         IF(DF4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"THERM ',WQ,'" REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)'"THERM" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'SHAPE'.AND.WQ.NE.'SPACE'.AND.WQ.NE.'THICK'&
      &.AND.WQ.NE.'GLASS'.AND.WQ.NE.'GAS'.AND.WQ.NE.'AIR'.AND.&
      &WQ.NE.'OXYGEN'.AND.WQ.NE.'HYDROGEN'.AND.WQ.NE.'HELIUM'&
      &.AND.WQ.NE.'ARGON'.AND.WQ.NE.'METHANE'.AND.WQ.NE.'ETHANE'&
      &.AND.WQ.NE.'NITROGEN') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "THERM"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'PRES') THEN
      IF(SST.EQ.1)THEN
         WRITE(OUTLYNE,*)'"PRES" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"PRES" TAKES NO NUMERIC WORD #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1)THEN
         W1=0.0
         DF1=0
      END IF
      IF(DF2.EQ.1) THEN
         W2=sys_last_surf()
         DF2=0
      END IF
      IF(INT(W1).LT.0.OR.INT(W1).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)&
         &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W2).LT.0.OR.INT(W2).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)&
         &'ENDING SURFACE NUMBER MUST BE LESS THAN ',INT(sys_last_surf())
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W2).LT.INT(W1)) THEN
         WRITE(OUTLYNE,*)&
         &'ERROR:'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'ENDING SURFACE LESS THAN STARTING SURFACE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF3.EQ.1) THEN
         WRITE(OUTLYNE,*)'"PRES" REQUIRES EXPLICIT NUMERIC WORD #3 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'GAS') THEN
         IF(DF4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PRES GAS" REQUIRES EXPLICIT NUMERIC WORD #4 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)'"PRES" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'GAS'.AND.WQ.NE.'AIR'.AND.&
      &WQ.NE.'OXYGEN'.AND.WQ.NE.'HYDROGEN'.AND.WQ.NE.'HELIUM'&
      &.AND.WQ.NE.'ARGON'.AND.WQ.NE.'METHANE'.AND.WQ.NE.'ETHANE'&
      &.AND.WQ.NE.'NITROGEN') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER WORD USED WITH "PRES"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'PCW') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"PCW" ONLY TAKES NUMERIC WORD #1 AND #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=2.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W1=3.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'SCW') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"SCW" ONLY TAKES NUMERIC WORD #1 AND #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=2.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W1=1.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'INR') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO STRING OR QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO NUMERIC WORD #2 TRHOUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.1.AND.W1.LE.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'GRO'.OR.WC.EQ.'GRS'.OR.WC.EQ.'GRX'.OR.WC.EQ.'GRY'&
   &.OR.WC.EQ.'GRZ') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO STRING OR QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO NUMERIC WORD #2 TRHOUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'GRT') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" TAKES NO QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'GRTD') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'INRD') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" TAKES NO ADDITIONAL INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'RD'.OR.WC.EQ.'CV') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'DELTFR') THEN
         IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),'" TAKES NO NUMERIC WORD #3 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      ELSE
!     WQ IS DELTFR
         IF(S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),'DELTFR" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
         IF(W2.LE.0.0D0.AND.DF2.EQ.0.OR.W3.LE.0.0D0 &
         &.AND.DF3.EQ.0) THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),&
            &'DELTFR" REQUIRES POSITIVE NUMERIC WORD #2 AN #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT'&
      &.AND.WQ.NE.'DELTFR') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER WORD USED WITH "',WC(1:2),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'DEFORM') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"DEFORM" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"DEFORM" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.5) THEN
         WRITE(OUTLYNE,*)&
         &'"DEFORM" REQUIRES TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
      END IF
      IF(SQ.EQ.0) THEN
         SQ=1
         WQ='F01'
      END IF
      IF(WQ.NE.'F01'.AND.WQ.NE.'F02'.AND.WQ.NE.'F03'&
      &.AND.WQ.NE.'F01'.AND.WQ.NE.'F02'.AND.WQ.NE.'F03'&
      &.AND.WQ.NE.'F04'.AND.WQ.NE.'F05'.AND.WQ.NE.'F06'&
      &.AND.WQ.NE.'F07'.AND.WQ.NE.'F08'.AND.WQ.NE.'F09'&
      &.AND.WQ.NE.'F10') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER WORD USED WITH "DEFORM"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
      END IF
      RETURN
   END IF
   IF(WC.EQ.'CHG') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"CHG" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)'"CHG" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(INT(W1).LT.0.OR.INT(W1).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'PUY'.OR.WC.EQ.'PUX') THEN
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" ONLY TAKES QUALIFIER OR NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
      END IF
   END IF
   IF(WC.EQ.'PUY'.OR.WC.EQ.'PUX') THEN
      IF(WQ.EQ.'FN'.AND.DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),' FN" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'PUY'.OR.WC.EQ.'PUX') THEN
      IF(WQ.EQ.'FN'.AND.W1.EQ.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),' FN" REQUIRES NON-ZERO NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'PUY'.OR.WC.EQ.'PUX') THEN
      IF(SQ.EQ.1.AND.WQ.NE.'FN') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER USED WITH "',WC(1:3),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
      END IF
   END IF
   IF(WC.EQ.'CC'.OR.WC.EQ.'AG'.OR.WC.EQ.'AH'.OR.WC.EQ.'AJ'&
   &.OR.WC.EQ.'CCTOR'.OR.WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.&
   &WC.EQ.'PICY'.OR.WC.EQ.'AK'.OR.WC.EQ.'AI'.OR.WC.EQ.&
   &'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'TH'&
   &.OR.WC.EQ.'PY'.OR.WC.EQ.'PCY'.OR.WC.EQ.'PX'.OR.WC.EQ.'PCX'&
   &.OR.WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR'.OR.WC.EQ.'AL'.OR.&
   &WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ALPHA'.OR.WC.EQ.'ADTOR'&
   &.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'&
   &.OR.WC.EQ.'BETA'.OR.WC.EQ.'GAMMA'.OR.WC.EQ.'AD'.OR.&
   &WC.EQ.'AE'.OR.WC.EQ.'AF'.OR.WC.EQ.'AC'.OR.WC.EQ.'ZD'&
   &.OR.WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'.OR.WC.EQ.'PIVZ'.OR.WC.EQ.'THM'&
   &.OR.WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ'.OR.WC.EQ.'GALPHA'&
   &.OR.WC.EQ.'GBETA'.OR.WC.EQ.'GGAMMA'.OR.WC.EQ.'SPGR'.OR.WQ.EQ.&
   &'PRICE'.OR.WC.EQ.'RAYERROR'.OR.WC.EQ.'CCR'.OR.WC.EQ.'ROO') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         IF(WC.EQ.'RAYERROR') THEN
            WRITE(OUTLYNE,*)&
            &'"RAYERROR" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(S4.EQ.1.OR.S5.EQ.1) THEN
         IF(WC.EQ.'ROO') THEN
            WRITE(OUTLYNE,*)&
            &'"ROO" TAKES NO NUMERIC WORD #4 AND #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
      END IF
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         IF(WC.EQ.'RAYERROR') THEN
            WRITE(OUTLYNE,*)&
            &'"RAYERROR" TAKES NO QUALIFIER OR STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'CCR') THEN
            WRITE(OUTLYNE,*)&
            &'"CCR" TAKES NO QUALIFIER OR STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'ROO') THEN
            WRITE(OUTLYNE,*)&
            &'"ROO" TAKES NO QUALIFIER OR STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
      END IF
!
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         IF(WC.EQ.'CC'.OR.WC.EQ.'AG'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'&
         &.OR.WC.EQ.'TH'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL'&
         &.OR.WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.&
         &WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ZD'&
         &.OR.WC.EQ.'AD'.OR.WC.EQ.'AC'.OR.&
         &WC.EQ.'AE'.OR.WC.EQ.'AF') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(&
         &WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.'THM'.OR.&
         &WC.EQ.'PRICE'.OR.WQ.EQ.&
         &'PCY'.OR.WC.EQ.'PCX'&
         &.OR.WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR')THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:5),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'PICY'.OR.WC.EQ.&
         &'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'PIVZ'&
         &.OR.WC.EQ.'BETA'.OR.WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'.OR.&
         &WC.EQ.'SPGR') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:4),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'CCTOR'&
         &.OR.WC.EQ.'ALPHA'.OR.WC.EQ.'ADTOR'.OR.WC.EQ.'GALPHA'&
         &.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'&
         &.OR.WC.EQ.'GAMMA'.OR.WC.EQ.'GBETA'.OR.WC.EQ.'GGAMMA') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:6),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:3),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         IF(WC.EQ.'CC'.OR.WC.EQ.'AG'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'&
         &.OR.WC.EQ.'TH'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'.OR.WC.EQ.'AL'&
         &.OR.WC.EQ.'PY'.OR.WC.EQ.'PX'.OR.&
         &WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ZD'&
         &.OR.WC.EQ.'AD'.OR.WC.EQ.'AC'.OR.&
         &WC.EQ.'AE'.OR.WC.EQ.'AF') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),'" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(&
         &WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.WC.EQ.'THM'.OR.&
         &WC.EQ.'PRICE'.OR.WQ.EQ.&
         &'PCY'.OR.WC.EQ.'PCX'&
         &.OR.WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR')THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:5),'" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'PICY'.OR.WC.EQ.&
         &'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'.OR.WC.EQ.'PIVZ'&
         &.OR.WC.EQ.'BETA'.OR.WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'.OR.&
         &WC.EQ.'SPGR') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:4),'" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'CCTOR'&
         &.OR.WC.EQ.'ALPHA'.OR.WC.EQ.'ADTOR'&
         &.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'&
         &.OR.WC.EQ.'GAMMA'.OR.WC.EQ.'GALPHA'.OR.WC.EQ.'GBETA'.OR.&
         &WC.EQ.'GGAMMA') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:6),'" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:3),'" TAKES NO STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'DELT'.AND.WQ.NE.'CENT') THEN
         IF(WC.EQ.'CC'.OR.WC.EQ.'AG'.OR.WC.EQ.'AH'.OR.WC.EQ.'AI'&
         &.OR.WC.EQ.'TH'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'.OR.WC.EQ.'AL'.OR.&
         &WC.EQ.'XD'.OR.WC.EQ.'YD'.OR.WC.EQ.'ZD'&
         &.OR.WC.EQ.'AD'.OR.WC.EQ.'AC'.OR.&
         &WC.EQ.'AE'.OR.WC.EQ.'AF') THEN
            WRITE(OUTLYNE,*)&
            &'INVALID QUALIFIER USED WITH "',WC(1:2),'"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'CVTOR'.OR.WC.EQ.'RDTOR'.OR.WC.EQ.'THM'.OR.&
         &WQ.EQ.'PRICE') THEN
            WRITE(OUTLYNE,*)&
            &'INVALID QUALIFIER USED WITH "',WC(1:5),'"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'BETA'.OR.WC.EQ.'PIVX'.OR.WC.EQ.'PIVY'.OR.WC.EQ.'PIVZ')&
         &THEN
            WRITE(OUTLYNE,*)&
            &'INVALID QUALIFIER USED WITH "',WC(1:4),'"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'CCTOR'&
         &.OR.WC.EQ.'ALPHA'.OR.WC.EQ.'ADTOR'&
         &.OR.WC.EQ.'AETOR'.OR.WC.EQ.'AFTOR'.OR.WC.EQ.'AGTOR'&
         &.OR.WC.EQ.'GAMMA'.OR.WC.EQ.'GALPHA'.OR.WC.EQ.'GBETA'&
         &.OR.WC.EQ.'GGAMMA') THEN
            WRITE(OUTLYNE,*)&
            &'INVALID QUALIFIER USED WITH "',WC(1:6),'"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'GDX'.OR.WC.EQ.'GDY'.OR.WC.EQ.'GDZ') THEN
            WRITE(OUTLYNE,*)&
            &'INVALID QUALIFIER USED WITH "',WC(1:3),'"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         IF(&
         &WC.EQ.'PY'.OR.WC.EQ.'PX') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),'" TAKES NO QUALIFIER INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(&
         &WC.EQ.'PIY'.OR.WC.EQ.'PIX'.OR.&
         &WC.EQ.&
         &'PCY'.OR.WC.EQ.'PCX'&
         &)THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:5),'" TAKES NO QUALIFIER INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'PICY'.OR.WC.EQ.&
         &'PICX'.OR.WC.EQ.'PUCY'.OR.WC.EQ.'PUCX'&
         &.OR.WC.EQ.'SPGR')THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:4),'" TAKES NO QUALIFIER INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'LI'.OR.WC.EQ.'LIC') THEN
      IF(SN.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"LI(C)" ONLY TAKES STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.0) THEN
         WS=AA//AA//AA//AA
      END IF
      RETURN
   END IF
   IF(WC.EQ.'INI') THEN
      IF(SN.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"INI" ONLY TAKES STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.0) THEN
         WS=AA//AA//AA//AA
      END IF
      RETURN
   END IF
   IF(WC.EQ.'LTYPE') THEN
      IF(SN.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"LTYPE" ONLY TAKES STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.0) THEN
         WS=AA//AA//AA//AA
      END IF
      RETURN
   END IF
   IF(WC.EQ.'LBL'.OR.WC.EQ.'PLBL') THEN
      IF(SN.EQ.1.OR.SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"LBL"/"LABEL" ONLY TAKES STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.0) THEN
         WS=AA//AA//AA//AA
      END IF
      RETURN
   END IF
   IF(WC.EQ.'CW') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"CW" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=1.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'UNITS') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"UNITS" ONLY TAKES QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0) THEN
         WQ='INCH'
      END IF
      IF(WQ.NE.'IN'.AND.WQ.NE.'INCH'.AND.WQ.NE.'INCHES'&
      &.AND.WQ.NE.'MM'.AND.WQ.NE.'MM'.AND.WQ.NE.'M') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'FOOTBLOK') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"FOOTBLOK" ONLY TAKES QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)'"FOOTBLOK" REQUIRES EXPLICIT QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF'.AND.WQ.NE.'YES'.AND.WQ.NE.'NO')THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'PIVAXIS') THEN
      IF(SN.EQ.1.OR.SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"PIVAXIS" ONLY TAKES QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)'"PIVAXIS" REQUIRES EXPLICIT QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'LOCAL'.AND.WQ.NE.'NORMAL')THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'N1'.OR.WC.EQ.'N2'.OR.WC.EQ.'N3'.OR.&
   &WC.EQ.'N4'.OR.WC.EQ.'N5'.OR.WC.EQ.'N6'.OR.WC.EQ.'N7'&
   &.OR.WC.EQ.'N8'.OR.WC.EQ.'N9'.OR.WC.EQ.'N10') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER USED WITH "',WC(1:2),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:2),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'INDEX'.OR.WC.EQ.'VNUM'.OR.WC.EQ.'DPART') THEN
      IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:5),'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:5),'" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'        '.AND.WQ.NE.'CENT'.AND.WQ.NE.'DELT') THEN
         WRITE(OUTLYNE,*)&
         &'INVALID QUALIFIER USED WITH "',WC(1:5),'"'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:5),'" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'ASPH'.OR.WC.EQ.'TASPH'.OR.WC.EQ.'ASPH2') THEN
      IF(SQ.EQ.1.OR.SST.EQ.1) THEN
         IF(WC.EQ.'ASPH') THEN
            WRITE(OUTLYNE,*)&
            &'"ASPH" TAKES NO QUALIFIER OR STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'ASPH2') THEN
            WRITE(OUTLYNE,*)&
            &'"ASPH2" TAKES NO QUALIFIER OR STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'TASPH') THEN
            WRITE(OUTLYNE,*)&
            &'"TASPH" TAKES NO QUALIFIER OR STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W2=0.0
      END IF
      IF(DF3.EQ.1) THEN
         DF3=0
         W3=0.0
      END IF
      IF(DF4.EQ.1) THEN
         DF4=0
         W4=0.0
      END IF
      IF(DF5.EQ.1) THEN
         DF5=0
         W5=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'ARRAY') THEN
      IF(SQ.EQ.1.AND.WQ.NE.'ODD'.AND.WQ.NE.'EVEN') THEN
         WRITE(OUTLYNE,*)&
         &'"ARRAY" ONLY TAKES "ODD" OR "EVEN" AS QUALIFIER NPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"ARRAY" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"ARRAY" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LE.0.0D0.OR.W2.LE.0.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"ARRAY" REQUIRES POSITIVE INTEGER NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"ARRAY" TAKES NO NUMERIC WORD #3, #4 OR #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'REFS'.OR.&
   &WC.EQ.'APY'.OR.WC.EQ.'PERFECT'.OR.WC.EQ.&
   &'APX'.OR.WC.EQ.'APCY'.OR.WC.EQ.'APCX'.OR.WC.EQ.'AIR'&
   &.OR.WC.EQ.'REFL'.OR.WC.EQ.'REFLTIR'.OR.WC.EQ.'GRT'.OR.&
   &WC.EQ.'REFLTIRO') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
!
         IF(WC.EQ.'APY'.OR.WC.EQ.&
         &'APX'.OR.WC.EQ.'AIR'.OR.WC.EQ.'GRT')THEN
            WRITE(OUTLYNE,*)'"',WC(1:3),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'PERFECT') THEN
            WRITE(OUTLYNE,*)'"',WC(1:7),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'REAL') THEN
            WRITE(OUTLYNE,*)'"',WC(1:4),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'PARAX') THEN
            WRITE(OUTLYNE,*)'"',WC(1:5),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
!


         IF(WC.EQ.'IDEAL') THEN
            IF(SST.EQ.1.OR.SQ.EQ.1) THEN
               WRITE(OUTLYNE,*)'"',WC(1:5),'" TAKES NO QUALIFIER,'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'OR STRING INPUT'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
               WRITE(OUTLYNE,*)'"',WC(1:5),'" ONLY TAKES NUMERIC'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'WORD #1 INPUT'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
            IF(W1.EQ.0.0D0) THEN
               WRITE(OUTLYNE,*)'"',WC(1:5),'" REQUIRES NON-ZERO INPUT'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
            END IF
         END IF
         IF(WC.EQ.'PERFECT') THEN
            WRITE(OUTLYNE,*)'"',WC(1:7),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
!
         IF(WC.EQ.'REFS'.OR.&
         &WC.EQ.'APCY'.OR.WC.EQ.'APCX'.OR.WC.EQ.'REFL') THEN
            WRITE(OUTLYNE,*)'"',WC(1:4),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'REFLTIRO') THEN
            WRITE(OUTLYNE,*)'"',WC(1:8),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'REFLTIR') THEN
            WRITE(OUTLYNE,*)'"',WC(1:7),'" TAKES NO EXPLICIT INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
!
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'MODE') THEN
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)'"MODE" ONLY TAKES QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'FOCAL'.OR.WQ.EQ.'AFOCAL'.OR.WQ.EQ.'UFOCAL'&
      &.OR.WQ.EQ.'UAFOCAL') THEN
         RETURN
      END IF
      WRITE(OUTLYNE,*)'"MODE" MUST BE "FOCAL", "AFOCAL", "UFOCAL" OR'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'"UAFOCAL". UNRECOGNIZED MODE WAS FOUND.'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
      RETRET=1
      CALL MACFAL
      RETURN
   END IF
   IF(WC.EQ.'MAGY'.OR.WC.EQ.'MAGX') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         IF(WC.EQ.'MAGY') THEN
            WRITE(OUTLYNE,*)&
            &'"MAGY" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'MAGX') THEN
            WRITE(OUTLYNE,*)&
            &'"MAGX" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.DF3.EQ.1) THEN
         IF(WC.EQ.'MAGY') THEN
            WRITE(OUTLYNE,*)&
            &'"MAGY" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'MAGX') THEN
            WRITE(OUTLYNE,*)&
            &'"MAGX" REQUIRES EXPLICIT NUMERIC WORD #1, #2 AND #3 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'FLDS') THEN
!     FLDS
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"FLDS" TAKES NO STRING INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0) THEN
         IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
            WRITE(OUTLYNE,*)&
            &'"FLDS" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.1) THEN
         IF(WQ.NE.'MAX')&
         &THEN
            WRITE(OUTLYNE,*)&
            &'"FLDS" ONLY TAKES "MAX" AS OPTIONAL'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)&
            &'QUALIFIER WORD INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.0.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"FLDS" REQUIRES SOME EXPLICIT NUMERIC WORD #2 OR #3 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WC.EQ.'FLDS'.AND.WQ.EQ.'MAX') THEN
         IF(DF1.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"FLDS MAX" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WC.EQ.'FLDS'.AND.WQ.EQ.'MAX') THEN
         IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
            WRITE(OUTLYNE,*)&
            &'"FLDS MAX" ONLY TAKES NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(SQ.EQ.0.AND.DF2.EQ.1) W2=0.0D0
      IF(SQ.EQ.0.AND.DF3.EQ.1) W3=0.0D0
      IF(SQ.EQ.0.AND.DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"FLDS" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0.AND.W1.LT.1.0D0.OR.SQ.EQ.0.AND.W2.GT.25.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"FLDS" REQUIRES NUMERIC WORD #1 TO BE BETWEEN 1 AND 25'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'MAX'.AND.W1.LT.1.0D0 &
      &.OR.WQ.EQ.'MAX'.AND.W2.GT.25.0D0) THEN
         WRITE(OUTLYNE,*)&
         &'"FLDS MAX" REQUIRES NUMERIC WORD #1 TO BE BETWEEN 1 AND 25'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX'.OR.WC.EQ.'ERY'.OR.&
   &WC.EQ.'ERX') THEN
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:4),'" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:3),'" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:4),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX') THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:3),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
         END IF
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0.OR.SQ.EQ.1.AND.WQ.EQ.'HLD') THEN
!       JUST PLANE OLD ADJUST OR HOLD
         IF(DF1.EQ.1) THEN
            WRITE(OUTLYNE,*)'F/NUMBER AND EXIT PUPIL ADJUSTMENTS REQUIRE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         IF(WQ.NE.'DELK') THEN
            WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         ELSE
!       QUALIFIER MUST BE 'DELK'
            IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
               IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX') THEN
                  WRITE(OUTLYNE,*)&
                  &'"',WC(1:4),&
                  &' DEL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
               END IF
               IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX') THEN
                  WRITE(OUTLYNE,*)&
                  &'"',WC(1:3),&
                  &' DEL" ONLY TAKES NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
               END IF
               RETRET=1
               CALL MACFAL
               RETURN
            END IF
            IF(DF1.EQ.1) THEN
               IF(WC.EQ.'FNBY'.OR.WC.EQ.'FNBX') THEN
                  WRITE(OUTLYNE,*)&
                  &'"',WC(1:4),&
                  &' DEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
               END IF
               IF(WC.EQ.'ERY'.OR.WC.EQ.'ERX') THEN
                  WRITE(OUTLYNE,*)&
                  &'"',WC(1:3),&
                  &' DEL" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
               END IF
               RETRET=1
               CALL MACFAL
               RETURN
            END IF
         END IF
         RETURN
      END IF
   END IF
   IF(WC.EQ.'COCY'.OR.WC.EQ.'COCX') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"',WC(1:4),'" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'DEC') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"DEC" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
         WRITE(OUTLYNE,*)'"DEC" REQUIRES SOME EXPLICIT NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'PIVOT') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"PIVOTD" ONLY TAKES ADDITIONAL INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'PIVOT') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"PIVOT" ONLY TAKES NUMERIC WORD #1, #2 AND #3 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.AND.DF2.EQ.1.AND.DF3.EQ.1) THEN
         WRITE(OUTLYNE,*)'"PIVOT" REQUIRES SOME EXPLICIT NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.NE.'AUTOD') THEN
      IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TILT" ONLY TAKES QUALIFIER AND NUMERIC WORDS #1, #2 AND #3'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         IF(WQ.NE.'AUTO'.AND.WQ.NE.'AUTOM'.AND.WQ.NE.'BEN'&
         &.AND.WQ.NE.'DAR'.AND.WQ.NE.'RET'.AND.WQ.NE.'REV') THEN
            WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W2=0.0
      END IF
      IF(DF3.EQ.1) THEN
         DF3=0
         W3=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'RTILT') THEN
      IF(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"RTILT" ONLY TAKES QUALIFIER AND NUMERIC WORDS #1, #2 AND #3'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         WRITE(OUTLYNE,*)'"RTILT" TAKES NO QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W2=0.0
      END IF
      IF(DF3.EQ.1) THEN
         DF3=0
         W3=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'RETD') THEN
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TILT RETD" TAKES NO STRING OR NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'BEND') THEN
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TILT BEND" TAKES NO STRING OR NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'DARD') THEN
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TILT DARD" TAKES NO STRING OR NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'TILT'.AND.WQ.EQ.'AUTOD') THEN
      IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TILT AUTOD" TAKES NO STRING OR NUMERIC WORDS #3 THROUGH #5'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"TILT AUTOD" USES EITHER TWO OR ZERO NUMERIC WORDS'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W1).LT.0) THEN
         WRITE(OUTLYNE,*)&
         &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(INT(W2).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)&
         &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
         &INT(sys_last_surf())
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      IF(W1.GT.W2) THEN
         WRITE(OUTLYNE,*)&
         &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)&
         &'THE STARTING SURFACE #'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'RTILT') THEN
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W2=0.0
      END IF
      IF(DF3.EQ.1) THEN
         DF3=0
         W3=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'SCHOTT'.OR.WC.EQ.'OHARA'.OR.WC.EQ.'HOYA'&
   &.OR.WC.EQ.'CHANCE'.OR.WC.EQ.'CORNIN'.OR.WC.EQ.'HIKARI'&
   &.OR.WC.EQ.'RADHARD'.OR.WC.EQ.'MATL'.OR.WC.EQ.'SCH2000'.OR.&
   &WC.EQ.'GLCAT'.OR.WC.EQ.'GLAK'.OR.WC.EQ.'RUSSIAN') THEN
      IF(SN.EQ.1.OR.SQ.EQ.1) THEN
!
         IF(WQ.EQ.'SCHOTT') THEN
            WRITE(OUTLYNE,*)'"SCHOTT" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
         IF(WQ.EQ.'SCH2000') THEN
            WRITE(OUTLYNE,*)'"SCH2000" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
         IF(WQ.EQ.'RUSSIAN') THEN
            WRITE(OUTLYNE,*)'"RUSSIAN" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'GLASS') THEN
            WRITE(OUTLYNE,*)'"GLASS" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'RADHARD') THEN
            WRITE(OUTLYNE,*)'"RADHARD" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'USER') THEN
            WRITE(OUTLYNE,*)'"USER" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'CHANCE') THEN
            WRITE(OUTLYNE,*)'"CHANCE" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'GLCAT') THEN
            WRITE(OUTLYNE,*)'"GLCAT" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'CHANCE') THEN
            WRITE(OUTLYNE,*)'"CHANCE" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'CORNIN') THEN
            WRITE(OUTLYNE,*)'"CORNIN" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'MATL') THEN
            WRITE(OUTLYNE,*)'"MATL" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
         IF(WQ.EQ.'OHARA') THEN
            WRITE(OUTLYNE,*)'"OHARA" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
!
!
         IF(WQ.EQ.'HOYA') THEN
            WRITE(OUTLYNE,*)'"HOYA" ONLY TAKES STRING INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'IN SPECIAL NO LEADING COLON (:) MODE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      ELSE
         RETURN
      END IF
!
      RETURN
   END IF
   IF(WC.EQ.'GLASS') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"GLASS" ONLY TAKES QUALIFIER AND NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=1.0
      END IF
      IF(DF2.EQ.1) THEN
         DF2=0
         W2=1.0
      END IF
      IF(DF3.EQ.1) THEN
         DF3=0
         W3=1.0
      END IF
      IF(DF4.EQ.1) THEN
         DF4=0
         W4=1.0
      END IF
      IF(DF5.EQ.1) THEN
         DF5=0
         W5=1.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'MODEL') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)'"MODEL" ONLY TAKES QUALIFIER AND NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)'"MODEL" TAKES NO NUMERIC WORD #4 OR #5 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(S1.EQ.0.OR.S2.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"MODEL" REQUIRES EXPLICIT NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.0) THEN
         WRITE(OUTLYNE,*)&
         &'"MODEL" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'ASTOP') THEN
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)'"ASTOP" ONLY TAKES QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         IF(WQ.EQ.'EN'.OR.WQ.EQ.'EX'.OR.WQ.EQ.'ENEX'.OR.&
         &WQ.EQ.'DELK') THEN
            RETURN
         ELSE
            WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
         END IF
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'NODUM') THEN
      IF(SST.EQ.1.OR.SN.EQ.1) THEN
         WRITE(OUTLYNE,*)'"NODUM" ONLY TAKES QUALIFIER INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(SQ.EQ.1) THEN
         IF(WQ.EQ.'ON'.OR.WQ.EQ.'OFF'.OR.WQ.EQ.'YES'.OR.&
         &WQ.EQ.'NO') THEN
            RETURN
         ELSE
            WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
         END IF
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'CAY'.OR.WC.EQ.'CAX') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1 &
      &.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:3),'" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      RETURN
   END IF
   IF(WC.EQ.'SPSRF')THEN
      IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"SPSRF" ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.NE.'ON'.AND.WQ.NE.'OFF') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         DF1=0
         W1=0.0
      END IF
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(INT(W1).LT.1.OR.INT(W1).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
   END IF
   IF(WC.EQ.'PIKUP') THEN
      IF(WQ.NE.'RD'.AND.WQ.NE.'CV'.AND.WQ.NE.'CC'.AND.WC.NE.'AH'&
      &.AND.WQ.NE.'TH'.AND.WQ.NE.'AD'.AND.WC.NE.'AE'.AND.WC.NE.'AJ'&
      &.AND.WQ.NE.'AF'.AND.WQ.NE.'AG'.AND.WQ.NE.'CVTOR'.AND.WC.NE.'AK'&
      &.AND.WQ.NE.'RDTOR'.AND.WQ.NE.'YD'.AND.WQ.NE.'XD'.AND.WC.NE.'AL'&
      &.AND.WQ.NE.'ALPHA'.AND.WQ.NE.'BETA'.AND.WQ.NE.'AI'.AND.WC.NE.&
      &'GAMMA'.AND.WQ.NE.'CLAP'.AND.WQ.NE.'COBS'.AND.WQ.NE.'ZD'.AND.&
      &WQ.NE.'CCTOR'.AND.WQ.NE.'ADTOR'.AND.WQ.NE.'AETOR'&
      &.AND.WQ.NE.'AFTOR'.AND.WQ.NE.'AGTOR'.AND.WQ.NE.'PIVX'.AND.&
      &WQ.NE.'GLASS'.AND.WQ.NE.'AC'.AND.WQ.NE.'PRO'.AND.WQ.NE.'PIVY'&
      &.AND.WQ.NE.'NPRO'.AND.WQ.NE.'THOAL'.AND.WQ.NE.'PIVZ'&
      &.AND.WQ.NE.'GDX'.AND.WQ.NE.'GDY'.AND.WQ.NE.'GDZ'.AND.WQ.NE.&
      &'GALPHA'.AND.WQ.NE.'GBETA'.AND.WQ.NE.'GGAMMA'.AND.WQ.NE.'GRT'&
      &.AND.WQ.NE.'COATING')THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
!
      IF(WQ.EQ.'RD'.OR.WQ.EQ.'CV'.OR.WQ.EQ.'CC'.OR.WQ.EQ.'AH'&
      &.OR.WQ.EQ.'TH'.OR.WQ.EQ.'AD'.OR.WQ.EQ.'AE'.OR.WQ.EQ.'AI'&
      &.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AG'.OR.WQ.EQ.'CVTOR'&
      &.OR.WQ.EQ.'RDTOR'.OR.WQ.EQ.'YD'.OR.WQ.EQ.'XD'.OR.WQ.EQ.'ZD'&
      &.OR.WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.'AJ'.OR.WQ.EQ.'AK'&
      &.OR.WQ.EQ.'GAMMA'.OR.WQ.EQ.'CLAP'.OR.WQ.EQ.'COBS'.OR.WQ.EQ.'AL'&
      &.OR.WQ.EQ.'CCTOR'.OR.WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'&
      &.OR.WQ.EQ.'AFTOR'.OR.WQ.EQ.'AGTOR'.OR.WQ.EQ.'PIVX'.OR.WQ.EQ.&
      &'PIVY'.OR.WQ.EQ.'GLASS'.OR.WQ.NE.'AC'.OR.WQ.EQ.'PRO'&
      &.OR.WQ.EQ.'NPRO'.OR.WC.EQ.'PIVZ'.OR.WQ.EQ.'GDX'.OR.WQ.EQ.'GDY'&
      &.OR.WQ.EQ.'GDZ'.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.&
      &'GGAMMA'.OR.WQ.EQ.'GRT'.OR.WQ.EQ.'COATING') THEN
         IF(DF1.EQ.1) THEN
            WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'WITH THIS PIKUP'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'THOAL') THEN
         IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'EXPLICIT NUMERIC WORDS #1 AND #2 INPUT ARE REQUIRED'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'WITH "PIKUP THOAL"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'RD'.OR.WQ.EQ.'CV'.OR.WQ.EQ.'CC'.OR.WQ.EQ.'AH'&
      &.OR.WQ.EQ.'TH'.OR.WQ.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AI'&
      &.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AG'.OR.WQ.EQ.'CVTOR'&
      &.OR.WQ.EQ.'RDTOR'.OR.WQ.EQ.'YD'.OR.WQ.EQ.'XD'.OR.WQ.EQ.'ZD'&
      &.OR.WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.'COATING'.OR.WQ.EQ.&
      &'GAMMA'.OR.WQ.EQ.'CLAP'.OR.WQ.EQ.'COBS'.OR.&
      &WQ.EQ.'CCTOR'.OR.WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'&
      &.OR.WQ.EQ.'AFTOR'.OR.WQ.EQ.'AGTOR'.OR.WC.EQ.'AJ'.OR.WC.EQ.'AK'&
      &.OR.WQ.EQ.'GLASS'.OR.WQ.NE.'AC'.OR.WQ.EQ.'PRO'&
      &.OR.WQ.EQ.'NPRO'.OR.WC.EQ.'AL'.OR.WQ.EQ.'PIVX'.OR.WQ.EQ.'PIVY'&
      &.OR.WQ.EQ.'PIVZ'.OR.WQ.EQ.'GDX'.OR.WQ.EQ.'GDY'.OR.WQ.EQ.'GDZ'&
      &.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.'GGAMMA') THEN
         IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
         IF(INT(W1).LT.0.OR.INT(W1).GT.INT(sys_last_surf())) THEN
            WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'THOAL') THEN
         IF(INT(W1).LT.0.OR.INT(W1).GT.INT(sys_last_surf()).OR.&
         &INT(W2).LT.0.OR.INT(W2).GT.INT(sys_last_surf()).OR.INT(W2)&
         &.LT.INT(W1)) THEN
            WRITE(OUTLYNE,*)'SURFACE NUMBERS BEYOND LEGAL RANGE'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'FOR "PIKUP THOAL"'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'GLASS') THEN
         IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKUP GLASS" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'GLASS') THEN
         IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKUP COATING" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'GRT') THEN
         IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKUP GRT" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'PRO') THEN
         IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKUP PRO" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(WQ.EQ.'NPRO') THEN
         IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKUP NPRO" ONLY TAKES NUMERIC WORD #1 AND #5 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)'"PIKUP" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(INT(W1).LT.0.OR.INT(W1).GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   IF(WC.EQ.'PIKD') THEN
      SST=0
      IF(WQ.NE.'RD'.AND.WQ.NE.'CV'.AND.WQ.NE.'CC'.AND.WQ.NE.'AH'&
      &.AND.WQ.NE.'TH'.AND.WQ.NE.'AD'.AND.WC.NE.'AE'.AND.WC.NE.'AC'&
      &.AND.WQ.NE.'AF'.AND.WQ.NE.'AG'.AND.WQ.NE.'CVTOR'&
      &.AND.WQ.NE.'RDTOR'.AND.WQ.NE.'YD'.AND.WQ.NE.'XD'.AND.WQ.NE.'ZD'&
      &.AND.WQ.NE.'ALPHA'.AND.WQ.NE.'BETA'.AND.WQ.NE.'AI'.AND.&
      &WQ.NE.'GAMMA'.AND.WQ.NE.'CLAP'.AND.WQ.NE.'COBS'.AND.&
      &WQ.NE.'CCTOR'.AND.WQ.NE.'ADTOR'.AND.WQ.NE.'AETOR'&
      &.AND.WQ.NE.'AFTOR'.AND.WQ.NE.'AGTOR'.AND.WQ.NE.'AJ'.AND.&
      &WQ.NE.'GLASS'.AND.WQ.NE.'PRO'.AND.WQ.NE.'NPRO'&
      &.AND.WQ.NE.'AK'.AND.WQ.NE.'AL'.AND.WQ.NE.'THOAL'.AND.WQ.NE.&
      &'PIVX'.AND.WQ.NE.'PIVY'.AND.WQ.NE.'PIVZ'.AND.WQ.NE.'GDX'.AND.&
      &WQ.NE.'GDY'.AND.WQ.NE.'GDZ'.AND.WQ.NE.'GALPHA'.AND.WQ.NE.&
      &'GBETA'.AND.WQ.NE.'GGAMMA'.AND.WQ.NE.'COATING') THEN
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'RD'.OR.WQ.EQ.'CV'.OR.WQ.EQ.'CC'.OR.WQ.EQ.'AH'&
      &.OR.WQ.EQ.'TH'.OR.WQ.EQ.'AD'.OR.WC.EQ.'AE'.OR.WC.EQ.'AC'&
      &.OR.WQ.EQ.'AF'.OR.WQ.EQ.'AG'.OR.WQ.EQ.'CVTOR'.OR.WQ.EQ.'AI'&
      &.OR.WQ.EQ.'RDTOR'.OR.WQ.EQ.'YD'.OR.WQ.EQ.'XD'.OR.WQ.EQ.'AJ'&
      &.OR.WQ.EQ.'ALPHA'.OR.WQ.EQ.'BETA'.OR.WQ.EQ.'AK'.OR.WQ &
      &.EQ.'GAMMA'.OR.WQ.EQ.'CLAP'.OR.WQ.EQ.'COBS'.OR.WQ.EQ.'AL'.OR.&
      &WQ.EQ.'CCTOR'.OR.WQ.EQ.'ADTOR'.OR.WQ.EQ.'AETOR'&
      &.OR.WQ.EQ.'AFTOR'.OR.WQ.EQ.'AGTOR'.OR.WQ.EQ.'GLASS'&
      &.OR.WQ.EQ.'PRO'.OR.WQ.EQ.'NPRO'.OR.WQ.EQ.'ZD'.OR.WQ.EQ.'PIVX'&
      &.OR.WQ.EQ.'PIVY'.OR.WQ.EQ.'PIVZ'.OR.WQ.EQ.'GDX'.OR.WQ.EQ.'GDY'&
      &.OR.WQ.EQ.'GDZ'.OR.WQ.EQ.'GALPHA'.OR.WQ.EQ.'GBETA'.OR.WQ.EQ.&
      &'GGAMMA'.OR.WQ.EQ.'COATING') THEN
         IF(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKD" TAKES NO STRING OR NUMERIC WORDS #3 THROUGH #5'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
         IF(DF1.EQ.1.AND.DF2.EQ.0.OR.DF1.EQ.0.AND.DF2.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"PIKD" USES EITHER TWO OR ZERO NUMERIC WORDS'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(INT(W1).LT.0) THEN
            WRITE(OUTLYNE,*)&
            &'STARTING SURFACE NUMBER MUST BE GREATER THAN OR EQUAL TO 0'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(INT(W2).GT.INT(sys_last_surf())) THEN
            WRITE(OUTLYNE,*)&
            &'ENDING SURFACE NUMBER MUST BE LESS THAN OR EQUAL TO ',&
            &INT(sys_last_surf())
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         IF(W1.GT.W2) THEN
            WRITE(OUTLYNE,*)&
            &'THE ENDING SURFACE # MUST BE GREATER THAN OR EQUAL TO#'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)&
            &'THE STARTING SURFACE #'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            CALL MACFAL
            RETURN
         END IF
         RETURN
      END IF
      RETURN
   END IF
!
   IF(WC.EQ.'CLAP'.OR.WC.EQ.'COBS') THEN
      IF(SST.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"',WC(1:4),'" ONLY TAKES QUALIFIER AND NUMERIC WORD INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(WQ.EQ.'        '.OR.WQ.EQ.'ERASE'.OR.WQ.EQ.'ELIP'&
      &.OR.WQ.EQ.'ELIPE'.OR.WQ.EQ.'RECT'.OR.WQ.EQ.'RECTE'.OR.&
      &WQ.EQ.'RCTK'.OR.WQ.EQ.'RCTKE'.OR.WQ.EQ.'POLY'.OR.&
      &WQ.EQ.'POLYE'.OR.WQ.EQ.'IPOLY'.OR.WQ.EQ.'IPOLYE') THEN
         IF(WQ.EQ.'        '.OR.WQ.EQ.'ERASE') THEN
            IF(DF1.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF2.EQ.1) THEN
               DF2=0
               W2=0.0
            END IF
            IF(DF3.EQ.1) THEN
               DF3=0
               W3=0.0
            END IF
            IF(WQ.EQ.'ERASE') THEN
               IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
                  WRITE(OUTLYNE,*)&
                  &'NUMERIC WORDS #4 AND #5 NOT USED IN THIS INSTANCE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETRET=1
                  CALL MACFAL
                  RETURN
               END IF
            END IF
            IF(WQ.EQ.'        ') THEN
               IF(DF5.EQ.0) THEN
                  WRITE(OUTLYNE,*)'NUMERIC WORD #5 NOT USED IN THIS INSTANCE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
                  CALL SHOWIT(1)
                  RETRET=1
                  CALL MACFAL
                  RETURN
               END IF
            END IF
            RETURN
         END IF
         IF(WQ.EQ.'ELIP'.OR.WQ.EQ.'ELIPE') THEN
            IF(DF1.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF2.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #2 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF3.EQ.1) THEN
               DF3=0
               W3=0.0
            END IF
            IF(DF4.EQ.1) THEN
               DF4=0
               W4=0.0
            END IF
            IF(DF5.EQ.0) THEN
               WRITE(OUTLYNE,*)'NUMERIC WORD #5 NOT USED IN THIS INSTANCE'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
         IF(WQ.EQ.'RECT'.OR.WQ.EQ.'RECTE') THEN
            IF(DF1.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF2.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #2 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF3.EQ.1) THEN
               DF3=0
               W3=0.0
            END IF
            IF(DF4.EQ.1) THEN
               DF4=0
               W4=0.0
            END IF
            IF(DF5.EQ.0) THEN
               WRITE(OUTLYNE,*)'NUMERIC WORD #5 NOT USED IN THIS INSTANCE'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               CALL MACFAL
               RETURN
            END IF
            RETURN
         END IF
         IF(WQ.EQ.'RCTK'.OR.WQ.EQ.'RCTKE') THEN
            IF(DF1.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF2.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #2 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF3.EQ.1) THEN
               DF3=0
               W3=0.0
            END IF
            IF(DF4.EQ.1) THEN
               DF4=0
               W4=0.0
            END IF
            IF(DF5.EQ.1) THEN
               DF5=0
               W5=0.01
            END IF
            RETURN
         END IF
         IF(WQ.EQ.'POLY'.OR.WQ.EQ.'POLYE') THEN
            IF(DF1.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF2.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #2 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF3.EQ.1) THEN
               DF3=0
               W3=0.0
            END IF
            IF(DF4.EQ.1) THEN
               DF4=0
               W4=0.0
            END IF
            IF(DF5.EQ.1) THEN
               DF5=0
               W5=0.01
            END IF
            RETURN
         END IF
         IF(WQ.EQ.'IPOLY'.OR.WQ.EQ.'IPOLYE') THEN
            IF(DF1.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #1 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF2.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #2 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF3.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #3 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF4.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #4 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            IF(DF5.EQ.1) THEN
               WRITE(OUTLYNE,*)'EXPLICIT NUMERIC WORD #5 INPUT IS REQUIRED'
               CALL SHOWIT(1)
               WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
               CALL SHOWIT(1)
               RETRET=1
               RETURN
            END IF
            RETURN
         END IF
      ELSE
!       INVALID QUALIFIER
         WRITE(OUTLYNE,*)'INVALID QUALIFIER FOUND'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      RETURN
   END IF
   CEE=.FALSE.
   IF(WC.EQ.'C1') CEE=.TRUE.
   IF(WC.EQ.'C2') CEE=.TRUE.
   IF(WC.EQ.'C3') CEE=.TRUE.
   IF(WC.EQ.'C4') CEE=.TRUE.
   IF(WC.EQ.'C5') CEE=.TRUE.
   IF(WC.EQ.'C6') CEE=.TRUE.
   IF(WC.EQ.'C7') CEE=.TRUE.
   IF(WC.EQ.'C8') CEE=.TRUE.
   IF(WC.EQ.'C9') CEE=.TRUE.
   IF(WC.EQ.'C10') CEE=.TRUE.
   IF(WC.EQ.'C11') CEE=.TRUE.
   IF(WC.EQ.'C12') CEE=.TRUE.
   IF(WC.EQ.'C13') CEE=.TRUE.
   IF(WC.EQ.'C14') CEE=.TRUE.
   IF(WC.EQ.'C15') CEE=.TRUE.
   IF(WC.EQ.'C16') CEE=.TRUE.
   IF(WC.EQ.'C17') CEE=.TRUE.
   IF(WC.EQ.'C18') CEE=.TRUE.
   IF(WC.EQ.'C19') CEE=.TRUE.
   IF(WC.EQ.'C20') CEE=.TRUE.
   IF(WC.EQ.'C21') CEE=.TRUE.
   IF(WC.EQ.'C22') CEE=.TRUE.
   IF(WC.EQ.'C23') CEE=.TRUE.
   IF(WC.EQ.'C24') CEE=.TRUE.
   IF(WC.EQ.'C25') CEE=.TRUE.
   IF(WC.EQ.'C26') CEE=.TRUE.
   IF(WC.EQ.'C27') CEE=.TRUE.
   IF(WC.EQ.'C28') CEE=.TRUE.
   IF(WC.EQ.'C29') CEE=.TRUE.
   IF(WC.EQ.'C30') CEE=.TRUE.
   IF(WC.EQ.'C31') CEE=.TRUE.
   IF(WC.EQ.'C32') CEE=.TRUE.
   IF(WC.EQ.'C33') CEE=.TRUE.
   IF(WC.EQ.'C34') CEE=.TRUE.
   IF(WC.EQ.'C35') CEE=.TRUE.
   IF(WC.EQ.'C36') CEE=.TRUE.
   IF(WC.EQ.'C37') CEE=.TRUE.
   IF(WC.EQ.'C38') CEE=.TRUE.
   IF(WC.EQ.'C39') CEE=.TRUE.
   IF(WC.EQ.'C40') CEE=.TRUE.
   IF(WC.EQ.'C41') CEE=.TRUE.
   IF(WC.EQ.'C42') CEE=.TRUE.
   IF(WC.EQ.'C43') CEE=.TRUE.
   IF(WC.EQ.'C44') CEE=.TRUE.
   IF(WC.EQ.'C45') CEE=.TRUE.
   IF(WC.EQ.'C46') CEE=.TRUE.
   IF(WC.EQ.'C47') CEE=.TRUE.
   IF(WC.EQ.'C48') CEE=.TRUE.
   IF(WC.EQ.'C49') CEE=.TRUE.
   IF(WC.EQ.'C50') CEE=.TRUE.
   IF(WC.EQ.'C51') CEE=.TRUE.
   IF(WC.EQ.'C52') CEE=.TRUE.
   IF(WC.EQ.'C53') CEE=.TRUE.
   IF(WC.EQ.'C54') CEE=.TRUE.
   IF(WC.EQ.'C55') CEE=.TRUE.
   IF(WC.EQ.'C56') CEE=.TRUE.
   IF(WC.EQ.'C57') CEE=.TRUE.
   IF(WC.EQ.'C58') CEE=.TRUE.
   IF(WC.EQ.'C59') CEE=.TRUE.
   IF(WC.EQ.'C60') CEE=.TRUE.
   IF(WC.EQ.'C61') CEE=.TRUE.
   IF(WC.EQ.'C62') CEE=.TRUE.
   IF(WC.EQ.'C63') CEE=.TRUE.
   IF(WC.EQ.'C64') CEE=.TRUE.
   IF(WC.EQ.'C65') CEE=.TRUE.
   IF(WC.EQ.'C66') CEE=.TRUE.
   IF(WC.EQ.'C67') CEE=.TRUE.
   IF(WC.EQ.'C68') CEE=.TRUE.
   IF(WC.EQ.'C69') CEE=.TRUE.
   IF(WC.EQ.'C70') CEE=.TRUE.
   IF(WC.EQ.'C71') CEE=.TRUE.
   IF(WC.EQ.'C72') CEE=.TRUE.
   IF(WC.EQ.'C73') CEE=.TRUE.
   IF(WC.EQ.'C74') CEE=.TRUE.
   IF(WC.EQ.'C75') CEE=.TRUE.
   IF(WC.EQ.'C76') CEE=.TRUE.
   IF(WC.EQ.'C77') CEE=.TRUE.
   IF(WC.EQ.'C78') CEE=.TRUE.
   IF(WC.EQ.'C79') CEE=.TRUE.
   IF(WC.EQ.'C80') CEE=.TRUE.
   IF(WC.EQ.'C81') CEE=.TRUE.
   IF(WC.EQ.'C82') CEE=.TRUE.
   IF(WC.EQ.'C83') CEE=.TRUE.
   IF(WC.EQ.'C84') CEE=.TRUE.
   IF(WC.EQ.'C85') CEE=.TRUE.
   IF(WC.EQ.'C86') CEE=.TRUE.
   IF(WC.EQ.'C87') CEE=.TRUE.
   IF(WC.EQ.'C88') CEE=.TRUE.
   IF(WC.EQ.'C89') CEE=.TRUE.
   IF(WC.EQ.'C90') CEE=.TRUE.
   IF(WC.EQ.'C91') CEE=.TRUE.
   IF(WC.EQ.'C92') CEE=.TRUE.
   IF(WC.EQ.'C93') CEE=.TRUE.
   IF(WC.EQ.'C94') CEE=.TRUE.
   IF(WC.EQ.'C95') CEE=.TRUE.
   IF(WC.EQ.'C96') CEE=.TRUE.
   IF(CEE) THEN
!
      IF(WC.EQ.'C1'.OR.WC.EQ.'C2'.OR.WC.EQ.'C3'.OR.WC.EQ.'C4'&
      &.OR.WC.EQ.'C5'.OR.WC.EQ.'C6'.OR.WC.EQ.'C7'.OR.WC.EQ.'C8'&
      &.OR.WC.EQ.'C9')THEN

         IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:2),'" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      ELSE
         IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
            WRITE(OUTLYNE,*)&
            &'"',WC(1:3),'" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
            CALL SHOWIT(1)
            WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
            CALL SHOWIT(1)
            RETRET=1
            CALL MACFAL
            RETURN
         END IF
      END IF
!
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.&
      &DF1.EQ.1.AND.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)'" ',WC,' " REQUIRES EXPLICIT NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'           FOR NUMERIC WORDS 1 AND 2'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
      IF(W1.EQ.0.0) THEN
         WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
!       CHECK SURFACE NUMBER RANGE
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(INT(W1).LT.1.OR.INT(W1)&
      &.GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
   END IF
   IF(WC.EQ.'SPECIAL'.OR.WC.EQ.'GENL') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
         WRITE(OUTLYNE,*)&
         &'"SPECIAL OR GENL" ONLY TAKES NUMERIC WORD #1 AND #2 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1.OR.DF2.EQ.1.OR.&
      &DF1.EQ.1.AND.DF2.EQ.1) THEN
         WRITE(OUTLYNE,*)'" ',WC,' " REQUIRES EXPLICIT NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'           FOR NUMERIC WORDS 1 AND 2'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
      IF(W1.EQ.0.0) THEN
         WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
!       CHECK SURFACE NUMBER RANGE
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(INT((W1)).LT.1.OR.INT((W1))&
      &.GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
   ELSE
!
!     SET THE SPECIAL SURFACE TRACKER TO THE SPECIAL SURFACE TYPE
      SPECFF(F12,INT(W1))=INT(W2)
   END IF
   IF(WC.EQ.'SPDEL') THEN
      IF(SST.EQ.1.OR.SQ.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1.OR.&
      &S2.EQ.1) THEN
         WRITE(OUTLYNE,*)'"SPDEL" ONLY TAKES NUMERIC WORD #1 INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         CALL MACFAL
         RETURN
      END IF
      IF(DF1.EQ.1) THEN
         WRITE(OUTLYNE,*)'" ',WC,' " REQUIRES EXPLICIT NUMERIC INPUT'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'           FOR NUMERIC WORD 1'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
      IF(W1.EQ.0.0) THEN
         WRITE(OUTLYNE,*)'SPECIAL SURFACE TYPE NOT VALID ON OBJECT SURFACE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      END IF
!       CHECK SURFACE NUMBER RANGE
      IF(W1.LT.0.0D0) W1=sys_last_surf()+W1
      IF(INT((W1)).LT.1.OR.INT((W1))&
      &.GT.INT(sys_last_surf())) THEN
         WRITE(OUTLYNE,*)'SURFACE NUMBER BEYOND LEGAL RANGE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
         CALL SHOWIT(1)
         RETRET=1
         RETURN
      ELSE
         SPECFF(F12,INT(W1))=0
      END IF
   END IF
!
   RETURN
END
! SUB CFCHG1.FOR
SUBROUTINE CFCHG1
!
   use DATMAI
   IMPLICIT NONE
!
!       THIS SUBROUTINE CALLS PROCESS FROM THE CFG CHANGE
!       ROUTINE
!
   CHARACTER DUMPY*140
!
   COMMON/LUMPY/DUMPY
!
   SAVE_KDP(11)=SAVEINPT(11)
   INPUT(1:140)=DUMPY(1:140)
10 FORMAT(A20)
   IF(INPUT(1:4).EQ.'ZERO') THEN
      CALL ZERO
      DUMPY=AA//AA//AA//AA//AA//AA//AA
      RETURN
   END IF
   IF(INPUT(1:4).NE.'ZERO') THEN
      DUMPY=AA//AA//AA//AA//AA//AA//AA
      CALL PROCES
      REST_KDP(11)=RESTINPT(11)
   END IF
   RETURN
END
! SUB LORDER.FOR
SUBROUTINE LORDER(I)
!
   use DATCFG
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf
   IMPLICIT NONE
!
!       THIS SUBROUTINE IS CALLED FROM CFGCLN. IT CLEANS
!       UP AND REORDERS THE UPDATE LENS DATA AFTER THAT
!       DATA IS PUT INTO ONE U L/EOS GROUP BY CFGCLN
!       I DESIGNATES THE CONFIG NUMBER FROM SUBROUTINE CFGCLN
!
   CHARACTER &
   &BLANK*140,ATEST*8,&
   &PART*23,A*140,BTEST*29
!
   COMMON/BLAAA/BLANK
!
   INTEGER SCRCNT,AATI,AATJ,AATJK,ALLOERR,&
   &ILINE,NUM,CHGLOC,CHGLO1,ELINE,&
   &ULINE,SCR(1:2000),LOCCNT(1:2000),EXTRA,CNT,CFGLAS,&
   &IL,CFGNOW,I,JJ,K,II,KK,KKK,JK,J
!
   COMMON/AATST/AATJK,AATI,AATJ
!
   REAL*8 PVALUE,XK
!
!
   CHARACTER*140 SCRATH
   DIMENSION SCRATH(:)
   ALLOCATABLE :: SCRATH
   INTEGER NANA
   NANA=2000
   ALLOCATE(SCRATH(NANA),STAT=ALLOERR)
!
   BLANK=AA//AA//AA//AA//AA//AA//AA
   SCRATH(1:2000)=BLANK
!
   GO TO 997
!       CLEAR ALL DOUBLE CHG'S
995 CONTINUE
   IF(CFGLAS.EQ.CFGNOW) GO TO 996
997 CNT=0
   DO 991 J=1, CFGCNT(I)
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF((HOLDER(1:3)).EQ.'CHG') THEN
         EE12=CONFG(I,(J+1))
         HOLDER=EE12
         IF((HOLDER(1:3)).EQ.'CHG'.OR.&
         &(HOLDER(1:3)).EQ.'SAY'.OR.(HOLDER(1:3)).EQ.'SAX'.OR.&
         &(HOLDER(1:3)).EQ.'WRX'.OR.(HOLDER(1:3)).EQ.'WRY'.OR.&
         &(HOLDER(1:3)).EQ.'BDX'.OR.(HOLDER(1:3)).EQ.'BDY'.OR.&
         &(HOLDER(1:4)).EQ.'NAOY'.OR.(HOLDER(1:4)).EQ.'NAOX'.OR.&
         &(HOLDER(1:4)).EQ.'FNOY'.OR.(HOLDER(1:4)).EQ.'FNOX'.OR.&
         &(HOLDER(1:3)).EQ.'SCY'.OR.(HOLDER(1:3)).EQ.'SCX'.OR.&
         &(HOLDER(1:2)).EQ.'LI'.OR.(HOLDER(1:3)).EQ.'LIC'.OR.&
         &(HOLDER(1:3)).EQ.'INI'.OR.(HOLDER(1:5)).EQ.'LTYPE'.OR.&
         &(HOLDER(1:4)).EQ.'MODE'.OR.(HOLDER(1:5)).EQ.'SPTWT'.OR.&
         &(HOLDER(1:6)).EQ.'SPTWT2'.OR.(HOLDER(1:2)).EQ.'WV'.OR.&
         &(HOLDER(1:3)).EQ.'WV2'.OR.(HOLDER(1:5)).EQ.'UNITS'.OR.&
         &(HOLDER(1:3)).EQ.'PCW'.OR.(HOLDER(1:3)).EQ.'SCW'.OR.&
         &(HOLDER(1:8)).EQ.'AUTOFUNC'.OR.HOLDER(1:4).EQ.'ZERO'.OR.&
         &(HOLDER(1:4)).EQ.'PXIM'.OR.(HOLDER(1:4)).EQ.'PYIM'.OR.&
         &(HOLDER(1:4)).EQ.'RXIM'.OR.(HOLDER(1:4)).EQ.'RYIM'.OR.&
         &(HOLDER(1:2)).EQ.'CW'.OR.(HOLDER(1:3)).EQ.'EOS') THEN
            CNT=CNT+1
            DO 992 JJ=J,CFGCNT(I)
               EE12=CONFG(I,(JJ+1))
               HOLDER=EE12
               CONFG(I,JJ)=HOLDER(1:140)
992         CONTINUE
         ELSE
         END IF
      ELSE
      END IF
991 CONTINUE
   CFGLAS=CFGCNT(I)
   CFGCNT(I)=CFGCNT(I)-CNT
   CFGNOW=CFGCNT(I)
   GO TO 995
996 CONTINUE
!       ALL DOUBLE CHG'S ARE PRE-PROCESSED
!
!       INITIALIZE THE FLCHG FLAGS WHICH TRACK THE CURRENT
!       LINE NUMBERS REFERENCED BY CHG COMMANDS
   FLCHG(0:INT(sys_last_surf()))=0
!
   ULINE=0
   ELINE=1
   DO 10 K=1,CFGCNT(I)
      EE12=CONFG(I,K)
      HOLDER=EE12
!       DETERMINE THE LINE NUMBERS FOR U L AND EOS
      IF((HOLDER(1:10)).EQ.'U        L'.OR.&
      &(HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.&
      &(HOLDER(1:10)).EQ.'UPDATE   L'.OR.&
      &(HOLDER(1:13)).EQ.'U        LENS') ULINE=K
      IF((HOLDER(1:3)).EQ.'EOS'.AND.K.GT.ULINE) THEN
         ELINE=K
      ELSE
      END IF
10 CONTINUE
!
!       ULINE IS THE LINE NUMBER OF THE U L COMMAND
!       ELINE IS THE LINE NUMBER OF ITS EOS COMMAND
!       TOTAL NUMBER OF LINES TO CLEAN UP IS
!
!               (ELINE-ULINE+1)
!       EXTRA IS THE COUNT OF ALL THINGS
!       AFTER THE LENS EOS
   EXTRA=CFGCNT(I)-ELINE
   IF(EXTRA.LT.0) EXTRA=0
!
!
!       SCRATCH MEMORY INITIALIZED
!
!       NOW IF ULINE=ELINE, THEN THERE IS NO CLEANUP TO DO
!       JUST RETURN
   IF(ULINE.EQ.(ELINE)) THEN
!       NO CLEANUP, JUST RETURN
      DEALLOCATE(SCRATH,STAT=ALLOERR)
      RETURN
   ELSE
!       THERE IS CLEANUP TO DO
   END IF
!
!       IF ULINE IS GREATER THAN 1, THERE IS NON-U L
!       DATA TO COPY TO THE SCRATH ARRAY
!
   IF(ULINE.GT.1) THEN
      DO 20 K=1,(ULINE-1)
         EE12=CONFG(I,K)
         HOLDER=EE12
         SCRATH(K)=HOLDER
         SCRCNT=K
20    CONTINUE
      SCRCNT=SCRCNT+1
      EE12=CONFG(I,ULINE)
      HOLDER=EE12
      SCRATH(SCRCNT)=HOLDER
   ELSE
!       ULINE NOT GT 1, SET ULINE=1
      ULINE=1
!       NO OTHER DATA IS INFRONT OF U L DATA
      SCRCNT=1
      EE12=CONFG(I,ULINE)
      HOLDER=EE12
      SCRATH(SCRCNT)=HOLDER
   END IF
!
!       NOW TO THE CLEANUP
!
!       FROM LINE ULINE+1 TO LINE ELINE-1 WE CHECK FOR
!       SPECIFIC COMMANDS AND WRITE THEM IN A SPECIFIC ORDER
!       INTO THE SCRATH ARRAY.
!       FIRST LOOK FOR NON-SURFACE DATA
   DO 25 II=1,36
      IF(II.EQ.1) ATEST= 'LI      '
      IF(II.EQ.2) ATEST= 'LIC     '
      IF(II.EQ.3) ATEST= 'MODE    '
      IF(II.EQ.4) ATEST= 'SPTWT   '
      IF(II.EQ.5) ATEST= 'SPTWT2  '
      IF(II.EQ.6) ATEST= 'WV      '
      IF(II.EQ.7) ATEST= 'WV2     '
      IF(II.EQ.8) ATEST= 'UNITS   '
      IF(II.EQ.9) ATEST= 'PCW     '
      IF(II.EQ.10) ATEST='SCW     '
      IF(II.EQ.11) ATEST='CW      '
      IF(II.EQ.12) ATEST='SAX     '
      IF(II.EQ.13) ATEST='SAY     '
      IF(II.EQ.14) ATEST='SAX     '
      IF(II.EQ.15) ATEST='SCY     '
      IF(II.EQ.16) ATEST='SCX     '
      IF(II.EQ.17) ATEST='PXIM    '
      IF(II.EQ.18) ATEST='PYIM    '
      IF(II.EQ.19) ATEST='RXIM    '
      IF(II.EQ.20) ATEST='RYIM    '
      IF(II.EQ.21) ATEST='NAOY    '
      IF(II.EQ.22) ATEST='NAOX    '
      IF(II.EQ.23) ATEST='FNOY    '
      IF(II.EQ.24) ATEST='FNOX    '
      IF(II.EQ.25) ATEST='WRX     '
      IF(II.EQ.26) ATEST='WRY     '
      IF(II.EQ.27) ATEST='BDX     '
      IF(II.EQ.28) ATEST='BDY     '
      IF(II.EQ.29) ATEST='INI     '
      IF(II.EQ.30) ATEST='LTYPE   '
      IF(II.EQ.31) ATEST='AUTOFUNC'
      IF(II.EQ.32) ATEST='ZERO    '
      DO 30 J=(ELINE-1),(ULINE+1),-1
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:8)).EQ.ATEST) THEN
            SCRCNT=SCRCNT+1
            SCRATH(SCRCNT)=HOLDER
            CONFG(I,J)=BLANK(1:140)
            GO TO 25
         ELSE
         END IF
30    CONTINUE
25 CONTINUE
!       NOW ONLY SURFACE DEPENDENT COMMANDS REMAIN
!
!       NOW FIND OUT WHICH LENS LINE NUMBERS ARE REFERENCED BY
!       CHG COMMANDS AND COUNT THEM UP AND KEEP TRACK
!       OF THEM IN ARRAY FLCHG.
   DO 31 J=(ULINE+1),(ELINE-1)
!       CHECK FOR BLANK OR NON-SURFACE AND SKIP
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF(HOLDER.EQ.BLANK.OR.HOLDER(1:3).EQ.'SAY'.OR.&
      &HOLDER(1:3).EQ.'SAX'.OR.HOLDER(1:3).EQ.'WRX'.OR.&
      &HOLDER(1:3).EQ.'WRY'.OR.HOLDER(1:3).EQ.'BDX'.OR.&
      &HOLDER(1:3).EQ.'BDY'.OR.HOLDER(1:4).EQ.'NAOY'.OR.&
      &HOLDER(1:4).EQ.'NAOX'.OR.HOLDER(1:4).EQ.'FNOY'.OR.&
      &HOLDER(1:4).EQ.'FNOX'.OR.HOLDER(1:3).EQ.'SCY'.OR.&
      &HOLDER(1:3).EQ.'SCX'.OR.HOLDER(1:2).EQ.'LI'.OR.&
      &HOLDER(1:3).EQ.'LIC'.OR.HOLDER(1:3).EQ.'INI'.OR.&
      &HOLDER(1:5).EQ.'LTYPE'.OR.HOLDER(1:4).EQ.'MODE'.OR.&
      &HOLDER(1:5).EQ.'SPTWT'.OR.HOLDER(1:6).EQ.'SPTWT2'.OR.&
      &HOLDER(1:2).EQ.'WV'.OR.HOLDER(1:3).EQ.'WV2'.OR.&
      &HOLDER(1:5).EQ.'UNITS'.OR.HOLDER(1:3).EQ.'PCW'.OR.&
      &(HOLDER(1:8)).EQ.'AUTOFUNC'.OR.HOLDER(1:4).EQ.'ZERO'.OR.&
      &(HOLDER(1:4)).EQ.'PXIM'.OR.(HOLDER(1:4)).EQ.'PYIM'.OR.&
      &(HOLDER(1:4)).EQ.'RXIM'.OR.(HOLDER(1:4)).EQ.'RYIM'.OR.&
      &HOLDER(1:3).EQ.'SCW'.OR.HOLDER(1:2).EQ.'CW') THEN
         CONFG(I,J)=BLANK(1:140)
         GO TO 31
      ELSE
!       PROCEED WITH PROCESSING
      END IF

!       CHECK FOR CHG
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF((HOLDER(1:3)).EQ.'CHG') THEN
!       CHECK FOR THE SURFACE IT IS CALLING
         PART=HOLDER(10:32)
!       PART IS A23 CHARACTER REPRESENTATION OF A POSITIVE DOUBLE
!       PRECISION NUMBER WRITTEN IN A D23.15 FORMAT
!       CONVERT TO REAL*8
         WRITE(A,100) PART
100      FORMAT(A23)
         READ(A,110) PVALUE
110      FORMAT(D23.15)
         ILINE=INT(PVALUE)
!               INCREMENT THE FLCHG FLAG FOR THIS SURFACE
         FLCHG(ILINE)=FLCHG(ILINE)+1
      ELSE
!       NOT CHG, PROCEED
      END IF
31 CONTINUE
!
!       WE NOW KNOW WHAT SURFACES ARE REFERENCED IN CONFIG I.
!       AND WE KNOW HOW MANNY TIMES THEY ARE EXPLICITLY REFERENCED
!
!       NOW SEARCH THE FLCHG ARRAY FOR NON ZERO VALUES
!       WHEN ON IS FOUND, PROCESS THAT DATA INTO A SEARCH
!       STARTING POINT FOR THE DATA TO WRITE TO SCRATH
!
!       NUM COUNTS THE NUMBER OF SIMILAR "CHG" PROCESSED
!       IN THE "33" LOOP
   DO 32 XK=0.0D0,sys_last_surf(),1.0D0
      NUM=0
!
!       CHECK FOR NO ENTRY IN FLCHG AND IF NONE, PROCEED
      IF(FLCHG(INT(XK)).EQ.0) THEN
!       NO DATA IN FLAG, GO TO 32
         GO TO 32
      ELSE
      END IF
!       FLCHG(K) MUST BE GE 1
!
!       WE NEED TO LOOK FOR DATA,DO IT HERE
      PVALUE=XK
      WRITE(A,110) PVALUE
      READ(A,100) PART
!
!       (PART) IS THE CHARACTER REPRESENTATOIN OF THE DOUBLE
!       PRECISION REPRESENTATION OF THE FLAG VALUE.
!       CREATE THE CHG,VALUE LINE TO TEST WITH AS (BTEST)
!
      BTEST='CHG     ,'//PART
!       NOW SEARCH FOR THE STARTING LOCATION WHERE (BTEST) IS
      DO 33 KK=(ULINE+1),(ELINE-1)
         EE12=CONFG(I,KK)
         HOLDER=EE12
         IF((HOLDER(1:29)).EQ.BTEST) THEN
!       FOUND THE LOCATION OF THE TARGET "CHG"
!
!       KK MARKS THE POSITION OF CURRENT FOUND (BTEST)
            NUM=NUM+1
!       NUM IS INCREMENTED TO COUNT THE NUMBER OF OCCURENCES OF
!       BEST THAT HAVE BEEN COPIED.
!
!       WE NOW KNOW THAT DATA TO BE WRITTEN TO SCRATH ARRAY STARTS IN
!       LINE KK OF THE CURRENT CFG FILE
            DO 35 KKK=KK,(ELINE-1)
               EE12=CONFG(I,KKK)
               HOLDER=EE12
!
!       THE KKK GOT INTO THIS PROGRAM !
!
               IF(NUM.EQ.1.AND.KKK.EQ.KK) THEN
!       WE WRITE 'CHG' BEFORE THE SURFACE DATA
                  SCRCNT=SCRCNT+1
                  SCRATH(SCRCNT)=HOLDER
                  CONFG(I,KKK)=BLANK(1:140)
                  NUM=NUM+1
!       GO TO NEXT ENTRY
                  GO TO 35
               ELSE
               END IF
               IF(NUM.GT.1.AND.KKK.EQ.KK) THEN
!       DONT WRITE "CHG", GO TO NEXT ENTRY, BLANK LINE FIRST
                  CONFG(I,KKK)=BLANK(1:140)
                  GO TO 35
               ELSE
               END IF
!       KKK GT KK, PROCEED
!       IS ENTRY ANOTHER CHG?
               IF(HOLDER(1:3).EQ.'CHG') THEN
!       IF FLCHG(K) GT 1 THEN WE MUST LOOK FOR MORE
!       DATA FOR THE CURRENT CHG, ELSE WE LOOK FOR THE
!       OCCURENCE OF THE NEXT BTEST
                  IF(FLCHG(INT(XK)).GT.1) THEN
!       DECREMENT THE FLCHG COUNTER
                     FLCHG(INT(XK))=FLCHG(INT(XK))-1
!       WE JUMP TO 33
                     GO TO 33
                  ELSE
!       PROCEED
                  END IF
!       STOP PROCESSING AND GO TO 32, LOOK FOR NEXT (BTEST)
                  GO TO 32
               ELSE
!       DATA TO WRITE TO SCRATH
                  IF(HOLDER.NE.BLANK) THEN
                     SCRCNT=SCRCNT+1
                     SCRATH(SCRCNT)=HOLDER
                     CONFG(I,KKK)=BLANK(1:140)
                  ELSE
                  END IF
!       FALL THROUGH TO 33
               END IF
!
35          CONTINUE
         ELSE
!       DID NOT FIND (BTEST)
!       CHECK THE NEXT LINE FOR (BTEST)
         END IF
33    CONTINUE
!
32 CONTINUE
!
!       IF YOU GOT HERE YOU FINISHED SORTING CHG ENTRIES
!       NOW FINISH SCRATH ARRAY
   DO 37 J=ELINE,CFGCNT(I)
      EE12=CONFG(I,J)
      HOLDER=EE12
      SCRCNT=SCRCNT+1
      SCRATH(SCRCNT)=HOLDER
      CONFG(I,J)=BLANK(1:140)
37 CONTINUE
!
!       NOW WRITE SCRATH TO CONFIG FILE AND CHANGE CFGCNT(I)
   CFGCNT(I)=SCRCNT
   DO 38 J=1,CFGCNT(I)
      CONFG(I,J)=SCRATH(J)(1:140)
38 CONTINUE
!
!       NEXT JOB WILL BE TO REMOVE REDUMDANT ENTRIES
!       WITHIN A CHG SET.
!       FIRST, DETERMINE AGAIN THE ULINE AND THE ELINE,
!       I E POSITIONS OF U L AND EOS
!
   DO 1000 K=1,CFGCNT(I)
!       DETERMINE THE LINE NUMBERS FOR U L AND EOS
      EE12=CONFG(I,K)
      HOLDER=EE12
      IF((HOLDER(1:10)).EQ.'U        L'.OR.&
      &(HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.&
      &(HOLDER(1:10)).EQ.'UPDATE   L'.OR.&
      &(HOLDER(1:13)).EQ.'U        LENS') ULINE=K
      IF((HOLDER(1:3)).EQ.'EOS'.AND.K.GT.ULINE) THEN
         ELINE=K
      ELSE
      END IF
1000 CONTINUE
!
!       ULINE IS THE LINE NUMBER OF THE U L COMMAND
!       ELINE IS THE LINE NUMBER OF ITS EOS COMMAND
!       CHECK FOR BLANK U L/EOS PAIR
   IF((ELINE-1).EQ.ULINE) THEN
!       WE ARE DONE, THE U L/EOS PAIR IS EMPTY
!       REMOVE IT!
      CONFG(I,K)=BLANK(1:140)
      CONFG(I,K+1)=BLANK(1:140)
      CONFG(I,ULINE)=BLANK(1:140)
      CONFG(I,ELINE)=BLANK(1:140)
      IF(ULINE.EQ.2) CONFG(I,1)=BLANK(1:140)
!
      DEALLOCATE(SCRATH,STAT=ALLOERR)
      RETURN
   ELSE
!       PAIR NOT EMPTY, PROCEED
   END IF
!
!       NOW WRITE OUT CONFIG TO ARRAY UP TO AND
!       INCLUDING ULINE
   SCRCNT=0
   DO 1001 J=1,ULINE
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF(HOLDER.EQ.BLANK) GO TO 1001
      SCRCNT=SCRCNT+1
      SCRATH(SCRCNT)=HOLDER(1:140)
      CONFG(I,J)=BLANK(1:140)
1001 CONTINUE
!       NOW WRITE IN THE NON-SURFACE DATA, ANYTHING BEFORE THE
!       FIRST 'CHG'
   DO 1002 J=(ULINE+1),ELINE
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF(HOLDER.EQ.BLANK) GO TO 1002
      IF(HOLDER(1:3).EQ.'CHG') THEN
!       WE JUST FOUND SURFACE, HANDEL IT HERE
!       WE JUST FOUND A CHG. WRITE IT TO THE
!       SCRATH ARRAY, REMEMBER ITS LOCATION IN CONFIG
!       AS CHGLOC, AND JUMP OUT OF THIS LOOP TO THE
!       NEXT STATMENT LABEL 1003
         SCRCNT=SCRCNT+1
         CHGLOC=J
         SCRATH(SCRCNT)=HOLDER
         CONFG(I,J)=BLANK(1:140)
         GO TO 1003
      ELSE
!       DATA IS NOT SURFACE DATA, PROCEED
         SCRCNT=SCRCNT+1
         SCRATH(SCRCNT)=HOLDER
         CONFG(I,J)=BLANK(1:140)
      END IF
!       NOW CHECK NEXT ENTRY IN CONFIG DATA LINE
1002 CONTINUE
1003 CONTINUE
!
!       WE HAVE JUST WRITTEN THE FIRST 'CHG' TO THE
!       U L/EOS PAIR. ALL OTHER DATA IS SURFACE DATA.
!       WE NOW PROCESS CONFIG FROM CHGLOC TO ELINE.
!

2000 SCR(1:2000)=0
   LOCCNT(1:2000)=0
   DO 1004 J=(CHGLOC+1),ELINE
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF(HOLDER.EQ.BLANK) GO TO 1004
      IF(HOLDER(1:3).EQ.'EOS') THEN
!       WRITE OUT 'EOS' AND RETURN, WE ARE DONE
         SCRCNT=SCRCNT+1
         SCRATH(SCRCNT)=HOLDER
         CONFG(I,J)=BLANK(1:140)
         GO TO 2010
      ELSE
!       NOT DONE YET, PROCEED
      END IF
      IF(HOLDER(1:3).EQ.'CHG') THEN
!               WE FOUND THE NEXT CHG. WRITE IT TO SCRATH
!       WE JUMP OUT OF THIS LOOP
!       TO LABEL 1006.
         CHGLO1=J
         SCRCNT=SCRCNT+1
         SCRATH(SCRCNT)=HOLDER
         CONFG(I,J)=BLANK(1:140)
         GO TO 1006
      ELSE
!       PROCEED, NOT 'CHG'
      END IF
!       WE ARE NOT AT AN 'EOS' OR 'CHG', WE MUST BE WHERE WE
!       NEED TO CHECK AGIANST THE REDUNDANT ENTRIES LIST,
!       DO THAT HERE:
!
!       UNTIL A SURFACE PARAMETER IS WRITTEN THE FIRST TIME,
!       SCR(JK) = 0
!       THEN IT IS SET TO 1 UNTIL WE RESTART WITH ANOTHER
!       CHG COMMAND
!       IF SCR(JK)=0, SCRCNT IS INCREMENTED BEFORE
!       SCRATH IS WRITTEN TO. IF SCR(JK) = 1
!       THEN SCRCNT IS NOT INCREMETED BEFORE SCRATH IS WITTEN TO.
!       ALL SCR(JK) RESET TO 0 WHEN GO TO 2000 IS PERFORMED
      JK=1
      AATJK=JK
      AATI=I
      AATJ=J
      CALL AATEST
      JK=AATJK
!       WHERE THE CALL TO AATEST PASSES THE VALUE OF CONFG(I,J)
!       TO AATEST AND AATEST RETURNS THE VALUE JK
      IF(SCR(JK).EQ.0) THEN
!       INCREMENT SCRCNT
         SCRCNT=SCRCNT+1
         SCR(JK)=1
!       THIS NEXT STEP REMEMBERS WHERE THE FIRST ENTRY WAS PLACED
!       IN THE SCRATH ARRAY FOR LATER POSSIBLE OVERWRITE
         LOCCNT(JK)=SCRCNT
      ELSE
!       SCR(JK) NOT 0, DONT INCREMENT SCRCNT
      END IF
!       THIS IS THE OVERWRITE OPERATION
      SCRATH(LOCCNT(JK))=HOLDER
      CONFG(I,J)=BLANK(1:140)
!
!       PROCEED TO NEXT DATA LINE IN CONFIG LIST
!
1004 CONTINUE
1005 CONTINUE
   GO TO 1007
1006 CHGLOC=CHGLO1
   GO TO 2000
1007 CONTINUE
2010 CONTINUE
!
!       NOW WRITE THE "EXTRA" NON- U L DATA
   DO 1022 J=ELINE+1,(ELINE+EXTRA)
      EE12=CONFG(I,J)
      HOLDER=EE12
      IF(HOLDER.EQ.BLANK) GO TO 1022
      SCRCNT=SCRCNT+1
      SCRATH(SCRCNT)=HOLDER
1022 CONTINUE
!       NOW WRITE BACK INTO CONFIG FILE
   CFGCNT(I)=SCRCNT
   DO 1020 J=1,SCRCNT
      CONFG(I,J)=SCRATH(J)(1:140)
1020 CONTINUE
!
!       CLEANUP OF THE U L/EOS LIST IS COMPLETED,
!       NOW RETURN TO CFGCLN SUBROUTINE
   DEALLOCATE(SCRATH,STAT=ALLOERR)
   RETURN
END
SUBROUTINE LCLEAN
!
   use DATCFG
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf, sys_high_cfg
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE LCLEAN. IT REMOVES INCONSISTENCIES
!       BETWEEN PIKUPS,SOLVES AND VALUE ASSIGNMENTS WITHIN
!       CONFIGURATION DATA.
!

   INTEGER SCRCNT,IEND,ULINE,ELINE,&
   &CHGCNT,CHG(1:2000),STP,I,J,K,ALLOERR
!
   CHARACTER BLANK*140,AVAL1*23
!
   COMMON/BLAAA/BLANK
!
   REAL*8 VAL1
!
   COMMON/JK_NTA3/VAL1,AVAL1
!
!
   CHARACTER*140 SCRATH
   DIMENSION SCRATH(:)
   ALLOCATABLE :: SCRATH
   INTEGER NANA,IL
   NANA=2000
   DEALLOCATE (SCRATH,STAT=ALLOERR)
   ALLOCATE (SCRATH(NANA),STAT=ALLOERR)
!
   BLANK=AA//AA//AA//AA//AA//AA//AA
   DO IL=1,2000
      SCRATH(IL)=BLANK
   END DO
!
!       LOOP THROUGH ALL NON-BLANK CONFIGS
!
!       IEND IS THE LAST NON-BLANK CONFIG
   IEND=INT(sys_high_cfg())
!
   DO I=2,IEND
!
!       FIND THE LOCATION OF THE U L AND ITS EOS LINE
!
      ULINE=0
      ELINE=1
!       CHECK FOR U L
      DO J=1,CFGCNT(I)
!
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:10)).EQ.'U        L'.OR.&
         &(HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.&
         &(HOLDER(1:10)).EQ.'UPDATE   L'.OR.&
         &(HOLDER(1:13)).EQ.'U        LENS') THEN
!       FOUND U L, SET ULINE TO J
            ULINE=J
         END IF
      END DO
      IF(ULINE.EQ.0) THEN
!       NOTHING TO CLEAN UP, JUST RETURN
         DEALLOCATE(SCRATH,STAT=ALLOERR)
         RETURN
      END IF
      DO J=(ULINE+1),CFGCNT(I)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:3)).EQ.'EOS') THEN
            ELINE=J
         END IF
      END DO
      IF(ELINE.EQ.0) THEN
         WRITE(OUTLYNE,*)'SERIOUS ERROR IN LCLEAN SUBROUTINE'
         CALL SHOWIT(1)
         WRITE(OUTLYNE,*)'LCLEAN TOOK NO ACTION'
         CALL SHOWIT(1)
         CALL MACFAL
         DEALLOCATE(SCRATH,STAT=ALLOERR)
         RETURN
      END IF
!
!       NOW WE HAVE ULINE AND ELINE. NOW FIND ALL THE CHG LINES
!       ASSUME A MAX OF 2000
!       CHGCNT WILL COUNT NUMBER OF CHGS, THE POSITION OF THE
!       KTH CHG IN CONFG(I,J) IS GIVEN BY CHG(K).
!       INITIALIZE ARRAYS
      CHGCNT=0
      CHG(1:2000)=0
      DO J=(ULINE+1),(ELINE-1)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:3)).EQ.'CHG') THEN
            CHGCNT=CHGCNT+1
            CHG(CHGCNT)=J
         END IF
      END DO
!
!       NOW CHECK TO SEE IF ANYTHING NEEDS TO BE BLANKED
!       OUT IN CONFG(I,J) AND THEN WRITE TO SCRATH ARRAY
!       AND UPDATE THE SCRCNT COUNTER
!
!       IF NO CHGS ARE PRESENT WE ARE DONE
      IF(CHGCNT.EQ.0) THEN
!       JUST RETURN
         DEALLOCATE(SCRATH,STAT=ALLOERR)
         RETURN
      END IF
!       NO DO THE FOLLOWING FOR AS MANNY CHGS AS ARE PRESENT
      DO K=1,CHGCNT
!       DETERMINE THE LAST LINE TO CHECK FOR A GIVEN K VALUE
         IF(K.EQ.CHGCNT) THEN
            STP=(ELINE-1)
         ELSE
            STP=CHG(K+1)-1
         END IF
         DO J=(CHG(K)+1),STP
!
            EE12=CONFG(I,J)
            HOLDER=EE12
!       DO ANY PIKUPS REFERENCE SURFACES BEYOND LEGAL RANGE?
            IF((HOLDER(1:5)).EQ.'PIKUP') THEN
               AVAL1=HOLDER(19:41)
               CALL ATON3
               IF(VAL1.LT.0.0D0) VAL1=sys_last_surf()-VAL1
               IF(VAL1.LT.0.0.OR.VAL1.GT.sys_last_surf()) THEN
!       PIKUP REFERS TO A NON-EXISTING SURFACE
                  WRITE(OUTLYNE,*)'PIKUP REFERENCE TO A NON-EXISTING SURFACE'
                  CALL SHOWIT(1)
                  WRITE(OUTLYNE,*)'HAS BEEN REMOVED FROM CONFIGURATION #',I
                  CALL SHOWIT(1)
                  CONFG(I,J)=BLANK(1:140)
               END IF
            END IF
!
         END DO
      END DO
!       NOW WRITE TO SCRATH
      SCRCNT=0
      DO J=1,CFGCNT(I)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF(HOLDER.EQ.BLANK) GO TO 52
         SCRCNT=SCRCNT+1
         SCRATH(SCRCNT)=HOLDER
52       CONTINUE
      END DO
!
!       NOW WRITE SCRATH BACK INTO CONFIG ARRAY
!
      CONFG(I,1:SCRCNT)=SCRATH(1:SCRCNT)(1:140)
      CFGCNT(I)=SCRCNT
   END DO
   DEALLOCATE(SCRATH,STAT=ALLOERR)
   RETURN
END
! SUB AATEST.FOR
SUBROUTINE AATEST
!       THIS IS CALLED BY LORDER.FOR DURING CONFIG
!       FILE CLEANUP
!
   use DATCFG
   IMPLICIT NONE
!
   CHARACTER ATEST*17
!
   INTEGER AJJK,AI,AJ
!
   COMMON/AATST/AJJK,AI,AJ
!
!
   EE12=CONFG(AI,AJ)
   HOLDER=EE12
   ATEST=(HOLDER(1:17))
!
   IF(ATEST(1:2).EQ.'CV')AJJK=1
   IF(ATEST(1:2).EQ.'RD')AJJK=1
   IF(ATEST(1:3).EQ.'APY')AJJK=1
   IF(ATEST(1:3).EQ.'PIY')AJJK=1
   IF(ATEST(1:3).EQ.'PUY')AJJK=1
   IF(ATEST(1:4).EQ.'APCY')AJJK=1
   IF(ATEST(1:4).EQ.'PICY')AJJK=1
   IF(ATEST(1:4).EQ.'PUCY')AJJK=1
   IF(ATEST(1:4).EQ.'COCY')AJJK=1
   IF(ATEST(1:3).EQ.'CSD')AJJK=1
   IF(ATEST(1:6).EQ.'YTORIC')AJJK=9
   IF(ATEST(1:6).EQ.'XTORIC')AJJK=9
   IF(ATEST(1:11).EQ.'PIKUP    RD')AJJK=1
   IF(ATEST(1:11).EQ.'PIKUP    CV')AJJK=1
   IF(ATEST(1:11).EQ.'PIKD     RD')AJJK=1
   IF(ATEST(1:11).EQ.'PIKD     CV')AJJK=1
   IF(ATEST(1:2).EQ.'TH')AJJK=2
   IF(ATEST(1:2).EQ.'PY')AJJK=2
   IF(ATEST(1:3).EQ.'PCY')AJJK=2
   IF(ATEST(1:3).EQ.'CAY')AJJK=2
   IF(ATEST(1:2).EQ.'PX')AJJK=2
   IF(ATEST(1:3).EQ.'PCX')AJJK=2
   IF(ATEST(1:3).EQ.'CAX')AJJK=2
   IF(ATEST(1:3).EQ.'TSD')AJJK=2
   IF(ATEST(1:11).EQ.'PIKUP    TH')AJJK=2
   IF(ATEST(1:14).EQ.'PIKUP    THOAL')AJJK=2
   IF(ATEST(1:11).EQ.'PIKD     TH')AJJK=2
   IF(ATEST(1:14).EQ.'PIKD     THOAL')AJJK=2
   IF(ATEST(1:2).EQ.'CC')AJJK=3
   IF(ATEST(1:11).EQ.'PIKUP    CC')AJJK=3
   IF(ATEST(1:11).EQ.'PIKD     CC')AJJK=3
   IF(ATEST(1:4).EQ.'ASPH')AJJK=4
   IF(ATEST(1:5).EQ.'ASPHD')AJJK=4
   IF(ATEST(1:2).EQ.'AC')AJJK=29
   IF(ATEST(1:2).EQ.'AD')AJJK=5
   IF(ATEST(1:2).EQ.'AE')AJJK=6
   IF(ATEST(1:2).EQ.'AF')AJJK=7
   IF(ATEST(1:2).EQ.'AG')AJJK=8
   IF(ATEST(1:11).EQ.'PIKUP    AC')AJJK=29
   IF(ATEST(1:11).EQ.'PIKUP    AD')AJJK=5
   IF(ATEST(1:11).EQ.'PIKUP    AE')AJJK=6
   IF(ATEST(1:11).EQ.'PIKUP    AF')AJJK=7
   IF(ATEST(1:11).EQ.'PIKUP    AG')AJJK=8
   IF(ATEST(1:11).EQ.'PIKD     AC')AJJK=29
   IF(ATEST(1:11).EQ.'PIKD     AD')AJJK=5
   IF(ATEST(1:11).EQ.'PIKD     AE')AJJK=6
   IF(ATEST(1:11).EQ.'PIKD     AF')AJJK=7
   IF(ATEST(1:11).EQ.'PIKD     AG')AJJK=8
   IF(ATEST(1:3).EQ.'APX')AJJK=10
   IF(ATEST(1:3).EQ.'PIX')AJJK=10
   IF(ATEST(1:3).EQ.'PUX')AJJK=10
   IF(ATEST(1:4).EQ.'APCX')AJJK=10
   IF(ATEST(1:4).EQ.'PICX')AJJK=10
   IF(ATEST(1:4).EQ.'PUCX')AJJK=10
   IF(ATEST(1:4).EQ.'COCX')AJJK=10
   IF(ATEST(1:5).EQ.'CVTOR')AJJK=10
   IF(ATEST(1:5).EQ.'RDTOR')AJJK=10
   IF(ATEST(1:14).EQ.'PIKUP    CVTOR')AJJK=10
   IF(ATEST(1:14).EQ.'PIKD     RDTOR')AJJK=10
   IF(ATEST(1:14).EQ.'PIKD     CVTOR')AJJK=10
   IF(ATEST(1:14).EQ.'PIKUP    RDTOR')AJJK=10
   IF(ATEST(1:4).EQ.'CSDX')AJJK=10
   IF(ATEST(1:4).EQ.'CSDY')AJJK=10
   IF(ATEST(1:2).EQ.'YD')AJJK=12
   IF(ATEST(1:2).EQ.'XD')AJJK=13
   IF(ATEST(1:4).EQ.'TILT')AJJK=14
   IF(ATEST(1:5).EQ.'RTILT')AJJK=14
   IF(ATEST(1:5).EQ.'TILTD') AJJK=14
   IF(ATEST(1:5).EQ.'ALPHA')AJJK=15
   IF(ATEST(1:4).EQ.'BETA')AJJK=16
   IF(ATEST(1:5).EQ.'GAMMA')AJJK=17
   IF(ATEST(1:11).EQ.'PIKUP    YD')AJJK=12
   IF(ATEST(1:11).EQ.'PIKUP    XD')AJJK=13
   IF(ATEST(1:14).EQ.'PIKUP    ALPHA')AJJK=15
   IF(ATEST(1:13).EQ.'PIKUP    BETA')AJJK=16
   IF(ATEST(1:14).EQ.'PIKUP    GAMMA')AJJK=17
   IF(ATEST(1:11).EQ.'PIKD     YD')AJJK=12
   IF(ATEST(1:11).EQ.'PIKD     XD')AJJK=13
   IF(ATEST(1:14).EQ.'PIKD     ALPHA')AJJK=15
   IF(ATEST(1:13).EQ.'PIKD     BETA')AJJK=16
   IF(ATEST(1:14).EQ.'PIKD     GAMMA')AJJK=17
   IF(ATEST(1:5).EQ.'ASTOP')AJJK=18
   IF(ATEST(1:4).EQ.'REFS') AJJK=19
   IF(ATEST(1:4).EQ.'CLAP')AJJK=21
   IF(ATEST(1:4).EQ.'COBS')AJJK=22
   IF(ATEST(1:13).EQ.'PIKUP    CLAP')AJJK=21
   IF(ATEST(1:13).EQ.'PIKUP    COBS')AJJK=22
   IF(ATEST(1:13).EQ.'PIKD     CLAP')AJJK=21
   IF(ATEST(1:13).EQ.'PIKD     COBS')AJJK=22
   IF(ATEST(1:5).EQ.'CLAPD')AJJK=21
   IF(ATEST(1:5).EQ.'COBSD')AJJK=22
   IF(ATEST(1:5).EQ.'CCTOR')AJJK=23
   IF(ATEST(1:14).EQ.'PIKUP    CCTOR')AJJK=23
   IF(ATEST(1:14).EQ.'PIKD     CCTOR')AJJK=23
   IF(ATEST(1:5).EQ.'TASPH')AJJK=24
   IF(ATEST(1:6).EQ.'TASPHD')AJJK=24
   IF(ATEST(1:5).EQ.'ADTOR')AJJK=25
   IF(ATEST(1:5).EQ.'AETOR')AJJK=26
   IF(ATEST(1:5).EQ.'AFTOR')AJJK=27
   IF(ATEST(1:5).EQ.'AGTOR')AJJK=28
   IF(ATEST(1:14).EQ.'PIKUP    ADTOR')AJJK=25
   IF(ATEST(1:14).EQ.'PIKUP    AETOR')AJJK=26
   IF(ATEST(1:14).EQ.'PIKUP    AFTOR')AJJK=27
   IF(ATEST(1:14).EQ.'PIKUP    AGTOR')AJJK=28
   IF(ATEST(1:14).EQ.'PIKD     ADTOR')AJJK=25
   IF(ATEST(1:14).EQ.'PIKD     AETOR')AJJK=26
   IF(ATEST(1:14).EQ.'PIKD     AFTOR')AJJK=27
   IF(ATEST(1:14).EQ.'PIKD     AGTOR')AJJK=28
   IF(ATEST(1:5).EQ.'GLAK')AJJK=33
   IF(ATEST(1:5).EQ.'MODEL')AJJK=33
   IF(ATEST(1:7).EQ.'GLASS')AJJK=33
   IF(ATEST(1:6).EQ.'SCHOTT')AJJK=33
   IF(ATEST(1:7).EQ.'SCH2000')AJJK=33
   IF(ATEST(1:7).EQ.'RADHARD')AJJK=33
   IF(ATEST(1:4).EQ.'MATL')AJJK=33
   IF(ATEST(1:7).EQ.'RUSSIAN')AJJK=33
   IF(ATEST(1:5).EQ.'OHARA')AJJK=33
   IF(ATEST(1:4).EQ.'HOYA')AJJK=33
   IF(ATEST(1:6).EQ.'HIKARI')AJJK=33
   IF(ATEST(1:6).EQ.'CHANCE')AJJK=33
   IF(ATEST(1:6).EQ.'CORNIN')AJJK=33
   IF(ATEST(1:5).EQ.'GLCAT')AJJK=33
   IF(ATEST(1:4).EQ.'USER')AJJK=33
   IF(ATEST(1:3).EQ.'AIR')AJJK=33
   IF(ATEST(1:4).EQ.'REFL')AJJK=33
   IF(ATEST(1:7).EQ.'REFLTIR')AJJK=33
   IF(ATEST(1:8).EQ.'REFLTIRO')AJJK=33
   IF(ATEST(1:7).EQ.'PERFECT')AJJK=33
   IF(ATEST(1:5).EQ.'IDEAL')AJJK=33
   IF(ATEST(1:2).EQ.'N1')AJJK=34
   IF(ATEST(1:2).EQ.'N2')AJJK=35
   IF(ATEST(1:5).EQ.'INDEX')AJJK=34
   IF(ATEST(1:4).EQ.'VNUM')AJJK=35
   IF(ATEST(1:2).EQ.'N3')AJJK=36
   IF(ATEST(1:2).EQ.'N4')AJJK=37
   IF(ATEST(1:2).EQ.'N5')AJJK=38
   IF(ATEST(1:2).EQ.'N6')AJJK=40
   IF(ATEST(1:2).EQ.'N7')AJJK=41
   IF(ATEST(1:2).EQ.'N8')AJJK=42
   IF(ATEST(1:2).EQ.'N9')AJJK=43
   IF(ATEST(1:3).EQ.'N10')AJJK=44
   IF(ATEST(1:3).EQ.'INR')AJJK=45
   IF(ATEST(1:4).EQ.'INRD')AJJK=45
   IF(ATEST(1:14).EQ.'PIKUP    GLASS')AJJK=33
   IF(ATEST(1:14).EQ.'PIKD     GLASS')AJJK=33
   IF(ATEST(1:3).EQ.'LBL') AJJK=39
   IF(ATEST(1:5).EQ.'LABEL') AJJK=39
   IF(ATEST(1:5).EQ.'ASPH2') AJJK=46
   IF(ATEST(1:2).EQ.'AH') AJJK=47
   IF(ATEST(1:2).EQ.'AI') AJJK=48
   IF(ATEST(1:2).EQ.'AJ') AJJK=49
   IF(ATEST(1:2).EQ.'AK') AJJK=50
   IF(ATEST(1:2).EQ.'AL') AJJK=51
   IF(ATEST(1:11).EQ.'PIKD     AH')AJJK=47
   IF(ATEST(1:11).EQ.'PIKD     AI')AJJK=48
   IF(ATEST(1:11).EQ.'PIKD     AJ')AJJK=49
   IF(ATEST(1:11).EQ.'PIKD     AK')AJJK=50
   IF(ATEST(1:11).EQ.'PIKD     AL')AJJK=51
   IF(ATEST(1:5).EQ.'NODUM') AJJK=52
   IF(ATEST(1:2).EQ.'ZD') AJJK=53
   IF(ATEST(1:8).EQ.'FOOTBLOK') AJJK=54
   IF(ATEST(1:4).EQ.'PIVX') AJJK=55
   IF(ATEST(1:13).EQ.'PIKUP    PIVX')AJJK=55
   IF(ATEST(1:13).EQ.'PIKD     PIVX')AJJK=55
   IF(ATEST(1:4).EQ.'PIVY') AJJK=56
   IF(ATEST(1:13).EQ.'PIKUP    PIVY')AJJK=56
   IF(ATEST(1:13).EQ.'PIKD     PIVY')AJJK=56
   IF(ATEST(1:4).EQ.'PIVZ') AJJK=57
   IF(ATEST(1:13).EQ.'PIKUP    PIVZ')AJJK=57
   IF(ATEST(1:13).EQ.'PIKD     PIVZ')AJJK=57
   IF(ATEST(1:5).EQ.'DPART')AJJK=58
   IF(ATEST(1:3).EQ.'GDX')AJJK=59
   IF(ATEST(1:3).EQ.'GDY')AJJK=60
   IF(ATEST(1:3).EQ.'GDZ')AJJK=61
   IF(ATEST(1:6).EQ.'GALPHA')AJJK=62
   IF(ATEST(1:5).EQ.'GBETA')AJJK=63
   IF(ATEST(1:6).EQ.'GGAMMA')AJJK=64
   IF(ATEST(1:4).EQ.'GRTD')AJJK=65
   IF(ATEST(1:3).EQ.'GRT')AJJK=65
   IF(ATEST(1:3).EQ.'GRO')AJJK=66
   IF(ATEST(1:3).EQ.'GRS')AJJK=67
   IF(ATEST(1:3).EQ.'GRX')AJJK=68
   IF(ATEST(1:3).EQ.'GRY')AJJK=69
   IF(ATEST(1:3).EQ.'GRZ')AJJK=70
   IF(ATEST(1:5).EQ.'PIVOT')AJJK=71
   IF(ATEST(1:6).EQ.'PIVOTD')AJJK=71
   IF(ATEST(1:4).EQ.'SPGR')AJJK=72
   IF(ATEST(1:8).EQ.'DELDEFOR')AJJK=73
   IF(ATEST(1:6).EQ.'DEFORM')AJJK=73
   IF(ATEST(1:3).EQ.'THM')AJJK=74
   IF(ATEST(1:5).EQ.'PRICE')AJJK=75
   IF(ATEST(1:7).EQ.'COATING')AJJK=76
   IF(ATEST(1:16).EQ.'PIKUP    COATING')AJJK=76
   IF(ATEST(1:16).EQ.'PIKD     COATING')AJJK=76
   IF(ATEST(1:14).EQ.'PIVAXIS  LOCAL')AJJK=77
   IF(ATEST(1:15).EQ.'PIVAXIS  NORMAL')AJJK=77
   IF(ATEST(1:4).EQ.'REAL')AJJK=78
   IF(ATEST(1:5).EQ.'PARAX')AJJK=78
   IF(ATEST(1:5).EQ.'ARRAY')   AJJK=108
   IF(ATEST(1:6).EQ.'ARRAYD')  AJJK=108
   IF(ATEST(1:6).EQ.'SPIDER')  AJJK=109
   IF(ATEST(1:3).EQ.'ROO')AJJK=79
   IF(ATEST(1:4).EQ.'CCR')AJJK=79
   IF(ATEST(1:12).EQ.'FLDS     MAX')AJJK=110
   IF(ATEST(1:4).EQ.'FLDS'.AND.&
   &ATEST(1:12).NE.'FLDS     MAX')AJJK=111
   IF(ATEST(1:3).EQ.'ROO')AJJK=112
   IF(ATEST(1:3).EQ.'CCR')AJJK=113
   IF(ATEST(1:8).EQ.'RAYERROR')AJJK=114
   RETURN
END
