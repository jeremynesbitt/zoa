!       SECOND FILE OF CONFIGS FILES

! SUB CFSC2.FOR
SUBROUTINE CFSC2
!
   use DATCFG
   use DATLEN
   use DATMAI
   use mod_system, only: sys_high_cfg
   IMPLICIT NONE
!
!       THIS IS SUBROUTINE CFSC2 DOES SCALING OF CONFIG
!       DATA WHEN SAY,SAX,SCY,SCX,SCY FANG AND SCX FANG
!       ARE NOT SCALED WITH SURFACE DATA.
!
   CHARACTER HOLD*140,AVAL1*23,AN1*23,AV1*23,&
   &AVAL2*23,AVAL3*23,AVAL4*23,AVAL5*23,SNAME*9,&
   &LNAME*18
!
   INTEGER &
   &ULINE,ELINE,IEND,CHG1,ISTART,ISTOP,STARL,STOPL,ISTA,&
   &ISTO,I,J
!
   REAL*8 VAL1,VAL2,N1,V1,MM1,&
   &VAL3,VAL4,VAL5
!
   COMMON/CAUX1/N1,AN1
!
   COMMON/JK_NTA3/V1,AV1
!
!
!       STRAIGHT SCALING (SC,FACT,I,J)
!       W1=FACTOR
!       W2=STARTING SURFACE
!       W3=ENDING SURFACE
!
!       I TRACKS THE CONFIGURATION BEING HANDLED
!
!       J TRACKS THE ENTRY NUMBER
!
   IEND=INT(sys_high_cfg())
   DO 10 I=2,IEND
!       LOOP THROUGH ALL NON BLANK CONFIGS
      ELINE=1
      ULINE=0
!
!       REMOVE ALL FNBY/ER/MAG ENTRIES BY CALLIN FNBDE
      CALL FNBDE(I)
!
!       DETERMINE LOCATION OF U L,EOS AND FIRST CHG
      DO 15 J=1,CFGCNT(I)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:10)).EQ.'U        L'.OR.&
         &(HOLDER(1:13)).EQ.'UPDATE   LENS'.OR.&
         &(HOLDER(1:10)).EQ.'UPDATE   L'.OR.&
         &(HOLDER(1:13)).EQ.'U        LENS') ULINE=J
         IF((HOLDER(1:3)).EQ.'EOS'.AND.J.GT.ULINE)THEN
            ELINE=J
            GO TO 16
         ELSE
         END IF
15    CONTINUE
16    CONTINUE
      CHG1=ELINE
      DO 20 J=(ULINE+1),(ELINE-1)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:3)).EQ.'CHG') THEN
            CHG1=J
            GO TO 21
         ELSE
         END IF
20    CONTINUE
21    CONTINUE
!
!       NOW SURFACE DATA. WE SCALE ALL SURFACE DATA BETWEEN
!       SURFACE W2 AND SURFACE W3 AS INPUT IN THE SCALING COMMAND
!
      ISTART=INT(W2)
      ISTOP= INT(W3)
      ISTA=0
      ISTO=0
!
!       THE LINES BETWEEN WHICH SCALING WILL OCCUR ARE
!       NOW DETERMINED.
!       SEARCH FOR THE STARTING LINE.(STARL)
      DO 100 J=CHG1,(ELINE-1)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:3)).EQ.'CHG') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            IF(INT(VAL1).LT.ISTART) GO TO 100
            IF(INT(VAL1).GE.ISTART) THEN
               IF(ISTA.EQ.0) THEN
                  ISTA=1
                  STARL=J+1
!       STARTING LINE STARL ASSIGNED
                  GO TO 101
               ELSE
!       ISTA NOT 0, STARL ALREADY ASSIGNED
               END IF
            ELSE
            END IF
         ELSE
!       NOT CHG, PROCEED
         END IF
!
100   CONTINUE
!       IF GOT HERE, DID NOT FIND STARTING LINE AND THEREFORE
!       NO ENDING LINE EXISTS SO NOTHING TO SCALE. GO TO 10
!       AND CHECK NEXT CONFIG
      GO TO 10
101   CONTINUE
!       SEARCH FOR THE ENDING LINE.(ENDL)
      DO 200 J=STARL,(ELINE-1)
         EE12=CONFG(I,J)
         HOLDER=EE12
         IF((HOLDER(1:3)).EQ.'CHG') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            IF(INT(VAL1).LE.ISTOP) GO TO 200
            IF(INT(VAL1).GT.ISTOP) THEN
               IF(ISTO.EQ.0) THEN
                  ISTO=1
                  STOPL=J-1
!       HALTING LINE STOPL ASSIGNED
                  GO TO 201
               ELSE
!       ISTO NOT 0
               END IF
            ELSE
            END IF
         ELSE
!       NOT CHG, PROCEED
         END IF
!
200   CONTINUE
!       IF GOT HERE, DID NOT FIND HALTING LINE AND THEREFORE
!       HALTING LINE IS ELINE-1
!                       ISTO=1
      STOPL=(ELINE-1)
201   CONTINUE
!
!       PROCEED TO SCALE NOW
      DO 300 J=STARL,STOPL
         EE12=CONFG(I,J)
         HOLDER=EE12
!
!       SCALE ALL APPROPRIATE SURFACE ENTRYS AND REMEMBER TO SKIP
!       INTERMEDIATE CHG STATMENTS.
!               SKIP CHG STATMENTS
         IF((HOLDER(1:3)).EQ.'CHG') GO TO 300
!
!       SCALE (CV)
         IF((HOLDER(1:2)).EQ.'CV') THEN
            AVAL1=(HOLDER(10:32))
            HOLD(1:140)=HOLDER(33:140)
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='CV      ,'//AVAL1//HOLD(1:100)
            GO TO 299
         ELSE
!       NOT CV
         END IF
!       SCALE (RD)
         IF((HOLDER(1:2)).EQ.'RD') THEN
            AVAL1=(HOLDER(10:32))
            HOLD(1:140)=HOLDER(33:140)
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='RD      ,'//AVAL1//HOLD(1:100)
            GO TO 299
         ELSE
!       NOT RD
         END IF
!       SCALE (TH)
         IF((HOLDER(1:2)).EQ.'TH') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='TH      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (THM)
         IF((HOLDER(1:3)).EQ.'THM') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='THM     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AC)
         IF((HOLDER(1:2)).EQ.'AC') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AC      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AD)
         IF((HOLDER(1:2)).EQ.'AD') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**3)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AD      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (ADTOR)
         IF((HOLDER(1:5)).EQ.'ADTOR') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**3)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='ADTOR   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AE)
         IF((HOLDER(1:2)).EQ.'AE') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**5)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AE      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (AETOR)
         IF((HOLDER(1:5)).EQ.'AETOR') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**5)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AETOR   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AF)
         IF((HOLDER(1:2)).EQ.'AF') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**7)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AF      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (AFTOR)
         IF((HOLDER(1:5)).EQ.'AFTOR') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**7)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AFTOR   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AG)
         IF((HOLDER(1:2)).EQ.'AG') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**9)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AG      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (AGTOR)
         IF((HOLDER(1:5)).EQ.'AGTOR') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**9)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AGTOR   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AH)
         IF((HOLDER(1:2)).EQ.'AH') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**11)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AH      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AI)
         IF((HOLDER(1:2)).EQ.'AI') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**13)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AI      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AJ)
         IF((HOLDER(1:2)).EQ.'AJ') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**15)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AJ      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AK)
         IF((HOLDER(1:2)).EQ.'AK') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**17)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AK      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (AL)
         IF((HOLDER(1:2)).EQ.'AL') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/(W1**19)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='AL      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (ASPH)
         IF((HOLDER(1:4)).EQ.'ASPH') THEN
            HOLDER=HOLDER(10:140)
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL1=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL1=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL2=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL2=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL3=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL3=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL4=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL4=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL5=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL5=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF(AVAL1.NE.',') THEN
               AV1=AVAL1
               CALL ATON3
               VAL1=V1
               VAL1=VAL1/(W1**3)
               V1=VAL1
               CALL NTOA3
               AVAL1=AV1
            ELSE
            END IF
!
            IF(AVAL2.NE.',') THEN
               AV1=AVAL2
               CALL ATON3
               VAL2=V1
               VAL2=VAL2/(W1**5)
               V1=VAL2
               CALL NTOA3
               AVAL2=AV1
            ELSE
            END IF
!
            IF(AVAL3.NE.',') THEN
               AV1=AVAL3
               CALL ATON3
               VAL3=V1
               VAL3=VAL3/(W1**7)
               V1=VAL3
               CALL NTOA3
               AVAL3=AV1
            ELSE
            END IF
!
            IF(AVAL4.NE.',') THEN
               AV1=AVAL4
               CALL ATON3
               VAL4=V1
               VAL4=VAL4/(W1**9)
               V1=VAL4
               CALL NTOA3
               AVAL4=AV1
            ELSE
            END IF
!
            IF(AVAL5.NE.',') THEN
               AV1=AVAL5
               CALL ATON3
               VAL5=V1
               VAL5=VAL5/(W1)
               V1=VAL5
               CALL NTOA3
               AVAL5=AV1
            ELSE
            END IF
!       RE-CONSTRUCT HOLDER
            HOLDER='ASPH    ,'
            IF(AVAL1.NE.',') THEN
               HOLDER=HOLDER//AVAL1//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL2.NE.',') THEN
               HOLDER=HOLDER//AVAL2//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL3.NE.',') THEN
               HOLDER=HOLDER//AVAL3//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL4.NE.',') THEN
               HOLDER=HOLDER//AVAL4//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL5.NE.',') THEN
               HOLDER=HOLDER//AVAL5//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            GO TO 299
         ELSE
         END IF
!
!       SCALE (ASPH2)
         IF((HOLDER(1:4)).EQ.'ASPH2') THEN
            HOLDER=HOLDER(10:140)
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL1=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL1=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL2=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL2=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL3=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL3=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL4=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL4=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL5=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL5=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF(AVAL1.NE.',') THEN
               AV1=AVAL1
               CALL ATON3
               VAL1=V1
               VAL1=VAL1/(W1**11)
               V1=VAL1
               CALL NTOA3
               AVAL1=AV1
            ELSE
            END IF
!
            IF(AVAL2.NE.',') THEN
               AV1=AVAL2
               CALL ATON3
               VAL2=V1
               VAL2=VAL2/(W1**13)
               V1=VAL2
               CALL NTOA3
               AVAL2=AV1
            ELSE
            END IF
!
            IF(AVAL3.NE.',') THEN
               AV1=AVAL3
               CALL ATON3
               VAL3=V1
               VAL3=VAL3/(W1**15)
               V1=VAL3
               CALL NTOA3
               AVAL3=AV1
            ELSE
            END IF
!
            IF(AVAL4.NE.',') THEN
               AV1=AVAL4
               CALL ATON3
               VAL4=V1
               VAL4=VAL4/(W1**17)
               V1=VAL4
               CALL NTOA3
               AVAL4=AV1
            ELSE
            END IF
!
            IF(AVAL5.NE.',') THEN
               AV1=AVAL5
               CALL ATON3
               VAL5=V1
               VAL5=VAL5/(W1**19)
               V1=VAL5
               CALL NTOA3
               AVAL5=AV1
            ELSE
            END IF
!       RE-CONSTRUCT HOLDER
            HOLDER='ASPH2   ,'
            IF(AVAL1.NE.',') THEN
               HOLDER=HOLDER//AVAL1//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL2.NE.',') THEN
               HOLDER=HOLDER//AVAL2//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL3.NE.',') THEN
               HOLDER=HOLDER//AVAL3//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL4.NE.',') THEN
               HOLDER=HOLDER//AVAL4//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL5.NE.',') THEN
               HOLDER=HOLDER//AVAL5//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            GO TO 299
         ELSE
         END IF
!       SCALE (TASPH)
         IF((HOLDER(1:5)).EQ.'TASPH') THEN
            HOLDER=HOLDER(10:140)
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL1=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL1=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL2=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL2=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL3=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL3=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL4=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL4=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL5=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL5=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF(AVAL1.NE.',') THEN
               AV1=AVAL1
               CALL ATON3
               VAL1=V1
               VAL1=VAL1/(W1**3)
               V1=VAL1
               CALL NTOA3
               AVAL1=AV1
            ELSE
            END IF
!
            IF(AVAL2.NE.',') THEN
               AV1=AVAL2
               CALL ATON3
               VAL2=V1
               VAL2=VAL2/(W1**5)
               V1=VAL2
               CALL NTOA3
               AVAL2=AV1
            ELSE
            END IF
!
            IF(AVAL3.NE.',') THEN
               AV1=AVAL3
               CALL ATON3
               VAL3=V1
               VAL3=VAL3/(W1**7)
               V1=VAL3
               CALL NTOA3
               AVAL3=AV1
            ELSE
            END IF
!
            IF(AVAL4.NE.',') THEN
               AV1=AVAL4
               CALL ATON3
               VAL4=V1
               VAL4=VAL4/(W1**9)
               V1=VAL4
               CALL NTOA3
               AVAL4=AV1
            ELSE
            END IF
!
!       RE-CONSTRUCT HOLDER
            HOLDER='TASPH   ,'
            IF(AVAL1.NE.',') THEN
               HOLDER=HOLDER//AVAL1//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL2.NE.',') THEN
               HOLDER=HOLDER//AVAL2//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL3.NE.',') THEN
               HOLDER=HOLDER//AVAL3//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL4.NE.',') THEN
               HOLDER=HOLDER//AVAL4//',,'
            ELSE
               HOLDER=HOLDER//',,'
            END IF
            GO TO 299
         ELSE
         END IF
!
!       SCALE (CLAP OR COBS)
         MM1=DABS(W1)
!
         IF((HOLDER(1:4)).EQ.'CLAP'.OR.&
         &(HOLDER(1:4)).EQ.'COBS') THEN
!
            IF((HOLDER(9:9)).EQ.',') THEN
               SNAME=HOLDER(1:9)
               LNAME='                  '
               HOLDER=HOLDER(10:140)
            ELSE
               SNAME='         '
               LNAME=HOLDER(1:18)
               HOLDER=HOLDER(19:140)
            END IF
!       RESOLVE NUMERIC VALUES
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL1=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL1=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL2=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL2=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL3=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL3=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL4=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL4=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL5=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL5=','
               HOLDER=HOLDER(2:140)
            END IF
!
!       NOW RESOLVE SCALING (DEPENDING ON QUALIFIER VALUE)
!
!       NO QUALIFIER JUST CLAP OR COBS
!
            IF(LNAME(1:6).EQ.'      ') THEN
!       JUST CLAP OR COBS, SCALE ALL VALUES
               IF(AVAL1.NE.',') THEN
                  AV1=AVAL1
                  CALL ATON3
                  VAL1=V1
                  VAL1=VAL1*MM1
                  V1=VAL1
                  CALL NTOA3
                  AVAL1=AV1
               ELSE
               END IF
               IF(AVAL2.NE.',') THEN
                  AV1=AVAL2
                  CALL ATON3
                  VAL2=V1
                  VAL2=VAL2*MM1
                  V1=VAL2
                  CALL NTOA3
                  AVAL2=AV1
               ELSE
               END IF
               IF(AVAL3.NE.',') THEN
                  AV1=AVAL3
                  CALL ATON3
                  VAL3=V1
                  VAL3=VAL3*MM1
                  V1=VAL3
                  CALL NTOA3
                  AVAL3=AV1
               ELSE
               END IF
               AVAL4=','
               AVAL5=','
!
!       RE-CONSTRUCT HOLDER
!
               HOLDER=SNAME
               IF(AVAL1.NE.',') THEN
                  HOLDER=HOLDER//AVAL1//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL2.NE.',') THEN
                  HOLDER=HOLDER//AVAL2//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL3.NE.',') THEN
                  HOLDER=HOLDER//AVAL3//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL4.NE.',') THEN
                  HOLDER=HOLDER//AVAL4//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL5.NE.',') THEN
                  HOLDER=HOLDER//AVAL5//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
            ELSE
!       LNAME WAS NOT BLANK, THERE MUST HAVE BEEN A QUALIFIER
!       HANDEL IT
!       BREAK OUT THE QUALIFIER PART OR LNAME
!       WHICH IS LNAME(10:17) AND DECIDE WHAT TO DO IN TERMS
!       OF SCALING. THEN RE-CONSTRUCT HOLDER
!
!       LNAME(10:17)= 'ERASE   '
               IF(LNAME(10:17).EQ.'ERASE   ') THEN
!       SAME SCALING AS CLAP OR COBS.
                  IF(AVAL1.NE.',') THEN
                     AV1=AVAL1
                     CALL ATON3
                     VAL1=V1
                     VAL1=VAL1*MM1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                  ELSE
                  END IF
                  IF(AVAL2.NE.',') THEN
                     AV1=AVAL2
                     CALL ATON3
                     VAL2=V1
                     VAL2=VAL2*MM1
                     V1=VAL2
                     CALL NTOA3
                     AVAL2=AV1
                  ELSE
                  END IF
                  IF(AVAL3.NE.',') THEN
                     AV1=AVAL3
                     CALL ATON3
                     VAL3=V1
                     VAL3=VAL3*MM1
                     V1=VAL3
                     CALL NTOA3
                     AVAL3=AV1
                  ELSE
                  END IF
                  AVAL4=','
                  AVAL5=','
               ELSE
!       NOT 'ERASE'
               END IF
!
!       LNAME(10:17)= 'RECT    ' OR 'RECTE'
               IF(LNAME(10:17).EQ.'RECT    '.OR.LNAME(10:17)&
               &.EQ.'RECTE   ') THEN
                  IF(AVAL1.NE.',') THEN
                     AV1=AVAL1
                     CALL ATON3
                     VAL1=V1
                     VAL1=VAL1*MM1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                  ELSE
                  END IF
                  IF(AVAL2.NE.',') THEN
                     AV1=AVAL2
                     CALL ATON3
                     VAL2=V1
                     VAL2=VAL2*MM1
                     V1=VAL2
                     CALL NTOA3
                     AVAL2=AV1
                  ELSE
                  END IF
                  IF(AVAL3.NE.',') THEN
                     AV1=AVAL3
                     CALL ATON3
                     VAL3=V1
                     VAL3=VAL3*MM1
                     V1=VAL3
                     CALL NTOA3
                     AVAL3=AV1
                  ELSE
                  END IF
                  IF(AVAL4.NE.',') THEN
                     AV1=AVAL4
                     CALL ATON3
                     VAL4=V1
                     VAL4=VAL4*MM1
                     V1=VAL4
                     CALL NTOA3
                     AVAL4=AV1
                  ELSE
                  END IF
                  AVAL5=','
               ELSE
!       NOT 'RECT'
               END IF
!
!       LNAME(10:17)= 'ELIP    ' OR 'ELIPE'
               IF(LNAME(10:17).EQ.'ELIP    '.OR.LNAME(10:17)&
               &.EQ.'ELIPE   ') THEN
                  IF(AVAL1.NE.',') THEN
                     AV1=AVAL1
                     CALL ATON3
                     VAL1=V1
                     VAL1=VAL1*MM1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                  ELSE
                  END IF
                  IF(AVAL2.NE.',') THEN
                     AV1=AVAL2
                     CALL ATON3
                     VAL2=V1
                     VAL2=VAL2*MM1
                     V1=VAL2
                     CALL NTOA3
                     AVAL2=AV1
                  ELSE
                  END IF
                  IF(AVAL3.NE.',') THEN
                     AV1=AVAL3
                     CALL ATON3
                     VAL3=V1
                     VAL3=VAL3*MM1
                     V1=VAL3
                     CALL NTOA3
                     AVAL3=AV1
                  ELSE
                  END IF
                  IF(AVAL4.NE.',') THEN
                     AV1=AVAL4
                     CALL ATON3
                     VAL4=V1
                     VAL4=VAL4*MM1
                     V1=VAL4
                     CALL NTOA3
                     AVAL4=AV1
                  ELSE
                  END IF
                  AVAL5=','
               ELSE
!       NOT 'ELIP'
               END IF
!
!       LNAME(10:17)=RCTK OR RCTKE
               IF(LNAME(10:17).EQ.'RCTK    '.OR.&
               &LNAME(10:17).EQ.'RCTKE   ') THEN
                  IF(AVAL1.NE.',') THEN
                     AV1=AVAL1
                     CALL ATON3
                     VAL1=V1
                     VAL1=VAL1*MM1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                  ELSE
                  END IF
                  IF(AVAL2.NE.',') THEN
                     AV1=AVAL2
                     CALL ATON3
                     VAL2=V1
                     VAL2=VAL2*MM1
                     V1=VAL2
                     CALL NTOA3
                     AVAL2=AV1
                  ELSE
                  END IF
                  IF(AVAL3.NE.',') THEN
                     AV1=AVAL3
                     CALL ATON3
                     VAL3=V1
                     VAL3=VAL3*MM1
                     V1=VAL3
                     CALL NTOA3
                     AVAL3=AV1
                  ELSE
                  END IF
                  IF(AVAL4.NE.',') THEN
                     AV1=AVAL4
                     CALL ATON3
                     VAL4=V1
                     VAL4=VAL4*MM1
                     V1=VAL4
                     CALL NTOA3
                     AVAL4=AV1
                  ELSE
                  END IF
                  IF(AVAL5.NE.',') THEN
                     AV1=AVAL5
                     CALL ATON3
                     VAL5=V1
                     VAL5=VAL5*MM1
                     V1=VAL5
                     CALL NTOA3
                     AVAL5=AV1
                  ELSE
                  END IF
               ELSE
!       NOT RCTK OR RCTKE
               END IF
!
!       LNAME(10:17)=POLY OR POLYE
               IF(LNAME(10:17).EQ.'POLY    '.OR.&
               &LNAME(10:17).EQ.'POLYE   ') THEN
                  IF(AVAL1.NE.',') THEN
                     AV1=AVAL1
                     CALL ATON3
                     VAL1=V1
                     VAL1=VAL1*MM1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                  ELSE
                  END IF
                  IF(AVAL2.NE.',') THEN
                     AV1=AVAL2
                     CALL ATON3
                     VAL2=V1
                     VAL2=VAL2
                     V1=VAL2
                     CALL NTOA3
                     AVAL2=AV1
                  ELSE
                  END IF
                  IF(AVAL3.NE.',') THEN
                     AV1=AVAL3
                     CALL ATON3
                     VAL3=V1
                     VAL3=VAL3*MM1
                     V1=VAL3
                     CALL NTOA3
                     AVAL3=AV1
                  ELSE
                  END IF
                  IF(AVAL4.NE.',') THEN
                     AV1=AVAL4
                     CALL ATON3
                     VAL4=V1
                     VAL4=VAL4*MM1
                     V1=VAL4
                     CALL NTOA3
                     AVAL4=AV1
                  ELSE
                  END IF
                  IF(AVAL5.NE.',') THEN
                     AV1=AVAL5
                     CALL ATON3
                     VAL5=V1
                     VAL5=VAL5
                     V1=VAL5
                     CALL NTOA3
                     AVAL5=AV1
                  ELSE
                  END IF
               ELSE
!       NOT POLY OR POLYE
               END IF
!
!       LNAME(10:17)=IPOLY OR IPOLYE
               IF(LNAME(10:17).EQ.'IPOLY   '.OR.&
               &LNAME(10:17).EQ.'IPOLYE  ') THEN
                  IF(AVAL1.NE.',') THEN
                     AV1=AVAL1
                     CALL ATON3
                     VAL1=V1
                     VAL1=VAL1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                     V1=VAL1
                     CALL NTOA3
                     AVAL1=AV1
                  ELSE
                  END IF
                  IF(AVAL2.NE.',') THEN
                     AV1=AVAL2
                     CALL ATON3
                     VAL2=V1
                     VAL2=VAL2
                     V1=VAL2
                     CALL NTOA3
                     AVAL2=AV1
                  ELSE
                  END IF
                  IF(AVAL3.NE.',') THEN
                     AV1=AVAL3
                     CALL ATON3
                     VAL3=V1
                     VAL3=VAL3*MM1
                     V1=VAL3
                     CALL NTOA3
                     AVAL3=AV1
                  ELSE
                  END IF
                  IF(AVAL4.NE.',') THEN
                     AV1=AVAL4
                     CALL ATON3
                     VAL4=V1
                     VAL4=VAL4*MM1
                     V1=VAL4
                     CALL NTOA3
                     AVAL4=AV1
                  ELSE
                  END IF
                  IF(AVAL5.NE.',') THEN
                     AV1=AVAL5
                     CALL ATON3
                     VAL5=V1
                     VAL5=VAL5*MM1
                     V1=VAL5
                     CALL NTOA3
                     AVAL5=AV1
                  ELSE
                  END IF
               ELSE
!       NOT IPOLY OR IPOLYE
               END IF
!
!       RE-CONSTRUCT HOLDER
!
               HOLDER=LNAME
               IF(AVAL1.NE.',') THEN
                  HOLDER=HOLDER//AVAL1//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL2.NE.',') THEN
                  HOLDER=HOLDER//AVAL2//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL3.NE.',') THEN
                  HOLDER=HOLDER//AVAL3//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL4.NE.',') THEN
                  HOLDER=HOLDER//AVAL4//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
               IF(AVAL5.NE.',') THEN
                  HOLDER=HOLDER//AVAL5//','
               ELSE
                  HOLDER=HOLDER//','
               END IF
            END IF
            GO TO 299
         ELSE
!       NOT CLAP OR COBS, PROCEED
         END IF
!
!       NOW TORIC CURVATUURE AND RADIUS
!       SCALE (CVTOR)
         IF((HOLDER(1:5)).EQ.'CVTOR') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1/W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='CVTOR   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (RDTOR)
         IF((HOLDER(1:5)).EQ.'RDTOR') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='RDTOR   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (DEC)
         IF((HOLDER(1:3)).EQ.'DEC') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            AVAL2=(HOLDER(34:56))
            AV1=AVAL2
            CALL ATON3
            VAL2=V1
            VAL2=VAL2*W1
            V1=VAL2
            CALL NTOA3
            AVAL2=AV1
            AVAL3=(HOLDER(58:80))
            AV1=AVAL3
            CALL ATON3
            VAL3=V1
            VAL3=VAL3*W1
            V1=VAL3
            CALL NTOA3
            AVAL3=AV1
            HOLDER='DEC     ,'//AVAL1//','//AVAL2//','//AVAL3//',,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (PIVOT)
         IF((HOLDER(1:5)).EQ.'PIVOT') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            AVAL2=(HOLDER(34:56))
            AV1=AVAL2
            CALL ATON3
            VAL2=V1
            VAL2=VAL2*W1
            V1=VAL2
            CALL NTOA3
            AVAL2=AV1
            AVAL3=(HOLDER(58:80))
            AV1=AVAL3
            CALL ATON3
            VAL3=V1
            VAL3=VAL3*W1
            V1=VAL3
            CALL NTOA3
            AVAL3=AV1
            HOLDER='PIVOT   ,'//AVAL1//','//AVAL2//','//AVAL3//',,,'
            GO TO 299
         ELSE
         END IF
!       SCALE (PY)
         IF((HOLDER(1:2)).EQ.'PY') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PY      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (PX)
         IF((HOLDER(1:2)).EQ.'PX') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PX      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (PCY)
         IF((HOLDER(1:3)).EQ.'PCY') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PCY     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (PCX)
         IF((HOLDER(1:3)).EQ.'PCX') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PCX     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (CAY)
         IF((HOLDER(1:3)).EQ.'CAY') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='CAY     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (CAX)
         IF((HOLDER(1:3)).EQ.'CAX') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='CAX     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (XD)
         IF((HOLDER(1:2)).EQ.'XD') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*DABS(W1)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='XD      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (YD)
         IF((HOLDER(1:2)).EQ.'YD') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*DABS(W1)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='YD      ,'//AVAL1//',,,,,'
         ELSE
         END IF
!
!       SCALE (ZD)
         IF((HOLDER(1:2)).EQ.'ZD') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='ZD      ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (GDX)
         IF((HOLDER(1:3)).EQ.'GDX') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*DABS(W1)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='GDX     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (GDY)
         IF((HOLDER(1:3)).EQ.'GDY') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*DABS(W1)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='GDY     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (GDZ)
         IF((HOLDER(1:3)).EQ.'GDZ') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='GDZ     ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (ALPHA,BETA,GAMMA,GALPHA,GBETA,GGAMMA)
         IF((HOLDER(1:5)).EQ.'ALPHA'.OR.&
         &(HOLDER(1:4)).EQ.'BETA'.OR.&
         &(HOLDER(1:5)).EQ.'GAMMA'.OR.&
         &(HOLDER(1:6)).EQ.'GALPHA'.OR.&
         &(HOLDER(1:5)).EQ.'GBETA'.OR.&
         &(HOLDER(1:6)).EQ.'GGAMMA') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=-VAL1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            IF(HOLDER(1:5).EQ.'ALPHA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
            IF(HOLDER(1:4).EQ.'BETA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
            IF(HOLDER(1:5).EQ.'GAMMA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
            IF(HOLDER(1:6).EQ.'GALPHA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
            IF(HOLDER(1:5).EQ.'GBETA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
            IF(HOLDER(1:6).EQ.'GGAMMA') HOLDER='ALPHA   ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (PIVX)
         IF((HOLDER(1:4)).EQ.'PIVX') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*DABS(W1)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PIVX    ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (PIVY)
         IF((HOLDER(1:4)).EQ.'PIVY') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*DABS(W1)
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PIVY    ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       SCALE (PIVZ)
         IF((HOLDER(1:4)).EQ.'PIVZ') THEN
            AVAL1=(HOLDER(10:32))
            AV1=AVAL1
            CALL ATON3
            VAL1=V1
            VAL1=VAL1*W1
            V1=VAL1
            CALL NTOA3
            AVAL1=AV1
            HOLDER='PIVZ    ,'//AVAL1//',,,,,'
            GO TO 299
         ELSE
         END IF
!
!       NOW WE MUST SCALE ALL PIKUPS CORRECTLY
!
!       SCALE (PIKUP)S AS APPROPRIATE
!
         IF((HOLDER(1:5)).EQ.'PIKUP')THEN
            IF((HOLDER(10:17)).EQ.'RD      '.OR.&
            &(HOLDER(10:17)).EQ.'CV      '.OR.&
            &(HOLDER(10:17)).EQ.'TH      '.OR.&
            &(HOLDER(10:17)).EQ.'AC      '.OR.&
            &(HOLDER(10:17)).EQ.'AD      '.OR.&
            &(HOLDER(10:17)).EQ.'AE      '.OR.&
            &(HOLDER(10:17)).EQ.'AF      '.OR.&
            &(HOLDER(10:17)).EQ.'AG      '.OR.&
            &(HOLDER(10:17)).EQ.'AH      '.OR.&
            &(HOLDER(10:17)).EQ.'AI      '.OR.&
            &(HOLDER(10:17)).EQ.'AJ      '.OR.&
            &(HOLDER(10:17)).EQ.'AK      '.OR.&
            &(HOLDER(10:17)).EQ.'AL      ') THEN
               GO TO 1000
            ELSE
            END IF
            IF((HOLDER(10:17)).EQ.'ADTOR   '.OR.&
            &(HOLDER(10:17)).EQ.'AETOR   '.OR.&
            &(HOLDER(10:17)).EQ.'AFTOR   '.OR.&
            &(HOLDER(10:17)).EQ.'AGTOR   '.OR.&
            &(HOLDER(10:17)).EQ.'YD      '.OR.&
            &(HOLDER(10:17)).EQ.'ZD      '.OR.&
            &(HOLDER(10:17)).EQ.'PIVX    '.OR.&
            &(HOLDER(10:17)).EQ.'PIVY    '.OR.&
            &(HOLDER(10:17)).EQ.'PIVZ    '.OR.&
            &(HOLDER(10:17)).EQ.'GDX     '.OR.&
            &(HOLDER(10:17)).EQ.'GDY     '.OR.&
            &(HOLDER(10:17)).EQ.'GDZ     '.OR.&
            &(HOLDER(10:17)).EQ.'ALPHA   '.OR.&
            &(HOLDER(10:17)).EQ.'BETA    '.OR.&
            &(HOLDER(10:17)).EQ.'GAMMA   '.OR.&
            &(HOLDER(10:17)).EQ.'GALPHA  '.OR.&
            &(HOLDER(10:17)).EQ.'GBETA   '.OR.&
            &(HOLDER(10:17)).EQ.'GGAMMA  '.OR.&
            &(HOLDER(10:17)).EQ.'XD      ')THEN
               GO TO 1000
            ELSE
            END IF
            IF((HOLDER(10:17)).EQ.'CLAP    '.OR.&
            &(HOLDER(10:17)).EQ.'COBS    '.OR.&
            &(HOLDER(10:17)).EQ.'RDTOR   '.OR.&
            &(HOLDER(10:17)).EQ.'CVTOR   ')THEN
!
!       FOUND PIKUP TO SCALE, JUMP TO 1000
               GO TO 1000
            ELSE
!       DON'T SCALE PIKUP JUST JUMP TO 300
!
            END IF
1000        CONTINUE
            LNAME=HOLDER(1:18)
            HOLDER=HOLDER(19:140)
!
!       RESOLVE NUMERIC VALUES
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL1=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL1=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL2=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL2=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL3=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL3=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL4=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL4=','
               HOLDER=HOLDER(2:140)
            END IF
!
            IF((HOLDER(1:1)).NE.',') THEN
               AVAL5=(HOLDER(1:23))
               HOLDER=HOLDER(24:140)
            ELSE
               AVAL5=','
               HOLDER=HOLDER(2:140)
            END IF
!
!       THE ALPHA STRING REPRESENTATIONS ARE NOW BROKEN OUT
!       AS AVAL1 TO AVAL5
!
            IF(AVAL4.NE.',') THEN
!       MAYBE AVAL4 IS 1.0 AND WE HAVE 'SPECIAL' PIKUP OPTION
!       IN WHICH CASE NO SCALEING OCCURS.
               AV1=AVAL4
               CALL ATON3
               VAL4=V1
               IF(VAL4.EQ.1.0D0) THEN
!       CASE OF 'SPECIAL' PIKUP OPTION, NO SCALING, JUMP TO 300
                  GO TO 300
!               ELSE
!       VAL4 NOT 1.0, JUST CONTINUE
               END IF
            ELSE
!       AVAL4 IS DEFAULT, CONTINUE
            END IF

!       WE ONLY SCALE THE THIRD NUMERIC WORD IF ANY
!
!       LNAME(10:17)= RD,TH,XD,YD,RDTOR,PIVX,PIVY,PIVZ
!       LNAME(10:17)= OR GLOBAL TILTS AND DECENTERS
            IF(LNAME(10:17).EQ.'RD      '.OR.&
            &LNAME(10:17).EQ.'RDTOR   '.OR.&
            &LNAME(10:17).EQ.'XD      '.OR.&
            &LNAME(10:17).EQ.'ZD      '.OR.&
            &LNAME(10:17).EQ.'YD      '.OR.&
            &LNAME(10:17).EQ.'GDX     '.OR.&
            &LNAME(10:17).EQ.'GDY     '.OR.&
            &LNAME(10:17).EQ.'GDZ     '.OR.&
            &LNAME(10:17).EQ.'ALPHA   '.OR.&
            &LNAME(10:17).EQ.'BETA    '.OR.&
            &LNAME(10:17).EQ.'GAMMA   '.OR.&
            &LNAME(10:17).EQ.'GALPHA  '.OR.&
            &LNAME(10:17).EQ.'GBETA   '.OR.&
            &LNAME(10:17).EQ.'GGAMMA  '.OR.&
            &LNAME(10:17).EQ.'PIVX    '.OR.&
            &LNAME(10:17).EQ.'PIVY    '.OR.&
            &LNAME(10:17).EQ.'PIVZ    '.OR.&
            &LNAME(10:17).EQ.'TH      ') THEN
!       LINEAR SCALING
               IF(AVAL4.NE.',') THEN
                  AV1=AVAL4
                  CALL ATON3
                  VAL4=V1
                  IF(LNAME(10:17).EQ.'XD      '.OR.&
                  &LNAME(10:17).EQ.'YD      '.OR.&
                  &LNAME(10:17).EQ.'PIVX    '.OR.&
                  &LNAME(10:17).EQ.'PIVY    '.OR.&
                  &LNAME(10:17).EQ.'GDX     '.OR.&
                  &LNAME(10:17).EQ.'GDY     ') THEN
                     VAL4=VAL4*DABS(W1)
                     GO TO 1001
                  END IF
                  IF(LNAME(10:17).EQ.'ALPHA   '.OR.&
                  &LNAME(10:17).EQ.'BETA    '.OR.&
                  &LNAME(10:17).EQ.'GAMMA   '.OR.&
                  &LNAME(10:17).EQ.'GALPHA  '.OR.&
                  &LNAME(10:17).EQ.'GBETA   '.OR.&
                  &LNAME(10:17).EQ.'GGAMMA  ') THEN
                     VAL4=-VAL4
                     GO TO 1001
                  END IF
                  VAL4=VAL4*(W1)
1001              V1=VAL4
                  V1=VAL4
                  CALL NTOA3
                  AVAL4=AV1
               ELSE
               END IF
            ELSE
            END IF
!       LNAME(10:17)= CLAP OR COBS
            IF(LNAME(10:17).EQ.'CLAP    '.OR.&
            &LNAME(10:17).EQ.'COBS    ') THEN
!       LINEAR SCALING
               IF(AVAL4.NE.',') THEN
                  AV1=AVAL4
                  CALL ATON3
                  VAL4=V1
                  VAL4=VAL4*DABS(W1)
                  V1=VAL4
                  CALL NTOA3
                  AVAL4=AV1
               ELSE
               END IF
            ELSE
            END IF
!       LNAME(10:17)= CV,CVTOR
            IF(LNAME(10:17).EQ.'CV      '.OR.&
            &LNAME(10:17).EQ.'CVTOR   ')THEN
!       RECIPROCAL SCALING
               IF(AVAL4.NE.',') THEN
                  AV1=AVAL4
                  CALL ATON3
                  VAL4=V1
                  VAL4=VAL4/W1
                  V1=VAL4
                  CALL NTOA3
                  AVAL4=AV1
               ELSE
               END IF
            ELSE
            END IF
!       LNAME(10:17)= AD OR ADTOR
            IF(LNAME(10:17).EQ.'AD      '.OR.&
            &LNAME(10:17).EQ.'AC      '.OR.&
            &LNAME(10:17).EQ.'AE      '.OR.&
            &LNAME(10:17).EQ.'AF      '.OR.&
            &LNAME(10:17).EQ.'AG      '.OR.&
            &LNAME(10:17).EQ.'AH      '.OR.&
            &LNAME(10:17).EQ.'AI     '.OR.&
            &LNAME(10:17).EQ.'AJ      '.OR.&
            &LNAME(10:17).EQ.'AK      '.OR.&
            &LNAME(10:17).EQ.'AL      '.OR.&
            &LNAME(10:17).EQ.'ADTOR   '.OR.&
            &LNAME(10:17).EQ.'AETOR   '.OR.&
            &LNAME(10:17).EQ.'AFTOR   '.OR.&
            &LNAME(10:17).EQ.'AGTOR   ') THEN
!       SPECIAL SCALING
               IF(AVAL4.NE.',') THEN
                  AV1=AVAL4
                  CALL ATON3
                  VAL4=V1
                  IF(LNAME(10:17).EQ.'AC     ')THEN
                     VAL4=VAL4/W1
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AD      '.OR.&
                  &LNAME(10:17).EQ.'ADTOR   ') THEN
                     VAL4=VAL4/(W1**3)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AE      '.OR.&
                  &LNAME(10:17).EQ.'AETOR   ') THEN
                     VAL4=VAL4/(W1**5)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AF      '.OR.&
                  &LNAME(10:17).EQ.'AFTOR   ') THEN
                     VAL4=VAL4/(W1**7)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AG      '.OR.&
                  &LNAME(10:17).EQ.'AGTOR   ') THEN
                     VAL4=VAL4/(W1**9)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AH      ') THEN
                     VAL4=VAL4/(W1**11)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AI      ') THEN
                     VAL4=VAL4/(W1**13)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AJ      ') THEN
                     VAL4=VAL4/(W1**15)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AK      ') THEN
                     VAL4=VAL4/(W1**17)
                  ELSE
                  END IF
                  IF(LNAME(10:17).EQ.'AL      ') THEN
                     VAL4=VAL4/(W1**19)
                  ELSE
                  END IF
                  V1=VAL4
                  CALL NTOA3
                  AVAL4=AV1
               ELSE
               END IF
            ELSE
            END IF
!
!       RE-CONSTRUCT HOLDER
!
            HOLDER=LNAME
            IF(AVAL1.NE.',') THEN
               HOLDER=HOLDER//AVAL1//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL2.NE.',') THEN
               HOLDER=HOLDER//AVAL2//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL3.NE.',') THEN
               HOLDER=HOLDER//AVAL3//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL4.NE.',') THEN
               HOLDER=HOLDER//AVAL4//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            IF(AVAL5.NE.',') THEN
               HOLDER=HOLDER//AVAL5//','
            ELSE
               HOLDER=HOLDER//','
            END IF
            GO TO 299
         ELSE
!       NOT PIKUP
         END IF
         GO TO 300
!************************************************************
299      CONFG(I,J)=HOLDER(1:140)
300   CONTINUE
!
!       FINISHED SCALING OF SURFACE DATA
      DO J=1,CFGCNT(I)
!     I IS THE CONFIG NUMBER, J IS THE ENTRY NUMBER
         IF(CONFG(I,J)(1:2).EQ.'C1') THEN
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
!     TYPE 4
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
!
         IF(CONFG(I,J)(1:2).EQ.'C2') THEN
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
!     TYPE 4
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
!
         IF(CONFG(I,J)(1:2).EQ.'C3') THEN
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.4.OR.SPECF2(I,J).EQ.4) THEN
!     TYPE 4
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
!
         IF(CONFG(I,J)(1:2).EQ.'C4') THEN
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/W1
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
!
         IF(CONFG(I,J)(1:2).EQ.'C5') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/W1
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:2).EQ.'C6') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:2).EQ.'C7') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**2)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:2).EQ.'C8') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**2)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:2).EQ.'C9') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**2)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C10') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**2)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C11') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/W1
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2*W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2*W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C12') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**2)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C13') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/W1
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C14') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**2)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C15') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**3)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C16') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/W1
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C17') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**2)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C18') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**2)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C19') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**2)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C20') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**2)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C21') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**4)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**3)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C22') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**12)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**3)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C23') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**13)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.12.OR.SPECF2(I,J).EQ.12) THEN
!     TYPE 12
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**3)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C24') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**14)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**3)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C25') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**15)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**3)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C26') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**16)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**4)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C27') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**17)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**4)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C28') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**18)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**5)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**4)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C29') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**19)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**4)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C30') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**20)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**4)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C31') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**21)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**4)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C32') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**22)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C33') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**23)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C34') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**24)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C35') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**25)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C36') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**26)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**6)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C37') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**27)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C38') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**28)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**5)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C39') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**29)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C40') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**30)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C41') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**31)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C42') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**32)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C43') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**33)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C44') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**34)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C45') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**35)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**7)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C46') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**36)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**6)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C47') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**37)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C48') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.1.OR.SPECF2(I,J).EQ.1.OR.&
            &SPECFF(I,J).EQ.6.OR.SPECF2(I,J).EQ.6) THEN
!     TYPE 1 OR 6
               VAL2=VAL2/(W1**38)
            END IF
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C49') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C50') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C51') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C52') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C53') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C54') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C55') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**8)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**7)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C56') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C57') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C58') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C59') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C61') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C62') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C63') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C64') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C65') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**8)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C66') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**9)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C67') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C68') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C69') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C70') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C71') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C72') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C73') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C74') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C76') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**9)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C77') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C78') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**10)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C79') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C80') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C81') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C82') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C83') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C84') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C85') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C86') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C87') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C88') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
            IF(SPECFF(I,J).EQ.13.OR.SPECF2(I,J).EQ.13) THEN
!     TYPE 13
               VAL2=VAL2/(W1**10)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C89') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C90') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
         IF(CONFG(I,J)(1:3).EQ.'C91') THEN
!                     SCALE IT
!     BREAK OUT SURF NUMBER AND VALUE
            AN1=CONFG(I,J)(10:23)
            CALL AUXATN
            VAL1=N1
            AN1=CONFG(I,J)(34:56)
            CALL AUXATN
            VAL2=N1
!     WHAT KIND OF SPECIAL SURFACE IS IT ?
            IF(SPECFF(I,J).EQ.7.OR.SPECF2(I,J).EQ.7.OR.&
            &SPECFF(I,J).EQ.8.OR.SPECF2(I,J).EQ.8) THEN
!     TYPE 7 OR 8
               VAL2=VAL2/(W1**11)
            END IF
!     CONVERT THE SCALED VALUE
            N1=VAL2
            CALL AUXNTA
            AVAL2=AN1
!     STORE THE SCALED VALUE
            CONFG(I,J)(34:56)=AVAL2
         END IF
      END DO
!
10 CONTINUE
   RETURN
END
! SUB CFPIKD.FOR
SUBROUTINE CFPIKD
!
   use DATCFG
   use DATLEN
   IMPLICIT NONE
!
!       WHEN A PARTICULAR CONFIGURATION BECOMES THE MAIN OR
!       CURRENT LENS, IF INDIRECT OR ILLEGAL PIKUPS ARE FOUND,
!       THIS SUBROUTINE REMOVES THEM FROM THAT CONFIGURATUIONS
!       DATA IN THE CONFIGS ARRAY.
!
   CHARACTER AVAL1*23,LINE1*32,LINE2*17
!
   INTEGER F12,I,J,CHGLIN,PIKLIN,K,CFI,CFJ,CFF12
!
   COMMON/DPIKER/CFI,CFJ,CFF12
!
   COMMON/DPIKER2/PIKLIN
!
   REAL*8 VAL1
!
   COMMON/JK_NTA3/VAL1,AVAL1
!
!
   F12=CFF12
   I=CFI
   J=CFJ
!
!       FOR CONFIGURATION F12, AFTER THE LINE
!       CHG     , (I REPRESENTED AS A REAL*8 NUMBER IN
!       D23.15) THERE IS A PIKUP WHICH MUST BE REMOVED.
!
!       THE TYPE OF PIKUP IS DESIGNATED BY THE INTEGER VALUE OF
!                               J
!                       ENCODED AS:
!                       1=RD
!                       2=CV
!                       3=TH
!                       4=CC
!                       5=AD
!                       6=AE
!                       7=AF
!                       8=AG
!                       9=CVTOR
!                       10=RDTOR
!                       11=PRO
!                       12=NPRO
!                       13=YD
!                       14=XD
!                       15=ALPHA
!                       16=BETA
!                       17=GAMMA
!                       18=CLAP
!                       19=COBS
!                       20=GLASS
!                       21=CCTOR
!                       22=ADTOR
!                       23=AETOR
!                       24=AFTOR
!                       25=AGTOR
!                       26=AC
!                       27=AH
!                       28=AI
!                       29=AJ
!                       30=AK
!                       31=AL
!                       32=THOAL
!                       33=ZD
!                       34=PIVX
!                       35=PIVY
!                       36=PIVZ
!                       37=GDX
!                       38=GDX
!                       39=GDX
!                       40=GALPHA
!                       41=GBETA
!                       42=GGAMMA
!                       43=GRT
!       DETERMINE THE VALUE OF LINE2
   LINE2='                 '
   IF(J.EQ.1)  LINE2='PIKUP    RD      '
   IF(J.EQ.2)  LINE2='PIKUP    CV      '
   IF(J.EQ.3)  LINE2='PIKUP    TH      '
   IF(J.EQ.4)  LINE2='PIKUP    CC      '
   IF(J.EQ.5)  LINE2='PIKUP    AD      '
   IF(J.EQ.6)  LINE2='PIKUP    AE      '
   IF(J.EQ.7)  LINE2='PIKUP    AF      '
   IF(J.EQ.8)  LINE2='PIKUP    AG      '
   IF(J.EQ.9)  LINE2='PIKUP    CVTOR   '
   IF(J.EQ.10) LINE2='PIKUP    RDTOR   '
   IF(J.EQ.11) LINE2='PIKUP    PRO     '
   IF(J.EQ.12) LINE2='PIKUP    NPRO    '
   IF(J.EQ.13) LINE2='PIKUP    YD      '
   IF(J.EQ.14) LINE2='PIKUP    XD      '
   IF(J.EQ.15) LINE2='PIKUP    ALPHA   '
   IF(J.EQ.16) LINE2='PIKUP    BETA    '
   IF(J.EQ.17) LINE2='PIKUP    GAMMA   '
   IF(J.EQ.18) LINE2='PIKUP    CLAP    '
   IF(J.EQ.19) LINE2='PIKUP    COBS    '
   IF(J.EQ.20) LINE2='PIKUP    GLASS   '
   IF(J.EQ.21) LINE2='PIKUP    CCTOR   '
   IF(J.EQ.22) LINE2='PIKUP    ADTOR   '
   IF(J.EQ.23) LINE2='PIKUP    AETOR   '
   IF(J.EQ.24) LINE2='PIKUP    AFTOR   '
   IF(J.EQ.25) LINE2='PIKUP    AGTOR   '
   IF(J.EQ.26) LINE2='PIKUP    AC      '
   IF(J.EQ.27) LINE2='PIKUP    AH      '
   IF(J.EQ.28) LINE2='PIKUP    AI      '
   IF(J.EQ.29) LINE2='PIKUP    AJ      '
   IF(J.EQ.30) LINE2='PIKUP    AK      '
   IF(J.EQ.31) LINE2='PIKUP    AL      '
   IF(J.EQ.32) LINE2='PIKUP    THOAL   '
   IF(J.EQ.33) LINE2='PIKUP    ZD      '
   IF(J.EQ.34) LINE2='PIKUP    PIVX    '
   IF(J.EQ.35) LINE2='PIKUP    PIVY    '
   IF(J.EQ.36) LINE2='PIKUP    PIVZ    '
   IF(J.EQ.37) LINE2='PIKUP    GDX     '
   IF(J.EQ.38) LINE2='PIKUP    GDY     '
   IF(J.EQ.39) LINE2='PIKUP    GDZ     '
   IF(J.EQ.40) LINE2='PIKUP    GALPHA  '
   IF(J.EQ.41) LINE2='PIKUP    GBETA   '
   IF(J.EQ.42) LINE2='PIKUP    GGAMMA  '
   IF(J.EQ.43) LINE2='PIKUP    GRT     '
!       CONSTRUCT THE CHG LINE WHICH MUST BE SEARCHED FOR AS
!       LINE1
   VAL1=DBLE(I)
   CALL NTOA3
   LINE1='CHG     ,'//AVAL1
!
!       CHGLIN IS THE LINE WERE THE CORRECT CHG IS LOCATED
!
   DO 100 K=1,CFGCNT(F12)
      EE12=CONFG(F12,K)
      HOLDER=EE12
      IF((HOLDER(1:32)).EQ.LINE1) THEN
         CHGLIN = K
         GO TO 101
      ELSE
!       KEEP LOOKING, WE KNOW IT IS THERE.
      END IF
100 CONTINUE
101 CONTINUE
!
!       NOW SEARCH FROM LINE CHGLIN TO CFGCNT(F12) FOR THE
!       PIKUP DESIGNATED BY LINE2
   DO 200 K=CHGLIN,CFGCNT(F12)
      EE12=CONFG(F12,K)
      HOLDER=EE12
      IF((HOLDER(1:17)).EQ.LINE2) THEN
         PIKLIN=K
      ELSE
!       KEEP GOING IT MUST BE THERE
      END IF
200 CONTINUE
!
!       NOW REMOVE LINE PIKLIN FROM CFG F12 USING SUBROUTINE
!       REMPIK
   CALL REMPIK
!
   RETURN
!
END
