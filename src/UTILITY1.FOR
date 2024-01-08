C       FIRST SET OF UTILTIY ROUTINES GO HERE

C SUB PPRINT.FOR
        SUBROUTINE PPRINT
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE HANDELS THE "PRINT" SPECT COMMAND.
C
C                       DEFINE VARIABLES
C
        INTEGER I,J,K,JK,KI
C
        CHARACTER NM*8,FN*10,DDATE*9,TTIME*8
C
        LOGICAL EXISJK
C
        REAL*8 WORK(1:1001,1:3),L1,L2,
     1  DATA1,DATA2,LAMB1,LAMB2,CUMULT(1:1001,1:3),DELLAM,WORKFC,
     2  SLOPE,TEMP(1:1001,1:3)
C
        INTEGER OCC,POINTS,NTOTAL
     4  ,TCOUNT,NF
C
        COMMON/CUM/CUMULT
C
        COMMON/WRK/WORK
C
        COMMON/WAVEL/LAMB1,LAMB2,NTOTAL
C
        INCLUDE 'DATMAI.INC'
C
C       IF SQ.EQ.0, DON'T LOOK FOR A FILE, JUST MOVE THE
C       CUMULATIVE AREA.
C
        IF(SQ.EQ.1) THEN
C
C       TABLE DATA IN BINARY FORMAT. TRA.DAT IS UNIT 35
C
C       DATA STORED IN TRA.DAT IS:
C               FILE NAME (TNAME) CHARACTER *8
C               DATA1 STARTING WAVELENGTH (DOUBLE PRES.)
C               DATA2 ENDIG WAVLENGTH (DOUBLE PRES.)
C               POINTS - NUMBER OF DATA POINTS (INTEGER)
C               OCC- OCCUPANCY FLAG, 0=EMPTY, 1- FULL (INTEGER)
C               DDATE-DATE FILED
C               TTIME=TIME FILED
C       FIND THE DESIRED FILE
C       ***************************************************************
                        EXISJK=.FALSE.
        INQUIRE(FILE=LIBTRA//'TRA.DAT',EXIST=EXISJK)
        IF(.NOT.EXISJK) THEN
        WRITE(OUTLYNE,*)'"TABLE FILES DO NOT YET EXIST'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'TO INITIALIZE IT, USE "ITF" AND "PROCEED"'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C       ***************************************************************
        OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//'TRA.DAT',
     1  FORM='UNFORMATTED',RECL=(50*NRECL),STATUS='UNKNOWN')
C
                        DO 200 I=1,200
        READ(UNIT=35,REC=I) NM,DATA1,DATA2,POINTS,OCC,DDATE,TTIME
        IF(NM.EQ.WQ) THEN
C       FOUND FILE
                        GO TO 300
                        END IF
 200                    CONTINUE
C       IF YOU GOT HERE, FILE DID NOT EXIST
        WRITE(OUTLYNE,*)'FILE ',WQ,' DOES NOT EXIST'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
        CALL CLOSE_FILE(35,1)
                        CALL MACFAL
                        RETURN
 300                    CONTINUE
C
C
C       LOAD FILE INTO TEMP ARRAY
C
                NF=I
        CALL TRAFIL(NF,FN)
C
C       OPEN AND READ FROM FILE
        OPEN(UNIT=35,ACCESS='DIRECT',FILE=LIBTRA//FN,
     1  FORM='UNFORMATTED',RECL=(80*NRECL),STATUS='UNKNOWN')
C
                        TCOUNT=POINTS
                        DO 210 J=1,POINTS
        READ(UNIT=35,REC=J) TEMP(J,1),TEMP(J,2),TEMP(J,3)
 210                    CONTINUE
        CALL CLOSE_FILE(35,1)
                        ELSE
C       QUALIFIER WAS BLANK, NO FILE TO LOAD
C       LOAD CUMULATIVE AREA INTO WORK
                        TCOUNT=NTOTAL
                        POINTS=NTOTAL
                        J=NTOTAL
                        TEMP(1:J,1:3)=CUMULT(1:J,1:3)
                        END IF
C
C       ESTABLISH NEW WAVELENGTH BOUNDARIES. NO WAVELENGTH
C       VALUE MAY BE LESS THAN 0.0 MICRONS
C
        L1=(LAMB1*W1)+W2
        IF(L1.LT.0.0D0) L1=0.0D0
        L2=(LAMB2*W1)+W2
        IF(L1.LE.0.0D0.OR.L2.LE.0.0D0) THEN
        WRITE(OUTLYNE,*)
     1  'A ZERO OR NEGATIVE WAVELENGTH VALUE IS NOT ALLOWED'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(L2.LE.0.0D0.OR.L2.LE.L1) THEN
C       NO WAVELENGTH RANGE EXISTS, NO INTERPOLATION POSSIBLE
        WRITE(OUTLYNE,*)'WAVELENGTH LIMITS ARE NOT REALISTIC'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                ELSE
C       REDEFINE LAMB1 AND LAMB2
                LAMB1=L1
                LAMB2=L2
                END IF

C       INTERPOLATE TO THE POINTS DEFINED IN THE CUMULATIVE
C       AREA BY LAMB1,LAMB2 AND NTOTAL.
C
C       THE DIFFERENCE IN WAVELENGTH BETWEEN ENTRIES IN THE NEW
C       DATA WILL BE
C       (LAMB2-LAMB1)/(NTOTAL-1)
C
                DELLAM=(LAMB2-LAMB1)/(DBLE(NTOTAL)-1.0D0)
C
C       SCALE THE CURRENT TEMP DATA BY THE REQUESTED
C       TRANSFORMATION.
C
                DO 505 J=1,TCOUNT
        TEMP(J,3)=(((TEMP(J,3)*W3)+W4)**INT(DABS(W5)))
 505                    CONTINUE
                IF(INT(W5).LT.0.0D0) THEN
                DO 506 J=1,TCOUNT
        IF(TEMP(J,3).NE.0.0D0) TEMP(J,3)=1.0D0/TEMP(J,3)
        IF(TEMP(J,3).EQ.0.0D0) TEMP(J,3)=0.0D0
 506            CONTINUE
                        END IF
C
C       THESE NEW FUNCTIONAL VALUES ARE FOR THE NEW WAVELENGTH
C       VALUES :
                DO 500 J=1,TCOUNT
        TEMP(J,2)=(TEMP(J,2)*W1)+W2
        IF(TEMP(J,2).LT.0.0D0) TEMP(J,2)=0.0D0
 500                    CONTINUE
C
        IF(TEMP(POINTS,2).LE.TEMP(1,2)) THEN
        WRITE(OUTLYNE,*)
     1  'NEW WAVELENGTHS ARE NOT IN ASCENDING ORDER AND'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'"PRINT" WAS ABORTED.'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
C       THE NEW FUNCTIONAL VALUES ARE SPREAD OVER THE NEW LAMB1 TO
C       LAMB2 RANGE IN WORK, WITH SPACING DELLAM.
C
                        DO 11 J=1,NTOTAL
                        WORK(J,1)=DBLE(J)
 11                     CONTINUE
                WORK(1,2)=LAMB1
                WORK(NTOTAL,2)=LAMB2
                        DO 10 J=2,(NTOTAL-1)
                WORK(J,2)=WORK((J-1),2)+DELLAM
 10                     CONTINUE
C
C       NOW INTERPOLATE THE TEMP AREA AND STORE INTO THE
C       WORK AREA FUNCTIONAL VALUES.
C
                SLOPE=0.0D0
                JK=1
                DO 20 J=1,TCOUNT-1
C
C       CALCULATE A SLOPE FROM THE WORK DATA
C
        SLOPE=(((TEMP((J+1),3))-(TEMP(J,3)))/
     1  ((TEMP((J+1),2))-(TEMP(J,2))))
                        KI=JK
C
C       RULES FOR INTERPOLATION (EXTRAPOLATION MAY OCCUR
C       AT END POINTS)
C
C       GET A WAVELENGTH VALUE FROM THE CUMULATIVE AREA
C       AS LONG AS THE WAVELENGTH VALUE FROM THE
C       CUMULATIVE AREA IS NOT GREATER THAN THE CURRENT
C       VALUE OF TEMP((J+1),2), WE KEEP USING THE CURRENT SLOPE.
C
C       IF THE WAVELENGTH VALUE FROM CUMULATIVE AREA BECOMES
C       GREATER THAN THE CURRENT TEMP((J+1),2), CALCULATE A NEW
C       SLOPE UNLESS J+1=TCOUNT. IF J+1=TCOUNT, KEEP USING
C       THE CURRENT SLOPE UNTIL THE CUMULATIVE AREA IS FILLED.
C
C       IF THE LAST WAVELENGTH VALUE IN WORK IS REACHED(J=TCOUNT),
C
                DO 30 I=KI,NTOTAL
        IF(WORK(I,2).LE.TEMP((J+1),2).AND.(J+1).LE.TCOUNT
     1  .OR.WORK(I,2).GT.TEMP((J+1),2).AND.(J+1).GE.TCOUNT) THEN
        WORKFC=(SLOPE*(WORK(I,2)-TEMP(J,2)))+TEMP(J,3)
        WORK(I,3)=WORKFC
                        ELSE
C       WE MUST
C       CALCULATE A NEW SLOPE. THE UNLESS OUT OF POINTS CASE WAS DONE
C       ABOVE.
                        JK=I
                        GO TO 20
                        END IF
 30                     CONTINUE
C
 20                     CONTINUE
C
C       WORK AREA HAS HAD STORED IN IT THE TRANSFORMED,
C       INTERPOLATED TEMP AREA. PRINT MESSAGE AND RETURN
        WRITE(OUTLYNE,*)
     1  'THE NEW CONTENTS OF THE "WORK" AREA AFTER ACTION'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'BY THE "PRINT" COMMAND ARE:.'
      CALL SHOWIT(1)
                        CALL PWORK
C
                        RETURN
C
                        END
C SUB PRNLP.FOR
        SUBROUTINE PRNLP
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO PRINT THE PRINTER FILE.
C
        LOGICAL EXIS7,OPEN7
C
      CHARACTER PLINE*80
C
      INTEGER I
C
      LOGICAL NOPRIN
C
      COMMON/PRINNO/NOPRIN
C
        INCLUDE 'DATMAI.INC'
C
        IF(SN.EQ.1.OR.SST.EQ.1) THEN
      OUTLYNE='"PRINT" TAKES NO NUMERIC OR STRING INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(SQ.EQ.0) THEN
                SQ=1
                WQ='P       '
                END IF
        IF(SQ.EQ.1.AND.WQ(1:1).NE.'P'.AND.WQ(1:1).NE.'L') THEN
      OUTLYNE='"PRINT" ONLY TAKES "P" AND "L" AS OPTIONAL QUALIFIERS'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                EXIS7=.FALSE.
                OPEN7=.FALSE.
        INQUIRE(FILE='PRINTER.TXT',EXIST=EXIS7)
                IF(EXIS7) THEN
        INQUIRE(FILE='PRINTER.TXT',OPENED=OPEN7)
      CALL CLOSE_FILE(7,1)
C WINTER
C      CALL PREDITOR
      IF(OUT.EQ.7) THEN
C     REOPEN
      IF(APPEND) THEN
      OPEN(UNIT=7,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PRINTER.TXT'
     2  ,STATUS='UNKNOWN')
                END IF
      IF(.NOT.APPEND) THEN
      OPEN(UNIT=7,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PRINTER.TXT'
     1  ,STATUS='UNKNOWN')
                       END IF
                       END IF
                        ELSE
C     FILE DOES NOT EXIST
      OUTLYNE='NO PRINTER FILE WAS FOUND'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                CALL MACFAL
                   RETURN
                        END IF
                RETURN
 9875           CONTINUE
      CALL CLOSE_FILE(7,1)
      OUTLYNE='PRINTER FILE CAN NOT BE READ'
      CALL SHOWIT(1)
      OUTLYNE='NO PRINTING WILL BE PERFORMED'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
 8989           CONTINUE
      OUTLYNE='PRINTER IS BUSY'
      CALL SHOWIT(1)
      OUTLYNE='TRY AGAIN AFTER THE PRINTER IS DONE'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END
C SUB NEWSEED.FOR
        SUBROUTINE NEWSEED
C
        IMPLICIT NONE
C
      REAL MSEED
C
      COMMON/SEEDER/MSEED
C
        INCLUDE 'DATMAI.INC'
C
        IF(STI.EQ.1) THEN
      OUTLYNE=
     1'"NEWSEED" RESETS THE RANDOM NUMBER GENERATOR PROGRAM WIDE'
      CALL SHOWIT(1)
                RETURN
                END IF
      MSEED=1618033.
      CALL RANDSET()
                RETURN
                END
C SUB PM.FOR
        SUBROUTINE PM
        USE GLOBALS
        USE PROFIT_GLOBAL
C
        IMPLICIT NONE
C
C       THIS DOES THE "PM" AND "PMP" CMD LEVEL COMMANDS
C       AND ALSO SETS THE DEFAULT OPCON OR OPERATING CONDITIONS.
C       USED FOR CHANGING, SETTING, AND QUERRYING
C       OPERATING CONDITIONS
C
        INTEGER IVAL,ALLOERR
C
        REAL*8 VAL
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
C
        IF(WC.EQ.'PM') THEN
C
        IF(WQ.NE.'SURTOL'.AND.WQ.NE.'AIMTOL'.AND.WQ.NE.'SERINC'
     1  .AND.WQ.NE.'PFAC'.AND.WQ.NE.'MRAYS'.AND.WQ.NE.'CAIMTOL'
     2  .AND.WQ.NE.'DINMUL'.AND.WQ.NE.'NRAITR'.AND.WQ.NE.'LINTOL'
     3  .AND.WQ.NE.'DAYS'.AND.WQ.NE.'ONTOL'.AND.WQ.NE.'SINGTOL'.AND.
     4  WQ.NE.'DIFTOL'.AND.WQ.NE.'DELSUR'.AND.
     5  WQ.NE.'SERLIM'.AND.WQ.NE.'SHORT'.AND.WQ.NE.'MEDIUM'.AND.WQ.NE.
     6  'LONG'.AND.WQ.NE.'SELDELAY'.AND.WQ.NE.'BUYDELAY'
     6  .AND.WQ.NE.'SAGDEL'.AND.WQ.NE.'MAXREG'.AND.WQ.NE.'MAXOPT') THEN
      OUTLYNE='INVALID QUALIFIER USED WITH "PM" COMMAND'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(DF1.EQ.1) THEN
      OUTLYNE=
     1'PM '//WQ//'REQUIRES NUMERIC EXPLICIT WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      OUTLYNE=
     1'PM '//WQ//'ONLY TAKES QUALIFIER AND NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(WQ.EQ.'NRAITR') THEN
        IF(WQ.EQ.'NRAITR') NRAITR=INT(W1)
        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'SHORT') THEN
        IF(WQ.EQ.'SHORT') SHORT_TIME=INT(W1)
        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'DAYS') THEN
        IF(WQ.EQ.'DAYS') PDAYS=INT(W1)
        IF(((W1/2.0D0)-DBLE(INT(W1/2.0D0))).NE.0.0D0.OR.
     1  INT(W1).LT.30.0D0) THEN
        WRITE(OUTLYNE,*) '"DAYS" MUST BE AN EVEN INTEGER'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*) 'GREATER THAN 30'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(WQ.EQ.'DAYS') PDAYS=INT(W1)

        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'MEDIUM') THEN
        IF(WQ.EQ.'MEDIUM') MEDIUM_TIME=INT(W1)
        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'LONG') THEN
        IF(WQ.EQ.'LONG') LONG_TIME=INT(W1)
        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'SELDEALY') THEN
        IF(WQ.EQ.'SELDELAY') SELL_DELAY=INT(W1)
        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'BUYDEALY') THEN
        IF(WQ.EQ.'BUYDELAY') BUY_DELAY=INT(W1)
        WRITE(OUTLYNE,1001) WQ,INT(W1)
      CALL SHOWIT(1)
                        RETURN
                        END IF
        IF(WQ.EQ.'SURTOL'.OR.WQ.EQ.'AIMTOL'.OR.WQ.EQ.'SERINC'
     1  .OR.WQ.EQ.'PFAC'.OR.WQ.EQ.'MRAYS'.OR.WQ.EQ.'CAIMTOL'
     2  .OR.WQ.EQ.'DINMUL'.OR.WQ.EQ.'DIFTOL'.OR.WQ.EQ.'DELSUR'
     3  .OR.WQ.EQ.'ONTOL'.OR.WQ.EQ.'SINGTOL'.OR.WQ.EQ.'LINTOL'.OR.
     4  WQ.EQ.'SERLIM'.OR.WQ.EQ.'SAGDEL'.OR.WQ.EQ.'MAXREG'.OR.
     5  WQ.EQ.'MAXOPT') THEN
        IF(WQ.EQ.'SURTOL') SURTOL=W1
C       LIMITS FOR AIMTOL
        IF(WQ.EQ.'AIMTOL') THEN
        IF(W1.LE.0.0D0) THEN
      OUTLYNE=
     1'"PM AIMTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LT.1.0D-25) THEN
        OUTLYNE=
     1  'THE "AIMTOL" VALUE MAY NOT BE LESS THAN 1.0D-25'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.1.0D0) THEN
        OUTLYNE=
     1  'THE "AIMTOL" VALUE MUST BE LESS THAN 1.0'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        AIMTOL=W1
                        ELSE
                        END IF
C       NOT CAIMTOL
C       LIMITS FOR CAIMTOL
        IF(WQ.EQ.'CAIMTOL') THEN
        IF(W1.LE.0.000001D0) THEN
      OUTLYNE=
     1'"PM CAIMTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT >= 0.000001'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LT.0.000001D0) THEN
        OUTLYNE=
     1  'THE "CAIMTOL" VALUE MAY NOT BE LESS THAN 1.0D-6'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.1.0D0) THEN
        OUTLYNE=
     1  'THE "CAIMTOL" VALUE MUST BE LESS THAN 1.0'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        CAIMTOL=W1
                        ELSE
                        END IF
C       NOT CAIMTOL
C       LIMITS FOR MRAYS
        IF(WQ.EQ.'MRAYS') THEN
        IF(W1.LT.1.0D0) THEN
        OUTLYNE=
     1  'THE "MRAYS" VALUE MUST BE LESS THAN GREATER THAN OR EQUAL TO 1'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        MRAYS=INT(W1)
                        ELSE
                        END IF
C       NOT MRAYS
C       LIMITS FOR MAXREG
        IF(WQ.EQ.'MAXREG') THEN
        IF(INT(W1).LT.4000) THEN
      OUTLYNE=
     1'"PM MAXREG" REQUIRES NUMERIC WORD #1 INPUT GREATER THAN OR'
      CALL SHOWIT(1)
      OUTLYNE=
     1'EQUAL TO 4000'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W1).GT.100000) THEN
        OUTLYNE=
     1  'THE "MAXREG" VALUE MAY NOT BE GREATER THAN 100000'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        MAXREG=W1
                        ELSE
                        END IF
C       NOT MAXREG
C       LIMITS FOR MAXOPT
        IF(WQ.EQ.'MAXOPT') THEN
        IF(INT(W1).LT.4000) THEN
      OUTLYNE=
     1'"PM MAXOPT" REQUIRES NUMERIC WORD #1 INPUT GREATER THAN OR'
      CALL SHOWIT(1)
      OUTLYNE=
     1'EQUAL TO 4000'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(INT(W1).GT.100000) THEN
        OUTLYNE=
     1  'THE "MAXOPT" VALUE MAY NOT BE GREATER THAN 100000'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        MAXOPT=INT(W1)
        MAXVB=INT(W1)
        WRITE(OUTLYNE,*) MAXVB,INT(W1),'HI3'
        CALL SHOWIT(1)
        MAXTVB=MAXVB-MAXCMP
                        ELSE
                        END IF
C       NOT MAXOPT
C       LIMITS FOR SAGDEL
        IF(WQ.EQ.'SAGDEL') THEN
        IF(W1.LE.0.0D0) THEN
      OUTLYNE=
     1'"PM SAGDEL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LT.1.0D-15) THEN
        OUTLYNE=
     1  'THE "SAGDEL" VALUE MAY NOT BE LESS THAN 1.0D-15'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        SAGDEL=W1
                        ELSE
                        END IF
C       NOT SAGDEL
C       LIMITS FOR SERINC
        IF(WQ.EQ.'SERINC') THEN
        IF(W1.LE.0.0D0) THEN
      OUTLYNE=
     1'"PM SERINC" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LT.1.0D-15) THEN
        OUTLYNE=
     1  'THE "SERINC" VALUE MAY NOT BE LESS THAN 1.0D-15'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.(0.5D0*SERLIM)) THEN
        OUTLYNE=
     1  'THE "SERINC" VALUE MUST BE LESS THAN ONE HALF OF SERLIM'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        SERINC=W1
                        ELSE
                        END IF
C       LIMITS FOR SERLIM
        IF(WQ.EQ.'SERLIM') THEN
        IF(W1.LE.0.0D0) THEN
      OUTLYNE=
     1'"PM SERLIM" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LE.(2.0D0*SERINC)) THEN
        OUTLYNE=
     1  'THE "SERLIM" VALUE MUST BE GREATER THAN 2 TIMES SERINC'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        SERLIM=W1
                        ELSE
                        END IF
C       NOT SERLIM
        IF(WQ.EQ.'DELSUR') THEN
        IF(W1.LE.0.0D0) THEN
        OUTLYNE=
     1  '"PM DELSUR" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LT.1.0D-15) THEN
        OUTLYNE=
     1  'THE "DELSUR" VALUE MAY NOT BE LESS THAN 1.0D-15'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.10.0D0) THEN
        OUTLYNE=
     1  'THE "DELSUR" VALUE MUST BE LESS THAN 10.0'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        DELSUR=W1
                        ELSE
C       NOT DELSUR
                        END IF
        IF(WQ.EQ.'DINMUL') THEN
        IF(W1.LE.0.0D0) THEN
        OUTLYNE=
     1  '"PM DINMUL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.LT.1.0D-15) THEN
        OUTLYNE=
     1  'THE "DINMUL" VALUE MAY NOT BE LESS THAN 1.0D-15'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        DINMUL=W1
                        ELSE
C       NOT DINMUL
                        END IF
        IF(WQ.EQ.'PFAC') PFAC=W1
        IF(WQ.EQ.'DIFTOL') THEN
        IF(W1.LE.0.0D0) THEN
        OUTLYNE=
     1  '"PM DIFTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.1.0D0) THEN
        OUTLYNE=
     1  'THE "DIFTOL" VALUE MUST BE LESS THAN 1.0'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        DIFTOL=W1
                        ELSE
C       NOT DIFTOL
                        END IF
        IF(WQ.EQ.'LINTOL') THEN
        IF(W1.LE.0.0D0.OR.W1.GT.1.0D0) THEN
        OUTLYNE=
     1  '"PM LINTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        OUTLYNE=
     1  'GREATER THAN 0.0 AND LESS THAN OR EQUAL TO 1.0'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        LINTOL=W1
                        ELSE
C       NOT LINTOL
                        END IF
        IF(WQ.EQ.'ONTOL') THEN
        IF(W1.LE.0.0D0) THEN
        OUTLYNE=
     1  '"PM ONTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.1.0D0) THEN
        OUTLYNE=
     1  'THE "ONTOL" VALUE MUST BE LESS THAN 1.0'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        ONTOL=W1
                        ELSE
C       NOT ONTOL
                        END IF
        IF(WQ.EQ.'SINGTOL') THEN
        IF(W1.LE.0.0D0) THEN
        OUTLYNE=
     1  '"PM SINGTOL" REQUIRES POSITIVE NUMERIC WORD #1 INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(W1.GE.1.0D0) THEN
        OUTLYNE=
     1  'THE "SINGTOL" VALUE MUST BE LESS THAN 1.0'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        SINGTOL=W1
                        ELSE
C       NOT SINGTOL
                        END IF
        WRITE(OUTLYNE,2001) WQ,W1
      CALL SHOWIT(0)
                        RETURN
                        END IF
                        RETURN
 1001   FORMAT(A8,' RESET TO ',I5)
 2001   FORMAT(A8,' RESET TO ',G18.10)
                        ELSE
C       NOT PM
                        END IF
C
        IF(WC.EQ.'PMP') THEN
C
        IF(WQ.NE.'SURTOL'.AND.WQ.NE.'AIMTOL'.AND.WQ.NE.'SERINC'
     1  .AND.WQ.NE.'PFAC'.AND.WQ.NE.'MRAYS'.AND.WQ.NE.'CAIMTOL'
     2  .AND.WQ.NE.'DINMUL'.AND.WQ.NE.'NRAITR'.AND.WQ.NE.'SHORT'
     3  .AND.WQ.NE.'MEDIUM'.AND.WQ.NE.'LONG'.AND.WQ.NE.'SELDELAY'
     4  .AND.WQ.NE.'DIFTOL'.AND.WQ.NE.'DELSUR'.AND.WQ.NE.'ONTOL'.AND.
     4  WQ.NE.'SINGTOL'.AND.WQ.NE.'LINTOL'
     5  .AND.WQ.NE.'BUYDELAY'.AND.WQ.NE.'DAYS'.AND.
     6 WQ.NE.'SERLIM'.AND.WQ.NE.'SAGDEL'.AND.WQ.NE.'MAXREG'.AND.
     7  WQ.NE.'MAXOPT') THEN
        OUTLYNE='INVALID QUALIFIER USED WITH "PMP" COMMAND'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SN.EQ.1.OR.SST.EQ.1) THEN
        OUTLYNE='PMP '//WQ//'TAKES NO NUMERIC OR STRING INPUT'
        CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(WQ.EQ.'NRAITR') THEN
        IF(WQ.EQ.'NRAITR') IVAL=NRAITR
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'SHORT') THEN
        IF(WQ.EQ.'SHORT') IVAL=SHORT_TIME
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'DAYS') THEN
        IF(WQ.EQ.'DAYS') IVAL=PDAYS
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'MEDIUM') THEN
        IF(WQ.EQ.'MEDIUM') IVAL=MEDIUM_TIME
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'LONG') THEN
        IF(WQ.EQ.'LONG') IVAL=LONG_TIME
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'BUYDELAY') THEN
        IF(WQ.EQ.'BUY_DELAY') IVAL=BUY_DELAY
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'SELDELAY') THEN
        IF(WQ.EQ.'SELDEALY') IVAL=SELL_DELAY
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'MRAYS') THEN
        IF(WQ.EQ.'MRAYS') IVAL=MRAYS
        WRITE(OUTLYNE,1000) WQ,IVAL
      CALL SHOWIT(0)
                        RETURN
                        ELSE
                        END IF
        IF(WQ.EQ.'SURTOL'.OR.WQ.EQ.'AIMTOL'.OR.WQ.EQ.'SERINC'
     1  .OR.WQ.EQ.'PFAC'.OR.WQ.EQ.'CAIMTOL'
     2  .OR.WQ.EQ.'DINMUL'.OR.WQ.EQ.'DIFTOL'.OR.WQ.EQ.'LINTOL'
     3  .OR.WQ.EQ.'DELSUR'.OR.WQ.EQ.'ONTOL'.OR.WQ.EQ.'SINGTOL'
     4  .OR.WQ.EQ.'SERLIM'.OR.WQ.EQ.'SAGDEL'.OR.WQ.EQ.'MAXREG'.OR.
     5  WQ.EQ.'MAXOPT') THEN
        IF(WQ.EQ.'LINTOL') VAL=LINTOL
        IF(WQ.EQ.'SURTOL') VAL=SURTOL
        IF(WQ.EQ.'AIMTOL') VAL=AIMTOL
        IF(WQ.EQ.'CAIMTOL') VAL=CAIMTOL
        IF(WQ.EQ.'MAXREG') VAL=MAXREG
        IF(WQ.EQ.'MAXOPT') VAL=MAXOPT
        IF(WQ.EQ.'SERINC') VAL=SERINC
        IF(WQ.EQ.'SERLIM') VAL=SERLIM
        IF(WQ.EQ.'PFAC') VAL=PFAC
        IF(WQ.EQ.'DINMUL') VAL=DINMUL
        IF(WQ.EQ.'DIFTOL') VAL=DIFTOL
        IF(WQ.EQ.'DELSUR') VAL=DELSUR
        IF(WQ.EQ.'SAGDEL') VAL=SAGDEL
        IF(WQ.EQ.'ONTOL') VAL=ONTOL
        IF(WQ.EQ.'SINGTOL') VAL=SINGTOL
        WRITE(OUTLYNE,2000) WQ,VAL
      CALL SHOWIT(0)
        IF(WQ.EQ.'MAXOPT') THEN
        WRITE(OUTLYNE,2000) 'MAXVB   ',MAXVB
        CALL SHOWIT(0)
                END IF
                        RETURN
                        ELSE
                        END IF
 1000   FORMAT(A8,' = ',I10)
 2000   FORMAT(A8,' = ',G18.10)
                        ELSE
C       NOT PMP
                        END IF
        IF(WC.EQ.'OPCON') THEN
        SURTOL=1.0D-10
        AIMTOL=1.0D-10
        CAIMTOL=0.001D0
        MRAYS=200
        DEALLOCATE(
     1P1ARAY,STAT=ALLOERR)
      ALLOCATE(
     1P1ARAY(0:360,1:3,1:MRAYS),
     1STAT=ALLOERR)
        MAXOPT=4000
        MAXREG=50000
        MAXVB=4000
        MAXTVB=MAXVB-MAXCMP
        SAGDEL=1.0D-3
        SERINC=1.0D-2
        SERLIM=10.0D0
        PFAC=1.0D0
        DINMUL=1.0D0
        LINTOL=0.1D0
        DELSUR=0.0025D0
C       DIFTOL IS THE DEFAULT INCREMENT FOR THE REAL RAYTRACE
C       SIMULATION OF THE DIFFERENTIAL RAY TRACE. THE DEFAULT VALUE
C       IS SET AT 1.0D-3 OR THE REFERENCE APERTURE HT AT THE REFERENCE SURFACE
        DIFTOL=1.0D-3
        ONTOL=1.0D-10
        SINGTOL=1.0D-12
C
        NRAITR=100
        SHORT_TIME=5
        MEDIUM_TIME=10
        LONG_TIME=30
        SELL_DELAY=5
        BUY_DELAY=5
        PDAYS=60
        ASIAD=0
        ASITD=0
        RETURN
                        ELSE
C       NOT OPCON
                        END IF
        END
C SUB OUTPUT2.FOR
        SUBROUTINE OUTPUT2
        USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET THE CURRENT OUTPUT DEVICE.
C
C
C       VALID QUALIFIER WORDS ARE:
C
C               T = KEYBOARD + FILE IN STRING (UNIT 97) THIS IS OUT = 69
C               FILE = FILE IN STRING = 97 THIS IS OUT = 97
C
        LOGICAL OPEN7,OPEN96,OPEN97,EXIS96,EXIS97
C
        LOGICAL EXIS7,EXIS8,EXIS9,EXIS10
C
        INTEGER OLD
C
        INCLUDE 'DATMAI.INC'
C
      IF(SQ.EQ.1.AND.SST.EQ.0) THEN
      OUTLYNE='A FILE NAME IS REQUIRED FOR THIS FORM OF "OUTPUT"'
      CALL SHOWIT(5)
                CALL MACFAL
                RETURN
                   END IF
      IF(SQ.EQ.1.AND.SST.EQ.1) THEN
        LASTFIL=OFILN
      OFILN=WS(1:80)
                   END IF
      IF(WQ.EQ.'T')   OUT=69
      IF(WQ.EQ.'FILE')OUT=97
C
C     OUTPUT T PLUS FILE NAME
C
      PRINT *, "FILE NAME IS ", OFILN
      IF(WQ.EQ.'T'.OR.WQ.EQ.'FILE') THEN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(WQ.EQ.'T')OUT=69
      IF(WQ.EQ.'FILE')OUT=97
      IF(WQ.EQ.'FILE    '.OR.WQ.EQ.'T       ') THEN
        EXIS97=.FALSE.
        INQUIRE(FILE=trim(basePath)//OFILN,EXIST=EXIS97)
      IF(APPEND)
     1  OPEN(UNIT=97,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=trim(basePath)//OFILN
     2  ,STATUS='UNKNOWN',ERR=314)
      IF(.NOT.APPEND)
     1  OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=trim(basePath)//OFILN
     2  ,STATUS='UNKNOWN',ERR=314)
                   END IF
                RETURN
                ELSE
                END IF
                RETURN
 314            CONTINUE
        OUTLYNE='WARNING:'
        CALL SHOWIT(1)
        OUTLYNE='FILE COULD NOT BE OPENED'
        CALL SHOWIT(1)
        OUTLYNE='CHECK YOUR TYPING'
        CALL SHOWIT(1)
                END
C SUB APPREP.FOR
        SUBROUTINE APPREP
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET FILES TO APPEND OR SEQ.
C
        LOGICAL OLDAPP,OPEN7
C
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATHGR.INC'
C
      IF(WC.EQ.'REPLACE'.AND.STI.EQ.1) THEN
      OUTLYNE=
     1'"REPLACE" MAKES INPUT/OUTPUT ASCII FILES SEQUENTIAL'
      CALL SHOWIT(5)
      IF(APPEND) OUTLYNE=
     1'"APPEND" IS CURRENTLY IN EFFECT'
      IF(.NOT.APPEND) OUTLYNE=
     1'"REPLACE" IS CURRENTLY IN EFFECT'
      CALL SHOWIT(5)
               RETURN
               END IF
      IF(WC.EQ.'APPEND'.AND.STI.EQ.1) THEN
      OUTLYNE=
     1'"APPEND" MAKES INPUT/OUTPUT ASCII FILES APPENDED'
      CALL SHOWIT(5)
      IF(APPEND) OUTLYNE=
     1'"APPEND" IS CURRENTLY IN EFFECT'
      IF(.NOT.APPEND) OUTLYNE=
     1'"REPLACE" IS CURRENTLY IN EFFECT'
      CALL SHOWIT(5)
               RETURN
               END IF
      IF(SN.EQ.1.OR.SST.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE=
     1'"APPEND" AND "REPLACE" TAKES NO ADDITIONAL INPUT'
      CALL SHOWIT(5)
               CALL MACFAL
               RETURN
               END IF
      OLDAPP=APPEND
      IF(WC.EQ.'APPEND') APPEND=.TRUE.
      IF(WC.EQ.'REPLACE') APPEND=.FALSE.
      IF(.NOT.OLDAPP.AND.APPEND.OR.OLDAPP.AND..NOT.APPEND) THEN
C     TRASH THE FILES
         OPEN(UNIT=7,BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PRINTER.TXT'
     2  ,STATUS='UNKNOWN')
      IF(IN.NE.7) CALL CLOSE_FILE(7,0)
 35   FORMAT(A2)
         IF(APPEND) OPEN(UNIT=8,BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='CARDTEXT.DAT'
     2  ,STATUS='UNKNOWN')
      IF(APPEND.AND.IN.NE.8) CALL CLOSE_FILE(8,0)
        IF(APPEND) OPEN(UNIT=9,BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='EDITTEXT.DAT'
     2  ,STATUS='UNKNOWN')
      IF(APPEND.AND.IN.NE.9) CALL CLOSE_FILE(9,0)
      IF(APPEND.AND.IN.NE.97) CALL CLOSE_FILE(97,0)
        OPEN(UNIT=10,BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PUNCH.DAT'
     2  ,STATUS='UNKNOWN')
      IF(APPEND.AND.IN.NE.10) CALL CLOSE_FILE(10,0)
                   END IF
                RETURN
          END
C SUB OUTPUT.FOR
        SUBROUTINE OUTPUT
          USE GLOBALS
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS CALLED TO SET THE CURRENT OUTPUT DEVICE.
C
C
C       VALID QUALIFIER WORDS ARE:
C
C               TP = KEYBOARD = 6
C               TP = KEYBOARD + FILE = 69
C               LP = LINE PRINTER = PRINTER.TXT = 7
C               CP = CARDTEXT.DAT = 8
C               ED = EDITTEXT.DAT = 9
C               PU = PUNCH.DAT = 10
C               NAMED FILE = 97
C               NULL=NOWHERE.DAT=98
C
        LOGICAL OPEN7,OPEN97
C
        LOGICAL EXIS7,EXIS8,EXIS9,EXIS10,EXIS97
     1,OPEN80,OPEN81
C
        INTEGER OLD
C
        INCLUDE 'DATMAI.INC'
C
      IF(WQ.NE.'LP'.AND.
     1WQ.NE.'ED'.AND.WQ.NE.'CP'
     2.AND.WQ.NE.'PU'.AND.WQ.NE.'TP'.AND.SQ.NE.0
     1.AND.WQ.NE.'NULL') THEN
      OUTLYNE='INVALID OUTPUT DEVICE NAME'
      CALL SHOWIT(5)
                CALL MACFAL
                RETURN
                END IF
                IF(SQ.EQ.0.OR.STI.EQ.1) THEN
C HANDEL PRINTING OF NAME OF CURRENT DEVICE
      IF(OUT.EQ.69)  THEN
      OUTLYNE='OUTPUT IS "T" PLUS FILE NAME = '//OFILN
      CALL SHOWIT(5)
                RETURN
                END IF
                IF(OUT.EQ.97)  THEN
      OUTLYNE='OUTPUT IS TO FILE NAME = '//OFILN
      CALL SHOWIT(5)
                RETURN
                END IF
      IF(OUT.EQ.6)  THEN
      OUTLYNE='OUTPUT IS "TP"'
      CALL SHOWIT(5)
                RETURN
                END IF
                IF(OUT.EQ.98)  THEN
      OUTLYNE='OUTPUT IS "NULL"'
      CALL SHOWIT(5)
                RETURN
                END IF
                IF(OUT.EQ.7)  THEN
      OUTLYNE='OUTPUT IS "LP"'
      CALL SHOWIT(5)
                RETURN
                END IF
                IF(OUT.EQ.8)  THEN
      OUTLYNE='OUTPUT IS "CP"'
      CALL SHOWIT(5)
                RETURN
                END IF
                IF(OUT.EQ.9)  THEN
      OUTLYNE='OUTPUT IS "ED"'
      CALL SHOWIT(5)
                RETURN
                END IF
                IF(OUT.EQ.10) THEN
      OUTLYNE='OUTPUT IS "PU"'
      CALL SHOWIT(5)
                RETURN
                END IF
                END IF
C
C     OUTPUT TP
C
C
                IF(WQ.EQ.'TP') THEN
      IF(CMDLINE(1:5).EQ.'BATCH') RETURN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=6
 35   FORMAT(A2)
                RETURN
                ELSE
C     NOT OUT TP
                END IF
                IF(WQ.EQ.'LP') THEN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=7
        EXIS7=.FALSE.
        INQUIRE(FILE='PRINTER.TXT',EXIST=EXIS7)
      IF(APPEND)
     1  OPEN(UNIT=7,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PRINTER.TXT'
     2  ,STATUS='UNKNOWN')
      IF(.NOT.APPEND)
     1  OPEN(UNIT=7,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PRINTER.TXT'
     2  ,STATUS='UNKNOWN')
                RETURN
                ELSE
                END IF
                IF(WQ.EQ.'FILE    '.OR.WQ.EQ.'T       ') THEN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=7
        EXIS97=.FALSE.
      OFILN=WS(1:80)
        INQUIRE(FILE=OFILN,EXIST=EXIS97)
      IF(APPEND)
     1  OPEN(UNIT=97,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='OFILN'
     2  ,STATUS='UNKNOWN')
      IF(.NOT.APPEND)
     1  OPEN(UNIT=97,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=OFILN
     2  ,STATUS='UNKNOWN')
                RETURN
                ELSE
                END IF
                IF(WQ.EQ.'CP') THEN
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=8
      EXIS8=.FALSE.
 222    CONTINUE
        INQUIRE(FILE='CARDTEXT.DAT',EXIST=EXIS8)
         IF(APPEND) OPEN(UNIT=8,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='CARDTEXT.DAT'
     2  ,STATUS='UNKNOWN',ERR=111)
         IF(.NOT.APPEND) OPEN(UNIT=8,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='CARDTEXT.DAT'
     2  ,STATUS='UNKNOWN',ERR=111)
                RETURN
 111    IF(APPEND) OPEN(UNIT=8,FILE='CARDTEXT.DAT',STATUS='UNKNOWN'
     1,ACCESS='APPEND')
        IF(.NOT.APPEND)
     1OPEN(UNIT=8,FILE='CARDTEXT.DAT',STATUS='UNKNOWN'
     1,ACCESS='SEQUENTIAL')
        IF(IN.NE.8) CALL CLOSE_FILE(8,0)
      GO TO 222
                ELSE
                END IF
                IF(WQ.EQ.'ED') THEN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=9
      EXIS9=.FALSE.
 444    CONTINUE
        INQUIRE(FILE='EDITTEXT.DAT',EXIST=EXIS9)
        IF(APPEND) OPEN(UNIT=9,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='EDITTEXT.DAT'
     2  ,STATUS='UNKNOWN',ERR=333)
        IF(.NOT.APPEND) OPEN(UNIT=9,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE=trim(basePath)//'EDITTEXT.DAT'
     2  ,STATUS='UNKNOWN',ERR=333)
                RETURN
 333    IF(APPEND) OPEN(UNIT=9,FILE='EDITTEXT.DAT',STATUS='UNKNOWN'
     1,ACCESS='APPEND')
        IF(.NOT.APPEND)
     1OPEN(UNIT=9,FILE='EDITTEXT.DAT',STATUS='UNKNOWN'
     1,ACCESS='SEQUENTIAL')
        IF(IN.NE.9) CALL CLOSE_FILE(9,0)
      GO TO 444
                ELSE
                END IF
                IF(WQ.EQ.'NULL') THEN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.10)CALL CLOSE_FILE(10,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=98
                RETURN
                ELSE
                END IF
                IF(WQ.EQ.'PU') THEN
      IF(IN.NE.8) CALL CLOSE_FILE(8,1)
      IF(IN.NE.9) CALL CLOSE_FILE(9,1)
      IF(IN.NE.40)CALL CLOSE_FILE(40,1)
      IF(IN.NE.96)CALL CLOSE_FILE(96,1)
      IF(IN.NE.97)CALL CLOSE_FILE(97,1)
        LASTFIL=OFILN
      OFILN='            '
                OUT=10
      EXIS10=.FALSE.
        INQUIRE(FILE='PUNCH.DAT',EXIST=EXIS10)
 666    IF(APPEND) OPEN(UNIT=10,ACCESS='APPEND',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PUNCH.DAT'
     2  ,STATUS='UNKNOWN',ERR=555)
        IF(.NOT.APPEND) OPEN(UNIT=10,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='PUNCH.DAT'
     2  ,STATUS='UNKNOWN',ERR=555)
                RETURN
 555    IF(APPEND) OPEN(UNIT=9,FILE='PUNCH.DAT',STATUS='UNKNOWN'
     1,ACCESS='APPEND')
        IF(.NOT.APPEND)
     1OPEN(UNIT=9,FILE='PUNCH.DAT',STATUS='UNKNOWN'
     1,ACCESS='SEQUENTIAL')
        IF(IN.NE.10) CALL CLOSE_FILE(10,0)
      GO TO 666
                ELSE
                END IF
                RETURN
          END
      SUBROUTINE PARAMOUT(IPAR,VPAR,TEMPCC,ZMXERROR)
      IMPLICIT NONE
      INTEGER IPAR,J
      REAL*8 VPAR,VALV
      LOGICAL ZMXERROR
      CHARACTER TEMPCC*1024,TEMPER*1024,VALA*23,AA23*23
      INCLUDE 'DATMAI.INC'
      AA23='                       '
C******************************************************************************
C     PARAMETERS
C     PULL OUT PARAMETER NUMBER
      TEMPER=TEMPCC(4:1024)//'    '
C
C     STRIP UP TO 10 LEFT BLANKS
                           DO J=1,10
          IF(TEMPER(1:1).NE.' ') THEN
          TEMPER(1:1024)=TEMPER(2:1024)//' '
                           ELSE
                           END IF
                           END DO
C     FIND BLANK AT END OF PARAMETER NUMBER
                           J=1
                           DO WHILE (TEMPER(J:J).NE.' ')
                           J=J+1
                           END DO
C     J REPRESENTS A BLANK TO THE RIGHT OF THE PARAMETER VALUE
      VALA=AA23
      VALA=TEMPER(1:J-1)
      CALL RIGHTJUST(VALA)
      CALL ATOIZMX(VALA,IPAR,ZMXERROR)
      IF(ZMXERROR) RETURN
C     IVAL IS PARAMETER NUMBER, NOW STRIP OUT PARAMETER VALUE
C
C     STRIP UP TO 10 LEFT BLANKS
                           DO J=1,10
          IF(TEMPER(1:1).NE.' ') THEN
          TEMPER(1:1024)=TEMPER(2:1024)//' '
                           ELSE
                           END IF
                           END DO
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) RETURN
C     VALV IS PARAMETER VALUE
C                  FINISHED BREAKING OUT PARAMETER NUMBER AND VALUES
C******************************************************************************
                           RETURN
                           END
C SUB WWORD.FOR
        SUBROUTINE WWORD
C
        IMPLICIT NONE
C
C       THIS DOES THE "W1" THROUGH "W5" CMD LEVEL COMMANDS
C
      INTEGER VALW,WWCODE
C
      LOGICAL QQ
C
      INCLUDE 'DATMAI.INC'
C
      IF(STI.EQ.1) THEN
      IF(W1CODE.NE.0) WRITE(OUTLYNE,1000) W1CODE
      CALL SHOWIT(0)
      IF(W1CODE.EQ.0) WRITE(OUTLYNE,1001)
      CALL SHOWIT(0)
      IF(W2CODE.NE.0) WRITE(OUTLYNE,2000) W2CODE
      CALL SHOWIT(0)
      IF(W2CODE.EQ.0) WRITE(OUTLYNE,2001)
      CALL SHOWIT(0)
      IF(W3CODE.NE.0) WRITE(OUTLYNE,3000) W3CODE
      CALL SHOWIT(0)
      IF(W3CODE.EQ.0) WRITE(OUTLYNE,3001)
      CALL SHOWIT(0)
      IF(W4CODE.NE.0) WRITE(OUTLYNE,4000) W4CODE
      CALL SHOWIT(0)
      IF(W4CODE.EQ.0) WRITE(OUTLYNE,4001)
      CALL SHOWIT(0)
      IF(W5CODE.NE.0) WRITE(OUTLYNE,5000) W5CODE
      CALL SHOWIT(0)
      IF(W5CODE.EQ.0) WRITE(OUTLYNE,5001)
      CALL SHOWIT(0)
 1001 FORMAT('THE "W1" POINTER IS CURRENTLY NOT DEFINED')
 2001 FORMAT('THE "W2" POINTER IS CURRENTLY NOT DEFINED')
 3001 FORMAT('THE "W3" POINTER IS CURRENTLY NOT DEFINED')
 4001 FORMAT('THE "W4" POINTER IS CURRENTLY NOT DEFINED')
 5001 FORMAT('THE "W5" POINTER IS CURRENTLY NOT DEFINED')
 1000 FORMAT('THE "W1" POINTER IS CURRENTLY = ',I6)
 2000 FORMAT('THE "W2" POINTER IS CURRENTLY = ',I6)
 3000 FORMAT('THE "W3" POINTER IS CURRENTLY = ',I6)
 4000 FORMAT('THE "W4" POINTER IS CURRENTLY = ',I6)
 5000 FORMAT('THE "W5" POINTER IS CURRENTLY = ',I6)

C     DISPLAY CURRENT SETTINGS
                        RETURN
                        ELSE
                        END IF
        IF(DF2.EQ.0.OR.DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
        OUTLYNE=
     1  '"'//WC(1:2)//'" TAKES NO NUMERIC WORD #2 THROUGH #5 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SST.EQ.1) THEN
        OUTLYNE=
     1  '"'//WC(1:2)//'" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(SQ.EQ.1.AND.S1.EQ.1) THEN
        OUTLYNE=
     1  '"'//WC(1:2)//'" TAKES EITHER NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
        OUTLYNE=
     1  'BUT NOT BOTH'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
        IF(SQ.EQ.1.AND.S1.EQ.0) THEN
C
                        QQ=.FALSE.
      IF(WQ.EQ.'A') QQ=.TRUE.
      IF(WQ.EQ.'B') QQ=.TRUE.
      IF(WQ.EQ.'C') QQ=.TRUE.
      IF(WQ.EQ.'D') QQ=.TRUE.
      IF(WQ.EQ.'E') QQ=.TRUE.
      IF(WQ.EQ.'F') QQ=.TRUE.
      IF(WQ.EQ.'G') QQ=.TRUE.
      IF(WQ.EQ.'H') QQ=.TRUE.
      IF(WQ.EQ.'I') QQ=.TRUE.
      IF(WQ.EQ.'J') QQ=.TRUE.
      IF(WQ.EQ.'K') QQ=.TRUE.
      IF(WQ.EQ.'L') QQ=.TRUE.
      IF(WQ.EQ.'M') QQ=.TRUE.
      IF(WQ.EQ.'N') QQ=.TRUE.
      IF(WQ.EQ.'ITEST') QQ=.TRUE.
      IF(WQ.EQ.'JTEST') QQ=.TRUE.
      IF(WQ.EQ.'KTEST') QQ=.TRUE.
      IF(WQ.EQ.'LTEST') QQ=.TRUE.
      IF(WQ.EQ.'MTEST') QQ=.TRUE.
      IF(WQ.EQ.'NTEST') QQ=.TRUE.
      IF(WQ.EQ.'X') QQ=.TRUE.
      IF(WQ.EQ.'Y') QQ=.TRUE.
      IF(WQ.EQ.'Z') QQ=.TRUE.
      IF(WQ.EQ.'T') QQ=.TRUE.
      IF(WQ.EQ.'IX') QQ=.TRUE.
      IF(WQ.EQ.'IY') QQ=.TRUE.
      IF(WQ.EQ.'IZ') QQ=.TRUE.
      IF(WQ.EQ.'IT') QQ=.TRUE.
      IF(WQ.EQ.'ACC') QQ=.TRUE.
        IF(.NOT.QQ) THEN
      OUTLYNE='INVALID QUALIFIER USED WITH "'//WC(1:2)//'" COMMAND'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                      ELSE
C     NO QUALIFIER
                      END IF
        IF(S1.EQ.1.AND.SQ.EQ.0) THEN
                 VALW=INT(W1)
        IF(VALW.LT.0.OR.VALW.GT.400) THEN
      WRITE(OUTLYNE,99) INT(VALW)
 99   FORMAT('GENERAL PURPOSE STORAGE REGISTER ',I6,' DOES NOT EXIST')
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
              WWCODE=0
      IF(SQ.EQ.0.AND.DF1.EQ.1) WWCODE=-92
      IF(WQ.EQ.'A') WWCODE=-100
      IF(WQ.EQ.'B') WWCODE=-99
      IF(WQ.EQ.'C') WWCODE=-98
      IF(WQ.EQ.'D') WWCODE=-97
      IF(WQ.EQ.'E') WWCODE=-96
      IF(WQ.EQ.'F') WWCODE=-95
      IF(WQ.EQ.'G') WWCODE=-94
      IF(WQ.EQ.'H') WWCODE=-93
        IF(WQ.EQ.'ACC'.OR.WQ.EQ.'X')
     1  WWCODE=-92
        IF(WQ.EQ.'Y') WWCODE=-91
        IF(WQ.EQ.'Z') WWCODE=-90
        IF(WQ.EQ.'T') WWCODE=-89
        IF(WQ.EQ.'IX') WWCODE=-88
        IF(WQ.EQ.'IY') WWCODE=-87
        IF(WQ.EQ.'IZ') WWCODE=-86
        IF(WQ.EQ.'IT') WWCODE=-85
        IF(WQ.EQ.'I') WWCODE=-84
        IF(WQ.EQ.'ITEST') WWCODE=-83
        IF(WQ.EQ.'J') WWCODE=-82
        IF(WQ.EQ.'JTEST') WWCODE=-81
        IF(WQ.EQ.'LASTX') WWCODE=-80
        IF(WQ.EQ.'LASTIX') WWCODE=-79
        IF(WQ.EQ.'K') WWCODE=-78
        IF(WQ.EQ.'L') WWCODE=-77
        IF(WQ.EQ.'M') WWCODE=-76
        IF(WQ.EQ.'N') WWCODE=-75
        IF(WQ.EQ.'KTEST') WWCODE=-74
        IF(WQ.EQ.'LTEST') WWCODE=-73
        IF(WQ.EQ.'MTEST') WWCODE=-72
        IF(WQ.EQ.'NTEST') WWCODE=-71
C
      IF(S1.EQ.1) THEN
              WWCODE=INT(W1)
                      END IF
          IF(WC.EQ.'W1') W1CODE=WWCODE
          IF(WC.EQ.'W2') W2CODE=WWCODE
          IF(WC.EQ.'W3') W3CODE=WWCODE
          IF(WC.EQ.'W4') W4CODE=WWCODE
          IF(WC.EQ.'W5') W5CODE=WWCODE

                        RETURN
                        END
C SUB UPPRLN.FOR
        SUBROUTINE UPPRLN
C
C     THIS CAUSES THE CURRENT LENS TO BECOME THE PERMANENT LENS
C     IT IS ONLY CALLED FROM CFG1 JUST BEFORE GOING TO ANOTHER CFG
C
      IMPLICIT NONE
C
        INCLUDE 'DATSUB.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATCFG.INC'
        INCLUDE 'DATMAC.INC'
C
        IF(F12.EQ.1) THEN
                CALL PTOC
C
C               FINISHED COPYING CFG 1 CURLENS TO PERLENS
C
                F6=1
                F1=0
                F22=1
                       LNSTYP=2
                       CALL LNSEOS
                        ELSE
                        END IF
                        RETURN
                        END
C SUB TABLE.FOR
        SUBROUTINE TABLE
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS "TABLE" AND DOES ALL THE TABLE
C       COMMANDS FROM THE CMD LEVEL.
C
        CHARACTER*12 HCOL(0:9),HCOL2(0:9),
     1  HROW(0:100),HROW2(0:100)
        CHARACTER AI*3,
     1  BLANK*12,HEADL1*132,
     2  HEADL2*132,HEADL3*80,HEADL4*80,BL12*12,LINE1*132,
     3  LINE3*80,A1*12,A2*12,A3*12,A4*12,
     4  A5*12,A6*12,A7*12,A8*12,A9*12
C
        INTEGER NROW,NCOL,
     6  I,J,FROW,FCOL
C
        REAL*8
     1  TABL(0:100,0:9)
C
        COMMON/HDTAB/HCOL,HCOL2,HROW,HROW2
C
        COMMON/TTABLE1/TABL
        LOGICAL TABEXIST
        COMMON/EXISTAB/TABEXIST
C
      INCLUDE 'DATMAI.INC'
C
        BLANK='            '
        BL12='            '
C
C       "ROWHD","ROWHD2","COLHD" AND "COLHD2" COMMANDS
C
C       CHECK FOR STRING INPUT
C
        IF(WC.EQ.'COLHD'.OR.WC.EQ.'COLHD2'.OR.WC.EQ.'ROWHD'
     1  .OR.WC.EQ.'ROWHD2') THEN
        IF(SST.EQ.0) THEN
        WRITE(OUTLYNE,*)
     1  WC,' COMMAND REQUIRES EXPLICIT STRING INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C       CHECK FOR QUALIFIER INPUT
C
        IF(WC.EQ.'COLHD'.OR.WC.EQ.'COLHD2'.OR.WC.EQ.'ROWHD'
     1  .OR.WC.EQ.'ROWHD2') THEN
        IF(SQ.EQ.0) THEN
        WRITE(OUTLYNE,*)
     1  WC,' COMMAND REQUIRES EXPLICIT QUALIFIER WORD INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C       CHECK FOR QUALIFIER INPUT
C
        IF(WC.EQ.'COLHD'.OR.WC.EQ.'COLHD2'.OR.WC.EQ.'ROWDH'
     1  .OR.WC.EQ.'ROWHD2') THEN
        IF(SN.EQ.1) THEN
        WRITE(OUTLYNE,*)
     1  WC,' COMMAND TAKES NO NUMERIC WORD INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C       "TABLE" COMMAND
C       VALID QUALIFIERS ARE "CLEAR","SETUP","PUT","GET",
C       "PRINT1", "PRINT2","PRINT3", "SAVE" AND "RELOAD"
C
C       CHECK FOR STRING INPUT
C
        IF(WC.EQ.'TABLE'.AND.SST.EQ.1) THEN
        WRITE(OUTLYNE,*)'"TABLE" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
C       CHECK FOR NO QUALIFIER
C
        IF(WC.EQ.'TABLE'.AND.SQ.EQ.0) THEN
        WRITE(OUTLYNE,*)
     1'"TABLE" REQUIRES EXPLICIT QUALIFIER WORD INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
C       CHECK FOR INVALID QUALIFIER
C
        IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
        IF(WQ.NE.'SETUP'.AND.WQ.NE.'CLEAR'.AND.WQ.NE.'PUT'.AND.
     1  WQ.NE.'GET'.AND.WQ.NE.'PRINT1'.AND.WQ.NE.'SAVE'.AND.WQ.NE.
     2  'RELOAD'.AND.WQ.NE.'PRINT2'.AND.WQ.NE.'PRINT3') THEN
        WRITE(OUTLYNE,*)
     1'INVALID QUALIFIER WORD USED WITH "TABLE" COMMAND'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
C
C       CHECK FOR NUMERIC INPUT
C
        IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
        IF(WQ.EQ.'SETUP'.OR.WQ.EQ.'CLEAR'.OR.
     1  WQ.EQ.'PRINT1'.OR.WQ.EQ.'PRINT2'.OR.WQ.EQ.'SAVE'.OR.WQ.EQ.
     2  'RELOAD'.OR.WQ.EQ.'PRINT3') THEN
        IF(SN.EQ.1) THEN
        WRITE(OUTLYNE,*)
     1'"TABLE SETUP", "TABLE CLEAR", "TABLE PRINT1/2/3"'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  '"TABLE SAVE" AND "TABLE RELOAD" TAKE NO NUMERIC WORD INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
                        END IF
C
C       EXTRA NW3,NW4 AND NW5 INPUT
C
        IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
        IF(WQ.EQ.'GET') THEN
        IF(DF3.EQ.0.OR.DF4.EQ.0.OR.DF5.EQ.0) THEN
        WRITE(OUTLYNE,*)
     1  '"TABLE GET"'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'ONLY TAKES NUMERIC WORD 1 AND 2 INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
        IF(WQ.EQ.'PUT') THEN
        IF(DF4.EQ.0.OR.DF5.EQ.0) THEN
        WRITE(OUTLYNE,*)
     1  '"TABLE PUT"'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'TAKES NO NUMERIC WORD 4 OR 5 INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
                        END IF
C
C       EXPLICIT NW1 AND NW2
C
        IF(WC.EQ.'TABLE'.AND.SQ.EQ.1) THEN
        IF(WQ.EQ.'PUT'.OR.WQ.EQ.'GET') THEN
        IF(DF1.EQ.1.OR.DF2.EQ.1) THEN
        WRITE(OUTLYNE,*)
     1  '"TABLE PUT" AND "TABLE GET"'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  'REQUIRE EXPLICIT NUMERIC WORD 1 AND 2 INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
                        END IF
C
C
C       TABLE SETUP
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'SETUP') THEN
                        NCOL=0
                        NROW=0
                        DO I=0,9
                        DO J=0,100
                        TABL(J,I)=0.0D0
                        END DO
                        END DO
                        DO I=0,9
                        CALL ITOAA(I,AI)
                        HCOL(I)=' COLUMN '//AI//' '
                        HCOL2(I)='            '
                        END DO
                        DO I=0,100
                        CALL ITOAA(I,AI)
                        HROW(I)='  ROW  '//AI//'  '
                        HROW2(I)='            '
                        END DO
        WRITE(OUTLYNE,*)
     1  'THE NEW "TABLE" HAS BEEN SETUP'
      CALL SHOWIT(1)
        TABEXIST=.TRUE.
                        RETURN
                        END IF
C
C
C       TABLE CLEAR
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'CLEAR') THEN
                        NCOL=0
                        NROW=0
                        DO I=0,9
                        DO J=0,100
                        TABL(J,I)=0.0D0
                        END DO
                        END DO
        WRITE(OUTLYNE,*)
     1  'THE CURRENT "TABLE" HAS BEEN CLEARED'
      CALL SHOWIT(1)
                        RETURN
                        END IF
C
C       TABLE PUT
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'PUT') THEN
        IF(.NOT.TABEXIST) THEN
        WRITE(OUTLYNE,*) '"TABLE SETUP" MUST BE ISSUED TO SET UP'
        CALL SHOWIT(1)
        WRITE(OUTLYNE,*) 'THE TABLE BEFORE DATA MAY BE ENTERED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF

        IF(DF3.EQ.0) THEN
C       EXPLICIT NW3 INPUT, SET TABLE ENTRY TO THAT VALUE
                TABL(INT(W1),INT(W2))=W3
        WRITE(OUTLYNE,*)
     1  'NUMERIC WORD 3 VALUE MOVED TO TABLE ROW ',INT(W1)
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  '                              TABLE COL ',INT(W2)
      CALL SHOWIT(1)
                        ELSE
C       USE ACCUMULATOR VALUE FOR TABLE ENTRY

                TABL(INT(W1),INT(W2))=REG(9)
        WRITE(OUTLYNE,*)
     1  'ACCUMULATOR CONTENTS MOVED TO TABLE ROW ',INT(W1)
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  '                              TABLE COL ',INT(W2)
      CALL SHOWIT(1)
                        END IF
C       SET OCCUPANCY FLAG TO TRUE
        IF(NROW.LT.INT(W1)) NROW=INT(W1)
        IF(NCOL.LT.INT(W2)) NCOL=INT(W2)
                        RETURN
                        END IF
C
C       TABLE GET
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'GET') THEN
        IF(.NOT.TABEXIST) THEN
        WRITE(OUTLYNE,*) 'NO TABLE EXISTS YET'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
               REG(40)=REG(9)
                REG(9)=TABL(INT(W1),INT(W2))
        WRITE(OUTLYNE,*)
     1  'CONTENTS OF TABLE ROW ',INT(W1)
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1  '            TABLE COL ',INT(W2)
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'MOVED TO ACCUMULATOR'
      CALL SHOWIT(1)
                        RETURN
                        END IF
C
C       ROWHD OR ROWHD2
        IF(WC.EQ.'ROWHD') THEN
                I=FROW(WQ)
        IF(NROW.LT.I) NROW=I
                HROW(I)=WS(1:12)
                RETURN
                END IF
        IF(WC.EQ.'ROWHD2') THEN
                I=FROW(WQ)
        IF(NROW.LT.I) NROW=I
                HROW2(I)=WS(1:12)
                RETURN
                END IF
C
C       COLHD OR COLHD2
        IF(WC.EQ.'COLHD') THEN
                I=FCOL(WQ)
        IF(NCOL.LT.I) NCOL=I
                HCOL(I)=WS(1:12)
                RETURN
                END IF
        IF(WC.EQ.'COLHD2') THEN
                I=FCOL(WQ)
        IF(NCOL.LT.I) NCOL=I
                HCOL2(I)=WS(1:12)
                RETURN
                END IF
C
C       TABLE SAVE
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'SAVE') THEN
        IF(.NOT.TABEXIST) THEN
        WRITE(OUTLYNE,*) 'NO TABLE EXISTS TO BE SAVED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C       OPEN AND WRITE TO FILE
        PRINT *, "About to open TAB.DAT in ZMX2PRG"
        OPEN(UNIT=36,ACCESS='DIRECT',FILE='TAB.DAT',
     1  FORM='UNFORMATTED',RECL=(132*NRECL),STATUS='UNKNOWN')
C
        WRITE(UNIT=36,REC=1) NROW,NCOL
        WRITE(UNIT=36,REC=2) HCOL(0),HCOL2(0)
        WRITE(UNIT=36,REC=3) HCOL(1),HCOL2(1)
        WRITE(UNIT=36,REC=4) HCOL(2),HCOL2(2)
        WRITE(UNIT=36,REC=5) HCOL(3),HCOL2(3)
        WRITE(UNIT=36,REC=6) HCOL(4),HCOL2(4)
        WRITE(UNIT=36,REC=7) HCOL(5),HCOL2(5)
        WRITE(UNIT=36,REC=8) HCOL(6),HCOL2(6)
        WRITE(UNIT=36,REC=9) HCOL(7),HCOL2(7)
        WRITE(UNIT=36,REC=10) HCOL(8),HCOL2(8)
        WRITE(UNIT=36,REC=11) HCOL(9),HCOL2(9)

                        DO 210 I=0,100
        WRITE(UNIT=36,REC=I+12) HROW(I),HROW2(I),TABL(I,0),
     1  TABL(I,1),TABL(I,2),TABL(I,3),TABL(I,4),TABL(I,5),
     2  TABL(I,6),TABL(I,7),TABL(I,8),TABL(I,9)
 210                    CONTINUE
        CALL CLOSE_FILE(36,1)
        WRITE(OUTLYNE,*)
     1  'THE CURRENT "TABLE" HAS BEEN SAVED IN "TAB.DAT"'
      CALL SHOWIT(1)
                        RETURN
                        END IF
C       TABLE RELOAD
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'RELOAD') THEN
C       OPEN AND READ FROM FILE
        OPEN(UNIT=36,ACCESS='DIRECT',FILE='TAB.DAT',
     1  FORM='UNFORMATTED',RECL=(132*NRECL),STATUS='UNKNOWN')
C
        READ(UNIT=36,REC=1) NROW,NCOL
        READ(UNIT=36,REC=2) HCOL(0),HCOL2(0)
        READ(UNIT=36,REC=3) HCOL(1),HCOL2(1)
        READ(UNIT=36,REC=4) HCOL(2),HCOL2(2)
        READ(UNIT=36,REC=5) HCOL(3),HCOL2(3)
        READ(UNIT=36,REC=6) HCOL(4),HCOL2(4)
        READ(UNIT=36,REC=7) HCOL(5),HCOL2(5)
        READ(UNIT=36,REC=8) HCOL(6),HCOL2(6)
        READ(UNIT=36,REC=9) HCOL(7),HCOL2(7)
        READ(UNIT=36,REC=10) HCOL(8),HCOL2(8)
        READ(UNIT=36,REC=11) HCOL(9),HCOL2(9)

                        DO 310 I=0,100
        READ(UNIT=36,REC=I+12) HROW(I),HROW2(I),TABL(I,0),
     1  TABL(I,1),TABL(I,2),TABL(I,3),TABL(I,4),TABL(I,5),
     2  TABL(I,6),TABL(I,7),TABL(I,8),TABL(I,9)
 310                    CONTINUE
        CALL CLOSE_FILE(36,1)
        WRITE(OUTLYNE,*)
     1  'THE CURRENT "TABLE" HAS BEEN RELOADED FROM "TAB.DAT"'
      CALL SHOWIT(1)
        TABEXIST=.TRUE.
                        RETURN
                        END IF
C       TABLE PRINT
        IF(WC.EQ.'TABLE'.AND.WQ.EQ.'PRINT1'.OR.
     1  WC.EQ.'TABLE'.AND.WQ.EQ.'PRINT2'.OR.
     3  WC.EQ.'TABLE'.AND.WQ.EQ.'PRINT3') THEN
        IF(.NOT.TABEXIST) THEN
        WRITE(OUTLYNE,*) 'NO TABLE EXISTS TO BE PRINTED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(NCOL.EQ.0.AND.NROW.EQ.0) THEN
        WRITE(OUTLYNE,*) 'NO TABLE EXISTS TO BE PRINTED'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C       PRINT TABLE DATA
C       HERE THE HEADINGS AND VALUES ARE OUTPUT. 3 FORMS ARE USED,
C       ONE FOR 80 COLS(PRINT1) AND ONE FOR 132 COLS(PRINT2) SPACE DELIMITED
C       AND ONE FOR 132 COLS(PRINT3) COMMA DELIMITED
C
        IF(WQ.EQ.'PRINT1') THEN
C       DATA DOES EXIST TO PRINT
C
C       PRINT FIRST 5 COLUMNS OF THE TABLE
C
C       PRINT COLUMN HEADING
        HEADL3='             '//HCOL(1)//' '//HCOL(2)//' '//
     1 HCOL(3)//' '//HCOL(4)//' '//HCOL(5)
        HEADL4='             '//HCOL2(1)//' '//HCOL2(2)//' '//
     1 HCOL2(3)//' '//HCOL2(4)//' '//HCOL2(5)
 1002   FORMAT(A79)
C       PRINT HEADING
        WRITE(OUTLYNE,1002) HEADL3(1:79)
        CALL SHOWIT(0)
        WRITE(OUTLYNE,1002) HEADL4(1:79)
        CALL SHOWIT(0)
                                DO I=1,NROW
        WRITE(OUTLYNE,123) HROW(I),TABL(I,1),TABL(I,2)
     1  ,TABL(I,3),TABL(I,4),TABL(I,5)
        CALL SHOWIT(0)
 123    FORMAT(A12,5(1X,G12.5))
        IF(HROW2(I).NE.BL12) THEN
        WRITE(OUTLYNE,453) HROW2(I)
        CALL SHOWIT(0)
 453    FORMAT(A12)
                                END IF
                                END DO
C       PRINT LAST 4 COLUMNS IF NOT BLANK
        IF(NCOL.GT.5) THEN
C
C       PRINT COLUMN HEADING
        HEADL3='             '//HCOL(6)//' '//HCOL(7)//' '//
     1 HCOL(8)//' '//HCOL(9)
        HEADL4='             '//HCOL2(6)//' '//HCOL2(7)//' '//
     1 HCOL2(8)//' '//HCOL2(9)
        WRITE(OUTLYNE,2222)
        CALL SHOWIT(0)
        WRITE(OUTLYNE,1002) HEADL3(1:79)
        CALL SHOWIT(0)
        WRITE(OUTLYNE,1002) HEADL4(1:79)
        CALL SHOWIT(0)
                                DO I=1,NROW
C       PRINT EACH ROW INCLUDING ROW HEADING. ZERO PRINTS AS BLANK
C       AND A LINE IS SKIPPED IN THE DATA IF A SECOND LINE OF
C       ROW HEADING DESCRIPTION IS USED.
C
C       PRINT THE LINE
        WRITE(OUTLYNE,454) HROW(I),TABL(I,6),TABL(I,7),
     1  TABL(I,8),TABL(I,9)
        CALL SHOWIT(0)
 454    FORMAT(A12,4(1X,G12.5))
        IF(HROW2(I).NE.BL12) THEN
        WRITE(OUTLYNE,455) HROW2(I)(1:12)
        CALL SHOWIT(0)
 455    FORMAT(A12)
                                END IF
                                END DO
                                END IF
                                RETURN
                                END IF
C
C       PRINT2
        IF(WQ.EQ.'PRINT2') THEN
        IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        OUTLYNE='"TABLE PRINT2" IS FOR USE WHEN OUTPUTING TO A DISK'
        CALL SHOWIT(1)
        OUTLYNE='FILE ONLY'
        CALL SHOWIT(1)
                                CALL MACFAL
                                RETURN
                                END IF
C       DATA DOES EXIST TO PRINT
C
C       PRINT FIRST 5 COLUMNS OF THE TABLE
C
C       PRINT COLUMN HEADING
        HEADL1='             '//HCOL(1)//'      '//HCOL(2)//'      '//
     1 HCOL(3)//'      '//HCOL(4)//'      '//HCOL(5)
        HEADL2='             '//HCOL2(1)//'      '//HCOL2(2)//'      '//
     1 HCOL2(3)//'      '//HCOL2(4)//'      '//HCOL2(5)
 1012   FORMAT(A131)
C       PRINT HEADING
        WRITE(OUTLYNE,1012) HEADL1(1:131)
        CALL SHOWIT(2)
        WRITE(OUTLYNE,1012) HEADL2(1:131)
        CALL SHOWIT(2)
                                DO I=1,NROW
        WRITE(OUTLYNE,124) HROW(I),TABL(I,1),TABL(I,2)
     1  ,TABL(I,3),TABL(I,4),TABL(I,5)
        CALL SHOWIT(2)
 124    FORMAT(A12,5(1X,G17.10))
        IF(HROW2(I).NE.BL12) THEN
        WRITE(OUTLYNE,453) HROW2(I)
        CALL SHOWIT(2)
                                END IF
                                END DO
C       PRINT LAST 4 COLUMNS IF NOT BLANK
        IF(NCOL.GT.5) THEN
        WRITE(OUTLYNE,2222)
        CALL SHOWIT(2)
C
C       PRINT COLUMN HEADING
        HEADL1='             '//HCOL(6)//'      '//HCOL(7)//'      '//
     1 HCOL(8)//'      '//HCOL(9)
        HEADL2='             '//HCOL2(6)//'      '//HCOL2(7)//'      '//
     1 HCOL2(8)//'      '//HCOL2(9)
C       PRINT HEADING
        WRITE(OUTLYNE,1012) HEADL1(1:131)
        CALL SHOWIT(2)
        WRITE(OUTLYNE,1012) HEADL2(1:131)
        CALL SHOWIT(2)
                                DO I=1,NROW
C       PRINT EACH ROW INCLUDING ROW HEADING. ZERO PRINTS AS BLANK
C       AND A LINE IS SKIPPED IN THE DATA IF A SECOND LINE OF
C       ROW HEADING DESCRIPTION IS USED.
C
C       PRINT THE LINE
        WRITE(OUTLYNE,456) HROW(I),TABL(I,6),TABL(I,7),
     1  TABL(I,8),TABL(I,9)
        CALL SHOWIT(2)
 456    FORMAT(A12,4(1X,G17.10))
        IF(HROW2(I).NE.BL12) THEN
        WRITE(OUTLYNE,454) HROW2(I)(1:12)
        CALL SHOWIT(2)
                                END IF
                                END DO
                                END IF
                                RETURN
                                END IF
C
C        PRINT 3
        IF(WQ.EQ.'PRINT3') THEN
        IF(OUT.NE.97) THEN
        OUTLYNE='"TABLE PRINT3" IS FOR USE WHEN OUTPUTING TO A'
        CALL SHOWIT(1)
        OUTLYNE='NAMED DISK FILE AS IN "OUT FILE (FILE NAME)"'
        CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C       DATA DOES EXIST TO PRINT
                                DO I=1,NROW
        WRITE(OUTLYNE_LONG,1124) TABL(I,1),TABL(I,2)
     1  ,TABL(I,3),TABL(I,4),TABL(I,5),TABL(I,6),TABL(I,7)
     2  ,TABL(I,8),TABL(I,9)
        CALL SHOWIT(20)
 1124    FORMAT(8(D23.15,','),D23.15)
                                END DO
                                END IF
                                RETURN
                                END IF
 2222   FORMAT(1X)
                                END
C SUB FCOL.FOR
      FUNCTION FCOL(WK)
C
        IMPLICIT NONE
C
        CHARACTER WK*8
C
        INTEGER FCOL
C
      INCLUDE 'DATMAI.INC'
C
        IF(
     1  WK.NE.'C1'.AND.WK.NE.'C2'.AND.WK.NE.'C3'.AND.WK.NE.'C4'.AND.
     2  WK.NE.'C5'.AND.WK.NE.'C6'.AND.WK.NE.'C7'.AND.WK.NE.'C8'.AND.
     3  WK.NE.'C9') THEN
                                FCOL=0
        WRITE(OUTLYNE,*)
     1'INVALID QUALIFIER USED WITH "COLHD" OR "COLHD2"'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                                RETURN
                                END IF
C
                IF(WK.EQ.'C1') FCOL=1
                IF(WK.EQ.'C2') FCOL=2
                IF(WK.EQ.'C3') FCOL=3
                IF(WK.EQ.'C4') FCOL=4
                IF(WK.EQ.'C5') FCOL=5
                IF(WK.EQ.'C6') FCOL=6
                IF(WK.EQ.'C7') FCOL=7
                IF(WK.EQ.'C8') FCOL=8
                IF(WK.EQ.'C9') FCOL=9
                               RETURN
                                END
C SUB FROW.FOR
      FUNCTION FROW(WK)
C
        IMPLICIT NONE
C
        CHARACTER WK*8
C
        INTEGER FROW
C
      INCLUDE 'DATMAI.INC'
C
        IF(WK.NE.'R1'.AND.WK.NE.'R2'.AND.WK.NE.'R3'.AND.WK.NE.'R4'.AND.
     2  WK.NE.'R5'.AND.WK.NE.'R6'.AND.WK.NE.'R7'.AND.WK.NE.'R8'.AND.
     2  WK.NE.'R9'.AND.WK.NE.'R10'.AND.WK.NE.'R11'.AND.WK.NE.'R12'.AND.
     2  WK.NE.'R13'.AND.WK.NE.'R14'.AND.WK.NE.'R15'.AND.WK.NE.'R16'.AND.
     2  WK.NE.'R17'.AND.WK.NE.'R18'.AND.WK.NE.'R19'.AND.WK.NE.'R16'.AND.
     2  WK.NE.'R21'.AND.WK.NE.'R22'.AND.WK.NE.'R23'.AND.WK.NE.'R24')THEN
                                GO TO 4
                                END IF
                                GO TO 100
 4     IF(WK.NE.'R25'.AND.WK.NE.'R26'.AND.WK.NE.'R27'.AND.WK.NE.'R28'
     2  .AND.
     2  WK.NE.'R29'.AND.WK.NE.'R30'.AND.WK.NE.'R31'.AND.WK.NE.'R32'.AND.
     2  WK.NE.'R33'.AND.WK.NE.'R34'.AND.WK.NE.'R35'.AND.WK.NE.'R36'.AND.
     2  WK.NE.'R37'.AND.WK.NE.'R38'.AND.WK.NE.'R39'.AND.WK.NE.'R40')THEN
                                GO TO 5
                                END IF
                                GO TO 100
 5      IF(WK.NE.'R41'.AND.WK.NE.'R42'.AND.WK.NE.'R43'.AND.WK.NE.'R44'
     1  .AND.
     2  WK.NE.'R45'.AND.WK.NE.'R46'.AND.WK.NE.'R47'.AND.WK.NE.'R48'.AND.
     2  WK.NE.'R49'.AND.WK.NE.'R50'.AND.WK.NE.'R51'.AND.WK.NE.'R52'.AND.
     2  WK.NE.'R53'.AND.WK.NE.'R54'.AND.WK.NE.'R55'.AND.WK.NE.'R56'.AND.
     2  WK.NE.'R57'.AND.WK.NE.'R58'.AND.WK.NE.'R59'.AND.WK.NE.'R60'.AND.
     2  WK.NE.'R61'.AND.WK.NE.'R62'.AND.WK.NE.'R63'.AND.WK.NE.'R64'.AND.
     2  WK.NE.'R65'.AND.WK.NE.'R66'.AND.WK.NE.'R67'.AND.WK.NE.'R68'.AND.
     2  WK.NE.'R69'.AND.WK.NE.'R70'.AND.WK.NE.'R71'.AND.WK.NE.'R72'.AND.
     2  WK.NE.'R73'.AND.WK.NE.'R74'.AND.WK.NE.'R75'.AND.WK.NE.'R76')THEN
                                GO TO 10
                                END IF
                                GO TO 100
 10     IF(
     2  WK.NE.'R77'.AND.WK.NE.'R78'.AND.WK.NE.'R79'.AND.WK.NE.'R80'.AND.
     2  WK.NE.'R81'.AND.WK.NE.'R82'.AND.WK.NE.'R83'.AND.WK.NE.'R84'.AND.
     2  WK.NE.'R85'.AND.WK.NE.'R86'.AND.WK.NE.'R87'.AND.WK.NE.'R88'.AND.
     2  WK.NE.'R89'.AND.WK.NE.'R90'.AND.WK.NE.'R91'.AND.WK.NE.'R92'.AND.
     2  WK.NE.'R93'.AND.WK.NE.'R94'.AND.WK.NE.'R95'.AND.WK.NE.'R96'.AND.
     2  WK.NE.'R97'.AND.WK.NE.'R98'.AND.WK.NE.'R99'.AND.WK.NE.'R100'
     2  )THEN
                                GO TO 15
                                END IF
                                GO TO 100
 15    WRITE(OUTLYNE,*)
     1'INVALID QUALIFIER USED WITH "ROWHD" OR "ROWHD2"'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                                RETURN
 100                            CONTINUE
                                FROW=0
C
                IF(WK.EQ.'R1')  FROW=1
                IF(WK.EQ.'R2')  FROW=2
                IF(WK.EQ.'R3')  FROW=3
                IF(WK.EQ.'R4')  FROW=4
                IF(WK.EQ.'R5')  FROW=5
                IF(WK.EQ.'R6')  FROW=6
                IF(WK.EQ.'R7')  FROW=7
                IF(WK.EQ.'R8')  FROW=8
                IF(WK.EQ.'R9')  FROW=9
                IF(WK.EQ.'R10') FROW=10
                IF(WK.EQ.'R11') FROW=11
                IF(WK.EQ.'R12') FROW=12
                IF(WK.EQ.'R13') FROW=13
                IF(WK.EQ.'R14') FROW=14
                IF(WK.EQ.'R15') FROW=15
                IF(WK.EQ.'R16') FROW=16
                IF(WK.EQ.'R17') FROW=17
                IF(WK.EQ.'R18') FROW=18
                IF(WK.EQ.'R19') FROW=19
                IF(WK.EQ.'R20') FROW=20
                IF(WK.EQ.'R21') FROW=21
                IF(WK.EQ.'R22') FROW=22
                IF(WK.EQ.'R23') FROW=23
                IF(WK.EQ.'R24') FROW=24
                IF(WK.EQ.'R25') FROW=25
                IF(WK.EQ.'R26') FROW=26
                IF(WK.EQ.'R27') FROW=27
                IF(WK.EQ.'R28') FROW=28
                IF(WK.EQ.'R29') FROW=29
                IF(WK.EQ.'R30') FROW=30
                IF(WK.EQ.'R31') FROW=31
                IF(WK.EQ.'R32') FROW=32
                IF(WK.EQ.'R33') FROW=33
                IF(WK.EQ.'R34') FROW=34
                IF(WK.EQ.'R35') FROW=35
                IF(WK.EQ.'R36') FROW=36
                IF(WK.EQ.'R37') FROW=37
                IF(WK.EQ.'R38') FROW=38
                IF(WK.EQ.'R39') FROW=39
                IF(WK.EQ.'R40') FROW=40
                IF(WK.EQ.'R41') FROW=41
                IF(WK.EQ.'R42') FROW=42
                IF(WK.EQ.'R43') FROW=43
                IF(WK.EQ.'R44') FROW=44
                IF(WK.EQ.'R45') FROW=45
                IF(WK.EQ.'R46') FROW=46
                IF(WK.EQ.'R47') FROW=47
                IF(WK.EQ.'R48') FROW=48
                IF(WK.EQ.'R49') FROW=49
                IF(WK.EQ.'R50') FROW=50
                IF(WK.EQ.'R51') FROW=51
                IF(WK.EQ.'R52') FROW=52
                IF(WK.EQ.'R53') FROW=53
                IF(WK.EQ.'R54') FROW=54
                IF(WK.EQ.'R55') FROW=55
                IF(WK.EQ.'R56') FROW=56
                IF(WK.EQ.'R57') FROW=57
                IF(WK.EQ.'R58') FROW=58
                IF(WK.EQ.'R59') FROW=59
                IF(WK.EQ.'R60') FROW=60
                IF(WK.EQ.'R61') FROW=61
                IF(WK.EQ.'R62') FROW=62
                IF(WK.EQ.'R63') FROW=63
                IF(WK.EQ.'R64') FROW=64
                IF(WK.EQ.'R65') FROW=65
                IF(WK.EQ.'R66') FROW=66
                IF(WK.EQ.'R67') FROW=67
                IF(WK.EQ.'R68') FROW=68
                IF(WK.EQ.'R69') FROW=69
                IF(WK.EQ.'R70') FROW=70
                IF(WK.EQ.'R71') FROW=71
                IF(WK.EQ.'R72') FROW=72
                IF(WK.EQ.'R73') FROW=73
                IF(WK.EQ.'R74') FROW=74
                IF(WK.EQ.'R75') FROW=75
                IF(WK.EQ.'R76') FROW=76
                IF(WK.EQ.'R77') FROW=77
                IF(WK.EQ.'R78') FROW=78
                IF(WK.EQ.'R89') FROW=89
                IF(WK.EQ.'R80') FROW=80
                IF(WK.EQ.'R81') FROW=81
                IF(WK.EQ.'R82') FROW=82
                IF(WK.EQ.'R83') FROW=83
                IF(WK.EQ.'R84') FROW=84
                IF(WK.EQ.'R85') FROW=85
                IF(WK.EQ.'R86') FROW=86
                IF(WK.EQ.'R87') FROW=87
                IF(WK.EQ.'R88') FROW=88
                IF(WK.EQ.'R89') FROW=89
                IF(WK.EQ.'R90') FROW=90
                IF(WK.EQ.'R91') FROW=91
                IF(WK.EQ.'R92') FROW=92
                IF(WK.EQ.'R93') FROW=93
                IF(WK.EQ.'R94') FROW=94
                IF(WK.EQ.'R95') FROW=95
                IF(WK.EQ.'R96') FROW=96
                IF(WK.EQ.'R97') FROW=97
                IF(WK.EQ.'R98') FROW=98
                IF(WK.EQ.'R99') FROW=99
                IF(WK.EQ.'R100') FROW=100
                                RETURN
                                END
C SUB STACK.FOR
        SUBROUTINE STACK
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO MANIPULATE THE REAL AND IMAGINARY
C       RPN STACK
C
        CHARACTER CSTRING*23
C
      INCLUDE 'DATMAI.INC'
      INTEGER I
C
C       X = REG(9),Y=REG(10),Z=REG(11),T=REG(12)
        IF(SST.EQ.1.OR.SQ.EQ.1.OR.SN.EQ.1) THEN
        WRITE(OUTLYNE,*)'STACK COMMANDS TAKE NO EXPLICIT INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        IF(WC.EQ.'RUP') THEN
        REG(50)=REG(12)
        REG(12)=REG(11)
        REG(11)=REG(10)
        REG(10)=REG(9)
        REG(40)=REG(9)
        REG(9)=REG(50)
                        RETURN
                        END IF
        IF(WC.EQ.'IRUP') THEN
        REG(49)=REG(16)
        REG(16)=REG(15)
        REG(15)=REG(14)
        REG(14)=REG(13)
        REG(30)=REG(13)
        REG(13)=REG(49)
                        RETURN
                        END IF
        IF(WC.EQ.'CRUP') THEN
        REG(49)=REG(16)
        REG(16)=REG(15)
        REG(15)=REG(14)
        REG(14)=REG(13)
        REG(30)=REG(13)
        REG(13)=REG(49)
        REG(50)=REG(12)
        REG(12)=REG(11)
        REG(11)=REG(10)
        REG(10)=REG(9)
        REG(40)=REG(9)
        REG(9) =REG(50)
                        RETURN
                        END IF
        IF(WC.EQ.'RDN') THEN
        REG(50)=REG(9)
        REG(40)=REG(9)
        REG(9)=REG(10)
        REG(10)=REG(11)
        REG(11)=REG(12)
        REG(12)=REG(50)
                        RETURN
                        END IF
        IF(WC.EQ.'IRDN') THEN
        REG(49)=REG(13)
        REG(30)=REG(13)
        REG(13)=REG(14)
        REG(14)=REG(15)
        REG(15)=REG(16)
        REG(16)=REG(49)
                        RETURN
                        END IF
        IF(WC.EQ.'CRDN') THEN
        REG(49)=REG(13)
        REG(30)=REG(13)
        REG(13)=REG(14)
        REG(14)=REG(15)
        REG(15)=REG(16)
        REG(16)=REG(49)
        REG(50)=REG(9)
        REG(40)=REG(9)
        REG(9)=REG(10)
        REG(10)=REG(11)
        REG(11)=REG(12)
        REG(12)=REG(50)
                        RETURN
                        END IF
        IF(WC.EQ.'ENTR'.OR.WC.EQ.'ENT') THEN
      CALL PUSH_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'ENTI') THEN
        REG(16)=REG(15)
        REG(15)=REG(14)
        REG(14)=REG(13)
                        RETURN
                        END IF
        IF(WC.EQ.'ENTC') THEN
        REG(16)=REG(15)
        REG(15)=REG(14)
        REG(14)=REG(13)
      CALL PUSH_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'PULL') THEN
      REG(40)=REG(9)
      REG(9)=REG(10)
      CALL PULL_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'IPULL') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'CPULL') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)
        REG(14)=REG(15)
        REG(15)=REG(16)
      REG(40)=REG(9)
      REG(9)=REG(10)
      CALL PULL_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'LASTIX') THEN

        REG(31)=REG(13)
        REG(13)=REG(30)
        REG(30)=REG(31)
                        RETURN
                        END IF
        IF(WC.EQ.'LASTX') THEN
        REG(41)=REG(9)
        REG(9)=REG(40)
        REG(40)=REG(41)
                        RETURN
                        END IF
        IF(WC.EQ.'X-Y') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)
        REG(10)=REG(40)
                        RETURN
                        END IF
        IF(WC.EQ.'IX-IY') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)
        REG(14)=REG(30)
                        RETURN
                        END IF
        IF(WC.EQ.'RE-IM') THEN
C       UPDATE LASTX AND LASTIX
        REG(40)=REG(9)
        REG(30)=REG(13)
C
        REG(32)=REG(9)
        REG(9)=REG(13)
        REG(13)=REG(32)
C
        REG(32)=REG(10)
        REG(10)=REG(14)
        REG(14)=REG(32)
C
        REG(32)=REG(11)
        REG(11)=REG(15)
        REG(15)=REG(32)
C
        REG(32)=REG(12)
        REG(12)=REG(16)
        REG(16)=REG(32)
C
        REG(32)=0.0D0
                        RETURN
                        END IF
        IF(WC.EQ.'X-Y') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)
        REG(10)=REG(40)
                        RETURN
                        END IF
        IF(WC.EQ.'+') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)+REG(9)
      CALL PULL_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'I+') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)+REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'C+') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)+REG(9)
      CALL PULL_STACK
        REG(30)=REG(13)
        REG(13)=REG(14)+REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'-') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)-REG(9)
      CALL PULL_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'I-') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)-REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'C-') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)-REG(9)
      CALL PULL_STACK
        REG(30)=REG(13)
        REG(13)=REG(14)-REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'*') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)*REG(9)
      CALL PULL_STACK
                        END IF
        IF(WC.EQ.'I*') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)*REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        END IF
        IF(WC.EQ.'C*') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)*REG(9)
      CALL PULL_STACK
        REG(30)=REG(13)
        REG(13)=REG(14)*REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        END IF
        IF(WC.EQ.'/') THEN
        IF(REG(9).NE.0.0D0) THEN
        REG(40)=REG(9)
        REG(9)=REG(10)/REG(9)
      CALL PULL_STACK
        RETURN
        ELSE
        IF(REG(9).EQ.0.0D0) THEN
        WRITE(OUTLYNE,*)'REAL DIVISION BY ZERO NOT ALLOWED'
      CALL SHOWIT(1)
                        CALL MACFAL
        END IF
        END IF
                        RETURN
                        END IF
        IF(WC.EQ.'I/') THEN
        IF(REG(13).NE.0.0D0) THEN
        REG(30)=REG(13)
        REG(13)=REG(14)/REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
        RETURN
        ELSE
        IF(REG(13).EQ.0.0D0) THEN
        WRITE(OUTLYNE,*)'IMAGINARY DIVISION BY ZERO NOT ALLOWED'
      CALL SHOWIT(1)
                CALL MACFAL
        END IF
        END IF
                        RETURN
                        END IF
        IF(WC.EQ.'C/') THEN
        IF(REG(9).NE.0.0D0.AND.REG(13).NE.0.0D0) THEN
        REG(40)=REG(9)
        REG(9)=REG(10)/REG(9)
      CALL PULL_STACK
        REG(30)=REG(13)
        REG(13)=REG(14)/REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
        RETURN
        ELSE
        IF(REG(9).EQ.0.0D0) THEN
        WRITE(OUTLYNE,*)'REAL DIVISION BY ZERO NOT ALLOWED'
      CALL SHOWIT(1)
        END IF
        IF(REG(13).EQ.0.0D0) THEN
        WRITE(OUTLYNE,*)'IMAGINARY DIVISION BY ZERO NOT ALLOWED'
      CALL SHOWIT(1)
        END IF
        IF(REG(9).EQ.0.0D0.OR.REG(13).EQ.0.0D0
     1  .OR.REG(9).EQ.0.0D0.AND.REG(13).EQ.0.0D0) THEN
        WRITE(OUTLYNE,*)'COMPLEX STACK NOT CHANGED'
      CALL SHOWIT(1)
                CALL MACFAL
        END IF
        END IF
                        RETURN
                        END IF
        IF(WC.EQ.'Y**X') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)**REG(9)
      CALL PULL_STACK
                        RETURN
                        END IF
        IF(WC.EQ.'IY**IX') THEN
        REG(30)=REG(13)
        REG(13)=REG(14)**REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'CY**CX') THEN
        REG(40)=REG(9)
        REG(9)=REG(10)**REG(9)
      CALL PULL_STACK
        REG(30)=REG(13)
        REG(13)=REG(14)**REG(13)
        REG(14)=REG(15)
        REG(15)=REG(16)
                        RETURN
                        END IF
        IF(WC.EQ.'CLIX') THEN
        REG(30)=REG(13)
        REG(13)=0.0D0
                        RETURN
                        END IF
        IF(WC.EQ.'CLX') THEN
        REG(40)=REG(9)
        REG(9)=0.0D0
                        RETURN
                        END IF
        IF(WC.EQ.'CLSTK') THEN
        REG(40)=REG(9)
        REG(9)=0.0D0
        REG(10)=0.0D0
        REG(11)=0.0D0
        REG(12)=0.0D0
                        RETURN
                        END IF
        IF(WC.EQ.'CLSTKI') THEN
        REG(30)=REG(13)
        REG(13)=0.0D0
        REG(14)=0.0D0
        REG(15)=0.0D0
        REG(16)=0.0D0
                        RETURN
                        END IF
        IF(WC.EQ.'CLSTKC') THEN
        REG(30)=REG(13)
        REG(13)=0.0D0
        REG(14)=0.0D0
        REG(15)=0.0D0
        REG(16)=0.0D0
        REG(40)=REG(9)
        REG(9)=0.0D0
        REG(10)=0.0D0
        REG(11)=0.0D0
        REG(12)=0.0D0
                        RETURN
                        END IF
        IF(WC.EQ.'PRLSTX') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF REGISTER REG(40). THIS IS THE LASTX REGISTER.
C       PRIOR TO REASSIGNMENT OF THE VALUE OF THE ACCUMULATOR
C       REG(9) THE CURRENT VALUE IS ALWAYS STORED IN REG(40)
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(40)
              WS='LASTX ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,101) REG(40)
      CALL SHOWIT(0)
                END IF
C       THE FOLLOWING ARE THE FORMAT STATEMENTS
 101    FORMAT(D23.15)
                        RETURN
                        END IF
        IF(WC.EQ.'PRLSTIX') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF REGISTER REG(30). THIS IS THE LASTIX REGISTER.
C       PRIOR TO REASSIGNMENT OF THE VALUE OF THE IX RERGISTER
C       REG(13) THE CURRENT VALUE IS ALWAYS STORED IN REG(30)
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(30)
              WS='LASTIX ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,102) REG(30)
      CALL SHOWIT(0)
                END IF
C       THE FOLLOWING ARE THE FORMAT STATEMENTS
 102    FORMAT(D23.15)
                        RETURN
                        END IF
        IF(WC.EQ.'PRSTK') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF A THE REAL STACK.
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(9)
              WS='X ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(9)
      CALL SHOWIT(0)
                END IF
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(10)
              WS='Y ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(10)
      CALL SHOWIT(0)
                END IF
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(11)
              WS='Z ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(11)
      CALL SHOWIT(0)
                END IF
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(12)
              WS='T ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(12)
      CALL SHOWIT(0)
                END IF
                        RETURN
                        END IF
        IF(WC.EQ.'PRSTKI') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF A THE IMAGINARY STACK.
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(13)
              WS='IX ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(13)
      CALL SHOWIT(0)
                END IF
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(14)
              WS='IY ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(14)
      CALL SHOWIT(0)
                END IF
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(15)
              WS='IZ ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(15)
      CALL SHOWIT(0)
                END IF
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
        WRITE(UNIT=CSTRING,FMT=WFORM,ERR=69) REG(16)
              WS='IT ='//' '//CSTRING(1:23)
        WRITE(OUTLYNE,1100) WS
      CALL SHOWIT(0)
                ELSE
                WRITE(OUTLYNE,1001) REG(16)
      CALL SHOWIT(0)
                END IF
                        RETURN
                        END IF
        IF(WC.EQ.'PRSTKC') THEN
C       THIS IS CALLED TO WRITE OUT THE CONTENTS
C       OF A THE COMPLEX STACK.
                IF(OUT.EQ.6.OR.OUT.EQ.7) THEN
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
                        ELSE
                WRITE(OUTLYNE,1001) REG(9)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(10)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(11)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(12)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(13)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(14)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(15)
      CALL SHOWIT(0)
                WRITE(OUTLYNE,1001) REG(16)
      CALL SHOWIT(0)
                END IF
                        RETURN
                        END IF
C
C       THE FOLLOWING ARE THE FORMAT STATEMENTS
C
 1001   FORMAT(D23.15)
                        RETURN
   69   CONTINUE
        WRITE(OUTLYNE,*)
     1'INVALID FORMAT SPECIFICATION EXISTS'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)
     1'RE-ISSUE THE "FORMAT" COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
 1100   FORMAT(A79)
                        END
C SUB MINMAXREG.FOR
        SUBROUTINE MINMAXREG
C
        IMPLICIT NONE
C
C       THIS SUBROUTINE IS USED TO MANIPULATE THE MIN/MAX REGISTERS
C
C
      INCLUDE 'DATMAI.INC'
      INTEGER I
C
        IF(SST.EQ.1.OR.SQ.EQ.1) THEN
      IF(WC.EQ.'STOREMIN')
     1WRITE(OUTLYNE,*)'"STOREMIN" TAKES NO STRING OR QUALIFIER INPUT'
      IF(WC.EQ.'STOREMAX')
     1WRITE(OUTLYNE,*)'"STOREMAX" TAKES NO STRING OR QUALIFIER INPUT'
      IF(WC.EQ.'RESETMIN')
     1WRITE(OUTLYNE,*)'"RESETMIN" TAKES NO STRING OR QUALIFIER INPUT'
      IF(WC.EQ.'RESETMAX')
     1WRITE(OUTLYNE,*)'"RESETMAX" TAKES NO STRING OR QUALIFIER INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(WC.EQ.'STOREMIN'.OR.WC.EQ.'STOREMAX') THEN
      IF(DF1.EQ.1) THEN
      W1=1.0D0
      DF1=0
      S1=1
      END IF
      END IF
      IF(WC.EQ.'RESETMIN') THEN
      IF(DF2.EQ.1) THEN
      W2=1.0D300
      DF2=0
      S2=1
      END IF
      END IF
      IF(WC.EQ.'RESETMAX') THEN
      IF(DF2.EQ.1) THEN
      W2=-1.0D300
      DF2=0
      S1=2
      END IF
      END IF
      IF(WC.EQ.'RESETMIN'.OR.WC.EQ.'RESETMAX') THEN
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      IF(WC.EQ.'RESETMIN')
     1WRITE(OUTLYNE,*)
     2'"RESETMIN" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
      IF(WC.EQ.'RESETMAX')
     1WRITE(OUTLYNE,*)
     2'"RESETMAX" ONLY TAKES NUMERIC WORDS #1 AND #2 INPUT'
      CALL SHOWIT(1)
      WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
      IF(WC.EQ.'STOREMIN'.OR.WC.EQ.'STOREMAX') THEN
        IF(S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
      IF(WC.EQ.'STOREMIN')
     1WRITE(OUTLYNE,*)'"STOREMIN" ONLY TAKES NUMERIC WORD #1 INPUT'
      IF(WC.EQ.'STOREMAX')
     1WRITE(OUTLYNE,*)'"STOREMAX" ONLY TAKES NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
        WRITE(OUTLYNE,*)'RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
                        END IF
      IF(WC.EQ.'RESETMIN') THEN
      IF(DF1.EQ.1) THEN
                           MIN_REG(1:100)=W2
                           RETURN
                   ELSE
                           MIN_REG(INT(W1))=W2
                           RETURN
                   END IF
                           END IF
      IF(WC.EQ.'RESETMAX') THEN
      IF(DF1.EQ.1) THEN
                           MAX_REG(1:100)=W2
                           RETURN
                   ELSE
                           MAX_REG(INT(W1))=W2
                           RETURN
                   END IF
                           END IF
      IF(WC.EQ.'STOREMAX') THEN
      IF(REG(9).GT.MAX_REG(INT(W1))) MAX_REG(INT(W1))=REG(9)
                           RETURN
                           END IF
      IF(WC.EQ.'STOREMIN') THEN
      IF(REG(9).LT.MIN_REG(INT(W1))) MIN_REG(INT(W1))=REG(9)
                           RETURN
                           END IF
                        END

C
      SUBROUTINE SAVEFONT(I)
      IMPLICIT NONE
      INTEGER I
      OPEN(UNIT=110,ACCESS='SEQUENTIAL',BLANK='NULL'
     1  ,FORM='FORMATTED',FILE='FONTSAVE.DAT'
     2  ,STATUS='UNKNOWN')
      REWIND(UNIT=110)
      IF(I.LT.2.OR.I.GT.36) I=8
      WRITE(110,*) I
      CALL CLOSE_FILE(110,1)
                        END
