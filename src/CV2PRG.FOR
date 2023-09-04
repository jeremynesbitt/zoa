C SUB CV2PRG.FOR
        SUBROUTINE CV2PRG
        USE GLOBALS
        use zoa_file_handler
        !use kdp_interfaces, only: OUTKDP
C
        IMPLICIT NONE
C
      CHARACTER CVFILENAME*80,KDPFILENAME*80,CV_INPUT_STRING*1024
      character(len=6) :: readformat
      character(len=3) :: substr
      character(len=1024) :: filePath
C
      CHARACTER TEMPA*1024,TEMPB*1024,CVA*23,THA*23,AINDEX*8
     1,GLASSA*40,VALA*23,AA23*23,SUMSTRING*1024,OUTLYNE2*1024
     2,TEMPC*1024,BL1024*1024,TOR*2,TIL*1,CYL*1,TEMPER*1024
     3,FIELDTYPE*3,AAS*1,CLTYPE*1,COTYPE*1,TEMPCC*1024,AVNUMB*7
C
      LOGICAL EXIS37,EXIS38,OPEN37,OPEN38,SEMI,ADD,RADON,OLDADD
C
      LOGICAL CVERROR
      LOGICAL :: COMMANDINFOCHECK, STRINGINPUTCHECK, CHECKMAXFLOATINPUTS
C
      LOGICAL DOWV,DOWV2,CLAP1,CLAP2,CLDX,CLDY,CLTILT
C
      LOGICAL COBS1,COBS2,COBX,COBY,COTILT
C
      INTEGER I,CVFILENAMELENGTH,KDPFILENAMELENGTH,NPERIOD,ALLOERR
C
      INTEGER J,K,L,NADD,II,OLDNADD,STRINGEND,WDEXIS(1:100),M
C
      INTEGER J1,J2,IVALV,IWD(1:100),SURFER, SUB
C
      REAL*8 CV,TH,VALV,WD(1:100),MAX,CL1,CL2,CL3,CL4,CL5
C
      REAL*8 CO1,CO2,CO3,CO4,CO5
C
      DIMENSION TEMPA(:),TEMPB(:),TEMPC(:)
C
      ALLOCATABLE :: TEMPA,TEMPB,TEMPC
C
      LOGICAL HOE
      INTEGER HOESUR
      CHARACTER*1 HV1,HV2
      REAL*8 HX1,HY1,HZ1,HX2,HY2,HZ2,HWL,HOR
      DIMENSION HV1(:),HV2(:),HX1(:),HY1(:),HZ1(:),HX2(:),HY2(:),
     1HZ2(:),HOESUR(:),HOE(:),HWL(:),HOR(:)
      ALLOCATABLE :: HWL,HV1,HV2,HX1,HY1,HZ1,HX2,HY2,
     1HZ2,HOESUR,HOE,HOR
C
      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATLEN.INC'

      SUB = 256
      write(substr, "(I3)") SUB
      readformat = '(A' // substr // ')'
      PRINT *, "READFORMAT IS ", readformat
      SURFER=0

      !PRINT *, "CV2PRG Starting.."
C
      TOR='? '
      TIL='?'
      CYL='?'
      AAS='?'
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CLTILT=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
      COTYPE='?'
      COBS1=.FALSE.
      COBS2=.FALSE.
      COBX=.FALSE.
      COBY=.FALSE.
      COTILT=.FALSE.
      CO1=0.0D0
      CO2=0.0D0
      CO3=0.0D0
      CO4=0.0D0
      CO5=0.0D0
C
C       THIS SUBROUTINE IS CALLED CONVERT A FILE FROM CODE V TO PRG
C     LENS INPUT FORMAT
C
      PRINT *, "CV2PRG Routine Started!"

      AA23='                       '
      BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//'    '
C
        IF (COMMANDINFOCHECK(
     1  "CV2PRG CONVERTS THE NAMED FILE (STRING INPUT)")) RETURN


        IF(SN.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE='"CV2PRG" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                                RETURN
                                END IF
        IF(SST.EQ.0) THEN
      OUTLYNE='"CV2PRG" REQUIRES EXPLICIT STRING (FILE NAME) INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                                RETURN
                                END IF

      CVFILENAME=WS
      call getInputStringLength(CVFILENAME, CVFILENAMELENGTH)

      IF (CVFILENAMELENGTH.EQ.0) THEN

          OUTLYNE='INVALID (ZERO LENGTH) CODE V FILE NAME'
          CALL SHOWIT(1)
          OUTLYNE='NO ACTION TAKEN'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN

      END IF

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End input error checking

      EXIS37=.FALSE.
      filePath = trim(getCodeVDir())//getFileSep()//
     1 CVFILENAME(1:CVFILENAMELENGTH)


      INQUIRE(
     1 FILE=trim(filePath),
     1 EXIST=EXIS37)

    !  INQUIRE(
    ! 1 FILE=trim(basePath)//'CodeV'//
    ! 1 getFileSep()//CVFILENAME(1:CVFILENAMELENGTH),
    ! 1 EXIST=EXIS37)

      IF(.NOT.EXIS37) THEN
          OUTLYNE='NO CODE V INPUT FILE EXISTS TO READ'
          CALL SHOWIT(1)
          OUTLYNE='NO ACTION TAKEN'
          CALL SHOWIT(1)
          CALL MACFAL
          RETURN
      END IF

C     FILES EXISTS, CREATE PROGRAM FILE NAME
      call logger%logText("CV2PRG Find Extension in File Name")

      NPERIOD=9
      DO I=1,80
         IF(CVFILENAME(I:I).EQ.'.') THEN
          NPERIOD=I
          EXIT
         END IF
      END DO

      ! TODO Investigate this part.  Looks sketchy
      IF(NPERIOD.GT.9) NPERIOD=9
                KDPFILENAME=CVFILENAME(1:NPERIOD)//'DAT'
                KDPFILENAMELENGTH=NPERIOD+4
C     OPEN THE CVFILENAME TO READ IT AS ASCII
C
      DEALLOCATE (TEMPA,TEMPB,TEMPC
     1,STAT=ALLOERR)
      DEALLOCATE (
     1HWL,
     1HV1,
     1HV2,
     1HX1,
     1HY1,
     1HZ1,
     1HX2,
     1HY2,
     1HZ2,
     1HOE,
     1HOR,
     1HOESUR,STAT=ALLOERR)
      ALLOCATE (TEMPA(1:5000),TEMPB(1:5000),TEMPC(1:5000)
     1,STAT=ALLOERR)
      ALLOCATE (
     1HWL(0:MAXSUR),
     1HV1(0:MAXSUR),
     1HV2(0:MAXSUR),
     1HX1(0:MAXSUR),
     1HY1(0:MAXSUR),
     1HZ1(0:MAXSUR),
     1HX2(0:MAXSUR),
     1HY2(0:MAXSUR),
     1HZ2(0:MAXSUR),
     1HOE(0:MAXSUR),
     1HOR(0:MAXSUR),
     1HOESUR(0:MAXSUR),STAT=ALLOERR)
                        I=MAXSUR
      HOE(0:I)=.FALSE.
      HOESUR(0:I)=-1
      TEMPA(1:5000)=BL1024
      TEMPB(1:5000)=BL1024
      TEMPC(1:5000)=BL1024
C

C     CLOSE IT IF OPEN AND KEEP IY
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,0)
      call logger%logText("CV2PRG Open File " // CVFILENAME)

      OPEN(UNIT=37,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE=trim(filePath)
     2,STATUS='UNKNOWN')
      OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',
     1 FILE=trim(basePath)//'CONVERT.ERR'
     2,STATUS='UNKNOWN')
      CALL CLOSE_FILE(38,0)
      OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',
     1 FILE=trim(basePath)//'CONVERT.ERR'
     2,STATUS='UNKNOWN')


      II=1
      ADD=.FALSE.
      OLDADD=.FALSE.
      DO I=1,5000
C     READ A LINE OF THE FILE AS A 132 CHARACTER, CHARACTER
      CV_INPUT_STRING=BL1024
      READ(37,readformat,ERR=75,END=76) CV_INPUT_STRING(1:SUB)
      call logger%logText("Processing Line: " //CV_INPUT_STRING(1:SUB))

C     IS THERE A CONTINUATION MARK AT THE END ?
C     REMEMBER IF ADD WAS ON LAST TIME
      OLDADD=ADD
      DO J=1024,1,-1

          ADD=.FALSE.
          IF(CV_INPUT_STRING(J:J).EQ.'&') THEN
C     YES
            ADD=.TRUE.
            NADD=J
            EXIT ! Exit do loop
            ELSE
            END IF

      END DO
      PRINT *, "J is ", J

      IF(.NOT.ADD.AND..NOT.OLDADD) THEN
C     NOTHING TO ADD, WRITE A TEMPA VALUE
          TEMPA(II)(1:SUB)=CV_INPUT_STRING(1:SUB)
C     BLANK OUT ALL &
          DO J=1024,1,-1
              IF(TEMPA(II)(J:J).EQ.'&') TEMPA(II)(J:J)=' '
          END DO

          II=II+1
          ADD=.FALSE.
          OLDADD=.FALSE.
      END IF
C
      IF(ADD.AND..NOT.OLDADD) THEN
C     THEN WRITE AN INITIAL SUMSTRING AND GET ITS LENGTH
        SUMSTRING(1:1024)=CV_INPUT_STRING(1:1024)
        DO J=1024,1,-1
          IF(SUMSTRING(J:J).NE.' ') THEN
            STRINGEND=J
            EXIT
          END IF
        END DO

      END IF

      IF(.NOT.ADD.AND.OLDADD) THEN
C     ADD, THEN WRITE A TEMPA VALUE
      IF(STRINGEND.LT.1024) THEN
      SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//
     1CV_INPUT_STRING(1:(1024-STRINGEND))
                        END IF
C     GET THE NEW STRINGEND
                        DO J=1024,1,-1
      IF(SUMSTRING(J:J).NE.' ') THEN
                        STRINGEND=J
                        EXIT
                        END IF
                        END DO

      TEMPA(II)(1:1024)=SUMSTRING(1:1024)
C     BLANK OUT ALL &
      DO J=1024,1,-1
          IF(TEMPA(II)(J:J).EQ.'&') TEMPA(II)(J:J)=' '
      END DO
      II=II+1
      ADD=.FALSE.
      OLDADD=.FALSE.
                        END IF
      IF(ADD.AND.OLDADD) THEN
C     ADD, TO MAKE A NEW SUMSTRING
      IF(STRINGEND.LT.1024) THEN
      SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//
     1CV_INPUT_STRING(1:(1024-STRINGEND))
                        END IF
C     GET THE NEW STRINGEND
                        DO J=1024,1,-1
      IF(SUMSTRING(J:J).NE.' ') THEN
                        STRINGEND=J
                        EXIT
                        END IF
                        END DO

                        END IF
                                END DO
                                GO TO 76
 75   CONTINUE
      OUTLYNE='ERROR READING CODE V INPUT FILE'
      CALL SHOWIT(1)
      OUTLYNE='NO FILE CONVERSION PERFORMED'
      CALL SHOWIT(1)
                        CALL MACFAL
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,0)
      DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
      DEALLOCATE (HOE,HOESUR,HV1,HV2,HX1,HY1,HZ1
     1,HWL,HX2,HY2,HZ2,HOR,STAT=ALLOERR)
                                RETURN
 76   CONTINUE

C     NOW REMOVE ALL VIRTUAL RETURNS (SEMI-COLONS) AND LOAD TEMPB
C     CYCLE THROUGH THE II-1 ENTRIES IN TEMPA
                        K=1
                        DO I=1,II-1
C     ARE THERE ANY SEMICOLONS
      L=1
      SEMI=.FALSE.
      DO J=1,1024
        IF(TEMPA(I)(J:J).EQ.';') SEMI=.TRUE.
      END DO
      IF(SEMI) THEN
C     MULTIPLE COMMANDS PER LINE
      DO J=1,1024
        IF(TEMPA(I)(J:J).EQ.';') THEN
          TEMPB(K)=TEMPA(I)(L:J-1)
          L=J+1
          K=K+1
        END IF
      IF(J.EQ.1024) THEN
        TEMPB(K)=TEMPA(I)(L:1024)
        K=K+1
        L=J+1
      END IF

      END DO

      ELSE
C     ONLY ONE INSTRUCTION ON THE LINE
        TEMPB(K)=TEMPA(I)
        K=K+1
      END IF

      END DO
C     NOW REMOVE ALL EVIL CHARACTERS AND LEADING BLANKS
          DO I=1,K-1
             DO J=1,1024
      IF(ICHAR(TEMPB(I)(J:J)).LT.32.OR.
     1ICHAR(TEMPB(I)(J:J)).GT.126) TEMPB(I)(J:J)=' '
                        END DO
                        DO J=1,1024
      IF(TEMPB(I)(1:1).EQ.' ') TEMPB(I)(1:1024)=TEMPB(I)(2:1024)//' '
                        END DO
                        END DO
                        L=1
                        DO I=1,K-1
      IF(TEMPB(I)(1:20).NE.'                    ') THEN
      TEMPC(L)(1:1024)=TEMPB(I)(1:1024)
                        L=L+1
                        END IF
                        END DO
                        L=L-1
C
C     INSTRUCTION TRASNLATION
C
 4000 FORMAT(' THE FOLLOWING CODE-V COMMAND DID NOT TRANSLATE:')
      WRITE(38,4000)
                        DO I=1,L
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
C     RDM
      IF(TEMPC(I)(1:3).EQ.'RDM') THEN
      IF(TEMPC(I)(1:5).EQ.'RDM N') RADON=.FALSE.
      IF(TEMPC(I)(1:5).NE.'RDM N') RADON=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     SPH OR CON
      IF(TEMPC(I)(1:3).EQ.'SPH'.OR.
     1TEMPC(I)(1:3).EQ.'CON'.OR.
     1TEMPC(I)(1:3).EQ.'ASP') THEN
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     LEN
      IF(TEMPC(I)(1:3).EQ.'LEN') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='LENS'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     TITLE
      IF(TEMPC(I)(1:5).EQ.'TITLE'.OR.TEMPC(I)(1:3).EQ.'TIT') THEN
                        DO J=1024,1,-1
      IF(TEMPC(I)(J:J).EQ.'''') THEN
                        J2=J
                        GO TO 201
                        END IF
                        END DO
 201                   CONTINUE
                        DO J=1,1024
      IF(TEMPC(I)(J:J).EQ.'''') THEN
                        J1=J
                        GO TO 102
                        END IF
                        END DO
 102                   CONTINUE
                        SAVE_KDP(1)=SAVEINPT(1)
      INPUT='LI '//TEMPC(I)(J1+1:J2-1)
      IF(TEMPC(I)(J1+1:J2-1).NE.BL1024(J1+1:J2-1)) THEN
          IF(INPUT(1:20).NE.AA) CALL PROCES
                        END IF
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     INI
      IF(TEMPC(I)(1:3).EQ.'INI') THEN
                        DO J=1024,1,-1
      IF(TEMPC(I)(J:J).EQ.'''') THEN
                        J2=J
                        GO TO 202
                        END IF
                        END DO
 202                   CONTINUE
                        DO J=1,1024
      IF(TEMPC(I)(J:J).EQ.'''') THEN
                        J1=J
                        GO TO 121
                        END IF
                        END DO
 121                   CONTINUE
                        SAVE_KDP(1)=SAVEINPT(1)
      call logger%logText('About to set INI '//TEMPC(I))
      PRINT *, "J1 = ", J1
      IF (J2.GT.1024) J2 = 1024
      PRINT *, "J2 = ", J2

      INPUT='INI '//TEMPC(I)(J1+1:J2-1)
      PRINT *, "INI Input is " // INPUT
      IF(TEMPC(I)(J1+1:J2-1).NE.BL1024(J1+1:J2-1)) THEN
      IF(INPUT(1:20).NE.AA) CALL PROCES
                        END IF
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     EPD
      IF(TEMPC(I)(1:3).EQ.'EPD') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE2,2001) VALV/2.0D0
 2001 FORMAT('SAY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     NAO
      IF(TEMPC(I)(1:3).EQ.'NAO') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE2,2002) VALV
 2002 FORMAT('NAO,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     REF
      IF(TEMPC(I)(1:3).EQ.'REF') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATOICODEV(VALA,IVALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE2,2003) IVALV
 2003 FORMAT('CW,',I2)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     YAN,XAN,YOB,XOB,XIM,YIM,XRI,YRI
      IF(TEMPC(I)(1:3).EQ.'YAN'
     1.OR.TEMPC(I)(1:3).EQ.'XAN'
     1.OR.TEMPC(I)(1:3).EQ.'XIM'
     1.OR.TEMPC(I)(1:3).EQ.'YIM'
     1.OR.TEMPC(I)(1:3).EQ.'XRI'
     1.OR.TEMPC(I)(1:3).EQ.'YRI'
     1.OR.TEMPC(I)(1:3).EQ.'YOB'
     1.OR.TEMPC(I)(1:3).EQ.'XOB') THEN
      FIELDTYPE='   '
      IF(TEMPC(I)(1:3).EQ.'XAN') FIELDTYPE='XAN'
      IF(TEMPC(I)(1:3).EQ.'YAN') FIELDTYPE='YAN'
      IF(TEMPC(I)(1:3).EQ.'XOB') FIELDTYPE='XOB'
      IF(TEMPC(I)(1:3).EQ.'YOB') FIELDTYPE='YOB'
      IF(TEMPC(I)(1:3).EQ.'XIM') FIELDTYPE='XIM'
      IF(TEMPC(I)(1:3).EQ.'YIM') FIELDTYPE='YIM'
      IF(TEMPC(I)(1:3).EQ.'XRI') FIELDTYPE='XRI'
      IF(TEMPC(I)(1:3).EQ.'YRI') FIELDTYPE='YRI'
      CALL ONEBLANK(3,TEMPC(I)(1:1024))
      IF(FIELDTYPE.EQ.'XAN'.OR.FIELDTYPE.EQ.'YAN'.OR.
     1FIELDTYPE.EQ.'XOB'.OR.FIELDTYPE.EQ.'YOB') THEN
C     MAX FIELD POS CONVERT TO SCY. SCY FANG, SCX AND SCX FANG
C     BREAK OUT UP TO 25 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN FILD LARGEST VALUE
C     AS NEEDED.
C     STRIP OFF 3 CHARACTERS
      !call OUTKDP("Entering YOB Loop")
      !call OUTKDP("Fieldtype is "//FIELDTYPE)
      TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),25,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      MAX=-1000.0D0
                        DO J=1,25
      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                        END DO
      IF(FIELDTYPE.EQ.'YAN') THEN
      WRITE(OUTLYNE2,2004) MAX
 2004 FORMAT('SCY FANG,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:SUB)=OUTLYNE2(1:SUB)
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XAN') THEN
      WRITE(OUTLYNE2,2005) MAX
 2005 FORMAT('SCX FANG,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YOB') THEN
      !CALL OUTKDP('Really in FIELDTYPE YOB LOOP')
      WRITE(OUTLYNE2,2006) MAX
      !CALL OUTKDP(trim(OUTLYNE2))
 2006 FORMAT('SCY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)

      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XOB') THEN
      WRITE(OUTLYNE2,2007) MAX
 2007 FORMAT('SCX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
                        END IF
      IF(FIELDTYPE.EQ.'XIM'.OR.FIELDTYPE.EQ.'YIM'.OR.
     1FIELDTYPE.EQ.'XRI'.OR.FIELDTYPE.EQ.'YRI') THEN
C     IMAGE HT SPEC
C     STRIP OFF 3 CHARACTERS
      TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),25,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      MAX=-1000.0D0
                        DO J=1,25
      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                        END DO
      IF(FIELDTYPE.EQ.'XIM') THEN
      WRITE(OUTLYNE2,4004) MAX
 4004 FORMAT('PXIM,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YIM') THEN
      WRITE(OUTLYNE2,4005) MAX
 4005 FORMAT('PYIM,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XRI') THEN
      WRITE(OUTLYNE2,4006) MAX
 4006 FORMAT('RXIM,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YRI') THEN
      WRITE(OUTLYNE2,4007) MAX
 4007 FORMAT('RYIM,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
C     WL
      IF(TEMPC(I)(1:2).EQ.'WL') THEN
      CALL ONEBLANK(2,TEMPC(I)(1:1024))
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE WV AND WV2 COMMANDS
C     AS NEEDED.
C     STRIP OFF WD
      TEMPC(I)(1:1024)=TEMPC(I)(3:1024)//'  '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),10,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      DOWV=.FALSE.
      DOWV2=.FALSE.
                        DO J=1,5
                 IF(WDEXIS(J).NE.0) DOWV =.TRUE.
                        END DO
                        DO J=6,10
                 IF(WDEXIS(J).NE.0) DOWV2 =.TRUE.
                        END DO
      IF(DOWV) THEN
      WRITE(OUTLYNE2,2008) WD(1)/1000.0D0,WD(2)/1000.0D0,WD(3)/1000.0D0
     1,WD(4)/1000.0D0,WD(5)/1000.0D0
 2008 FORMAT('WV,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(DOWV2) THEN
      WRITE(OUTLYNE2,2009) WD(6)/1000.0D0,WD(7)/1000.0D0,WD(8)/1000.0D0
     1,WD(9)/1000.0D0,WD(10)/1000.0D0
 2009 FORMAT('WV2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
C     WTW
      IF(TEMPC(I)(1:3).EQ.'WTW') THEN
      CALL ONEBLANK(3,TEMPC(I)(1:1024))
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE SPTWT/SPTWT2 COMMANDS
C     AS NEEDED.
C     STRIP OFF WTW
      TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      CALL IMULTIPROCESS(IWD,WDEXIS,TEMPC(I)(1:1024),10,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      DOWV=.FALSE.
      DOWV2=.FALSE.
                        DO J=1,5
                       IF(WDEXIS(J).NE.0) DOWV =.TRUE.
                        END DO
                        DO J=6,10
                       IF(WDEXIS(J).NE.0) DOWV2 =.TRUE.
                        END DO
      IF(DOWV) THEN
      WRITE(OUTLYNE2,2010) WD(1),WD(2),WD(3),WD(4),WD(5)
 2010 FORMAT('SPTWT,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(DOWV2) THEN
      WRITE(OUTLYNE2,2011) WD(6),WD(7),WD(8),WD(9),WD(10)
 2011 FORMAT('SPTWT2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
C     TEL Y
      IF(TEMPC(I)(1:5).EQ.'TEL Y') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='TEL YES'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     STO
      IF(TEMPC(I)(1:3).EQ.'STO') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='ASTOP'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='REFS'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CYL
C     CYL IS JUST AN INTERNAL SETTING
      IF(TEMPC(I)(1:3).EQ.'CYL') THEN
      TOR='? '
      CYL='C'
      AAS='?'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     YTO OR AAS
      IF(TEMPC(I)(1:3).EQ.'YTO'.OR.TEMPC(I)(1:3).EQ.'AAS') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='YTORIC'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TOR='YT'
      AAS='?'
      CYL='?'
      IF(TEMPC(I)(1:3).EQ.'AAS') AAS='Y'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     XTO
      IF(TEMPC(I)(1:3).EQ.'XTO') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='XTORIC'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TOR='XT'
      AAS='?'
      CYL='?'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     DIM
      IF(TEMPC(I)(1:3).EQ.'DIM') THEN
        PRINT *, "CV2PRG DIM " //TEMPC(I)
        PRINT *, "Unit check is " //TEMPC(I)(7:7)
      IF(TEMPC(I)(7:7).EQ.'M') THEN
                        PRINT *, "Setting units to mm"
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='UNITS MM'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(TEMPC(I)(7:7).EQ.'C') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='UNITS CM'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(TEMPC(I)(7:7).EQ.'I') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='UNITS IN'
                        IF(INPUT(1:20).NE.AA) CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     PROCESS EACH LINE INTO A KDP COMMAND AND THEN DISPLAY IT
      IF(TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.' '.OR.
     1TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'I'.OR.
     1TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'O'.OR.
     1TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'0') THEN
      IF(TEMPC(I)(1:2).NE.'SO'.AND.TEMPC(I)(1:2).NE.'S0') THEN
C
C     WRITE CLAP DATA, COBS DATA, THEN GLASS DATA
C     CLAP DATA
      IF(CLTYPE.EQ.'C') THEN
      WRITE(OUTLYNE2,2012) CL1,CL4,CL3,CL2
 2012 FORMAT('CLAP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CLTILT=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
                        END IF
      IF(CLTYPE.EQ.'R') THEN
      WRITE(OUTLYNE2,2013) CL2,CL1,CL4,CL3
 2013 FORMAT('CLAP RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CLTILT=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
                        END IF
      IF(CLTYPE.EQ.'E') THEN
      WRITE(OUTLYNE2,2014) CL2,CL1,CL4,CL3
 2014 FORMAT('CLAP ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CLTILT=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
                        END IF
      IF(CLTILT) THEN
      WRITE(OUTLYNE2,2015) CL5
 2015 FORMAT('CLAP TILT,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      CLTILT=.FALSE.
                        END IF
C     COBS DATA
      IF(COTYPE.EQ.'C') THEN
      WRITE(OUTLYNE2,4012) CO1,CO4,CO3
 4012 FORMAT('COBS,',D23.15,',',D23.15,',',D23.15,',,,')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      COTYPE='?'
      COBS1=.FALSE.
      COBS2=.FALSE.
      COBX=.FALSE.
      COBY=.FALSE.
      CO1=0.0D0
      CO2=0.0D0
      CO3=0.0D0
      CO4=0.0D0
      CO5=0.0D0
                        END IF
      IF(COTYPE.EQ.'R') THEN
      WRITE(OUTLYNE2,4013) CO2,CO1,CO4,CO3
 4013 FORMAT('COBS RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      COTYPE='?'
      COBS1=.FALSE.
      COBS2=.FALSE.
      COBX=.FALSE.
      COBY=.FALSE.
      CO1=0.0D0
      CO2=0.0D0
      CO3=0.0D0
      CO4=0.0D0
      CO5=0.0D0
                        END IF
      IF(COTYPE.EQ.'E') THEN
      WRITE(OUTLYNE2,4014) CO2,CO1,CO4,CO3
 4014 FORMAT('COBS ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      COTYPE='?'
      COBS1=.FALSE.
      COBS2=.FALSE.
      COBX=.FALSE.
      COBY=.FALSE.
      CO1=0.0D0
      CO2=0.0D0
      CO3=0.0D0
      CO4=0.0D0
      CO5=0.0D0
                        END IF
      IF(COTILT) THEN
      WRITE(OUTLYNE2,4015) CO5
 4015 FORMAT('COBS TILT,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      COTILT=.FALSE.
                        END IF
C     GLASS DATA
      WRITE(OUTLYNE2,2016) GLASSA
 2016 FORMAT(A40)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      SURFER=SURFER+1
      HOESUR(SURFER)=-1
      TOR='? '
      TIL='?'
      CYL='?'
      AAS='?'
                        END IF
C     A SURFACE LINE, BREAK IT OUT
C     STRIP OFF TO THE START OF THE CURVATURE
      IF(TEMPC(I)(1:2).EQ.'SO'.OR.
     1TEMPC(I)(1:2).EQ.'S0'.OR.
     2TEMPC(I)(1:2).EQ.'S '.OR.
     3TEMPC(I)(1:2).EQ.'SI')
     4TEMPC(I)(1:1024)=TEMPC(I)(3:1024)//'     '
C     REMOVE MORE LEADING BLANKS
                        DO M=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
C     STRIP OFF THE CURVATURE
                        DO J=1,1024
      IF(TEMPC(I)(J:J).EQ.' ') THEN
      CVA=AA(1:(23-J-1))//TEMPC(I)(1:J-1)
      CALL ATODCODEV(CVA,CV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
C     IF RADON THEN CODE V REPORTED RADIUS, CONVERT TO
C     CURVATURE ELSE DON'T AND DON'T CONVERT IF RADON AND RAD=0
      IF(RADON.AND.CV.NE.0.0D0) CV=1.0D0/CV
      TEMPC(I)(1:1024)=TEMPC(I)(J+1:1024)//BL1024(1:J)
                        GO TO 50
                        END IF
                        END DO
C     REMOVE SOME MORE LEADING SPACES
 50                     DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
C     STRIP OFF THE THICKNESS
                        DO J=1,1024
      IF(TEMPC(I)(J:J).EQ.' ') THEN
      THA=AA(1:(23-J-1))//TEMPC(I)(1:J-1)
      CALL ATODCODEV(THA,TH,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      TEMPC(I)(1:1024)=TEMPC(I)(J+1:1024)//BL1024(1:J)
                        DO M=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      CALL ATODCODEV(THA,TH,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
                        GO TO 60
                        END IF
                        END DO
 60                     CONTINUE
C     NOW THE GLASS TYPE
C     ALL BLANKS, GLASS WAS AIR
      IF(TEMPC(I)(1:1024).EQ.BL1024) THEN
      GLASSA='AIR'
      GO TO 7777
                        END IF
C     REMOVE BLANKS
                        DO M=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
C     DETERMINE GLASS NAME
      WRITE(OUTLYNE, *) 'TEMPC is ', TEMPC(I)(1:20)
      call logger%logText(OUTLYNE)
                        DO J=1,1024
      IF(TEMPC(I)(J:J).EQ.'_') THEN
      IF(TEMPC(I)(J+1:J+1).EQ.'S'.AND.TEMPC(I)(1:4).EQ.'BAF2') THEN
      GLASSA='MATL '//TEMPC(I)(1:J-1)
                       ELSE
      GLASSA='GLA '//TEMPC(I)(1:J-1)
                       END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
      GO TO 7777
                        END IF
                        END DO
      IF(TEMPC(I)(1:4).EQ.'AIR ') THEN
                        GLASSA='AIR'
      TEMPC(I)(1:1024)=BL1024(1:1024)
      GO TO 7777
                        END IF
      IF(TEMPC(I)(1:4).EQ.'REFL') THEN
                        GLASSA='REFL'
      TEMPC(I)(1:1024)=BL1024(1:1024)
      GO TO 7777
                        END IF
C
C     CODE-V MODEL GLASS
C
      IF(TEMPC(I)(1:1).EQ.'0'.OR.TEMPC(I)(1:1).EQ.'1'.OR.
     1TEMPC(I)(1:1).EQ.'2'.OR.TEMPC(I)(1:1).EQ.'3'.OR.
     1TEMPC(I)(1:1).EQ.'4'.OR.TEMPC(I)(1:1).EQ.'5'.OR.
     1TEMPC(I)(1:1).EQ.'6'.OR.TEMPC(I)(1:1).EQ.'7'.OR.
     1TEMPC(I)(1:1).EQ.'8'.OR.TEMPC(I)(1:1).EQ.'9') THEN
      AINDEX='1.'//TEMPC(I)(1:6)
      AVNUMB=TEMPC(I)(8:9)//'.'//TEMPC(I)(10:13)
      GLASSA='MODEL G'//TEMPC(I)(1:3)//
     1TEMPC(I)(8:10)//','//AINDEX//','//AVNUMB
      TEMPC(I)(1:1024)=BL1024(1:1024)
      GO TO 7777
                        END IF
C       FALL THROUGH
        GLASSA='GLASS '//TEMPC(I)(1:20)
 7777                   CONTINUE
        IF(.NOT.RADON) THEN
      WRITE(OUTLYNE2,2017) CV
                ELSE
        IF(CV.EQ.0.0D0) THEN
      WRITE(OUTLYNE2,2041) CV
                ELSE
      WRITE(OUTLYNE2,2041) 1.0D0/CV
                END IF
                END IF
 2017 FORMAT('CV,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2018) TH
 2018 FORMAT('TH,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        ELSE
C     NOT A SURFACE COMMAND
C     RESOLVE NON-SURFACE COMMANDS
                        END IF
C
C     TILTS AND DECENTERS GO HERE
      IF(TEMPC(I)(1:3).EQ.'DAR') THEN
      WRITE(OUTLYNE2,2019)
 2019 FORMAT('TILT DAR')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TIL='T'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        GO TO 8888
                        END IF
      IF(TEMPC(I)(1:3).EQ.'REV') THEN
      WRITE(OUTLYNE2,9019)
 9019 FORMAT('TILT REV')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TIL='T'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        GO TO 8888
                        END IF
      IF(TEMPC(I)(1:3).EQ.'BEN') THEN
      WRITE(OUTLYNE2,2020)
 2020 FORMAT('TILT BEN')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TIL='T'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        GO TO 8888
                        END IF
C
C     ADE,BDE,CDE,XDE,YDE,ZDE
      IF(TEMPC(I)(1:3).EQ.'ADE'.OR.
     1TEMPC(I)(1:3).EQ.'BDE'.OR.
     1TEMPC(I)(1:3).EQ.'CDE'.OR.
     1TEMPC(I)(1:3).EQ.'XDE'.OR.
     1TEMPC(I)(1:3).EQ.'YDE'.OR.
     1TEMPC(I)(1:3).EQ.'ZDE') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TIL.EQ.'?') THEN
      IF(TEMPC(I)(1:3).EQ.'ADE'.OR.
     1TEMPC(I)(1:3).EQ.'BDE'.OR.
     1TEMPC(I)(1:3).EQ.'CDE') THEN
                TIL='T'
      WRITE(OUTLYNE2,2022)
 2022 FORMAT('TILT')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        END IF
      IF(TEMPC(I)(1:3).EQ.'ADE') THEN
      WRITE(OUTLYNE2,2023) VALV
 2023 FORMAT('ALPHA,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'BDE') THEN
      WRITE(OUTLYNE2,2024) VALV
 2024 FORMAT('BETA,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'CDE') THEN
      WRITE(OUTLYNE2,2025) VALV
 2025 FORMAT('GAMMA,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'XDE') THEN
      WRITE(OUTLYNE2,2026) VALV
 2026 FORMAT('XD,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'YDE') THEN
      WRITE(OUTLYNE2,2027) VALV
 2027 FORMAT('YD,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'ZDE') THEN
      WRITE(OUTLYNE2,2028) VALV
 2028 FORMAT('ZD,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C
C     RET
      IF(TEMPC(I)(1:3).EQ.'RET') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TRIM(TEMPER(1:23))
      VALA=TEMPER(2:23)//' '
      CALL RIGHTJUST(VALA)
                        CALL ATOICODEV(VALA,IVALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE2,2029) IVALV
 2029 FORMAT('TILT RET,',I3)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     TILTS AND DECENTERD DONE
C
C     THICKNESS THI WITHOUT A SOLVE
C
C     NOT THI HMY, THI HMX, THI HCY THI HCX
      IF(TEMPC(I)(1:3).EQ.'THI') THEN
      IF(TEMPC(I)(1:7).NE.'THI HMY'.AND.
     1TEMPC(I)(1:7).NE.'THI HMX'.AND.
     1TEMPC(I)(1:7).NE.'THI HCY'.AND.
     1TEMPC(I)(1:7).NE.'THI HCX') THEN
C     JUST THI
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE2,2030) VALV
 2030 FORMAT('TH,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     THICKNESS DONE
C
C
C     CLAP DIMENSIONS
C
      IF(TEMPC(I)(1:3).EQ.'CIR'.OR.
     1TEMPC(I)(1:3).EQ.'REX'.OR.
     1TEMPC(I)(1:3).EQ.'REY'.OR.
     1TEMPC(I)(1:3).EQ.'ELX'.OR.
     1TEMPC(I)(1:3).EQ.'ELY') THEN
      IF(TEMPC(I)(1:7).NE.'CIR OBS'.AND.
     1TEMPC(I)(1:7).NE.'CIR EDG'.AND.
     2TEMPC(I)(1:7).NE.'CIR HOL') THEN
C     PROCEED
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:3).EQ.'CIR') THEN
      CLTYPE='C'
      CL1=VALV
      CL2=VALV
      CLAP1=.TRUE.
      CLAP2=.TRUE.
                        ELSE
      IF(CLTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'E') CLTYPE='E'
      IF(CLTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'R') CLTYPE='R'
      IF(TEMPC(I)(3:3).EQ.'X') THEN
C     X VALUE
      CL1=VALV
      CLAP1=.TRUE.
                        ELSE
C     Y VALUE
      CL2=VALV
      CLAP2=.TRUE.
                        END IF
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                ELSE
C     HOL OR EDGE OR OBS
      IF(TEMPC(I)(5:7).NE.'OBS') THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
                       END IF
                                END IF
                                END IF
C     CLAP DIMENSIONS DONE
C
C
C     CLAP DECENTERS  AND TILTS
C
      IF(TEMPC(I)(1:3).EQ.'ADX'.OR.
     1TEMPC(I)(1:3).EQ.'ADY'.OR.
     1TEMPC(I)(1:3).EQ.'ARO') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(.NOT.CLDX.AND.TEMPC(I)(3:3).EQ.'X') THEN
      CLDX=.TRUE.
      CL3=VALV
                        END IF
      IF(.NOT.CLDY.AND.TEMPC(I)(3:3).EQ.'Y') THEN
      CLDY=.TRUE.
      CL4=VALV
                        END IF
      IF(.NOT.CLTILT.AND.TEMPC(I)(1:3).EQ.'ARO') THEN
      CLTILT=.TRUE.
      CL5=VALV
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CLAP DECENTERS AND TILTS DONE
C
C     COBS DIMENSIONS
C
      IF(TEMPC(I)(1:7).EQ.'CIR OBS'.OR.
     1TEMPC(I)(1:7).EQ.'REX OBS'.OR.
     1TEMPC(I)(1:7).EQ.'REY OBS'.OR.
     1TEMPC(I)(1:7).EQ.'ELX OBS'.OR.
     1TEMPC(I)(1:7).EQ.'ELY OBS') THEN
C     PROCEED
      TEMPER=TEMPC(I)(8:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:3).EQ.'CIR') THEN
      COTYPE='C'
      CO1=VALV
      CO2=VALV
      COBS1=.TRUE.
      COBS2=.TRUE.
                        ELSE
      IF(COTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'E') COTYPE='E'
      IF(COTYPE.EQ.'?'.AND.TEMPC(I)(1:1).EQ.'R') COTYPE='R'
      IF(TEMPC(I)(3:3).EQ.'X') THEN
C     X VALUE
      CO1=VALV
      COBS1=.TRUE.
                        ELSE
C     Y VALUE
      CO2=VALV
      COBS2=.TRUE.
                        END IF
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     COBS DIMENSIONS DONE
C
C
C     COBS DECENTERS  AND TILTS
C
      IF(TEMPC(I)(1:7).EQ.'ADX OBS'.OR.
     1TEMPC(I)(1:7).EQ.'ADY OBS '.OR.
     1TEMPC(I)(1:7).EQ.'ARO OBS') THEN
      TEMPER=TEMPC(I)(8:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(.NOT.COBX.AND.TEMPC(I)(3:3).EQ.'X') THEN
      COBX=.TRUE.
      CO3=VALV
                        END IF
      IF(.NOT.COBY.AND.TEMPC(I)(3:3).EQ.'Y') THEN
      COBY=.TRUE.
      CO4=VALV
                        END IF
      IF(.NOT.COTILT.AND.TEMPC(I)(1:3).EQ.'ARO') THEN
      COTILT=.TRUE.
      CO5=VALV
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     COBS DECENTERS AND TILTS DONE
C
C     DIFFRACTION GRATING COMMANDS
C
C     GRT, GRO, GRS, GRX, GRY, GRX
      IF(TEMPC(I)(1:3).EQ.'GRT'.OR.
     1TEMPC(I)(1:3).EQ.'GRS'.OR.
     1TEMPC(I)(1:3).EQ.'GRO'.OR.
     1TEMPC(I)(1:3).EQ.'GRX'.OR.
     1TEMPC(I)(1:3).EQ.'GRY'.OR.
     1TEMPC(I)(1:3).EQ.'GRZ') THEN
      IF(TEMPC(I)(1:3).EQ.'GRT') THEN
      WRITE(OUTLYNE2,2031)
 2031 FORMAT('GRT')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).NE.'GRT') THEN
      IF(TEMPC(I)(1:3).NE.'GRO') THEN
C     BREAK OUT A VALUE
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
                   ELSE
C     BREAK OUT A VALUE
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      CALL ATOICODEV(VALA,IVALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
          END IF
      IF(TEMPC(I)(1:3).EQ.'GRO') THEN
      WRITE(OUTLYNE2,2032) DBLE(IVALV)
 2032 FORMAT('GRO,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'GRS') THEN
      WRITE(OUTLYNE2,2033) VALV
 2033 FORMAT('GRS,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'GRX') THEN
      WRITE(OUTLYNE2,2034) VALV
 2034 FORMAT('GRX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'GRY') THEN
      WRITE(OUTLYNE2,2035) VALV
 2035 FORMAT('GRY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'GRZ') THEN
      WRITE(OUTLYNE2,2036) VALV
 2036 FORMAT('GRZ,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     GRATING COMMANDS DONE
C
C     HOE COMMANDS
C
C     HOE, HTO, HOR, HWL, HX1,HY1,HZ1,HX2,HY2,HZ2
      IF(TEMPC(I)(1:3).EQ.'HOE'.OR.
     1TEMPC(I)(1:3).EQ.'HOR'.OR.
     1TEMPC(I)(1:3).EQ.'HWL'.OR.
     1TEMPC(I)(1:3).EQ.'HX1'.OR.
     1TEMPC(I)(1:3).EQ.'HY1'.OR.
     1TEMPC(I)(1:3).EQ.'HZ1'.OR.
     1TEMPC(I)(1:3).EQ.'HX2'.OR.
     1TEMPC(I)(1:3).EQ.'HY2'.OR.
     1TEMPC(I)(1:3).EQ.'HZ2'.OR.
     1TEMPC(I)(1:3).EQ.'HV1'.OR.
     1TEMPC(I)(1:3).EQ.'HV2') THEN
      IF(TEMPC(I)(1:3).NE.'HOE'.AND.
     1TEMPC(I)(1:3).NE.'HV1'.AND.
     1TEMPC(I)(1:3).NE.'HV2') THEN
C     BREAK OUT A VALUE
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:3).EQ.'HOE')
     1HOE(SURFER)=.TRUE.
      IF(TEMPC(I)(1:7).EQ.'HV1 REA')
     1HV1(SURFER)='R'
      IF(TEMPC(I)(1:7).EQ.'HV1 VIR')
     1HV1(SURFER)='V'
      IF(TEMPC(I)(1:7).EQ.'HV2 REA')
     1HV2(SURFER)='R'
      IF(TEMPC(I)(1:7).EQ.'HV2 VIR')
     1HV2(SURFER)='V'
      IF(TEMPC(I)(1:3).EQ.'HX1')
     1HX1(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HY1')
     1HY1(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HZ1')
     1HZ1(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HX2')
     1HX2(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HY2')
     1HY2(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HZ2')
     1HZ2(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HWL')
     1HWL(SURFER)=VALV
      IF(TEMPC(I)(1:3).EQ.'HOR')
     1HOR(SURFER)=VALV
      HOESUR(SURFER)=SURFER
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     GRATING COMMANDS DONE
C
C     THICKNESS SOLVES GO HERE
C
C     THI HMY, THI HMX, THI HCY THI HCX
      IF(TEMPC(I)(1:7).EQ.'THI HMY'.OR.
     1TEMPC(I)(1:7).EQ.'THI HMX'.OR.
     1TEMPC(I)(1:7).EQ.'THI HCY'.OR.
     1TEMPC(I)(1:7).EQ.'THI HCX') THEN
      TEMPER=TEMPC(I)(8:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:7).EQ.'THI HMX') THEN
      WRITE(OUTLYNE2,2037) VALV
 2037 FORMAT('PX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'THI HMY') THEN
      WRITE(OUTLYNE2,2038) VALV
 2038 FORMAT('PY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'THI HCX') THEN
      WRITE(OUTLYNE2,2039) VALV
 2039 FORMAT('PCX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'THI HCY') THEN
      WRITE(OUTLYNE2,2040) VALV
 2040 FORMAT('PCY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     THICKNESS SOLVES DONE
C
C     CURVATURE/RADII GO HERE
C
      IF(TEMPC(I)(1:3).EQ.'CUX'.OR.
     1TEMPC(I)(1:3).EQ.'CUY'.OR.
     2TEMPC(I)(1:3).EQ.'RDX'.OR.
     3TEMPC(I)(1:3).EQ.'RDY') THEN
      IF(TEMPC(I)(1:7).NE.'CUX UMX'.AND.
     1   TEMPC(I)(1:7).NE.'CUY UMY'.AND.
     1   TEMPC(I)(1:7).NE.'CUX UCX'.AND.
     1   TEMPC(I)(1:7).NE.'CUY UCY'.AND.
     1   TEMPC(I)(1:7).NE.'CUX IMX'.AND.
     1   TEMPC(I)(1:7).NE.'CUY IMY'.AND.
     1   TEMPC(I)(1:7).NE.'CUX ICX'.AND.
     1   TEMPC(I)(1:7).NE.'CUY ICY') THEN
C     PROCEED, CURVATURE OR RADII
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
C     CONVERT TO RADII IF NECESSARY
      IF(TEMPC(I)(1:1).EQ.'R'.AND.VALV.NE.0.0D0) VALV=1.0D0/VALV
C     FIRST IF ROTATIONALLY SYMMETRIC
      IF(TOR.EQ.'? '.AND.AAS.EQ.'?'.AND.CYL.EQ.'?') THEN
      IF(TEMPC(I)(3:3).EQ.'Y') THEN
C     YZ PLANE
      IF(TEMPC(I)(1:1).EQ.'R') THEN
      WRITE(OUTLYNE2,2041) VALV
 2041 FORMAT('RD,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'C') THEN
      WRITE(OUTLYNE2,2042) VALV
 2042 FORMAT('CV,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        ELSE
C     NOTHING TO DO, RDX AND CUX NOT RECOGNIZED
                        END IF
                        END IF
C
C     SET TOR IF CYL NOT ?
      IF(CYL.EQ.'C') THEN
                IF(TEMPC(I)(3:3).EQ.'Y') TOR='YT'
                IF(TEMPC(I)(3:3).EQ.'Y') THEN
      WRITE(OUTLYNE2,2043)
 2043 FORMAT('YTORIC')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                IF(TEMPC(I)(3:3).EQ.'X') TOR='XT'
                IF(TEMPC(I)(3:3).EQ.'X') THEN
      WRITE(OUTLYNE2,2044)
 2044 FORMAT('XTORIC')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        END IF
C
      IF(TOR.EQ.'YT') THEN
C     Y TORIC
      IF(TEMPC(I)(3:3).EQ.'Y') THEN
C     YZ PLANE
      IF(TEMPC(I)(1:1).EQ.'R') THEN
      WRITE(OUTLYNE2,2045) VALV
 2045 FORMAT('RDTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'C') THEN
      WRITE(OUTLYNE2,2046) VALV
 2046 FORMAT('CVTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        ELSE
C     XZ PLANE
      IF(TEMPC(I)(1:1).EQ.'R') THEN
      WRITE(OUTLYNE2,2047) VALV
 2047 FORMAT('RD,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'C') THEN
      WRITE(OUTLYNE2,2048) VALV
 2048 FORMAT('CV,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        END IF
                        END IF
      IF(TOR.EQ.'XT') THEN
C     X TORIC
      IF(TEMPC(I)(3:3).EQ.'Y') THEN
C     YZ PLANE
      IF(TEMPC(I)(1:1).EQ.'R') THEN
      WRITE(OUTLYNE2,2049) VALV
 2049 FORMAT('RD,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'C') THEN
      WRITE(OUTLYNE2,2050) VALV
 2050 FORMAT('CV,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        ELSE
C     XZ PLANE
      IF(TEMPC(I)(1:1).EQ.'R') THEN
      WRITE(OUTLYNE2,2051) VALV
 2051 FORMAT('RDTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'C') THEN
      WRITE(OUTLYNE2,2052) VALV
 2052 FORMAT('CVTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        END IF
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     CURVATURE/RADIUS DONE
C
C     CONICS AND ASPHERICS GO HERE IF TOR='? '
      IF(TOR.EQ.'? ') THEN
C     ROTATIONALLY SYMMETRIC SURFACES
      IF(TEMPC(I)(1:2).EQ.'K '.OR.
     1   TEMPC(I)(1:2).EQ.'A '.OR.
     2   TEMPC(I)(1:2).EQ.'B '.OR.
     2   TEMPC(I)(1:2).EQ.'C '.OR.
     2   TEMPC(I)(1:2).EQ.'D '.OR.
     2   TEMPC(I)(1:2).EQ.'E '.OR.
     2   TEMPC(I)(1:2).EQ.'F '.OR.
     2   TEMPC(I)(1:2).EQ.'G '.OR.
     2   TEMPC(I)(1:2).EQ.'H '.OR.
     3   TEMPC(I)(1:2).EQ.'J ') THEN
C     PROCEED
      TEMPER=TEMPC(I)(3:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:1).EQ.'K') THEN
      WRITE(OUTLYNE2,2053) VALV
 2053 FORMAT('CC,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'A') THEN
      WRITE(OUTLYNE2,2054) VALV
 2054 FORMAT('AD,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'B') THEN
      WRITE(OUTLYNE2,2154) VALV
 2154 FORMAT('AE,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'C') THEN
      WRITE(OUTLYNE2,2055) VALV
 2055 FORMAT('AF,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'D') THEN
      WRITE(OUTLYNE2,2056) VALV
 2056 FORMAT('AG,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'E') THEN
      WRITE(OUTLYNE2,2057) VALV
 2057 FORMAT('AH,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'F') THEN
      WRITE(OUTLYNE2,2058) VALV
 2058 FORMAT('AI,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'G') THEN
      WRITE(OUTLYNE2,2059) VALV
 2059 FORMAT('AJ,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'H') THEN
      WRITE(OUTLYNE2,2060) VALV
 2060 FORMAT('AK,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:1).EQ.'J') THEN
      WRITE(OUTLYNE2,2061) VALV
 2061 FORMAT('AL,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     ROTATIONALLY SYMMETRIC SURFACES ASPHERICS DONE
C
C     CONICS AND ASPHERICS GO HERE IF TOR NOT '?'
      IF(TOR.NE.'? '.AND.AAS.EQ.'?') THEN
C     TORICS
      IF(TEMPC(I)(1:2).EQ.'K '.OR.
     1   TEMPC(I)(1:2).EQ.'A '.OR.
     2   TEMPC(I)(1:2).EQ.'B '.OR.
     2   TEMPC(I)(1:2).EQ.'C '.OR.
     3   TEMPC(I)(1:2).EQ.'D ') THEN
      TEMPER=TEMPC(I)(3:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:1).EQ.'K') THEN
      WRITE(OUTLYNE2,2062) VALV
 2062 FORMAT('CCTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(2:2).EQ.'A ') THEN
      WRITE(OUTLYNE2,2063) VALV
 2063 FORMAT('ADTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(2:2).EQ.'B ') THEN
      WRITE(OUTLYNE2,2064) VALV
 2064 FORMAT('AETOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(2:2).EQ.'C ') THEN
      WRITE(OUTLYNE2,2065) VALV
 2065 FORMAT('AFTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(2:2).EQ.'D ') THEN
      WRITE(OUTLYNE2,2066) VALV
 2066 FORMAT('AGTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        GO TO 213
 213                  CONTINUE
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     NON AAS TORICS ASPHERICS DONE
C
C     ANAMORPHIC ASPHERICS
      IF(AAS.EQ.'Y') THEN
C     ANAMORPHIC ASPHERICS
      IF(TEMPC(I)(1:3).EQ.'KY '.OR.
     1   TEMPC(I)(1:3).EQ.'KX '.OR.
     2   TEMPC(I)(1:3).EQ.'AR '.OR.
     2   TEMPC(I)(1:3).EQ.'AP '.OR.
     2   TEMPC(I)(1:3).EQ.'BR '.OR.
     2   TEMPC(I)(1:3).EQ.'BP '.OR.
     2   TEMPC(I)(1:3).EQ.'CR '.OR.
     2   TEMPC(I)(1:3).EQ.'CP '.OR.
     2   TEMPC(I)(1:3).EQ.'DR '.OR.
     3   TEMPC(I)(1:3).EQ.'DP ') THEN
      TEMPER=TEMPC(I)(4:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(1:2).EQ.'KY') THEN
      WRITE(OUTLYNE2,2067) VALV
 2067 FORMAT('CC,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:2).EQ.'KX') THEN
      WRITE(OUTLYNE2,2068) VALV
 2068 FORMAT('CCTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'AR ') THEN
      WRITE(OUTLYNE2,2069) VALV
 2069 FORMAT('AD,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'AP ') THEN
      WRITE(OUTLYNE2,2070) VALV
 2070 FORMAT('ADTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'BR ') THEN
      WRITE(OUTLYNE2,2071) VALV
 2071 FORMAT('AE,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'BP ') THEN
      WRITE(OUTLYNE2,2072) VALV
 2072 FORMAT('AETOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'CR ') THEN
      WRITE(OUTLYNE2,2073) VALV
 2073 FORMAT('AF,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'CP ') THEN
      WRITE(OUTLYNE2,2074) VALV
 2074 FORMAT('AFTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'DR ') THEN
      WRITE(OUTLYNE2,2075) VALV
 2075 FORMAT('AG,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:3).EQ.'DP ') THEN
      WRITE(OUTLYNE2,2076) VALV
 2076 FORMAT('AGTOR,'D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        GO TO 216
 216                  CONTINUE
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C     AAS TORICS ASPHERICS DONE
C
C     CURVATURE SOLVES GO HERE
C
C     CUY AND CUX WITH UMY/UMX,UCY/UCX,IMY/IMX AND ICY/ICX
      IF(TEMPC(I)(1:7).EQ.'CUX UMX'.OR.
     1   TEMPC(I)(1:7).EQ.'CUY UMY'.OR.
     1   TEMPC(I)(1:7).EQ.'CUX UCX'.OR.
     1   TEMPC(I)(1:7).EQ.'CUY UCY'.OR.
     1   TEMPC(I)(1:7).EQ.'CUX IMX'.OR.
     1   TEMPC(I)(1:7).EQ.'CUY IMY'.OR.
     1   TEMPC(I)(1:7).EQ.'CUX ICX'.OR.
     1   TEMPC(I)(1:7).EQ.'CUY ICY') THEN
      TEMPER=TEMPC(I)(8:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
                        CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      IF(TEMPC(I)(3:3).EQ.'X') THEN
      IF(CYL.EQ.'C') THEN
      WRITE(OUTLYNE2,2077)
 2077 FORMAT('XTORIC')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(CYL.EQ.'C') TOR='XT'
                        END IF
      IF(TEMPC(I)(3:3).EQ.'Y') THEN
      IF(CYL.EQ.'C') THEN
      WRITE(OUTLYNE2,2078)
 2078 FORMAT('YTORIC')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(CYL.EQ.'C') TOR='YT'
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUX UMX') THEN
      WRITE(OUTLYNE2,2079) VALV
 2079 FORMAT('PUX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUY UMY') THEN
      WRITE(OUTLYNE2,2080) VALV
 2080 FORMAT('PUY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUX UCX') THEN
      WRITE(OUTLYNE2,2081) VALV
 2081 FORMAT('PUCX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUY UCY') THEN
      WRITE(OUTLYNE2,2082) VALV
 2082 FORMAT('PUCY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUX IMX') THEN
      WRITE(OUTLYNE2,2083) VALV
 2083 FORMAT('PIX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUY IMY') THEN
      WRITE(OUTLYNE2,2084) VALV
 2084 FORMAT('PIY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUX ICX') THEN
      WRITE(OUTLYNE2,2085) VALV
 2085 FORMAT('PICX,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(TEMPC(I)(1:7).EQ.'CUY ICY') THEN
      WRITE(OUTLYNE2,2086) VALV
 2086 FORMAT('PICY,',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CURVATURE SOLVES DONE
C
      IF(TEMPC(I)(1:2).EQ.'GO') THEN
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
C     IF WE ARE HERE, THE COMMAND DID NOT TRANSLATE
      IF(TEMPC(I)(1:75).NE.BL1024(1:78)) THEN
      WRITE(38,4001) TEMPCC(1:78)
                        END IF
 4001 FORMAT(' ',A78)
C     DO NEXT LINE IN THE CODE V SEQUENCE FILE
 8888                   CONTINUE
                        END DO
      WRITE(OUTLYNE2,2016) GLASSA
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                SURFER=SURFER+1
      HOESUR(SURFER)=-1
      TOR='? '
      TIL='?'
      CYL='?'
      AAS='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CLTILT=.FALSE.
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2088)
 2088 FORMAT('EOS')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
C     NOW HOE STUFF IF NEEDED
                        DO I=0,MAXSUR
      IF(HOE(I)) GO TO 220
                        END DO
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,1)
                        RETURN
 220                    CONTINUE
      WRITE(OUTLYNE2,2089)
 2089 FORMAT('SPSRF')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        DO I=0,MAXSUR
      IF(HOE(I)) THEN
      WRITE(OUTLYNE2,2090) I
 2090 FORMAT('SPECIAL,',I3,', 12')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2091) I,HOR(I)
 2091 FORMAT('C1,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2092) I,HWL(I)
 2092 FORMAT('C2,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2093) I,HX1(I)
 2093 FORMAT('C3,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2094) I,HY1(I)
 2094 FORMAT('C4,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2095) I,HZ1(I)
 2095 FORMAT('C5,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      IF(HV1(I).EQ.'R') THEN
      WRITE(OUTLYNE2,2097) I
 2097 FORMAT('C5,',I3,', +1.0D0')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(HV1(I).EQ.'V') THEN
      WRITE(OUTLYNE2,2098) I
 2098 FORMAT('C5,',I3,', -1.0D0')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      WRITE(OUTLYNE2,2099) I,HX2(I)
 2099 FORMAT('C7,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2100) I,HY2(I)
 2100 FORMAT('C8,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      WRITE(OUTLYNE2,2101) I,HZ2(I)
 2101 FORMAT('C9,',I3,',',D23.15)
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      IF(HV2(I).EQ.'R') THEN
      WRITE(OUTLYNE2,2102) I
 2102 FORMAT('C10,',I3,', +1.0D0')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
      IF(HV2(I).EQ.'V') THEN
      WRITE(OUTLYNE2,2103) I
 2103 FORMAT('C10,',I3,', -1.0D0')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                        END IF
                        END IF
                        END DO
      WRITE(OUTLYNE2,2104)
 2104 FORMAT('EOS')
      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,1)
      DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
      DEALLOCATE (HOE,HOESUR,HV1,HV2,HX1,HY1,HZ1
     1,HWL,HX2,HY2,HZ2,HOR,STAT=ALLOERR)
                                RETURN
 !100  FORMAT(A132)
 !100  FORMAT(readformat)

                                END

      SUBROUTINE UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)

        USE GLOBALS
        IMPLICIT NONE

        CHARACTER(LEN=1024) :: OUTLYNE2
        INTEGER :: SUB

      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATLEN.INC'

          SAVE_KDP(1)=SAVEINPT(1)
          INPUT(1:SUB)=OUTLYNE2(1:SUB)
          IF(INPUT(1:20).NE.AA) CALL PROCES
          REST_KDP(1)=RESTINPT(1)

      END SUBROUTINE
