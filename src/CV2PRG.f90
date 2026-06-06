! SUB CV2PRG.FOR
SUBROUTINE CV2PRG
   USE GLOBALS
   use zoa_file_handler
   use zoa_ui_callbacks, only: notify_close_all_tabs
   use strings
   !use kdp_interfaces, only: OUTKDP
!
   use DATLEN
   use DATMAI
   use mod_lens_data_manager, only: ldm
   use undo_manager, only: undo_reset_baseline
   use iso_fortran_env, only: real64
   IMPLICIT NONE
!
   character(len=80) :: tokens(40)
   integer :: numTokens
   CHARACTER CVFILENAME*80,KDPFILENAME*80,CV_INPUT_STRING*1024
   character(len=6) :: readformat
   character(len=3) :: substr
   character(len=1024) :: filePath
!
   CHARACTER TEMPA*1024,TEMPB*1024,CVA*23,THA*23,AINDEX*8 &
   &,GLASSA*40,VALA*23,AA23*23,SUMSTRING*1024,OUTLYNE2*1024 &
   &,TEMPC*1024,BL1024*1024,TOR*2,TIL*1,CYL*1,TEMPER*1024 &
   &,FIELDTYPE*3,AAS*1,CLTYPE*1,COTYPE*1,TEMPCC*1024,AVNUMB*7
!
   LOGICAL EXIS37,EXIS38,OPEN37,OPEN38,SEMI,ADD,RADON,OLDADD
!
   LOGICAL CVERROR
   LOGICAL :: COMMANDINFOCHECK, STRINGINPUTCHECK, CHECKMAXFLOATINPUTS
!
   LOGICAL DOWV,DOWV2,CLAP1,CLAP2,CLDX,CLDY,CLTILT
!
   LOGICAL COBS1,COBS2,COBX,COBY,COTILT
!
   INTEGER I,CVFILENAMELENGTH,KDPFILENAMELENGTH,NPERIOD,ALLOERR
!
   INTEGER J,K,L,NADD,II,OLDNADD,STRINGEND,WDEXIS(1:100),M
!
   INTEGER J1,J2,IVALV,IWD(1:100),SURFER, SUB
!
   real(real64) CV,TH,VALV,WD(1:100),MAX,CL1,CL2,CL3,CL4,CL5
!
   real(real64) CO1,CO2,CO3,CO4,CO5
!
   DIMENSION TEMPA(:),TEMPB(:),TEMPC(:)
!
   ALLOCATABLE :: TEMPA,TEMPB,TEMPC
!
   LOGICAL HOE
   INTEGER HOESUR
   CHARACTER*1 HV1,HV2
   real(real64) HX1,HY1,HZ1,HX2,HY2,HZ2,HWL,HOR
   DIMENSION HV1(:),HV2(:),HX1(:),HY1(:),HZ1(:),HX2(:),HY2(:),&
   &HZ2(:),HOESUR(:),HOE(:),HWL(:),HOR(:)
   ALLOCATABLE :: HWL,HV1,HV2,HX1,HY1,HZ1,HX2,HY2,&
   &HZ2,HOESUR,HOE,HOR

   integer :: dotLoc
!

   SUB = 256
   write(substr, "(I3)") SUB
   readformat = '(A' // substr // ')'
   PRINT *, "READFORMAT IS ", readformat
   SURFER=0

   !PRINT *, "CV2PRG Starting.."
!
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
!
!       THIS SUBROUTINE IS CALLED CONVERT A FILE FROM CODE V TO PRG
!     LENS INPUT FORMAT
!
   PRINT *, "CV2PRG Routine Started!"

   AA23='                       '
   BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA &
   &//AA//AA//AA//AA//AA//AA//AA//AA//AA//AA &
   &//AA//AA//AA//AA//AA//AA//AA//AA//AA//AA &
   &//AA//AA//AA//AA//AA//AA//AA//AA//AA//AA &
   &//AA//AA//AA//AA//AA//AA//AA//AA//AA//AA &
   &//AA//'    '
!
   IF (COMMANDINFOCHECK(&
   &"CV2PRG CONVERTS THE NAMED FILE (STRING INPUT)")) RETURN


   IF(SN.EQ.1.OR.SQ.EQ.1) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"CV2PRG" TAKES NO NUMERIC OR QUALIFIER INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF
   IF(SST.EQ.0) THEN
      CALL REPORT_ERROR_AND_FAIL(&
      & '"CV2PRG" REQUIRES EXPLICIT STRING (FILE NAME) INPUT'//'\n'//&
      & 'RE-ENTER COMMAND', 1)
      RETURN
   END IF

   CVFILENAME=WS
   call getInputStringLength(CVFILENAME, CVFILENAMELENGTH)

   IF (CVFILENAMELENGTH.EQ.0) THEN

      CALL REPORT_ERROR_AND_FAIL(&
      & 'INVALID (ZERO LENGTH) CODE V FILE NAME'//'\n'//&
      & 'NO ACTION TAKEN', 1)
      RETURN

   END IF

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! End input error checking

   EXIS37=.FALSE.
   filePath = trim(getCodeVDir())//getFileSep()//&
   &CVFILENAME(1:CVFILENAMELENGTH)


   INQUIRE(&
   &FILE=trim(filePath),&
   &EXIST=EXIS37)

   !  INQUIRE(
   ! 1 FILE=trim(basePath)//'CodeV'//
   ! 1 getFileSep()//CVFILENAME(1:CVFILENAMELENGTH),
   ! 1 EXIST=EXIS37)

   IF(.NOT.EXIS37) THEN
      CALL REPORT_ERROR_AND_FAIL('NO CODE V INPUT FILE EXISTS TO READ'//'\n'//'NO ACTION TAKEN', 1)
      RETURN
   END IF


   call notify_close_all_tabs("You are about to open a new " //&
   &"lens system.  This will invalidate all plots.   " //&
   &"Press yes to close them.")

!     FILES EXISTS, CREATE PROGRAM FILE NAME
   !call logger%logText("CV2PRG Find Extension in File Name")

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
!     OPEN THE CVFILENAME TO READ IT AS ASCII
!
   DEALLOCATE (TEMPA,TEMPB,TEMPC &
   &,STAT=ALLOERR)
   DEALLOCATE (&
   &HWL,&
   &HV1,&
   &HV2,&
   &HX1,&
   &HY1,&
   &HZ1,&
   &HX2,&
   &HY2,&
   &HZ2,&
   &HOE,&
   &HOR,&
   &HOESUR,STAT=ALLOERR)
   ALLOCATE (TEMPA(1:5000),TEMPB(1:5000),TEMPC(1:5000)&
   &,STAT=ALLOERR)
   ALLOCATE (&
   &HWL(0:MAXSUR),&
   &HV1(0:MAXSUR),&
   &HV2(0:MAXSUR),&
   &HX1(0:MAXSUR),&
   &HY1(0:MAXSUR),&
   &HZ1(0:MAXSUR),&
   &HX2(0:MAXSUR),&
   &HY2(0:MAXSUR),&
   &HZ2(0:MAXSUR),&
   &HOE(0:MAXSUR),&
   &HOR(0:MAXSUR),&
   &HOESUR(0:MAXSUR),STAT=ALLOERR)
   I=MAXSUR
   HOE(0:I)=.FALSE.
   HOESUR(0:I)=-1
   TEMPA(1:5000)=BL1024
   TEMPB(1:5000)=BL1024
   TEMPC(1:5000)=BL1024
!

!     CLOSE IT IF OPEN AND KEEP IY
   CALL CLOSE_FILE(37,1)
   CALL CLOSE_FILE(38,0)
   call logger%logText("CV2PRG Open File " // CVFILENAME)

   OPEN(UNIT=37,ACCESS='SEQUENTIAL',BLANK='NULL'&
   &,FORM='FORMATTED',FILE=trim(filePath)&
   &,STATUS='UNKNOWN')
   OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'&
   &,FORM='FORMATTED',&
   &FILE=trim(basePath)//'CONVERT.ERR'&
   &,STATUS='UNKNOWN')
   CALL CLOSE_FILE(38,0)
   OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'&
   &,FORM='FORMATTED',&
   &FILE=trim(basePath)//'CONVERT.ERR'&
   &,STATUS='UNKNOWN')


   II=1
   ADD=.FALSE.
   OLDADD=.FALSE.
   DO I=1,5000
!     READ A LINE OF THE FILE AS A 132 CHARACTER, CHARACTER
      CV_INPUT_STRING=BL1024
      READ(37,readformat,ERR=75,END=76) CV_INPUT_STRING(1:SUB)
      call logger%logText("Processing Line: " //CV_INPUT_STRING(1:SUB))

!     IS THERE A CONTINUATION MARK AT THE END ?
!     REMEMBER IF ADD WAS ON LAST TIME
      OLDADD=ADD
      DO J=1024,1,-1

         ADD=.FALSE.
         IF(CV_INPUT_STRING(J:J).EQ.'&') THEN
!     YES
            ADD=.TRUE.
            NADD=J
            EXIT ! Exit do loop
         ELSE
         END IF

      END DO
      PRINT *, "J is ", J

      IF(.NOT.ADD.AND..NOT.OLDADD) THEN
!     NOTHING TO ADD, WRITE A TEMPA VALUE
         TEMPA(II)(1:SUB)=CV_INPUT_STRING(1:SUB)
!     BLANK OUT ALL &
         DO J=1024,1,-1
            IF(TEMPA(II)(J:J).EQ.'&') TEMPA(II)(J:J)=' '
         END DO

         II=II+1
         ADD=.FALSE.
         OLDADD=.FALSE.
      END IF
!
      IF(ADD.AND..NOT.OLDADD) THEN
!     THEN WRITE AN INITIAL SUMSTRING AND GET ITS LENGTH
         SUMSTRING(1:1024)=CV_INPUT_STRING(1:1024)
         DO J=1024,1,-1
            IF(SUMSTRING(J:J).NE.' ') THEN
               STRINGEND=J
               EXIT
            END IF
         END DO

      END IF

      IF(.NOT.ADD.AND.OLDADD) THEN
!     ADD, THEN WRITE A TEMPA VALUE
         IF(STRINGEND.LT.1024) THEN
            SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//&
            &CV_INPUT_STRING(1:(1024-STRINGEND))
         END IF
!     GET THE NEW STRINGEND
         DO J=1024,1,-1
            IF(SUMSTRING(J:J).NE.' ') THEN
               STRINGEND=J
               EXIT
            END IF
         END DO

         TEMPA(II)(1:1024)=SUMSTRING(1:1024)
!     BLANK OUT ALL &
         DO J=1024,1,-1
            IF(TEMPA(II)(J:J).EQ.'&') TEMPA(II)(J:J)=' '
         END DO
         II=II+1
         ADD=.FALSE.
         OLDADD=.FALSE.
      END IF
      IF(ADD.AND.OLDADD) THEN
!     ADD, TO MAKE A NEW SUMSTRING
         IF(STRINGEND.LT.1024) THEN
            SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//&
            &CV_INPUT_STRING(1:(1024-STRINGEND))
         END IF
!     GET THE NEW STRINGEND
         DO J=1024,1,-1
            IF(SUMSTRING(J:J).NE.' ') THEN
               STRINGEND=J
               EXIT
            END IF
         END DO

      END IF
   END DO
   GO TO 76
75 CONTINUE
   CALL REPORT_ERROR_AND_FAIL(&
   & 'ERROR READING CODE V INPUT FILE'//'\n'//&
   & 'NO FILE CONVERSION PERFORMED', 1)
   CALL CLOSE_FILE(37,1)
   CALL CLOSE_FILE(38,0)
   DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
   DEALLOCATE (HOE,HOESUR,HV1,HV2,HX1,HY1,HZ1 &
   &,HWL,HX2,HY2,HZ2,HOR,STAT=ALLOERR)
   RETURN
76 CONTINUE

!     NOW REMOVE ALL VIRTUAL RETURNS (SEMI-COLONS) AND LOAD TEMPB
!     CYCLE THROUGH THE II-1 ENTRIES IN TEMPA
   K=1
   DO I=1,II-1
!     ARE THERE ANY SEMICOLONS
      L=1
      SEMI=.FALSE.
      DO J=1,1024
         IF(TEMPA(I)(J:J).EQ.';') SEMI=.TRUE.
      END DO
      IF(SEMI) THEN
!     MULTIPLE COMMANDS PER LINE
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
!     ONLY ONE INSTRUCTION ON THE LINE
         TEMPB(K)=TEMPA(I)
         K=K+1
      END IF

   END DO
!     NOW REMOVE ALL EVIL CHARACTERS AND LEADING BLANKS
   DO I=1,K-1
      DO J=1,1024
         IF(ICHAR(TEMPB(I)(J:J)).LT.32.OR.&
         &ICHAR(TEMPB(I)(J:J)).GT.126) TEMPB(I)(J:J)=' '
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
!
!     INSTRUCTION TRASNLATION
!
4000 FORMAT(' THE FOLLOWING CODE-V COMMAND DID NOT TRANSLATE:')
   WRITE(38,4000)
   DO I=1,L
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
!     RDM
      IF(TEMPC(I)(1:3).EQ.'RDM') THEN
         IF(TEMPC(I)(1:5).EQ.'RDM N') RADON=.FALSE.
         IF(TEMPC(I)(1:5).NE.'RDM N') RADON=.TRUE.
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     SPH OR CON
      IF(TEMPC(I)(1:3).EQ.'SPH'.OR.&
      &TEMPC(I)(1:3).EQ.'CON'.OR.&
      &TEMPC(I)(1:3).EQ.'ASP') THEN
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     LEN
      IF(TEMPC(I)(1:3).EQ.'LEN') THEN
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='LENS'
         IF(INPUT(1:20).NE.AA) CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     TITLE
      IF(TEMPC(I)(1:5).EQ.'TITLE'.OR.TEMPC(I)(1:3).EQ.'TIT') THEN
         J1=0
         J2=0
         DO J=1024,1,-1
            IF(TEMPC(I)(J:J).EQ.'''') THEN
               J2=J
               GO TO 201
            END IF
         END DO
201      CONTINUE
         DO J=1,1024
            IF(TEMPC(I)(J:J).EQ.'''') THEN
               J1=J
               GO TO 102
            END IF
         END DO
102      CONTINUE
!     Only process a quoted title; if quotes are missing or empty,
!     J1/J2 stay 0 and we skip (avoids slicing with bad bounds).
         IF(J1.GE.1.AND.J2.LE.1024.AND.J2.GT.J1+1) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='LI '//TEMPC(I)(J1+1:J2-1)
            IF(TEMPC(I)(J1+1:J2-1).NE.BL1024(J1+1:J2-1)) THEN
               IF(INPUT(1:20).NE.AA) CALL PROCES
            END IF
            REST_KDP(1)=RESTINPT(1)
         END IF
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     INI
      IF(TEMPC(I)(1:3).EQ.'INI') THEN
         J1=0
         J2=0
         DO J=1024,1,-1
            IF(TEMPC(I)(J:J).EQ.'''') THEN
               J2=J
               GO TO 202
            END IF
         END DO
202      CONTINUE
         DO J=1,1024
            IF(TEMPC(I)(J:J).EQ.'''') THEN
               J1=J
               GO TO 121
            END IF
         END DO
121      CONTINUE
!     Only process a quoted INI string; if quotes are missing or empty,
!     J1/J2 stay 0 and we skip (avoids slicing with bad bounds).
         IF(J1.GE.1.AND.J2.LE.1024.AND.J2.GT.J1+1) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='INI '//TEMPC(I)(J1+1:J2-1)
            IF(TEMPC(I)(J1+1:J2-1).NE.BL1024(J1+1:J2-1)) THEN
               IF(INPUT(1:20).NE.AA) CALL PROCES
            END IF
            REST_KDP(1)=RESTINPT(1)
         END IF
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     EPD
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
2001     FORMAT('SAY,',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     NAO
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
2002     FORMAT('NAO,',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     REF
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
2003     FORMAT('CW,',I2)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     YAN,XAN,YOB,XOB,XIM,YIM,XRI,YRI
      IF(TEMPC(I)(1:3).EQ.'YAN'&
      &.OR.TEMPC(I)(1:3).EQ.'XAN'&
      &.OR.TEMPC(I)(1:3).EQ.'XIM'&
      &.OR.TEMPC(I)(1:3).EQ.'YIM'&
      &.OR.TEMPC(I)(1:3).EQ.'XRI'&
      &.OR.TEMPC(I)(1:3).EQ.'YRI'&
      &.OR.TEMPC(I)(1:3).EQ.'YOB'&
      &.OR.TEMPC(I)(1:3).EQ.'XOB') THEN
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
         IF(FIELDTYPE.EQ.'XAN'.OR.FIELDTYPE.EQ.'YAN'.OR.&
         &FIELDTYPE.EQ.'XOB'.OR.FIELDTYPE.EQ.'YOB') THEN
!     MAX FIELD POS CONVERT TO SCY. SCY FANG, SCX AND SCX FANG
!     BREAK OUT UP TO 25 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
!     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
!     THEN FILD LARGEST VALUE
!     AS NEEDED.
!     STRIP OFF 3 CHARACTERS
            !call OUTKDP("Entering YOB Loop")
            !call OUTKDP("Fieldtype is "//FIELDTYPE)
            TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
!     STRIP LEADING BLANKS
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
2004           FORMAT('SCY FANG,',D23.15)
               SAVE_KDP(1)=SAVEINPT(1)
               INPUT(1:SUB)=OUTLYNE2(1:SUB)
               IF(INPUT(1:20).NE.AA) CALL PROCES
               REST_KDP(1)=RESTINPT(1)
               TEMPC(I)(1:1024)=BL1024(1:1024)
            END IF
            IF(FIELDTYPE.EQ.'XAN') THEN
               WRITE(OUTLYNE2,2005) MAX
2005           FORMAT('SCX FANG,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
               TEMPC(I)(1:1024)=BL1024(1:1024)
            END IF
            IF(FIELDTYPE.EQ.'YOB') THEN
               !CALL OUTKDP('Really in FIELDTYPE YOB LOOP')
               WRITE(OUTLYNE2,2006) MAX
               !CALL OUTKDP(trim(OUTLYNE2))
2006           FORMAT('SCY,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)

               TEMPC(I)(1:1024)=BL1024(1:1024)
            END IF
            IF(FIELDTYPE.EQ.'XOB') THEN
               WRITE(OUTLYNE2,2007) MAX
2007           FORMAT('SCX,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
               TEMPC(I)(1:1024)=BL1024(1:1024)
            END IF
            GO TO 8888
         END IF
      END IF
      IF(FIELDTYPE.EQ.'XIM'.OR.FIELDTYPE.EQ.'YIM'.OR.&
      &FIELDTYPE.EQ.'XRI'.OR.FIELDTYPE.EQ.'YRI') THEN
!     IMAGE HT SPEC
!     STRIP OFF 3 CHARACTERS
         TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
!     STRIP LEADING BLANKS
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
4004        FORMAT('PXIM,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         IF(FIELDTYPE.EQ.'YIM') THEN
            WRITE(OUTLYNE2,4005) MAX
4005        FORMAT('PYIM,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         IF(FIELDTYPE.EQ.'XRI') THEN
            WRITE(OUTLYNE2,4006) MAX
4006        FORMAT('RXIM,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         IF(FIELDTYPE.EQ.'YRI') THEN
            WRITE(OUTLYNE2,4007) MAX
4007        FORMAT('RYIM,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         GO TO 8888
      END IF
!     WL
      IF(TEMPC(I)(1:2).EQ.'WL') THEN
         CALL ONEBLANK(2,TEMPC(I)(1:1024))
!     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
!     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
!     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE WV AND WV2 COMMANDS
!     AS NEEDED.
!     STRIP OFF WD
         TEMPC(I)(1:1024)=TEMPC(I)(3:1024)//'  '
!     STRIP LEADING BLANKS
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
            WRITE(OUTLYNE2,2008) WD(1)/1000.0D0,WD(2)/1000.0D0,WD(3)/1000.0D0 &
            &,WD(4)/1000.0D0,WD(5)/1000.0D0
2008        FORMAT('WV,',D23.15,',',D23.15,',',D23.15,',',D23.15,','&
            &,D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         IF(DOWV2) THEN
            WRITE(OUTLYNE2,2009) WD(6)/1000.0D0,WD(7)/1000.0D0,WD(8)/1000.0D0 &
            &,WD(9)/1000.0D0,WD(10)/1000.0D0
2009        FORMAT('WV2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','&
            &,D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         GO TO 8888
      END IF
!     WTW
      IF(TEMPC(I)(1:3).EQ.'WTW') THEN
         CALL ONEBLANK(3,TEMPC(I)(1:1024))
!     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
!     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
!     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE SPTWT/SPTWT2 COMMANDS
!     AS NEEDED.
!     STRIP OFF WTW
         TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
!     STRIP LEADING BLANKS
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
2010        FORMAT('SPTWT,',D23.15,',',D23.15,',',D23.15,',',D23.15,','&
            &,D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         IF(DOWV2) THEN
            WRITE(OUTLYNE2,2011) WD(6),WD(7),WD(8),WD(9),WD(10)
2011        FORMAT('SPTWT2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','&
            &,D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
         END IF
         GO TO 8888
      END IF
!     TEL Y
      IF(TEMPC(I)(1:5).EQ.'TEL Y') THEN
         SAVE_KDP(1)=SAVEINPT(1)
         INPUT='TEL YES'
         IF(INPUT(1:20).NE.AA) CALL PROCES
         REST_KDP(1)=RESTINPT(1)
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     STO
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
!     CYL
!     CYL IS JUST AN INTERNAL SETTING
      IF(TEMPC(I)(1:3).EQ.'CYL') THEN
         TOR='? '
         CYL='C'
         AAS='?'
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     YTO OR AAS
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
!     XTO
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
!     DIM
      IF(TEMPC(I)(1:3).EQ.'DIM') THEN
         call parse(trim(TEMPC(I)), ' ', tokens, numTokens)

         select case (trim(tokens(2)))

          case ('M')
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='UNITS MM'
            IF(INPUT(1:20).NE.AA) CALL PROCES
            REST_KDP(1)=RESTINPT(1)
          case('C')
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='UNITS CM'
            IF(INPUT(1:20).NE.AA) CALL PROCES
            REST_KDP(1)=RESTINPT(1)
          case('I')
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='UNITS IN'
            IF(INPUT(1:20).NE.AA) CALL PROCES
            REST_KDP(1)=RESTINPT(1)
         end select

         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     PROCESS EACH LINE INTO A KDP COMMAND AND THEN DISPLAY IT
      IF(TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.' '.OR.&
      &TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'I'.OR.&
      &TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'O'.OR.&
      &TEMPC(I)(1:1).EQ.'S'.AND.TEMPC(I)(2:2).EQ.'0') THEN
         IF(TEMPC(I)(1:2).NE.'SO'.AND.TEMPC(I)(1:2).NE.'S0') THEN
!
!     WRITE CLAP DATA, COBS DATA, THEN GLASS DATA
!     CLAP DATA
            IF(CLTYPE.EQ.'C') THEN
               WRITE(OUTLYNE2,2012) CL1,CL4,CL3,CL2
2012           FORMAT('CLAP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
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
2013           FORMAT('CLAP RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
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
2014           FORMAT('CLAP ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
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
2015           FORMAT('CLAP TILT,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
               CLTILT=.FALSE.
            END IF
!     COBS DATA
            IF(COTYPE.EQ.'C') THEN
               WRITE(OUTLYNE2,4012) CO1,CO4,CO3
4012           FORMAT('COBS,',D23.15,',',D23.15,',',D23.15,',,,')
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
4013           FORMAT('COBS RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
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
4014           FORMAT('COBS ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
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
4015           FORMAT('COBS TILT,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
               COTILT=.FALSE.
            END IF
!     GLASS DATA
            WRITE(OUTLYNE2,2016) GLASSA
2016        FORMAT(A40)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            SURFER=SURFER+1
            HOESUR(SURFER)=-1
            TOR='? '
            TIL='?'
            CYL='?'
            AAS='?'
         END IF
!     A SURFACE LINE, BREAK IT OUT
!     STRIP OFF TO THE START OF THE CURVATURE
         IF(TEMPC(I)(1:2).EQ.'SO'.OR.&
         &TEMPC(I)(1:2).EQ.'S0'.OR.&
         &TEMPC(I)(1:2).EQ.'S '.OR.&
         &TEMPC(I)(1:2).EQ.'SI')&
         &TEMPC(I)(1:1024)=TEMPC(I)(3:1024)//'     '
!     REMOVE MORE LEADING BLANKS
         DO M=1,1024
            IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
         END DO
!     STRIP OFF THE CURVATURE
         DO J=1,1024
            IF(TEMPC(I)(J:J).EQ.' ') THEN
               CVA=AA(1:(23-J-1))//TEMPC(I)(1:J-1)
               CALL ATODCODEV(CVA,CV,CVERROR)
               IF(CVERROR) THEN
                  WRITE(38,4001) TEMPCC(1:78)
                  GO TO 8888
               END IF
!     IF RADON THEN CODE V REPORTED RADIUS, CONVERT TO
!     CURVATURE ELSE DON'T AND DON'T CONVERT IF RADON AND RAD=0
               IF(RADON.AND.CV.NE.0.0D0) CV=1.0D0/CV
               TEMPC(I)(1:1024)=TEMPC(I)(J+1:1024)//BL1024(1:J)
               GO TO 50
            END IF
         END DO
!     REMOVE SOME MORE LEADING SPACES
50       DO J=1,1024
            IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
         END DO
!     STRIP OFF THE THICKNESS
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
60       CONTINUE
!     NOW THE GLASS TYPE
!     ALL BLANKS, GLASS WAS AIR
         IF(TEMPC(I)(1:1024).EQ.BL1024) THEN
            GLASSA='AIR'
            GO TO 7777
         END IF
!     REMOVE BLANKS
         DO M=1,1024
            IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
         END DO
!     DETERMINE GLASS NAME
         WRITE(OUTLYNE, *) 'TEMPC is ', TEMPC(I)(1:20)
         !call logger%logText(OUTLYNE)
         DO J=1,1024
            IF(TEMPC(I)(J:J).EQ.'_') THEN
               IF(TEMPC(I)(J+1:J+1).EQ.'S'.AND.TEMPC(I)(1:4).EQ.'BAF2') THEN
                  GLASSA='MATL '//TEMPC(I)(1:J-1)
               ELSE
                  GLASSA='GLAK '//TEMPC(I)(1:J-1)
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
!
!     CODE-V MODEL GLASS
!
         IF(TEMPC(I)(1:1).EQ.'0'.OR.TEMPC(I)(1:1).EQ.'1'.OR.&
         &TEMPC(I)(1:1).EQ.'2'.OR.TEMPC(I)(1:1).EQ.'3'.OR.&
         &TEMPC(I)(1:1).EQ.'4'.OR.TEMPC(I)(1:1).EQ.'5'.OR.&
         &TEMPC(I)(1:1).EQ.'6'.OR.TEMPC(I)(1:1).EQ.'7'.OR.&
         &TEMPC(I)(1:1).EQ.'8'.OR.TEMPC(I)(1:1).EQ.'9') THEN
            PRINT *, "Found Model Glass!"
            dotLoc = INDEX(TEMPC(I), '.')
            AINDEX='1.'//TEMPC(I)(1:dotLoc-1)
            AVNUMB=TEMPC(I)(dotLoc+1:dotLoc+2)//'.'//&
            &TEMPC(I)(dotLoc+3:dotLoc+3)
            GLASSA='MODEL G'//TEMPC(I)(1:dotLoc+3)//&
            &','//AINDEX//','//AVNUMB
            PRINT *, "GLASSA is ", GLASSA
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 7777
         END IF
!       FALL THROUGH
         GLASSA='GLASS '//TEMPC(I)(1:20)
7777     CONTINUE
         IF(.NOT.RADON) THEN
            WRITE(OUTLYNE2,2017) CV
         ELSE
            IF(CV.EQ.0.0D0) THEN
               WRITE(OUTLYNE2,2041) CV
            ELSE
               WRITE(OUTLYNE2,2041) 1.0D0/CV
            END IF
         END IF
2017     FORMAT('CV,',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2018) TH
2018     FORMAT('TH,',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      ELSE
!     NOT A SURFACE COMMAND
!     RESOLVE NON-SURFACE COMMANDS
      END IF
!
!     TILTS AND DECENTERS GO HERE
      IF(TEMPC(I)(1:3).EQ.'DAR') THEN
         WRITE(OUTLYNE2,2019)
2019     FORMAT('TILT DAR')
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TIL='T'
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
      IF(TEMPC(I)(1:3).EQ.'REV') THEN
         WRITE(OUTLYNE2,9019)
9019     FORMAT('TILT REV')
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TIL='T'
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
      IF(TEMPC(I)(1:3).EQ.'BEN') THEN
         WRITE(OUTLYNE2,2020)
2020     FORMAT('TILT BEN')
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TIL='T'
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!
!     ADE,BDE,CDE,XDE,YDE,ZDE
      IF(TEMPC(I)(1:3).EQ.'ADE'.OR.&
      &TEMPC(I)(1:3).EQ.'BDE'.OR.&
      &TEMPC(I)(1:3).EQ.'CDE'.OR.&
      &TEMPC(I)(1:3).EQ.'XDE'.OR.&
      &TEMPC(I)(1:3).EQ.'YDE'.OR.&
      &TEMPC(I)(1:3).EQ.'ZDE') THEN
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
            IF(TEMPC(I)(1:3).EQ.'ADE'.OR.&
            &TEMPC(I)(1:3).EQ.'BDE'.OR.&
            &TEMPC(I)(1:3).EQ.'CDE') THEN
               TIL='T'
               WRITE(OUTLYNE2,2022)
2022           FORMAT('TILT')
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
         END IF
         IF(TEMPC(I)(1:3).EQ.'ADE') THEN
            WRITE(OUTLYNE2,2023) VALV
2023        FORMAT('ALPHA,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:3).EQ.'BDE') THEN
            WRITE(OUTLYNE2,2024) VALV
2024        FORMAT('BETA,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:3).EQ.'CDE') THEN
            WRITE(OUTLYNE2,2025) VALV
2025        FORMAT('GAMMA,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:3).EQ.'XDE') THEN
            WRITE(OUTLYNE2,2026) VALV
2026        FORMAT('XD,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:3).EQ.'YDE') THEN
            WRITE(OUTLYNE2,2027) VALV
2027        FORMAT('YD,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:3).EQ.'ZDE') THEN
            WRITE(OUTLYNE2,2028) VALV
2028        FORMAT('ZD,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!
!     RET
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
2029     FORMAT('TILT RET,',I3)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     TILTS AND DECENTERD DONE
!
!     THICKNESS THI WITHOUT A SOLVE
!
!     NOT THI HMY, THI HMX, THI HCY THI HCX
      IF(TEMPC(I)(1:3).EQ.'THI') THEN
         IF(TEMPC(I)(1:7).NE.'THI HMY'.AND.&
         &TEMPC(I)(1:7).NE.'THI HMX'.AND.&
         &TEMPC(I)(1:7).NE.'THI HCY'.AND.&
         &TEMPC(I)(1:7).NE.'THI HCX') THEN
!     JUST THI
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
2030        FORMAT('TH,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     THICKNESS DONE
!
!
!     CLAP DIMENSIONS
!
      IF(TEMPC(I)(1:3).EQ.'CIR'.OR.&
      &TEMPC(I)(1:3).EQ.'REX'.OR.&
      &TEMPC(I)(1:3).EQ.'REY'.OR.&
      &TEMPC(I)(1:3).EQ.'ELX'.OR.&
      &TEMPC(I)(1:3).EQ.'ELY') THEN
         IF(TEMPC(I)(1:7).NE.'CIR OBS'.AND.&
         &TEMPC(I)(1:7).NE.'CIR EDG'.AND.&
         &TEMPC(I)(1:7).NE.'CIR HOL') THEN
!     PROCEED
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
!     X VALUE
                  CL1=VALV
                  CLAP1=.TRUE.
               ELSE
!     Y VALUE
                  CL2=VALV
                  CLAP2=.TRUE.
               END IF
            END IF
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         ELSE
!     HOL OR EDGE OR OBS
            IF(TEMPC(I)(5:7).NE.'OBS') THEN
               WRITE(38,4001) TEMPCC(1:78)
               GO TO 8888
            END IF
         END IF
      END IF
!     CLAP DIMENSIONS DONE
!
!
!     CLAP DECENTERS  AND TILTS
!
      IF(TEMPC(I)(1:3).EQ.'ADX'.OR.&
      &TEMPC(I)(1:3).EQ.'ADY'.OR.&
      &TEMPC(I)(1:3).EQ.'ARO') THEN
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
!     CLAP DECENTERS AND TILTS DONE
!
!     COBS DIMENSIONS
!
      IF(TEMPC(I)(1:7).EQ.'CIR OBS'.OR.&
      &TEMPC(I)(1:7).EQ.'REX OBS'.OR.&
      &TEMPC(I)(1:7).EQ.'REY OBS'.OR.&
      &TEMPC(I)(1:7).EQ.'ELX OBS'.OR.&
      &TEMPC(I)(1:7).EQ.'ELY OBS') THEN
!     PROCEED
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
!     X VALUE
               CO1=VALV
               COBS1=.TRUE.
            ELSE
!     Y VALUE
               CO2=VALV
               COBS2=.TRUE.
            END IF
         END IF
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     COBS DIMENSIONS DONE
!
!
!     COBS DECENTERS  AND TILTS
!
      IF(TEMPC(I)(1:7).EQ.'ADX OBS'.OR.&
      &TEMPC(I)(1:7).EQ.'ADY OBS '.OR.&
      &TEMPC(I)(1:7).EQ.'ARO OBS') THEN
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
!     COBS DECENTERS AND TILTS DONE
!
!     DIFFRACTION GRATING COMMANDS
!
!     GRT, GRO, GRS, GRX, GRY, GRX
      IF(TEMPC(I)(1:3).EQ.'GRT'.OR.&
      &TEMPC(I)(1:3).EQ.'GRS'.OR.&
      &TEMPC(I)(1:3).EQ.'GRO'.OR.&
      &TEMPC(I)(1:3).EQ.'GRX'.OR.&
      &TEMPC(I)(1:3).EQ.'GRY'.OR.&
      &TEMPC(I)(1:3).EQ.'GRZ') THEN
         IF(TEMPC(I)(1:3).EQ.'GRT') THEN
            WRITE(OUTLYNE2,2031)
2031        FORMAT('GRT')
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:3).NE.'GRT') THEN
            IF(TEMPC(I)(1:3).NE.'GRO') THEN
!     BREAK OUT A VALUE
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
!     BREAK OUT A VALUE
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
2032           FORMAT('GRO,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'GRS') THEN
               WRITE(OUTLYNE2,2033) VALV
2033           FORMAT('GRS,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'GRX') THEN
               WRITE(OUTLYNE2,2034) VALV
2034           FORMAT('GRX,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'GRY') THEN
               WRITE(OUTLYNE2,2035) VALV
2035           FORMAT('GRY,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'GRZ') THEN
               WRITE(OUTLYNE2,2036) VALV
2036           FORMAT('GRZ,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     GRATING COMMANDS DONE
!
!     HOE COMMANDS
!
!     HOE, HTO, HOR, HWL, HX1,HY1,HZ1,HX2,HY2,HZ2
      IF(TEMPC(I)(1:3).EQ.'HOE'.OR.&
      &TEMPC(I)(1:3).EQ.'HOR'.OR.&
      &TEMPC(I)(1:3).EQ.'HWL'.OR.&
      &TEMPC(I)(1:3).EQ.'HX1'.OR.&
      &TEMPC(I)(1:3).EQ.'HY1'.OR.&
      &TEMPC(I)(1:3).EQ.'HZ1'.OR.&
      &TEMPC(I)(1:3).EQ.'HX2'.OR.&
      &TEMPC(I)(1:3).EQ.'HY2'.OR.&
      &TEMPC(I)(1:3).EQ.'HZ2'.OR.&
      &TEMPC(I)(1:3).EQ.'HV1'.OR.&
      &TEMPC(I)(1:3).EQ.'HV2') THEN
         IF(TEMPC(I)(1:3).NE.'HOE'.AND.&
         &TEMPC(I)(1:3).NE.'HV1'.AND.&
         &TEMPC(I)(1:3).NE.'HV2') THEN
!     BREAK OUT A VALUE
            TEMPER=TEMPC(I)(4:1024)
            CALL LEFTJUST(TEMPER)
            VALA=TEMPER(1:23)
            CALL RIGHTJUST(VALA)
            CALL ATODCODEV(VALA,VALV,CVERROR)
            IF(CVERROR) THEN
               WRITE(38,4001) TEMPCC(1:78)
               GO TO 8888
            END IF
            IF(TEMPC(I)(1:3).EQ.'HOE')&
            &HOE(SURFER)=.TRUE.
            IF(TEMPC(I)(1:7).EQ.'HV1 REA')&
            &HV1(SURFER)='R'
            IF(TEMPC(I)(1:7).EQ.'HV1 VIR')&
            &HV1(SURFER)='V'
            IF(TEMPC(I)(1:7).EQ.'HV2 REA')&
            &HV2(SURFER)='R'
            IF(TEMPC(I)(1:7).EQ.'HV2 VIR')&
            &HV2(SURFER)='V'
            IF(TEMPC(I)(1:3).EQ.'HX1')&
            &HX1(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HY1')&
            &HY1(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HZ1')&
            &HZ1(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HX2')&
            &HX2(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HY2')&
            &HY2(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HZ2')&
            &HZ2(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HWL')&
            &HWL(SURFER)=VALV
            IF(TEMPC(I)(1:3).EQ.'HOR')&
            &HOR(SURFER)=VALV
            HOESUR(SURFER)=SURFER
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     GRATING COMMANDS DONE
!
!     THICKNESS SOLVES GO HERE
!
!     THI HMY, THI HMX, THI HCY THI HCX
      IF(TEMPC(I)(1:7).EQ.'THI HMY'.OR.&
      &TEMPC(I)(1:7).EQ.'THI HMX'.OR.&
      &TEMPC(I)(1:7).EQ.'THI HCY'.OR.&
      &TEMPC(I)(1:7).EQ.'THI HCX') THEN
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
2037        FORMAT('PX,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'THI HMY') THEN
            WRITE(OUTLYNE2,2038) VALV
2038        FORMAT('PY,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'THI HCX') THEN
            WRITE(OUTLYNE2,2039) VALV
2039        FORMAT('PCX,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'THI HCY') THEN
            WRITE(OUTLYNE2,2040) VALV
2040        FORMAT('PCY,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     THICKNESS SOLVES DONE
!
!     CURVATURE/RADII GO HERE
!
      IF(TEMPC(I)(1:3).EQ.'CUX'.OR.&
      &TEMPC(I)(1:3).EQ.'CUY'.OR.&
      &TEMPC(I)(1:3).EQ.'RDX'.OR.&
      &TEMPC(I)(1:3).EQ.'RDY') THEN
         IF(TEMPC(I)(1:7).NE.'CUX UMX'.AND.&
         &TEMPC(I)(1:7).NE.'CUY UMY'.AND.&
         &TEMPC(I)(1:7).NE.'CUX UCX'.AND.&
         &TEMPC(I)(1:7).NE.'CUY UCY'.AND.&
         &TEMPC(I)(1:7).NE.'CUX IMX'.AND.&
         &TEMPC(I)(1:7).NE.'CUY IMY'.AND.&
         &TEMPC(I)(1:7).NE.'CUX ICX'.AND.&
         &TEMPC(I)(1:7).NE.'CUY ICY') THEN
!     PROCEED, CURVATURE OR RADII
            TEMPER=TEMPC(I)(4:1024)
            CALL LEFTJUST(TEMPER)
            VALA=TEMPER(1:23)
            CALL RIGHTJUST(VALA)
            CALL ATODCODEV(VALA,VALV,CVERROR)
            IF(CVERROR) THEN
               WRITE(38,4001) TEMPCC(1:78)
               GO TO 8888
            END IF
!     CONVERT TO RADII IF NECESSARY
            IF(TEMPC(I)(1:1).EQ.'R'.AND.VALV.NE.0.0D0) VALV=1.0D0/VALV
!     FIRST IF ROTATIONALLY SYMMETRIC
            IF(TOR.EQ.'? '.AND.AAS.EQ.'?'.AND.CYL.EQ.'?') THEN
               IF(TEMPC(I)(3:3).EQ.'Y') THEN
!     YZ PLANE
                  IF(TEMPC(I)(1:1).EQ.'R') THEN
                     WRITE(OUTLYNE2,2041) VALV
2041                 FORMAT('RD,',D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
                  IF(TEMPC(I)(1:1).EQ.'C') THEN
                     WRITE(OUTLYNE2,2042) VALV
2042                 FORMAT('CV,',D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
               ELSE
!     NOTHING TO DO, RDX AND CUX NOT RECOGNIZED
               END IF
            END IF
!
!     SET TOR IF CYL NOT ?
            IF(CYL.EQ.'C') THEN
               IF(TEMPC(I)(3:3).EQ.'Y') TOR='YT'
               IF(TEMPC(I)(3:3).EQ.'Y') THEN
                  WRITE(OUTLYNE2,2043)
2043              FORMAT('YTORIC')
                  CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
               END IF
               IF(TEMPC(I)(3:3).EQ.'X') TOR='XT'
               IF(TEMPC(I)(3:3).EQ.'X') THEN
                  WRITE(OUTLYNE2,2044)
2044              FORMAT('XTORIC')
                  CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
               END IF
            END IF
!
            IF(TOR.EQ.'YT') THEN
!     Y TORIC
               IF(TEMPC(I)(3:3).EQ.'Y') THEN
!     YZ PLANE
                  IF(TEMPC(I)(1:1).EQ.'R') THEN
                     WRITE(OUTLYNE2,2045) VALV
2045                 FORMAT('RDTOR,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
                  IF(TEMPC(I)(1:1).EQ.'C') THEN
                     WRITE(OUTLYNE2,2046) VALV
2046                 FORMAT('CVTOR,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
               ELSE
!     XZ PLANE
                  IF(TEMPC(I)(1:1).EQ.'R') THEN
                     WRITE(OUTLYNE2,2047) VALV
2047                 FORMAT('RD,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
                  IF(TEMPC(I)(1:1).EQ.'C') THEN
                     WRITE(OUTLYNE2,2048) VALV
2048                 FORMAT('CV,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
               END IF
            END IF
            IF(TOR.EQ.'XT') THEN
!     X TORIC
               IF(TEMPC(I)(3:3).EQ.'Y') THEN
!     YZ PLANE
                  IF(TEMPC(I)(1:1).EQ.'R') THEN
                     WRITE(OUTLYNE2,2049) VALV
2049                 FORMAT('RD,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
                  IF(TEMPC(I)(1:1).EQ.'C') THEN
                     WRITE(OUTLYNE2,2050) VALV
2050                 FORMAT('CV,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
               ELSE
!     XZ PLANE
                  IF(TEMPC(I)(1:1).EQ.'R') THEN
                     WRITE(OUTLYNE2,2051) VALV
2051                 FORMAT('RDTOR,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
                  IF(TEMPC(I)(1:1).EQ.'C') THEN
                     WRITE(OUTLYNE2,2052) VALV
2052                 FORMAT('CVTOR,'D23.15)
                     CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
                  END IF
               END IF
            END IF
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     CURVATURE/RADIUS DONE
!
!     CONICS AND ASPHERICS GO HERE IF TOR='? '
      IF(TOR.EQ.'? ') THEN
!     ROTATIONALLY SYMMETRIC SURFACES
         IF(TEMPC(I)(1:2).EQ.'K '.OR.&
         &TEMPC(I)(1:2).EQ.'A '.OR.&
         &TEMPC(I)(1:2).EQ.'B '.OR.&
         &TEMPC(I)(1:2).EQ.'C '.OR.&
         &TEMPC(I)(1:2).EQ.'D '.OR.&
         &TEMPC(I)(1:2).EQ.'E '.OR.&
         &TEMPC(I)(1:2).EQ.'F '.OR.&
         &TEMPC(I)(1:2).EQ.'G '.OR.&
         &TEMPC(I)(1:2).EQ.'H '.OR.&
         &TEMPC(I)(1:2).EQ.'J ') THEN
!     PROCEED
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
2053           FORMAT('CC,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'A') THEN
               WRITE(OUTLYNE2,2054) VALV
2054           FORMAT('AD,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'B') THEN
               WRITE(OUTLYNE2,2154) VALV
2154           FORMAT('AE,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'C') THEN
               WRITE(OUTLYNE2,2055) VALV
2055           FORMAT('AF,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'D') THEN
               WRITE(OUTLYNE2,2056) VALV
2056           FORMAT('AG,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'E') THEN
               WRITE(OUTLYNE2,2057) VALV
2057           FORMAT('AH,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'F') THEN
               WRITE(OUTLYNE2,2058) VALV
2058           FORMAT('AI,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'G') THEN
               WRITE(OUTLYNE2,2059) VALV
2059           FORMAT('AJ,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'H') THEN
               WRITE(OUTLYNE2,2060) VALV
2060           FORMAT('AK,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:1).EQ.'J') THEN
               WRITE(OUTLYNE2,2061) VALV
2061           FORMAT('AL,',D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     ROTATIONALLY SYMMETRIC SURFACES ASPHERICS DONE
!
!     CONICS AND ASPHERICS GO HERE IF TOR NOT '?'
      IF(TOR.NE.'? '.AND.AAS.EQ.'?') THEN
!     TORICS
         IF(TEMPC(I)(1:2).EQ.'K '.OR.&
         &TEMPC(I)(1:2).EQ.'A '.OR.&
         &TEMPC(I)(1:2).EQ.'B '.OR.&
         &TEMPC(I)(1:2).EQ.'C '.OR.&
         &TEMPC(I)(1:2).EQ.'D ') THEN
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
2062           FORMAT('CCTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(2:2).EQ.'A ') THEN
               WRITE(OUTLYNE2,2063) VALV
2063           FORMAT('ADTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(2:2).EQ.'B ') THEN
               WRITE(OUTLYNE2,2064) VALV
2064           FORMAT('AETOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(2:2).EQ.'C ') THEN
               WRITE(OUTLYNE2,2065) VALV
2065           FORMAT('AFTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(2:2).EQ.'D ') THEN
               WRITE(OUTLYNE2,2066) VALV
2066           FORMAT('AGTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            GO TO 213
213         CONTINUE
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     NON AAS TORICS ASPHERICS DONE
!
!     ANAMORPHIC ASPHERICS
      IF(AAS.EQ.'Y') THEN
!     ANAMORPHIC ASPHERICS
         IF(TEMPC(I)(1:3).EQ.'KY '.OR.&
         &TEMPC(I)(1:3).EQ.'KX '.OR.&
         &TEMPC(I)(1:3).EQ.'AR '.OR.&
         &TEMPC(I)(1:3).EQ.'AP '.OR.&
         &TEMPC(I)(1:3).EQ.'BR '.OR.&
         &TEMPC(I)(1:3).EQ.'BP '.OR.&
         &TEMPC(I)(1:3).EQ.'CR '.OR.&
         &TEMPC(I)(1:3).EQ.'CP '.OR.&
         &TEMPC(I)(1:3).EQ.'DR '.OR.&
         &TEMPC(I)(1:3).EQ.'DP ') THEN
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
2067           FORMAT('CC,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:2).EQ.'KX') THEN
               WRITE(OUTLYNE2,2068) VALV
2068           FORMAT('CCTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'AR ') THEN
               WRITE(OUTLYNE2,2069) VALV
2069           FORMAT('AD,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'AP ') THEN
               WRITE(OUTLYNE2,2070) VALV
2070           FORMAT('ADTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'BR ') THEN
               WRITE(OUTLYNE2,2071) VALV
2071           FORMAT('AE,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'BP ') THEN
               WRITE(OUTLYNE2,2072) VALV
2072           FORMAT('AETOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'CR ') THEN
               WRITE(OUTLYNE2,2073) VALV
2073           FORMAT('AF,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'CP ') THEN
               WRITE(OUTLYNE2,2074) VALV
2074           FORMAT('AFTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'DR ') THEN
               WRITE(OUTLYNE2,2075) VALV
2075           FORMAT('AG,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(TEMPC(I)(1:3).EQ.'DP ') THEN
               WRITE(OUTLYNE2,2076) VALV
2076           FORMAT('AGTOR,'D23.15)
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            GO TO 216
216         CONTINUE
            TEMPC(I)(1:1024)=BL1024(1:1024)
            GO TO 8888
         END IF
      END IF
!     AAS TORICS ASPHERICS DONE
!
!     CURVATURE SOLVES GO HERE
!
!     CUY AND CUX WITH UMY/UMX,UCY/UCX,IMY/IMX AND ICY/ICX
      IF(TEMPC(I)(1:7).EQ.'CUX UMX'.OR.&
      &TEMPC(I)(1:7).EQ.'CUY UMY'.OR.&
      &TEMPC(I)(1:7).EQ.'CUX UCX'.OR.&
      &TEMPC(I)(1:7).EQ.'CUY UCY'.OR.&
      &TEMPC(I)(1:7).EQ.'CUX IMX'.OR.&
      &TEMPC(I)(1:7).EQ.'CUY IMY'.OR.&
      &TEMPC(I)(1:7).EQ.'CUX ICX'.OR.&
      &TEMPC(I)(1:7).EQ.'CUY ICY') THEN
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
2077           FORMAT('XTORIC')
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(CYL.EQ.'C') TOR='XT'
         END IF
         IF(TEMPC(I)(3:3).EQ.'Y') THEN
            IF(CYL.EQ.'C') THEN
               WRITE(OUTLYNE2,2078)
2078           FORMAT('YTORIC')
               CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
            END IF
            IF(CYL.EQ.'C') TOR='YT'
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUX UMX') THEN
            WRITE(OUTLYNE2,2079) VALV
2079        FORMAT('PUX,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUY UMY') THEN
            WRITE(OUTLYNE2,2080) VALV
2080        FORMAT('PUY,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUX UCX') THEN
            WRITE(OUTLYNE2,2081) VALV
2081        FORMAT('PUCX,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUY UCY') THEN
            WRITE(OUTLYNE2,2082) VALV
2082        FORMAT('PUCY,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUX IMX') THEN
            WRITE(OUTLYNE2,2083) VALV
2083        FORMAT('PIX,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUY IMY') THEN
            WRITE(OUTLYNE2,2084) VALV
2084        FORMAT('PIY,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUX ICX') THEN
            WRITE(OUTLYNE2,2085) VALV
2085        FORMAT('PICX,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(TEMPC(I)(1:7).EQ.'CUY ICY') THEN
            WRITE(OUTLYNE2,2086) VALV
2086        FORMAT('PICY,',D23.15)
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         TEMPC(I)(1:1024)=BL1024(1:1024)
         GO TO 8888
      END IF
!     CURVATURE SOLVES DONE
!
      IF(TEMPC(I)(1:2).EQ.'GO') THEN
         TEMPC(I)(1:1024)=BL1024(1:1024)
      END IF
!     IF WE ARE HERE, THE COMMAND DID NOT TRANSLATE
      IF(TEMPC(I)(1:75).NE.BL1024(1:78)) THEN
         WRITE(38,4001) TEMPCC(1:78)
      END IF
4001  FORMAT(' ',A78)
!     DO NEXT LINE IN THE CODE V SEQUENCE FILE
8888  CONTINUE
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
!     NOW HOE STUFF IF NEEDED
   DO I=0,MAXSUR
      IF(HOE(I)) GO TO 220
   END DO
   CALL CLOSE_FILE(37,1)
   CALL CLOSE_FILE(38,1)
   RETURN
220 CONTINUE
   WRITE(OUTLYNE2,2089)
2089 FORMAT('SPSRF')
   CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
   DO I=0,MAXSUR
      IF(HOE(I)) THEN
         WRITE(OUTLYNE2,2090) I
2090     FORMAT('SPECIAL,',I3,', 12')
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2091) I,HOR(I)
2091     FORMAT('C1,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2092) I,HWL(I)
2092     FORMAT('C2,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2093) I,HX1(I)
2093     FORMAT('C3,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2094) I,HY1(I)
2094     FORMAT('C4,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2095) I,HZ1(I)
2095     FORMAT('C5,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         IF(HV1(I).EQ.'R') THEN
            WRITE(OUTLYNE2,2097) I
2097        FORMAT('C5,',I3,', +1.0D0')
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(HV1(I).EQ.'V') THEN
            WRITE(OUTLYNE2,2098) I
2098        FORMAT('C5,',I3,', -1.0D0')
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         WRITE(OUTLYNE2,2099) I,HX2(I)
2099     FORMAT('C7,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2100) I,HY2(I)
2100     FORMAT('C8,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         WRITE(OUTLYNE2,2101) I,HZ2(I)
2101     FORMAT('C9,',I3,',',D23.15)
         CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         IF(HV2(I).EQ.'R') THEN
            WRITE(OUTLYNE2,2102) I
2102        FORMAT('C10,',I3,', +1.0D0')
            CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
         END IF
         IF(HV2(I).EQ.'V') THEN
            WRITE(OUTLYNE2,2103) I
2103        FORMAT('C10,',I3,', -1.0D0')
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
   DEALLOCATE (HOE,HOESUR,HV1,HV2,HX1,HY1,HZ1 &
   &,HWL,HX2,HY2,HZ2,HOR,STAT=ALLOERR)
   call ldm%load_surfaces_from_alens()
   ! Converting a CODE V file replaces the lens: reset undo history with it as baseline.
   call undo_reset_baseline()
   RETURN
   !100  FORMAT(A132)
   !100  FORMAT(readformat)

END

SUBROUTINE UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)

   USE GLOBALS
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE

   CHARACTER(LEN=1024) :: OUTLYNE2
   INTEGER :: SUB


   SAVE_KDP(1)=SAVEINPT(1)
   INPUT(1:SUB)=OUTLYNE2(1:SUB)
   IF(INPUT(1:20).NE.AA) CALL PROCES
   REST_KDP(1)=RESTINPT(1)

END SUBROUTINE
