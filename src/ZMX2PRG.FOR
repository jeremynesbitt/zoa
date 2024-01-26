
! Unsupported commands notes
! GFAC - related to apodization setting 
! GFAC 0 0 - Uniform 
! GFAC 0 1 - Gaussian
! GFAC 0 2 - Cosine cubed
! ROPD - reference for OPD
! ROPD 0 - Absolute
! ROPD 1 - Infinity
! ROPD 2 - Exit Pupil (recommended)
! ROPD 3 - Absolute 2
! PICB - Paraxial Rays
! PICB 0 - Consider Coordinate Breaks
! PICB 1 - Ignore Coordinate Breaks (recommended)
! GLRS - global coordinate reference surface
! MNUM - number of configurations and active one
! eg MNUM 2 1 - two configs 1 active


C SUB ZMX2PRG.FOR
        SUBROUTINE ZMX2PRG
        USE GLOBALS        
        use zoa_file_handler
        use kdp_data_types, only: RAYAIM_PARAX, RAYAIM_REAL
        use global_widgets, only : sysConfig
        use handlers, only: zoatabMgr        
C
        IMPLICIT NONE
C
      CHARACTER ZMXFILENAME*80,KDPFILENAME*80,ZMX_INPUT_STRING*1024
     1,SURFTYPE*8
C
      CHARACTER TEMPA*1024,TEMPB*1024,CVA*23,THA*23
     1,GLASSA*27,VALA*23,AA23*23,SUMSTRING*1024
     2,TEMPC*1024,BL1024*1024,TIL*1,TEMPER*1024,PREV*80
     3,FIELDTYPE*3,CLTYPE*1,COTYPE*1,TEMPCC*1024,TEMPCCC*1024

      CHARACTER OUTLYNE2*1024
      character(len=6) :: readformat
      character(len=1024) :: filePath
C
      LOGICAL EXIS37,EXIS38,OPEN37,OPEN38,SEMI,ADD,OLDADD
C
      LOGICAL ZMXERROR
C
      LOGICAL DOWV,DOWV2,CLAP1,CLAP2,CLDX,CLDY
C
      LOGICAL SURFIT,COBS1,COBS2,COBX,COBY
C
      INTEGER I,ZMXFILENAMELENGTH,KDPFILENAMELENGTH,N,NPERIOD,ALLOERR
C
      INTEGER J,K,L,NADD,II,IC,OLDNADD,STRINGEND,WDEXIS(1:100),M
C
      INTEGER NJ,J1,J2,IVALV,IWD(1:100),SURFER,FLDTYP,IPAR, SUB

      INTEGER :: RAIM(5)
      
      CHARACTER(len=1) :: IS(5)
C
      REAL*8 CV,TH,VALV,WD(1:100),MAX,CL1,CL2,CL3,CL4,CL5

      REAL*8 :: rayError
C
      REAL*8 CO1,CO2,CO3,CO4,CO5,VPAR
      character(len=3) :: substr
C
      DIMENSION TEMPA(:),TEMPB(:),TEMPC(:)
C
      ALLOCATABLE :: TEMPA,TEMPB,TEMPC
C
      INCLUDE 'DATMAI.INC'
      INCLUDE 'DATLEN.INC'

      SUB = 256
      write(substr, "(I3)") SUB
      readformat = '(A' // substr // ')'
      PRINT *, "READFORMAT IS ", readformat      
      SURFER=0
      GLASSA='AIR                        '
      SURFIT=.FALSE.
C
      TIL='?'
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
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
      CO1=0.0D0
      CO2=0.0D0
      CO3=0.0D0
      CO4=0.0D0
      CO5=0.0D0
C
C       THIS SUBROUTINE IS CALLED CONVERT A FILE FROM ZMX TO PROGRAM
C     LENS INPUT FORMAT
C
      AA23='                       '
      BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//'    '
C
        IF(STI.EQ.1) THEN
      OUTLYNE='"ZMX2PRG" CONVERTS THE NAMED FILE (STRING INPUT)'
      CALL SHOWIT(1)
      OUTLYNE='FROM ZEMAX TO PROGRAM FORMAT'
      CALL SHOWIT(1)
                                RETURN
                                END IF
        IF(SN.EQ.1.OR.SQ.EQ.1) THEN
      OUTLYNE='"ZMX2PRG" TAKES NO NUMERIC OR QUALIFIER INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                                RETURN
                                END IF
        IF(SST.EQ.0) THEN
      OUTLYNE='"ZMX2PRG" REQUIRES EXPLICIT STRING (FILE NAME) INPUT'
      CALL SHOWIT(1)
      OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                                RETURN
                                END IF
      ZMXFILENAME=WS
                                DO I=80,1,-1
                        IF(ZMXFILENAME(I:I).NE.' ') THEN
                                N=I
                                GO TO 10
                                END IF
                                END DO
      OUTLYNE='INVALID (ZERO LENGTH) ZEMAX FILE NAME'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
 10                             CONTINUE
      ZMXFILENAMELENGTH=N
      EXIS37=.FALSE.
      filePath = trim(getCodeVDir())//getFileSep()//
     1 ZMXFILENAME(1:N)  

      INQUIRE(FILE=trim(filePath),EXIST=EXIS37)     
    
               IF(.NOT.EXIS37) THEN
      OUTLYNE='NO ZEMAX INPUT FILE EXISTS TO READ'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                   END IF
C     FILES EXISTS, CREATE PROGRAM FILE NAME
                        NPERIOD=9
                                DO I=1,80
                        IF(ZMXFILENAME(I:I).EQ.'.') THEN
                        NPERIOD=I
                        GO TO 20
                        END IF
                        END DO
 20                     CONTINUE
      IF(NPERIOD.GT.9) NPERIOD=9
                KDPFILENAME=ZMXFILENAME(1:NPERIOD)//'DAT'
                KDPFILENAMELENGTH=NPERIOD+4
C     OPEN THE ZMXFILENAME TO READ IT AS ASCII
C
      DEALLOCATE (TEMPA,TEMPB,TEMPC
     1,STAT=ALLOERR)
      ALLOCATE (TEMPA(1:5000),TEMPB(1:5000),TEMPC(1:5000)
     1,STAT=ALLOERR)
      TEMPA(1:5000)=BL1024
      TEMPB(1:5000)=BL1024
      TEMPC(1:5000)=BL1024
C
C     CLOSE IT IF OPEN AND KEEP IT
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,0)
      OPEN(UNIT=37,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE=trim(basePath)//
     2ZMXFILENAME(1:ZMXFILENAMELENGTH)
     3,STATUS='UNKNOWN')
      OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE=trim(basePath)//'CONVERT.ERR'
     2,STATUS='UNKNOWN')
      CALL CLOSE_FILE(38,0)
      OPEN(UNIT=38,ACCESS='SEQUENTIAL',BLANK='NULL'
     1,FORM='FORMATTED',FILE=trim(basePath)//'CONVERT.ERR'
     2,STATUS='UNKNOWN')
                        II=1
                        ADD=.FALSE.
                        OLDADD=.FALSE.
                        DO I=1,5000
C     READ A LINE OF THE FILE AS A 132 CHARACTER, CHARACTER
      ZMX_INPUT_STRING=BL1024
      READ(37,100,ERR=75,END=76) ZMX_INPUT_STRING(1:132)
      GO TO 77
 76   ZMX_INPUT_STRING(1:132)='GO'
      GO TO 78
 77            CONTINUE
C     IS THERE A CONTINUATION MARK AT THE END ?
C     REMEMBER IF ADD WAS ON LAST TIME
                        OLDADD=ADD
                        DO J=1024,1,-1
                        ADD=.FALSE.
      IF(ZMX_INPUT_STRING(J:J).EQ.'&') THEN
C     YES
                        ADD=.TRUE.
                        NADD=J
                        GO TO 11
                        ELSE
                        END IF
                        END DO
 11                     CONTINUE
      IF(.NOT.ADD.AND..NOT.OLDADD) THEN
C     NOTHING TO ADD, WRITE A TEMPA VALUE
      TEMPA(II)(1:132)=ZMX_INPUT_STRING(1:132)
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
      SUMSTRING(1:1024)=ZMX_INPUT_STRING(1:1024)
                        DO J=1024,1,-1
      IF(SUMSTRING(J:J).NE.' ') THEN
                        STRINGEND=J
                        GO TO 15
                        END IF
                        END DO
 15                     CONTINUE
                        END IF
      IF(.NOT.ADD.AND.OLDADD) THEN
C     ADD, THEN WRITE A TEMPA VALUE
      IF(STRINGEND.LT.1024) THEN
      SUMSTRING(1:1024)=SUMSTRING(1:STRINGEND)//
     1ZMX_INPUT_STRING(1:(1024-STRINGEND))
                        END IF
C     GET THE NEW STRINGEND
                        DO J=1024,1,-1
      IF(SUMSTRING(J:J).NE.' ') THEN
                        STRINGEND=J
                        GO TO 25
                        END IF
                        END DO
 25                     CONTINUE
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
     1ZMX_INPUT_STRING(1:(1024-STRINGEND))
                        END IF
C     GET THE NEW STRINGEND
                        DO J=1024,1,-1
      IF(SUMSTRING(J:J).NE.' ') THEN
                        STRINGEND=J
                        GO TO 30
                        END IF
                        END DO
 30                     CONTINUE
                        END IF
                                END DO
                                GO TO 78
 75   CONTINUE
      OUTLYNE='ERROR READING ZEMAX INPUT FILE'
      CALL SHOWIT(1)
      OUTLYNE='NO FILE CONVERSION PERFORMED'
      CALL SHOWIT(1)
                        CALL MACFAL
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,0)
      DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
                                RETURN
 78   CONTINUE
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
 4000 FORMAT(' THE FOLLOWING ZEMAX DID NOT TRANSLATE:')
      WRITE(38,4000)
C******************************************************************************
C
                        PREV=AA//AA//AA//AA
                        DO I=1,L
      PRINT *, "Processing Zemax command ", TEMPC(I)(1:124)                          
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
      TEMPCCC(1:1024)=TEMPC(I)(1:1024)
      IF(SURFTYPE.EQ.'EVENASPH') THEN
 2054 FORMAT('AD,',D23.15)
 2154 FORMAT('AE,',D23.15)
 2055 FORMAT('AF,',D23.15)
 2056 FORMAT('AG,',D23.15)
 2057 FORMAT('AH,',D23.15)
 2058 FORMAT('AI,',D23.15)
 2059 FORMAT('AJ,',D23.15)
 2060 FORMAT('AK,',D23.15)
      IF(TEMPCC(1:4).EQ.'PARM') THEN
      ZMXERROR=.FALSE.
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
      CALL PARAMOUT(IPAR,VPAR,TEMPCC,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
      IF(IPAR.EQ.1) THEN
      WRITE(OUTLYNE,2054) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.2) THEN
      WRITE(OUTLYNE,2154) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.3) THEN
      WRITE(OUTLYNE,2055) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.4) THEN
      WRITE(OUTLYNE,2056) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.5) THEN
      WRITE(OUTLYNE,2057) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.6) THEN
      WRITE(OUTLYNE,2058) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.7) THEN
      WRITE(OUTLYNE,2059) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.8) THEN
      WRITE(OUTLYNE,2060) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                   ELSE
C     NOT PARM
                   END IF
                               END IF
      IF(SURFTYPE.EQ.'DGRATING') THEN
 2091 FORMAT('GRT,',D23.15)
 2092 FORMAT('GRO,',D23.15)
      IF(TEMPCC(1:4).EQ.'PARM') THEN
      ZMXERROR=.FALSE.
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
      CALL PARAMOUT(IPAR,VPAR,TEMPCC,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
      IF(IPAR.EQ.1) THEN
      IF(SYSTEM(6).EQ.1.0D0) VPAR=(1.0D-3/VPAR)/25.4D0
      IF(SYSTEM(6).EQ.2.0D0) VPAR=(1.0D-4/VPAR)
      IF(SYSTEM(6).EQ.3.0D0) VPAR=(1.0D-3/VPAR)
      IF(SYSTEM(6).EQ.4.0D0) VPAR=1.0D-6/VPAR
      WRITE(OUTLYNE,2091) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.2) THEN
      WRITE(OUTLYNE,2092) VPAR
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                   ELSE
C     NOT PARM
                   END IF
                               END IF
C     TYPE STANDARD OR TYPE EVENASPH
      IF(TEMPC(I)(1:13).EQ.'TYPE STANDARD'.OR.
     1TEMPC(I)(1:13).EQ.'TYPE EVENASPH') THEN
      IF(TEMPC(I)(1:13).EQ.'TYPE STANDARD') SURFTYPE='STANDARD'
      IF(TEMPC(I)(1:13).EQ.'TYPE EVENASPH') SURFTYPE='EVENASPH'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     TYPE DIFFRACTION GRATING
      IF(TEMPC(I)(1:13).EQ.'TYPE DGRATING') THEN
      IF(TEMPC(I)(1:13).EQ.'TYPE DGRATING') SURFTYPE='DGRATING'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     VERS
      IF(TEMPC(I)(1:4).EQ.'VERS') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='LENS'
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
C     SET DEFAULT UNITS TO MM SINCE THAT IS HOW ZEMAX WAKES UP
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=BL1024(1:132)
                        INPUT(1:132)='UNITS MM'
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     NAME
      IF(TEMPC(I)(1:4).EQ.'NAME') THEN
      ! Need to enter lens input level.  Put it here for now  
        CALL PROCESKDP('LENS')         
                        SAVE_KDP(1)=SAVEINPT(1)
      INPUT='LI '//TEMPC(I)(5:80)
      CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
      IF(INDEX(TEMPC(I),'NOTE 1').GT.0) THEN
        INPUT = 'LIC '//TEMPC(I)(8:80)
        CALL PROCES
        GO TO 8888
      END IF                          
C     COMM
      IF(TEMPC(I)(1:4).EQ.'COMM') THEN
      INPUT='LBL'//TEMPC(I)(5:80)
      CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     ENPD
      IF(TEMPC(I)(1:4).EQ.'ENPD') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
      CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2001) VALV/2.0D0
 2001 FORMAT('SAY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
      IF(INDEX(TEMPC(I),'RAIM').GT.0) THEN
        ! Parse Ray aiming.  Looks like the format is
        ! REAL INT INT INT INT INT
        ! For Zoa, only the first INT is of interest (I think)
        ! 0 is no ray aiming, 1 is paraxial, 2 is real
      PRINT *, "String to Parse is ", TEMPC(I)(6:80)  
 3023 FORMAT(E6.1,A1,I1,A1,I1,A1,I1,A1,I1,A1,I1)
        READ(TEMPC(I)(6:80), 3023) rayError, IS(1), RAIM(1), IS(2), 
     1 RAIM(2), IS(3), RAIM(3), IS(4), RAIM(4), IS(5), RAIM(5)   
       PRINT *, "RayError is ", rayError
       PRINT *, "IS is ", IS
       PRINT *, "RAIM(1) is ", RAIM(1)
       PRINT *, "RAIM is ", RAIM

       select case (RAIM(1))

       case (0) ! Ray Aim off

       case (1) ! Paraxial
        PRINT *, "Paraxial Ray Aiming Set"
        call sysConfig%updateRayAimSelectionByCode(RAYAIM_PARAX)

       case (2) ! Real
        PRINT *, "Real Ray Aiming Set"
        call sysConfig%updateRayAimSelectionByCode(RAYAIM_REAL)

       end select

       GO TO 8888
      END IF
C     FLOA
      IF(TEMPC(I)(1:4).EQ.'FLOA') THEN
      WRITE(OUTLYNE,2085)
 2085 FORMAT('SAY FLOAT')
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     OBNA
      IF(TEMPC(I)(1:4).EQ.'OBNA') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODCODEV(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2002) VALV
 2002 FORMAT('NAO,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     PWAV
      IF(TEMPC(I)(1:4).EQ.'PWAV') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATOIZMX(VALA,IVALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2003) IVALV
 2003 FORMAT('CW,',I2)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     FTYP 
      IF(TEMPC(I)(1:4).EQ.'FTYP') THEN
        TEMPER=TEMPC(I)(5:1024)
        CALL LEFTJUST(TEMPER)
        VALA=TEMPER(1:23)
        CALL RIGHTJUST(VALA)
        ZMXERROR=.FALSE.
        CALL ATOIZMX(VALA,IVALV,ZMXERROR)
        FLDTYP=IVALV
C     0 = FIELD ANGLE
C     1 = OBJECT HT.
C     2 = PARAX IMAGE HT.
C     3 = REAL IMAGE HT.
        PRINT *, "Field Type is ", FLDTYP
        TEMPC(I)(1:1024)=BL1024(1:1024)
        GO TO 8888
      END IF
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                          
      ! Process  XFLN or YFLN  
          
      IF(INDEX(TEMPC(I),'XFLN').GT.0
     1.OR.INDEX(TEMPC(I),'YFLN').GT.0) THEN
      PRINT *, "Zemax convert field points started"
C     MAX FIELD POS CONVERT TO SCY. SCY FANG, SCX AND SCX FANG
C     BREAK OUT UP TO 25 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN FILD LARGEST VALUE
C     AS NEEDED.
      FIELDTYPE = '   '   
      select case (FLDTYP)
         case (0) ! Field Angle
            IF(INDEX(TEMPC(I),'XFLN').GT.0) FIELDTYPE='XAN'
            IF(INDEX(TEMPC(I),'YFLN').GT.0) FIELDTYPE='YAN'
         case (1) ! Object Height
            IF(INDEX(TEMPC(I),'XFLN').GT.0) FIELDTYPE='XOB'
            IF(INDEX(TEMPC(I),'YFLN').GT.0) FIELDTYPE='YOB'
         case (2) ! Parax Image Height
            IF(INDEX(TEMPC(I),'XFLN').GT.0) FIELDTYPE='XIM'
            IF(INDEX(TEMPC(I),'YFLN').GT.0) FIELDTYPE='YIM' 
         case (3) ! Real Image Height
            IF(INDEX(TEMPC(I),'XFLN').GT.0) FIELDTYPE='XIR'
            IF(INDEX(TEMPC(I),'YFLN').GT.0) FIELDTYPE='YIR'                                        
         
      end select



C     STRIP OFF 3 CHARACTERS
      !call OUTKDP("Entering YOB Loop")
      !call OUTKDP("Fieldtype is "//FIELDTYPE)
      TEMPC(I)(1:1024)=TEMPC(I)(4:1024)//'   '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),25,ZMXERROR)
      IF(ZMXERROR) THEN
        PRINT *, "ZMXError triggered?"
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF

      MAX=-1000.0D0
                        DO J=1,25
      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                        END DO
      PRINT *, "FIELDTYPE IS ", FIELDTYPE                  
      IF(FIELDTYPE.EQ.'YAN') THEN
        PRINT *, "Set Y Angle"
      WRITE(OUTLYNE2,2004) MAX

        SAVE_KDP(1)=SAVEINPT(1)
        INPUT(1:SUB)=OUTLYNE2(1:SUB)
        IF(INPUT(1:20).NE.AA) CALL PROCES
        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XAN') THEN
        PRINT *, "Set X Angle"
      WRITE(OUTLYNE2,2005) MAX

      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YOB') THEN
      !CALL OUTKDP('Really in FIELDTYPE YOB LOOP')
      WRITE(OUTLYNE2,2006) MAX
      !CALL OUTKDP(trim(OUTLYNE2))

      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)

      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XOB') THEN
      WRITE(OUTLYNE2,2007) MAX

      CALL UPDATECV2INPUTANDPROCESCOMMAND(OUTLYNE2, SUB)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
        GO TO 8888
                              
      END IF  
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     XFLD OR YFLD
      IF(TEMPC(I)(1:4).EQ.'XFLD'
     1.OR.TEMPC(I)(1:4).EQ.'YFLD') THEN
      FIELDTYPE='   '
      IF(TEMPC(I)(1:4).EQ.'XFLD') THEN
      IF(FLDTYP.EQ.0) FIELDTYPE='XAN'
      IF(FLDTYP.EQ.1) FIELDTYPE='XOB'
      IF(FLDTYP.EQ.2) FIELDTYPE='XIM'
      IF(FLDTYP.EQ.3) FIELDTYPE='XIR'
                   END IF
      IF(TEMPC(I)(1:4).EQ.'YFLD') THEN
      IF(FLDTYP.EQ.0) FIELDTYPE='YAN'
      IF(FLDTYP.EQ.1) FIELDTYPE='YOB'
      IF(FLDTYP.EQ.2) FIELDTYPE='YIM'
      IF(FLDTYP.EQ.3) FIELDTYPE='YIR'
                   END IF
      CALL ONEBLANK(4,TEMPC(I)(1:1024))
      IF(FIELDTYPE.EQ.'XAN'.OR.FIELDTYPE.EQ.'YAN'.OR.
     1FIELDTYPE.EQ.'XOB'.OR.FIELDTYPE.EQ.'YOB') THEN
C     STRIP OFF 4 CHARACTERS
      TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      PRINT *, "TEMPC(I) is  ", TEMPC(I)(1:132)                 
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),9,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      MAX=-1000.0D0
                        DO J=1,9
      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                        END DO
      IF(FIELDTYPE.EQ.'YAN') THEN
      WRITE(OUTLYNE,2004) MAX
 2004 FORMAT('SCY FANG,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        PRINT *, "SCY ABout to proess ", INPUT(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XAN') THEN
      WRITE(OUTLYNE,2005) MAX
 2005 FORMAT('SCX FANG,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        PRINT *, "SCX ABout to proess ", INPUT(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YOB') THEN
      WRITE(OUTLYNE,2006) MAX
 2006 FORMAT('SCY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XOB') THEN
      WRITE(OUTLYNE,2007) MAX
 2007 FORMAT('SCX,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
                        END IF
      IF(FIELDTYPE.EQ.'XIM'.OR.FIELDTYPE.EQ.'YIM') THEN
C     IMAGE HT SPEC
C     STRIP OFF 4 CHARACTERS
      TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),9,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      MAX=-1000.0D0
                        DO J=1,25
      IF(DABS(WD(J)).GT.MAX) MAX=DABS(WD(J))
                        END DO
      IF(FIELDTYPE.EQ.'XIM') THEN
      WRITE(OUTLYNE,4004) MAX
 4004 FORMAT('PXIM,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YIM') THEN
      WRITE(OUTLYNE,4005) MAX
 4005 FORMAT('PYIM,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'XIR') THEN
      WRITE(OUTLYNE,4006) MAX
 4006 FORMAT('RXIM,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(FIELDTYPE.EQ.'YIR') THEN
      WRITE(OUTLYNE,4007) MAX
 4007 FORMAT('RYIM,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
C     WAVL
      IF(TEMPC(I)(1:4).EQ.'WAVL') THEN
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE WV AND WV2 COMMANDS
C     AS NEEDED.
C     STRIP OFF FOUR CHARACTERS
      TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),10,ZMXERROR)
      IF(ZMXERROR) THEN
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
      WRITE(OUTLYNE,2008) WD(1),WD(2),WD(3)
     1,WD(4),WD(5)
 2008 FORMAT('WV,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(DOWV2) THEN
      WRITE(OUTLYNE,2009) WD(6),WD(7),WD(8)
     1,WD(9),WD(10)
 2009 FORMAT('WV2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
C     WWGT
      IF(TEMPC(I)(1:4).EQ.'WWGT') THEN
C     BREAK OUT UP TO 10 NUMERIC WORDS AND RETURN THEM RIGHT ADJUSTED
C     IN THE ARRAY WD WITH 0/1 OCCUPANCY FLAGS IN ARRAY WDEXIS
C     THEN CONVERT THEM TO NUMERIC WORDS AND ISSUE THE SPTWT/SPTWT2 COMMANDS
C     AS NEEDED.
C     STRIP OFF WWGT
      TEMPC(I)(1:1024)=TEMPC(I)(5:1024)//'    '
C     STRIP LEADING BLANKS
                        DO J=1,1024
      IF(TEMPC(I)(1:1).EQ.' ') TEMPC(I)(1:1024)=TEMPC(I)(2:1024)//' '
                        END DO
      ZMXERROR=.FALSE.
      CALL IMULTIPROCESS(IWD,WDEXIS,TEMPC(I)(1:1024),10,ZMXERROR)
      IF(ZMXERROR) THEN
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
      WRITE(OUTLYNE,2010) WD(1),WD(2),WD(3),WD(4),WD(5)
 2010 FORMAT('SPTWT,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
      IF(DOWV2) THEN
      WRITE(OUTLYNE,2011) WD(6),WD(7),WD(8),WD(9),WD(10)
 2011 FORMAT('SPTWT2,',D23.15,',',D23.15,',',D23.15,',',D23.15,','
     1,D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
                                GO TO 8888
                                END IF
C     STOP
      IF(TEMPC(I)(1:4).EQ.'STOP') THEN
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='ASTOP'
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT='REFS'
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     UNIT
      IF(TEMPC(I)(1:4).EQ.'UNIT') THEN 
        IF(INDEX(TEMPC(I)(5:1024),'MM').GT.0) THEN
            PRINT *, "Set Units to MM"
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='UNITS MM'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)   
        ELSEIF(INDEX(TEMPC(I)(5:1024),'M ').GT.0) THEN             
                SAVE_KDP(1)=SAVEINPT(1)
                INPUT='UNITS M'
                CALL PROCES
                REST_KDP(1)=RESTINPT(1)                 
        END IF        
        IF(INDEX(TEMPC(I)(5:1024),'CM').GT.0) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='UNITS CM'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
        END IF            
        IF(INDEX(TEMPC(I)(5:1024),'IN').GT.0) THEN
            SAVE_KDP(1)=SAVEINPT(1)
            INPUT='UNITS IN'
            CALL PROCES
            REST_KDP(1)=RESTINPT(1)
        END IF    
        

        GO TO 8888
        END IF
C
C     PROCESS EACH LINE INTO A KDP COMMAND AND THEN DISPLAY IT
      IF(TEMPC(I)(1:4).EQ.'SURF') THEN
        PRINT *, "FOUND SURF, SURFER ", SURFER
      IF(.NOT.SURFIT) THEN
               SURFIT=.TRUE.
               GO TO 8888
               END IF
C
C     WRITE CLAP DATA, COBS DATA, THEN GLASS DATA
C     CLAP DATA
      IF(CLTYPE.EQ.'C') THEN
      WRITE(OUTLYNE,2012) CL1,CL4,CL3,CL2
 2012 FORMAT('CLAP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
                        END IF
      IF(CLTYPE.EQ.'R') THEN
      WRITE(OUTLYNE,2013) CL2,CL1,CL4,CL3
 2013 FORMAT('CLAP RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
                        END IF
      IF(CLTYPE.EQ.'E') THEN
      WRITE(OUTLYNE,2014) CL2,CL1,CL4,CL3
 2014 FORMAT('CLAP ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      CLTYPE='?'
      CLAP1=.FALSE.
      CLAP2=.FALSE.
      CLDX=.FALSE.
      CLDY=.FALSE.
      CL1=0.0D0
      CL2=0.0D0
      CL3=0.0D0
      CL4=0.0D0
      CL5=0.0D0
                        END IF
C     COBS DATA
      IF(COTYPE.EQ.'C') THEN
      WRITE(OUTLYNE,4012) CO1,CO4,CO3
 4012 FORMAT('COBS,',D23.15,',',D23.15,',',D23.15,',,,')
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
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
      WRITE(OUTLYNE,4013) CO2,CO1,CO4,CO3
 4013 FORMAT('COBS RECT,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
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
      WRITE(OUTLYNE,4014) CO2,CO1,CO4,CO3
 4014 FORMAT('COBS ELIP,',D23.15,',',D23.15,',',D23.15,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
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
C     GLASS DATA
      IF(GLASSA(1:3).EQ.'   ') GLASSA='AIR                        '
      WRITE(OUTLYNE,2016) GLASSA
 2016 FORMAT(A27)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        SURFER=SURFER+1
      GLASSA='AIR                        '
      TEMPC(I)(1:1024)=BL1024(1:1024)
      TIL='?'
                        GO TO 8888
                        END IF
C
C     TILTS AND DECENTERS GO HERE
      IF(TEMPC(I)(1:13).EQ.'TYPE TILTSURF') THEN
      WRITE(OUTLYNE,2019)
 2019 FORMAT('TILT DAR')
      SURFTYPE='TILTSURF'
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TIL='T'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        GO TO 8888
                        END IF
      IF(TEMPC(I)(1:13).EQ.'TYPE COORDBRK') THEN
      SURFTYPE='COORDBRK'
      WRITE(OUTLYNE,2021)
 2021 FORMAT('TILT')
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TIL='T'
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        GO TO 8888
                        END IF
C
      IF(SURFTYPE.EQ.'COORDBRK') THEN
C
      IF(TEMPCC(1:4).EQ.'PARM') THEN
      ZMXERROR=.FALSE.
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
      CALL PARAMOUT(IPAR,VPAR,TEMPCC,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
      IF(IPAR.EQ.3) THEN
      WRITE(OUTLYNE,2023) -VPAR
 2023 FORMAT('ALPHA,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.4) THEN
      WRITE(OUTLYNE,2024) -VPAR
 2024 FORMAT('BETA,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.5) THEN
      WRITE(OUTLYNE,2025) VPAR
 2025 FORMAT('GAMMA,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.1) THEN
      WRITE(OUTLYNE,2026) VPAR
 2026 FORMAT('XD,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.2) THEN
      WRITE(OUTLYNE,2027) VPAR
 2027 FORMAT('YD,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                   ELSE
C     NOT PARM
                   END IF
                   ELSE
C     NOT COORDBRK
                   END IF
C
      IF(SURFTYPE.EQ.'TILTSURF') THEN
C
      IF(TEMPCC(1:4).EQ.'PARM') THEN
      ZMXERROR=.FALSE.
      TEMPCC(1:1024)=TEMPC(I)(1:1024)
      CALL PARAMOUT(IPAR,VPAR,TEMPCC,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
C
C     PROCEED PROCESSING THE PARAMETER INPUT WITH COORDBRK SURFACE
      IF(IPAR.EQ.1) THEN
      WRITE(OUTLYNE,2024) -(180.0D0*DATAN(VPAR))/PII
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      IF(IPAR.EQ.2) THEN
      WRITE(OUTLYNE,2023) -(180.0D0*DATAN(VPAR))/PII
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
                        END IF
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                   ELSE
C     NOT PARM
                   END IF
                   ELSE
C     NOT TILTSURF
                   END IF
C
C     TILTS AND DECENTERD DONE
C
C     THICKNESS DISZ
C
      IF(TEMPC(I)(1:4).EQ.'DISZ') THEN
      TEMPER=TEMPC(I)(6:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      IF(VALA(1:8).EQ.'INFINITY') THEN
                       VALV=1.0D20
                       ELSE
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                       CALL ATODZMX(VALA,VALV,ZMXERROR)
                       END IF
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2030) VALV
 2030 FORMAT('TH,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     THICKNESS DONE
C
C     CHIEF RAY THICKNESS SOLVE
C
      IF(TEMPC(I)(1:4).EQ.'CHZH') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2040) VALV
 2040 FORMAT('PCY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CHIEF RAY THICKNESS SOLVE DONE
C
C     MARGINAL RAY THICKNESS SOLVE
C
      IF(TEMPC(I)(1:4).EQ.'MAZH') THEN
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2038) WD(1)
 2038 FORMAT('PY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     MARGINAL RAY THICKNESS SOLVE DONE
C
      IF(TEMPC(I)(1:4).EQ.'EDGE') THEN
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPC(I)(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,3038) WD(1)
 3038 FORMAT('CAY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     EDGE THICKNESS SOLVE DONE
C
C
C
C     CHIEF RAY CURVATURE SOLVE
C
      IF(TEMPC(I)(1:4).EQ.'CHIA') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2082) VALV
 2082 FORMAT('PUCY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CHIEF RAY CURVATURE SOLVE DONE
C
C
C     MARGINAL RAY CURVATURE SOLVE
C
      IF(TEMPC(I)(1:4).EQ.'MARA') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2080) VALV
 2080 FORMAT('PUY,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C
C     CENTER OF CURVATURE SOLVE
C
      IF(TEMPC(I)(1:4).EQ.'TOCO') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATOIZMX(VALA,IVALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2083) IVALV
 2083 FORMAT('PUY,',I3)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CENTER OF CURVATURE SOLVE DONE
C
C     APLANATIC MARGINAL CURVATURE SOLVE
C
      IF(TEMPC(I)(1:4).EQ.'APLA') THEN
      WRITE(OUTLYNE,2081)
 2081 FORMAT('APY')
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     MARGINAL RAY CURVATURE SOLVE DONE
C
C     CURVATURE CURV
C
      IF(TEMPC(I)(1:4).EQ.'CURV') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2042) VALV
 2042 FORMAT('CV,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        PRINT *, "INPUT IS ", INPUT(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CURVATURE DONE
C
C     CURVATURE PIKUP
C
      IF(TEMPC(I)(1:4).EQ.'PCUP') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2086) INT(WD(1)),WD(2)
 2086 FORMAT('PIKUP CV,',I3,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CURVATURE PIKUP DONE
C
C     CONIC PIKUP
C
      IF(TEMPC(I)(1:4).EQ.'PKUP') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2087) INT(WD(1)),WD(2)
 2087 FORMAT('PIKUP CC,',I3,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CONIC PIKUP DONE
C
C     THICKNESS PIKUP
C
      IF(TEMPC(I)(1:4).EQ.'PZUP') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2088) INT(WD(1)),WD(2)
 2088 FORMAT('PIKUP TH,',I3,',',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     THICKNESS PIKUP DONE
C
C     RADII RADI
C
      IF(TEMPC(I)(1:4).EQ.'RADI') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2084) VALV
 2084 FORMAT('RD,',D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     RADII DONE
C
C     CONIC CONI
C
      IF(TEMPC(I)(1:4).EQ.'CONI') THEN
      TEMPER=TEMPC(I)(5:1024)
      CALL LEFTJUST(TEMPER)
      VALA=TEMPER(1:23)
      CALL RIGHTJUST(VALA)
      ZMXERROR=.FALSE.
                        CALL ATODZMX(VALA,VALV,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      WRITE(OUTLYNE,2053) VALV
 2053 FORMAT('CC,'D23.15)
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CONIC DONE
C
C     DETERMINE GLASS TYPE
C
      IF(TEMPC(I)(1:4).EQ.'GLAS') THEN
      TEMPER=TEMPC(I)(5:1024)
C     REMOVE UP TO 10 LEADING BLANKS
                       DO J=1,10
      IF(TEMPER(1:1).EQ.' ') TEMPER(1:1024)=TEMPER(2:1024)//' '
                       END DO
C     FIND NEXT BLANK
                       J=1
                       DO WHILE(TEMPER(J:J).NE.' ')
                       J=J+1
                       END DO
      IF(TEMPER(1:3).EQ.'AIR') GLASSA='AIR                        '
      IF(TEMPER(1:4).EQ.'REFL') GLASSA='REFL                       '
      IF(TEMPER(1:6).EQ.'MIRROR') GLASSA='REFL                       '
      IF(TEMPER(1:3).NE.'AIR'.AND.TEMPER(1:4).NE.'REFL'.AND.
     1TEMPER(1:6).NE.'MIRROR')
     2GLASSA='GLA '//TEMPER(1:J-1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                ELSE
C     NOT GLAS
                                END IF
C     GLASSA DETERMINED
C
C     CIRCULAR CLAP DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'CLAP') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      CLTYPE='C'
      CL1=WD(2)
      CL2=WD(2)
      CLAP1=.TRUE.
      CLAP2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CIRCULAR CLAP DIMENSIONS DONE
C
C     CIRCULAR DIAM DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'DIAM') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),3,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      CLTYPE='C'
      CL1=WD(1)
      CL2=WD(1)
      CLAP1=.TRUE.
      CLAP2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CIRCULAR CLAP DIMENSIONS DONE
C
C     ELLIPTICAL CLAP DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'ELAP') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      CLTYPE='E'
      CL1=WD(2)
      CL2=WD(1)
      CLAP1=.TRUE.
      CLAP2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     ELIPTICAL CLAP DIMENSIONS DONE
C
C     RECTANGULAR CLAP DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'SQAP') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      CLTYPE='R'
      CL1=WD(2)
      CL2=WD(1)
      CLAP1=.TRUE.
      CLAP2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     RECTANGULAR CLAP DIMENSIONS DONE
C
C     APERTURE DECENTERS
C
      IF(PREV(1:4).EQ.'CLAP'.OR.
     1PREV(1:4).EQ.'ELAP'.OR.
     2PREV(1:4).EQ.'SQAP') THEN
      IF(TEMPC(I)(1:4).EQ.'OBDC') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      CLDX=.TRUE.
      CLDY=.TRUE.
      CL3=WD(2)
      CL4=WD(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C
C     COBS DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'OBSC') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      COTYPE='C'
      CO1=WD(2)
      CO2=WD(2)
      COBS1=.TRUE.
      COBS2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     CIRCULAR COBS DIMENSIONS DONE
C
C     ELLIPTICAL CLAP DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'ELOB') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      COTYPE='E'
      CO1=WD(2)
      CO2=WD(1)
      COBS1=.TRUE.
      COBS2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     ELIPTICAL COBS DIMENSIONS DONE
C
C     RECTANGULAR COBS DIMENSIONS
C
      IF(TEMPC(I)(1:4).EQ.'SQOB') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      COTYPE='R'
      CO1=WD(2)
      CO2=WD(1)
      COBS1=.TRUE.
      COBS2=.TRUE.
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
C     RECTANGULAR COBS DIMENSIONS DONE
C
C     OBSCURATION DECENTERS
C
      IF(PREV.EQ.'OBSC'.OR.
     1PREV(1:4).EQ.'ELOB'.OR.
     2PREV(1:4).EQ.'SQOB') THEN
      IF(TEMPC(I)(1:4).EQ.'OBDC') THEN
C     PROCEED
      TEMPER=TEMPC(I)(5:1024)
C     PROCESS MULTI ENTRY
      ZMXERROR=.FALSE.
      CALL DMULTIPROCESS(WD,WDEXIS,TEMPER(1:1024),2,ZMXERROR)
      IF(ZMXERROR) THEN
      WRITE(38,4001) TEMPCC(1:78)
      GO TO 8888
          END IF
      COBX=.TRUE.
      COBY=.TRUE.
      CO3=WD(2)
      CO4=WD(1)
      TEMPC(I)(1:1024)=BL1024(1:1024)
                                GO TO 8888
                                END IF
                                END IF
C
      IF(TEMPC(I)(1:2).EQ.'GO') THEN
      TEMPC(I)(1:1024)=BL1024(1:1024)
                        END IF
C     IF WE ARE HERE, THE COMMAND DID NOT TRANSLATE
      IF(TEMPC(I)(1:75).NE.BL1024(1:78)) THEN
      WRITE(38,4001) TEMPCC(1:78)
                        END IF
 4001 FORMAT(' ',A78)
C     DO NEXT LINE IN THE ZEMAX FILE
 8888                   CONTINUE
      PREV(1:80)=TEMPCCC(1:80)
                        END DO
      IF(GLASSA(1:3).EQ.'   ') GLASSA='AIR                        '
      WRITE(OUTLYNE,2016) GLASSA

        SAVE_KDP(1)=SAVEINPT(1)
        INPUT(1:132)=OUTLYNE(1:132)
        PRINT *, "8888 SURF GLASSA is ", GLASSA
        PRINT *, "LINE TO PROCES IS ", INPUT(1:132)
        CALL PROCES
        REST_KDP(1)=RESTINPT(1)

                SURFER=SURFER+1
      WRITE(OUTLYNE,2089)
 2089 FORMAT('EOS')
                        SAVE_KDP(1)=SAVEINPT(1)
                        INPUT(1:132)=OUTLYNE(1:132)
                        CALL PROCES
                        REST_KDP(1)=RESTINPT(1)
      CALL CLOSE_FILE(37,1)
      CALL CLOSE_FILE(38,1)
      DEALLOCATE (TEMPA,TEMPB,TEMPC,STAT=ALLOERR)
                                RETURN
 100  FORMAT(A132)
                                END

