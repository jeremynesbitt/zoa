      SUBROUTINE DMULTIPROCESS(WD,WDEXIS,INSTRING,N,CVERROR)
      IMPLICIT NONE
      CHARACTER AA23*23,WORD*23,VALA*23
      REAL*8 WD,VALV
      CHARACTER INSTRING*1024,BL1024*1024
      LOGICAL CVERROR
      INTEGER WDEXIS,N,I,K
      DIMENSION WD(1:N),WDEXIS(1:N)
      INCLUDE 'DATMAI.INC'
      AA23='                       '
      BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//'    '
C     INITIALIZE RETURN ARRAYS
                        DO I=1,N
                        WD(I)=0.0D0
                        WDEXIS(I)=0
                        END DO
C     INSTRING HAS NO LEADING BLANKS
                        DO K=1,N
C     REMOVE LEADING BLANKS FROM REBUILT STRING
                        DO I=1,1024
      IF(INSTRING(1:1).EQ.' ') THEN
      INSTRING(1:1024)=INSTRING(2:1024)//' '
                        END IF
                        END DO
                        DO I=1,1024
      IF(INSTRING(I:I).EQ.' ') THEN
      IF(INSTRING(1:1024).EQ.BL1024(1:1024)) RETURN
C     STRIP OUT WORD
      WORD(1:23)=INSTRING(1:I-1)
C     REBUILD INSTRING
      INSTRING(1:1024)=INSTRING(I:1024)//BL1024(1:I-1)
C     PACK RIGHT AND TRANSLATE
      VALA(1:23)=AA23(1:(23-(I-1)))//WORD(1:(I-1))
      CALL ATODCODEV(VALA,VALV,CVERROR)
      IF(CVERROR) THEN
      GO TO 6666
          END IF
C     STORE IN RETURN ARRAY
      WD(K)=VALV
C     SET FLAG ARRAY
      WDEXIS(K)=1
C     PROCESS NEXT NUMERIC WORD
                        GO TO 6666
                        ELSE
C     CHECK NEXT CHARACTER FOR A BLANK
                        END IF
                        END DO
 6666                   CONTINUE
                        END DO
                        RETURN
                        END
      SUBROUTINE IMULTIPROCESS(IWD,WDEXIS,INSTRING,N,CVERROR)
      IMPLICIT NONE
      CHARACTER AA23*23,WORD*23,VALA*23
      INTEGER IWD,IVALV
      LOGICAL CVERROR
      CHARACTER INSTRING*1024,BL1024*1024
      INTEGER WDEXIS,N,I,K
      DIMENSION IWD(1:N),WDEXIS(1:N)
      INCLUDE 'DATMAI.INC'
      AA23='                       '
      BL1024=AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//AA//AA//AA//AA//AA//AA//AA//AA//AA
     1     //AA//'    '
C     INITIALIZE RETURN ARRAYS
                        DO I=1,N
                        IWD(I)=0
                        WDEXIS(I)=0
                        END DO
C     INSTRING HAS NO LEADING BLANKS
                        DO K=1,N
C     REMOVE LEADING BLANKS FROM REBUILT STRING
                        DO I=1,1024
      IF(INSTRING(1:1).EQ.' ') THEN
      INSTRING(1:1024)=INSTRING(2:1024)//' '
                        END IF
                        END DO
                        DO I=1,1024
      IF(INSTRING(I:I).EQ.' ') THEN
      IF(INSTRING(1:1024).EQ.BL1024(1:1024)) RETURN
C     STRIP OUT WORD
      WORD(1:23)=INSTRING(1:I-1)
C     REBUILD INSTRING
      INSTRING(1:1024)=INSTRING(I:1024)//BL1024(1:I-1)
C     PACK RIGHT AND TRANSLATE
      VALA(1:23)=AA23(1:(23-(I-1)))//WORD(1:(I-1))
      CALL ATOICODEV(VALA,IVALV,CVERROR)
      IF(CVERROR) THEN
      GO TO 6666
          END IF
C     STORE IN RETURN ARRAY
      IWD(K)=IVALV
C     SET FLAG ARRAY
      WDEXIS(K)=1
C     PROCESS NEXT NUMERIC WORD
                        GO TO 6666
                        ELSE
C     CHECK NEXT CHARACTER FOR A BLANK
                        END IF
                        END DO
 6666                   CONTINUE
                        END DO
                        RETURN
                        END
      SUBROUTINE ONEBLANK(N,STRING1024)
      IMPLICIT NONE
C
      INTEGER N
C
      CHARACTER STRING1024*1024
  10                    CONTINUE
      IF(STRING1024(N+1:N+2).EQ.' ') THEN
      STRING1024(1:1024)=STRING1024(1:N+1)//STRING1024((N+3):1024)//' '
                        GO TO 10
                        ELSE
                        RETURN
                        END IF
                        END
      SUBROUTINE RIGHTJUST(VALA)
      IMPLICIT NONE
      CHARACTER*23 VALA
      INTEGER I
                        DO I=1,23
      IF(VALA(23:23).EQ.' ') THEN
      VALA(1:23)=' '//VALA(1:22)
                        ELSE
                        RETURN
                        END IF
                        END DO
                         RETURN
                         END
      SUBROUTINE LEFTJUST(TEMPER)
      IMPLICIT NONE
      CHARACTER*1024 TEMPER
      INTEGER I
                        DO I=1,1024
      IF(TEMPER(1:1).EQ.' ') THEN
      TEMPER(1:1024)=TEMPER(2:1024)//' '
                        ELSE
                        RETURN
                        END IF
                        END DO
                         RETURN
                         END
C SUB CONTRO.FOR
        SUBROUTINE CONTRO
        use zoa_file_handler, only: getPermMacroDir
        use global_widgets, only: lens_editor_window,
     1 curr_asph_data, sysConfig, curr_lens_data
        use lens_editor, only: buildBasicTable, buildAsphereTable
        use handlers, only: zoatabMgr
        use codeV_commands, only: startCodeVLensUpdateCmd, 
     1   isCodeVCommand
        use iso_c_binding, only: c_associated  
        use DATLEN      
C
        IMPLICIT NONE
C
        CHARACTER WCC(1:16000)*8,WC1*8,WQ1*8,WS1*80
C
        LOGICAL YESEOS,STP,IS,FNYES,FNYES1,NOGO,OHFUN,OREFEXT,ORAYEXT
C
      INTEGER I,NNN,NNNN,OS1,OS2,OS3,OS4,ODF1,ODF2,ODF3,ODF4
     1,MACYES,RET,RETRET,SST1,FF29,FF51,FF52,SQ1,SPEOS,LEOS
C
      COMMON/RETIT/RET,RETRET,LEOS,SPEOS
C
        REAL*8 WWW1,OW1,OW2,OW3,OW4
C
        COMMON/COMWDS/WCC
C
        CHARACTER REMWQ*8
        COMMON/WQREM/REMWQ
C
        INCLUDE 'DATMAC.INC'
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATSUB.INC'
        !INCLUDE 'DATLEN.INC'

        !PRINT *, "CONTRO ROUTINE STARTED! "
        !WRITE(OUTLYNE,*)  "CONTROL INPUT IS ", WC(1:8)
        !CALL SHOWIT(19)

        IF(WC.EQ.'ANGLES') RETURN
        IF(WC.EQ.'MYGLASS') WC='GLASS'

      IF(WC.NE.'PLTCHRSH') CHRSHIFTEXT=.FALSE.
      IF(F3.NE.1.AND.F2.NE.1) THEN
      IF(WC(1:8).EQ.'SHO     '.AND.SQ.EQ.0.AND.S1.EQ.0) THEN
                       SQ=1
                       WQ='X       '
                       END IF
                       END IF
      IF(WC.EQ.'GLASS   '.AND.WS(1:4).EQ.'REFL') THEN
                        WS=' '
                        SST=0
                        WC='REFL    '
                        END IF
      IF(WC.EQ.'GLASS   '.AND.WS(1:4).EQ.'REFLTIRO') THEN
                        WS=' '
                        SST=0
                        WC='REFLTIRO'
                        END IF
      IF(WC.EQ.'GLASS   '.AND.WS(1:4).EQ.'REFLTIR') THEN
                        WS=' '
                        SST=0
                        WC='REFLTIR'
                        END IF
      IF(WC.EQ.'GLASS   '.AND.WS(1:3).EQ.'AIR') THEN
                        WS=' '
                        SST=0
                        WC='AIR     '
                        END IF

      REMWQ='        '
C
      IF(WS(1:40).EQ.'                                        ') THEN
                   SST=0
                   END IF
C
      OREFEXT=REFEXT
      ORAYEXT=RAYEXT
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
      IF(LASTWASFOB) REFEXT=OREFEXT
      IF(LASTWASFOB) RAYEXT=ORAYEXT
C
      IF(ALLSET) THEN
      IF(SN.EQ.0.AND.STI.EQ.0.AND.SQ.EQ.0.AND.SST.EQ.0) THEN
      IF(WC.EQ.'RTG') GO TO 5
      IF(WC.EQ.'RTGLBL') GO TO 5
      IF(WC.EQ.'CTG') GO TO 5
      IF(WC.EQ.'CTGLBL') GO TO 5
      IF(WC.EQ.'RIN') GO TO 5
      IF(WC.EQ.'CAOB') GO TO 5
      IF(WC.EQ.'TAD') GO TO 5
      IF(WC.EQ.'ASPH') GO TO 5
      IF(WC.EQ.'ASPH2') GO TO 5
      IF(WC.EQ.'TASPH') GO TO 5
      IF(WC.EQ.'TR') GO TO 5
      IF(WC.EQ.'TC') GO TO 5
      IF(WC.EQ.'SLV') GO TO 5
      IF(WC.EQ.'PIK') GO TO 5
      IF(WC.EQ.'RIN') GO TO 5
      IF(WC.EQ.'RIN2') GO TO 5
      IF(WC.EQ.'DUMOUT') GO TO 5
      IF(WC.EQ.'INR') GO TO 5
      IF(WC.EQ.'PRSPR') GO TO 5
      IF(WC.EQ.'PIVOT') GO TO 5
      IF(WC.EQ.'PIVAXIS') GO TO 5
      IF(WC.EQ.'PXTY') GO TO 5
      IF(WC.EQ.'PXTX') GO TO 5
      IF(WC.EQ.'PITY') GO TO 5
      IF(WC.EQ.'PITX') GO TO 5
      IF(WC.EQ.'PRTY') GO TO 5
      IF(WC.EQ.'PRTX') GO TO 5
      IF(WC.EQ.'PRX') GO TO 5
      IF(WC.EQ.'PRY') GO TO 5
      IF(WC.EQ.'PRXYZ') GO TO 5
      IF(WC.EQ.'PRXYI') GO TO 5
      IF(WC.EQ.'PRXYIP') GO TO 5
      IF(WC.EQ.'PRXYD') GO TO 5
      IF(WC.EQ.'PRZ') GO TO 5
      IF(WC.EQ.'PRR') GO TO 5
      IF(WC.EQ.'PRREF') GO TO 5
      IF(WC.EQ.'PRLMN') GO TO 5
      IF(WC.EQ.'PRDIFFXR') GO TO 5
      IF(WC.EQ.'PRDIFFYR') GO TO 5
      IF(WC.EQ.'PRDIFFXM') GO TO 5
      IF(WC.EQ.'PRDIFFYM') GO TO 5
      IF(WC.EQ.'GPXTY') GO TO 5
      IF(WC.EQ.'GPXTX') GO TO 5
      IF(WC.EQ.'VERTEX') GO TO 5
      IF(WC.EQ.'PRGLOBAL') GO TO 5
      IF(WC.EQ.'BEAM') GO TO 5
      IF(WC.EQ.'NDEX') GO TO 5
      IF(WC.EQ.'NDEX2') GO TO 5
                       GO TO 6
 5    WQ='ALL'
      SQ=1
 6                     CONTINUE
                       END IF
                       END IF
C
      IF(WC(1:8).EQ.'END     ') WC='EOS     '
C
C       IT IS WITHIN THIS SUBROUTINE THAT THE DECISION (BASED
C       UPON INPUT) IS MADE AS TO WHAT TO DO OR CALCULATE.
C       FLAG STATUS AS TO WHICH LEVEL OF THE PROGRAM IS BEING
C       RUN IS ALWAYS PASSED VIA A (COMMON) BACK TO THIS
C       SUBROUTINE. THE COMMON IS NAMED FLAGS COMMON.
C
      IF(F3.NE.1.AND.F2.NE.1) THEN
      IF(WC(1:8).EQ.'SHO     '.AND.SQ.EQ.0.OR.
     1WQ.EQ.'SHOW'.AND.SQ.EQ.0) THEN
      IF(SQ.EQ.0.AND.S1.EQ.0) THEN
                      ELSE
                      CALL SHOWREG
                      END IF
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
                      END IF
      IF(F3.NE.1.AND.F2.NE.1) THEN
C     NOT MACRO EDITTING OR INPUT, DO A REPLACEMENT
      IF(WC(1:8).EQ.'SHO     '.AND.SQ.EQ.1.OR.
     1WQ.EQ.'SHOW'.AND.SQ.EQ.1) THEN
                      REMWQ=WQ
                      SHOW=.TRUE.
                      WC='GET'
                      ELSE
                      SHOW=.FALSE.
                      END IF
                      END IF
      IF(F3.NE.1.AND.F2.NE.1) THEN
C     NOT MACRO EDITTING OR INPUT, DO A REPLACEMENT
      IF(WC(1:4).EQ.'ASHO'.AND.SQ.EQ.1.OR.
     1WQ.EQ.'ASHOW'.AND.SQ.EQ.1) THEN
                      REMWQ=WQ
                      ASHOW=.TRUE.
                      WC='AGET'
                      ELSE
                      ASHOW=.FALSE.
                      END IF
                      END IF
C
C     3/8/94 HERE IS THE ACTION FOR "W1" THROUGH "W5"
C
      IF(F2.NE.1.AND.F3.NE.1) THEN
      IF(WC.NE.'W1'.AND.WC.NE.'W2'.AND.WC.NE.'W3'.AND.WC.NE.'W4'
     1.AND.WC.NE.'W5'.AND.WC.NE.'GET'.AND.WC.NE.'AGET') THEN
                      IF(W1CODE.NE.0) THEN
      IF(DF1.EQ.1) THEN
C     ASSIGN A VALUE
      IF(W1CODE.LT.0.0D0) THEN
      IF(W1CODE.EQ.-100) W1=REG(1)
      IF(W1CODE.EQ.-99) W1=REG(2)
      IF(W1CODE.EQ.-98) W1=REG(3)
      IF(W1CODE.EQ.-97) W1=REG(4)
      IF(W1CODE.EQ.-96) W1=REG(5)
      IF(W1CODE.EQ.-95) W1=REG(6)
      IF(W1CODE.EQ.-94) W1=REG(7)
      IF(W1CODE.EQ.-93) W1=REG(8)
      IF(W1CODE.EQ.-92) W1=REG(9)
      IF(W1CODE.EQ.-91) W1=REG(10)
      IF(W1CODE.EQ.-90) W1=REG(11)
      IF(W1CODE.EQ.-89) W1=REG(12)
      IF(W1CODE.EQ.-88) W1=REG(13)
      IF(W1CODE.EQ.-87) W1=REG(14)
      IF(W1CODE.EQ.-86) W1=REG(15)
      IF(W1CODE.EQ.-85) W1=REG(16)
      IF(W1CODE.EQ.-84) W1=REG(17)
      IF(W1CODE.EQ.-83) W1=REG(18)
      IF(W1CODE.EQ.-82) W1=REG(19)
      IF(W1CODE.EQ.-81) W1=REG(20)
      IF(W1CODE.EQ.-80) W1=REG(40)
      IF(W1CODE.EQ.-79) W1=REG(30)
      IF(W1CODE.EQ.-78) W1=REG(21)
      IF(W1CODE.EQ.-77) W1=REG(22)
      IF(W1CODE.EQ.-76) W1=REG(23)
      IF(W1CODE.EQ.-75) W1=REG(24)
      IF(W1CODE.EQ.-74) W1=REG(25)
      IF(W1CODE.EQ.-73) W1=REG(26)
      IF(W1CODE.EQ.-72) W1=REG(27)
      IF(W1CODE.EQ.-71) W1=REG(28)
                      ELSE
                      W1=GPREG(W1CODE)
                      END IF
                      DF1=0
                      S1=1
                      SN=1
C     DON'T ASSIGN W1 A VALUE
                      END IF
C     W1CODE WAS ZERO
                      END IF
                      IF(W2CODE.NE.0) THEN
      IF(DF2.EQ.1) THEN
      IF(W2CODE.LT.0.0D0) THEN
      IF(W2CODE.EQ.-100) W2=REG(1)
      IF(W2CODE.EQ.-99) W2=REG(2)
      IF(W2CODE.EQ.-98) W2=REG(3)
      IF(W2CODE.EQ.-97) W2=REG(4)
      IF(W2CODE.EQ.-96) W2=REG(5)
      IF(W2CODE.EQ.-95) W2=REG(6)
      IF(W2CODE.EQ.-94) W2=REG(7)
      IF(W2CODE.EQ.-93) W2=REG(8)
      IF(W2CODE.EQ.-92) W2=REG(9)
      IF(W2CODE.EQ.-91) W2=REG(10)
      IF(W2CODE.EQ.-90) W2=REG(11)
      IF(W2CODE.EQ.-89) W2=REG(12)
      IF(W2CODE.EQ.-88) W2=REG(13)
      IF(W2CODE.EQ.-87) W2=REG(14)
      IF(W2CODE.EQ.-86) W2=REG(15)
      IF(W2CODE.EQ.-85) W2=REG(16)
      IF(W2CODE.EQ.-84) W2=REG(17)
      IF(W2CODE.EQ.-83) W2=REG(18)
      IF(W2CODE.EQ.-82) W2=REG(19)
      IF(W2CODE.EQ.-81) W2=REG(20)
      IF(W2CODE.EQ.-80) W2=REG(40)
      IF(W2CODE.EQ.-79) W2=REG(30)
      IF(W2CODE.EQ.-78) W2=REG(21)
      IF(W2CODE.EQ.-77) W2=REG(22)
      IF(W2CODE.EQ.-76) W2=REG(23)
      IF(W2CODE.EQ.-75) W2=REG(24)
      IF(W2CODE.EQ.-74) W2=REG(25)
      IF(W2CODE.EQ.-73) W2=REG(26)
      IF(W2CODE.EQ.-72) W2=REG(27)
      IF(W2CODE.EQ.-71) W2=REG(28)
                      ELSE
                      W2=GPREG(W2CODE)
                      END IF
                      DF2=0
                      S2=1
                      SN=1
                      END IF
                      END IF
                      IF(W3CODE.NE.0) THEN
      IF(DF3.EQ.1) THEN
      IF(W3CODE.LT.0.0D0) THEN
      IF(W3CODE.EQ.-100) W3=REG(1)
      IF(W3CODE.EQ.-99) W3=REG(2)
      IF(W3CODE.EQ.-98) W3=REG(3)
      IF(W3CODE.EQ.-97) W3=REG(4)
      IF(W3CODE.EQ.-96) W3=REG(5)
      IF(W3CODE.EQ.-95) W3=REG(6)
      IF(W3CODE.EQ.-94) W3=REG(7)
      IF(W3CODE.EQ.-93) W3=REG(8)
      IF(W3CODE.EQ.-92) W3=REG(9)
      IF(W3CODE.EQ.-91) W3=REG(10)
      IF(W3CODE.EQ.-90) W3=REG(11)
      IF(W3CODE.EQ.-89) W3=REG(12)
      IF(W3CODE.EQ.-88) W3=REG(13)
      IF(W3CODE.EQ.-87) W3=REG(14)
      IF(W3CODE.EQ.-86) W3=REG(15)
      IF(W3CODE.EQ.-85) W3=REG(16)
      IF(W3CODE.EQ.-84) W3=REG(17)
      IF(W3CODE.EQ.-83) W3=REG(18)
      IF(W3CODE.EQ.-82) W3=REG(19)
      IF(W3CODE.EQ.-81) W3=REG(20)
      IF(W3CODE.EQ.-80) W3=REG(40)
      IF(W3CODE.EQ.-79) W3=REG(30)
      IF(W3CODE.EQ.-78) W3=REG(21)
      IF(W3CODE.EQ.-77) W3=REG(22)
      IF(W3CODE.EQ.-76) W3=REG(23)
      IF(W3CODE.EQ.-75) W3=REG(24)
      IF(W3CODE.EQ.-74) W3=REG(25)
      IF(W3CODE.EQ.-73) W3=REG(26)
      IF(W3CODE.EQ.-72) W3=REG(27)
      IF(W3CODE.EQ.-71) W3=REG(28)
                      ELSE
                      W3=GPREG(W3CODE)
                      END IF
                      DF3=0
                      S3=1
                      SN=1
                      END IF
                      END IF
                      IF(W4CODE.NE.0) THEN
      IF(DF4.EQ.1) THEN
      IF(W1CODE.LT.0.0D0) THEN
      IF(W4CODE.EQ.-100) W4=REG(1)
      IF(W4CODE.EQ.-99) W4=REG(2)
      IF(W4CODE.EQ.-98) W4=REG(3)
      IF(W4CODE.EQ.-97) W4=REG(4)
      IF(W4CODE.EQ.-96) W4=REG(5)
      IF(W4CODE.EQ.-95) W4=REG(6)
      IF(W4CODE.EQ.-94) W4=REG(7)
      IF(W4CODE.EQ.-93) W4=REG(8)
      IF(W4CODE.EQ.-92) W4=REG(9)
      IF(W4CODE.EQ.-91) W4=REG(10)
      IF(W4CODE.EQ.-90) W4=REG(11)
      IF(W4CODE.EQ.-89) W4=REG(12)
      IF(W4CODE.EQ.-88) W4=REG(13)
      IF(W4CODE.EQ.-87) W4=REG(14)
      IF(W4CODE.EQ.-86) W4=REG(15)
      IF(W4CODE.EQ.-85) W4=REG(16)
      IF(W4CODE.EQ.-84) W4=REG(17)
      IF(W4CODE.EQ.-83) W4=REG(18)
      IF(W4CODE.EQ.-82) W4=REG(19)
      IF(W4CODE.EQ.-81) W4=REG(20)
      IF(W4CODE.EQ.-80) W4=REG(40)
      IF(W4CODE.EQ.-79) W4=REG(30)
      IF(W4CODE.EQ.-78) W4=REG(21)
      IF(W4CODE.EQ.-77) W4=REG(22)
      IF(W4CODE.EQ.-76) W4=REG(23)
      IF(W4CODE.EQ.-75) W4=REG(24)
      IF(W4CODE.EQ.-74) W4=REG(25)
      IF(W4CODE.EQ.-73) W4=REG(26)
      IF(W4CODE.EQ.-72) W4=REG(27)
      IF(W4CODE.EQ.-71) W4=REG(28)
                      ELSE
                      W4=GPREG(W4CODE)
                      END IF
                      DF4=0
                      S4=1
                      SN=1
                      END IF
                      END IF
                      IF(W5CODE.NE.0) THEN
      IF(DF5.EQ.1) THEN
      IF(W5CODE.LT.0.0D0) THEN
      IF(W5CODE.EQ.-100) W5=REG(1)
      IF(W5CODE.EQ.-99) W5=REG(2)
      IF(W5CODE.EQ.-98) W5=REG(3)
      IF(W5CODE.EQ.-97) W5=REG(4)
      IF(W5CODE.EQ.-96) W5=REG(5)
      IF(W5CODE.EQ.-95) W5=REG(6)
      IF(W5CODE.EQ.-94) W5=REG(7)
      IF(W5CODE.EQ.-93) W5=REG(8)
      IF(W5CODE.EQ.-92) W5=REG(9)
      IF(W5CODE.EQ.-91) W5=REG(10)
      IF(W5CODE.EQ.-90) W5=REG(11)
      IF(W5CODE.EQ.-89) W5=REG(12)
      IF(W5CODE.EQ.-88) W5=REG(13)
      IF(W5CODE.EQ.-87) W5=REG(14)
      IF(W5CODE.EQ.-86) W5=REG(15)
      IF(W5CODE.EQ.-85) W5=REG(16)
      IF(W5CODE.EQ.-84) W5=REG(17)
      IF(W5CODE.EQ.-83) W5=REG(18)
      IF(W5CODE.EQ.-82) W5=REG(19)
      IF(W5CODE.EQ.-81) W5=REG(20)
      IF(W5CODE.EQ.-80) W5=REG(40)
      IF(W5CODE.EQ.-79) W5=REG(30)
      IF(W5CODE.EQ.-78) W5=REG(21)
      IF(W5CODE.EQ.-77) W5=REG(22)
      IF(W5CODE.EQ.-76) W5=REG(23)
      IF(W5CODE.EQ.-75) W5=REG(24)
      IF(W5CODE.EQ.-74) W5=REG(25)
      IF(W5CODE.EQ.-73) W5=REG(26)
      IF(W5CODE.EQ.-72) W5=REG(27)
      IF(W5CODE.EQ.-71) W5=REG(28)
                      ELSE
                      W5=GPREG(W5CODE)
                      END IF
                      DF5=0
                      S5=1
                      SN=1
                      END IF
                      END IF
C     NOW CLEAR THE CODES
              W1CODE=0
              W2CODE=0
              W3CODE=0
              W4CODE=0
              W5CODE=0
                      ELSE
C     COMMAND WAS W1 THROUGH W5, CALL WWORD AND RETURN
C     NO MATER WHAT PROGRAM LEVEL WE ARE AT
      IF(WC.EQ.'W1'.OR.WC.EQ.'W2'.OR.WC.EQ.'W3'.OR.WC.EQ.'W4'
     1.OR.WC.EQ.'W5') CALL WWORD
      IF(WC.EQ.'GET') CALL GET
      IF(WC.EQ.'AGET') CALL AGET
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
        IF(WC.EQ.'A='.OR.WC.EQ.'B='.OR.WC.EQ.'KTEST='.OR.WC.EQ.'LTEST='
     1  .OR.WC.EQ.'C='.OR.WC.EQ.'K='.OR.WC.EQ.'L='.OR.WC.EQ.'M='.OR.
     1  WC.EQ.'D='.OR.WC.EQ.'E='.OR.WC.EQ.'F='.OR.WC.EQ.'N='.OR.
     2  WC.EQ.'G='.OR.WC.EQ.'H='.OR.WC.EQ.'I='.OR.WC.EQ.'MTEST='.OR.
     3  WC.EQ.'J='.OR.WC.EQ.'ITEST='.OR.WC.EQ.'JTEST='.OR.
     4  WC.EQ.'X='.OR.WC.EQ.'Y='.OR.WC.EQ.'Z='.OR.WC.EQ.'NTEST='.OR.
     5  WC.EQ.'T='.OR.WC.EQ.'IX='.OR.WC.EQ.'IY='.OR.
     6  WC.EQ.'IZ='.OR.WC.EQ.'IT=') THEN
        CALL AUXSET
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
      IF(WC.EQ.'SET') THEN
                      CALL SET
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
      IF(WC.EQ.'STOK') THEN
                      CALL GGPREG
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
      IF(WC.EQ.'SAVEREG') THEN
                      CALL GGPREG_SAVE
                      RETURN
                      END IF
      IF(WC.EQ.'RCL') THEN
                      CALL GGPREG
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
      IF(WC.EQ.'ARCL') THEN
                      CALL GPRGA
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
      IF(WC.EQ.'STOAX') THEN
                      CALL STOAX
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
      IF(WC.EQ.'PI      '.OR.WC.EQ.'INTGR   '.OR.WC.EQ.'FRAC    '.OR.
     1   WC.EQ.'CHS     '.OR.WC.EQ.'RTD     '.OR.WC.EQ.'DTR     '.OR.
     1   WC.EQ.'ASIN    '.OR.WC.EQ.'ACOS    '.OR.WC.EQ.'PLUS    '.OR.
     1   WC.EQ.'MINUS   '.OR.WC.EQ.'DIV     '.OR.WC.EQ.'MPY     '.OR.
     1   WC.EQ.'MOVE    '.OR.WC.EQ.'ATAN    '.OR.WC.EQ.'RAND    '.OR.
     1   WC.EQ.'SIN     '.OR.WC.EQ.'COS     '.OR.WC.EQ.'TAN     '.OR.
     1   WC.EQ.'TANH    '.OR.WC.EQ.'SINH    '.OR.WC.EQ.'COSH    '.OR.
     1   WC.EQ.'SQRT    '.OR.WC.EQ.'ABS     '.OR.WC.EQ.'EXP     '.OR.
     1   WC.EQ.'RECIP   '.OR.WC.EQ.'MINVAL  '.OR.WC.EQ.'LOG10   '.OR.
     1   WC.EQ.'LN      '.OR.WC.EQ.'POW     '.OR.WC.EQ.'STORE   '.OR.
     1   WC.EQ.'SGN     '.OR.WC.EQ.'CLREG   '.OR.WC.EQ.'MAXVAL  '.OR.
     1   WC.EQ.'WRITE   ') THEN
                      CALL RGMATH
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                      RETURN
                      END IF
        IF(WC.EQ.'ENT'.OR.
     1  WC.EQ.'ENTI'.OR.
     2  WC.EQ.'ENTC'.OR.WC.EQ.'PULL'.OR.
     3  WC.EQ.'IPULL'.OR.WC.EQ.'CPULL'.OR.
     4  WC.EQ.'RUP'.OR.WC.EQ.'IRUP'.OR.WC
     5  .EQ.'CRUP'.OR.WC.EQ.'RDN'.OR.WC.EQ.
     6  'IRDN'.OR.WC.EQ.'CRDN'.OR.WC.EQ.'LASTX'.OR.WC.EQ.
     7  'X-Y'.OR.
     1  WC.EQ.'LASTIX'.OR.WC.EQ.'IX-IY'.OR.WC.EQ.'RE-IM'.OR.
     1  WC.EQ.'CLX'.OR.WC.EQ.'CLIX'.OR.WC.EQ.
     2  'CLSTK'.OR.WC.EQ.'CLSTKI'.OR.WC.EQ.'CLSTKC'.OR.WC.EQ.
     3  '+'.OR.WC.EQ.'-'.OR.WC.EQ.'*'.OR.WC.EQ.'/'.OR.WC.EQ.
     5  'C+'.OR.WC.EQ.'C-'.OR.WC.EQ.'C*'.OR.WC.EQ.'C/'.OR.WC.EQ.
     6  'I+'.OR.WC.EQ.'I-'.OR.WC.EQ.'I*'.OR.WC.EQ.'I/'.OR.
     7  WC.EQ.'Y**X'.OR.WC.EQ.'CY**CX'.OR.
     8  WC.EQ.'IY**IX') THEN
        CALL STACK
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
      IF(WC.EQ.'INCR    ') THEN
      CALL INCR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
      RETURN
      END IF
        IF(WC.EQ.'MOD') THEN
        CALL MMOD
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'CLASTO'.OR.WC.EQ.'ASTO'.OR.WC.EQ.'ARCL'.OR.
     1WC.EQ.'AWRITE') THEN
        CALL GPRGA
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'CLGREG'.OR.WC.EQ.'CLSTREG'
     1  .OR.WC.EQ.'STADD'.OR.WC.EQ.'STSUB'.OR.
     2  WC.EQ.'STDEV'.OR.WC.EQ.'MEAN'
     3  ) THEN
        CALL GGPREG
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'ATAN2   ') THEN
        CALL ATANN2
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'J1      ') THEN
        CALL BESS
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'PREAD   ') THEN
        CALL PREAD
      LASTCOMWRD=WC
        RETURN
        END IF
        IF(WC.EQ.'ATON') THEN
C       CONVERTS STRING TO NUMBER
        CALL MACATON
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'CLASTO'.OR.WC.EQ.'ASTO'.OR.WC.EQ.'ARCL'.OR.
     1WC.EQ.'AWRITE') THEN
        CALL GPRGA
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'R-P'.OR.WC.EQ.'P-R'.OR.WC.EQ.'R-SP'.OR.
     1  WC.EQ.'SP-R'.OR.WC.EQ.'R-CYL'.OR.WC.EQ.'CYL-R'.OR.
     2  WC.EQ.'H-HMS'.OR.WC.EQ.'HMS-H') THEN
        CALL COORD
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.'IN-MM'.OR.WC.EQ.'IN-CM'.OR.WC.EQ.'IN-M'.OR.
     1  WC.EQ.'MM-IN'.OR.WC.EQ.'CM-IN'.OR.WC.EQ.'M-IN') THEN
        CALL COORD
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
C     CRAETING A MACRO OR EDITING ONE
                        END IF
C
C
C       HERE IS WHERE WE BRANCH TO VARIOUS SUBROUTINES DEPENDING
C       UPON THE COMMAND WORD.
C
C       INITIALIZE MACYES TO ZERO (NOT A MACRO)
C
                MACYES=0
C
      IF(F27.EQ.0.OR.F53.EQ.0.OR.F54.EQ.0) THEN
                FNYES=.FALSE.
                IF(WC.EQ.'FUN01') FNYES=.TRUE.
                IF(WC.EQ.'FUN02') FNYES=.TRUE.
                IF(WC.EQ.'FUN03') FNYES=.TRUE.
                IF(WC.EQ.'FUN04') FNYES=.TRUE.
                IF(WC.EQ.'FUN05') FNYES=.TRUE.
                IF(WC.EQ.'FUN06') FNYES=.TRUE.
                IF(WC.EQ.'FUN07') FNYES=.TRUE.
                IF(WC.EQ.'FUN08') FNYES=.TRUE.
                IF(WC.EQ.'FUN09') FNYES=.TRUE.
                IF(WC.EQ.'FUN10') FNYES=.TRUE.
                END IF
C
C     LOOK FOR RENAMED FUNCTIONS
              IF(WC.EQ.FNAMED(1))  WC='FUN01'
              IF(WC.EQ.FNAMED(2))  WC='FUN02'
              IF(WC.EQ.FNAMED(3))  WC='FUN03'
              IF(WC.EQ.FNAMED(4))  WC='FUN04'
              IF(WC.EQ.FNAMED(5))  WC='FUN05'
              IF(WC.EQ.FNAMED(6))  WC='FUN06'
              IF(WC.EQ.FNAMED(7))  WC='FUN07'
              IF(WC.EQ.FNAMED(8))  WC='FUN08'
              IF(WC.EQ.FNAMED(9))  WC='FUN09'
              IF(WC.EQ.FNAMED(10)) WC='FUN10'
C
C     IF F27 IS 1 OR 2 WE ARE WORKING THE MERIT FUNCTION
C     IF NOT A SPECIFIC ALLOWED COMMAND, INSERT FUNC01 FOR
C     WC AND MAKE OLD WC THE NEW WQ
                IF(F27.EQ.1.OR.F27.EQ.2) THEN
                NOGO=.FALSE.
                OHFUN=.FALSE.
                IF(WC.EQ.'FUNC01  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC02  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC03  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC04  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC05  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC06  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC07  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC08  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC09  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC10  ')   NOGO=.TRUE.
                IF(WC.EQ.'DEL     ')   NOGO=.TRUE.
                IF(WC.EQ.'COR     ')   NOGO=.TRUE.
                IF(WC.EQ.'BYP     ')   NOGO=.TRUE.
                IF(WC.EQ.'BLO     ')   NOGO=.TRUE.
                IF(WC.EQ.'BHI     ')   NOGO=.TRUE.
                IF(WC.EQ.'GTE     ')   NOGO=.TRUE.
                IF(WC.EQ.'LTE     ')   NOGO=.TRUE.
                IF(WC.EQ.'HLD     ')   NOGO=.TRUE.
                IF(WC.EQ.'GET     ')   NOGO=.TRUE.
                IF(WC.EQ.'AGET    ')   NOGO=.TRUE.
                IF(WC.EQ.'RCL     ')   NOGO=.TRUE.
                IF(WC.EQ.'STOK    ')   NOGO=.TRUE.
                IF(WC.EQ.'WRITE   ')   NOGO=.TRUE.
                IF(WC.EQ.'MAXVAL  ')   NOGO=.TRUE.
                IF(WC.EQ.'MINVAL  ')   NOGO=.TRUE.
                IF(WC.EQ.'PI      ')   NOGO=.TRUE.
                IF(WC.EQ.'CHS     ')   NOGO=.TRUE.
                IF(WC.EQ.'MOVE    ')   NOGO=.TRUE.
                IF(WC.EQ.'LASTX   ')   NOGO=.TRUE.
                IF(WC.EQ.'LASTIX  ')   NOGO=.TRUE.
                IF(WC.EQ.'INCR    ')   NOGO=.TRUE.
                IF(WC.EQ.'PLUS    ')   NOGO=.TRUE.
                IF(WC.EQ.'MINUS   ')   NOGO=.TRUE.
                IF(WC.EQ.'MPY     ')   NOGO=.TRUE.
                IF(WC.EQ.'DIV     ')   NOGO=.TRUE.
                IF(WC.EQ.'SQRT    ')   NOGO=.TRUE.
                IF(WC.EQ.'SIN     ')   NOGO=.TRUE.
                IF(WC.EQ.'COS     ')   NOGO=.TRUE.
                IF(WC.EQ.'TAN     ')   NOGO=.TRUE.
                IF(WC.EQ.'SINH    ')   NOGO=.TRUE.
                IF(WC.EQ.'COSH    ')   NOGO=.TRUE.
                IF(WC.EQ.'TANH    ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTK   ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTKI  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTKC  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLREG   ')   NOGO=.TRUE.
                IF(WC.EQ.'CLGREG  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLX     ')   NOGO=.TRUE.
                IF(WC.EQ.'CLIX    ')   NOGO=.TRUE.
                IF(WC.EQ.'ASIN    ')   NOGO=.TRUE.
                IF(WC.EQ.'ACOS    ')   NOGO=.TRUE.
                IF(WC.EQ.'ATAN    ')   NOGO=.TRUE.
                IF(WC.EQ.'ABS     ')   NOGO=.TRUE.
                IF(WC.EQ.'EXP     ')   NOGO=.TRUE.
                IF(WC.EQ.'LN      ')   NOGO=.TRUE.
                IF(WC.EQ.'LOG10   ')   NOGO=.TRUE.
                IF(WC.EQ.'FACT    ')   NOGO=.TRUE.
                IF(WC.EQ.'SGN     ')   NOGO=.TRUE.
                IF(WC.EQ.'RECIP   ')   NOGO=.TRUE.
                IF(WC.EQ.'INTGR   ')   NOGO=.TRUE.
                IF(WC.EQ.'FRAC    ')   NOGO=.TRUE.
                IF(WC.EQ.'POW     ')   NOGO=.TRUE.
                IF(WC.EQ.'STORE   ')   NOGO=.TRUE.
                IF(WC.EQ.'RAND    ')   NOGO=.TRUE.
                IF(WC.EQ.'MOD     ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTREG ')   NOGO=.TRUE.
                IF(WC.EQ.'STADD   ')   NOGO=.TRUE.
                IF(WC.EQ.'STSUB   ')   NOGO=.TRUE.
                IF(WC.EQ.'MEAN    ')   NOGO=.TRUE.
                IF(WC.EQ.'STDEV   ')   NOGO=.TRUE.
                IF(WC.EQ.'RTD     ')   NOGO=.TRUE.
                IF(WC.EQ.'DTR     ')   NOGO=.TRUE.
                IF(WC.EQ.'ATAN2   ')   NOGO=.TRUE.
                IF(WC.EQ.'J1      ')   NOGO=.TRUE.
                IF(WC.EQ.'PREAD   ')   NOGO=.TRUE.
                IF(WC.EQ.'ATON    ')   NOGO=.TRUE.
                IF(WC.EQ.'STOAX   ')   NOGO=.TRUE.
                IF(WC.EQ.'ARCL    ')   NOGO=.TRUE.
                IF(WC.EQ.'AWRITE  ')   NOGO=.TRUE.
                IF(WC.EQ.'ASTO    ')   NOGO=.TRUE.
                IF(WC.EQ.'CLASTO  ')   NOGO=.TRUE.
                IF(WC.EQ.'SHOW    ')   NOGO=.TRUE.
                IF(WC.EQ.'ENT     ')   NOGO=.TRUE.
                IF(WC.EQ.'ENTI    ')   NOGO=.TRUE.
                IF(WC.EQ.'ENTC    ')   NOGO=.TRUE.
                IF(WC.EQ.'PULL    ')   NOGO=.TRUE.
                IF(WC.EQ.'IPULL   ')   NOGO=.TRUE.
                IF(WC.EQ.'CPULL   ')   NOGO=.TRUE.
                IF(WC.EQ.'RUP     ')   NOGO=.TRUE.
                IF(WC.EQ.'IRUP    ')   NOGO=.TRUE.
                IF(WC.EQ.'CRUP    ')   NOGO=.TRUE.
                IF(WC.EQ.'RDN     ')   NOGO=.TRUE.
                IF(WC.EQ.'IRDN    ')   NOGO=.TRUE.
                IF(WC.EQ.'CRDN    ')   NOGO=.TRUE.
                IF(WC.EQ.'X-Y     ')   NOGO=.TRUE.
                IF(WC.EQ.'IX-IY   ')   NOGO=.TRUE.
                IF(WC.EQ.'+       ')   NOGO=.TRUE.
                IF(WC.EQ.'-       ')   NOGO=.TRUE.
                IF(WC.EQ.'*       ')   NOGO=.TRUE.
                IF(WC.EQ.'/       ')   NOGO=.TRUE.
                IF(WC.EQ.'I+      ')   NOGO=.TRUE.
                IF(WC.EQ.'I-      ')   NOGO=.TRUE.
                IF(WC.EQ.'I*      ')   NOGO=.TRUE.
                IF(WC.EQ.'I/      ')   NOGO=.TRUE.
                IF(WC.EQ.'C+      ')   NOGO=.TRUE.
                IF(WC.EQ.'C-      ')   NOGO=.TRUE.
                IF(WC.EQ.'C*      ')   NOGO=.TRUE.
                IF(WC.EQ.'C/      ')   NOGO=.TRUE.
                IF(WC.EQ.'Y**X    ')   NOGO=.TRUE.
                IF(WC.EQ.'IY**IX  ')   NOGO=.TRUE.
                IF(WC.EQ.'CY**CX  ')   NOGO=.TRUE.
                IF(WC.EQ.'P-R     ')   NOGO=.TRUE.
                IF(WC.EQ.'R-P     ')   NOGO=.TRUE.
                IF(WC.EQ.'CYL-R   ')   NOGO=.TRUE.
                IF(WC.EQ.'R-CYL   ')   NOGO=.TRUE.
                IF(WC.EQ.'SP-R    ')   NOGO=.TRUE.
                IF(WC.EQ.'R-SP    ')   NOGO=.TRUE.
                IF(WC.EQ.'RE-IM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IM-RE   ')   NOGO=.TRUE.
                IF(WC.EQ.'H-HMS   ')   NOGO=.TRUE.
                IF(WC.EQ.'HMS-H   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-MM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-CM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-M    ')   NOGO=.TRUE.
                IF(WC.EQ.'MM-IN   ')   NOGO=.TRUE.
                IF(WC.EQ.'CM-IN   ')   NOGO=.TRUE.
                IF(WC.EQ.'M-IN    ')   NOGO=.TRUE.
                IF(WC.EQ.'SET     ')   NOGO=.TRUE.
                IF(WC.EQ.'W1      ')   NOGO=.TRUE.
                IF(WC.EQ.'W2      ')   NOGO=.TRUE.
                IF(WC.EQ.'W3      ')   NOGO=.TRUE.
                IF(WC.EQ.'W4      ')   NOGO=.TRUE.
                IF(WC.EQ.'W5      ')   NOGO=.TRUE.
                IF(WC.EQ.'MR      ')   NOGO=.TRUE.
                IF(WC.EQ.'MRA     ')   NOGO=.TRUE.
                IF(WC.EQ.'OP      ')   NOGO=.TRUE.
                IF(WC.EQ.'OPA     ')   NOGO=.TRUE.
                IF(WC.EQ.'CFG     ')   NOGO=.TRUE.
                IF(WC.EQ.'C       ')   NOGO=.TRUE.
                IF(WC.EQ.'M       ')   NOGO=.TRUE.
                IF(WC.EQ.'EOS     ')   NOGO=.TRUE.
                IF(WC.EQ.'?       ')   NOGO=.TRUE.
                IF(WC.EQ.'        ')   NOGO=.TRUE.
                IF(WC.EQ.'OP_DESC ')   NOGO=.TRUE.
C     PREDEFINED OPERAND NAMES SET OHFUN TO TRUE
                IF(WC.EQ.'MACOPT  ')   OHFUN=.TRUE.
                IF(WC.EQ.'X       ')   OHFUN=.TRUE.
                IF(WC.EQ.'Y       ')   OHFUN=.TRUE.
                IF(WC.EQ.'Z       ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCL     ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCM     ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCN     ')   OHFUN=.TRUE.
                IF(WC.EQ.'K       ')   OHFUN=.TRUE.
                IF(WC.EQ.'L       ')   OHFUN=.TRUE.
                IF(WC.EQ.'M       ')   OHFUN=.TRUE.
      IF(WC.EQ.'DX      ') OHFUN=.TRUE.
      IF(WC.EQ.'DY      ') OHFUN=.TRUE.
      IF(WC.EQ.'DR      ') OHFUN=.TRUE.
      IF(WC.EQ.'DXA     ') OHFUN=.TRUE.
      IF(WC.EQ.'DYA     ') OHFUN=.TRUE.
      IF(WC.EQ.'DRA     ') OHFUN=.TRUE.
      IF(WC.EQ.'XANG    ') OHFUN=.TRUE.
      IF(WC.EQ.'YANG    ') OHFUN=.TRUE.
      IF(WC.EQ.'OPL     ') OHFUN=.TRUE.
      IF(WC.EQ.'OPD     ') OHFUN=.TRUE.
      IF(WC.EQ.'OPDW    ') OHFUN=.TRUE.
      IF(WC.EQ.'LOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'MOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'NOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'LEN     ') OHFUN=.TRUE.
      IF(WC.EQ.'AII     ') OHFUN=.TRUE.
      IF(WC.EQ.'AIP     ') OHFUN=.TRUE.
      IF(WC.EQ.'LN      ') OHFUN=.TRUE.
      IF(WC.EQ.'MN      ') OHFUN=.TRUE.
      IF(WC.EQ.'NN      ') OHFUN=.TRUE.
      IF(WC.EQ.'PXPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PXPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PYPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PYPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PXAPX   ') OHFUN=.TRUE.
      IF(WC.EQ.'PXAPY   ') OHFUN=.TRUE.
      IF(WC.EQ.'PYAPX   ') OHFUN=.TRUE.
      IF(WC.EQ.'PYAPY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DXDX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DXDY    ') OHFUN=.TRUE.
      IF(WC.EQ.'DYDX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DYDY    ') OHFUN=.TRUE.
      IF(WC.EQ.'DXADX   ') OHFUN=.TRUE.
      IF(WC.EQ.'DXADY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DYADX   ') OHFUN=.TRUE.
      IF(WC.EQ.'DYADY   ') OHFUN=.TRUE.
      IF(WC.EQ.'XREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'YREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'ZREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'LREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'MACOPT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'NREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'LREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'MREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'NREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'IREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'IPREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'YAREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'LNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'MNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'NNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'GLX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLZ     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLL     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLM     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLN     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLLOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'GLMOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'GLNOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'LENREF  ') OHFUN=.TRUE.
      IF(WC.EQ.'OPLREF  ') OHFUN=.TRUE.
      IF(WC.EQ.'RD      ') OHFUN=.TRUE.
      IF(WC.EQ.'CV      ') OHFUN=.TRUE.
      IF(WC.EQ.'TH      ') OHFUN=.TRUE.
      IF(WC.EQ.'THM     ') OHFUN=.TRUE.
      IF(WC.EQ.'PRICE   ') OHFUN=.TRUE.
      IF(WC.EQ.'CC      ') OHFUN=.TRUE.
      IF(WC.EQ.'AC      ') OHFUN=.TRUE.
      IF(WC.EQ.'AD      ') OHFUN=.TRUE.
      IF(WC.EQ.'AE      ') OHFUN=.TRUE.
      IF(WC.EQ.'AF      ') OHFUN=.TRUE.
      IF(WC.EQ.'AG      ') OHFUN=.TRUE.
      IF(WC.EQ.'AH      ') OHFUN=.TRUE.
      IF(WC.EQ.'AI      ') OHFUN=.TRUE.
      IF(WC.EQ.'AJ      ') OHFUN=.TRUE.
      IF(WC.EQ.'AK      ') OHFUN=.TRUE.
      IF(WC.EQ.'AL      ') OHFUN=.TRUE.
      IF(WC.EQ.'RDTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'CVTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'GRS     ') OHFUN=.TRUE.
      IF(WC.EQ.'CCTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'ADTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AETOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AFTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AGTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'ALPHA   ') OHFUN=.TRUE.
      IF(WC.EQ.'BETA    ') OHFUN=.TRUE.
      IF(WC.EQ.'GAMMA   ') OHFUN=.TRUE.
      IF(WC.EQ.'GDX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GDY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GDZ     ') OHFUN=.TRUE.
      IF(WC.EQ.'GALPHA  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBETA   ') OHFUN=.TRUE.
      IF(WC.EQ.'GGAMMA  ') OHFUN=.TRUE.
      IF(WC.EQ.'VNUM    ') OHFUN=.TRUE.
      IF(WC.EQ.'DPART   ') OHFUN=.TRUE.
      IF(WC.EQ.'ABBE    ') OHFUN=.TRUE.
      IF(WC.EQ.'PARTL   ') OHFUN=.TRUE.
      IF(WC.EQ.'INDEX   ') OHFUN=.TRUE.
      IF(WC.EQ.'N1      ') OHFUN=.TRUE.
      IF(WC.EQ.'N2      ') OHFUN=.TRUE.
      IF(WC.EQ.'N3      ') OHFUN=.TRUE.
      IF(WC.EQ.'N4      ') OHFUN=.TRUE.
      IF(WC.EQ.'N5      ') OHFUN=.TRUE.
      IF(WC.EQ.'N6      ') OHFUN=.TRUE.
      IF(WC.EQ.'N7      ') OHFUN=.TRUE.
      IF(WC.EQ.'N8      ') OHFUN=.TRUE.
      IF(WC.EQ.'N9      ') OHFUN=.TRUE.
      IF(WC.EQ.'N10     ') OHFUN=.TRUE.
      IF(WC.EQ.'XD      ') OHFUN=.TRUE.
      IF(WC.EQ.'YD      ') OHFUN=.TRUE.
      IF(WC.EQ.'ZD      ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'XVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'YVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'ZVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'LXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LENGTH  ') OHFUN=.TRUE.
      IF(WC.EQ.'OAL     ') OHFUN=.TRUE.
      IF(WC.EQ.'MLENGTH ') OHFUN=.TRUE.
      IF(WC.EQ.'OPTLEN  ') OHFUN=.TRUE.
      IF(WC.EQ.'WEIGHT  ') OHFUN=.TRUE.
      IF(WC.EQ.'ET      ') OHFUN=.TRUE.
      IF(WC.EQ.'ETY     ') OHFUN=.TRUE.
      IF(WC.EQ.'ETX     ') OHFUN=.TRUE.
      IF(WC.EQ.'SHAPEFAC') OHFUN=.TRUE.
      IF(WC.EQ.'C1      ') OHFUN=.TRUE.
      IF(WC.EQ.'C2      ') OHFUN=.TRUE.
      IF(WC.EQ.'C3      ') OHFUN=.TRUE.
      IF(WC.EQ.'C4      ') OHFUN=.TRUE.
      IF(WC.EQ.'C5      ') OHFUN=.TRUE.
      IF(WC.EQ.'C6      ') OHFUN=.TRUE.
      IF(WC.EQ.'C7      ') OHFUN=.TRUE.
      IF(WC.EQ.'C8      ') OHFUN=.TRUE.
      IF(WC.EQ.'C9      ') OHFUN=.TRUE.
      IF(WC.EQ.'C10     ') OHFUN=.TRUE.
      IF(WC.EQ.'C11     ') OHFUN=.TRUE.
      IF(WC.EQ.'C12     ') OHFUN=.TRUE.
      IF(WC.EQ.'C13     ') OHFUN=.TRUE.
      IF(WC.EQ.'C14     ') OHFUN=.TRUE.
      IF(WC.EQ.'C15     ') OHFUN=.TRUE.
      IF(WC.EQ.'C16     ') OHFUN=.TRUE.
      IF(WC.EQ.'C17     ') OHFUN=.TRUE.
      IF(WC.EQ.'C18     ') OHFUN=.TRUE.
      IF(WC.EQ.'C19     ') OHFUN=.TRUE.
      IF(WC.EQ.'C20     ') OHFUN=.TRUE.
      IF(WC.EQ.'C21     ') OHFUN=.TRUE.
      IF(WC.EQ.'C22     ') OHFUN=.TRUE.
      IF(WC.EQ.'C23     ') OHFUN=.TRUE.
      IF(WC.EQ.'C24     ') OHFUN=.TRUE.
      IF(WC.EQ.'C25     ') OHFUN=.TRUE.
      IF(WC.EQ.'C26     ') OHFUN=.TRUE.
      IF(WC.EQ.'C27     ') OHFUN=.TRUE.
      IF(WC.EQ.'C28     ') OHFUN=.TRUE.
      IF(WC.EQ.'C29     ') OHFUN=.TRUE.
      IF(WC.EQ.'C30     ') OHFUN=.TRUE.
      IF(WC.EQ.'C31     ') OHFUN=.TRUE.
      IF(WC.EQ.'C32     ') OHFUN=.TRUE.
      IF(WC.EQ.'C33     ') OHFUN=.TRUE.
      IF(WC.EQ.'C34     ') OHFUN=.TRUE.
      IF(WC.EQ.'C35     ') OHFUN=.TRUE.
      IF(WC.EQ.'C36     ') OHFUN=.TRUE.
      IF(WC.EQ.'C37     ') OHFUN=.TRUE.
      IF(WC.EQ.'C38     ') OHFUN=.TRUE.
      IF(WC.EQ.'C39     ') OHFUN=.TRUE.
      IF(WC.EQ.'C40     ') OHFUN=.TRUE.
      IF(WC.EQ.'C41     ') OHFUN=.TRUE.
      IF(WC.EQ.'C42     ') OHFUN=.TRUE.
      IF(WC.EQ.'C43     ') OHFUN=.TRUE.
      IF(WC.EQ.'C44     ') OHFUN=.TRUE.
      IF(WC.EQ.'C45     ') OHFUN=.TRUE.
      IF(WC.EQ.'C46     ') OHFUN=.TRUE.
      IF(WC.EQ.'C47     ') OHFUN=.TRUE.
      IF(WC.EQ.'C48     ') OHFUN=.TRUE.
      IF(WC.EQ.'C49     ') OHFUN=.TRUE.
      IF(WC.EQ.'C50     ') OHFUN=.TRUE.
      IF(WC.EQ.'C51     ') OHFUN=.TRUE.
      IF(WC.EQ.'C52     ') OHFUN=.TRUE.
      IF(WC.EQ.'C53     ') OHFUN=.TRUE.
      IF(WC.EQ.'C54     ') OHFUN=.TRUE.
      IF(WC.EQ.'C55     ') OHFUN=.TRUE.
      IF(WC.EQ.'C56     ') OHFUN=.TRUE.
      IF(WC.EQ.'C57     ') OHFUN=.TRUE.
      IF(WC.EQ.'C58     ') OHFUN=.TRUE.
      IF(WC.EQ.'C59     ') OHFUN=.TRUE.
      IF(WC.EQ.'C60     ') OHFUN=.TRUE.
      IF(WC.EQ.'C61     ') OHFUN=.TRUE.
      IF(WC.EQ.'C62     ') OHFUN=.TRUE.
      IF(WC.EQ.'C63     ') OHFUN=.TRUE.
      IF(WC.EQ.'C64     ') OHFUN=.TRUE.
      IF(WC.EQ.'C65     ') OHFUN=.TRUE.
      IF(WC.EQ.'C66     ') OHFUN=.TRUE.
      IF(WC.EQ.'C67     ') OHFUN=.TRUE.
      IF(WC.EQ.'C68     ') OHFUN=.TRUE.
      IF(WC.EQ.'C69     ') OHFUN=.TRUE.
      IF(WC.EQ.'C70     ') OHFUN=.TRUE.
      IF(WC.EQ.'C71     ') OHFUN=.TRUE.
      IF(WC.EQ.'C72     ') OHFUN=.TRUE.
      IF(WC.EQ.'C73     ') OHFUN=.TRUE.
      IF(WC.EQ.'C74     ') OHFUN=.TRUE.
      IF(WC.EQ.'C75     ') OHFUN=.TRUE.
      IF(WC.EQ.'C76     ') OHFUN=.TRUE.
      IF(WC.EQ.'C77     ') OHFUN=.TRUE.
      IF(WC.EQ.'C78     ') OHFUN=.TRUE.
      IF(WC.EQ.'C79     ') OHFUN=.TRUE.
      IF(WC.EQ.'C80     ') OHFUN=.TRUE.
      IF(WC.EQ.'C81     ') OHFUN=.TRUE.
      IF(WC.EQ.'C82     ') OHFUN=.TRUE.
      IF(WC.EQ.'C83     ') OHFUN=.TRUE.
      IF(WC.EQ.'C84     ') OHFUN=.TRUE.
      IF(WC.EQ.'C85     ') OHFUN=.TRUE.
      IF(WC.EQ.'C86     ') OHFUN=.TRUE.
      IF(WC.EQ.'C87     ') OHFUN=.TRUE.
      IF(WC.EQ.'C88     ') OHFUN=.TRUE.
      IF(WC.EQ.'C89     ') OHFUN=.TRUE.
      IF(WC.EQ.'C90     ') OHFUN=.TRUE.
      IF(WC.EQ.'C91     ') OHFUN=.TRUE.
      IF(WC.EQ.'C92     ') OHFUN=.TRUE.
      IF(WC.EQ.'C93     ') OHFUN=.TRUE.
      IF(WC.EQ.'C94     ') OHFUN=.TRUE.
      IF(WC.EQ.'C95     ') OHFUN=.TRUE.
      IF(WC.EQ.'C96     ') OHFUN=.TRUE.
      IF(WC.EQ.'PWRY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PWRX    ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTHY ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTHX ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTH  ') OHFUN=.TRUE.
      IF(WC.EQ.'REDSLV')   OHFUN=.TRUE.
      IF(WC.EQ.'PY      ') OHFUN=.TRUE.
      IF(WC.EQ.'PX      ') OHFUN=.TRUE.
      IF(WC.EQ.'PCY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PCX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PUCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PIX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PICY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PICX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIYP    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIXP    ') OHFUN=.TRUE.
      IF(WC.EQ.'PICYP   ') OHFUN=.TRUE.
      IF(WC.EQ.'PICXP   ') OHFUN=.TRUE.
      IF(WC.EQ.'PACY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'IMDISX  ') OHFUN=.TRUE.
      IF(WC.EQ.'IMDISY  ') OHFUN=.TRUE.
      IF(WC.EQ.'CENTX   ') OHFUN=.TRUE.
      IF(WC.EQ.'CENTY   ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMS     ') OHFUN=.TRUE.
      IF(WC.EQ.'RSSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'RSSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'RSS     ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSOPD  ') OHFUN=.TRUE.
      IF(WC.EQ.'ZERN37  ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGX    ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGY    ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGXOR  ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGYOR  ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'FFNX    ') OHFUN=.TRUE.
      IF(WC.EQ.'FFNY    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFNX    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFNY    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'ENDIAX  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENDIAY  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXDIAX  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXDIAY  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSX  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSY  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSZ  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSX  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSY  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSZ  ') OHFUN=.TRUE.
      IF(WC.EQ.'FNUMX   ') OHFUN=.TRUE.
      IF(WC.EQ.'FNUMY   ') OHFUN=.TRUE.
      IF(WC.EQ.'OBFNUMX ') OHFUN=.TRUE.
      IF(WC.EQ.'ONFNUMY ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDISX ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDISY ') OHFUN=.TRUE.
      IF(WC.EQ.'CHFIMX  ') OHFUN=.TRUE.
      IF(WC.EQ.'CHFIMY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GPX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GPY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUX    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUY    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUCX   ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUCY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIST    ') OHFUN=.TRUE.
      IF(WC.EQ.'FISHDIST') OHFUN=.TRUE.
      IF(WC.EQ.'XFOC    ') OHFUN=.TRUE.
      IF(WC.EQ.'YFOC    ') OHFUN=.TRUE.
      IF(WC.EQ.'AST     ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3   ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5    ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5   ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5   ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5   ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSA  ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSA  ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMA  ') OHFUN=.TRUE.
      IF(WC.EQ.'TAS     ') OHFUN=.TRUE.
      IF(WC.EQ.'XTAS    ') OHFUN=.TRUE.
      IF(WC.EQ.'SAS     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSAS    ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7    ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAP ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAP ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAP ') OHFUN=.TRUE.
      IF(WC.EQ.'TASP    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASP   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASP    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASP   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7P   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAS ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAS ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAS ') OHFUN=.TRUE.
      IF(WC.EQ.'TASS    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASS   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASS    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASS   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7S   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5I    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAI  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAI ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAI ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAI  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAI ') OHFUN=.TRUE.
      IF(WC.EQ.'TASI    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASI   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASI    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASI   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7I    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7I   ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZCV   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZCV  ') OHFUN=.TRUE.
      IF(WC.EQ.'MGOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'PGOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'MDOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'PDOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'GOTFM   ') OHFUN=.TRUE.
      IF(WC.EQ.'GOTFP   ') OHFUN=.TRUE.
      IF(WC.EQ.'DOTFM  ') OHFUN=.TRUE.
      IF(WC.EQ.'DOTFP  ') OHFUN=.TRUE.
      IF(WC.EQ.'REDK     ') OHFUN=.TRUE.
      IF(WC.EQ.'REDCEN  ') OHFUN=.TRUE.
      IF(WC.EQ.'SYMX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SYMY    ') OHFUN=.TRUE.
      IF(WC.EQ.'ASYMX   ') OHFUN=.TRUE.
      IF(WC.EQ.'ASYMY   ') OHFUN=.TRUE.
      IF(WC.EQ.'CTSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'CTSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SCEX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SCEY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACM    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACM    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCM    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCM    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'GREYS   ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRADX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRADY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBDISX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBDISY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRCVX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRCVY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBWAISTX') OHFUN=.TRUE.
      IF(WC.EQ.'GBWAISTY') OHFUN=.TRUE.
      IF(WC.EQ.'CLPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'CLPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DMINUSD ') OHFUN=.TRUE.
      IF(WC.EQ.'COST    ') OHFUN=.TRUE.
      IF(WC.EQ.'ACT     ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSYX   ') OHFUN=.TRUE.
      IF(WC.EQ.'CLEARX  ') OHFUN=.TRUE.
      IF(WC.EQ.'CLEARY  ') OHFUN=.TRUE.
                IF(.NOT.NOGO.AND..NOT.OHFUN) THEN
      OUTLYNE='INVALID OPERAND ENTRY'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                       RETURN
                        END IF
                IF(.NOT.NOGO.AND.OHFUN) THEN
                WQ=WC
                SQ=1
                WC='FUNC00'
C               NO REPLACEMENT DONE
                NOGO=.FALSE.
                OHFUN=.FALSE.
                        END IF
C     NOT IN MERIT OR UPDATE MERIT
                END IF
C     IF F53 IS 1 OR 2 WE ARE WORKING THE TOPER FUNCTION
C     IF NOT A SPECIFIC ALLOWED COMMAND, INSERT FUNC01 FOR
C     WC AND MAKE OLD WC THE NEW WQ
                IF(F53.EQ.1.OR.F53.EQ.2) THEN
                NOGO=.FALSE.
                OHFUN=.FALSE.
                IF(WC.EQ.'FUNC01  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC02  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC03  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC04  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC05  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC06  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC07  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC08  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC09  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC10  ')   NOGO=.TRUE.
                IF(WC.EQ.'DEL     ')   NOGO=.TRUE.
                IF(WC.EQ.'GET     ')   NOGO=.TRUE.
                IF(WC.EQ.'AGET    ')   NOGO=.TRUE.
                IF(WC.EQ.'RCL     ')   NOGO=.TRUE.
                IF(WC.EQ.'STOK    ')   NOGO=.TRUE.
                IF(WC.EQ.'WRITE   ')   NOGO=.TRUE.
                IF(WC.EQ.'MAXVAL  ')   NOGO=.TRUE.
                IF(WC.EQ.'MINVAL  ')   NOGO=.TRUE.
                IF(WC.EQ.'PI      ')   NOGO=.TRUE.
                IF(WC.EQ.'CHS     ')   NOGO=.TRUE.
                IF(WC.EQ.'MOVE    ')   NOGO=.TRUE.
                IF(WC.EQ.'LASTX   ')   NOGO=.TRUE.
                IF(WC.EQ.'LASTIX  ')   NOGO=.TRUE.
                IF(WC.EQ.'INCR    ')   NOGO=.TRUE.
                IF(WC.EQ.'PLUS    ')   NOGO=.TRUE.
                IF(WC.EQ.'MINUS   ')   NOGO=.TRUE.
                IF(WC.EQ.'MPY     ')   NOGO=.TRUE.
                IF(WC.EQ.'DIV     ')   NOGO=.TRUE.
                IF(WC.EQ.'SQRT    ')   NOGO=.TRUE.
                IF(WC.EQ.'SIN     ')   NOGO=.TRUE.
                IF(WC.EQ.'COS     ')   NOGO=.TRUE.
                IF(WC.EQ.'TAN     ')   NOGO=.TRUE.
                IF(WC.EQ.'SINH    ')   NOGO=.TRUE.
                IF(WC.EQ.'COSH    ')   NOGO=.TRUE.
                IF(WC.EQ.'TANH    ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTK   ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTKI  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTKC  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLREG   ')   NOGO=.TRUE.
                IF(WC.EQ.'CLGREG  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLX     ')   NOGO=.TRUE.
                IF(WC.EQ.'CLIX    ')   NOGO=.TRUE.
                IF(WC.EQ.'ASIN    ')   NOGO=.TRUE.
                IF(WC.EQ.'ACOS    ')   NOGO=.TRUE.
                IF(WC.EQ.'ATAN    ')   NOGO=.TRUE.
                IF(WC.EQ.'ABS     ')   NOGO=.TRUE.
                IF(WC.EQ.'EXP     ')   NOGO=.TRUE.
                IF(WC.EQ.'LN      ')   NOGO=.TRUE.
                IF(WC.EQ.'LOG10   ')   NOGO=.TRUE.
                IF(WC.EQ.'FACT    ')   NOGO=.TRUE.
                IF(WC.EQ.'SGN     ')   NOGO=.TRUE.
                IF(WC.EQ.'RECIP   ')   NOGO=.TRUE.
                IF(WC.EQ.'INTGR   ')   NOGO=.TRUE.
                IF(WC.EQ.'FRAC    ')   NOGO=.TRUE.
                IF(WC.EQ.'POW     ')   NOGO=.TRUE.
                IF(WC.EQ.'STORE   ')   NOGO=.TRUE.
                IF(WC.EQ.'RAND    ')   NOGO=.TRUE.
                IF(WC.EQ.'MOD     ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTREG ')   NOGO=.TRUE.
                IF(WC.EQ.'STADD   ')   NOGO=.TRUE.
                IF(WC.EQ.'STSUB   ')   NOGO=.TRUE.
                IF(WC.EQ.'MEAN    ')   NOGO=.TRUE.
                IF(WC.EQ.'STDEV   ')   NOGO=.TRUE.
                IF(WC.EQ.'RTD     ')   NOGO=.TRUE.
                IF(WC.EQ.'DTR     ')   NOGO=.TRUE.
                IF(WC.EQ.'ATAN2   ')   NOGO=.TRUE.
                IF(WC.EQ.'J1      ')   NOGO=.TRUE.
                IF(WC.EQ.'PREAD   ')   NOGO=.TRUE.
                IF(WC.EQ.'ATON    ')   NOGO=.TRUE.
                IF(WC.EQ.'STOAX   ')   NOGO=.TRUE.
                IF(WC.EQ.'ARCL    ')   NOGO=.TRUE.
                IF(WC.EQ.'AWRITE  ')   NOGO=.TRUE.
                IF(WC.EQ.'ASTO    ')   NOGO=.TRUE.
                IF(WC.EQ.'CLASTO  ')   NOGO=.TRUE.
                IF(WC.EQ.'SHOW    ')   NOGO=.TRUE.
                IF(WC.EQ.'ENT     ')   NOGO=.TRUE.
                IF(WC.EQ.'ENTI    ')   NOGO=.TRUE.
                IF(WC.EQ.'ENTC    ')   NOGO=.TRUE.
                IF(WC.EQ.'PULL    ')   NOGO=.TRUE.
                IF(WC.EQ.'IPULL   ')   NOGO=.TRUE.
                IF(WC.EQ.'CPULL   ')   NOGO=.TRUE.
                IF(WC.EQ.'RUP     ')   NOGO=.TRUE.
                IF(WC.EQ.'IRUP    ')   NOGO=.TRUE.
                IF(WC.EQ.'CRUP    ')   NOGO=.TRUE.
                IF(WC.EQ.'RDN     ')   NOGO=.TRUE.
                IF(WC.EQ.'IRDN    ')   NOGO=.TRUE.
                IF(WC.EQ.'CRDN    ')   NOGO=.TRUE.
                IF(WC.EQ.'X-Y     ')   NOGO=.TRUE.
                IF(WC.EQ.'IX-IY   ')   NOGO=.TRUE.
                IF(WC.EQ.'+       ')   NOGO=.TRUE.
                IF(WC.EQ.'-       ')   NOGO=.TRUE.
                IF(WC.EQ.'*       ')   NOGO=.TRUE.
                IF(WC.EQ.'/       ')   NOGO=.TRUE.
                IF(WC.EQ.'I+      ')   NOGO=.TRUE.
                IF(WC.EQ.'I-      ')   NOGO=.TRUE.
                IF(WC.EQ.'I*      ')   NOGO=.TRUE.
                IF(WC.EQ.'I/      ')   NOGO=.TRUE.
                IF(WC.EQ.'C+      ')   NOGO=.TRUE.
                IF(WC.EQ.'C-      ')   NOGO=.TRUE.
                IF(WC.EQ.'C*      ')   NOGO=.TRUE.
                IF(WC.EQ.'C/      ')   NOGO=.TRUE.
                IF(WC.EQ.'Y**X    ')   NOGO=.TRUE.
                IF(WC.EQ.'IY**IX  ')   NOGO=.TRUE.
                IF(WC.EQ.'CY**CX  ')   NOGO=.TRUE.
                IF(WC.EQ.'P-R     ')   NOGO=.TRUE.
                IF(WC.EQ.'R-P     ')   NOGO=.TRUE.
                IF(WC.EQ.'CYL-R   ')   NOGO=.TRUE.
                IF(WC.EQ.'R-CYL   ')   NOGO=.TRUE.
                IF(WC.EQ.'SP-R    ')   NOGO=.TRUE.
                IF(WC.EQ.'R-SP    ')   NOGO=.TRUE.
                IF(WC.EQ.'RE-IM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IM-RE   ')   NOGO=.TRUE.
                IF(WC.EQ.'H-HMS   ')   NOGO=.TRUE.
                IF(WC.EQ.'HMS-H   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-MM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-CM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-M    ')   NOGO=.TRUE.
                IF(WC.EQ.'MM-IN   ')   NOGO=.TRUE.
                IF(WC.EQ.'CM-IN   ')   NOGO=.TRUE.
                IF(WC.EQ.'M-IN    ')   NOGO=.TRUE.
                IF(WC.EQ.'SET     ')   NOGO=.TRUE.
                IF(WC.EQ.'W1      ')   NOGO=.TRUE.
                IF(WC.EQ.'W2      ')   NOGO=.TRUE.
                IF(WC.EQ.'W3      ')   NOGO=.TRUE.
                IF(WC.EQ.'W4      ')   NOGO=.TRUE.
                IF(WC.EQ.'W5      ')   NOGO=.TRUE.
                IF(WC.EQ.'TOPS    ')   NOGO=.TRUE.
                IF(WC.EQ.'C       ')   NOGO=.TRUE.
                IF(WC.EQ.'M       ')   NOGO=.TRUE.
                IF(WC.EQ.'EOS     ')   NOGO=.TRUE.
                IF(WC.EQ.'?       ')   NOGO=.TRUE.
                IF(WC.EQ.'        ')   NOGO=.TRUE.
                IF(WC.EQ.'OP_DESC ')   NOGO=.TRUE.
C     PREDEFINED TOL OPERAND NAMES SET OHFUN TO TRUE
                IF(WC.EQ.'X       ')   OHFUN=.TRUE.
                IF(WC.EQ.'Y       ')   OHFUN=.TRUE.
                IF(WC.EQ.'Z       ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCL     ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCM     ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCN     ')   OHFUN=.TRUE.
                IF(WC.EQ.'K       ')   OHFUN=.TRUE.
                IF(WC.EQ.'L       ')   OHFUN=.TRUE.
                IF(WC.EQ.'M       ')   OHFUN=.TRUE.
      IF(WC.EQ.'DX      ') OHFUN=.TRUE.
      IF(WC.EQ.'DY      ') OHFUN=.TRUE.
      IF(WC.EQ.'DR      ') OHFUN=.TRUE.
      IF(WC.EQ.'DXA     ') OHFUN=.TRUE.
      IF(WC.EQ.'DYA     ') OHFUN=.TRUE.
      IF(WC.EQ.'DRA     ') OHFUN=.TRUE.
      IF(WC.EQ.'XANG    ') OHFUN=.TRUE.
      IF(WC.EQ.'YANG    ') OHFUN=.TRUE.
      IF(WC.EQ.'OPL     ') OHFUN=.TRUE.
      IF(WC.EQ.'OPD     ') OHFUN=.TRUE.
      IF(WC.EQ.'OPDW    ') OHFUN=.TRUE.
      IF(WC.EQ.'LOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'MOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'NOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'LEN     ') OHFUN=.TRUE.
      IF(WC.EQ.'AII     ') OHFUN=.TRUE.
      IF(WC.EQ.'AIP     ') OHFUN=.TRUE.
      IF(WC.EQ.'LN      ') OHFUN=.TRUE.
      IF(WC.EQ.'MN      ') OHFUN=.TRUE.
      IF(WC.EQ.'NN      ') OHFUN=.TRUE.
      IF(WC.EQ.'PXPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PXPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PYPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PYPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PXAPX   ') OHFUN=.TRUE.
      IF(WC.EQ.'PXAPY   ') OHFUN=.TRUE.
      IF(WC.EQ.'PYAPX   ') OHFUN=.TRUE.
      IF(WC.EQ.'PYAPY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DXDX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DXDY    ') OHFUN=.TRUE.
      IF(WC.EQ.'DYDX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DYDY    ') OHFUN=.TRUE.
      IF(WC.EQ.'DXADX   ') OHFUN=.TRUE.
      IF(WC.EQ.'DXADY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DYADX   ') OHFUN=.TRUE.
      IF(WC.EQ.'DYADY   ') OHFUN=.TRUE.
      IF(WC.EQ.'XREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'YREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'ZREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'LREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'MACOPT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'NREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'LREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'MREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'NREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'IREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'IPREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'YAREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'LNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'MNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'NNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'GLX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLZ     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLL     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLM     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLN     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLLOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'GLMOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'GLNOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'LENREF  ') OHFUN=.TRUE.
      IF(WC.EQ.'OPLREF  ') OHFUN=.TRUE.
      IF(WC.EQ.'RD      ') OHFUN=.TRUE.
      IF(WC.EQ.'CV      ') OHFUN=.TRUE.
      IF(WC.EQ.'TH      ') OHFUN=.TRUE.
      IF(WC.EQ.'THM     ') OHFUN=.TRUE.
      IF(WC.EQ.'PRICE   ') OHFUN=.TRUE.
      IF(WC.EQ.'CC      ') OHFUN=.TRUE.
      IF(WC.EQ.'AC      ') OHFUN=.TRUE.
      IF(WC.EQ.'AD      ') OHFUN=.TRUE.
      IF(WC.EQ.'AE      ') OHFUN=.TRUE.
      IF(WC.EQ.'AF      ') OHFUN=.TRUE.
      IF(WC.EQ.'AG      ') OHFUN=.TRUE.
      IF(WC.EQ.'AH      ') OHFUN=.TRUE.
      IF(WC.EQ.'AI      ') OHFUN=.TRUE.
      IF(WC.EQ.'AJ      ') OHFUN=.TRUE.
      IF(WC.EQ.'AK      ') OHFUN=.TRUE.
      IF(WC.EQ.'AL      ') OHFUN=.TRUE.
      IF(WC.EQ.'RDTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'CVTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'GRS     ') OHFUN=.TRUE.
      IF(WC.EQ.'CCTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'ADTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AETOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AFTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AGTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'ALPHA   ') OHFUN=.TRUE.
      IF(WC.EQ.'BETA    ') OHFUN=.TRUE.
      IF(WC.EQ.'GAMMA   ') OHFUN=.TRUE.
      IF(WC.EQ.'GDX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GDY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GDZ     ') OHFUN=.TRUE.
      IF(WC.EQ.'GALPHA  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBETA   ') OHFUN=.TRUE.
      IF(WC.EQ.'GGAMMA  ') OHFUN=.TRUE.
      IF(WC.EQ.'VNUM    ') OHFUN=.TRUE.
      IF(WC.EQ.'DPART   ') OHFUN=.TRUE.
      IF(WC.EQ.'ABBE    ') OHFUN=.TRUE.
      IF(WC.EQ.'PARTL   ') OHFUN=.TRUE.
      IF(WC.EQ.'INDEX   ') OHFUN=.TRUE.
      IF(WC.EQ.'N1      ') OHFUN=.TRUE.
      IF(WC.EQ.'N2      ') OHFUN=.TRUE.
      IF(WC.EQ.'N3      ') OHFUN=.TRUE.
      IF(WC.EQ.'N4      ') OHFUN=.TRUE.
      IF(WC.EQ.'N5      ') OHFUN=.TRUE.
      IF(WC.EQ.'N6      ') OHFUN=.TRUE.
      IF(WC.EQ.'N7      ') OHFUN=.TRUE.
      IF(WC.EQ.'N8      ') OHFUN=.TRUE.
      IF(WC.EQ.'N9      ') OHFUN=.TRUE.
      IF(WC.EQ.'N10     ') OHFUN=.TRUE.
      IF(WC.EQ.'XD      ') OHFUN=.TRUE.
      IF(WC.EQ.'YD      ') OHFUN=.TRUE.
      IF(WC.EQ.'ZD      ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'XVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'YVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'ZVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'LXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LENGTH  ') OHFUN=.TRUE.
      IF(WC.EQ.'OAL     ') OHFUN=.TRUE.
      IF(WC.EQ.'MLENGTH ') OHFUN=.TRUE.
      IF(WC.EQ.'OPTLEN  ') OHFUN=.TRUE.
      IF(WC.EQ.'WEIGHT  ') OHFUN=.TRUE.
      IF(WC.EQ.'ET      ') OHFUN=.TRUE.
      IF(WC.EQ.'ETY     ') OHFUN=.TRUE.
      IF(WC.EQ.'ETX     ') OHFUN=.TRUE.
      IF(WC.EQ.'SHAPEFAC') OHFUN=.TRUE.
      IF(WC.EQ.'C1      ') OHFUN=.TRUE.
      IF(WC.EQ.'C2      ') OHFUN=.TRUE.
      IF(WC.EQ.'C3      ') OHFUN=.TRUE.
      IF(WC.EQ.'C4      ') OHFUN=.TRUE.
      IF(WC.EQ.'C5      ') OHFUN=.TRUE.
      IF(WC.EQ.'C6      ') OHFUN=.TRUE.
      IF(WC.EQ.'C7      ') OHFUN=.TRUE.
      IF(WC.EQ.'C8      ') OHFUN=.TRUE.
      IF(WC.EQ.'C9      ') OHFUN=.TRUE.
      IF(WC.EQ.'C10     ') OHFUN=.TRUE.
      IF(WC.EQ.'C11     ') OHFUN=.TRUE.
      IF(WC.EQ.'C12     ') OHFUN=.TRUE.
      IF(WC.EQ.'C13     ') OHFUN=.TRUE.
      IF(WC.EQ.'C14     ') OHFUN=.TRUE.
      IF(WC.EQ.'C15     ') OHFUN=.TRUE.
      IF(WC.EQ.'C16     ') OHFUN=.TRUE.
      IF(WC.EQ.'C17     ') OHFUN=.TRUE.
      IF(WC.EQ.'C18     ') OHFUN=.TRUE.
      IF(WC.EQ.'C19     ') OHFUN=.TRUE.
      IF(WC.EQ.'C20     ') OHFUN=.TRUE.
      IF(WC.EQ.'C21     ') OHFUN=.TRUE.
      IF(WC.EQ.'C22     ') OHFUN=.TRUE.
      IF(WC.EQ.'C23     ') OHFUN=.TRUE.
      IF(WC.EQ.'C24     ') OHFUN=.TRUE.
      IF(WC.EQ.'C25     ') OHFUN=.TRUE.
      IF(WC.EQ.'C26     ') OHFUN=.TRUE.
      IF(WC.EQ.'C27     ') OHFUN=.TRUE.
      IF(WC.EQ.'C28     ') OHFUN=.TRUE.
      IF(WC.EQ.'C29     ') OHFUN=.TRUE.
      IF(WC.EQ.'C30     ') OHFUN=.TRUE.
      IF(WC.EQ.'C31     ') OHFUN=.TRUE.
      IF(WC.EQ.'C32     ') OHFUN=.TRUE.
      IF(WC.EQ.'C33     ') OHFUN=.TRUE.
      IF(WC.EQ.'C34     ') OHFUN=.TRUE.
      IF(WC.EQ.'C35     ') OHFUN=.TRUE.
      IF(WC.EQ.'C36     ') OHFUN=.TRUE.
      IF(WC.EQ.'C37     ') OHFUN=.TRUE.
      IF(WC.EQ.'C38     ') OHFUN=.TRUE.
      IF(WC.EQ.'C39     ') OHFUN=.TRUE.
      IF(WC.EQ.'C40     ') OHFUN=.TRUE.
      IF(WC.EQ.'C41     ') OHFUN=.TRUE.
      IF(WC.EQ.'C42     ') OHFUN=.TRUE.
      IF(WC.EQ.'C43     ') OHFUN=.TRUE.
      IF(WC.EQ.'C44     ') OHFUN=.TRUE.
      IF(WC.EQ.'C45     ') OHFUN=.TRUE.
      IF(WC.EQ.'C46     ') OHFUN=.TRUE.
      IF(WC.EQ.'C47     ') OHFUN=.TRUE.
      IF(WC.EQ.'C48     ') OHFUN=.TRUE.
      IF(WC.EQ.'C49     ') OHFUN=.TRUE.
      IF(WC.EQ.'C50     ') OHFUN=.TRUE.
      IF(WC.EQ.'C51     ') OHFUN=.TRUE.
      IF(WC.EQ.'C52     ') OHFUN=.TRUE.
      IF(WC.EQ.'C53     ') OHFUN=.TRUE.
      IF(WC.EQ.'C54     ') OHFUN=.TRUE.
      IF(WC.EQ.'C55     ') OHFUN=.TRUE.
      IF(WC.EQ.'C56     ') OHFUN=.TRUE.
      IF(WC.EQ.'C57     ') OHFUN=.TRUE.
      IF(WC.EQ.'C58     ') OHFUN=.TRUE.
      IF(WC.EQ.'C59     ') OHFUN=.TRUE.
      IF(WC.EQ.'C60     ') OHFUN=.TRUE.
      IF(WC.EQ.'C61     ') OHFUN=.TRUE.
      IF(WC.EQ.'C62     ') OHFUN=.TRUE.
      IF(WC.EQ.'C63     ') OHFUN=.TRUE.
      IF(WC.EQ.'C64     ') OHFUN=.TRUE.
      IF(WC.EQ.'C65     ') OHFUN=.TRUE.
      IF(WC.EQ.'C66     ') OHFUN=.TRUE.
      IF(WC.EQ.'C67     ') OHFUN=.TRUE.
      IF(WC.EQ.'C68     ') OHFUN=.TRUE.
      IF(WC.EQ.'C69     ') OHFUN=.TRUE.
      IF(WC.EQ.'C70     ') OHFUN=.TRUE.
      IF(WC.EQ.'C71     ') OHFUN=.TRUE.
      IF(WC.EQ.'C72     ') OHFUN=.TRUE.
      IF(WC.EQ.'C73     ') OHFUN=.TRUE.
      IF(WC.EQ.'C74     ') OHFUN=.TRUE.
      IF(WC.EQ.'C75     ') OHFUN=.TRUE.
      IF(WC.EQ.'C76     ') OHFUN=.TRUE.
      IF(WC.EQ.'C77     ') OHFUN=.TRUE.
      IF(WC.EQ.'C78     ') OHFUN=.TRUE.
      IF(WC.EQ.'C79     ') OHFUN=.TRUE.
      IF(WC.EQ.'C80     ') OHFUN=.TRUE.
      IF(WC.EQ.'C81     ') OHFUN=.TRUE.
      IF(WC.EQ.'C82     ') OHFUN=.TRUE.
      IF(WC.EQ.'C83     ') OHFUN=.TRUE.
      IF(WC.EQ.'C84     ') OHFUN=.TRUE.
      IF(WC.EQ.'C85     ') OHFUN=.TRUE.
      IF(WC.EQ.'C86     ') OHFUN=.TRUE.
      IF(WC.EQ.'C87     ') OHFUN=.TRUE.
      IF(WC.EQ.'C88     ') OHFUN=.TRUE.
      IF(WC.EQ.'C89     ') OHFUN=.TRUE.
      IF(WC.EQ.'C90     ') OHFUN=.TRUE.
      IF(WC.EQ.'C91     ') OHFUN=.TRUE.
      IF(WC.EQ.'C92     ') OHFUN=.TRUE.
      IF(WC.EQ.'C93     ') OHFUN=.TRUE.
      IF(WC.EQ.'C94     ') OHFUN=.TRUE.
      IF(WC.EQ.'C95     ') OHFUN=.TRUE.
      IF(WC.EQ.'C96     ') OHFUN=.TRUE.
      IF(WC.EQ.'PWRY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PWRX    ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTHY ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTHX ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTH  ') OHFUN=.TRUE.
      IF(WC.EQ.'REDSLV')   OHFUN=.TRUE.
      IF(WC.EQ.'PY      ') OHFUN=.TRUE.
      IF(WC.EQ.'PX      ') OHFUN=.TRUE.
      IF(WC.EQ.'PCY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PCX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PUCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PIX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PICY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PICX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIYP    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIXP    ') OHFUN=.TRUE.
      IF(WC.EQ.'PICYP   ') OHFUN=.TRUE.
      IF(WC.EQ.'PICXP   ') OHFUN=.TRUE.
      IF(WC.EQ.'PACY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'IMDISX  ') OHFUN=.TRUE.
      IF(WC.EQ.'IMDISY  ') OHFUN=.TRUE.
      IF(WC.EQ.'CENTX   ') OHFUN=.TRUE.
      IF(WC.EQ.'CENTY   ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMS     ') OHFUN=.TRUE.
      IF(WC.EQ.'RSSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'RSSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'RSS     ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSOPD  ') OHFUN=.TRUE.
      IF(WC.EQ.'ZERN37  ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGX    ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGY    ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGXOR  ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGYOR  ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'FFNX    ') OHFUN=.TRUE.
      IF(WC.EQ.'FFNY    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFNX    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFNY    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'ENDIAX  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENDIAY  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXDIAX  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXDIAY  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSX  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSY  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSZ  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSX  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSY  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSZ  ') OHFUN=.TRUE.
      IF(WC.EQ.'FNUMX   ') OHFUN=.TRUE.
      IF(WC.EQ.'FNUMY   ') OHFUN=.TRUE.
      IF(WC.EQ.'OBFNUMX ') OHFUN=.TRUE.
      IF(WC.EQ.'ONFNUMY ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDISX ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDISY ') OHFUN=.TRUE.
      IF(WC.EQ.'CHFIMX  ') OHFUN=.TRUE.
      IF(WC.EQ.'CHFIMY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GPX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GPY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUX    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUY    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUCX   ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUCY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIST    ') OHFUN=.TRUE.
      IF(WC.EQ.'FISHDIST') OHFUN=.TRUE.
      IF(WC.EQ.'XFOC    ') OHFUN=.TRUE.
      IF(WC.EQ.'YFOC    ') OHFUN=.TRUE.
      IF(WC.EQ.'AST     ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3   ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5    ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5   ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5   ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5   ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSA  ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSA  ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMA  ') OHFUN=.TRUE.
      IF(WC.EQ.'TAS     ') OHFUN=.TRUE.
      IF(WC.EQ.'XTAS    ') OHFUN=.TRUE.
      IF(WC.EQ.'SAS     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSAS    ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7    ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAP ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAP ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAP ') OHFUN=.TRUE.
      IF(WC.EQ.'TASP    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASP   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASP    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASP   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7P   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAS ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAS ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAS ') OHFUN=.TRUE.
      IF(WC.EQ.'TASS    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASS   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASS    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASS   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7S   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5I    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAI  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAI ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAI ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAI  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAI ') OHFUN=.TRUE.
      IF(WC.EQ.'TASI    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASI   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASI    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASI   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7I    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7I   ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZCV   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZCV  ') OHFUN=.TRUE.
      IF(WC.EQ.'MGOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'PGOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'MDOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'PDOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'GOTFM   ') OHFUN=.TRUE.
      IF(WC.EQ.'GOTFP   ') OHFUN=.TRUE.
      IF(WC.EQ.'DOTFM   ') OHFUN=.TRUE.
      IF(WC.EQ.'DOTFP   ') OHFUN=.TRUE.
      IF(WC.EQ.'REDK     ') OHFUN=.TRUE.
      IF(WC.EQ.'REDCEN  ') OHFUN=.TRUE.
      IF(WC.EQ.'SYMX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SYMY    ') OHFUN=.TRUE.
      IF(WC.EQ.'ASYMX   ') OHFUN=.TRUE.
      IF(WC.EQ.'ASYMY   ') OHFUN=.TRUE.
      IF(WC.EQ.'CTSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'CTSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SCEX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SCEY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACM    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACM    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCM    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCM    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'GREYS   ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRADX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRADY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBDISX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBDISY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRCVX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRCVY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBWAISTX') OHFUN=.TRUE.
      IF(WC.EQ.'GBWAISTY') OHFUN=.TRUE.
      IF(WC.EQ.'CLPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'CLPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DMINUSD ') OHFUN=.TRUE.
      IF(WC.EQ.'COST    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSYX   ') OHFUN=.TRUE.
      IF(WC.EQ.'CLEARX  ') OHFUN=.TRUE.
      IF(WC.EQ.'CLEARY  ') OHFUN=.TRUE.
                IF(.NOT.NOGO.AND..NOT.OHFUN) THEN
      OUTLYNE='INVALID OPERAND ENTRY'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                       RETURN
                        END IF
                IF(.NOT.NOGO.AND.OHFUN) THEN
                WQ=WC
                SQ=1
                WC='FUNC00'
C               NO REPLACEMENT DONE
                NOGO=.FALSE.
                OHFUN=.FALSE.
                        END IF
C     NOT IN TOP OR UPDATE TOPER
                END IF
C
C     IF F54 IS 1 OR 2 WE ARE WORKING THE FOCRIT FUNCTION
C     IF NOT A SPECIFIC ALLOWED COMMAND, INSERT FUNC01 FOR
C     WC AND MAKE OLD WC THE NEW WQ
                IF(F54.EQ.1.OR.F54.EQ.2) THEN
                NOGO=.FALSE.
                OHFUN=.FALSE.
                IF(WC.EQ.'FUNC01  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC02  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC03  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC04  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC05  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC06  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC07  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC08  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC09  ')   NOGO=.TRUE.
                IF(WC.EQ.'FUNC10  ')   NOGO=.TRUE.
                IF(WC.EQ.'DEL     ')   NOGO=.TRUE.
                IF(WC.EQ.'COR     ')   NOGO=.TRUE.
                IF(WC.EQ.'BYP     ')   NOGO=.TRUE.
                IF(WC.EQ.'BLO     ')   NOGO=.TRUE.
                IF(WC.EQ.'BHI     ')   NOGO=.TRUE.
                IF(WC.EQ.'GTE     ')   NOGO=.TRUE.
                IF(WC.EQ.'LTE     ')   NOGO=.TRUE.
                IF(WC.EQ.'HLD     ')   NOGO=.TRUE.
                IF(WC.EQ.'GET     ')   NOGO=.TRUE.
                IF(WC.EQ.'AGET    ')   NOGO=.TRUE.
                IF(WC.EQ.'RCL     ')   NOGO=.TRUE.
                IF(WC.EQ.'STOK    ')   NOGO=.TRUE.
                IF(WC.EQ.'WRITE   ')   NOGO=.TRUE.
                IF(WC.EQ.'MAXVAL  ')   NOGO=.TRUE.
                IF(WC.EQ.'MINVAL  ')   NOGO=.TRUE.
                IF(WC.EQ.'PI      ')   NOGO=.TRUE.
                IF(WC.EQ.'CHS     ')   NOGO=.TRUE.
                IF(WC.EQ.'MOVE    ')   NOGO=.TRUE.
                IF(WC.EQ.'LASTX   ')   NOGO=.TRUE.
                IF(WC.EQ.'LASTIX  ')   NOGO=.TRUE.
                IF(WC.EQ.'INCR    ')   NOGO=.TRUE.
                IF(WC.EQ.'PLUS    ')   NOGO=.TRUE.
                IF(WC.EQ.'MINUS   ')   NOGO=.TRUE.
                IF(WC.EQ.'MPY     ')   NOGO=.TRUE.
                IF(WC.EQ.'DIV     ')   NOGO=.TRUE.
                IF(WC.EQ.'SQRT    ')   NOGO=.TRUE.
                IF(WC.EQ.'SIN     ')   NOGO=.TRUE.
                IF(WC.EQ.'COS     ')   NOGO=.TRUE.
                IF(WC.EQ.'TAN     ')   NOGO=.TRUE.
                IF(WC.EQ.'SINH    ')   NOGO=.TRUE.
                IF(WC.EQ.'COSH    ')   NOGO=.TRUE.
                IF(WC.EQ.'TANH    ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTK   ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTKI  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTKC  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLREG   ')   NOGO=.TRUE.
                IF(WC.EQ.'CLGREG  ')   NOGO=.TRUE.
                IF(WC.EQ.'CLX     ')   NOGO=.TRUE.
                IF(WC.EQ.'CLIX    ')   NOGO=.TRUE.
                IF(WC.EQ.'ASIN    ')   NOGO=.TRUE.
                IF(WC.EQ.'ACOS    ')   NOGO=.TRUE.
                IF(WC.EQ.'ATAN    ')   NOGO=.TRUE.
                IF(WC.EQ.'ABS     ')   NOGO=.TRUE.
                IF(WC.EQ.'EXP     ')   NOGO=.TRUE.
                IF(WC.EQ.'LN      ')   NOGO=.TRUE.
                IF(WC.EQ.'LOG10   ')   NOGO=.TRUE.
                IF(WC.EQ.'FACT    ')   NOGO=.TRUE.
                IF(WC.EQ.'SGN     ')   NOGO=.TRUE.
                IF(WC.EQ.'RECIP   ')   NOGO=.TRUE.
                IF(WC.EQ.'INTGR   ')   NOGO=.TRUE.
                IF(WC.EQ.'FRAC    ')   NOGO=.TRUE.
                IF(WC.EQ.'POW     ')   NOGO=.TRUE.
                IF(WC.EQ.'STORE   ')   NOGO=.TRUE.
                IF(WC.EQ.'RAND    ')   NOGO=.TRUE.
                IF(WC.EQ.'MOD     ')   NOGO=.TRUE.
                IF(WC.EQ.'CLSTREG ')   NOGO=.TRUE.
                IF(WC.EQ.'STADD   ')   NOGO=.TRUE.
                IF(WC.EQ.'STSUB   ')   NOGO=.TRUE.
                IF(WC.EQ.'MEAN    ')   NOGO=.TRUE.
                IF(WC.EQ.'STDEV   ')   NOGO=.TRUE.
                IF(WC.EQ.'RTD     ')   NOGO=.TRUE.
                IF(WC.EQ.'DTR     ')   NOGO=.TRUE.
                IF(WC.EQ.'ATAN2   ')   NOGO=.TRUE.
                IF(WC.EQ.'J1      ')   NOGO=.TRUE.
                IF(WC.EQ.'PREAD   ')   NOGO=.TRUE.
                IF(WC.EQ.'ATON    ')   NOGO=.TRUE.
                IF(WC.EQ.'STOAX   ')   NOGO=.TRUE.
                IF(WC.EQ.'ARCL    ')   NOGO=.TRUE.
                IF(WC.EQ.'AWRITE  ')   NOGO=.TRUE.
                IF(WC.EQ.'ASTO    ')   NOGO=.TRUE.
                IF(WC.EQ.'CLASTO  ')   NOGO=.TRUE.
                IF(WC.EQ.'SHOW    ')   NOGO=.TRUE.
                IF(WC.EQ.'ENT     ')   NOGO=.TRUE.
                IF(WC.EQ.'ENTI    ')   NOGO=.TRUE.
                IF(WC.EQ.'ENTC    ')   NOGO=.TRUE.
                IF(WC.EQ.'PULL    ')   NOGO=.TRUE.
                IF(WC.EQ.'IPULL   ')   NOGO=.TRUE.
                IF(WC.EQ.'CPULL   ')   NOGO=.TRUE.
                IF(WC.EQ.'RUP     ')   NOGO=.TRUE.
                IF(WC.EQ.'IRUP    ')   NOGO=.TRUE.
                IF(WC.EQ.'CRUP    ')   NOGO=.TRUE.
                IF(WC.EQ.'RDN     ')   NOGO=.TRUE.
                IF(WC.EQ.'IRDN    ')   NOGO=.TRUE.
                IF(WC.EQ.'CRDN    ')   NOGO=.TRUE.
                IF(WC.EQ.'X-Y     ')   NOGO=.TRUE.
                IF(WC.EQ.'IX-IY   ')   NOGO=.TRUE.
                IF(WC.EQ.'+       ')   NOGO=.TRUE.
                IF(WC.EQ.'-       ')   NOGO=.TRUE.
                IF(WC.EQ.'*       ')   NOGO=.TRUE.
                IF(WC.EQ.'/       ')   NOGO=.TRUE.
                IF(WC.EQ.'I+      ')   NOGO=.TRUE.
                IF(WC.EQ.'I-      ')   NOGO=.TRUE.
                IF(WC.EQ.'I*      ')   NOGO=.TRUE.
                IF(WC.EQ.'I/      ')   NOGO=.TRUE.
                IF(WC.EQ.'C+      ')   NOGO=.TRUE.
                IF(WC.EQ.'C-      ')   NOGO=.TRUE.
                IF(WC.EQ.'C*      ')   NOGO=.TRUE.
                IF(WC.EQ.'C/      ')   NOGO=.TRUE.
                IF(WC.EQ.'Y**X    ')   NOGO=.TRUE.
                IF(WC.EQ.'IY**IX  ')   NOGO=.TRUE.
                IF(WC.EQ.'CY**CX  ')   NOGO=.TRUE.
                IF(WC.EQ.'P-R     ')   NOGO=.TRUE.
                IF(WC.EQ.'R-P     ')   NOGO=.TRUE.
                IF(WC.EQ.'CYL-R   ')   NOGO=.TRUE.
                IF(WC.EQ.'R-CYL   ')   NOGO=.TRUE.
                IF(WC.EQ.'SP-R    ')   NOGO=.TRUE.
                IF(WC.EQ.'R-SP    ')   NOGO=.TRUE.
                IF(WC.EQ.'RE-IM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IM-RE   ')   NOGO=.TRUE.
                IF(WC.EQ.'H-HMS   ')   NOGO=.TRUE.
                IF(WC.EQ.'HMS-H   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-MM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-CM   ')   NOGO=.TRUE.
                IF(WC.EQ.'IN-M    ')   NOGO=.TRUE.
                IF(WC.EQ.'MM-IN   ')   NOGO=.TRUE.
                IF(WC.EQ.'CM-IN   ')   NOGO=.TRUE.
                IF(WC.EQ.'M-IN    ')   NOGO=.TRUE.
                IF(WC.EQ.'SET     ')   NOGO=.TRUE.
                IF(WC.EQ.'W1      ')   NOGO=.TRUE.
                IF(WC.EQ.'W2      ')   NOGO=.TRUE.
                IF(WC.EQ.'W3      ')   NOGO=.TRUE.
                IF(WC.EQ.'W4      ')   NOGO=.TRUE.
                IF(WC.EQ.'W5      ')   NOGO=.TRUE.
                IF(WC.EQ.'MR      ')   NOGO=.TRUE.
                IF(WC.EQ.'MRA     ')   NOGO=.TRUE.
                IF(WC.EQ.'OP      ')   NOGO=.TRUE.
                IF(WC.EQ.'OPA     ')   NOGO=.TRUE.
                IF(WC.EQ.'CFG     ')   NOGO=.TRUE.
                IF(WC.EQ.'C       ')   NOGO=.TRUE.
                IF(WC.EQ.'M       ')   NOGO=.TRUE.
                IF(WC.EQ.'EOS     ')   NOGO=.TRUE.
                IF(WC.EQ.'?       ')   NOGO=.TRUE.
                IF(WC.EQ.'        ')   NOGO=.TRUE.
                IF(WC.EQ.'OP_DESC ')   NOGO=.TRUE.
C     PREDEFINED FOCRIT OPERAND NAMES SET OHFUN TO TRUE
                IF(WC.EQ.'X       ')   OHFUN=.TRUE.
                IF(WC.EQ.'Y       ')   OHFUN=.TRUE.
                IF(WC.EQ.'Z       ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCL     ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCM     ')   OHFUN=.TRUE.
                IF(WC.EQ.'DCN     ')   OHFUN=.TRUE.
                IF(WC.EQ.'K       ')   OHFUN=.TRUE.
                IF(WC.EQ.'L       ')   OHFUN=.TRUE.
                IF(WC.EQ.'M       ')   OHFUN=.TRUE.
      IF(WC.EQ.'DX      ') OHFUN=.TRUE.
      IF(WC.EQ.'DY      ') OHFUN=.TRUE.
      IF(WC.EQ.'DR      ') OHFUN=.TRUE.
      IF(WC.EQ.'DXA     ') OHFUN=.TRUE.
      IF(WC.EQ.'DYA     ') OHFUN=.TRUE.
      IF(WC.EQ.'DRA     ') OHFUN=.TRUE.
      IF(WC.EQ.'XANG    ') OHFUN=.TRUE.
      IF(WC.EQ.'YANG    ') OHFUN=.TRUE.
      IF(WC.EQ.'OPL     ') OHFUN=.TRUE.
      IF(WC.EQ.'OPD     ') OHFUN=.TRUE.
      IF(WC.EQ.'OPDW    ') OHFUN=.TRUE.
      IF(WC.EQ.'LOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'MOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'NOLD    ') OHFUN=.TRUE.
      IF(WC.EQ.'LEN     ') OHFUN=.TRUE.
      IF(WC.EQ.'AII     ') OHFUN=.TRUE.
      IF(WC.EQ.'AIP     ') OHFUN=.TRUE.
      IF(WC.EQ.'LN      ') OHFUN=.TRUE.
      IF(WC.EQ.'MN      ') OHFUN=.TRUE.
      IF(WC.EQ.'NN      ') OHFUN=.TRUE.
      IF(WC.EQ.'PXPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PXPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PYPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PYPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PXAPX   ') OHFUN=.TRUE.
      IF(WC.EQ.'PXAPY   ') OHFUN=.TRUE.
      IF(WC.EQ.'PYAPX   ') OHFUN=.TRUE.
      IF(WC.EQ.'PYAPY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DXDX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DXDY    ') OHFUN=.TRUE.
      IF(WC.EQ.'DYDX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DYDY    ') OHFUN=.TRUE.
      IF(WC.EQ.'DXADX   ') OHFUN=.TRUE.
      IF(WC.EQ.'DXADY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DYADX   ') OHFUN=.TRUE.
      IF(WC.EQ.'DYADY   ') OHFUN=.TRUE.
      IF(WC.EQ.'XREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'YREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'ZREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'LREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'MACOPT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'NREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'LREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'MREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'NREFOL  ') OHFUN=.TRUE.
      IF(WC.EQ.'IREF    ') OHFUN=.TRUE.
      IF(WC.EQ.'IPREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'YAREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'LNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'MNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'NNREF   ') OHFUN=.TRUE.
      IF(WC.EQ.'GLX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLZ     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLL     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLM     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLN     ') OHFUN=.TRUE.
      IF(WC.EQ.'GLLOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'GLMOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'GLNOLD  ') OHFUN=.TRUE.
      IF(WC.EQ.'LENREF  ') OHFUN=.TRUE.
      IF(WC.EQ.'OPLREF  ') OHFUN=.TRUE.
      IF(WC.EQ.'RD      ') OHFUN=.TRUE.
      IF(WC.EQ.'CV      ') OHFUN=.TRUE.
      IF(WC.EQ.'TH      ') OHFUN=.TRUE.
      IF(WC.EQ.'THM     ') OHFUN=.TRUE.
      IF(WC.EQ.'PRICE   ') OHFUN=.TRUE.
      IF(WC.EQ.'CC      ') OHFUN=.TRUE.
      IF(WC.EQ.'AC      ') OHFUN=.TRUE.
      IF(WC.EQ.'AD      ') OHFUN=.TRUE.
      IF(WC.EQ.'AE      ') OHFUN=.TRUE.
      IF(WC.EQ.'AF      ') OHFUN=.TRUE.
      IF(WC.EQ.'AG      ') OHFUN=.TRUE.
      IF(WC.EQ.'AH      ') OHFUN=.TRUE.
      IF(WC.EQ.'AI      ') OHFUN=.TRUE.
      IF(WC.EQ.'AJ      ') OHFUN=.TRUE.
      IF(WC.EQ.'AK      ') OHFUN=.TRUE.
      IF(WC.EQ.'AL      ') OHFUN=.TRUE.
      IF(WC.EQ.'RDTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'CVTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'GRS     ') OHFUN=.TRUE.
      IF(WC.EQ.'CCTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'ADTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AETOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AFTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'AGTOR   ') OHFUN=.TRUE.
      IF(WC.EQ.'ALPHA   ') OHFUN=.TRUE.
      IF(WC.EQ.'BETA    ') OHFUN=.TRUE.
      IF(WC.EQ.'GAMMA   ') OHFUN=.TRUE.
      IF(WC.EQ.'GDX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GDY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GDZ     ') OHFUN=.TRUE.
      IF(WC.EQ.'GALPHA  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBETA   ') OHFUN=.TRUE.
      IF(WC.EQ.'GGAMMA  ') OHFUN=.TRUE.
      IF(WC.EQ.'VNUM    ') OHFUN=.TRUE.
      IF(WC.EQ.'DPART   ') OHFUN=.TRUE.
      IF(WC.EQ.'ABBE    ') OHFUN=.TRUE.
      IF(WC.EQ.'PARTL   ') OHFUN=.TRUE.
      IF(WC.EQ.'INDEX   ') OHFUN=.TRUE.
      IF(WC.EQ.'N1      ') OHFUN=.TRUE.
      IF(WC.EQ.'N2      ') OHFUN=.TRUE.
      IF(WC.EQ.'N3      ') OHFUN=.TRUE.
      IF(WC.EQ.'N4      ') OHFUN=.TRUE.
      IF(WC.EQ.'N5      ') OHFUN=.TRUE.
      IF(WC.EQ.'N6      ') OHFUN=.TRUE.
      IF(WC.EQ.'N7      ') OHFUN=.TRUE.
      IF(WC.EQ.'N8      ') OHFUN=.TRUE.
      IF(WC.EQ.'N9      ') OHFUN=.TRUE.
      IF(WC.EQ.'N10     ') OHFUN=.TRUE.
      IF(WC.EQ.'XD      ') OHFUN=.TRUE.
      IF(WC.EQ.'YD      ') OHFUN=.TRUE.
      IF(WC.EQ.'ZD      ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIVZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'XVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'YVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'ZVERT   ') OHFUN=.TRUE.
      IF(WC.EQ.'LXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NXVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NYVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'MZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'NZVERT  ') OHFUN=.TRUE.
      IF(WC.EQ.'LENGTH  ') OHFUN=.TRUE.
      IF(WC.EQ.'OAL     ') OHFUN=.TRUE.
      IF(WC.EQ.'MLENGTH ') OHFUN=.TRUE.
      IF(WC.EQ.'OPTLEN  ') OHFUN=.TRUE.
      IF(WC.EQ.'WEIGHT  ') OHFUN=.TRUE.
      IF(WC.EQ.'ET      ') OHFUN=.TRUE.
      IF(WC.EQ.'ETY     ') OHFUN=.TRUE.
      IF(WC.EQ.'ETX     ') OHFUN=.TRUE.
      IF(WC.EQ.'SHAPEFAC') OHFUN=.TRUE.
      IF(WC.EQ.'C1      ') OHFUN=.TRUE.
      IF(WC.EQ.'C2      ') OHFUN=.TRUE.
      IF(WC.EQ.'C3      ') OHFUN=.TRUE.
      IF(WC.EQ.'C4      ') OHFUN=.TRUE.
      IF(WC.EQ.'C5      ') OHFUN=.TRUE.
      IF(WC.EQ.'C6      ') OHFUN=.TRUE.
      IF(WC.EQ.'C7      ') OHFUN=.TRUE.
      IF(WC.EQ.'C8      ') OHFUN=.TRUE.
      IF(WC.EQ.'C9      ') OHFUN=.TRUE.
      IF(WC.EQ.'C10     ') OHFUN=.TRUE.
      IF(WC.EQ.'C11     ') OHFUN=.TRUE.
      IF(WC.EQ.'C12     ') OHFUN=.TRUE.
      IF(WC.EQ.'C13     ') OHFUN=.TRUE.
      IF(WC.EQ.'C14     ') OHFUN=.TRUE.
      IF(WC.EQ.'C15     ') OHFUN=.TRUE.
      IF(WC.EQ.'C16     ') OHFUN=.TRUE.
      IF(WC.EQ.'C17     ') OHFUN=.TRUE.
      IF(WC.EQ.'C18     ') OHFUN=.TRUE.
      IF(WC.EQ.'C19     ') OHFUN=.TRUE.
      IF(WC.EQ.'C20     ') OHFUN=.TRUE.
      IF(WC.EQ.'C21     ') OHFUN=.TRUE.
      IF(WC.EQ.'C22     ') OHFUN=.TRUE.
      IF(WC.EQ.'C23     ') OHFUN=.TRUE.
      IF(WC.EQ.'C24     ') OHFUN=.TRUE.
      IF(WC.EQ.'C25     ') OHFUN=.TRUE.
      IF(WC.EQ.'C26     ') OHFUN=.TRUE.
      IF(WC.EQ.'C27     ') OHFUN=.TRUE.
      IF(WC.EQ.'C28     ') OHFUN=.TRUE.
      IF(WC.EQ.'C29     ') OHFUN=.TRUE.
      IF(WC.EQ.'C30     ') OHFUN=.TRUE.
      IF(WC.EQ.'C31     ') OHFUN=.TRUE.
      IF(WC.EQ.'C32     ') OHFUN=.TRUE.
      IF(WC.EQ.'C33     ') OHFUN=.TRUE.
      IF(WC.EQ.'C34     ') OHFUN=.TRUE.
      IF(WC.EQ.'C35     ') OHFUN=.TRUE.
      IF(WC.EQ.'C36     ') OHFUN=.TRUE.
      IF(WC.EQ.'C37     ') OHFUN=.TRUE.
      IF(WC.EQ.'C38     ') OHFUN=.TRUE.
      IF(WC.EQ.'C39     ') OHFUN=.TRUE.
      IF(WC.EQ.'C40     ') OHFUN=.TRUE.
      IF(WC.EQ.'C41     ') OHFUN=.TRUE.
      IF(WC.EQ.'C42     ') OHFUN=.TRUE.
      IF(WC.EQ.'C43     ') OHFUN=.TRUE.
      IF(WC.EQ.'C44     ') OHFUN=.TRUE.
      IF(WC.EQ.'C45     ') OHFUN=.TRUE.
      IF(WC.EQ.'C46     ') OHFUN=.TRUE.
      IF(WC.EQ.'C47     ') OHFUN=.TRUE.
      IF(WC.EQ.'C48     ') OHFUN=.TRUE.
      IF(WC.EQ.'C49     ') OHFUN=.TRUE.
      IF(WC.EQ.'C50     ') OHFUN=.TRUE.
      IF(WC.EQ.'C51     ') OHFUN=.TRUE.
      IF(WC.EQ.'C52     ') OHFUN=.TRUE.
      IF(WC.EQ.'C53     ') OHFUN=.TRUE.
      IF(WC.EQ.'C54     ') OHFUN=.TRUE.
      IF(WC.EQ.'C55     ') OHFUN=.TRUE.
      IF(WC.EQ.'C56     ') OHFUN=.TRUE.
      IF(WC.EQ.'C57     ') OHFUN=.TRUE.
      IF(WC.EQ.'C58     ') OHFUN=.TRUE.
      IF(WC.EQ.'C59     ') OHFUN=.TRUE.
      IF(WC.EQ.'C60     ') OHFUN=.TRUE.
      IF(WC.EQ.'C61     ') OHFUN=.TRUE.
      IF(WC.EQ.'C62     ') OHFUN=.TRUE.
      IF(WC.EQ.'C63     ') OHFUN=.TRUE.
      IF(WC.EQ.'C64     ') OHFUN=.TRUE.
      IF(WC.EQ.'C65     ') OHFUN=.TRUE.
      IF(WC.EQ.'C66     ') OHFUN=.TRUE.
      IF(WC.EQ.'C67     ') OHFUN=.TRUE.
      IF(WC.EQ.'C68     ') OHFUN=.TRUE.
      IF(WC.EQ.'C69     ') OHFUN=.TRUE.
      IF(WC.EQ.'C70     ') OHFUN=.TRUE.
      IF(WC.EQ.'C71     ') OHFUN=.TRUE.
      IF(WC.EQ.'C72     ') OHFUN=.TRUE.
      IF(WC.EQ.'C73     ') OHFUN=.TRUE.
      IF(WC.EQ.'C74     ') OHFUN=.TRUE.
      IF(WC.EQ.'C75     ') OHFUN=.TRUE.
      IF(WC.EQ.'C76     ') OHFUN=.TRUE.
      IF(WC.EQ.'C77     ') OHFUN=.TRUE.
      IF(WC.EQ.'C78     ') OHFUN=.TRUE.
      IF(WC.EQ.'C79     ') OHFUN=.TRUE.
      IF(WC.EQ.'C80     ') OHFUN=.TRUE.
      IF(WC.EQ.'C81     ') OHFUN=.TRUE.
      IF(WC.EQ.'C82     ') OHFUN=.TRUE.
      IF(WC.EQ.'C83     ') OHFUN=.TRUE.
      IF(WC.EQ.'C84     ') OHFUN=.TRUE.
      IF(WC.EQ.'C85     ') OHFUN=.TRUE.
      IF(WC.EQ.'C86     ') OHFUN=.TRUE.
      IF(WC.EQ.'C87     ') OHFUN=.TRUE.
      IF(WC.EQ.'C88     ') OHFUN=.TRUE.
      IF(WC.EQ.'C89     ') OHFUN=.TRUE.
      IF(WC.EQ.'C90     ') OHFUN=.TRUE.
      IF(WC.EQ.'C91     ') OHFUN=.TRUE.
      IF(WC.EQ.'C92     ') OHFUN=.TRUE.
      IF(WC.EQ.'C93     ') OHFUN=.TRUE.
      IF(WC.EQ.'C94     ') OHFUN=.TRUE.
      IF(WC.EQ.'C95     ') OHFUN=.TRUE.
      IF(WC.EQ.'C96     ') OHFUN=.TRUE.
      IF(WC.EQ.'PWRY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PWRX    ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTHY ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTHX ') OHFUN=.TRUE.
      IF(WC.EQ.'FLCLTH  ') OHFUN=.TRUE.
      IF(WC.EQ.'REDSLV')   OHFUN=.TRUE.
      IF(WC.EQ.'PY      ') OHFUN=.TRUE.
      IF(WC.EQ.'PX      ') OHFUN=.TRUE.
      IF(WC.EQ.'PCY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PCX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PUCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PUCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIY     ') OHFUN=.TRUE.
      IF(WC.EQ.'PIX     ') OHFUN=.TRUE.
      IF(WC.EQ.'PICY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PICX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIYP    ') OHFUN=.TRUE.
      IF(WC.EQ.'PIXP    ') OHFUN=.TRUE.
      IF(WC.EQ.'PICYP   ') OHFUN=.TRUE.
      IF(WC.EQ.'PICXP   ') OHFUN=.TRUE.
      IF(WC.EQ.'PACY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACX    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'IMDISX  ') OHFUN=.TRUE.
      IF(WC.EQ.'IMDISY  ') OHFUN=.TRUE.
      IF(WC.EQ.'CENTX   ') OHFUN=.TRUE.
      IF(WC.EQ.'CENTY   ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMS     ') OHFUN=.TRUE.
      IF(WC.EQ.'RSSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'RSSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'RSS     ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSOPD  ') OHFUN=.TRUE.
      IF(WC.EQ.'ZERN37  ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGX    ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGY    ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGXOR  ') OHFUN=.TRUE.
      IF(WC.EQ.'MAGYOR  ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'FFNX    ') OHFUN=.TRUE.
      IF(WC.EQ.'FFNY    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFNX    ') OHFUN=.TRUE.
      IF(WC.EQ.'BFNY    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLX    ') OHFUN=.TRUE.
      IF(WC.EQ.'EFLY    ') OHFUN=.TRUE.
      IF(WC.EQ.'ENDIAX  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENDIAY  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXDIAX  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXDIAY  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSX  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSY  ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPOSZ  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSX  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSY  ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPOSZ  ') OHFUN=.TRUE.
      IF(WC.EQ.'FNUMX   ') OHFUN=.TRUE.
      IF(WC.EQ.'FNUMY   ') OHFUN=.TRUE.
      IF(WC.EQ.'OBFNUMX ') OHFUN=.TRUE.
      IF(WC.EQ.'ONFNUMY ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'ENPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'EXPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDIAX ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDIAY ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDISX ') OHFUN=.TRUE.
      IF(WC.EQ.'PUPDISY ') OHFUN=.TRUE.
      IF(WC.EQ.'CHFIMX  ') OHFUN=.TRUE.
      IF(WC.EQ.'CHFIMY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GPX     ') OHFUN=.TRUE.
      IF(WC.EQ.'GPY     ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUX    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUY    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPCX    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPCY    ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUCX   ') OHFUN=.TRUE.
      IF(WC.EQ.'GPUCY   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIST    ') OHFUN=.TRUE.
      IF(WC.EQ.'FISHDIST') OHFUN=.TRUE.
      IF(WC.EQ.'XFOC    ') OHFUN=.TRUE.
      IF(WC.EQ.'YFOC    ') OHFUN=.TRUE.
      IF(WC.EQ.'AST     ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3   ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5    ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5   ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5   ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5   ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5   ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSA  ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSA  ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMA  ') OHFUN=.TRUE.
      IF(WC.EQ.'TAS     ') OHFUN=.TRUE.
      IF(WC.EQ.'XTAS    ') OHFUN=.TRUE.
      IF(WC.EQ.'SAS     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSAS    ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7     ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7    ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5P  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAP ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAP ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAP  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAP ') OHFUN=.TRUE.
      IF(WC.EQ.'TASP    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASP   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASP    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASP   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7P    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7P   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA3S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5S  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAS ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAS ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAS  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAS ') OHFUN=.TRUE.
      IF(WC.EQ.'TASS    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASS   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASS    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASS   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7S    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7S   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA5I    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'CMA5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XCMA5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'AST5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XAST5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'DIS5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XDIS5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZ5I   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZ5I  ') OHFUN=.TRUE.
      IF(WC.EQ.'TOBSAI  ') OHFUN=.TRUE.
      IF(WC.EQ.'XTOBSAI ') OHFUN=.TRUE.
      IF(WC.EQ.'SOBSA   ') OHFUN=.TRUE.
      IF(WC.EQ.'XSOBSAI ') OHFUN=.TRUE.
      IF(WC.EQ.'ELCMAI  ') OHFUN=.TRUE.
      IF(WC.EQ.'XELCMAI ') OHFUN=.TRUE.
      IF(WC.EQ.'TASI    ') OHFUN=.TRUE.
      IF(WC.EQ.'XTASI   ') OHFUN=.TRUE.
      IF(WC.EQ.'SASI    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSASI   ') OHFUN=.TRUE.
      IF(WC.EQ.'SA7I    ') OHFUN=.TRUE.
      IF(WC.EQ.'XSA7I   ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3    ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3  ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3P   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3P  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3P ') OHFUN=.TRUE.
      IF(WC.EQ.'PSA3S   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPSA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'PCMA3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPCMA3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PAST3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPAST3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PDIS3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPDIS3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PPTZ3S  ') OHFUN=.TRUE.
      IF(WC.EQ.'XPPTZ3S ') OHFUN=.TRUE.
      IF(WC.EQ.'PTZCV   ') OHFUN=.TRUE.
      IF(WC.EQ.'XPTZCV  ') OHFUN=.TRUE.
      IF(WC.EQ.'MGOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'PGOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'MDOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'PDOTF   ') OHFUN=.TRUE.
      IF(WC.EQ.'GOTFM   ') OHFUN=.TRUE.
      IF(WC.EQ.'GOTFP   ') OHFUN=.TRUE.
      IF(WC.EQ.'DOTFM   ') OHFUN=.TRUE.
      IF(WC.EQ.'DOTFP   ') OHFUN=.TRUE.
      IF(WC.EQ.'REDK     ') OHFUN=.TRUE.
      IF(WC.EQ.'REDCEN  ') OHFUN=.TRUE.
      IF(WC.EQ.'SYMX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SYMY    ') OHFUN=.TRUE.
      IF(WC.EQ.'ASYMX   ') OHFUN=.TRUE.
      IF(WC.EQ.'ASYMY   ') OHFUN=.TRUE.
      IF(WC.EQ.'CTSX    ') OHFUN=.TRUE.
      IF(WC.EQ.'CTSY    ') OHFUN=.TRUE.
      IF(WC.EQ.'SCEX    ') OHFUN=.TRUE.
      IF(WC.EQ.'SCEY    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACM    ') OHFUN=.TRUE.
      IF(WC.EQ.'PACZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACM    ') OHFUN=.TRUE.
      IF(WC.EQ.'SACZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCM    ') OHFUN=.TRUE.
      IF(WC.EQ.'PLCZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCM    ') OHFUN=.TRUE.
      IF(WC.EQ.'SLCZ    ') OHFUN=.TRUE.
      IF(WC.EQ.'GREYS   ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRADX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRADY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBDISX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBDISY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRCVX  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBRCVY  ') OHFUN=.TRUE.
      IF(WC.EQ.'GBWAISTX') OHFUN=.TRUE.
      IF(WC.EQ.'GBWAISTY') OHFUN=.TRUE.
      IF(WC.EQ.'CLPY    ') OHFUN=.TRUE.
      IF(WC.EQ.'CLPX    ') OHFUN=.TRUE.
      IF(WC.EQ.'DMINUSD ') OHFUN=.TRUE.
      IF(WC.EQ.'COST    ') OHFUN=.TRUE.
      IF(WC.EQ.'RMSYX   ') OHFUN=.TRUE.
      IF(WC.EQ.'CLEARX  ') OHFUN=.TRUE.
      IF(WC.EQ.'CLEARY  ') OHFUN=.TRUE.
                IF(.NOT.NOGO.AND..NOT.OHFUN) THEN
      OUTLYNE='INVALID OPERAND ENTRY'
      CALL SHOWIT(1)
      OUTLYNE='NO ACTION TAKEN'
      CALL SHOWIT(1)
                       RETURN
                        END IF
                IF(.NOT.NOGO.AND.OHFUN) THEN
                WQ=WC
                SQ=1
                WC='FUNC00'
                NOGO=.FALSE.
                OHFUN=.FALSE.
C               NO REPLACEMENT DONE
                        END IF
C     NOT IN FOCRIT OR UPDATE FOCRIT
                END IF
C
                DO 3000 NNN=1,16000
                NNNN=NNN
                IF(WC.NE.WCC(NNN)) GO TO 3000
                IF(WC.EQ.WCC(NNN).AND..NOT.FNYES) GO TO 3001
 3000           CONTINUE
                IF(NNNN.EQ.16000) THEN
C       NO MATCH WAS EVER FOUND (ONLY IF IN = 5)
C       HERE IS WHERE WE RUN OFF AND SEE IF THE COMMAND IS
C       REALLY A MACRO NAME WHICH IS TO BE EXECUTED.
C       IF IT WAS A MACRO NAME THE RETURN WILL PASS THE
C       VALUE OF 1 FOR VARRIABLE MACYES.
C       IF IT WAS NOT A MACRO NAME, THE RETURN WILL PASS
C       A VALUE OF MACYES=0.
C
        IF(F2.EQ.1.OR.F3.EQ.1) GO TO 3001
C       (JUST PROCEED.
C       ALLOW THE INSTRUCTION TO BE PUT INTO THE MACRO
C       EVEN WITHOUT A MATCH.)
C
C
      IF(IN.NE.5.AND.IN.NE.8.AND.IN.NE.9.AND.IN.NE.97.AND.IN.NE.96)
     1GO TO 3001
      IF(F1.EQ.1) THEN
C     CMD LEVEL
        IF(F4.NE.1) THEN
C       NOT COMMING FROM WITHIN A MACRO
C       SET INITIAL NESTING TRACKING PARAMETERS.
                NEST=0
                DO 10 I=0,20
                NESTI(I)=0
                NESTIJ(I)=0
 10             CONTINUE
                END IF
C
C       NOW SET THE SPECIAL QUALIFIER AND NUMERIC WORDS WHICH
C       ARE USED WITH THE CALL TO THE MACRO. NAMES ARE
C       MWQ,MWS,MNW1,MNW2,MNW3,MNW4,MNW5
C
                        MWQ(0)=WQ
                        MWS(0)=WS
                        MSQ(0)=SQ
                        MST(0)=SST
                        MNW(0,1)=W1
                        MNW(0,2)=W2
                        MNW(0,3)=W3
                        MNW(0,4)=W4
                        MNW(0,5)=W5
                        MDF(0,1)=DF1
                        MDF(0,2)=DF2
                        MDF(0,3)=DF3
                        MDF(0,4)=DF4
                        MDF(0,5)=DF5
C
        DO I=0,10
        NESFUN(I)=.FALSE.
        END DO
                   DO I=0,20
                   TF(I)=0
                   END DO
C
C     CODE ADDED 5/17/2006 TO MAKE THE PERMANENT MACRO DIRECTORY
C     AUTO SEARCHED AFTER THER CURRENT DIRECTORY
C
C     SAVE THE "CURRENT" MACRO DIRECTORY NAME
        OLIBMAC=LIBMAC
        CALL MACRUN(MACYES)
C     IF MACYES=0, TRY THE PERMANENT MACRO DIRECTORY
      IF(MACYES.EQ.0) THEN
        LIBMAC=trim(getPermMacroDir())
        CALL MACRUN(MACYES)
            END IF
C     SET THE MACRO DIRECTOR BACK TO THE ORIGINAL "CURRENT" VALUE
        LIBMAC=OLIBMAC
C
C       OTHER VARIABLES ARE PASSED TO MACRUN VIA NAMED
C       COMMONS.
C
                IF(MACYES.EQ.1) THEN
C
C       THE MACRO HAS BEEN EXECUTED FROM THE MACRO PROCESSOR
C       LEVEL SO JUST RETURN AND LOOK FOR THE NEXT INPUT COMMAND.
C       SET MACYES BACK TO ZERO.
C
                MACYES=0
C
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
C
C       MACYES=0 THE COMMAND WAS INVALID. CHECK PROGRAM
C       LEVEL AND PRINT APPROPRIATE MESSAGE, THEN RETURN
C
                END IF
C               F1 IS NOT 1, NO NEED TO CHECK FOR RUNNING A
C               MACRO
                END IF
        IF(F1.EQ.1.AND.F4.EQ.0.AND.F17.EQ.0) THEN
      OUTLYNE='INVALID CMD LEVEL COMMAND'
      CALL SHOWIT(1)
      RETURN
                END IF
        IF(F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.0) THEN
      OUTLYNE='INVALID CMD LEVEL COMMAND'
      CALL SHOWIT(1)
      RETURN
                END IF
        IF(F1.EQ.0.AND.F5.EQ.1) THEN
      OUTLYNE='INVALID LENS INPUT COMMAND'
      CALL SHOWIT(1)
      OUTLYNE=INPUT
      CALL SHOWIT(1)
      OUTLYNE = "LINE 2962"
      CALL SHOWIT(19)
      RETURN
                END IF
        IF(F1.EQ.0.AND.F6.EQ.1) THEN
      OUTLYNE='INVALID LENS UPDATE COMMAND'
      CALL SHOWIT(1)
      RETURN
                END IF
        IF(F1.EQ.0.AND.F7.EQ.1) THEN
      OUTLYNE='INVALID SPSRF INPUT COMMAND'
      CALL SHOWIT(1)
      RETURN
                END IF
        IF(F1.EQ.0.AND.F8.EQ.1) THEN
      OUTLYNE='INVALID UPDATE SPSRF COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F9.EQ.1) THEN
      OUTLYNE='INVALID SPFIT COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F10.EQ.1) THEN
      OUTLYNE='INVALID CONFIGS INPUT COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F11.EQ.1) THEN
      OUTLYNE='INVALID CONFIGS UPDATE COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.1.AND.F17.EQ.1) THEN
      OUTLYNE='INVALID SPECT COMMAND'
      CALL SHOWIT(1)
                END IF
          FNYES1=.FALSE.
                PRINT *, "Checking Lens Edit fcns? "
                IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
                IF(WC.EQ.'COR     ') FNYES1=.TRUE.
                IF(WC.EQ.'BYP     ') FNYES1=.TRUE.
                IF(WC.EQ.'BLO     ') FNYES1=.TRUE.
                IF(WC.EQ.'BHI     ') FNYES1=.TRUE.
                IF(WC.EQ.'GTE     ') FNYES1=.TRUE.
                IF(WC.EQ.'LTE     ') FNYES1=.TRUE.
                IF(WC.EQ.'HLD     ') FNYES1=.TRUE.
                IF(WC.EQ.'GET     ') FNYES1=.TRUE.
                IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
                IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
                IF(WC.EQ.'STOK    ') FNYES1=.TRUE.
                IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
                IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'PI      ') FNYES1=.TRUE.
                IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
                IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
                IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
                IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
                IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
                IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
                IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
                IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
                IF(WC.EQ.'COS     ') FNYES1=.TRUE.
                IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
                IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
                IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
                IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
                IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
                IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
                IF(WC.EQ.'LN      ') FNYES1=.TRUE.
                IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
                IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
                IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
                IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
                IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
                IF(WC.EQ.'POW     ') FNYES1=.TRUE.
                IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
                IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
                IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
                IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
                IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
                IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
                IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
                IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
                IF(WC.EQ.'J1      ') FNYES1=.TRUE.
                IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
                IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
                IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
                IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
                IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
                IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
                IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
                IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
                IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
                IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
                IF(WC.EQ.'+       ') FNYES1=.TRUE.
                IF(WC.EQ.'-       ') FNYES1=.TRUE.
                IF(WC.EQ.'*       ') FNYES1=.TRUE.
                IF(WC.EQ.'/       ') FNYES1=.TRUE.
                IF(WC.EQ.'I+      ') FNYES1=.TRUE.
                IF(WC.EQ.'I-      ') FNYES1=.TRUE.
                IF(WC.EQ.'I*      ') FNYES1=.TRUE.
                IF(WC.EQ.'I/      ') FNYES1=.TRUE.
                IF(WC.EQ.'C+      ') FNYES1=.TRUE.
                IF(WC.EQ.'C-      ') FNYES1=.TRUE.
                IF(WC.EQ.'C*      ') FNYES1=.TRUE.
                IF(WC.EQ.'C/      ') FNYES1=.TRUE.
                IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
                IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
                IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
                IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
                IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
                IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
                IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
                IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
                IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
                IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
                IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
                IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
                IF(WC.EQ.'SET     ') FNYES1=.TRUE.
                IF(WC.EQ.'W1      ') FNYES1=.TRUE.
                IF(WC.EQ.'W2      ') FNYES1=.TRUE.
                IF(WC.EQ.'W3      ') FNYES1=.TRUE.
                IF(WC.EQ.'W4      ') FNYES1=.TRUE.
                IF(WC.EQ.'W5      ') FNYES1=.TRUE.
                IF(WC.EQ.'MR      ') FNYES1=.TRUE.
                IF(WC.EQ.'MRA     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP      ') FNYES1=.TRUE.
                IF(WC.EQ.'OPA     ') FNYES1=.TRUE.
                IF(WC.EQ.'CFG     ') FNYES1=.TRUE.
                IF(WC.EQ.'C       ') FNYES1=.TRUE.
                IF(WC.EQ.'M       ') FNYES1=.TRUE.
                IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
        IF(F1.EQ.0.AND.F27.EQ.1.AND..NOT.FNYES1) THEN
      OUTLYNE='INVALID (MERIT INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F27.EQ.1.AND.FNYES1) GO TO 3001
          FNYES1=.FALSE.
                IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
                IF(WC.EQ.'GET     ') FNYES1=.TRUE.
                IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
                IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
                IF(WC.EQ.'STOK    ') FNYES1=.TRUE.
                IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
                IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'PI      ') FNYES1=.TRUE.
                IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
                IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
                IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
                IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
                IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
                IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
                IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
                IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
                IF(WC.EQ.'COS     ') FNYES1=.TRUE.
                IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
                IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
                IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
                IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
                IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
                IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
                IF(WC.EQ.'LN      ') FNYES1=.TRUE.
                IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
                IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
                IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
                IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
                IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
                IF(WC.EQ.'POW     ') FNYES1=.TRUE.
                IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
                IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
                IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
                IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
                IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
                IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
                IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
                IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
                IF(WC.EQ.'J1      ') FNYES1=.TRUE.
                IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
                IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
                IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
                IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
                IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
                IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
                IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
                IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
                IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
                IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
                IF(WC.EQ.'+       ') FNYES1=.TRUE.
                IF(WC.EQ.'-       ') FNYES1=.TRUE.
                IF(WC.EQ.'*       ') FNYES1=.TRUE.
                IF(WC.EQ.'/       ') FNYES1=.TRUE.
                IF(WC.EQ.'I+      ') FNYES1=.TRUE.
                IF(WC.EQ.'I-      ') FNYES1=.TRUE.
                IF(WC.EQ.'I*      ') FNYES1=.TRUE.
                IF(WC.EQ.'I/      ') FNYES1=.TRUE.
                IF(WC.EQ.'C+      ') FNYES1=.TRUE.
                IF(WC.EQ.'C-      ') FNYES1=.TRUE.
                IF(WC.EQ.'C*      ') FNYES1=.TRUE.
                IF(WC.EQ.'C/      ') FNYES1=.TRUE.
                IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
                IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
                IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
                IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
                IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
                IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
                IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
                IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
                IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
                IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
                IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
                IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
                IF(WC.EQ.'SET     ') FNYES1=.TRUE.
                IF(WC.EQ.'W1      ') FNYES1=.TRUE.
                IF(WC.EQ.'W2      ') FNYES1=.TRUE.
                IF(WC.EQ.'W3      ') FNYES1=.TRUE.
                IF(WC.EQ.'W4      ') FNYES1=.TRUE.
                IF(WC.EQ.'W5      ') FNYES1=.TRUE.
                IF(WC.EQ.'MR      ') FNYES1=.TRUE.
                IF(WC.EQ.'MRA     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP      ') FNYES1=.TRUE.
                IF(WC.EQ.'OPA     ') FNYES1=.TRUE.
                IF(WC.EQ.'CFG     ') FNYES1=.TRUE.
                IF(WC.EQ.'C       ') FNYES1=.TRUE.
                IF(WC.EQ.'M       ') FNYES1=.TRUE.
                IF(WC.EQ.'DEL     ') FNYES1=.TRUE.
                IF(WC.EQ.'COR     ') FNYES1=.TRUE.
                IF(WC.EQ.'BYP     ') FNYES1=.TRUE.
                IF(WC.EQ.'BLO     ') FNYES1=.TRUE.
                IF(WC.EQ.'BHI     ') FNYES1=.TRUE.
                IF(WC.EQ.'GTE     ') FNYES1=.TRUE.
                IF(WC.EQ.'LTE     ') FNYES1=.TRUE.
                IF(WC.EQ.'HLD     ') FNYES1=.TRUE.
                IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
        IF(F1.EQ.0.AND.F27.EQ.2.AND..NOT.FNYES1) THEN
      OUTLYNE='INVALID (MERIT UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F27.EQ.2.AND.FNYES1) GO TO 3001
                FNYES1=.FALSE.
          FNYES1=.FALSE.
                IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
                IF(WC.EQ.'GET     ') FNYES1=.TRUE.
                IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
                IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
                IF(WC.EQ.'STOK    ') FNYES1=.TRUE.
                IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
                IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'PI      ') FNYES1=.TRUE.
                IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
                IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
                IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
                IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
                IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
                IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
                IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
                IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
                IF(WC.EQ.'COS     ') FNYES1=.TRUE.
                IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
                IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
                IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
                IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
                IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
                IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
                IF(WC.EQ.'LN      ') FNYES1=.TRUE.
                IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
                IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
                IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
                IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
                IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
                IF(WC.EQ.'POW     ') FNYES1=.TRUE.
                IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
                IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
                IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
                IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
                IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
                IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
                IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
                IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
                IF(WC.EQ.'J1      ') FNYES1=.TRUE.
                IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
                IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
                IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
                IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
                IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
                IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
                IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
                IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
                IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
                IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
                IF(WC.EQ.'+       ') FNYES1=.TRUE.
                IF(WC.EQ.'-       ') FNYES1=.TRUE.
                IF(WC.EQ.'*       ') FNYES1=.TRUE.
                IF(WC.EQ.'/       ') FNYES1=.TRUE.
                IF(WC.EQ.'I+      ') FNYES1=.TRUE.
                IF(WC.EQ.'I-      ') FNYES1=.TRUE.
                IF(WC.EQ.'I*      ') FNYES1=.TRUE.
                IF(WC.EQ.'I/      ') FNYES1=.TRUE.
                IF(WC.EQ.'C+      ') FNYES1=.TRUE.
                IF(WC.EQ.'C-      ') FNYES1=.TRUE.
                IF(WC.EQ.'C*      ') FNYES1=.TRUE.
                IF(WC.EQ.'C/      ') FNYES1=.TRUE.
                IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
                IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
                IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
                IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
                IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
                IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
                IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
                IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
                IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
                IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
                IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
                IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
                IF(WC.EQ.'SET     ') FNYES1=.TRUE.
                IF(WC.EQ.'W1      ') FNYES1=.TRUE.
                IF(WC.EQ.'W2      ') FNYES1=.TRUE.
                IF(WC.EQ.'W3      ') FNYES1=.TRUE.
                IF(WC.EQ.'W4      ') FNYES1=.TRUE.
                IF(WC.EQ.'W5      ') FNYES1=.TRUE.
                IF(WC.EQ.'TOPS    ') FNYES1=.TRUE.
                IF(WC.EQ.'C       ') FNYES1=.TRUE.
                IF(WC.EQ.'M       ') FNYES1=.TRUE.
                IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
        IF(F1.EQ.0.AND.F53.EQ.1.AND..NOT.FNYES1) THEN
      OUTLYNE='INVALID (TOPER INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F53.EQ.1.AND.FNYES1) GO TO 3001
          FNYES1=.FALSE.
                IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
                IF(WC.EQ.'GET     ') FNYES1=.TRUE.
                IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
                IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
                IF(WC.EQ.'STOK    ') FNYES1=.TRUE.
                IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
                IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'PI      ') FNYES1=.TRUE.
                IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
                IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
                IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
                IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
                IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
                IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
                IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
                IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
                IF(WC.EQ.'COS     ') FNYES1=.TRUE.
                IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
                IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
                IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
                IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
                IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
                IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
                IF(WC.EQ.'LN      ') FNYES1=.TRUE.
                IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
                IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
                IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
                IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
                IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
                IF(WC.EQ.'POW     ') FNYES1=.TRUE.
                IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
                IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
                IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
                IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
                IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
                IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
                IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
                IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
                IF(WC.EQ.'J1      ') FNYES1=.TRUE.
                IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
                IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
                IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
                IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
                IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
                IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
                IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
                IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
                IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
                IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
                IF(WC.EQ.'+       ') FNYES1=.TRUE.
                IF(WC.EQ.'-       ') FNYES1=.TRUE.
                IF(WC.EQ.'*       ') FNYES1=.TRUE.
                IF(WC.EQ.'/       ') FNYES1=.TRUE.
                IF(WC.EQ.'I+      ') FNYES1=.TRUE.
                IF(WC.EQ.'I-      ') FNYES1=.TRUE.
                IF(WC.EQ.'I*      ') FNYES1=.TRUE.
                IF(WC.EQ.'I/      ') FNYES1=.TRUE.
                IF(WC.EQ.'C+      ') FNYES1=.TRUE.
                IF(WC.EQ.'C-      ') FNYES1=.TRUE.
                IF(WC.EQ.'C*      ') FNYES1=.TRUE.
                IF(WC.EQ.'C/      ') FNYES1=.TRUE.
                IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
                IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
                IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
                IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
                IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
                IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
                IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
                IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
                IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
                IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
                IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
                IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
                IF(WC.EQ.'SET     ') FNYES1=.TRUE.
                IF(WC.EQ.'W1      ') FNYES1=.TRUE.
                IF(WC.EQ.'W2      ') FNYES1=.TRUE.
                IF(WC.EQ.'W3      ') FNYES1=.TRUE.
                IF(WC.EQ.'W4      ') FNYES1=.TRUE.
                IF(WC.EQ.'W5      ') FNYES1=.TRUE.
                IF(WC.EQ.'TOPS    ') FNYES1=.TRUE.
                IF(WC.EQ.'C       ') FNYES1=.TRUE.
                IF(WC.EQ.'M       ') FNYES1=.TRUE.
                IF(WC.EQ.'DEL     ') FNYES1=.TRUE.
                IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
        IF(F1.EQ.0.AND.F53.EQ.2.AND..NOT.FNYES1) THEN
      OUTLYNE='INVALID (TOPER UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F53.EQ.2.AND.FNYES1) GO TO 3001
                FNYES1=.FALSE.
                IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
                IF(WC.EQ.'COR     ') FNYES1=.TRUE.
                IF(WC.EQ.'BYP     ') FNYES1=.TRUE.
                IF(WC.EQ.'BLO     ') FNYES1=.TRUE.
                IF(WC.EQ.'BHI     ') FNYES1=.TRUE.
                IF(WC.EQ.'GTE     ') FNYES1=.TRUE.
                IF(WC.EQ.'LTE     ') FNYES1=.TRUE.
                IF(WC.EQ.'HLD     ') FNYES1=.TRUE.
                IF(WC.EQ.'GET     ') FNYES1=.TRUE.
                IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
                IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
                IF(WC.EQ.'STOK    ') FNYES1=.TRUE.
                IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
                IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'PI      ') FNYES1=.TRUE.
                IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
                IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
                IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
                IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
                IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
                IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
                IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
                IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
                IF(WC.EQ.'COS     ') FNYES1=.TRUE.
                IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
                IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
                IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
                IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
                IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
                IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
                IF(WC.EQ.'LN      ') FNYES1=.TRUE.
                IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
                IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
                IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
                IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
                IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
                IF(WC.EQ.'POW     ') FNYES1=.TRUE.
                IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
                IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
                IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
                IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
                IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
                IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
                IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
                IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
                IF(WC.EQ.'J1      ') FNYES1=.TRUE.
                IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
                IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
                IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
                IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
                IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
                IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
                IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
                IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
                IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
                IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
                IF(WC.EQ.'+       ') FNYES1=.TRUE.
                IF(WC.EQ.'-       ') FNYES1=.TRUE.
                IF(WC.EQ.'*       ') FNYES1=.TRUE.
                IF(WC.EQ.'/       ') FNYES1=.TRUE.
                IF(WC.EQ.'I+      ') FNYES1=.TRUE.
                IF(WC.EQ.'I-      ') FNYES1=.TRUE.
                IF(WC.EQ.'I*      ') FNYES1=.TRUE.
                IF(WC.EQ.'I/      ') FNYES1=.TRUE.
                IF(WC.EQ.'C+      ') FNYES1=.TRUE.
                IF(WC.EQ.'C-      ') FNYES1=.TRUE.
                IF(WC.EQ.'C*      ') FNYES1=.TRUE.
                IF(WC.EQ.'C/      ') FNYES1=.TRUE.
                IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
                IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
                IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
                IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
                IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
                IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
                IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
                IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
                IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
                IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
                IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
                IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
                IF(WC.EQ.'SET     ') FNYES1=.TRUE.
                IF(WC.EQ.'W1      ') FNYES1=.TRUE.
                IF(WC.EQ.'W2      ') FNYES1=.TRUE.
                IF(WC.EQ.'W3      ') FNYES1=.TRUE.
                IF(WC.EQ.'W4      ') FNYES1=.TRUE.
                IF(WC.EQ.'W5      ') FNYES1=.TRUE.
                IF(WC.EQ.'CRITS   ') FNYES1=.TRUE.
                IF(WC.EQ.'C       ') FNYES1=.TRUE.
                IF(WC.EQ.'M       ') FNYES1=.TRUE.
                IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
        IF(F1.EQ.0.AND.F54.EQ.1.AND..NOT.FNYES1) THEN
      OUTLYNE='INVALID (FOCRIT INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F54.EQ.1.AND.FNYES1) GO TO 3001
          FNYES1=.FALSE.
                IF(WC.EQ.'FUNC01  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC02  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC03  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC04  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC05  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC06  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC07  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC08  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC09  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC10  ') FNYES1=.TRUE.
                IF(WC.EQ.'FUNC00  ') FNYES1=.TRUE.
                IF(WC.EQ.'GET     ') FNYES1=.TRUE.
                IF(WC.EQ.'AGET    ') FNYES1=.TRUE.
                IF(WC.EQ.'RCL     ') FNYES1=.TRUE.
                IF(WC.EQ.'STOK    ') FNYES1=.TRUE.
                IF(WC.EQ.'WRITE   ') FNYES1=.TRUE.
                IF(WC.EQ.'MAXVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'MINVAL  ') FNYES1=.TRUE.
                IF(WC.EQ.'PI      ') FNYES1=.TRUE.
                IF(WC.EQ.'CHS     ') FNYES1=.TRUE.
                IF(WC.EQ.'MOVE    ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTX   ') FNYES1=.TRUE.
                IF(WC.EQ.'LASTIX  ') FNYES1=.TRUE.
                IF(WC.EQ.'INCR    ') FNYES1=.TRUE.
                IF(WC.EQ.'PLUS    ') FNYES1=.TRUE.
                IF(WC.EQ.'MINUS   ') FNYES1=.TRUE.
                IF(WC.EQ.'MPY     ') FNYES1=.TRUE.
                IF(WC.EQ.'DIV     ') FNYES1=.TRUE.
                IF(WC.EQ.'SQRT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SIN     ') FNYES1=.TRUE.
                IF(WC.EQ.'COS     ') FNYES1=.TRUE.
                IF(WC.EQ.'TAN     ') FNYES1=.TRUE.
                IF(WC.EQ.'SINH    ') FNYES1=.TRUE.
                IF(WC.EQ.'COSH    ') FNYES1=.TRUE.
                IF(WC.EQ.'TANH    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTK   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKI  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTKC  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLREG   ') FNYES1=.TRUE.
                IF(WC.EQ.'CLGREG  ') FNYES1=.TRUE.
                IF(WC.EQ.'CLX     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLIX    ') FNYES1=.TRUE.
                IF(WC.EQ.'ASIN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ACOS    ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'ABS     ') FNYES1=.TRUE.
                IF(WC.EQ.'EXP     ') FNYES1=.TRUE.
                IF(WC.EQ.'LN      ') FNYES1=.TRUE.
                IF(WC.EQ.'LOG10   ') FNYES1=.TRUE.
                IF(WC.EQ.'FACT    ') FNYES1=.TRUE.
                IF(WC.EQ.'SGN     ') FNYES1=.TRUE.
                IF(WC.EQ.'RECIP   ') FNYES1=.TRUE.
                IF(WC.EQ.'INTGR   ') FNYES1=.TRUE.
                IF(WC.EQ.'FRAC    ') FNYES1=.TRUE.
                IF(WC.EQ.'POW     ') FNYES1=.TRUE.
                IF(WC.EQ.'STORE   ') FNYES1=.TRUE.
                IF(WC.EQ.'RAND    ') FNYES1=.TRUE.
                IF(WC.EQ.'MOD     ') FNYES1=.TRUE.
                IF(WC.EQ.'CLSTREG ') FNYES1=.TRUE.
                IF(WC.EQ.'STADD   ') FNYES1=.TRUE.
                IF(WC.EQ.'STSUB   ') FNYES1=.TRUE.
                IF(WC.EQ.'MEAN    ') FNYES1=.TRUE.
                IF(WC.EQ.'STDEV   ') FNYES1=.TRUE.
                IF(WC.EQ.'RTD     ') FNYES1=.TRUE.
                IF(WC.EQ.'DTR     ') FNYES1=.TRUE.
                IF(WC.EQ.'ATAN2   ') FNYES1=.TRUE.
                IF(WC.EQ.'J1      ') FNYES1=.TRUE.
                IF(WC.EQ.'PREAD   ') FNYES1=.TRUE.
                IF(WC.EQ.'ATON    ') FNYES1=.TRUE.
                IF(WC.EQ.'STOAX   ') FNYES1=.TRUE.
                IF(WC.EQ.'ARCL    ') FNYES1=.TRUE.
                IF(WC.EQ.'AWRITE  ') FNYES1=.TRUE.
                IF(WC.EQ.'ASTO    ') FNYES1=.TRUE.
                IF(WC.EQ.'CLASTO  ') FNYES1=.TRUE.
                IF(WC.EQ.'SHOW    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENT     ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTI    ') FNYES1=.TRUE.
                IF(WC.EQ.'ENTC    ') FNYES1=.TRUE.
                IF(WC.EQ.'PULL    ') FNYES1=.TRUE.
                IF(WC.EQ.'IPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'CPULL   ') FNYES1=.TRUE.
                IF(WC.EQ.'RUP     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRUP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RDN     ') FNYES1=.TRUE.
                IF(WC.EQ.'IRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'CRDN    ') FNYES1=.TRUE.
                IF(WC.EQ.'X-Y     ') FNYES1=.TRUE.
                IF(WC.EQ.'IX-IY   ') FNYES1=.TRUE.
                IF(WC.EQ.'+       ') FNYES1=.TRUE.
                IF(WC.EQ.'-       ') FNYES1=.TRUE.
                IF(WC.EQ.'*       ') FNYES1=.TRUE.
                IF(WC.EQ.'/       ') FNYES1=.TRUE.
                IF(WC.EQ.'I+      ') FNYES1=.TRUE.
                IF(WC.EQ.'I-      ') FNYES1=.TRUE.
                IF(WC.EQ.'I*      ') FNYES1=.TRUE.
                IF(WC.EQ.'I/      ') FNYES1=.TRUE.
                IF(WC.EQ.'C+      ') FNYES1=.TRUE.
                IF(WC.EQ.'C-      ') FNYES1=.TRUE.
                IF(WC.EQ.'C*      ') FNYES1=.TRUE.
                IF(WC.EQ.'C/      ') FNYES1=.TRUE.
                IF(WC.EQ.'Y**X    ') FNYES1=.TRUE.
                IF(WC.EQ.'IY**IX  ') FNYES1=.TRUE.
                IF(WC.EQ.'CY**CX  ') FNYES1=.TRUE.
                IF(WC.EQ.'P-R     ') FNYES1=.TRUE.
                IF(WC.EQ.'R-P     ') FNYES1=.TRUE.
                IF(WC.EQ.'CYL-R   ') FNYES1=.TRUE.
                IF(WC.EQ.'R-CYL   ') FNYES1=.TRUE.
                IF(WC.EQ.'SP-R    ') FNYES1=.TRUE.
                IF(WC.EQ.'R-SP    ') FNYES1=.TRUE.
                IF(WC.EQ.'RE-IM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IM-RE   ') FNYES1=.TRUE.
                IF(WC.EQ.'H-HMS   ') FNYES1=.TRUE.
                IF(WC.EQ.'HMS-H   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-MM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-CM   ') FNYES1=.TRUE.
                IF(WC.EQ.'IN-M    ') FNYES1=.TRUE.
                IF(WC.EQ.'MM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'CM-IN   ') FNYES1=.TRUE.
                IF(WC.EQ.'M-IN    ') FNYES1=.TRUE.
                IF(WC.EQ.'SET     ') FNYES1=.TRUE.
                IF(WC.EQ.'W1      ') FNYES1=.TRUE.
                IF(WC.EQ.'W2      ') FNYES1=.TRUE.
                IF(WC.EQ.'W3      ') FNYES1=.TRUE.
                IF(WC.EQ.'W4      ') FNYES1=.TRUE.
                IF(WC.EQ.'W5      ') FNYES1=.TRUE.
                IF(WC.EQ.'CRITS   ') FNYES1=.TRUE.
                IF(WC.EQ.'C       ') FNYES1=.TRUE.
                IF(WC.EQ.'M       ') FNYES1=.TRUE.
                IF(WC.EQ.'DEL     ') FNYES1=.TRUE.
                IF(WC.EQ.'EOS     ') FNYES1=.TRUE.
                IF(WC.EQ.'OP_DESC ') FNYES1=.TRUE.
        IF(F1.EQ.0.AND.F54.EQ.2.AND..NOT.FNYES1) THEN
          FNYES1=.FALSE.
      OUTLYNE='INVALID (FOCRIT UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F54.EQ.2.AND.FNYES1) GO TO 3001
        IF(F1.EQ.0.AND.F29.EQ.1) THEN
      OUTLYNE='INVALID (VARIABLE INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F29.EQ.2) THEN
      OUTLYNE='INVALID (VARIABLE UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F51.EQ.1) THEN
      OUTLYNE='INVALID (TVAR INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F51.EQ.2) THEN
      OUTLYNE='INVALID (TVAR UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F52.EQ.1) THEN
      OUTLYNE='INVALID (COMPVAR INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F52.EQ.2) THEN
      OUTLYNE='INVALID (COMPVAR UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F53.EQ.1) THEN
      OUTLYNE='INVALID (TOPER INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F53.EQ.2) THEN
      OUTLYNE='INVALID (TOPER UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F54.EQ.1) THEN
      OUTLYNE='INVALID (FOCRIT INPUT) COMMAND'
      CALL SHOWIT(1)
                END IF
        IF(F1.EQ.0.AND.F54.EQ.2) THEN
      OUTLYNE='INVALID (FOCRIT UPDATE) COMMAND'
      CALL SHOWIT(1)
                END IF
C       SET F48 AND F49 TO ZERO
                F48=0
                F49=0
                CALL MACFAL
                RETURN
C       A MATCH WAS FOUND
                END IF
 3001   CONTINUE
C
C       AT THIS POINT A SEARCH IS PERFORMED FOR EACH LEVEL OF THE
C       PROGRAM TO DETERMINE IF THE VALID PROGRAM COMMAND IS VALID
C       WITHIN THAT LEVEL.
C
C       FOR THE CMD LEVEL FLAG F1=1
C
C       MANY COMMANDS MAY BE ISSUED WHEN F1=1. THE COMMAND WHICH
C       ARE NOT ALLOWED ARE:
C               EOM - END OF MACRO
C
C       AND THE LMEDITING COMMANDS:
C
C               PR -  PRINT A LINE OR LINES
C               BT -  GO TO BOTTOM OF MACRO
C               DE -  DELETE  MACRO LINES
C               EX -  EXTRACT ANOTHER MACRO INTO THE
C                     CURRENT MACRO
C               FL -  STOP EDITING AND FILE THE MACRO
C               GO -  GO TO A SPECIFIED MACRO LINE
C             LO C -  LOCATE THE NEXT OCCURENCE OF A COMMAND
C                     WORD
C             LO Q -  LOCATE THE NEXT OCCURENCE OF A QUALIFIER
C                     WORD
C            LO CQ -  LOCATE THE NEXT OCCURENCE OF THE COMMAND
C                     AND QUALIFIER WORD.
C           LO COQ -  LOCATE THE NEXT OCCURENCE OT THE COMMAND
C                     OR QUALIFIER WORD.
C               NEXT- REPREAT PRIOR SEARCH FURTER DOWN MACRO
C       QUIT OR QU -  QUIT WITHOUT CHANGING MACRO FILE(MAC.DAT)
C               RE -  REPLACE CURRENT LINE WITH WHAT FOLLOWS
C               TP -  GO TO TOP OF THE MACRO
C
C       AND ALL MACRO EXECUTION SPECIFIC COMMANDS:
C
C       NSUB,SSUB,QSUB,MOVE (WITH NW AS QUALIFIER),CSUB,AND PUTR,
C       TRACE (ON OR OFF),ACCSUB,RETURN
C       AND ALL THE LENS INPUT AND LENS UPDATE COMMANDS ETC.
C       SO FOR CMD LEVEL WITHOUT MACRO EXECUTION;
C
        IF(F1.EQ.1.AND.F4.EQ.0.AND.F17.EQ.0) THEN
C       SPECT LEVEL IS NOT ACTIVE
C
        CALL CONT1(STP)
        IF(STP) RETURN
                END IF
C       IF THE SPECT LEVEL IS ACTIVE THEN
C
        IF(F1.EQ.1.AND.F4.EQ.0.AND.F17.EQ.1) THEN
C
        CALL CONT2(STP)
        IF(STP) RETURN
C
                END IF
C
C       IF F1=1 AND F4=1 (MACRO EXECUTION) FEWER COMMAND ARE
C       DISALLOWED.
C
        IF(F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.0.AND.F10.EQ.0
     1.OR.F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.0.AND.F11.EQ.0) THEN
C
C       THESE ARE THE COMMANDS NOT ALLOWED AT THE CMD LEVEL
C       DURING MACRO EXECUTION
C
                WC1=WC
                CALL CONTA(WC1,IS)
                        IF(IS) THEN
      OUTLYNE='INVALID CMD LEVEL COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        END IF
C       COMMANDS NOT ALLOWED DURING SPECT OPERATION
        IF(F1.EQ.1.AND.F4.EQ.1.AND.F17.EQ.1) THEN
C
      WC1=WC
      CALL CONTB(WC1,IS)
                IF(IS) THEN
      OUTLYNE='INVALID SPECT LEVEL COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
C
C       FOR MACRO CREATION MODE, MANNY COMMANDS MAY BE ISSUED.
C       THE COMMANDS WHICH MAY NOT BE ISSUED ARE:
C
C               MACRO - CREATE A NEW MACRO
C               IMF   - INITIALIZE MACRO FILE
C               MDEL  - DELETE A MACRO
C               MKEY  - CHANGE A MACRO KEY
C
C       AND ALL MACRO LMEDIT COMMANDS:
C
C       LMEDIT,FL,BT,EX,FL,GO,LO,RE,TP
C       ,QU,QUIT, AND NEXT
C
        IF(F2.EQ.1) THEN
      WC1=WC
      WQ1=WQ
      CALL CONTC(WC1,WQ1,IS)
                        IF(IS) THEN
      OUTLYNE='INVALID COMMAND USED IN MACRO CREATION'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        END IF
C
C       FOR THE MACRO EDIT LEVEL A FEW COMMANDS
C       ARE EXECUTABLE. THEY ARE:
C
C               BT -  GO TO BOTTOM OF MACRO
C               DE -  DELETE  MACRO LINES
C               EX -  EXTRACT ANOTHER MACRO INTO THE
C                     CURRENT MACRO
C               FL -  STOP EDITING AND FILE THE MACRO
C               GO -  GO TO A SPECIFIED MACRO LINE
C             LO C -  LOCATE THE NEXT OCCURENCE OF A COMMAND
C                     WORD
C             LO Q -  LOCATE THE NEXT OCCURENCE OF A QUALIFIER
C                     WORD
C            LO CQ -  LOCATE THE NEXT OCCURENCE OF THE COMMAND
C                     AND QUALIFIER WORD.
C           LO COQ -  LOCATE THE NEXT OCCURENCE OT THE COMMAND
C                     OR QUALIFIER WORD.
C               NEXT- PROCEED WITH SEARCH FURTHER DOWN MACRO.
C       QUIT OR QU -  QUIT WITHOUT CHANGING MACRO FILE(MAC.DAT)
C               RE -  REPLACE CURRENT LINE WITH WHAT FOLLOWS
C               TP -  GO TO TOP OF THE MACRO
C
C       AN INPUT LINE IN THE LMEDIT MODE BEGINNING WITH ANY
C       OTHER WORD IS CONSIDERED THE NEXT LINE TO BE INPUT
C       AS IN MACRO INPUT MODE. IT IS EITHER APPENDED OR INSERTED
C       DEPENDING ON WHETHER WE ARE AT THE END OF THE CURRENT
C       MACRO OR NOT. THE TESTING FOR WHETHER OR NOT TO
C       EXECUTE A COMMAND OR NOT IS DONE IN THE MEDIT SUBROUTINE.
C
C       THE ONLY FORBIDDEN COMMANDS IN THE MEDIT MODE ARE:
C
C               MACRO,EOM,IMF,AND LMEDIT
C
        IF(F3.EQ.1) THEN
      WC1=WC
      CALL CONTD(WC1,IS)
                IF(IS) THEN
      OUTLYNE='INVALID LMEDIT COMMAND'
      CALL SHOWIT(1)
        CALL MACFAL
        RETURN
                END IF
                END IF
C       THE ONLY FORBIDDEN COMMANDS IN THE MACRO INPUT MODE ARE:
C
C               MACRO,IMF,LMEDIT AND FL
C
        IF(F2.EQ.1) THEN
      WC1=WC
      CALL CONTE(WC1,IS)
                IF(IS) THEN
      OUTLYNE='INVALID MACRO INPUT COMMAND'
      CALL SHOWIT(1)
        CALL MACFAL
        RETURN
                END IF
                END IF
C
C       OTHER PROGRAM LEVELS MAY ONLY ALLOW A SMALL
C       NUMBER OF COMMANDS. TESTS AT THESE LEVELS WILL
C       TEST FOR ALLOWED VICE DISALLOWED COMMANDS.
C
        IF(F3.EQ.1) THEN
C               WE ARE IN MACRO EDITING (LMEDIT). THE SUBROUTINE
C               MMEDIT AND MACEDT AND MACMOD CONTAIN THE RULES FOR
C               RESPONDING TO INPUT DURING THIS MODE.SUBROUTINE
C               MMEDIT WAS CALLED WHEN F1=1. IT CALLED SUBROUTINE
C               MACEDT.
C       HERE WE CALL SUBROUTINE MACMOD
        CALL MACMOD
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                        RETURN
        END IF
        !WRITE(OUTLYNE, *), "F5 = ", F5, " F10 = ", F10, " F11 = ", F11
        !CALL SHOWIT(19)
C
        IF(F5.EQ.1.AND.F10.EQ.0.OR.F5.EQ.1.AND.F11.EQ.0) THEN
          OUTLYNE = "CONTRO IN LENS INPUT ROUTINE"
          CALL SHOWIT(19)
C               WE ARE IN THE LENS INPUT ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE LENS INPUT LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN LENS INPUT MODE. THE SUBROUTINE
C               LENIN AND LENNS CONTAIN RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
      WC1=WC
      WS1=WS
      SST1=SST
      CALL CONTF(WC1,WS1,SST1,IS)
                IF(IS) THEN
      OUTLYNE = "ABOUT TO CALL LENIN"
      CALL SHOWIT(19)
                CALL LENIN
                call curr_lens_data%update()
      OUTLYNE = "GOT BACK FROM LENIN "
      CALL SHOWIT(19)
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                END IF
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
      OUTLYNE='INVALID LENS INPUT COMMAND'
      CALL SHOWIT(1)
      OUTLYNE=INPUT
      CALL SHOWIT(1)
      OUTLYNE = "LINE 4132"
      CALL SHOWIT(19)
                        CALL MACFAL
                        RETURN
                END IF

C
 !111  FORMAT('F6,F10,F11', I2,I2,I2)
 !112  FORMAT('F1,F2,F3,F4,F5,F6', I2,I2,I2,I2,I2,I2)
 !113  FORMAT('F7,F8,F9,F10,F11,F12', I2,I2,I2,I2,I2,I2)
 !114  FORMAT('F13,F14,F15,F16,F17,F18', I2,I2,I2,I2,I2,I2)
        !PRINT *, "F6,F10,F11"
        ! WRITE(OUTLYNE, 112) F1,F2,F3,F4,F5,F6
        ! PRINT *, OUTLYNE
        ! CALL SHOWIT(19)
        !
        ! WRITE(OUTLYNE, 113) F7,F8,F9,F10,F11,F12
        ! PRINT *, OUTLYNE
        ! CALL SHOWIT(19)
        !
        ! WRITE(OUTLYNE, 114) F13,F14,F15,F16,F17,F18
        ! PRINT *, OUTLYNE
        ! CALL SHOWIT(19)

        !PRINT *, "WC is ", WC


        IF(F6.EQ.1.AND.F10.EQ.0.OR.F6.EQ.1.AND.F11.EQ.0) THEN
          !PRINT *, "LENS UPDATE ROUTINE"
C
C               WE ARE IN THE LENS UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE LENS UPDATE LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C
C               WE ARE IN LENS UPDATE MODE. THE SUBROUTINE
C               LENUP AND ULENNS CONTAIN RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
C
C       THIS IS THE LIST OF VALID LENS UPDATE COMMANDS
      WC1=WC
      WS1=WS
      SST1=SST
      CALL CONTG(WC1,WS1,SST1,IS)
                        IF(IS) THEN
                ! If command is a codeV command then call it here as the cmd
                ! will need to be translated to something that will be accessed
                ! in lenup
                if (isCodeVCommand(WC)) then 
                   if(startCodeVLensUpdateCmd(WC)) then
                     return
                   end if
                  end if

                CALL LENUP
      !PRINT *, "F31 = ", F31
      !PRINT *, "F28 = ", F28
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
      IF(WC.EQ.'EOS') YESEOS=.TRUE.
      IF(WC.NE.'EOS') YESEOS=.FALSE.
C     CALL RE_DISPLAY_LENS(YESEOS)
      call curr_lens_data%update()
      call curr_asph_data%updateAsphereTable(INT(SYSTEM(20))+1)
      call sysConfig%updateParameters()
      if (c_associated(lens_editor_window)) then
        call buildBasicTable(.FALSE.)
        call buildAsphereTable(.FALSE.)
      end if   
      PRINT *, "IN EOS in CONTRO"
      call zoatabMgr%replotifneeded()      
               RETURN
               END IF
        IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
      OUTLYNE='INVALID LENS UPDATE COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F7.EQ.1.AND.F10.EQ.0.OR.F7.EQ.1.AND.F11.EQ.0) THEN
          PRINT *, "SPSRF INPUT ROUTINE"
C               WE ARE IN THE SPSRF INPUT ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN SPSRF INPUT MODE. THE SUBROUTINE
C               SPSIN AND SPIN2 AND SUBROUTINES THEY CALL
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
      WC1=WC
      WQ1=WQ
      CALL CONTH(WC1,WQ1,IS)
                IF(IS) THEN
                CALL SPSRF2(1)
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                END IF
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
      OUTLYNE='INVALID SPSRF INPUT COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
C
        IF(F8.EQ.1.AND.F10.EQ.0.OR.F8.EQ.1.AND.F11.EQ.0) THEN
          PRINT *, "SPSRF UPDATE ROUTINE"
C               WE ARE IN THE SPSRF UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN SPSRF UPDATE MODE. THE SUBROUTINE
C               SPSUP,SPUP2 AND AND SUBROUTINES THEY CALL
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
      WC1=WC
      WQ1=WQ
                CALL CONTI(WC1,WQ1,IS)
                IF(IS) THEN
                CALL SPSRF2(2)
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                END IF
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
      OUTLYNE='INVALID SPSRF UPDATE COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F9.EQ.1.AND.F10.EQ.0.OR.F9.EQ.1.AND.F11.EQ.0) THEN
          PRINT *, "SPFIT INPUT ROUTINE"
C               WE ARE IN THE SPFIT INPUT ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN SPFIT  MODE. THE SUBROUTINE
C               SPFIT AND SPFIT2 AND THE SUBROUTINES THEY CALL
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
                WC1=WC
                        CALL CONTJ(WC1,IS)
                IF(IS) THEN
                CALL SPFIT2
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
      OUTLYNE='INVALID SPFIT INPUT COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
        IF(F27.EQ.1.OR.F27.EQ.2) THEN
          PRINT *, "MERIT INPUT OR UPDATE ROUTINE"
C               WE ARE IN THE MERIT INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN MERIT INPUT OR UPDATE MODE. THE SUBROUTINE
C               MERIT1.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
                WC1=WC
                CALL CONTK(WC1,IS)
                IF(IS) THEN
                CALL MERIT1
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F27.EQ.1) THEN
      OUTLYNE='INVALID (MERIT INPUT) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F27.EQ.2) THEN
      OUTLYNE='INVALID (MERIT UPDATE) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                END IF
        IF(F53.EQ.1.OR.F53.EQ.2) THEN
          PRINT *, "TOPER INPUT OR UPDATE ROUTINE"
C               WE ARE IN THE TOPER INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN TOPER INPUT OR UPDATE MODE. THE SUBROUTINE
C               MCOMP.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
                WC1=WC
                CALL CONTL(WC1,IS)
                IF(IS) THEN
                        CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
                        CURFIG=1

                CALL TOPER1
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F53.EQ.1) THEN
      OUTLYNE='INVALID (TOPER INPUT) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F53.EQ.2) THEN
      OUTLYNE='INVALID (TOPER UPDATE) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                END IF
        IF(F54.EQ.1.OR.F54.EQ.2) THEN
          PRINT *, "FOCRIT INPUT OR UPDATE"
C               WE ARE IN THE FOCRIT INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               FOCRIT1.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
                WC1=WC
                CALL CONTM(WC1,IS)
                IF(IS) THEN
C
                        CORMOD=1
C       INITIALIZE THE CURRENT CONFIGURATION NUMBER TO 1
                        CURFIG=1
                CALL FOCRIT1
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F54.EQ.1) THEN
      OUTLYNE='INVALID (FOCRIT INPUT) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F54.EQ.2) THEN
      OUTLYNE='INVALID (FOCRIT UPDATE) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                END IF
C
        IF(F29.EQ.1.OR.F29.EQ.2) THEN
          PRINT *, "VAR INPUT OR UPDATE"
C               WE ARE IN THE VARIABLE INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN VARIABLE INPUT OR UPDATE MODE. THE SUBROUTINE
C               VARBLE.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
      WC1=WC
      FF29=F29
                CALL CONTN(WC1,FF29,IS)
                IF(IS) THEN
               IF(WQ.EQ.'ALL'.AND.SQ.EQ.1) THEN
               WQ='        '
               SQ=0
               OW4=W4
               OW3=W3
               OW2=W2
               OW1=W1
               OS4=S4
               OS3=S3
               OS2=S2
               OS1=S1
               ODF4=DF4
               ODF3=DF3
               ODF2=DF2
               ODF1=DF1
                       DO I=0,INT(SYSTEM(20))
      IF(WC.EQ.'CV'.AND.I.EQ.0.OR.WC.EQ.'CVTOR'.AND.I.EQ.0) GO TO 67
      IF(WC.EQ.'RD'.AND.I.EQ.0) GO TO 67
      IF(WC.EQ.'RDTOR'.AND.I.EQ.0) GO TO 67
      IF(WC.EQ.'CV'.AND.I.EQ.INT(SYSTEM(20))
     1.OR.WC.EQ.'CVTOR'.AND.I.EQ.INT(SYSTEM(20))) GO TO 67
      IF(WC.EQ.'RD'.AND.I.EQ.INT(SYSTEM(20))
     3.OR.WC.EQ.'RDTOR'.AND.I.EQ.INT(SYSTEM(20))) GO TO 67
      IF(DUMMMY(I).AND.WC.EQ.'TH') GO TO 67
      IF(DUMMMY(I).AND.WC.EQ.'CV') GO TO 67
      IF(DUMMMY(I).AND.WC.EQ.'RD') GO TO 67
      IF(DUMMMY(I).AND.WC.EQ.'CVTOR') GO TO 67
      IF(DUMMMY(I).AND.WC.EQ.'RDTOR') GO TO 67
      IF(WC.EQ.'TH'.AND.I.EQ.INT(SYSTEM(20))) GO TO 67
      IF(WC.EQ.'TH'.AND.I.EQ.0) GO TO 67
      IF(WC.EQ.'N1'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N2'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N3'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N4'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N5'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N6'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N7'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N8'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N9'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'N10'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 67
      IF(WC.EQ.'INDEX'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 67
      IF(WC.EQ.'VNUM'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 67
      IF(WC.EQ.'DPART'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 67
      IF(SYSTEM(27).EQ.1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
      IF(I.EQ.1.OR.I.EQ.2) THEN
      IF(WC.EQ.'TH') GO TO 67
                   END IF
                   END IF
      IF(SYSTEM(27).EQ.-1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
      IF(I.EQ.INT(SYSTEM(20))-1.OR.I.EQ.INT(SYSTEM(20))-2) THEN
      IF(WC.EQ.'TH') GO TO 67
                   END IF
                   END IF
               W5=OW4
               W4=OW3
               W3=OW2
               W2=OW1
               S5=OS4
               S4=OS3
               S3=OS2
               S2=OS1
               DF5=ODF4
               DF4=ODF3
               DF3=ODF2
               DF2=ODF1
                   W1=DBLE(I)
                   S1=1
                   DF1=0
                CALL VARBLL
 67                    CONTINUE
                       END DO
                       ELSE
                CALL VARBLL
                       END IF
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                END IF
C
      WC1=WC
                CALL CONTO(WC1,IS)
                IF(IS) THEN
               IF(WQ.EQ.'ALL'.AND.SQ.EQ.1) THEN
               WQ='        '
               SQ=0
               OW4=W4
               OW3=W3
               OW2=W2
               OW1=W1
               OS4=S4
               OS3=S3
               OS2=S2
               OS1=S1
               ODF4=DF4
               ODF3=DF3
               ODF2=DF2
               ODF1=DF1
                       DO I=0,INT(SYSTEM(20))
      IF(WC.EQ.'CV'.AND.I.EQ.0.OR.WC.EQ.'CVTOR'.AND.I.EQ.0) GO TO 68
      IF(WC.EQ.'RD'.AND.I.EQ.0) GO TO 68
      IF(WC.EQ.'RDTOR'.AND.I.EQ.0) GO TO 68
      IF(WC.EQ.'CV'.AND.I.EQ.INT(SYSTEM(20))
     1.OR.WC.EQ.'CVTOR'.AND.I.EQ.INT(SYSTEM(20))) GO TO 68
      IF(WC.EQ.'RD'.AND.I.EQ.INT(SYSTEM(20))
     3.OR.WC.EQ.'RDTOR'.AND.I.EQ.INT(SYSTEM(20))) GO TO 68
      IF(DUMMMY(I).AND.WC.EQ.'TH') GO TO 68
      IF(DUMMMY(I).AND.WC.EQ.'CV') GO TO 68
      IF(DUMMMY(I).AND.WC.EQ.'RD') GO TO 68
      IF(DUMMMY(I).AND.WC.EQ.'CVTOR') GO TO 68
      IF(DUMMMY(I).AND.WC.EQ.'RDTOR') GO TO 68
      IF(WC.EQ.'TH'.AND.I.EQ.0) GO TO 68
      IF(WC.EQ.'TH'.AND.I.EQ.INT(SYSTEM(20))) GO TO 68
      IF(WC.EQ.'N1'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N2'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N3'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N4'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N5'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N6'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N7'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N8'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N9'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'N10'.AND.GLANAM(I,1).NE.'GLASS      ') GO TO 68
      IF(WC.EQ.'INDEX'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 68
      IF(WC.EQ.'VNUM'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 68
      IF(WC.EQ.'DPART'.AND.GLANAM(I,1).NE.'MODEL        ') GO TO 68
      IF(SYSTEM(27).EQ.1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
      IF(I.EQ.1.OR.I.EQ.2) THEN
      IF(WC.EQ.'TH') GO TO 68
                   END IF
                   END IF
      IF(SYSTEM(27).EQ.-1.0D0.OR.SYSTEM(27).EQ.2.0D0) THEN
      IF(I.EQ.INT(SYSTEM(20))-1.OR.I.EQ.INT(SYSTEM(20))-2) THEN
      IF(WC.EQ.'TH') GO TO 68
                   END IF
                   END IF
               W5=OW4
               W4=OW3
               W3=OW2
               W2=OW1
               S5=OS4
               S4=OS3
               S3=OS2
               S2=OS1
               DF5=ODF4
               DF4=ODF3
               DF3=ODF2
               DF2=ODF1
                   W1=DBLE(I)
                   S1=1
                   DF1=0
                CALL VARBLL
 68                    CONTINUE
                       END DO
                       ELSE
                CALL VARBLL
                       END IF
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F29.EQ.1) THEN
      OUTLYNE='INVALID (VARIABLE INPUT) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F29.EQ.2) THEN
      OUTLYNE='INVALID (VARIABLE UPDATE) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                END IF
C
        IF(F51.EQ.1.OR.F51.EQ.2) THEN
          PRINT *, "TVAR INPUT OR UPDATE"
C               WE ARE IN THE TVAR INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN VARIABLE INPUT OR UPDATE MODE. THE SUBROUTINE
C               VARBLE.FOR
C                CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
                WC1=WC
                FF51=F51
                CALL CONTP(WC1,FF51,IS)
                IF(IS) THEN
                CALL TVARBLL
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F51.EQ.1) THEN
      OUTLYNE='INVALID (TVAR INPUT) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F51.EQ.2) THEN
      OUTLYNE='INVALID (TVAR UPDATE) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                END IF
        IF(F52.EQ.1.OR.F52.EQ.2) THEN
          PRINT *, "COMPVAR INPUT OR UPDATE"
C               WE ARE IN THE COMPVAR INPUT OR UPDATE ROUTINE
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THIS LEVEL. TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C               WE ARE IN VARIABLE INPUT OR UPDATE MODE.
      WC1=WC
      FF52=F52
      CALL CONTQ(WC1,FF52,IS)
                IF(IS) THEN
                CALL CVARBLL
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                ELSE
       IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F52.EQ.1) THEN
      OUTLYNE='INVALID (COMPVAR INPUT) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F52.EQ.2) THEN
      OUTLYNE='INVALID (COMPVAR UPDATE) COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                END IF
C
        IF(F10.EQ.1.OR.F11.EQ.1) THEN
          PRINT *, "CONFIGS INPUT OR UPDATE ROUTINE"
C
C               WE ARE IN THE CONFIGS INPUT (F10=1) OR
C       CONFIGS UPDATE (F11=0) ROUTINE
C
C
C       THERE ARE A LIMITED NUMBER OF VALID COMMANDS WHICH MAY BE
C       ENTERED AT THE CONFIGS INPUT OR UPDATE LEVEL.
C       TEST FOR THOSE VALID COMMANDS.
C       IF THE COMMAND IS VALID, THEN PROCEED. IF NOT THEN SKIP
C       TO THE NEXT INSTRUCTION.
C
C               WE ARE IN CONFIGS INPUT LEVEL. THE SUBROUTINE
C        CFGIN,CFGUP  AND CFGIN2 CONTAIN RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
C       ALL OF THE COMMAND VALID AT THE UPDATE LENS LEVEL ARE
C       VALID IN CONFIGS AND UPDATE CONFIGS
C       THIS IS THE LIST OF VALID LENS UPDATE COMMANDS
      WC1=WC
      WQ1=WQ
      SQ1=SQ
                CALL CONTR(WC1,WQ1,SQ1,IS)
                        IF(IS) THEN
                RET=0
                CALL CFGIN2
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXCVAR
      IF(WC.EQ.'EOS'.AND.F31.EQ.0.AND.F28.EQ.0) CALL FIXTVAR
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
                RETURN
                END IF
        IF(WC.EQ.'?') THEN
        CALL QUERRYY
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        IF(WC.EQ.' '.AND.F50.EQ.1) THEN
        CALL BLANK
        F50=0
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        ELSE
        IF(WC.EQ.' '.AND.F50.NE.1) THEN
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
        END IF
        END IF
        IF(F1.EQ.0.AND.F10.EQ.1) THEN
      OUTLYNE='INVALID CONFIGS INPUT COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
        IF(F1.EQ.0.AND.F11.EQ.1) THEN
      OUTLYNE='INVALID CONFIGS UPDATE COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
C       CHECK FOR VALID QUALIFIERS FOR UPDATE OR U
C       ISSUED AT THE CMD LEVEL
      IF(F1.EQ.1.AND.F17.EQ.0.AND.WC.EQ.'U') THEN
        !PRINT *, "CHECK FOR VALID QUALIFIERS"
      IF(WQ.NE.'LENS'.AND.WQ.NE.'SPSRF'.AND.
     1WQ.NE.'CONFIGS'.AND.WQ.NE.'CONFIG'.AND.WQ
     2.NE.'VARIABLE'.AND.WQ.NE.'MERIT'.AND.WQ.NE.
     3'TVAR'.AND.WQ.NE.'COMPVAR'.AND.WQ.NE.'TOPER'.AND.WQ.NE.'FOCRIT'
     4.AND.WQ.NE.'L'.AND.WQ.NE.'SP'.AND.WQ.NE.'VB'.AND.
     5WQ.NE.'CF'.AND.WQ.NE.'RS'.AND.WQ.NE.'M'.AND.WQ.NE.'TVB'
     6.AND.WQ.NE.'CMP'.AND.WQ.NE.'TOP'.AND.WQ.NE.'FC') THEN
      OUTLYNE='INVALID CMD LEVEL COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
                IF(F1.EQ.1.AND.F17.EQ.1.AND.WC.EQ.'U') THEN
      OUTLYNE='INVALID SPECT LEVEL COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
      IF(F1.EQ.1..AND.F17.EQ.0.AND.WC.EQ.'UPDATE') THEN
        PRINT *, "UPDATE"
      IF(WQ.NE.'LENS'.AND.WQ.NE.'SPSRF'.AND.
     1WQ.NE.'CONFIGS'.AND.WQ.NE.'CONFIG'.AND.WQ
     2.NE.'VARIABLE'.AND.WQ.NE.'MERIT'.AND.WQ.NE.
     3'TVAR'.AND.WQ.NE.'COMPVAR'.AND.WQ.NE.'TOPER'.AND.WQ.NE.'FOCRIT'
     4.AND.WQ.NE.'L'.AND.WQ.NE.'SP'.AND.WQ.NE.'VB'.AND.
     5WQ.NE.'CF'.AND.WQ.NE.'RS'.AND.WQ.NE.'M'.AND.WQ.NE.'TVB'
     6.AND.WQ.NE.'CMP'.AND.WQ.NE.'TOP'.AND.WQ.NE.'FC') THEN
      OUTLYNE='INVALID CMD LEVEL COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
                END IF
        IF(F1.EQ.1.AND.F17.EQ.1.AND.WC.EQ.'UPDATE') THEN
      OUTLYNE='INVALID SPECT LEVEL COMMAND'
      CALL SHOWIT(1)
                CALL MACFAL
                RETURN
                END IF
C
        IF(F2.EQ.1) THEN
C               WE ARE IN MACRO CREATION MODE. THE SUBROUTINE
C               MACIN CONTAINS RULES FOR RESPONDING TO INPUT
C               DURING THIS MODE.
        WWW1=0.0D0
        CALL MACIN(WWW1)
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
C       F2 NOT 1,PROCEED
        END IF
C
C       FOR THE CMD LEVEL COMMANDS F1=1, SO NOW CALL CMDER
        CALL CMDER
      LASTCOMWRD=WC
      LASTWASFOB=.FALSE.
      IF(LASTCOMWRD.EQ.'FOB     ') LASTWASFOB=.TRUE.
        RETURN
                                END
      SUBROUTINE CONTA(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
      IS=.FALSE.
                IF(WC.EQ.'START')               IS=.TRUE.
                IF(WC.EQ.'WAVLN')               IS=.TRUE.
                IF(WC.EQ.'INT')                 IS=.TRUE.
                IF(WC.EQ.'DIRECT')              IS=.TRUE.
                IF(WC.EQ.'INSERT')              IS=.TRUE.
C               IF(WC.EQ.'LOADPHOT')            IS=.TRUE.
C               IF(WC.EQ.'LOADSCOT')            IS=.TRUE.
                IF(WC.EQ.'DROP')                IS=.TRUE.
                IF(WC.EQ.'DELETE')              IS=.TRUE.
                IF(WC.EQ.'GETFILE')             IS=.TRUE.
                IF(WC.EQ.'BLACKBDY')            IS=.TRUE.
                IF(WC.EQ.'PHOTOPIC')            IS=.TRUE.
                IF(WC.EQ.'SCOTOPIC')            IS=.TRUE.
                IF(WC.EQ.'PUT')                 IS=.TRUE.
                IF(WC.EQ.'LIST')                IS=.TRUE.
                IF(WC.EQ.'RENAME')              IS=.TRUE.
                IF(WC.EQ.'PUNCH')               IS=.TRUE.
                IF(WC.EQ.'INTER')               IS=.TRUE.
                IF(WC.EQ.'NARCIN')              IS=.TRUE.
                IF(WC.EQ.'FLNAME')              IS=.TRUE.
                IF(WC.EQ.'ENDTABLE')            IS=.TRUE.
                IF(WC.EQ.'NARC')                IS=.TRUE.
                IF(WC.EQ.'EOS')                 IS=.TRUE.
                IF(WC.EQ.'DATA')                IS=.TRUE.
                IF(WC.EQ.'CUME')                IS=.TRUE.
                IF(WC.EQ.'WFACTOR')             IS=.TRUE.
                IF(WC.EQ.'WORK')                IS=.TRUE.
                IF(WC.EQ.'PTABLE')              IS=.TRUE.
                IF(WC.EQ.'DIR')                 IS=.TRUE.
                IF(WC.EQ.'FILE')                IS=.TRUE.
                IF(WC.EQ.'NAME')                IS=.TRUE.
                IF(WC.EQ.'PLOTR')               IS=.TRUE.
                IF(WC.EQ.'PLOTT')               IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTB(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                IS=.FALSE.
                IF(WC.EQ.'EXIT')                IS=.TRUE.
                IF(WC.EQ.'EXI')                 IS=.TRUE.
                IF(WC.EQ.'LENS')                IS=.TRUE.
                IF(WC.EQ.'CONFIGS')             IS=.TRUE.
                IF(WC.EQ.'CONFIG')              IS=.TRUE.
                IF(WC.EQ.'SPSRF')               IS=.TRUE.
                IF(WC.EQ.'SPFIT')               IS=.TRUE.
                IF(WC.EQ.'SPECT')               IS=.TRUE.
                IF(WC.EQ.'MACRO')               IS=.TRUE.
                IF(WC.EQ.'LMEDIT')              IS=.TRUE.
                IF(WC.EQ.'UPDATE')              IS=.TRUE.
                IF(WC.EQ.'U')                   IS=.TRUE.
                IF(WC.EQ.'TOLER')               IS=.TRUE.
                IF(WC.EQ.'FOE')                 IS=.TRUE.
                IF(WC.EQ.'VARIABLE')            IS=.TRUE.
                IF(WC.EQ.'VARI')                IS=.TRUE.
                IF(WC.EQ.'MERIT')               IS=.TRUE.
                IF(WC.EQ.'TVAR')                IS=.TRUE.
                IF(WC.EQ.'COMPVAR')             IS=.TRUE.
                IF(WC.EQ.'LAYOUT')              IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTC(WC,WQ,IS)
      IMPLICIT NONE
      CHARACTER WC*8,WQ*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'IMF')         IS=.TRUE.
                IF(WC.EQ.'MACRO')       IS=.TRUE.
                IF(WC.EQ.'MDEL')        IS=.TRUE.
                IF(WC.EQ.'MKEY')        IS=.TRUE.
                IF(WC.EQ.'LMEDIT')      IS=.TRUE.
                IF(WC.EQ.'FL')          IS=.TRUE.
                IF(WC.EQ.'BT')          IS=.TRUE.
                IF(WC.EQ.'EX')          IS=.TRUE.
                IF(WC.EQ.'GO')          IS=.TRUE.
                IF(WC.EQ.'GOQUIET')     IS=.TRUE.
      IF(WC.EQ.'LO'.AND.WQ.EQ.'Q')      IS=.TRUE.
      IF(WC.EQ.'LO'.AND.WQ.EQ.'C')      IS=.TRUE.
      IF(WC.EQ.'LO'.AND.WQ.EQ.'CQ')     IS=.TRUE.
      IF(WC.EQ.'LO'.AND.WQ.EQ.'COQ')    IS=.TRUE.
                IF(WC.EQ.'RE')          IS=.TRUE.
                IF(WC.EQ.'TP')          IS=.TRUE.
                IF(WC.EQ.'QU')          IS=.TRUE.
                IF(WC.EQ.'QUIT')        IS=.TRUE.
                IF(WC.EQ.'NEXT')        IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTD(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'MACRO')             IS=.TRUE.
                IF(WC.EQ.'EOM')               IS=.TRUE.
                IF(WC.EQ.'IMF')               IS=.TRUE.
                IF(WC.EQ.'LMEDIT')            IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTE(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'MACRO')             IS=.TRUE.
                IF(WC.EQ.'FL')                IS=.TRUE.
                IF(WC.EQ.'IMF')               IS=.TRUE.
                IF(WC.EQ.'LMEDIT')            IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTF(WC,WS,SST,IS)
      use glass_manager
      IMPLICIT NONE
      CHARACTER WC*8,WS*80
      INTEGER SST
      LOGICAL IS
                        IS=.FALSE.
                PRINT *, "Checking ", WC
C       THIS IS THE LIST OF VALID LENS INPUT COMMANDS
                IF(gdb%isNameInCatalog(WC)) IS=.TRUE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'LI')          IS=.TRUE.
                IF(WC.EQ.'MFG')         IS=.TRUE.
                IF(WC.EQ.'CATNUM')      IS=.TRUE.
                IF(WC.EQ.'LABEL')       IS=.TRUE.
                IF(WC.EQ.'LBL')         IS=.TRUE.
                IF(WC.EQ.'LIC')         IS=.TRUE.
                IF(WC.EQ.'INI')         IS=.TRUE.
                IF(WC.EQ.'LTYPE')       IS=.TRUE.
                IF(WC.EQ.'MODE')        IS=.TRUE.
                IF(WC.EQ.'SPTWT')       IS=.TRUE.
                IF(WC.EQ.'NODUM')       IS=.TRUE.
                IF(WC.EQ.'SPTWT2')      IS=.TRUE.
                IF(WC.EQ.'WV')          IS=.TRUE.
                IF(WC.EQ.'WV2')         IS=.TRUE.
                IF(WC.EQ.'UNITS')       IS=.TRUE.
                IF(WC.EQ.'PCW')         IS=.TRUE.
                IF(WC.EQ.'SCW')         IS=.TRUE.
                IF(WC.EQ.'CW')          IS=.TRUE.
                IF(WC.EQ.'SAY')         IS=.TRUE.
                IF(WC.EQ.'SAX')         IS=.TRUE.
                IF(WC.EQ.'WRX')         IS=.TRUE.
                IF(WC.EQ.'WRY')         IS=.TRUE.
                IF(WC.EQ.'BDX')         IS=.TRUE.
                IF(WC.EQ.'BDY')         IS=.TRUE.
                IF(WC.EQ.'NAOY')        IS=.TRUE.
                IF(WC.EQ.'NAOX')        IS=.TRUE.
                IF(WC.EQ.'FNOY')        IS=.TRUE.
                IF(WC.EQ.'FNOX')        IS=.TRUE.
                IF(WC.EQ.'SCY')         IS=.TRUE.
                IF(WC.EQ.'SCX')         IS=.TRUE.
                IF(WC.EQ.'PYIM')        IS=.TRUE.
                IF(WC.EQ.'PXIM')        IS=.TRUE.
                IF(WC.EQ.'RYIM')        IS=.TRUE.
                IF(WC.EQ.'RXIM')        IS=.TRUE.
                IF(WC.EQ.'CV')          IS=.TRUE.
                IF(WC.EQ.'CVTOR')       IS=.TRUE.
                IF(WC.EQ.'GRT')         IS=.TRUE.
                IF(WC.EQ.'GRO')         IS=.TRUE.
                IF(WC.EQ.'GRS')         IS=.TRUE.
                IF(WC.EQ.'GRX')         IS=.TRUE.
                IF(WC.EQ.'GRY')         IS=.TRUE.
                IF(WC.EQ.'GRZ')         IS=.TRUE.
                IF(WC.EQ.'RD')          IS=.TRUE.
                IF(WC.EQ.'RDTOR')       IS=.TRUE.
                IF(WC.EQ.'REAL')        IS=.TRUE.
                IF(WC.EQ.'PARAX')       IS=.TRUE.
                IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
                IF(WC.EQ.'PIVOT')       IS=.TRUE.
                IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
                IF(WC.EQ.'CC')          IS=.TRUE.
                IF(WC.EQ.'CCTOR')       IS=.TRUE.
                IF(WC.EQ.'ARRAY')       IS=.TRUE.
                IF(WC.EQ.'ASPH')        IS=.TRUE.
                IF(WC.EQ.'ASPH2')       IS=.TRUE.
                IF(WC.EQ.'TASPH')       IS=.TRUE.
                IF(WC.EQ.'AC')          IS=.TRUE.
                IF(WC.EQ.'AD')          IS=.TRUE.
                IF(WC.EQ.'AE')          IS=.TRUE.
                IF(WC.EQ.'AF')          IS=.TRUE.
                IF(WC.EQ.'AG')          IS=.TRUE.
                IF(WC.EQ.'AH')          IS=.TRUE.
                IF(WC.EQ.'AI')          IS=.TRUE.
                IF(WC.EQ.'AJ')          IS=.TRUE.
                IF(WC.EQ.'AK')          IS=.TRUE.
                IF(WC.EQ.'AL')          IS=.TRUE.
                IF(WC.EQ.'ADTOR')       IS=.TRUE.
                IF(WC.EQ.'AETOR')       IS=.TRUE.
                IF(WC.EQ.'AFTOR')       IS=.TRUE.
                IF(WC.EQ.'AGTOR')       IS=.TRUE.
                IF(WC.EQ.'YTORIC')      IS=.TRUE.
                IF(WC.EQ.'XTORIC')      IS=.TRUE.
                IF(WC.EQ.'NORMAL')      IS=.TRUE.
                IF(WC.EQ.'SPIDER')      IS=.TRUE.
                IF(WC.EQ.'APY')         IS=.TRUE.
                IF(WC.EQ.'APX')         IS=.TRUE.
                IF(WC.EQ.'PIY')         IS=.TRUE.
                IF(WC.EQ.'PIX')         IS=.TRUE.
                IF(WC.EQ.'PUY')         IS=.TRUE.
                IF(WC.EQ.'PUX')         IS=.TRUE.
                IF(WC.EQ.'APCY')        IS=.TRUE.
                IF(WC.EQ.'APCX')        IS=.TRUE.
                IF(WC.EQ.'PICY')        IS=.TRUE.
                IF(WC.EQ.'PICX')        IS=.TRUE.
                IF(WC.EQ.'PUCY')        IS=.TRUE.
                IF(WC.EQ.'PUCX')        IS=.TRUE.
                IF(WC.EQ.'COCY')        IS=.TRUE.
                IF(WC.EQ.'COCX')        IS=.TRUE.
                IF(WC.EQ.'PIKUP')       IS=.TRUE.
                IF(WC.EQ.'DEC')         IS=.TRUE.
                IF(WC.EQ.'YD')          IS=.TRUE.
                IF(WC.EQ.'XD')          IS=.TRUE.
                IF(WC.EQ.'ZD')          IS=.TRUE.
                IF(WC.EQ.'PIVY')        IS=.TRUE.
                IF(WC.EQ.'PIVX')        IS=.TRUE.
                IF(WC.EQ.'PIVZ')        IS=.TRUE.
                IF(WC.EQ.'TILT')        IS=.TRUE.
                IF(WC.EQ.'RTILT')       IS=.TRUE.
                IF(WC.EQ.'ALPHA')       IS=.TRUE.
                IF(WC.EQ.'BETA')        IS=.TRUE.
                IF(WC.EQ.'GAMMA')       IS=.TRUE.
                IF(WC.EQ.'GDX     ')    IS=.TRUE.
                IF(WC.EQ.'GDY     ')    IS=.TRUE.
                IF(WC.EQ.'GDZ     ')    IS=.TRUE.
                IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
                IF(WC.EQ.'GBETA   ')    IS=.TRUE.
                IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
                IF(WC.EQ.'ASTOP')       IS=.TRUE.
                IF(WC.EQ.'REFS')        IS=.TRUE.
                IF(WC.EQ.'TH')          IS=.TRUE.
                IF(WC.EQ.'THM')         IS=.TRUE.
                IF(WC.EQ.'PRICE')       IS=.TRUE.
                IF(WC.EQ.'AUTOFUNC')    IS=.TRUE.
              

C     BEGINNING OF NON-SEQUENTIAL COMMANDS
                IF(WC.EQ.'CCR')         IS=.TRUE.
                IF(WC.EQ.'ROO')         IS=.TRUE.
C     END OF NON-SEQUENTIAL COMMANDS
                IF(WC.EQ.'REDSLV')      IS=.TRUE.               
                IF(WC.EQ.'PY')          IS=.TRUE.
                IF(WC.EQ.'RAYERROR')    IS=.TRUE.
                IF(WC.EQ.'PCY')         IS=.TRUE.
                IF(WC.EQ.'CAY')         IS=.TRUE.
                IF(WC.EQ.'PX')          IS=.TRUE.
                IF(WC.EQ.'PCX')         IS=.TRUE.
                IF(WC.EQ.'CAX')         IS=.TRUE.
                IF(WC.EQ.'CLAP')        IS=.TRUE.
                IF(WC.EQ.'COBS')        IS=.TRUE.
                IF(WC.EQ.'GLAK')         IS=.TRUE.
                IF(WC.EQ.'DEFORM')      IS=.TRUE.
                IF(WC.EQ.'DELDEFOR')    IS=.TRUE.
                IF(WC.EQ.'GLASS')       IS=.TRUE.
                IF(WC.EQ.'N6')          IS=.TRUE.
                IF(WC.EQ.'N7')          IS=.TRUE.
                IF(WC.EQ.'N8')          IS=.TRUE.
                IF(WC.EQ.'N9')          IS=.TRUE.
                IF(WC.EQ.'N10')         IS=.TRUE.
                IF(WC.EQ.'N1')          IS=.TRUE.
                IF(WC.EQ.'N2')          IS=.TRUE.
                IF(WC.EQ.'N3')          IS=.TRUE.
                IF(WC.EQ.'N4')          IS=.TRUE.
                IF(WC.EQ.'N5')          IS=.TRUE.
                IF(WC.EQ.'INDEX')       IS=.TRUE.
                IF(WC.EQ.'VNUM')        IS=.TRUE.
                IF(WC.EQ.'DPART')       IS=.TRUE.
                IF(WC.EQ.'MULTCLAP')    IS=.TRUE.
                IF(WC.EQ.'MULTCOBS')    IS=.TRUE.
                IF(WC.EQ.'SPIDER')      IS=.TRUE.
                IF(WC.EQ.'SPGR')        IS=.TRUE.
                IF(WC.EQ.'MODEL')       IS=.TRUE.
                IF(WC.EQ.'USER')        IS=.TRUE.
                IF(WC.EQ.'GLCAT')       IS=.TRUE.
                IF(WC.EQ.'AIR')         IS=.TRUE.
                IF(WC.EQ.'COATING ')    IS=.TRUE.
                IF(WC.EQ.'REFL')        IS=.TRUE.
                IF(WC.EQ.'REFLTIRO')    IS=.TRUE.
                IF(WC.EQ.'REFLTIR')     IS=.TRUE.
                IF(WC.EQ.'PERFECT')     IS=.TRUE.
                IF(WC.EQ.'IDEAL')       IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'TASPH')       IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'INR')         IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTG(WC,WS,SST,IS)
C     LENS UPDATE
      use codeV_commands, only: isCodeVCommand
      use glass_manager
      IMPLICIT NONE
      CHARACTER WC*8,WS*80
      INTEGER SST
      LOGICAL IS

      !PRINT *, "CONTG ROUTINE"
                        IS=.FALSE.
                IF(isCodeVCommand(WC))  IS=.TRUE. 
                IF(gdb%isNameInCatalog(WC)) IS=.TRUE.  
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'LI')          IS=.TRUE.
                IF(WC.EQ.'MFG')         IS=.TRUE.
                IF(WC.EQ.'CATNUM')      IS=.TRUE.
                IF(WC.EQ.'LABEL')       IS=.TRUE.
                IF(WC.EQ.'LBL')         IS=.TRUE.
                IF(WC.EQ.'LIC')         IS=.TRUE.
                IF(WC.EQ.'INI')         IS=.TRUE.
                IF(WC.EQ.'LTYPE')       IS=.TRUE.
                IF(WC.EQ.'MODE')        IS=.TRUE.
                IF(WC.EQ.'SPTWT')       IS=.TRUE.
                IF(WC.EQ.'NODUM')       IS=.TRUE.
                IF(WC.EQ.'SPTWT2')      IS=.TRUE.
                IF(WC.EQ.'WV')          IS=.TRUE.
                IF(WC.EQ.'WV2')         IS=.TRUE.
                IF(WC.EQ.'UNITS')       IS=.TRUE.
                IF(WC.EQ.'PCW')         IS=.TRUE.
                IF(WC.EQ.'SCW')         IS=.TRUE.
                IF(WC.EQ.'CW')          IS=.TRUE.
                IF(WC.EQ.'SAY')         IS=.TRUE.
                IF(WC.EQ.'SAX')         IS=.TRUE.
                IF(WC.EQ.'WRX')         IS=.TRUE.
                IF(WC.EQ.'WRY')         IS=.TRUE.
                IF(WC.EQ.'BDX')         IS=.TRUE.
                IF(WC.EQ.'BDY')         IS=.TRUE.
                IF(WC.EQ.'NAOY')        IS=.TRUE.
                IF(WC.EQ.'NAOX')        IS=.TRUE.
                IF(WC.EQ.'FNOY')        IS=.TRUE.
                IF(WC.EQ.'FNOX')        IS=.TRUE.
                IF(WC.EQ.'SCY')         IS=.TRUE.
                IF(WC.EQ.'SCX')         IS=.TRUE.
                IF(WC.EQ.'PYIM')        IS=.TRUE.
                IF(WC.EQ.'PXIM')        IS=.TRUE.
                IF(WC.EQ.'RYIM')        IS=.TRUE.
                IF(WC.EQ.'RXIM')        IS=.TRUE.
                IF(WC.EQ.'CV')          IS=.TRUE.
                IF(WC.EQ.'RD')          IS=.TRUE.
                IF(WC.EQ.'CC')          IS=.TRUE.
                IF(WC.EQ.'ASPH')        IS=.TRUE.
                IF(WC.EQ.'ASPH2')       IS=.TRUE.
                IF(WC.EQ.'YTORIC')      IS=.TRUE.
                IF(WC.EQ.'XTORIC')      IS=.TRUE.
                IF(WC.EQ.'NORMAL')      IS=.TRUE.
                IF(WC.EQ.'SPIDER')      IS=.TRUE.
                IF(WC.EQ.'APY')         IS=.TRUE.
                IF(WC.EQ.'APX')         IS=.TRUE.
                IF(WC.EQ.'PIY')         IS=.TRUE.
                IF(WC.EQ.'PIX')         IS=.TRUE.
                IF(WC.EQ.'PUY')         IS=.TRUE.
                IF(WC.EQ.'PUX')         IS=.TRUE.
                IF(WC.EQ.'APCY')        IS=.TRUE.
                IF(WC.EQ.'APCX')        IS=.TRUE.
                IF(WC.EQ.'PICY')        IS=.TRUE.
                IF(WC.EQ.'PICX')        IS=.TRUE.
                IF(WC.EQ.'PUCY')        IS=.TRUE.
                IF(WC.EQ.'PUCX')        IS=.TRUE.
                IF(WC.EQ.'COCY')        IS=.TRUE.
                IF(WC.EQ.'COCX')        IS=.TRUE.
                IF(WC.EQ.'PIKUP')       IS=.TRUE.
                IF(WC.EQ.'DEC')         IS=.TRUE.
                IF(WC.EQ.'TILT')        IS=.TRUE.
                IF(WC.EQ.'RTILT')       IS=.TRUE.
                IF(WC.EQ.'ASTOP')       IS=.TRUE.
                IF(WC.EQ.'REFS')        IS=.TRUE.
                IF(WC.EQ.'TH')          IS=.TRUE.
                IF(WC.EQ.'THM')         IS=.TRUE.
                IF(WC.EQ.'PRICE')       IS=.TRUE.
                IF(WC.EQ.'AUTOFUNC')    IS=.TRUE.
C     BEGINNING OF NON-SEQUENTIAL COMMANDS
                IF(WC.EQ.'CCR')         IS=.TRUE.
                IF(WC.EQ.'ROO')         IS=.TRUE.
C     END OF NON-SEQUENTIAL COMMANDS
                IF(WC.EQ.'RAYERROR')    IS=.TRUE.
                IF(WC.EQ.'REDSLV')      IS=.TRUE.
                IF(WC.EQ.'PY')          IS=.TRUE.
                IF(WC.EQ.'PCY')         IS=.TRUE.
                IF(WC.EQ.'CAY')         IS=.TRUE.
                IF(WC.EQ.'PX')          IS=.TRUE.
                IF(WC.EQ.'PCX')         IS=.TRUE.
                IF(WC.EQ.'CAX')         IS=.TRUE.
                IF(WC.EQ.'CLAP')        IS=.TRUE.
                IF(WC.EQ.'COBS')        IS=.TRUE.
                IF(WC.EQ.'GLAK')         IS=.TRUE.
                IF(WC.EQ.'GLASS')       IS=.TRUE.
                IF(WC.EQ.'DEFORM')      IS=.TRUE.
                IF(WC.EQ.'DELDEFOR')    IS=.TRUE.
                IF(WC.EQ.'MULTCLAP')    IS=.TRUE.
                IF(WC.EQ.'MULTCOBS')    IS=.TRUE.
                IF(WC.EQ.'SPIDER')      IS=.TRUE.
                IF(WC.EQ.'SPGR')        IS=.TRUE.
                IF(WC.EQ.'MODEL')       IS=.TRUE.
                IF(WC.EQ.'USER')        IS=.TRUE.
                IF(WC.EQ.'GLCAT')       IS=.TRUE.
                IF(WC.EQ.'AIR')         IS=.TRUE.
                IF(WC.EQ.'COATING ')    IS=.TRUE.
                IF(WC.EQ.'REFL')        IS=.TRUE.
                IF(WC.EQ.'REFLTIRO')    IS=.TRUE.
                IF(WC.EQ.'REFLTIR')     IS=.TRUE.
                IF(WC.EQ.'PERFECT')     IS=.TRUE.
                IF(WC.EQ.'IDEAL')       IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'TASPH')       IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'CHG')         IS=.TRUE.
                IF(WC.EQ.'CVTOR')       IS=.TRUE.
                IF(WC.EQ.'GRT')         IS=.TRUE.
                IF(WC.EQ.'GRO')         IS=.TRUE.
                IF(WC.EQ.'GRS')         IS=.TRUE.
                IF(WC.EQ.'GRX')         IS=.TRUE.
                IF(WC.EQ.'GRY')         IS=.TRUE.
                IF(WC.EQ.'GRZ')         IS=.TRUE.
                IF(WC.EQ.'RDTOR')       IS=.TRUE.
                IF(WC.EQ.'REAL')        IS=.TRUE.
                IF(WC.EQ.'PARAX')       IS=.TRUE.
                IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
                IF(WC.EQ.'PIVOT')       IS=.TRUE.
                IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
                IF(WC.EQ.'PIVOTD')      IS=.TRUE.
                IF(WC.EQ.'XD')          IS=.TRUE.
                IF(WC.EQ.'YD')          IS=.TRUE.
                IF(WC.EQ.'ZD')          IS=.TRUE.
                IF(WC.EQ.'PIVY')        IS=.TRUE.
                IF(WC.EQ.'PIVX')        IS=.TRUE.
                IF(WC.EQ.'PIVZ')        IS=.TRUE.
                IF(WC.EQ.'ALPHA')       IS=.TRUE.
                IF(WC.EQ.'GDX     ')    IS=.TRUE.
                IF(WC.EQ.'GDY     ')    IS=.TRUE.
                IF(WC.EQ.'GDZ     ')    IS=.TRUE.
                IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
                IF(WC.EQ.'GBETA   ')    IS=.TRUE.
                IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
                IF(WC.EQ.'TASPHD')      IS=.TRUE.
                IF(WC.EQ.'ARRAYD')      IS=.TRUE.
                IF(WC.EQ.'ARRAY')       IS=.TRUE.
                IF(WC.EQ.'CCTOR')       IS=.TRUE.
                IF(WC.EQ.'ADTOR')       IS=.TRUE.
                IF(WC.EQ.'AETOR')       IS=.TRUE.
                IF(WC.EQ.'AFTOR')       IS=.TRUE.
                IF(WC.EQ.'AGTOR')       IS=.TRUE.
                IF(WC.EQ.'AC')          IS=.TRUE.
                IF(WC.EQ.'AD')          IS=.TRUE.
                IF(WC.EQ.'AE')          IS=.TRUE.
                IF(WC.EQ.'AF')          IS=.TRUE.
                IF(WC.EQ.'AG')          IS=.TRUE.
                IF(WC.EQ.'AH')          IS=.TRUE.
                IF(WC.EQ.'AI')          IS=.TRUE.
                IF(WC.EQ.'AJ')          IS=.TRUE.
                IF(WC.EQ.'AK')          IS=.TRUE.
                IF(WC.EQ.'AL')          IS=.TRUE.
                IF(WC.EQ.'N1')          IS=.TRUE.
                IF(WC.EQ.'N2')          IS=.TRUE.
                IF(WC.EQ.'N3')          IS=.TRUE.
                IF(WC.EQ.'N4')          IS=.TRUE.
                IF(WC.EQ.'N5')          IS=.TRUE.
                IF(WC.EQ.'N6')          IS=.TRUE.
                IF(WC.EQ.'N7')          IS=.TRUE.
                IF(WC.EQ.'N8')          IS=.TRUE.
                IF(WC.EQ.'N9')          IS=.TRUE.
                IF(WC.EQ.'N10')         IS=.TRUE.
                IF(WC.EQ.'INDEX')       IS=.TRUE.
                IF(WC.EQ.'VNUM')        IS=.TRUE.
                IF(WC.EQ.'DPART')       IS=.TRUE.
                IF(WC.EQ.'BETA')        IS=.TRUE.
                IF(WC.EQ.'GAMMA')       IS=.TRUE.
                IF(WC.EQ.'ASPHD')       IS=.TRUE.
                IF(WC.EQ.'TORD')        IS=.TRUE.
                IF(WC.EQ.'TILTD')       IS=.TRUE.
                IF(WC.EQ.'CSD')         IS=.TRUE.
                IF(WC.EQ.'CSDX')        IS=.TRUE.
                IF(WC.EQ.'CSDY')        IS=.TRUE.
                IF(WC.EQ.'TSD')         IS=.TRUE.
                IF(WC.EQ.'GRTD')        IS=.TRUE.
                IF(WC.EQ.'PIKD')        IS=.TRUE.
                IF(WC.EQ.'CLAPD')       IS=.TRUE.
                IF(WC.EQ.'COBSD')       IS=.TRUE.
                IF(WC.EQ.'INSK')         IS=.TRUE.
                IF(WC.EQ.'DELK')         IS=.TRUE.
                IF(WC.EQ.'INR')         IS=.TRUE.
                IF(WC.EQ.'INRD')        IS=.TRUE.
                IF(WC.EQ.'ZERO')        IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTH(WC,WQ,IS)
      IMPLICIT NONE
      CHARACTER WC*8,WQ*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'EOS')                         IS=.TRUE.
                IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')        IS=.TRUE.
                IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')       IS=.TRUE.
                IF(WC.EQ.'C')                           IS=.TRUE.
                IF(WC.EQ.'M')                           IS=.TRUE.
                IF(WC.EQ.'SPDEL')                       IS=.TRUE.
                !IF(WC.EQ.'SPECIAL')                     IS=.TRUE.
                IF(WC.EQ.'GENL')                        IS=.TRUE.
                IF(WC.EQ.'C1')                          IS=.TRUE.
                IF(WC.EQ.'C2')                          IS=.TRUE.
                IF(WC.EQ.'C3')                          IS=.TRUE.
                IF(WC.EQ.'C4')                          IS=.TRUE.
                IF(WC.EQ.'C5')                          IS=.TRUE.
                IF(WC.EQ.'C6')                          IS=.TRUE.
                IF(WC.EQ.'C7')                          IS=.TRUE.
                IF(WC.EQ.'C8')                          IS=.TRUE.
                IF(WC.EQ.'C9')                          IS=.TRUE.
                IF(WC.EQ.'C10')                         IS=.TRUE.
                IF(WC.EQ.'C11')                         IS=.TRUE.
                IF(WC.EQ.'C12')                         IS=.TRUE.
                IF(WC.EQ.'C13')                         IS=.TRUE.
                IF(WC.EQ.'C14')                         IS=.TRUE.
                IF(WC.EQ.'C15')                         IS=.TRUE.
                IF(WC.EQ.'C16')                         IS=.TRUE.
                IF(WC.EQ.'C17')                         IS=.TRUE.
                IF(WC.EQ.'C18')                         IS=.TRUE.
                IF(WC.EQ.'C19')                         IS=.TRUE.
                IF(WC.EQ.'C20')                         IS=.TRUE.
                IF(WC.EQ.'C21')                         IS=.TRUE.
                IF(WC.EQ.'C22')                         IS=.TRUE.
                IF(WC.EQ.'C23')                         IS=.TRUE.
                IF(WC.EQ.'C24')                         IS=.TRUE.
                IF(WC.EQ.'C25')                         IS=.TRUE.
                IF(WC.EQ.'C26')                         IS=.TRUE.
                IF(WC.EQ.'C27')                         IS=.TRUE.
                IF(WC.EQ.'C28')                         IS=.TRUE.
                IF(WC.EQ.'C29')                         IS=.TRUE.
                IF(WC.EQ.'C30')                         IS=.TRUE.
                IF(WC.EQ.'C31')                         IS=.TRUE.
                IF(WC.EQ.'C32')                         IS=.TRUE.
                IF(WC.EQ.'C33')                         IS=.TRUE.
                IF(WC.EQ.'C34')                         IS=.TRUE.
                IF(WC.EQ.'C35')                         IS=.TRUE.
                IF(WC.EQ.'C36')                         IS=.TRUE.
                IF(WC.EQ.'C37')                         IS=.TRUE.
                IF(WC.EQ.'C38')                         IS=.TRUE.
                IF(WC.EQ.'C39')                         IS=.TRUE.
                IF(WC.EQ.'C40')                         IS=.TRUE.
                IF(WC.EQ.'C41')                         IS=.TRUE.
                IF(WC.EQ.'C42')                         IS=.TRUE.
                IF(WC.EQ.'C43')                         IS=.TRUE.
                IF(WC.EQ.'C44')                         IS=.TRUE.
                IF(WC.EQ.'C45')                         IS=.TRUE.
                IF(WC.EQ.'C46')                         IS=.TRUE.
                IF(WC.EQ.'C47')                         IS=.TRUE.
                IF(WC.EQ.'C48')                         IS=.TRUE.
                IF(WC.EQ.'C49')                         IS=.TRUE.
                IF(WC.EQ.'C50')                         IS=.TRUE.
                IF(WC.EQ.'C51')                         IS=.TRUE.
                IF(WC.EQ.'C52')                         IS=.TRUE.
                IF(WC.EQ.'C53')                         IS=.TRUE.
                IF(WC.EQ.'C54')                         IS=.TRUE.
                IF(WC.EQ.'C55')                         IS=.TRUE.
                IF(WC.EQ.'C56')                         IS=.TRUE.
                IF(WC.EQ.'C57')                         IS=.TRUE.
                IF(WC.EQ.'C58')                         IS=.TRUE.
                IF(WC.EQ.'C59')                         IS=.TRUE.
                IF(WC.EQ.'C60')                         IS=.TRUE.
                IF(WC.EQ.'C61')                         IS=.TRUE.
                IF(WC.EQ.'C62')                         IS=.TRUE.
                IF(WC.EQ.'C63')                         IS=.TRUE.
                IF(WC.EQ.'C64')                         IS=.TRUE.
                IF(WC.EQ.'C65')                         IS=.TRUE.
                IF(WC.EQ.'C66')                         IS=.TRUE.
                IF(WC.EQ.'C67')                         IS=.TRUE.
                IF(WC.EQ.'C68')                         IS=.TRUE.
                IF(WC.EQ.'C69')                         IS=.TRUE.
                IF(WC.EQ.'C70')                         IS=.TRUE.
                IF(WC.EQ.'C71')                         IS=.TRUE.
                IF(WC.EQ.'C72')                         IS=.TRUE.
                IF(WC.EQ.'C73')                         IS=.TRUE.
                IF(WC.EQ.'C74')                         IS=.TRUE.
                IF(WC.EQ.'C75')                         IS=.TRUE.
                IF(WC.EQ.'C76')                         IS=.TRUE.
                IF(WC.EQ.'C77')                         IS=.TRUE.
                IF(WC.EQ.'C78')                         IS=.TRUE.
                IF(WC.EQ.'C79')                         IS=.TRUE.
                IF(WC.EQ.'C80')                         IS=.TRUE.
                IF(WC.EQ.'C81')                         IS=.TRUE.
                IF(WC.EQ.'C82')                         IS=.TRUE.
                IF(WC.EQ.'C83')                         IS=.TRUE.
                IF(WC.EQ.'C84')                         IS=.TRUE.
                IF(WC.EQ.'C85')                         IS=.TRUE.
                IF(WC.EQ.'C86')                         IS=.TRUE.
                IF(WC.EQ.'C87')                         IS=.TRUE.
                IF(WC.EQ.'C88')                         IS=.TRUE.
                IF(WC.EQ.'C89')                         IS=.TRUE.
                IF(WC.EQ.'C90')                         IS=.TRUE.
                IF(WC.EQ.'C91')                         IS=.TRUE.
                IF(WC.EQ.'C92')                         IS=.TRUE.
                IF(WC.EQ.'C93')                         IS=.TRUE.
                IF(WC.EQ.'C94')                         IS=.TRUE.
                IF(WC.EQ.'C95')                         IS=.TRUE.
                IF(WC.EQ.'C96')                         IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTI(WC,WQ,IS)
      IMPLICIT NONE
      CHARACTER WC*8,WQ*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'EOS')                         IS=.TRUE.
                IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')        IS=.TRUE.
                IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')       IS=.TRUE.
                IF(WC.EQ.'C')                           IS=.TRUE.
                IF(WC.EQ.'M')                           IS=.TRUE.
                IF(WC.EQ.'SPDEL')                       IS=.TRUE.
                !IF(WC.EQ.'SPECIAL')                     IS=.TRUE.
                IF(WC.EQ.'GENL')                        IS=.TRUE.
                IF(WC.EQ.'C1')                          IS=.TRUE.
                IF(WC.EQ.'C2')                          IS=.TRUE.
                IF(WC.EQ.'C3')                          IS=.TRUE.
                IF(WC.EQ.'C4')                          IS=.TRUE.
                IF(WC.EQ.'C5')                          IS=.TRUE.
                IF(WC.EQ.'C6')                          IS=.TRUE.
                IF(WC.EQ.'C7')                          IS=.TRUE.
                IF(WC.EQ.'C8')                          IS=.TRUE.
                IF(WC.EQ.'C9')                          IS=.TRUE.
                IF(WC.EQ.'C10')                         IS=.TRUE.
                IF(WC.EQ.'C11')                         IS=.TRUE.
                IF(WC.EQ.'C12')                         IS=.TRUE.
                IF(WC.EQ.'C13')                         IS=.TRUE.
                IF(WC.EQ.'C14')                         IS=.TRUE.
                IF(WC.EQ.'C15')                         IS=.TRUE.
                IF(WC.EQ.'C16')                         IS=.TRUE.
                IF(WC.EQ.'C17')                         IS=.TRUE.
                IF(WC.EQ.'C18')                         IS=.TRUE.
                IF(WC.EQ.'C19')                         IS=.TRUE.
                IF(WC.EQ.'C20')                         IS=.TRUE.
                IF(WC.EQ.'C21')                         IS=.TRUE.
                IF(WC.EQ.'C22')                         IS=.TRUE.
                IF(WC.EQ.'C23')                         IS=.TRUE.
                IF(WC.EQ.'C24')                         IS=.TRUE.
                IF(WC.EQ.'C25')                         IS=.TRUE.
                IF(WC.EQ.'C26')                         IS=.TRUE.
                IF(WC.EQ.'C27')                         IS=.TRUE.
                IF(WC.EQ.'C28')                         IS=.TRUE.
                IF(WC.EQ.'C29')                         IS=.TRUE.
                IF(WC.EQ.'C30')                         IS=.TRUE.
                IF(WC.EQ.'C31')                         IS=.TRUE.
                IF(WC.EQ.'C32')                         IS=.TRUE.
                IF(WC.EQ.'C33')                         IS=.TRUE.
                IF(WC.EQ.'C34')                         IS=.TRUE.
                IF(WC.EQ.'C35')                         IS=.TRUE.
                IF(WC.EQ.'C36')                         IS=.TRUE.
                IF(WC.EQ.'C37')                         IS=.TRUE.
                IF(WC.EQ.'C38')                         IS=.TRUE.
                IF(WC.EQ.'C39')                         IS=.TRUE.
                IF(WC.EQ.'C40')                         IS=.TRUE.
                IF(WC.EQ.'C41')                         IS=.TRUE.
                IF(WC.EQ.'C42')                         IS=.TRUE.
                IF(WC.EQ.'C43')                         IS=.TRUE.
                IF(WC.EQ.'C44')                         IS=.TRUE.
                IF(WC.EQ.'C45')                         IS=.TRUE.
                IF(WC.EQ.'C46')                         IS=.TRUE.
                IF(WC.EQ.'C47')                         IS=.TRUE.
                IF(WC.EQ.'C48')                         IS=.TRUE.
                IF(WC.EQ.'C49')                         IS=.TRUE.
                IF(WC.EQ.'C50')                         IS=.TRUE.
                IF(WC.EQ.'C51')                         IS=.TRUE.
                IF(WC.EQ.'C52')                         IS=.TRUE.
                IF(WC.EQ.'C53')                         IS=.TRUE.
                IF(WC.EQ.'C54')                         IS=.TRUE.
                IF(WC.EQ.'C55')                         IS=.TRUE.
                IF(WC.EQ.'C56')                         IS=.TRUE.
                IF(WC.EQ.'C57')                         IS=.TRUE.
                IF(WC.EQ.'C58')                         IS=.TRUE.
                IF(WC.EQ.'C59')                         IS=.TRUE.
                IF(WC.EQ.'C60')                         IS=.TRUE.
                IF(WC.EQ.'C61')                         IS=.TRUE.
                IF(WC.EQ.'C62')                         IS=.TRUE.
                IF(WC.EQ.'C63')                         IS=.TRUE.
                IF(WC.EQ.'C64')                         IS=.TRUE.
                IF(WC.EQ.'C65')                         IS=.TRUE.
                IF(WC.EQ.'C66')                         IS=.TRUE.
                IF(WC.EQ.'C67')                         IS=.TRUE.
                IF(WC.EQ.'C68')                         IS=.TRUE.
                IF(WC.EQ.'C69')                         IS=.TRUE.
                IF(WC.EQ.'C70')                         IS=.TRUE.
                IF(WC.EQ.'C71')                         IS=.TRUE.
                IF(WC.EQ.'C72')                         IS=.TRUE.
                IF(WC.EQ.'C73')                         IS=.TRUE.
                IF(WC.EQ.'C74')                         IS=.TRUE.
                IF(WC.EQ.'C75')                         IS=.TRUE.
                IF(WC.EQ.'C76')                         IS=.TRUE.
                IF(WC.EQ.'C77')                         IS=.TRUE.
                IF(WC.EQ.'C78')                         IS=.TRUE.
                IF(WC.EQ.'C79')                         IS=.TRUE.
                IF(WC.EQ.'C80')                         IS=.TRUE.
                IF(WC.EQ.'C81')                         IS=.TRUE.
                IF(WC.EQ.'C82')                         IS=.TRUE.
                IF(WC.EQ.'C83')                         IS=.TRUE.
                IF(WC.EQ.'C84')                         IS=.TRUE.
                IF(WC.EQ.'C85')                         IS=.TRUE.
                IF(WC.EQ.'C86')                         IS=.TRUE.
                IF(WC.EQ.'C87')                         IS=.TRUE.
                IF(WC.EQ.'C88')                         IS=.TRUE.
                IF(WC.EQ.'C89')                         IS=.TRUE.
                IF(WC.EQ.'C90')                         IS=.TRUE.
                IF(WC.EQ.'C91')                         IS=.TRUE.
                IF(WC.EQ.'C92')                         IS=.TRUE.
                IF(WC.EQ.'C93')                         IS=.TRUE.
                IF(WC.EQ.'C94')                         IS=.TRUE.
                IF(WC.EQ.'C95')                         IS=.TRUE.
                IF(WC.EQ.'C96')                         IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTJ(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'SURF')        IS=.TRUE.
                IF(WC.EQ.'COEF')        IS=.TRUE.
                IF(WC.EQ.'DATA')        IS=.TRUE.
                IF(WC.EQ.'FIT')         IS=.TRUE.
                IF(WC.EQ.'FITGLASS')    IS=.TRUE.
                IF(WC.EQ.'GDATA')       IS=.TRUE.
                IF(WC.EQ.'LIST')        IS=.TRUE.
                IF(WC.EQ.'COEFS')       IS=.TRUE.
                IF(WC.EQ.'LISTCOEF')    IS=.TRUE.
                IF(WC.EQ.'EVAL')        IS=.TRUE.
                IF(WC.EQ.'FILE')        IS=.TRUE.
                IF(WC.EQ.'FETCH')       IS=.TRUE.
                IF(WC.EQ.'TYPE')        IS=.TRUE.
                IF(WC.EQ.'READ')        IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTK(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'SET     ')       IS=.TRUE.
                IF(WC.EQ.'W1      ')       IS=.TRUE.
                IF(WC.EQ.'W2      ')       IS=.TRUE.
                IF(WC.EQ.'W3      ')       IS=.TRUE.
                IF(WC.EQ.'W4      ')       IS=.TRUE.
                IF(WC.EQ.'W5      ')       IS=.TRUE.
                IF(WC.EQ.'EOS     ')       IS=.TRUE.
                IF(WC.EQ.'MR      ')       IS=.TRUE.
                IF(WC.EQ.'MRA     ')       IS=.TRUE.
                IF(WC.EQ.'OP      ')       IS=.TRUE.
                IF(WC.EQ.'OPA     ')       IS=.TRUE.
                IF(WC.EQ.'CFG     ')       IS=.TRUE.
                IF(WC.EQ.'C       ')       IS=.TRUE.
                IF(WC.EQ.'M       ')       IS=.TRUE.
                IF(WC.EQ.'DEL     ')       IS=.TRUE.
                IF(WC.EQ.'FUNC01  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC02  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC03  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC04  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC05  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC06  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC07  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC08  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC09  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC10  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC00  ')       IS=.TRUE.
                IF(WC.EQ.'COR     ')       IS=.TRUE.
                IF(WC.EQ.'BYP     ')       IS=.TRUE.
                IF(WC.EQ.'BLO     ')       IS=.TRUE.
                IF(WC.EQ.'BHI     ')       IS=.TRUE.
                IF(WC.EQ.'HLD     ')       IS=.TRUE.
                IF(WC.EQ.'GTE     ')       IS=.TRUE.
                IF(WC.EQ.'LTE     ')       IS=.TRUE.
                IF(WC.EQ.'OP_DESC ')       IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTL(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'SET     ')       IS=.TRUE.
                IF(WC.EQ.'W1      ')       IS=.TRUE.
                IF(WC.EQ.'W2      ')       IS=.TRUE.
                IF(WC.EQ.'W3      ')       IS=.TRUE.
                IF(WC.EQ.'W4      ')       IS=.TRUE.
                IF(WC.EQ.'W5      ')       IS=.TRUE.
                IF(WC.EQ.'EOS     ')       IS=.TRUE.
                IF(WC.EQ.'TOPS    ')       IS=.TRUE.
                IF(WC.EQ.'C       ')       IS=.TRUE.
                IF(WC.EQ.'M       ')       IS=.TRUE.
                IF(WC.EQ.'DEL     ')       IS=.TRUE.
                IF(WC.EQ.'FUNC01  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC02  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC03  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC04  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC05  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC06  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC07  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC08  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC09  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC10  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC00  ')       IS=.TRUE.
                IF(WC.EQ.'OP_DESC ')       IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTM(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'SET     ')       IS=.TRUE.
                IF(WC.EQ.'W1      ')       IS=.TRUE.
                IF(WC.EQ.'W2      ')       IS=.TRUE.
                IF(WC.EQ.'W3      ')       IS=.TRUE.
                IF(WC.EQ.'W4      ')       IS=.TRUE.
                IF(WC.EQ.'W5      ')       IS=.TRUE.
                IF(WC.EQ.'EOS     ')       IS=.TRUE.
                IF(WC.EQ.'CRITS   ')       IS=.TRUE.
                IF(WC.EQ.'C       ')       IS=.TRUE.
                IF(WC.EQ.'M       ')       IS=.TRUE.
                IF(WC.EQ.'DEL     ')       IS=.TRUE.
                IF(WC.EQ.'FUNC01  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC02  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC03  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC04  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC05  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC06  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC07  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC08  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC09  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC10  ')       IS=.TRUE.
                IF(WC.EQ.'FUNC00  ')       IS=.TRUE.
                IF(WC.EQ.'OP_DESC ')       IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTN(WC,F29,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      INTEGER F29
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'VB')          IS=.TRUE.
                IF(WC.EQ.'VBA')         IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'CFG')         IS=.TRUE.
                IF(WC.EQ.'DELK'.AND.F29.EQ.2)            IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTO(WC,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      LOGICAL IS
                IS=.FALSE.
                IF(WC.EQ.'MACVAR')      IS=.TRUE.
                IF(WC.EQ.'RD')          IS=.TRUE.
                IF(WC.EQ.'CV')          IS=.TRUE.
                IF(WC.EQ.'TH')          IS=.TRUE.
                IF(WC.EQ.'CC')          IS=.TRUE.
                IF(WC.EQ.'AC')          IS=.TRUE.
                IF(WC.EQ.'AD')          IS=.TRUE.
                IF(WC.EQ.'AE')          IS=.TRUE.
                IF(WC.EQ.'AF')          IS=.TRUE.
                IF(WC.EQ.'AG')          IS=.TRUE.
                IF(WC.EQ.'AH')          IS=.TRUE.
                IF(WC.EQ.'AI')          IS=.TRUE.
                IF(WC.EQ.'AJ')          IS=.TRUE.
                IF(WC.EQ.'AK')          IS=.TRUE.
                IF(WC.EQ.'AL')          IS=.TRUE.
                IF(WC.EQ.'RDTOR')       IS=.TRUE.
                IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
                IF(WC.EQ.'PIVOT')       IS=.TRUE.
                IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
                IF(WC.EQ.'PIVOTD')      IS=.TRUE.
                IF(WC.EQ.'CVTOR')       IS=.TRUE.
                IF(WC.EQ.'GRS')         IS=.TRUE.
                IF(WC.EQ.'CCTOR')       IS=.TRUE.
                IF(WC.EQ.'ADTOR')       IS=.TRUE.
                IF(WC.EQ.'AETOR')       IS=.TRUE.
                IF(WC.EQ.'AFTOR')       IS=.TRUE.
                IF(WC.EQ.'AGTOR')       IS=.TRUE.
                IF(WC.EQ.'ALPHA')       IS=.TRUE.
                IF(WC.EQ.'BETA')        IS=.TRUE.
                IF(WC.EQ.'GAMMA')       IS=.TRUE.
                IF(WC.EQ.'GDX     ')    IS=.TRUE.
                IF(WC.EQ.'GDY     ')    IS=.TRUE.
                IF(WC.EQ.'GDZ     ')    IS=.TRUE.
                IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
                IF(WC.EQ.'GBETA   ')    IS=.TRUE.
                IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
                IF(WC.EQ.'XD')          IS=.TRUE.
                IF(WC.EQ.'YD')          IS=.TRUE.
                IF(WC.EQ.'ZD')          IS=.TRUE.
                IF(WC.EQ.'PIVY')        IS=.TRUE.
                IF(WC.EQ.'PIVX')        IS=.TRUE.
                IF(WC.EQ.'PIVZ')        IS=.TRUE.
                IF(WC.EQ.'N1')          IS=.TRUE.
                IF(WC.EQ.'N2')          IS=.TRUE.
                IF(WC.EQ.'N3')          IS=.TRUE.
                IF(WC.EQ.'N4')          IS=.TRUE.
                IF(WC.EQ.'N5')          IS=.TRUE.
                IF(WC.EQ.'N6')          IS=.TRUE.
                IF(WC.EQ.'N7')          IS=.TRUE.
                IF(WC.EQ.'N8')          IS=.TRUE.
                IF(WC.EQ.'N9')          IS=.TRUE.
                IF(WC.EQ.'N10')         IS=.TRUE.
                IF(WC.EQ.'INDEX')       IS=.TRUE.
                IF(WC.EQ.'VNUM')        IS=.TRUE.
                IF(WC.EQ.'DPART')       IS=.TRUE.
                IF(WC.EQ.'CLPX')        IS=.TRUE.
                IF(WC.EQ.'CLPY')        IS=.TRUE.
                IF(WC.EQ.'C1')          IS=.TRUE.
                IF(WC.EQ.'C2')          IS=.TRUE.
                IF(WC.EQ.'C3')          IS=.TRUE.
                IF(WC.EQ.'C4')          IS=.TRUE.
                IF(WC.EQ.'C5')          IS=.TRUE.
                IF(WC.EQ.'C6')          IS=.TRUE.
                IF(WC.EQ.'C7')          IS=.TRUE.
                IF(WC.EQ.'C8')          IS=.TRUE.
                IF(WC.EQ.'C9')          IS=.TRUE.
                IF(WC.EQ.'C10')         IS=.TRUE.
                IF(WC.EQ.'C11')         IS=.TRUE.
                IF(WC.EQ.'C12')         IS=.TRUE.
                IF(WC.EQ.'C13')         IS=.TRUE.
                IF(WC.EQ.'C14')         IS=.TRUE.
                IF(WC.EQ.'C15')         IS=.TRUE.
                IF(WC.EQ.'C16')         IS=.TRUE.
                IF(WC.EQ.'C17')         IS=.TRUE.
                IF(WC.EQ.'C18')         IS=.TRUE.
                IF(WC.EQ.'C19')         IS=.TRUE.
                IF(WC.EQ.'C20')         IS=.TRUE.
                IF(WC.EQ.'C21')         IS=.TRUE.
                IF(WC.EQ.'C22')         IS=.TRUE.
                IF(WC.EQ.'C23')         IS=.TRUE.
                IF(WC.EQ.'C24')         IS=.TRUE.
                IF(WC.EQ.'C25')         IS=.TRUE.
                IF(WC.EQ.'C26')         IS=.TRUE.
                IF(WC.EQ.'C27')         IS=.TRUE.
                IF(WC.EQ.'C28')         IS=.TRUE.
                IF(WC.EQ.'C29')         IS=.TRUE.
                IF(WC.EQ.'C30')         IS=.TRUE.
                IF(WC.EQ.'C31')         IS=.TRUE.
                IF(WC.EQ.'C32')         IS=.TRUE.
                IF(WC.EQ.'C33')         IS=.TRUE.
                IF(WC.EQ.'C34')         IS=.TRUE.
                IF(WC.EQ.'C35')         IS=.TRUE.
                IF(WC.EQ.'C36')         IS=.TRUE.
                IF(WC.EQ.'C37')         IS=.TRUE.
                IF(WC.EQ.'C38')         IS=.TRUE.
                IF(WC.EQ.'C39')         IS=.TRUE.
                IF(WC.EQ.'C40')         IS=.TRUE.
                IF(WC.EQ.'C41')         IS=.TRUE.
                IF(WC.EQ.'C42')         IS=.TRUE.
                IF(WC.EQ.'C43')         IS=.TRUE.
                IF(WC.EQ.'C44')         IS=.TRUE.
                IF(WC.EQ.'C45')         IS=.TRUE.
                IF(WC.EQ.'C46')         IS=.TRUE.
                IF(WC.EQ.'C47')         IS=.TRUE.
                IF(WC.EQ.'C48')         IS=.TRUE.
                IF(WC.EQ.'C49')                         IS=.TRUE.
                IF(WC.EQ.'C50')                         IS=.TRUE.
                IF(WC.EQ.'C51')                         IS=.TRUE.
                IF(WC.EQ.'C52')                         IS=.TRUE.
                IF(WC.EQ.'C53')                         IS=.TRUE.
                IF(WC.EQ.'C54')                         IS=.TRUE.
                IF(WC.EQ.'C55')                         IS=.TRUE.
                IF(WC.EQ.'C56')                         IS=.TRUE.
                IF(WC.EQ.'C57')                         IS=.TRUE.
                IF(WC.EQ.'C58')                         IS=.TRUE.
                IF(WC.EQ.'C59')                         IS=.TRUE.
                IF(WC.EQ.'C60')                         IS=.TRUE.
                IF(WC.EQ.'C61')                         IS=.TRUE.
                IF(WC.EQ.'C62')                         IS=.TRUE.
                IF(WC.EQ.'C63')                         IS=.TRUE.
                IF(WC.EQ.'C64')                         IS=.TRUE.
                IF(WC.EQ.'C65')                         IS=.TRUE.
                IF(WC.EQ.'C66')                         IS=.TRUE.
                IF(WC.EQ.'C67')                         IS=.TRUE.
                IF(WC.EQ.'C68')                         IS=.TRUE.
                IF(WC.EQ.'C69')                         IS=.TRUE.
                IF(WC.EQ.'C70')                         IS=.TRUE.
                IF(WC.EQ.'C71')                         IS=.TRUE.
                IF(WC.EQ.'C72')                         IS=.TRUE.
                IF(WC.EQ.'C73')                         IS=.TRUE.
                IF(WC.EQ.'C74')                         IS=.TRUE.
                IF(WC.EQ.'C75')                         IS=.TRUE.
                IF(WC.EQ.'C76')                         IS=.TRUE.
                IF(WC.EQ.'C77')                         IS=.TRUE.
                IF(WC.EQ.'C78')                         IS=.TRUE.
                IF(WC.EQ.'C79')                         IS=.TRUE.
                IF(WC.EQ.'C80')                         IS=.TRUE.
                IF(WC.EQ.'C81')                         IS=.TRUE.
                IF(WC.EQ.'C82')                         IS=.TRUE.
                IF(WC.EQ.'C83')                         IS=.TRUE.
                IF(WC.EQ.'C84')                         IS=.TRUE.
                IF(WC.EQ.'C85')                         IS=.TRUE.
                IF(WC.EQ.'C86')                         IS=.TRUE.
                IF(WC.EQ.'C87')                         IS=.TRUE.
                IF(WC.EQ.'C88')                         IS=.TRUE.
                IF(WC.EQ.'C89')                         IS=.TRUE.
                IF(WC.EQ.'C90')                         IS=.TRUE.
                IF(WC.EQ.'C91')                         IS=.TRUE.
                IF(WC.EQ.'C92')                         IS=.TRUE.
                IF(WC.EQ.'C93')                         IS=.TRUE.
                IF(WC.EQ.'C94')                         IS=.TRUE.
                IF(WC.EQ.'C95')                         IS=.TRUE.
                IF(WC.EQ.'C96')                         IS=.TRUE.
                IF(WC(1:3).EQ.'ACT')                    IS=.TRUE.
                IF(WC(1:3).EQ.'PAR')                     IS=.TRUE.
                IF(WC.EQ.'V1      ')                    IS=.TRUE.
                IF(WC.EQ.'V2      ')                    IS=.TRUE.
                IF(WC.EQ.'V3      ')                    IS=.TRUE.
                IF(WC.EQ.'V4      ')                    IS=.TRUE.
                IF(WC.EQ.'V5      ')                    IS=.TRUE.
                IF(WC.EQ.'NSSXPOS ')                    IS=.TRUE.
                IF(WC.EQ.'NSSYPOS ')                    IS=.TRUE.
                IF(WC.EQ.'NSSZPOS ')                    IS=.TRUE.
                IF(WC.EQ.'NSSALPH ')                    IS=.TRUE.
                IF(WC.EQ.'NSSBETA ')                    IS=.TRUE.
                IF(WC.EQ.'NSSGAMM ')                    IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTP(WC,F51,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      INTEGER F51
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'TVB')          IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'RD')          IS=.TRUE.
                IF(WC.EQ.'CV')          IS=.TRUE.
                IF(WC.EQ.'TH')          IS=.TRUE.
                IF(WC.EQ.'CC')          IS=.TRUE.
                IF(WC.EQ.'AC')          IS=.TRUE.
                IF(WC.EQ.'AD')          IS=.TRUE.
                IF(WC.EQ.'AE')          IS=.TRUE.
                IF(WC.EQ.'AF')          IS=.TRUE.
                IF(WC.EQ.'AG')          IS=.TRUE.
                IF(WC.EQ.'AH')          IS=.TRUE.
                IF(WC.EQ.'AI')          IS=.TRUE.
                IF(WC.EQ.'AJ')          IS=.TRUE.
                IF(WC.EQ.'AK')          IS=.TRUE.
                IF(WC.EQ.'AL')          IS=.TRUE.
                IF(WC.EQ.'RDTOR')       IS=.TRUE.
                IF(WC.EQ.'CVTOR')       IS=.TRUE.
                IF(WC.EQ.'GRS')         IS=.TRUE.
                IF(WC.EQ.'CCTOR')       IS=.TRUE.
                IF(WC.EQ.'ADTOR')       IS=.TRUE.
                IF(WC.EQ.'AETOR')       IS=.TRUE.
                IF(WC.EQ.'AFTOR')       IS=.TRUE.
                IF(WC.EQ.'AGTOR')       IS=.TRUE.
                IF(WC.EQ.'ALPHA')       IS=.TRUE.
                IF(WC.EQ.'BETA')        IS=.TRUE.
                IF(WC.EQ.'GAMMA')       IS=.TRUE.
                IF(WC.EQ.'GDX     ')    IS=.TRUE.
                IF(WC.EQ.'GDY     ')    IS=.TRUE.
                IF(WC.EQ.'GDZ     ')    IS=.TRUE.
                IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
                IF(WC.EQ.'GBETA   ')    IS=.TRUE.
                IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
                IF(WC.EQ.'XD')          IS=.TRUE.
                IF(WC.EQ.'YD')          IS=.TRUE.
                IF(WC.EQ.'ZD')          IS=.TRUE.
                IF(WC.EQ.'PIVY')        IS=.TRUE.
                IF(WC.EQ.'PIVX')        IS=.TRUE.
                IF(WC.EQ.'PIVZ')        IS=.TRUE.
                IF(WC.EQ.'N1')          IS=.TRUE.
                IF(WC.EQ.'N2')          IS=.TRUE.
                IF(WC.EQ.'N3')          IS=.TRUE.
                IF(WC.EQ.'N4')          IS=.TRUE.
                IF(WC.EQ.'N5')          IS=.TRUE.
                IF(WC.EQ.'N6')          IS=.TRUE.
                IF(WC.EQ.'N7')          IS=.TRUE.
                IF(WC.EQ.'N8')          IS=.TRUE.
                IF(WC.EQ.'N9')          IS=.TRUE.
                IF(WC.EQ.'N10')         IS=.TRUE.
                IF(WC.EQ.'INDEX')       IS=.TRUE.
                IF(WC.EQ.'VNUM')        IS=.TRUE.
                IF(WC.EQ.'DPART')       IS=.TRUE.
                IF(WC.EQ.'CLPX')        IS=.TRUE.
                IF(WC.EQ.'CLPY')        IS=.TRUE.
                IF(WC.EQ.'C1')          IS=.TRUE.
                IF(WC.EQ.'C2')          IS=.TRUE.
                IF(WC.EQ.'C3')          IS=.TRUE.
                IF(WC.EQ.'C4')          IS=.TRUE.
                IF(WC.EQ.'C5')          IS=.TRUE.
                IF(WC.EQ.'C6')          IS=.TRUE.
                IF(WC.EQ.'C7')          IS=.TRUE.
                IF(WC.EQ.'C8')          IS=.TRUE.
                IF(WC.EQ.'C9')          IS=.TRUE.
                IF(WC.EQ.'C10')         IS=.TRUE.
                IF(WC.EQ.'C11')         IS=.TRUE.
                IF(WC.EQ.'C12')         IS=.TRUE.
                IF(WC.EQ.'C13')         IS=.TRUE.
                IF(WC.EQ.'C14')         IS=.TRUE.
                IF(WC.EQ.'C15')         IS=.TRUE.
                IF(WC.EQ.'C16')         IS=.TRUE.
                IF(WC.EQ.'C17')         IS=.TRUE.
                IF(WC.EQ.'C18')         IS=.TRUE.
                IF(WC.EQ.'C19')         IS=.TRUE.
                IF(WC.EQ.'C20')         IS=.TRUE.
                IF(WC.EQ.'C21')         IS=.TRUE.
                IF(WC.EQ.'C22')         IS=.TRUE.
                IF(WC.EQ.'C23')         IS=.TRUE.
                IF(WC.EQ.'C24')         IS=.TRUE.
                IF(WC.EQ.'C25')         IS=.TRUE.
                IF(WC.EQ.'C26')         IS=.TRUE.
                IF(WC.EQ.'C27')         IS=.TRUE.
                IF(WC.EQ.'C28')         IS=.TRUE.
                IF(WC.EQ.'C29')         IS=.TRUE.
                IF(WC.EQ.'C30')         IS=.TRUE.
                IF(WC.EQ.'C31')         IS=.TRUE.
                IF(WC.EQ.'C32')         IS=.TRUE.
                IF(WC.EQ.'C33')         IS=.TRUE.
                IF(WC.EQ.'C34')         IS=.TRUE.
                IF(WC.EQ.'C35')         IS=.TRUE.
                IF(WC.EQ.'C36')         IS=.TRUE.
                IF(WC.EQ.'C37')         IS=.TRUE.
                IF(WC.EQ.'C38')         IS=.TRUE.
                IF(WC.EQ.'C39')         IS=.TRUE.
                IF(WC.EQ.'C40')         IS=.TRUE.
                IF(WC.EQ.'C41')         IS=.TRUE.
                IF(WC.EQ.'C42')         IS=.TRUE.
                IF(WC.EQ.'C43')         IS=.TRUE.
                IF(WC.EQ.'C44')         IS=.TRUE.
                IF(WC.EQ.'C45')         IS=.TRUE.
                IF(WC.EQ.'C46')         IS=.TRUE.
                IF(WC.EQ.'C47')         IS=.TRUE.
                IF(WC.EQ.'C48')         IS=.TRUE.
                IF(WC.EQ.'C49')                         IS=.TRUE.
                IF(WC.EQ.'C50')                         IS=.TRUE.
                IF(WC.EQ.'C51')                         IS=.TRUE.
                IF(WC.EQ.'C52')                         IS=.TRUE.
                IF(WC.EQ.'C53')                         IS=.TRUE.
                IF(WC.EQ.'C54')                         IS=.TRUE.
                IF(WC.EQ.'C55')                         IS=.TRUE.
                IF(WC.EQ.'C56')                         IS=.TRUE.
                IF(WC.EQ.'C57')                         IS=.TRUE.
                IF(WC.EQ.'C58')                         IS=.TRUE.
                IF(WC.EQ.'C59')                         IS=.TRUE.
                IF(WC.EQ.'C60')                         IS=.TRUE.
                IF(WC.EQ.'C61')                         IS=.TRUE.
                IF(WC.EQ.'C62')                         IS=.TRUE.
                IF(WC.EQ.'C63')                         IS=.TRUE.
                IF(WC.EQ.'C64')                         IS=.TRUE.
                IF(WC.EQ.'C65')                         IS=.TRUE.
                IF(WC.EQ.'C66')                         IS=.TRUE.
                IF(WC.EQ.'C67')                         IS=.TRUE.
                IF(WC.EQ.'C68')                         IS=.TRUE.
                IF(WC.EQ.'C69')                         IS=.TRUE.
                IF(WC.EQ.'C70')                         IS=.TRUE.
                IF(WC.EQ.'C71')                         IS=.TRUE.
                IF(WC.EQ.'C72')                         IS=.TRUE.
                IF(WC.EQ.'C73')                         IS=.TRUE.
                IF(WC.EQ.'C74')                         IS=.TRUE.
                IF(WC.EQ.'C75')                         IS=.TRUE.
                IF(WC.EQ.'C76')                         IS=.TRUE.
                IF(WC.EQ.'C77')                         IS=.TRUE.
                IF(WC.EQ.'C78')                         IS=.TRUE.
                IF(WC.EQ.'C79')                         IS=.TRUE.
                IF(WC.EQ.'C80')                         IS=.TRUE.
                IF(WC.EQ.'C81')                         IS=.TRUE.
                IF(WC.EQ.'C82')                         IS=.TRUE.
                IF(WC.EQ.'C83')                         IS=.TRUE.
                IF(WC.EQ.'C84')                         IS=.TRUE.
                IF(WC.EQ.'C85')                         IS=.TRUE.
                IF(WC.EQ.'C86')                         IS=.TRUE.
                IF(WC.EQ.'C87')                         IS=.TRUE.
                IF(WC.EQ.'C88')                         IS=.TRUE.
                IF(WC.EQ.'C89')                         IS=.TRUE.
                IF(WC.EQ.'C90')                         IS=.TRUE.
                IF(WC.EQ.'C91')                         IS=.TRUE.
                IF(WC.EQ.'C92')                         IS=.TRUE.
                IF(WC.EQ.'C93')                         IS=.TRUE.
                IF(WC.EQ.'C94')                         IS=.TRUE.
                IF(WC.EQ.'C95')                         IS=.TRUE.
                IF(WC.EQ.'C96')                         IS=.TRUE.
                IF(WC.EQ.'CV_FR')                       IS=.TRUE.
                IF(WC.EQ.'RD_FR')                       IS=.TRUE.
                IF(WC.EQ.'RDTFR')                       IS=.TRUE.
                IF(WC.EQ.'CVTFR')                       IS=.TRUE.
      IF(WC.EQ.'STILTA') IS=.TRUE.
      IF(WC.EQ.'STILTB') IS=.TRUE.
      IF(WC.EQ.'STILTG') IS=.TRUE.
      IF(WC.EQ.'BTILTA') IS=.TRUE.
      IF(WC.EQ.'BTILTB') IS=.TRUE.
      IF(WC.EQ.'BTILTG') IS=.TRUE.
      IF(WC.EQ.'DISPX')  IS=.TRUE.
      IF(WC.EQ.'DISPY')  IS=.TRUE.
      IF(WC.EQ.'DISPZ')  IS=.TRUE.
      IF(WC.EQ.'ROLLX')  IS=.TRUE.
      IF(WC.EQ.'ROLLY')  IS=.TRUE.
                IF(WC.EQ.'DELK'.AND.F51.EQ.2)            IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTQ(WC,F52,IS)
      IMPLICIT NONE
      CHARACTER WC*8
      INTEGER F52
      LOGICAL IS
                        IS=.FALSE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'COMPS')       IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'RD')          IS=.TRUE.
                IF(WC.EQ.'CV')          IS=.TRUE.
                IF(WC.EQ.'TH')          IS=.TRUE.
                IF(WC.EQ.'CC')          IS=.TRUE.
                IF(WC.EQ.'AC')          IS=.TRUE.
                IF(WC.EQ.'AD')          IS=.TRUE.
                IF(WC.EQ.'AE')          IS=.TRUE.
                IF(WC.EQ.'AF')          IS=.TRUE.
                IF(WC.EQ.'AG')          IS=.TRUE.
                IF(WC.EQ.'AH')          IS=.TRUE.
                IF(WC.EQ.'AI')          IS=.TRUE.
                IF(WC.EQ.'AJ')          IS=.TRUE.
                IF(WC.EQ.'AK')          IS=.TRUE.
                IF(WC.EQ.'AL')          IS=.TRUE.
                IF(WC.EQ.'RDTOR')       IS=.TRUE.
                IF(WC.EQ.'CVTOR')       IS=.TRUE.
                IF(WC.EQ.'GRS')         IS=.TRUE.
                IF(WC.EQ.'CCTOR')       IS=.TRUE.
                IF(WC.EQ.'ADTOR')       IS=.TRUE.
                IF(WC.EQ.'AETOR')       IS=.TRUE.
                IF(WC.EQ.'AFTOR')       IS=.TRUE.
                IF(WC.EQ.'AGTOR')       IS=.TRUE.
                IF(WC.EQ.'ALPHA')       IS=.TRUE.
                IF(WC.EQ.'BETA')        IS=.TRUE.
                IF(WC.EQ.'GAMMA')       IS=.TRUE.
                IF(WC.EQ.'GDX     ')    IS=.TRUE.
                IF(WC.EQ.'GDY     ')    IS=.TRUE.
                IF(WC.EQ.'GDZ     ')    IS=.TRUE.
                IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
                IF(WC.EQ.'GBETA   ')    IS=.TRUE.
                IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
                IF(WC.EQ.'XD')          IS=.TRUE.
                IF(WC.EQ.'YD')          IS=.TRUE.
                IF(WC.EQ.'ZD')          IS=.TRUE.
                IF(WC.EQ.'PIVY')        IS=.TRUE.
                IF(WC.EQ.'PIVX')        IS=.TRUE.
                IF(WC.EQ.'PIVZ')        IS=.TRUE.
                IF(WC.EQ.'N1')          IS=.TRUE.
                IF(WC.EQ.'N2')          IS=.TRUE.
                IF(WC.EQ.'N3')          IS=.TRUE.
                IF(WC.EQ.'N4')          IS=.TRUE.
                IF(WC.EQ.'N5')          IS=.TRUE.
                IF(WC.EQ.'N6')          IS=.TRUE.
                IF(WC.EQ.'N7')          IS=.TRUE.
                IF(WC.EQ.'N8')          IS=.TRUE.
                IF(WC.EQ.'N9')          IS=.TRUE.
                IF(WC.EQ.'N10')         IS=.TRUE.
                IF(WC.EQ.'INDEX')       IS=.TRUE.
                IF(WC.EQ.'VNUM')        IS=.TRUE.
                IF(WC.EQ.'DPART')       IS=.TRUE.
                IF(WC.EQ.'CLPX')        IS=.TRUE.
                IF(WC.EQ.'CLPY')        IS=.TRUE.
                IF(WC.EQ.'C1')          IS=.TRUE.
                IF(WC.EQ.'C2')          IS=.TRUE.
                IF(WC.EQ.'C3')          IS=.TRUE.
                IF(WC.EQ.'C4')          IS=.TRUE.
                IF(WC.EQ.'C5')          IS=.TRUE.
                IF(WC.EQ.'C6')          IS=.TRUE.
                IF(WC.EQ.'C7')          IS=.TRUE.
                IF(WC.EQ.'C8')          IS=.TRUE.
                IF(WC.EQ.'C9')          IS=.TRUE.
                IF(WC.EQ.'C10')         IS=.TRUE.
                IF(WC.EQ.'C11')         IS=.TRUE.
                IF(WC.EQ.'C12')         IS=.TRUE.
                IF(WC.EQ.'C13')         IS=.TRUE.
                IF(WC.EQ.'C14')         IS=.TRUE.
                IF(WC.EQ.'C15')         IS=.TRUE.
                IF(WC.EQ.'C16')         IS=.TRUE.
                IF(WC.EQ.'C17')         IS=.TRUE.
                IF(WC.EQ.'C18')         IS=.TRUE.
                IF(WC.EQ.'C19')         IS=.TRUE.
                IF(WC.EQ.'C20')         IS=.TRUE.
                IF(WC.EQ.'C21')         IS=.TRUE.
                IF(WC.EQ.'C22')         IS=.TRUE.
                IF(WC.EQ.'C23')         IS=.TRUE.
                IF(WC.EQ.'C24')         IS=.TRUE.
                IF(WC.EQ.'C25')         IS=.TRUE.
                IF(WC.EQ.'C26')         IS=.TRUE.
                IF(WC.EQ.'C27')         IS=.TRUE.
                IF(WC.EQ.'C28')         IS=.TRUE.
                IF(WC.EQ.'C29')         IS=.TRUE.
                IF(WC.EQ.'C30')         IS=.TRUE.
                IF(WC.EQ.'C31')         IS=.TRUE.
                IF(WC.EQ.'C32')         IS=.TRUE.
                IF(WC.EQ.'C33')         IS=.TRUE.
                IF(WC.EQ.'C34')         IS=.TRUE.
                IF(WC.EQ.'C35')         IS=.TRUE.
                IF(WC.EQ.'C36')         IS=.TRUE.
                IF(WC.EQ.'C37')         IS=.TRUE.
                IF(WC.EQ.'C38')         IS=.TRUE.
                IF(WC.EQ.'C39')         IS=.TRUE.
                IF(WC.EQ.'C40')         IS=.TRUE.
                IF(WC.EQ.'C41')         IS=.TRUE.
                IF(WC.EQ.'C42')         IS=.TRUE.
                IF(WC.EQ.'C43')         IS=.TRUE.
                IF(WC.EQ.'C44')         IS=.TRUE.
                IF(WC.EQ.'C45')         IS=.TRUE.
                IF(WC.EQ.'C46')         IS=.TRUE.
                IF(WC.EQ.'C47')         IS=.TRUE.
                IF(WC.EQ.'C48')         IS=.TRUE.
                IF(WC.EQ.'C49')                         IS=.TRUE.
                IF(WC.EQ.'C50')                         IS=.TRUE.
                IF(WC.EQ.'C51')                         IS=.TRUE.
                IF(WC.EQ.'C52')                         IS=.TRUE.
                IF(WC.EQ.'C53')                         IS=.TRUE.
                IF(WC.EQ.'C54')                         IS=.TRUE.
                IF(WC.EQ.'C55')                         IS=.TRUE.
                IF(WC.EQ.'C56')                         IS=.TRUE.
                IF(WC.EQ.'C57')                         IS=.TRUE.
                IF(WC.EQ.'C58')                         IS=.TRUE.
                IF(WC.EQ.'C59')                         IS=.TRUE.
                IF(WC.EQ.'C60')                         IS=.TRUE.
                IF(WC.EQ.'C61')                         IS=.TRUE.
                IF(WC.EQ.'C62')                         IS=.TRUE.
                IF(WC.EQ.'C63')                         IS=.TRUE.
                IF(WC.EQ.'C64')                         IS=.TRUE.
                IF(WC.EQ.'C65')                         IS=.TRUE.
                IF(WC.EQ.'C66')                         IS=.TRUE.
                IF(WC.EQ.'C67')                         IS=.TRUE.
                IF(WC.EQ.'C68')                         IS=.TRUE.
                IF(WC.EQ.'C69')                         IS=.TRUE.
                IF(WC.EQ.'C70')                         IS=.TRUE.
                IF(WC.EQ.'C71')                         IS=.TRUE.
                IF(WC.EQ.'C72')                         IS=.TRUE.
                IF(WC.EQ.'C73')                         IS=.TRUE.
                IF(WC.EQ.'C74')                         IS=.TRUE.
                IF(WC.EQ.'C75')                         IS=.TRUE.
                IF(WC.EQ.'C76')                         IS=.TRUE.
                IF(WC.EQ.'C77')                         IS=.TRUE.
                IF(WC.EQ.'C78')                         IS=.TRUE.
                IF(WC.EQ.'C79')                         IS=.TRUE.
                IF(WC.EQ.'C80')                         IS=.TRUE.
                IF(WC.EQ.'C81')                         IS=.TRUE.
                IF(WC.EQ.'C82')                         IS=.TRUE.
                IF(WC.EQ.'C83')                         IS=.TRUE.
                IF(WC.EQ.'C84')                         IS=.TRUE.
                IF(WC.EQ.'C85')                         IS=.TRUE.
                IF(WC.EQ.'C86')                         IS=.TRUE.
                IF(WC.EQ.'C87')                         IS=.TRUE.
                IF(WC.EQ.'C88')                         IS=.TRUE.
                IF(WC.EQ.'C89')                         IS=.TRUE.
                IF(WC.EQ.'C90')                         IS=.TRUE.
                IF(WC.EQ.'C91')                         IS=.TRUE.
                IF(WC.EQ.'C92')                         IS=.TRUE.
                IF(WC.EQ.'C93')                         IS=.TRUE.
                IF(WC.EQ.'C94')                         IS=.TRUE.
                IF(WC.EQ.'C95')                         IS=.TRUE.
                IF(WC.EQ.'C96')                         IS=.TRUE.
                IF(WC.EQ.'DELK'.AND.F52.EQ.2)            IS=.TRUE.
                        RETURN
                        END
      SUBROUTINE CONTR(WC,WQ,SQ,IS)
      use glass_manager  
C       CONFIGS INPUT AND UPDATE
      IMPLICIT NONE
      CHARACTER WC*8,WQ*8
      INTEGER SQ
      LOGICAL IS
                IS=.FALSE.
                IF(gdb%isNameInCatalog(WC)) IS=.TRUE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'LI')          IS=.TRUE.
                IF(WC.EQ.'LABEL')       IS=.TRUE.
                IF(WC.EQ.'LBL')         IS=.TRUE.
                IF(WC.EQ.'LIC')         IS=.TRUE.
                IF(WC.EQ.'INI')         IS=.TRUE.
                IF(WC.EQ.'LTYPE')       IS=.TRUE.
                IF(WC.EQ.'MODE')        IS=.TRUE.
                IF(WC.EQ.'SPTWT')       IS=.TRUE.
                IF(WC.EQ.'NODUM')       IS=.TRUE.
                IF(WC.EQ.'SPTWT2')      IS=.TRUE.
                IF(WC.EQ.'WV')          IS=.TRUE.
                IF(WC.EQ.'WV2')         IS=.TRUE.
                IF(WC.EQ.'UNITS')       IS=.TRUE.
                IF(WC.EQ.'PCW')         IS=.TRUE.
                IF(WC.EQ.'SCW')         IS=.TRUE.
                IF(WC.EQ.'CW')          IS=.TRUE.
                IF(WC.EQ.'SAY')         IS=.TRUE.
                IF(WC.EQ.'SAX')         IS=.TRUE.
                IF(WC.EQ.'WRX')         IS=.TRUE.
                IF(WC.EQ.'WRY')         IS=.TRUE.
                IF(WC.EQ.'BDX')         IS=.TRUE.
                IF(WC.EQ.'BDY')         IS=.TRUE.
                IF(WC.EQ.'NAOY')        IS=.TRUE.
                IF(WC.EQ.'NAOX')        IS=.TRUE.
                IF(WC.EQ.'FNOY')        IS=.TRUE.
                IF(WC.EQ.'FNOX')        IS=.TRUE.
                IF(WC.EQ.'SCY')         IS=.TRUE.
                IF(WC.EQ.'SCX')         IS=.TRUE.
                IF(WC.EQ.'PYIM')        IS=.TRUE.
                IF(WC.EQ.'PXIM')        IS=.TRUE.
                IF(WC.EQ.'RYIM')        IS=.TRUE.
                IF(WC.EQ.'RXIM')        IS=.TRUE.
                IF(WC.EQ.'CV')          IS=.TRUE.
                IF(WC.EQ.'RD')          IS=.TRUE.
                IF(WC.EQ.'CC')          IS=.TRUE.
                IF(WC.EQ.'ASPH')        IS=.TRUE.
                IF(WC.EQ.'ASPH2')       IS=.TRUE.
                IF(WC.EQ.'YTORIC')      IS=.TRUE.
                IF(WC.EQ.'XTORIC')      IS=.TRUE.
                IF(WC.EQ.'NORMAL')      IS=.TRUE.
                IF(WC.EQ.'SPIDER')      IS=.TRUE.
                IF(WC.EQ.'APY')         IS=.TRUE.
                IF(WC.EQ.'APX')         IS=.TRUE.
                IF(WC.EQ.'PIY')         IS=.TRUE.
                IF(WC.EQ.'PIX')         IS=.TRUE.
                IF(WC.EQ.'PUY')         IS=.TRUE.
                IF(WC.EQ.'PUX')         IS=.TRUE.
                IF(WC.EQ.'APCY')        IS=.TRUE.
                IF(WC.EQ.'APCX')        IS=.TRUE.
                IF(WC.EQ.'PICY')        IS=.TRUE.
                IF(WC.EQ.'PICX')        IS=.TRUE.
                IF(WC.EQ.'PUCY')        IS=.TRUE.
                IF(WC.EQ.'PUCX')        IS=.TRUE.
                IF(WC.EQ.'COCY')        IS=.TRUE.
                IF(WC.EQ.'COCX')        IS=.TRUE.
                IF(WC.EQ.'PIKUP')       IS=.TRUE.
                IF(WC.EQ.'DEC')         IS=.TRUE.
                IF(WC.EQ.'TILT')        IS=.TRUE.
                IF(WC.EQ.'RTILT')       IS=.TRUE.
                IF(WC.EQ.'ASTOP')       IS=.TRUE.
                IF(WC.EQ.'REFS')        IS=.TRUE.
                IF(WC.EQ.'TH')          IS=.TRUE.
                IF(WC.EQ.'THM')         IS=.TRUE.
                IF(WC.EQ.'PRICE')       IS=.TRUE.
                IF(WC.EQ.'AUTOFUNC')    IS=.TRUE.
C     BEGINNING OF NON-SEQUENTIAL COMMANDS
                IF(WC.EQ.'CCR')         IS=.TRUE.
                IF(WC.EQ.'ROO')         IS=.TRUE.
C     END OF NON-SEQUENTIAL COMMANDS
                IF(WC.EQ.'RAYERROR')    IS=.TRUE.
                IF(WC.EQ.'REDSLV')      IS=.TRUE.
                IF(WC.EQ.'PY')          IS=.TRUE.
                IF(WC.EQ.'PCY')         IS=.TRUE.
                IF(WC.EQ.'CAY')         IS=.TRUE.
                IF(WC.EQ.'PX')          IS=.TRUE.
                IF(WC.EQ.'PCX')         IS=.TRUE.
                IF(WC.EQ.'CAX')         IS=.TRUE.
                IF(WC.EQ.'CLAP')        IS=.TRUE.
                IF(WC.EQ.'COBS')        IS=.TRUE.
                IF(WC.EQ.'GLAK')         IS=.TRUE.
                IF(WC.EQ.'GLASS')       IS=.TRUE.
                IF(WC.EQ.'DEFORM')      IS=.TRUE.
                IF(WC.EQ.'DELDEFOR')    IS=.TRUE.
                IF(WC.EQ.'MULTCLAP')    IS=.TRUE.
                IF(WC.EQ.'MULTCOBS')    IS=.TRUE.
                IF(WC.EQ.'SPIDER')      IS=.TRUE.
                IF(WC.EQ.'SPGR')        IS=.TRUE.
                IF(WC.EQ.'MODEL')       IS=.TRUE.
                IF(WC.EQ.'USER')        IS=.TRUE.
                IF(WC.EQ.'GLCAT')       IS=.TRUE.
                IF(WC.EQ.'AIR')         IS=.TRUE.
                IF(WC.EQ.'COATING ')    IS=.TRUE.
                IF(WC.EQ.'REFL')        IS=.TRUE.
                IF(WC.EQ.'REFLTIRO')    IS=.TRUE.
                IF(WC.EQ.'REFLTIR')     IS=.TRUE.
                IF(WC.EQ.'PERFECT')     IS=.TRUE.
                IF(WC.EQ.'IDEAL')       IS=.TRUE.
                IF(WC.EQ.'EOS')         IS=.TRUE.
                IF(WC.EQ.'TASPH')       IS=.TRUE.
                IF(WC.EQ.'C')           IS=.TRUE.
                IF(WC.EQ.'M')           IS=.TRUE.
                IF(WC.EQ.'CHG')         IS=.TRUE.
                IF(WC.EQ.'CVTOR')       IS=.TRUE.
                IF(WC.EQ.'REAL')        IS=.TRUE.
                IF(WC.EQ.'PARAX')       IS=.TRUE.
                IF(WC.EQ.'GRT')         IS=.TRUE.
                IF(WC.EQ.'GRO')         IS=.TRUE.
                IF(WC.EQ.'GRS')         IS=.TRUE.
                IF(WC.EQ.'GRX')         IS=.TRUE.
                IF(WC.EQ.'GRY')         IS=.TRUE.
                IF(WC.EQ.'GRZ')         IS=.TRUE.
                IF(WC.EQ.'RDTOR')       IS=.TRUE.
                IF(WC.EQ.'YTORIC')      IS=.TRUE.
                IF(WC.EQ.'XTORIC')      IS=.TRUE.
                IF(WC.EQ.'NORMAL')      IS=.TRUE.
                IF(WC.EQ.'FOOTBLOK')    IS=.TRUE.
                IF(WC.EQ.'PIVOT')       IS=.TRUE.
                IF(WC.EQ.'PIVAXIS')     IS=.TRUE.
                IF(WC.EQ.'PIVOTD')      IS=.TRUE.
                IF(WC.EQ.'XD')          IS=.TRUE.
                IF(WC.EQ.'YD')          IS=.TRUE.
                IF(WC.EQ.'ZD')          IS=.TRUE.
                IF(WC.EQ.'PIVY')        IS=.TRUE.
                IF(WC.EQ.'PIVX')        IS=.TRUE.
                IF(WC.EQ.'PIVZ')        IS=.TRUE.
                IF(WC.EQ.'ALPHA')       IS=.TRUE.
                IF(WC.EQ.'GDX     ')    IS=.TRUE.
                IF(WC.EQ.'GDY     ')    IS=.TRUE.
                IF(WC.EQ.'GDZ     ')    IS=.TRUE.
                IF(WC.EQ.'GALPHA  ')    IS=.TRUE.
                IF(WC.EQ.'GBETA   ')    IS=.TRUE.
                IF(WC.EQ.'GGAMMA  ')    IS=.TRUE.
                IF(WC.EQ.'TASPHD')      IS=.TRUE.
                IF(WC.EQ.'ARRAYD')      IS=.TRUE.
                IF(WC.EQ.'ARRAY')       IS=.TRUE.
                IF(WC.EQ.'CCTOR')       IS=.TRUE.
                IF(WC.EQ.'ADTOR')       IS=.TRUE.
                IF(WC.EQ.'AETOR')       IS=.TRUE.
                IF(WC.EQ.'AFTOR')       IS=.TRUE.
                IF(WC.EQ.'AGTOR')       IS=.TRUE.
                IF(WC.EQ.'AC')          IS=.TRUE.
                IF(WC.EQ.'AD')          IS=.TRUE.
                IF(WC.EQ.'AE')          IS=.TRUE.
                IF(WC.EQ.'AF')          IS=.TRUE.
                IF(WC.EQ.'AG')          IS=.TRUE.
                IF(WC.EQ.'AH')          IS=.TRUE.
                IF(WC.EQ.'AI')          IS=.TRUE.
                IF(WC.EQ.'AJ')          IS=.TRUE.
                IF(WC.EQ.'AK')          IS=.TRUE.
                IF(WC.EQ.'AL')          IS=.TRUE.
                IF(WC.EQ.'N1')          IS=.TRUE.
                IF(WC.EQ.'N2')          IS=.TRUE.
                IF(WC.EQ.'N3')          IS=.TRUE.
                IF(WC.EQ.'N4')          IS=.TRUE.
                IF(WC.EQ.'N5')          IS=.TRUE.
                IF(WC.EQ.'N6')          IS=.TRUE.
                IF(WC.EQ.'N7')          IS=.TRUE.
                IF(WC.EQ.'N8')          IS=.TRUE.
                IF(WC.EQ.'N9')          IS=.TRUE.
                IF(WC.EQ.'N10')         IS=.TRUE.
                IF(WC.EQ.'INDEX')       IS=.TRUE.
                IF(WC.EQ.'BETA')        IS=.TRUE.
                IF(WC.EQ.'GAMMA')       IS=.TRUE.
                IF(WC.EQ.'ASPHD')       IS=.TRUE.
                IF(WC.EQ.'TORD')        IS=.TRUE.
                IF(WC.EQ.'TILTD')       IS=.TRUE.
                IF(WC.EQ.'CSD')         IS=.TRUE.
                IF(WC.EQ.'CSDX')        IS=.TRUE.
                IF(WC.EQ.'CSDY')        IS=.TRUE.
                IF(WC.EQ.'TSD')         IS=.TRUE.
                IF(WC.EQ.'GRTD')        IS=.TRUE.
                IF(WC.EQ.'PIKD')        IS=.TRUE.
                IF(WC.EQ.'CLAPD')       IS=.TRUE.
                IF(WC.EQ.'COBSD')       IS=.TRUE.
                IF(WC.EQ.'INR')         IS=.TRUE.
                IF(WC.EQ.'INRD')        IS=.TRUE.
                IF(WC.EQ.'ZERO')        IS=.TRUE.
C
C       THE FOLLOWING SPSRF COMMANDS
C        ARE VALID AS WELL
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'ON')        IS=.TRUE.
                IF(WC.EQ.'SPSRF'.AND.WQ.EQ.'OFF')       IS=.TRUE.
                IF(WC.EQ.'SPDEL')                       IS=.TRUE.
                !IF(WC.EQ.'SPECIAL')                     IS=.TRUE.
                IF(WC.EQ.'GENL')                        IS=.TRUE.
                IF(WC.EQ.'C1')                          IS=.TRUE.
                IF(WC.EQ.'C2')                          IS=.TRUE.
                IF(WC.EQ.'C3')                          IS=.TRUE.
                IF(WC.EQ.'C4')                          IS=.TRUE.
                IF(WC.EQ.'C5')                          IS=.TRUE.
                IF(WC.EQ.'C6')                          IS=.TRUE.
                IF(WC.EQ.'C7')                          IS=.TRUE.
                IF(WC.EQ.'C8')                          IS=.TRUE.
                IF(WC.EQ.'C9')                          IS=.TRUE.
                IF(WC.EQ.'C10')                         IS=.TRUE.
                IF(WC.EQ.'C11')                         IS=.TRUE.
                IF(WC.EQ.'C12')                         IS=.TRUE.
                IF(WC.EQ.'C13')                         IS=.TRUE.
                IF(WC.EQ.'C14')                         IS=.TRUE.
                IF(WC.EQ.'C15')                         IS=.TRUE.
                IF(WC.EQ.'C16')                         IS=.TRUE.
                IF(WC.EQ.'C17')                         IS=.TRUE.
                IF(WC.EQ.'C18')                         IS=.TRUE.
                IF(WC.EQ.'C19')                         IS=.TRUE.
                IF(WC.EQ.'C20')                         IS=.TRUE.
                IF(WC.EQ.'C21')                         IS=.TRUE.
                IF(WC.EQ.'C22')                         IS=.TRUE.
                IF(WC.EQ.'C23')                         IS=.TRUE.
                IF(WC.EQ.'C24')                         IS=.TRUE.
                IF(WC.EQ.'C25')                         IS=.TRUE.
                IF(WC.EQ.'C26')                         IS=.TRUE.
                IF(WC.EQ.'C27')                         IS=.TRUE.
                IF(WC.EQ.'C28')                         IS=.TRUE.
                IF(WC.EQ.'C29')                         IS=.TRUE.
                IF(WC.EQ.'C30')                         IS=.TRUE.
                IF(WC.EQ.'C31')                         IS=.TRUE.
                IF(WC.EQ.'C32')                         IS=.TRUE.
                IF(WC.EQ.'C33')                         IS=.TRUE.
                IF(WC.EQ.'C34')                         IS=.TRUE.
                IF(WC.EQ.'C35')                         IS=.TRUE.
                IF(WC.EQ.'C36')                         IS=.TRUE.
                IF(WC.EQ.'C37')                         IS=.TRUE.
                IF(WC.EQ.'C38')                         IS=.TRUE.
                IF(WC.EQ.'C39')                         IS=.TRUE.
                IF(WC.EQ.'C40')                         IS=.TRUE.
                IF(WC.EQ.'C41')                         IS=.TRUE.
                IF(WC.EQ.'C42')                         IS=.TRUE.
                IF(WC.EQ.'C43')                         IS=.TRUE.
                IF(WC.EQ.'C44')                         IS=.TRUE.
                IF(WC.EQ.'C45')                         IS=.TRUE.
                IF(WC.EQ.'C46')                         IS=.TRUE.
                IF(WC.EQ.'C47')                         IS=.TRUE.
                IF(WC.EQ.'C48')                         IS=.TRUE.
                IF(WC.EQ.'C49')                         IS=.TRUE.
                IF(WC.EQ.'C50')                         IS=.TRUE.
                IF(WC.EQ.'C51')                         IS=.TRUE.
                IF(WC.EQ.'C52')                         IS=.TRUE.
                IF(WC.EQ.'C53')                         IS=.TRUE.
                IF(WC.EQ.'C54')                         IS=.TRUE.
                IF(WC.EQ.'C55')                         IS=.TRUE.
                IF(WC.EQ.'C56')                         IS=.TRUE.
                IF(WC.EQ.'C57')                         IS=.TRUE.
                IF(WC.EQ.'C58')                         IS=.TRUE.
                IF(WC.EQ.'C59')                         IS=.TRUE.
                IF(WC.EQ.'C60')                         IS=.TRUE.
                IF(WC.EQ.'C61')                         IS=.TRUE.
                IF(WC.EQ.'C62')                         IS=.TRUE.
                IF(WC.EQ.'C63')                         IS=.TRUE.
                IF(WC.EQ.'C64')                         IS=.TRUE.
                IF(WC.EQ.'C65')                         IS=.TRUE.
                IF(WC.EQ.'C66')                         IS=.TRUE.
                IF(WC.EQ.'C67')                         IS=.TRUE.
                IF(WC.EQ.'C68')                         IS=.TRUE.
                IF(WC.EQ.'C69')                         IS=.TRUE.
                IF(WC.EQ.'C70')                         IS=.TRUE.
                IF(WC.EQ.'C71')                         IS=.TRUE.
                IF(WC.EQ.'C72')                         IS=.TRUE.
                IF(WC.EQ.'C73')                         IS=.TRUE.
                IF(WC.EQ.'C74')                         IS=.TRUE.
                IF(WC.EQ.'C75')                         IS=.TRUE.
                IF(WC.EQ.'C76')                         IS=.TRUE.
                IF(WC.EQ.'C77')                         IS=.TRUE.
                IF(WC.EQ.'C78')                         IS=.TRUE.
                IF(WC.EQ.'C79')                         IS=.TRUE.
                IF(WC.EQ.'C80')                         IS=.TRUE.
                IF(WC.EQ.'C81')                         IS=.TRUE.
                IF(WC.EQ.'C82')                         IS=.TRUE.
                IF(WC.EQ.'C83')                         IS=.TRUE.
                IF(WC.EQ.'C84')                         IS=.TRUE.
                IF(WC.EQ.'C85')                         IS=.TRUE.
                IF(WC.EQ.'C86')                         IS=.TRUE.
                IF(WC.EQ.'C87')                         IS=.TRUE.
                IF(WC.EQ.'C88')                         IS=.TRUE.
                IF(WC.EQ.'C89')                         IS=.TRUE.
                IF(WC.EQ.'C90')                         IS=.TRUE.
                IF(WC.EQ.'C91')                         IS=.TRUE.
                IF(WC.EQ.'C92')                         IS=.TRUE.
                IF(WC.EQ.'C93')                         IS=.TRUE.
                IF(WC.EQ.'C94')                         IS=.TRUE.
                IF(WC.EQ.'C95')                         IS=.TRUE.
                IF(WC.EQ.'C96')                         IS=.TRUE.
C
C      ALSO   THE FOLLOWING CMD LEVEL COMMANDS
C
                IF(WC.EQ.'UPDATE'.OR.WC.EQ.'U') THEN
                IF(WQ.EQ.'LENS'.OR.WQ.EQ.'L')           IS=.TRUE.
                IF(WQ.EQ.'SP'.OR.WQ.EQ.'SPSRF')         IS=.TRUE.
                               END IF
                IF(WC.EQ.'MODE')                        IS=.TRUE.
                IF(WC.EQ.'FNBX')                        IS=.TRUE.
                IF(WC.EQ.'FNBY')                        IS=.TRUE.
                IF(WC.EQ.'ERX')                         IS=.TRUE.
                IF(WC.EQ.'ERY')                         IS=.TRUE.
                IF(WC.EQ.'SPTWT')                       IS=.TRUE.
                IF(WC.EQ.'NODUM')       IS=.TRUE.
                IF(WC.EQ.'SPTWT2')                      IS=.TRUE.
                IF(WC.EQ.'THERM')                       IS=.TRUE.
                IF(WC.EQ.'PRES')                        IS=.TRUE.
                IF(WC.EQ.'MAGY')                        IS=.TRUE.
                IF(WC.EQ.'MAGX')                        IS=.TRUE.
                IF(WC.EQ.'FLDS')                        IS=.TRUE.
                IF(WC.EQ.'CFG')                         IS=.TRUE.
                IF(WC.EQ.'GET     ') IS=.TRUE.
                IF(WC.EQ.'AGET    ') IS=.TRUE.
                IF(WC.EQ.'RCL     ') IS=.TRUE.
                IF(WC.EQ.'STOK    ') IS=.TRUE.
                IF(WC.EQ.'WRITE   ') IS=.TRUE.
                IF(WC.EQ.'MAXVAL  ') IS=.TRUE.
                IF(WC.EQ.'MINVAL  ') IS=.TRUE.
                IF(WC.EQ.'PI      ') IS=.TRUE.
                IF(WC.EQ.'CHS     ') IS=.TRUE.
                IF(WC.EQ.'MOVE    ') IS=.TRUE.
                IF(WC.EQ.'LASTX   ') IS=.TRUE.
                IF(WC.EQ.'LASTIX  ') IS=.TRUE.
                IF(WC.EQ.'INCR    ') IS=.TRUE.
                IF(WC.EQ.'PLUS    ') IS=.TRUE.
                IF(WC.EQ.'MINUS   ') IS=.TRUE.
                IF(WC.EQ.'MPY     ') IS=.TRUE.
                IF(WC.EQ.'DIV     ') IS=.TRUE.
                IF(WC.EQ.'SQRT    ') IS=.TRUE.
                IF(WC.EQ.'SIN     ') IS=.TRUE.
                IF(WC.EQ.'COS     ') IS=.TRUE.
                IF(WC.EQ.'TAN     ') IS=.TRUE.
                IF(WC.EQ.'SINH    ') IS=.TRUE.
                IF(WC.EQ.'COSH    ') IS=.TRUE.
                IF(WC.EQ.'TANH    ') IS=.TRUE.
                IF(WC.EQ.'CLSTK   ') IS=.TRUE.
                IF(WC.EQ.'CLSTKI  ') IS=.TRUE.
                IF(WC.EQ.'CLSTKC  ') IS=.TRUE.
                IF(WC.EQ.'CLREG   ') IS=.TRUE.
                IF(WC.EQ.'CLGREG  ') IS=.TRUE.
                IF(WC.EQ.'CLX     ') IS=.TRUE.
                IF(WC.EQ.'CLIX    ') IS=.TRUE.
                IF(WC.EQ.'ASIN    ') IS=.TRUE.
                IF(WC.EQ.'ACOS    ') IS=.TRUE.
                IF(WC.EQ.'ATAN    ') IS=.TRUE.
                IF(WC.EQ.'ABS     ') IS=.TRUE.
                IF(WC.EQ.'EXP     ') IS=.TRUE.
                IF(WC.EQ.'LN      ') IS=.TRUE.
                IF(WC.EQ.'LOG10   ') IS=.TRUE.
                IF(WC.EQ.'FACT    ') IS=.TRUE.
                IF(WC.EQ.'SGN     ') IS=.TRUE.
                IF(WC.EQ.'RECIP   ') IS=.TRUE.
                IF(WC.EQ.'INTGR   ') IS=.TRUE.
                IF(WC.EQ.'FRAC    ') IS=.TRUE.
                IF(WC.EQ.'POW     ') IS=.TRUE.
                IF(WC.EQ.'STORE   ') IS=.TRUE.
                IF(WC.EQ.'RAND    ') IS=.TRUE.
                IF(WC.EQ.'MOD     ') IS=.TRUE.
                IF(WC.EQ.'CLSTREG ') IS=.TRUE.
                IF(WC.EQ.'STADD   ') IS=.TRUE.
                IF(WC.EQ.'STSUB   ') IS=.TRUE.
                IF(WC.EQ.'MEAN    ') IS=.TRUE.
                IF(WC.EQ.'STDEV   ') IS=.TRUE.
                IF(WC.EQ.'RTD     ') IS=.TRUE.
                IF(WC.EQ.'DTR     ') IS=.TRUE.
                IF(WC.EQ.'ATAN2   ') IS=.TRUE.
                IF(WC.EQ.'J1      ') IS=.TRUE.
                IF(WC.EQ.'PREAD   ') IS=.TRUE.
                IF(WC.EQ.'ATON    ') IS=.TRUE.
                IF(WC.EQ.'STOAX   ') IS=.TRUE.
                IF(WC.EQ.'ARCL    ') IS=.TRUE.
                IF(WC.EQ.'AWRITE  ') IS=.TRUE.
                IF(WC.EQ.'ASTO    ') IS=.TRUE.
                IF(WC.EQ.'CLASTO  ') IS=.TRUE.
                IF(WC.EQ.'SHOW    ') IS=.TRUE.
                IF(WC.EQ.'ENT     ') IS=.TRUE.
                IF(WC.EQ.'ENTI    ') IS=.TRUE.
                IF(WC.EQ.'ENTC    ') IS=.TRUE.
                IF(WC.EQ.'PULL    ') IS=.TRUE.
                IF(WC.EQ.'IPULL   ') IS=.TRUE.
                IF(WC.EQ.'CPULL   ') IS=.TRUE.
                IF(WC.EQ.'RUP     ') IS=.TRUE.
                IF(WC.EQ.'IRUP    ') IS=.TRUE.
                IF(WC.EQ.'CRUP    ') IS=.TRUE.
                IF(WC.EQ.'RDN     ') IS=.TRUE.
                IF(WC.EQ.'IRDN    ') IS=.TRUE.
                IF(WC.EQ.'CRDN    ') IS=.TRUE.
                IF(WC.EQ.'X-Y     ') IS=.TRUE.
                IF(WC.EQ.'IX-IY   ') IS=.TRUE.
                IF(WC.EQ.'+       ') IS=.TRUE.
                IF(WC.EQ.'-       ') IS=.TRUE.
                IF(WC.EQ.'*       ') IS=.TRUE.
                IF(WC.EQ.'/       ') IS=.TRUE.
                IF(WC.EQ.'I+      ') IS=.TRUE.
                IF(WC.EQ.'I-      ') IS=.TRUE.
                IF(WC.EQ.'I*      ') IS=.TRUE.
                IF(WC.EQ.'I/      ') IS=.TRUE.
                IF(WC.EQ.'C+      ') IS=.TRUE.
                IF(WC.EQ.'C-      ') IS=.TRUE.
                IF(WC.EQ.'C*      ') IS=.TRUE.
                IF(WC.EQ.'C/      ') IS=.TRUE.
                IF(WC.EQ.'Y**X    ') IS=.TRUE.
                IF(WC.EQ.'IY**IX  ') IS=.TRUE.
                IF(WC.EQ.'CY**CX  ') IS=.TRUE.
                IF(WC.EQ.'P-R     ') IS=.TRUE.
                IF(WC.EQ.'R-P     ') IS=.TRUE.
                IF(WC.EQ.'CYL-R   ') IS=.TRUE.
                IF(WC.EQ.'R-CYL   ') IS=.TRUE.
                IF(WC.EQ.'SP-R    ') IS=.TRUE.
                IF(WC.EQ.'R-SP    ') IS=.TRUE.
                IF(WC.EQ.'RE-IM   ') IS=.TRUE.
                IF(WC.EQ.'IM-RE   ') IS=.TRUE.
                IF(WC.EQ.'H-HMS   ') IS=.TRUE.
                IF(WC.EQ.'HMS-H   ') IS=.TRUE.
                IF(WC.EQ.'IN-MM   ') IS=.TRUE.
                IF(WC.EQ.'IN-CM   ') IS=.TRUE.
                IF(WC.EQ.'IN-M    ') IS=.TRUE.
                IF(WC.EQ.'MM-IN   ') IS=.TRUE.
                IF(WC.EQ.'CM-IN   ') IS=.TRUE.
                IF(WC.EQ.'M-IN    ') IS=.TRUE.
                IF(WC.EQ.'ARCL')        IS=.TRUE.
                IF(WC.EQ.'STOAX')       IS=.TRUE.
                IF(WC.EQ.'SET')         IS=.TRUE.
                IF(WC.EQ.'W1')          IS=.TRUE.
                IF(WC.EQ.'W2')          IS=.TRUE.
                IF(WC.EQ.'W3')          IS=.TRUE.
                IF(WC.EQ.'W4')          IS=.TRUE.
                IF(WC.EQ.'W5')          IS=.TRUE.
                        RETURN
                        END
C SUB CONT1.FOR
        SUBROUTINE CONT1(STP)
C
C       THIS CHECKS FOR INVALID COMMANDS ISSUED FROM THE
C       CMD LEVEL WHEN SPECT IS NOT BEING USED AND THE COMMANDS
C       ARE NOT COMMING FROM WITHIN A MACRO.
C
        IMPLICIT NONE
C
      LOGICAL IS,STP
C
      INCLUDE 'DATMAI.INC'
C
                        IS=.FALSE.
        IF(WC.EQ.'START')       IS=.TRUE.
        IF(WC.EQ.'WAVLN')       IS=.TRUE.
        IF(WC.EQ.'INT')         IS=.TRUE.
        IF(WC.EQ.'DIRECT')      IS=.TRUE.
C       IF(WC.EQ.'LOADPHOT')    IS=.TRUE.
C       IF(WC.EQ.'LOADSCOT')    IS=.TRUE.
        IF(WC.EQ.'INSERT')      IS=.TRUE.
        IF(WC.EQ.'DROP')        IS=.TRUE.
        IF(WC.EQ.'DELETE')      IS=.TRUE.
        IF(WC.EQ.'GETFILE')     IS=.TRUE.
        IF(WC.EQ.'BLACKBDY')    IS=.TRUE.
        IF(WC.EQ.'PHOTOPIC')    IS=.TRUE.
        IF(WC.EQ.'SCOTOPIC')    IS=.TRUE.
        IF(WC.EQ.'PUT')         IS=.TRUE.
        IF(WC.EQ.'LIST')        IS=.TRUE.
        IF(WC.EQ.'RENAME')      IS=.TRUE.
        IF(WC.EQ.'PUNCH')       IS=.TRUE.
        IF(WC.EQ.'SPRINT')      IS=.TRUE.
        IF(WC.EQ.'INTER')       IS=.TRUE.
        IF(WC.EQ.'NARCIN')      IS=.TRUE.
        IF(WC.EQ.'FLNAME')      IS=.TRUE.
        IF(WC.EQ.'ENDTABLE')    IS=.TRUE.
        IF(WC.EQ.'NARC')        IS=.TRUE.
        IF(WC.EQ.'EOS')         IS=.TRUE.
        IF(WC.EQ.'DATA')        IS=.TRUE.
        IF(WC.EQ.'CUME')        IS=.TRUE.
        IF(WC.EQ.'WFACTOR')     IS=.TRUE.
        IF(WC.EQ.'WORK')        IS=.TRUE.
        IF(WC.EQ.'PTABLE')      IS=.TRUE.
        IF(WC.EQ.'DIR')         IS=.TRUE.
        IF(WC.EQ.'FILE')        IS=.TRUE.
        IF(WC.EQ.'NAME')        IS=.TRUE.
        IF(WC.EQ.'NSUB')        IS=.TRUE.
        IF(WC.EQ.'QSUB')        IS=.TRUE.
        IF(WC.EQ.'SSUB')        IS=.TRUE.
        IF(WC.EQ.'QRSUB')       IS=.TRUE.
        IF(WC.EQ.'CSUB')        IS=.TRUE.
        IF(WC.EQ.'CRSUB')       IS=.TRUE.
        IF(WC.EQ.'PUTR')        IS=.TRUE.
        IF(WC.EQ.'TRACE')       IS=.TRUE.
        IF(WC.EQ.'ACCSUB')      IS=.TRUE.
        IF(WC.EQ.'RETURN')      IS=.TRUE.
        IF(WC.EQ.'BP')          IS=.TRUE.
        IF(WC.EQ.'BRQ')         IS=.TRUE.
        IF(WC.EQ.'BPOS')        IS=.TRUE.
        IF(WC.EQ.'BRDQ')        IS=.TRUE.
        IF(WC.EQ.'BRDF1')       IS=.TRUE.
        IF(WC.EQ.'BRDF2')       IS=.TRUE.
        IF(WC.EQ.'BRDF3')       IS=.TRUE.
        IF(WC.EQ.'BRDF4')       IS=.TRUE.
        IF(WC.EQ.'BRDF5')       IS=.TRUE.
        IF(WC.EQ.'BNEG')        IS=.TRUE.
        IF(WC.EQ.'BZE')         IS=.TRUE.
        IF(WC.EQ.'IF(X>0)')     IS=.TRUE.
        IF(WC.EQ.'IF(X<0)')     IS=.TRUE.
        IF(WC.EQ.'IF(X=0)')     IS=.TRUE.
        IF(WC.EQ.'IF(X>Y)')     IS=.TRUE.
        IF(WC.EQ.'IF(X<Y)')     IS=.TRUE.
        IF(WC.EQ.'IF(X=Y)')     IS=.TRUE.
        IF(WC.EQ.'BRI')         IS=.TRUE.
        IF(WC.EQ.'BRJ')         IS=.TRUE.
        IF(WC.EQ.'BRK')         IS=.TRUE.
        IF(WC.EQ.'BRL')         IS=.TRUE.
        IF(WC.EQ.'BRM')         IS=.TRUE.
        IF(WC.EQ.'BRM')         IS=.TRUE.
        IF(WC.EQ.'BRU')         IS=.TRUE.
        IF(WC.EQ.'BRANCH')      IS=.TRUE.
        IF(WC.EQ.'BRT')         IS=.TRUE.
        IF(WC.EQ.'BRF')         IS=.TRUE.
        IF(WC.EQ.'BRERR')       IS=.TRUE.
        IF(WC.EQ.'PAUSE')       IS=.TRUE.
        IF(WC.EQ.'SSTEP')       IS=.TRUE.
                IF(IS) THEN
      OUTLYNE='INVALID CMD LEVEL COMMAND'
      CALL SHOWIT(1)
                STP=.TRUE.
                CALL MACFAL
                RETURN
                ELSE
                STP=.FALSE.
                END IF
                RETURN
                END
C SUB CONT2.FOR
        SUBROUTINE CONT2(STP)
C
C       THIS CHECKS FOR INVALID SPECT COMMANDS GIVEN FROM WITHIN THE
C       SPECTRAL SUBFILE WHEN NOT COMMING FROM WITHIN A MACRO.
C
        IMPLICIT NONE
C
      INCLUDE 'DATMAI.INC'
C
        LOGICAL STP,IS
C
                        IS=.FALSE.
                IF(WC.EQ.'EXIT')        IS=.TRUE.
                IF(WC.EQ.'EXI')         IS=.TRUE.
                IF(WC.EQ.'LENS')        IS=.TRUE.
                IF(WC.EQ.'CONFIGS')     IS=.TRUE.
                IF(WC.EQ.'CONFIG ')     IS=.TRUE.
                IF(WC.EQ.'SPSRF')       IS=.TRUE.
                IF(WC.EQ.'SPFIT')       IS=.TRUE.
                IF(WC.EQ.'SPECT')       IS=.TRUE.
                IF(WC.EQ.'MACRO')       IS=.TRUE.
                IF(WC.EQ.'LMEDIT')      IS=.TRUE.
                IF(WC.EQ.'UPDATE')      IS=.TRUE.
                IF(WC.EQ.'U')           IS=.TRUE.
                IF(WC.EQ.'RAYSET')      IS=.TRUE.
                IF(WC.EQ.'DEP')         IS=.TRUE.
                IF(WC.EQ.'FOE')         IS=.TRUE.
                IF(WC.EQ.'VARIABLE')    IS=.TRUE.
                IF(WC.EQ.'VARI')        IS=.TRUE.
                IF(WC.EQ.'TVAR')        IS=.TRUE.
                IF(WC.EQ.'TOPER')       IS=.TRUE.
                IF(WC.EQ.'FOCRIT')      IS=.TRUE.
                IF(WC.EQ.'COMPVAR')     IS=.TRUE.
                IF(WC.EQ.'MERIT')       IS=.TRUE.
                IF(WC.EQ.'LAYOUT')      IS=.TRUE.
                IF(WC.EQ.'NSUB')        IS=.TRUE.
                IF(WC.EQ.'QSUB')        IS=.TRUE.
                IF(WC.EQ.'SSUB')        IS=.TRUE.
                IF(WC.EQ.'QRSUB')       IS=.TRUE.
                IF(WC.EQ.'CSUB')        IS=.TRUE.
                IF(WC.EQ.'CRSUB')       IS=.TRUE.
                IF(WC.EQ.'PUTR')        IS=.TRUE.
                IF(WC.EQ.'TRACE')       IS=.TRUE.
                IF(WC.EQ.'ACCSUB')      IS=.TRUE.
                IF(WC.EQ.'RETURN')      IS=.TRUE.
                IF(WC.EQ.'BP')          IS=.TRUE.
                IF(WC.EQ.'BRQ')         IS=.TRUE.
                IF(WC.EQ.'BPOS')        IS=.TRUE.
                IF(WC.EQ.'BRDQ')        IS=.TRUE.
                IF(WC.EQ.'BRDF1')       IS=.TRUE.
                IF(WC.EQ.'BRDF2')       IS=.TRUE.
                IF(WC.EQ.'BRDF3')       IS=.TRUE.
                IF(WC.EQ.'BRDF4')       IS=.TRUE.
                IF(WC.EQ.'BRDF5')       IS=.TRUE.
                IF(WC.EQ.'BNEG')        IS=.TRUE.
                IF(WC.EQ.'BZE')         IS=.TRUE.
                IF(WC.EQ.'IF(X>0)')     IS=.TRUE.
                IF(WC.EQ.'IF(X<0)')     IS=.TRUE.
                IF(WC.EQ.'IF(X=0)')     IS=.TRUE.
                IF(WC.EQ.'IF(X>Y)')     IS=.TRUE.
                IF(WC.EQ.'IF(X<Y)')     IS=.TRUE.
                IF(WC.EQ.'IF(X=Y)')     IS=.TRUE.
                IF(WC.EQ.'BRI')         IS=.TRUE.
                IF(WC.EQ.'BRJ')         IS=.TRUE.
                IF(WC.EQ.'BRK')         IS=.TRUE.
                IF(WC.EQ.'BRL')         IS=.TRUE.
                IF(WC.EQ.'BRM')         IS=.TRUE.
                IF(WC.EQ.'BRN')         IS=.TRUE.
                IF(WC.EQ.'BRU')         IS=.TRUE.
                IF(WC.EQ.'BRANCH')      IS=.TRUE.
                IF(WC.EQ.'BRT')         IS=.TRUE.
                IF(WC.EQ.'BRF')         IS=.TRUE.
                IF(WC.EQ.'BRERR')       IS=.TRUE.
                IF(WC.EQ.'PAUSE')       IS=.TRUE.
                IF(WC.EQ.'SSTEP')       IS=.TRUE.
                        IF(IS) THEN
      OUTLYNE='INVALID SPECT LEVEL COMMAND'
      CALL SHOWIT(1)
                STP=.TRUE.
                CALL MACFAL
                RETURN
                ELSE
                STP=.FALSE.
                END IF
                                RETURN
                                END
C       FIFTH SET OF UTILTIY ROUTINES GO HERE
