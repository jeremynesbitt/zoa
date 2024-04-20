C       THIRTEENTH FILE FOR LENS DATABASE MANAGER FILES

C SUB FNDGLS.FOR
        SUBROUTINE FNDGLS
C
        IMPLICIT NONE
C
C       THIS IS SUBROUTINE FNDGLS WHICH IMPLEMENTS THE FINDGLAS
C       COMMAND AT THE CMD LEVEL.
C
        INTEGER NUMSRCH,ALLOERR,I,J,K,TOTAL,II,JJ,KK
        REAL*8 SORTIT,A0,A1,A2,A3,A4,A5,D,NMOD,NCAT
        REAL*8 LAMBDA,PN,PNSC,SORTIT2
        DIMENSION SORTIT(:),SORTIT2(:)
        CHARACTER ASORTIT*13,NAME*13,NUMBER*13,ASORTIT2*13
        DIMENSION ASORTIT(:,:),ASORTIT2(:,:)
        ALLOCATABLE :: SORTIT,ASORTIT,SORTIT2,ASORTIT2
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        PN(LAMBDA,A0,A1,A2,A3,A4,A5)=
     1  DSQRT(A0+(A1*(LAMBDA**2))+(A2*(1.0D0/(LAMBDA**2)))+
     2  (A3*(1.0D0/(LAMBDA**4)))+(A4*(1.0D0/(LAMBDA**6)))+
     3  (A5*(1.0D0/(LAMBDA**8))))
C
        PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)=
     1  DSQRT(
     1  ((A0*(LAMBDA**2))/((LAMBDA**2)-A3))+
     2  ((A1*(LAMBDA**2))/((LAMBDA**2)-A4))+
     2  ((A2*(LAMBDA**2))/((LAMBDA**2)-A5))+1.0D0)
C
        DEALLOCATE(SORTIT,ASORTIT,SORTIT2,ASORTIT2,STAT=ALLOERR)
C
      IF(STI.EQ.1) THEN
      OUTLYNE='"FINDGLAS" SEARCHES FOR GLASSES CLOSE TO A "MODEL" GLASS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        RETURN
                        END IF
      IF(SST.EQ.1) THEN
        OUTLYNE='"FINDGLAS" TAKES NO STRING INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(SQ.EQ.0) THEN
               SQ=1
               WQ='ALL'
                        END IF
      IF(WQ.NE.'SCHOTT'.AND.WQ.NE.'HOYA'.AND.WQ.NE.'OHARA'
     1.AND.WQ.NE.'CORNIN'.AND.WQ.NE.'CHANCE'.AND.WQ.NE.'ALL'
     1.AND.WQ.NE.'HIKARI'.AND.WQ.NE.'SCH2000') THEN
        OUTLYNE='INVALID CATALOG NAME ISSUED WITH "FINDGLAS"'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
        OUTLYNE='"FINDGLAS" ONLY TAKES NUMERIC WORDS #1 AND #2'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(DF1.EQ.1) THEN
        OUTLYNE='"FINDGLAS" REQUIRES EXPLICIT NUMERIC WORD #1 INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
        OUTLYNE='SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(DF2.EQ.1) THEN
                   DF2=0
                   S2=1
                   W2=5.0D0
                   END IF
      IF(W2.LT.1.0D0) THEN
      OUTLYNE='"N" MUST BE 1 GREATER'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      NUMSRCH=INT(W2)
      IF(W1.LT.0.0D0.OR.W1.GT.SYSTEM(20)) THEN
      OUTLYNE='SURFACE NUMBER BEYOND LEGAL BOUNDS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(GLANAM(INT(W1),1)(1:5).NE.'MODEL') THEN
      OUTLYNE='GLASS IS NOT A "MODEL" GLASS'
      CALL SHOWIT(1)
        OUTLYNE='"FINDGLAS" TOOK NO ACTION'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
        ALLOCATE(SORTIT(1:10000),ASORTIT(1:2,1:10000),STAT=ALLOERR)
                   SORTIT(1:10000)=0.0D0
                   ASORTIT(1:2,1:10000)='             '
C     NOW SEARCH THE CATALOGS AND CALCULATE THE DISTANCE AND TABULATE IT
      IF(WQ.EQ.'SCHOTT') THEN
               K=0
        OPEN(UNIT=99,ACCESS='DIRECT',
     1     FILE=trim(LIBGLA)//'SCHOTT.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='SCHOTT       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'SCH2000') THEN
               K=0
        OPEN(UNIT=99,ACCESS='DIRECT',
     1    FILE=trim(LIBGLA)//'SCH2000.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='SCH2000      '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'HOYA') THEN
               K=0
      OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'HOYA.BIN',
     1FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
      READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='HOYA         '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'HIKARI') THEN
               K=0
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'HIKARI.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='HIKARI       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'OHARA') THEN
               K=0
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='OHARA        '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
C     NOW THE SECOND OHARA FILE
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='OHARA        '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'CORNIN') THEN
               K=0
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'CORNIN.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='CORNIN       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'CHANCE') THEN
               K=0
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'CHANCE.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='CHANCE       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
      IF(WQ.EQ.'ALL') THEN
                       K=0
        OPEN(UNIT=99,ACCESS='DIRECT',
     1  FILE=trim(LIBGLA)//'SCHOTT.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='SCHOTT       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'SCH2000.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='SCH2000      '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'HOYA.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='HOYA         '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'HIKARI.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='HIKARI       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'OHARA.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PNSC(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='OHARA        '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'OHARA-O.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='OHARA        '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'CORNIN.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='CORNIN       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
        OPEN(UNIT=99,ACCESS='DIRECT',FILE=LIBGLA//'CHANCE.BIN',
     1  FORM='UNFORMATTED',RECL=(NRECL*33),STATUS='UNKNOWN')
        READ(UNIT=99,REC=1) TOTAL
                        DO J=2,TOTAL+1
      READ(UNIT=99,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
C     CALCULATE DISTANCE AND THEN PLACE IN THE SORTING ARRAY
      D=0.0D0
                        DO I=1,10
      IF(I.EQ.1)  II=31
      IF(I.EQ.2)  II=32
      IF(I.EQ.3)  II=33
      IF(I.EQ.4)  II=34
      IF(I.EQ.5)  II=35
      IF(I.EQ.6)  II=76
      IF(I.EQ.7)  II=77
      IF(I.EQ.8)  II=78
      IF(I.EQ.9)  II=79
      IF(I.EQ.10) II=80
      IF(I.EQ.1)  JJ=1
      IF(I.EQ.2)  JJ=2
      IF(I.EQ.3)  JJ=3
      IF(I.EQ.4)  JJ=4
      IF(I.EQ.5)  JJ=5
      IF(I.EQ.6)  JJ=71
      IF(I.EQ.7)  JJ=72
      IF(I.EQ.8)  JJ=73
      IF(I.EQ.9)  JJ=74
      IF(I.EQ.10) JJ=75
      IF(I.EQ.1)  KK=46
      IF(I.EQ.2)  KK=47
      IF(I.EQ.3)  KK=48
      IF(I.EQ.4)  KK=49
      IF(I.EQ.5)  KK=50
      IF(I.EQ.6)  KK=71
      IF(I.EQ.7)  KK=72
      IF(I.EQ.8)  KK=73
      IF(I.EQ.9)  KK=74
      IF(I.EQ.10) KK=75
      IF(SYSTEM(II).NE.0.0D0) THEN
C     SPECTRAL WEIGHT NOT ZERO
      IF(JJ.GE.1.AND.JJ.LE.5)
     1LAMBDA=SYSTEM(JJ)
      IF(JJ.GE.6.AND.JJ.LE.10)
     1LAMBDA=SYSTEM(65+JJ)
C     GET INDEX OF MODEL GLASS
      NMOD=ALENS(KK,INT(W1))
C     GET INDEX OF CAT GLASS
      NCAT=PN(LAMBDA,A0,A1,A2,A3,A4,A5)
      D=D+((NMOD-NCAT)**2)
                       END IF
                        END DO
                   K=K+1
      ASORTIT(1,K)='CHANCE       '
      ASORTIT(2,K)=NAME
      SORTIT(K)=DSQRT(D)
                        END DO
C     FINISHED LOADING, GO TO THE SORT
      CALL CLOSE_FILE(99,1)
                        GO TO 999
                        END IF
 999                    CONTINUE
C     SORT SORTIT IN ASSENDING ORDER AND REORDER
C      THE ASORTIT ARRAY AT THE SAME TIME
        ALLOCATE(SORTIT2(1:K),ASORTIT2(1:2,1:K),STAT=ALLOERR)
        SORTIT2(1:K)=SORTIT(1:K)
        ASORTIT2(1:2,1:K)=ASORTIT(1:2,1:K)
      CALL SORT_JK(K,SORTIT2,ASORTIT2)
 10   FORMAT('THE CLOSEST ',I3,' CATALOG GLASSES')
 20   FORMAT('FOUND IN THE LAST "FINDGLAS" SEARCH')
 30   FORMAT('FOR SURFACE #',I3,' ARE:')
      WRITE(OUTLYNE,10) NUMSRCH
      CALL SHOWIT(0)
      WRITE(OUTLYNE,20)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,30) INT(W1)
      CALL SHOWIT(0)
 50   FORMAT('  # ',1X,'CAT. NAME    ',1X,'GLASS NAME   ',6X,'DISTANCE')
      WRITE(OUTLYNE,50)
      CALL SHOWIT(0)
        IF(NUMSRCH.GT.5000) NUMSRCH=5000
                       DO I=1,NUMSRCH

      IF(ASORTIT2(1,I)(1:13).NE.'SCHOTT       '.AND.
     1 ASORTIT2(1,I)(1:13).NE.'SCH2000      '.AND.
     1 ASORTIT2(1,I)(1:13).NE.'OHARA        '.AND.
     2 ASORTIT2(1,I)(1:13).NE.'HOYA         '.AND.
     3 ASORTIT2(1,I)(1:13).NE.'CHANCE       '.AND.
     4 ASORTIT2(1,I)(1:13).NE.'CORNIN       '.AND.
     5 ASORTIT2(1,I)(1:13).NE.'HIKARI       ') THEN
                       GO TO 41
                        ELSE
      WRITE(OUTLYNE,40) I,ASORTIT2(1,I),ASORTIT2(2,I),SORTIT2(I)
      CALL SHOWIT(0)
                       END IF
                       END DO
 41                    CONTINUE
 40   FORMAT(I4,1X,A13,1X,A13,1X,G23.15)
C
        DEALLOCATE(SORTIT,ASORTIT,STAT=ALLOERR)
        DEALLOCATE(SORTIT2,ASORTIT2,STAT=ALLOERR)
                        RETURN
                        END
      SUBROUTINE GLASSP_RUSSIAN1
      IMPLICIT NONE
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
      CHARACTER*13 RNAME(1:113)
      INTEGER I
      RNAME(1)=   'LK1'
      RNAME(2)=   'LK3'
      RNAME(3)=   'LK4'
      RNAME(4)=   'LK5'
      RNAME(5)=   'LK6'
      RNAME(6)=   'LK7'
      RNAME(7)=   'LK8'
      RNAME(8)=   'LK11'
      RNAME(9)=   'LK13'
      RNAME(10)=  'LK14'
      RNAME(11)=  'TFK1'
      RNAME(12)=  'K100'
      RNAME(13)=  'K2'
      RNAME(14)=  'K3'
      RNAME(15)=  'K8'
      RNAME(16)=  'K14'
      RNAME(17)=  'K15'
      RNAME(18)=  'K18'
      RNAME(19)=  'K19'
      RNAME(20)=  'K20'
      RNAME(21)=  'BK4'
      RNAME(22)=  'BK6'
      RNAME(23)=  'BK8'
      RNAME(24)=  'BK10'
      RNAME(25)=  'BK13'
      RNAME(26)=  'TK2'
      RNAME(27)=  'TK4'
      RNAME(28)=  'TK8'
      RNAME(29)=  'TK9'
      RNAME(30)=  'TK12'
      RNAME(31)=  'TK13'
      RNAME(32)=  'TK14'
      RNAME(33)=  'TK16'
      RNAME(34)=  'TK17'
      RNAME(35)=  'TK20'
      RNAME(36)=  'TK21'
      RNAME(37)=  'TK23'
      RNAME(38)=  'STK3'
      RNAME(39)=  'STK7'
      RNAME(40)=  'STK8'
      RNAME(41)=  'STK9'
      RNAME(42)=  'STK10'
      RNAME(43)=  'STK12'
      RNAME(44)=  'STK15'
      RNAME(45)=  'STK16'
      RNAME(46)=  'STK19'
      RNAME(47)=  'STK20'
      RNAME(48)=  'OK1'
      RNAME(49)=  'OK2'
      RNAME(50)=  'KF4'
      RNAME(51)=  'KF6'
      RNAME(52)=  'KF7'
      RNAME(53)=  'BF1'
      RNAME(54)=  'BF4'
      RNAME(55)=  'BF6'
      RNAME(56)=  'BF7'
      RNAME(57)=  'BF8'
      RNAME(58)=  'BF11'
      RNAME(59)=  'BF12'
      RNAME(60)=  'BF13'
      RNAME(61)=  'BF16'
      RNAME(62)=  'BF21'
      RNAME(63)=  'BF24'
      RNAME(64)=  'BF25'
      RNAME(65)=  'BF26'
      RNAME(66)=  'BF27'
      RNAME(67)=  'BF28'
      RNAME(68)=  'BF32'
      RNAME(69)=  'TBF3'
      RNAME(70)=  'TBF4'
      RNAME(71)=  'TBF8'
      RNAME(72)=  'TBF9'
      RNAME(73)=  'TBF25'
      RNAME(74)=  'TBF10'
      RNAME(75)=  'TBF11'
      RNAME(76)=  'LF5'
      RNAME(77)=  'LF7'
      RNAME(78)=  'LF8'
      RNAME(79)=  'LF9'
      RNAME(80)=  'LF10'
      RNAME(81)=  'LF11'
      RNAME(82)=  'LF12'
      RNAME(83)=  'F1'
      RNAME(84)=  'F2'
      RNAME(85)=  'F4'
      RNAME(86)=  'F6'
      RNAME(87)=  'F9'
      RNAME(88)=  'F13'
      RNAME(89)=  'F18'
      RNAME(90)=  'TF1'
      RNAME(91)=  'TF2'
      RNAME(92)=  'TF3'
      RNAME(93)=  'TF4'
      RNAME(94)=  'TF5'
      RNAME(95)=  'TF7'
      RNAME(96)=  'TF8'
      RNAME(97)=  'TF10'
      RNAME(98)=  'TF11'
      RNAME(99)=  'TF12'
      RNAME(100)= 'TF13'
      RNAME(101)= 'ST2'
      RNAME(102)= 'ST3'
      RNAME(103)= 'ST11'
      RNAME(104)= 'OF1'
      RNAME(105)= 'OF3'
      RNAME(106)= 'OF4'
      RNAME(107)= 'OF5'
      RNAME(108)= 'OF6'
      RNAME(109)= 'KY1'
      RNAME(110)= 'KY2'
      RNAME(111)= 'KB'
      RNAME(112)= 'KI'
      RNAME(113)= 'KB-P'
1     FORMAT('RUSSIAN       ',A13)
                        DO I=1,113



      WRITE(OUTLYNE,1) RNAME(I)(1:13)
      CALL SHOWIT(0)
                        END DO
                        RETURN
                        END
      SUBROUTINE GLASSP_RUSSIAN2
      IMPLICIT NONE
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
        CHARACTER*13 RNAME(1:113),NAME1,NAME2
        CHARACTER*12 FLNAME
      REAL*8 LAMUPP,LAMLOW
      INTEGER I
      RNAME(1)=   'LK1'
      RNAME(2)=   'LK3'
      RNAME(3)=   'LK4'
      RNAME(4)=   'LK5'
      RNAME(5)=   'LK6'
      RNAME(6)=   'LK7'
      RNAME(7)=   'LK8'
      RNAME(8)=   'LK11'
      RNAME(9)=   'LK13'
      RNAME(10)=  'LK14'
      RNAME(11)=  'TFK1'
      RNAME(12)=  'K100'
      RNAME(13)=  'K2'
      RNAME(14)=  'K3'
      RNAME(15)=  'K8'
      RNAME(16)=  'K14'
      RNAME(17)=  'K15'
      RNAME(18)=  'K18'
      RNAME(19)=  'K19'
      RNAME(20)=  'K20'
      RNAME(21)=  'BK4'
      RNAME(22)=  'BK6'
      RNAME(23)=  'BK8'
      RNAME(24)=  'BK10'
      RNAME(25)=  'BK13'
      RNAME(26)=  'TK2'
      RNAME(27)=  'TK4'
      RNAME(28)=  'TK8'
      RNAME(29)=  'TK9'
      RNAME(30)=  'TK12'
      RNAME(31)=  'TK13'
      RNAME(32)=  'TK14'
      RNAME(33)=  'TK16'
      RNAME(34)=  'TK17'
      RNAME(35)=  'TK20'
      RNAME(36)=  'TK21'
      RNAME(37)=  'TK23'
      RNAME(38)=  'STK3'
      RNAME(39)=  'STK7'
      RNAME(40)=  'STK8'
      RNAME(41)=  'STK9'
      RNAME(42)=  'STK10'
      RNAME(43)=  'STK12'
      RNAME(44)=  'STK15'
      RNAME(45)=  'STK16'
      RNAME(46)=  'STK19'
      RNAME(47)=  'STK20'
      RNAME(48)=  'OK1'
      RNAME(49)=  'OK2'
      RNAME(50)=  'KF4'
      RNAME(51)=  'KF6'
      RNAME(52)=  'KF7'
      RNAME(53)=  'BF1'
      RNAME(54)=  'BF4'
      RNAME(55)=  'BF6'
      RNAME(56)=  'BF7'
      RNAME(57)=  'BF8'
      RNAME(58)=  'BF11'
      RNAME(59)=  'BF12'
      RNAME(60)=  'BF13'
      RNAME(61)=  'BF16'
      RNAME(62)=  'BF21'
      RNAME(63)=  'BF24'
      RNAME(64)=  'BF25'
      RNAME(65)=  'BF26'
      RNAME(66)=  'BF27'
      RNAME(67)=  'BF28'
      RNAME(68)=  'BF32'
      RNAME(69)=  'TBF3'
      RNAME(70)=  'TBF4'
      RNAME(71)=  'TBF8'
      RNAME(72)=  'TBF9'
      RNAME(73)=  'TBF25'
      RNAME(74)=  'TBF10'
      RNAME(75)=  'TBF11'
      RNAME(76)=  'LF5'
      RNAME(77)=  'LF7'
      RNAME(78)=  'LF8'
      RNAME(79)=  'LF9'
      RNAME(80)=  'LF10'
      RNAME(81)=  'LF11'
      RNAME(82)=  'LF12'
      RNAME(83)=  'F1'
      RNAME(84)=  'F2'
      RNAME(85)=  'F4'
      RNAME(86)=  'F6'
      RNAME(87)=  'F9'
      RNAME(88)=  'F13'
      RNAME(89)=  'F18'
      RNAME(90)=  'TF1'
      RNAME(91)=  'TF2'
      RNAME(92)=  'TF3'
      RNAME(93)=  'TF4'
      RNAME(94)=  'TF5'
      RNAME(95)=  'TF7'
      RNAME(96)=  'TF8'
      RNAME(97)=  'TF10'
      RNAME(98)=  'TF11'
      RNAME(99)= 'TF12'
      RNAME(100)= 'TF13'
      RNAME(101)= 'ST2'
      RNAME(102)= 'ST3'
      RNAME(103)= 'ST11'
      RNAME(104)= 'OF1'
      RNAME(105)= 'OF3'
      RNAME(106)= 'OF4'
      RNAME(107)= 'OF5'
      RNAME(108)= 'OF6'
      RNAME(109)= 'KY1'
      RNAME(110)= 'KY2'
      RNAME(111)= 'KB'
      RNAME(112)= 'KI'
      RNAME(113)= 'KB-P'
1     FORMAT('RUSSIAN       ',A13)
10    FORMAT('Normal Cubic Spline Fit to SOVOPTIKS Data')
                        DO I=1,113
      IF(WS(1:13).EQ.RNAME(I)(1:13)) THEN
 991  FORMAT('WAVELENGTH RANGE:')
 992  FORMAT('LOWER WAVELENGTH = ',G15.8,' MICRONS')
 993  FORMAT('UPPER WAVELENGTH = ',G15.8,' MICRONS')
      NAME1='RUSSIAN      '
      NAME2=RNAME(I)(1:13)
      FLNAME=' '
      CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
      WRITE(OUTLYNE,991)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,992) LAMLOW
      CALL SHOWIT(0)
      WRITE(OUTLYNE,993) LAMUPP
      CALL SHOWIT(0)



      WRITE(OUTLYNE,1) RNAME(I)(1:13)
      CALL SHOWIT(0)



      WRITE(OUTLYNE,10)
      CALL SHOWIT(0)
                        GO TO 3
                        ELSE
                        END IF
                        END DO



        OUTLYNE='THE REQUESTED GLASS NAME WAS NOT FOUND'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
3                       CONTINUE
                        RETURN
                        END
C SUB GLASSP.FOR
        SUBROUTINE GLASSP
        use glass_manager
        use command_utils, only : parseCommandIntoTokens
C
        IMPLICIT NONE

        character(len=80) :: tokens(40)
        character(len=80) :: glassCand
        integer :: numTokens

               

C
C       THIS IS SUBROUTINE GLASSP WHICH IMPLEMENTS THE GLASSP
C       COMMAND AT THE CMD LEVEL.
C
        INTEGER J,TOTAL, i
C
      REAL*8 A0,A1,A2,A3,A4,A5,LAMUPP,LAMLOW
C
      CHARACTER NAME*13,NUMBER*13,FLNAME*12,NAME1*13,NAME2*13
     1,CATNAME*13
C
        INCLUDE 'DATLEN.INC'
        INCLUDE 'DATMAI.INC'
C
        LOGICAL EXIS_USER
C
C

 110  FORMAT(A13,1X,A13,1X,A13)

      IF(SN.EQ.1) THEN
        OUTLYNE='"GLASSP" TAKES NO NUMERIC WORD INPUT'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
      IF(SST.EQ.1.AND.SQ.EQ.0) THEN
        OUTLYNE='"THE GLASS CATALOG NAME IS MISSING'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
C     SYNTAX OK, PROCEED
C
        IF(STI.EQ.1) THEN
        OUTLYNE='"GLASSP" INTERROGATES GLASS CATALOGS'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        END IF
C
      IF(SST.EQ.0.AND.SQ.EQ.0) THEN
C     JUST GLASSP
            WRITE(OUTLYNE,100)
            CALL SHOWIT(0)
        do i=1,size(gdb%catalogDescriptions)
            OUTLYNE = trim(gdb%catalogDescriptions(i))
            CALL SHOWIT(0)
        end do

        WRITE(OUTLYNE,106)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,117)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,116)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,2161)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,107)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,2082)
      CALL SHOWIT(0)
        WRITE(OUTLYNE,2083)
      CALL SHOWIT(0)
 100  FORMAT(
     1'THE CURRENT PROGRAM GLASS CATALOG NAMES ARE:')

 106  FORMAT(
     110X,'"GLCAT     (Searches all catalogs)')
 117  FORMAT(
     110X,
     2'"GLASS     (Searches all catalogs)')
 116  FORMAT(
     110X,'            [specified without giving catalog name]')
 2161 FORMAT(
     110X,'              [but just by glass name or number]')
 107  FORMAT(
     110X,'"MATL"     (Special IR, UV, Visual Materials and Plastics)')
 2082 FORMAT(
     110X,'"USER"     (Use "EDIT USER.DAT" from the command line)')
 2083 FORMAT(
     110X,'"SCH2000"  (SCHOTT POST-2000 Optical Glass Catalog)')
                        RETURN
                        ELSE
C     QUALIFIER AND STRING NOT BLANK
                        END IF
      IF(SQ.EQ.1.AND.SST.EQ.0) THEN
C     OPEN THE APPROPRIATE CATALOG AND DISPLAY THE GLASS NAMES
C     AND NUMBERS
        IF(gdb%isNameInCatalog(trim(WQ)).EQV..FALSE.) THEN
            call LogTermFOR("Not in .bin files!")
            if(WQ.EQ.'MATL') then
                  call LogTermFOR("Matl entered!")
            end if
        END IF

        ! This is a crutch that shoudl be addressed with a more
        ! sophisticated glass database
        IF(WQ.EQ.'SPECIAL') THEN
            call PRINTSPECIALCATALOGMATERIALS()
            return
        END IF
        IF(WQ.EQ.'MATL') THEN
            CALL PRINTMATLGLASSES()
            RETURN
      END IF        

      

      ! Since some glasses are more than 8 characters parse the input 
      ! instead of using WQ
      !call parseCommandIntoTokens(trim(INPUT), tokens, numTokens, ' ') 
      !glassCand = trim(tokens(2))

      !call LogTermFOR("Can I use input? "//trim(glassCand))

C       IS THE SURFACE CATALOG IMPLEMENTED ?
        IF(gdb%isNameInCatalog(trim(WQ)).EQV..FALSE.
     2  .AND.WQ.NE.'GLAK'.AND.WQ.NE.'GLCAT'.AND.WQ.NE.'MATL') THEN
    !    IF(WQ.NE.'SCHOTT'.AND.WQ.NE.'CHANCE'.AND.WQ.NE.'GLAK'
    ! 1  .AND.WQ.NE.'OHARA'.AND.WQ.NE.'CORNIN'.AND.WQ.NE.'HIKARI'
    ! 2  .AND.WQ.NE.'HOYA'.AND.WQ.NE.'GLCAT'.AND.WQ.NE.'MATL'
    ! 3  .AND.WQ.NE.'RUSSIAN'.AND.WQ.NE.'RADHARD'.AND.WQ.NE.'USER'
    ! 4  .AND.WQ.NE.'SCH2000'.AND.WQ.NE.'SCH2023') THEN
C     NOT A VALID CATALOG
        WRITE(OUTLYNE,*) WQ
        CALL SHOWIT(1)
        OUTLYNE='THE REQUESTED GLASS CATALOG WAS NOT RECOGNIZED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        ELSE
        IF(WQ.EQ.'USER') THEN
        WRITE(OUTLYNE,2082)
        CALL SHOWIT(1)
                RETURN
                END IF
      IF(WQ.NE.'GLCAT'.AND.WQ.NE.'GLAK') THEN
C     VALID SINGLE CATALOG, PROCEED LISTING GLASSES
C     DETERMINE FILE NAME
      IF(WQ.NE.'MATL'.AND.WQ.NE.'RUSSIAN') THEN
        IF(WQ.EQ.'SCHOTT')   FLNAME='SCHOTT.BIN  '
        IF(WQ.EQ.'SCH2000')  FLNAME='SCH2000.BIN '
        IF(WQ.EQ.'HOYA')     FLNAME='HOYA.BIN    '
        IF(WQ.EQ.'HIKARI')   FLNAME='HIKARI.BIN  '
        IF(WQ.EQ.'OHARA')    FLNAME='OHARAMULTI  '
        IF(WQ.EQ.'CORNIN')   FLNAME='CORNIN.BIN  '
        IF(WQ.EQ.'CHANCE')   FLNAME='CHANCE.BIN  '
        IF(WQ.EQ.'RADHARD')  FLNAME='RADHARD.BIN  '
        IF(WQ.EQ.'SCH2023')  FLNAME='SCH2023.BIN '
        IF(FLNAME.NE.'OHARAMULTI  ') THEN
          CALL READGLASSFILE(FLNAME, 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
                           ELSE
          CALL READGLASSFILE('OHARA.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
          CALL READGLASSFILE('OHARA-O.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)

                           END IF
                        RETURN
                      ELSE
C     WQ WAS MATL OR RUSSIAN


      !IF(WQ.EQ.'RUSSIAN') CALL GLASSP_RUSSIAN1
                      END IF
                        ELSE
                        END IF
      IF(WQ.EQ.'GLCAT'.OR.WQ.EQ.'GLAK') THEN
C     GLCAT MEANS DO ALL CATALOGS
          CALL READGLASSFILE('SCHOTT.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
          CALL READGLASSFILE('OHARA.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
            CALL READGLASSFILE('OHARA-O.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
            CALL READGLASSFILE('SCH2000.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
            CALL READGLASSFILE('HOYA.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
            CALL READGLASSFILE('HIKARI.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
            CALL READGLASSFILE('CORNIN.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
            CALL READGLASSFILE('CHANCE.BIN', 1, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)

                        RETURN
                        END IF
                        END IF
                        ELSE
C     NOT A REQUEST FOR GLASS NAMES IN A CATALOG
                        END IF
C
      IF(SQ.EQ.1.AND.SST.EQ.1) THEN
C     OPEN THE APPROPRIATE CATALOG AND DISPLAY DATA FOR THE
C     SPECIFIED GLASS
C       IS THE SURFACE CATALOG IMPLEMENTED ?
        IF(WQ.NE.'SCHOTT'.AND.WQ.NE.'CHANCE'.AND.WQ.NE.'GLAK'
     1  .AND.WQ.NE.'OHARA'.AND.WQ.NE.'CORNIN'.AND.WQ.NE.'HIKARI'
     2  .AND.WQ.NE.'HOYA'.AND.WQ.NE.'GLCAT'.AND.WQ.NE.'MATL'
     3  .AND.WQ.NE.'RUSSIAN'.AND.WQ.NE.'RADHARD'.AND.WQ.NE.'SCH2000')
     4   THEN
C     NOT A VALID CATALOG
        OUTLYNE='2THE REQUESTED GLASS CATALOG WAS NOT RECOGNIZED'
      CALL SHOWIT(1)
        OUTLYNE='RE-ENTER COMMAND'
      CALL SHOWIT(1)
                        CALL MACFAL
                        RETURN
                        ELSE
C     VALID CATALOG, PROCEED LISTING GLASSES
C     DETERMINE FILE NAME
                IF(WQ.NE.'GLCAT'.AND.WQ.NE.'GLAK') THEN
      IF(WQ.NE.'MATL'.AND.WQ.NE.'RUSSIAN') THEN
        IF(WQ.EQ.'SCHOTT') FLNAME='SCHOTT.BIN  '
        IF(WQ.EQ.'SCH2000')FLNAME='SCH2000.BIN '
        IF(WQ.EQ.'HOYA')   FLNAME='HOYA.BIN    '
        IF(WQ.EQ.'HIKARI') FLNAME='HIKARI.BIN  '
        IF(WQ.EQ.'OHARA')  FLNAME='OHARAMULTI  '
        IF(WQ.EQ.'CORNIN') FLNAME='CORNIN.BIN  '
        IF(WQ.EQ.'CHANCE') FLNAME='CHANCE.BIN  '
        IF(WQ.EQ.'RADHARD') FLNAME='RADHARD.BIN  '
      IF(FLNAME.NE.'OHARAMULTI  ') THEN

          CALL READGLASSFILE(FLNAME, 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)

                       END IF
C     OHARAMULTI
      IF(FLNAME.EQ.'OHARAMULTI  ') THEN
          CALL READGLASSFILE('OHARA.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
          CALL READGLASSFILE('OHARA-O.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
      END IF



      IF(WQ.EQ.'RUSSIAN') CALL GLASSP_RUSSIAN2
C
                      END IF
                        ELSE
C     GLCAT/GLASS SEARCH
          CALL READGLASSFILE('SCHOTT.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('SCH2000.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('HOYA.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('HIKARI.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('OHARA.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('OHARA-O.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('CHANCE.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)
           CALL READGLASSFILE('CORNIN.BIN', 2, NAME, NUMBER, A0,
     1                      A1,A2,A3,A4,A5)


                        END IF
                      END IF
                    END IF

      END


      SUBROUTINE  READGLASSFILE(FLNAME, FLAG, NAME,NUMBER,A0,A1,A2,
     1  A3,A4,A5)


        integer :: J, TOTAL, FLAG, uG

        REAL*8 A0,A1,A2,A3,A4,A5
        CHARACTER(len=13) NAME,NUMBER, CATNAME
        CHARACTER(len=*) :: FLNAME
        !CHARACTER(LEN=140) :: OUTLYNE

        !COMMON/REPLWRIT/OUTLYNE

        include "DATMAI.INC"

        call OPENGLASSFILE(trim(LIBGLA)//FLNAME, uG, TOTAL)


 109  FORMAT(A25)
 119  FORMAT(A33)
 111  FORMAT('GLASS NAME       GLASS NUMBER')

        DO J=2,TOTAL+1
      READ(UNIT=uG,REC=J)NAME,NUMBER,A0,A1,A2,A3,A4,A5
        INCLUDE 'CATNAMER.INC'

      SELECT CASE (FLAG)

       CASE (1)
        !PRINT *, "FLNAME is ", FLNAME
        !PRINT *, "CATNAME is ", CATNAME
        WRITE(OUTLYNE,110) CATNAME,NAME,NUMBER
        CALL SHOWIT(0)
       CASE (2)
        IF(WS(1:13).EQ.NAME.OR.WS(1:13).EQ.NUMBER) THEN

          CALL SHOWGLASSDATA(FLNAME, NAME, A0, A1, A2,
     1      A3, A4, A5)
          CLOSE(UNIT=uG)
          RETURN
        END IF
C
      END SELECT
 110  FORMAT(A13,1X,A13,1X,A13)
C      PRINT *, "DONE WITH RECORD ",J
                        END DO
      CLOSE(UNIT=36)


      END SUBROUTINE

      SUBROUTINE SHOWGLASSDATA(FLNAME, NAME, A0, A1, A2,
     1                         A3, A4, A5)


      CHARACTER(len=13) :: NAME,NUMBER, CATNAME, NAME1, NAME2
      CHARACTER(len=*) :: FLNAME
      REAL*8 A0,A1,A2,A3,A4,A5,LAMUPP,LAMLOW
      include "DATMAI.INC"
C
 991  FORMAT('WAVELENGTH RANGE:')
 992  FORMAT('LOWER WAVELENGTH = ',G15.8,' MICRONS')
 993  FORMAT('UPPER WAVELENGTH = ',G15.8,' MICRONS')


      NAME1=WQ//'     '
      NAME2=NAME
      CALL LAMHILO(NAME1,NAME2,FLNAME,LAMUPP,LAMLOW)
      WRITE(OUTLYNE,991)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,992) LAMLOW
      CALL SHOWIT(0)
      WRITE(OUTLYNE,993) LAMUPP
      CALL SHOWIT(0)

      WRITE(OUTLYNE,120) NAME
      CALL SHOWIT(0)
 120  FORMAT('GLASS NAME   = ',A13)


      !WRITE(OUTLYNE,121) NUMBER
      CALL SHOWIT(0)
 121  FORMAT('GLASS NUMBER = ',A13)
 122  FORMAT('          A0 = ',G23.15)
 123  FORMAT('          A1 = ',G23.15)
 124  FORMAT('          A2 = ',G23.15)
 125  FORMAT('          A3 = ',G23.15)
 126  FORMAT('          A4 = ',G23.15)
 127  FORMAT('          A5 = ',G23.15)
 1122 FORMAT('          B1 = ',G23.15)
 1123 FORMAT('          B2 = ',G23.15)
 1124 FORMAT('          B3 = ',G23.15)
 1125 FORMAT('          C1 = ',G23.15)
 1126 FORMAT('          C2 = ',G23.15)
 1127 FORMAT('          C3 = ',G23.15)
 22   FORMAT('SCHOTT TYPE INTERPOLATION EQUATION')
 2222 FORMAT('SELLMEIER INTERPOLATION EQUATION')
 2223 FORMAT('HERZBERGER INTERPOLATION EQUATION')
      IF(FLNAME.NE.'SCHOTT.BIN  '.AND.FLNAME.NE.'SCH2000.BIN ') THEN

      WRITE(OUTLYNE,22)
      CALL SHOWIT(0)

      WRITE(OUTLYNE,122) A0
      CALL SHOWIT(0)

      WRITE(OUTLYNE,123) A1
      CALL SHOWIT(0)

      WRITE(OUTLYNE,124) A2
      CALL SHOWIT(0)

      WRITE(OUTLYNE,125) A3
      CALL SHOWIT(0)

      WRITE(OUTLYNE,126) A4
      CALL SHOWIT(0)

      WRITE(OUTLYNE,127) A5
      CALL SHOWIT(0)
                        ELSE

      WRITE(OUTLYNE,2222)
      CALL SHOWIT(0)

      WRITE(OUTLYNE,1122) A0
      CALL SHOWIT(0)

      WRITE(OUTLYNE,1123) A1
      CALL SHOWIT(0)

      WRITE(OUTLYNE,1124) A2
      CALL SHOWIT(0)

      WRITE(OUTLYNE,1125) A3
      CALL SHOWIT(0)

      WRITE(OUTLYNE,1126) A4
      CALL SHOWIT(0)

      WRITE(OUTLYNE,1127) A5
      CALL SHOWIT(0)
                        END IF

      END SUBROUTINE


      SUBROUTINE PRINTSPECIALCATALOGMATERIALS
            use handlers, only:  updateTerminalLog
            use glass_manager

            integer :: i
            INCLUDE 'DATLEN.INC'
            INCLUDE 'DATMAI.INC'  

            do i=1,size(specCat%names)
                call updateTerminalLog('SPECIAL  '
     1     //specCat%names(i), "black")

            end do



      END SUBROUTINE

      SUBROUTINE PRINTMATLGLASSES

            INCLUDE 'DATLEN.INC'
            INCLUDE 'DATMAI.INC'            

 3000 FORMAT('MATL          ACRYLIC      ',
     1'-   Acrylic (Lucite)')
      WRITE(OUTLYNE,3000)
      CALL SHOWIT(0)
 3010 FORMAT('MATL          PLYSTY       ',
     1'-   Polystyrene')
      WRITE(OUTLYNE,3010)
      CALL SHOWIT(0)
 3020 FORMAT('MATL          POLYCARB     ',
     1'-   Polycarbonate')
      WRITE(OUTLYNE,3020)
      CALL SHOWIT(0)
 3030 FORMAT('MATL          SAN          ',
     1'-   Copolymer Styrene-Acrylonitrile (SAN)')
      WRITE(OUTLYNE,3030)
      CALL SHOWIT(0)

 200  FORMAT('MATL          GERMSC       ',
     1'-   Single Crystal Germanium')
      WRITE(OUTLYNE,200)
      CALL SHOWIT(0)
 201  FORMAT('MATL          GERMPC       ',
     1'-   Poly-Crystalline Germanium')
      WRITE(OUTLYNE,201)
      CALL SHOWIT(0)
 202  FORMAT('MATL          SILICON      ',
     1'-   Silicon')
      WRITE(OUTLYNE,202)
      CALL SHOWIT(0)
 203  FORMAT('MATL          IRG100       ',
     1'-   Schott IRG100 Infrared Glass')
      WRITE(OUTLYNE,203)
      CALL SHOWIT(0)
 204  FORMAT('MATL          ZNSE         ',
     1'-   CVD Zinc Selenide (IRTRAN4)')
      WRITE(OUTLYNE,204)
      CALL SHOWIT(0)
 205  FORMAT('MATL          ZNS          ',
     1'-   CVD Zinc Sulfide (IRTRAN2)')
      WRITE(OUTLYNE,205)
      CALL SHOWIT(0)
1215  FORMAT('MATL          IRTRAN1      ',
     1'-   IRTRAN1 (Magnesium Fluoride (o) ray')
      WRITE(OUTLYNE,1215)
      CALL SHOWIT(0)
1205  FORMAT('MATL          IRTRAN2      ',
     1'-   IRTRAN2 (CVD Zinc Sulfide)')
      WRITE(OUTLYNE,1205)
      CALL SHOWIT(0)
1217  FORMAT('MATL          IRTRAN3      ',
     1'-   IRTRAN3 (Calcium Fluoride)')
      WRITE(OUTLYNE,1217)
      CALL SHOWIT(0)
1204  FORMAT('MATL          IRTRAN4      ',
     1'-   IRTRAN4 (CVD Zinc Selenide)')
      WRITE(OUTLYNE,1204)
      CALL SHOWIT(0)
1218  FORMAT('MATL          IRTRAN5      ',
     1'-   IRTRAN5 (Magnesium Oxide)')
      WRITE(OUTLYNE,1218)
      CALL SHOWIT(0)
1214  FORMAT('MATL          IRTRAN6      ',
     1'-   IRTRAN6 (Cadmium Telluride)')
      WRITE(OUTLYNE,1214)
      CALL SHOWIT(0)
 206  FORMAT('MATL          CLRTRAN      ',
     1'-   CVD CLEARTRAN')
      WRITE(OUTLYNE,206)
      CALL SHOWIT(0)
 207  FORMAT('MATL          SILICA       ',
     1'-   Fused Silica (SiO2)')
      WRITE(OUTLYNE,1207)
      CALL SHOWIT(0)
 2071 FORMAT('MATL          SIO2         ',
     1'-   Fused Silica (SiO2) (SILICA equiv.)')
      WRITE(OUTLYNE,1207)
      CALL SHOWIT(0)
 1207 FORMAT('MATL          SUPRASIL     ',
     1'-   Fused Silica (SiO2)')
      WRITE(OUTLYNE,2207)
      CALL SHOWIT(0)
 2207 FORMAT('MATL          HOMOSIL      ',
     1'-   Fused Silica (SiO2)')
      WRITE(OUTLYNE,207)
      CALL SHOWIT(0)
      WRITE(OUTLYNE,2071)
      CALL SHOWIT(0)
 208  FORMAT('MATL          SAPPHIRE     ',
     1'-  Sapphire (Al2O3) (o)rdinary ray')
      WRITE(OUTLYNE,208)
      CALL SHOWIT(0)
 209  FORMAT('MATL          DYNASIL      ',
     1'-   Synthetic Fused Silica')
      WRITE(OUTLYNE,209)
      CALL SHOWIT(0)
 210  FORMAT('MATL          AMTIR1       ',
     1'-   AMTIR-1')
      WRITE(OUTLYNE,210)
      CALL SHOWIT(0)
 211  FORMAT('MATL          AMTIR3       ',
     1'-   AMTIR-3 (TI 1173 Equivalent)')
      WRITE(OUTLYNE,211)
      CALL SHOWIT(0)
 212  FORMAT('MATL          AS2S3        ',
     1'-   Arsenic Trisulfide IR Glass')
      WRITE(OUTLYNE,212)
      CALL SHOWIT(0)
 213  FORMAT('MATL          GAAS         ',
     1'-   Gallium Arsenide')
      WRITE(OUTLYNE,213)
      CALL SHOWIT(0)
 214  FORMAT('MATL          CDTE         ',
     1'-   Cadmium Telluride (IRTRAN6)')
      WRITE(OUTLYNE,214)
      CALL SHOWIT(0)
 215  FORMAT('MATL          MGF2(O)      ',
     1'-   Magnesium Fluoride (IRTRAN1) (o) ray')
      WRITE(OUTLYNE,215)
      CALL SHOWIT(0)
2215  FORMAT('MATL          MGF2         ',
     1'-   Magnesium Fluoride (IRTRAN1) (o) ray')
      WRITE(OUTLYNE,2215)
      CALL SHOWIT(0)
 216  FORMAT('MATL          MGF2(E)      ',
     1'-   Magnesium Fluoride (IRTRAN1) (e) ray')
      WRITE(OUTLYNE,216)
      CALL SHOWIT(0)
 217  FORMAT('MATL          CAF2         ',
     1'-   Calcium Fluoride (IRTRAN3)')
      WRITE(OUTLYNE,217)
      CALL SHOWIT(0)
 218  FORMAT('MATL          MGO          ',
     1'-   Magnesium Oxide (IRTRAN5)')
      WRITE(OUTLYNE,218)
      CALL SHOWIT(0)
 219  FORMAT('MATL          BAF2         ',
     1'-   Barium Fluoride')
      WRITE(OUTLYNE,219)
      CALL SHOWIT(0)
 220  FORMAT('MATL          KBR          ',
     1'-   Potassium Bromide')
      WRITE(OUTLYNE,220)
      CALL SHOWIT(0)
 221  FORMAT('MATL          CSI          ',
     1'-   Cesium Iodide')
      WRITE(OUTLYNE,221)
      CALL SHOWIT(0)
 222  FORMAT('MATL         CSBR          ',
     1'-   Cesium Bromide')
      WRITE(OUTLYNE,222)
      CALL SHOWIT(0)
 223  FORMAT('MATL         KRS5          ',
     1'-   Thallium Bromoiodide (KRS-5)')
      WRITE(OUTLYNE,223)
      CALL SHOWIT(0)
 224  FORMAT('MATL         SIO2O         ',
     1'-   Crystalline Quartz (SiO2) (o) ray')
      WRITE(OUTLYNE,224)
      CALL SHOWIT(0)
 225  FORMAT('MATL         SIO2E         ',
     1'-   Crystalline Quartz (SiO2) (e) ray')
      WRITE(OUTLYNE,225)
      CALL SHOWIT(0)
 226  FORMAT('MATL         NACL          ',
     1'-   Sodium Chloride')
      WRITE(OUTLYNE,226)
      CALL SHOWIT(0)
 227  FORMAT('MATL         LIF           ',
     1'-   Lithium Fluoride')
      WRITE(OUTLYNE,227)
      CALL SHOWIT(0)
 228  FORMAT('MATL         VIR3          ',
     1'-   Corning VIR 3 Infrared Glass')
      WRITE(OUTLYNE,228)
      CALL SHOWIT(0)
 229  FORMAT('MATL         9754          ',
     1'-   Corning 9754 Infrared Glass')
      WRITE(OUTLYNE,229)
      CALL SHOWIT(0)
 230  FORMAT('MATL         ALON          ',
     1'-   Raytheon Aluminum Oxynitride')
      WRITE(OUTLYNE,230)
      CALL SHOWIT(0)
 231  FORMAT('MATL         SPINEL        ',
     1'-   Magnesium Aluminate Spinel (MgAl2O4)')
      WRITE(OUTLYNE,231)
      CALL SHOWIT(0)
 232  FORMAT('MATL         CALAL         ',
     1'-   Calcium Aluminate Glass')
      WRITE(OUTLYNE,232)
      CALL SHOWIT(0)
 233  FORMAT('MATL         B270          ',
     1'-   Schott B270 Glass')
      WRITE(OUTLYNE,233)
      CALL SHOWIT(0)
 234  FORMAT('MATL         IRG2          ',
     1'-   Schott IRG2 Glass')
      WRITE(OUTLYNE,234)
      CALL SHOWIT(0)
 235  FORMAT('MATL         IRG3          ',
     1'-   Schott IRG3 Glass')
      WRITE(OUTLYNE,235)
      CALL SHOWIT(0)
 236  FORMAT('MATL         IRGN6         ',
     1'-   Schott IRGN6 Glass')
      WRITE(OUTLYNE,236)
      CALL SHOWIT(0)
 237  FORMAT('MATL         IRG7          ',
     1'-   Schott IRG7 Glass')
      WRITE(OUTLYNE,237)
      CALL SHOWIT(0)
 238  FORMAT('MATL         IRG9          ',
     1'-   Schott IRG9 Glass')
      WRITE(OUTLYNE,238)
      CALL SHOWIT(0)
 239  FORMAT('MATL         IRG11         ',
     1'-   Schott IRG11 Glass')
      WRITE(OUTLYNE,239)
      CALL SHOWIT(0)
 240  FORMAT('MATL         IRG15         ',
     1'-   Schott IRG15 Glass')
      WRITE(OUTLYNE,240)
      CALL SHOWIT(0)
 241  FORMAT('MATL         WATER          ',
     1'-   Distilled Water')
      WRITE(OUTLYNE,241)
      CALL SHOWIT(0)
 242  FORMAT('MATL         VAC           ',
     1'-   Vacuum')
      WRITE(OUTLYNE,242)
      CALL SHOWIT(0)
 243  FORMAT('MATL         ZNS-MS        ',
     1'-   II-VI Multi-Spectral ZNS')
      WRITE(OUTLYNE,243)
      CALL SHOWIT(0)
 244  FORMAT('MATL         CEF3          ',
     1'-   Cerium Flouride for coatings')
      WRITE(OUTLYNE,244)
      CALL SHOWIT(0)
 245  FORMAT('MATL         LA2O3         ',
     1'-   Lanthanum Oxide for coatings')
      WRITE(OUTLYNE,245)
      CALL SHOWIT(0)
 246  FORMAT('MATL         THF4          ',
     1'-   Thorium Flouride for coatings')
      WRITE(OUTLYNE,246)
      CALL SHOWIT(0)
 247  FORMAT('MATL         ZRO2          ',
     1'-   Zirconium Oxide for coatings')
      WRITE(OUTLYNE,247)
      CALL SHOWIT(0)
 248  FORMAT('MATL         DIAMOND       ',
     1'-   Herzberger formula')
      WRITE(OUTLYNE,248)
      CALL SHOWIT(0)
 249  FORMAT('MATL         YAG - Yittrium Aluminum Garnet')
      WRITE(OUTLYNE,249)
      CALL SHOWIT(0)
            
      END SUBROUTINE
