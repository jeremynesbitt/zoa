        PROGRAM MAIN
C
        IMPLICIT NONE
C
        CHARACTER NAME(1:5000)*13,NUMBER(1:5000)*13,
     1  DUMMY*20,N*20
     
        CHARACTER FILENAME*32
C
        INTEGER I,J,JJ,rec11,rec15
        
        CHARACTER NAMELOG*13, NUMBERLOG*13
        
C
C
        DOUBLE PRECISION A0(1:5000),A1(1:5000),
     1  A2(1:5000),A3(1:5000),A4(1:5000),A5(1:5000)
     
        DOUBLE PRECISION A0LOG, A1LOG, A2LOG, A3LOG, A4LOG, A5LOG
C
        OPEN(UNIT=10,ACCESS='SEQUENTIAL',FILE='SCHOTT.DAT',
     1  FORM='FORMATTED',STATUS='UNKNOWN')
        REWIND (UNIT=10)
                J=1
 50             READ(10,105) NAME(J)
                PRINT*,NAME(J),J
C
        IF(NAME(J)(1:3).EQ.'EOS') THEN
                JJ=J-1
                GO TO 5000
                ELSE
                END IF
                READ(10,105) NUMBER(J)
C     READ THE COEFFICIENTS
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A0(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A0(J)
 120    FORMAT(D20.10)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A1(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A1(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A2(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A2(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A3(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A3(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A4(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A4(J)
                END IF
                READ(10,100) N
        IF(N(1:1).EQ.'N') THEN
                A5(J)=0.0D0
                ELSE
        BACKSPACE(10)
        READ(10,120) A5(J)
                END IF
C                DO I=9,136
                READ(10,100) DUMMY
C                END DO
                J=J+1
                GO TO 50
 5000           CONTINUE
        PRINT*,'READ COMPLETE'
        CLOSE(UNIT=10,STATUS='KEEP')
        
        FILENAME = 'SCHOTT.BIN'
        CALL WRITEKDPGLASSLIB(FILENAME, JJ, NAME, NUMBER, A0,
     1  A1, A2, A3,A4,A5)
     
        PRINT *, "IT WORKED!"   
        
C        inquire(iolength=rec11) JJ
C        inquire(iolength=rec15) NAME(1),NUMBER(1),A0(1),A1(1),
C     1  A2(1),A3(1),A4(1),A5(1)      
        
C        OPEN(UNIT=30,ACCESS='DIRECT',FILE='SCHOTT.BIN',
C     1  FORM='UNFORMATTED',RECL=rec11,STATUS='UNKNOWN')
C        WRITE(UNIT=30,REC=1) JJ
C        CLOSE(UNIT=30, STATUS='KEEP')
C        print*,jj
C        OPEN(UNIT=30,ACCESS='DIRECT',FILE='SCHOTT.BIN',
C     1  FORM='UNFORMATTED',RECL=rec15,STATUS='UNKNOWN')        
C                DO 6000 I=1,JJ
C        PRINT*,NAME(I),NUMBER(I),A0(I),A1(I),
C     1  A2(I),A3(I),A4(I),A5(I)
C        WRITE(UNIT=30,REC=I+1) NAME(I),NUMBER(I),A0(I),A1(I),
C     1  A2(I),A3(I),A4(I),A5(I)
C 6000           CONTINUE
C        PRINT*,'WRITE COMPLETE'
C                CLOSE(UNIT=30)
 100    FORMAT(A20)
 105    FORMAT(A13)
 
C   Practice reading file
  
C       OPEN(UNIT=36,ACCESS='DIRECT',FILE='SCHOTT.BIN',
C     1  FORM='UNFORMATTED',RECL=rec11,STATUS='UNKNOWN')
C        READ(UNIT=36,REC=1) JJ
C        CLOSE(UNIT=36,STATUS='KEEP')
C        PRINT *, JJ
C        PRINT *, "LDM13 okay SCHOTT.BIN ", TOTAL       
C        OPEN(UNIT=36,ACCESS='DIRECT',FILE='SCHOTT.BIN',
C     1  FORM='UNFORMATTED',RECL=rec15,STATUS='UNKNOWN') 
C                        DO 7000 I=1,JJ
C        READ(UNIT=36,REC=I+1)NAMELOG,NUMBERLOG,A0LOG,A1LOG,
C     1  A2LOG,A3LOG,A4LOG,A5LOG
C        PRINT*,NAMELOG,NUMBERLOG,A0LOG,A1LOG,
C     1  A2LOG,A3LOG,A4LOG,A5LOG
C 7000         CONTINUE       
C        CLOSE(UNIT=36)     
                STOP
                END
