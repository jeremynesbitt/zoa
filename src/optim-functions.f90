module optim_functions


contains

subroutine aut_go()
    use kdp_utils, only: OUTKDP



    ! Default merit function is spot diagram for now
    call PROCESKDP('MERIT; RMS,,,1;EOS')

    ! Temp - print out statement
    call outKDP("Iteration 0")
    call PROCESKDP("FMT")    
    call PROCESKDP("SUR SA")



    !call PROCESKDP('OPSPOT RECT; OPRECT, 20')
    call PROCESKDP('ITER; PFIND;ITER')

    ! Temp - print out statement
    call outKDP("Iteration 1")
    call PROCESKDP("FMT")    
    call PROCESKDP("SUR SA")

end subroutine

!Before I spend a bunch of time coding, I think I should just
!Spreadsheet out what KDP is doing.  Information needed
!For each iteration
!Merit function (RMS)
!dT used
!dRMS/dT (derivative)
!Computed new position
!Since it oscillates, I really only need to record this twice


! In derivates 
!C     NEW VALUE IS:
!      V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
! VARABL(INT(W1),8) =DINCR

!DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/
!1(DINMUL*VARABL(I,8))

! What is OPERND(1,14) ?  it is about 2 for my test case

!Agree with calc - for RMS and thickness result is unstable unless I increase dT by 100 
!Scratch that - repro default SPO settings and then it works
!Scratch that - I can make it stable, but not sure how


! subroutine damped_least_squares_optimization(numIters)
!     logical :: boolResult

!     do i=1,numIters
!     call computeDerivative(operands)
!     call computeDampingFactor(operands)
!     call computeNewOperands(operands)
!     boolResult = checkEndPoints(operands)
!     if (boolResult) return
!     end do






! end subroutine

! SUB DERIVATIVES.FOR
!  SUBROUTINE DERIVATIVES(MAXCNT,DERIV)
!     !
!           IMPLICIT NONE
!           INTEGER ALLOERR,IIID,JJJD,I,J,VCFG,VTYPE,ALTYPE,VADD,II,JJ
!           INTEGER VIP,MAXCNT,IFUNCTION,ICHK
!           CHARACTER AV1*23,OLDAV1*23,AN1*23
!           REAL*8 N1,V1,OLDV1,CV1,DERVAL,OREG
!           REAL*8 DERIV
!           DIMENSION DERIV(1:MAXCNT,1:MAXCNT)
!           INTEGER ISURF,DNUM
!           REAL*8 NEWDEFVAL &
!           ,DMEAN,DSTDEV,DMEAN2
!           COMMON/DEFVALCOM/NEWDEFVAL
!           LOGICAL GETTER,PLL,SILENT,ERR1,ERR2
!           COMMON/PLLPLL/PLL
!           COMMON/RETTEG/GETTER,DERVAL
!           COMMON/CAUX1/N1,AN1

!             INCLUDE 'DATSUB.INC'
!             INCLUDE 'DATMAI.INC'
!             INCLUDE 'DATLEN.INC'
!             INCLUDE 'DATCFG.INC'
!             INCLUDE 'DATMAC.INC'
!     !
!     !       THIS IS SUBROUTINE DERIVATIVES. IT COMPUTES THE DAMPED LEAST SQUARES
!     !       DERIVATIVE MATRIX
!     !
!     !
!     !     IF OPCNT=0 OR VBCNT=0, THEN THERE CAN BE NO ITER. DO THIS FIRST.
!     !
!           IF(OPCNT.EQ.0.OR.VBCNT.EQ.0) THEN
!             WRITE(OUTLYNE,*) &
!             '"ITER", "IT", "SV" AND "RSV" REQUIRE'
!           CALL SHOWIT(1)
!             WRITE(OUTLYNE,*) &
!             'VARIABLES AND OPERANDS TO EXIST'
!           CALL SHOWIT(1)
!             WRITE(OUTLYNE,*) &
!             'BEFORE THEY CAN FUNCTION'
!           CALL SHOWIT(1)
!             WRITE(OUTLYNE,*)'NO ACTION TAKEN'
!           CALL SHOWIT(1)
!                             CALL MACFAL
!                             RETURN
!                             END IF
    
!     !     THE INTEGER VARIABLE DERSIZ IS NOW SET TO THE LARGER
!     !     OF THE VALUES OPCNT AND VBCNT
    
!           IF(VBCNT.GT.OPCNT) DERSIZ=VBCNT
!           IF(VBCNT.LE.OPCNT) DERSIZ=OPCNT
!     !
!                            PLL=.FALSE.
!     !
!     !     CALCULATE THE VALUES OF ALL OPERANDS AND LOAD THEM INTO THE
!     !     OPERND ARRAY
!     !
!                            OPCALC_TYPE=3
!                             CALL OPCALC
!           !JN:  After this, print out current operand value!
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     REMEMBER THE CURRENT, UN-PERTURBED FMT
!                             FMTFMT=0.0D0
!                             FMTFLG=.TRUE.
!                             FMTEXT=.TRUE.
!                             DO I=1,OPCNT
!            IF(OPERND(I,19).EQ.0.0D0) FMTFMT=FMTFMT+(OPERND(I,14)**2)
!                             END DO
!                          OLDOP(1:OPCNT,1:20)=OPERND(1:OPCNT,1:20)
!           IF(DABS(FMTFMT).LE.1.0D-50) THEN
!             WRITE(OUTLYNE,*) &
!             '"FMT=0" AND NO IMPROVEMENT IS POSSIBLE'
!           CALL SHOWIT(1)
!             WRITE(OUTLYNE,*)'NO ACTION TAKEN'
!           CALL SHOWIT(1)
!                             RETURN
!                             END IF
!     !
!     !     NOW FOR EACH VARIABLE IN THE VARIABLES LIST, MAKE THE APPROPRIATE
!     !     CHANGE TO THE LENS, CALCULATE THE OPERANDS, LOAD THE OPERND ARRAY,
!     !     FORM THE PARTIAL DERIVATIVES AND LOAD THE APPROPRIATE ROW ENTRIES
!     !     OF THE COLUMN IN DERIV FOR THAT VARIABLE. PRINT A MESSAGE TO THE
!     !     EFFECT THAT THE DERIVATIVE MATRIX IS BEING GENERATED.
!     !     EACH ROW BELONGS TO AN OPERAND
!     !     EACH COLUMN BELONGS TO AN VARIABLE
!     !
!                             DO I=1,VBCNT
!     !     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
!     !     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
!           IF(VARABL(I,2).EQ.1.0D0) THEN
!     !     NON-CONFIGS VARIABLE MEANING CONFIG 1
!     !     THIS IS A LENS LEVEL VARIABLE CHANGE AND DERIVATIVE STUFF
!     !     GET THE DATA TYPE NUMBER OF THE VARIABLE
!           VTYPE=INT(VARABL(I,1))
!           IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
!     !     SURFACE CURVATURE
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!                           ALENS(1,INT(VARABL(I,3)))=V1
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                            OPCALC_TYPE=3
!                             CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!           ALENS(1,INT(VARABL(I,3)))=VARABL(I,4)
!     !                       CURVATURE DONE
!                             END IF
!           IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
!     !     SURFACE TORIC CURVATURE
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!                           ALENS(24,INT(VARABL(I,3)))=V1
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                            OPCALC_TYPE=3
!                             CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!           ALENS(24,INT(VARABL(I,3)))=VARABL(I,4)
!     !                       TORIC CURVATURE DONE
!                             END IF
!           IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
!           IF(VTYPE.EQ.3) ALTYPE=3
!           IF(VTYPE.EQ.4) ALTYPE=2
!           IF(VTYPE.EQ.5) ALTYPE=4
!           IF(VTYPE.EQ.6) ALTYPE=5
!           IF(VTYPE.EQ.7) ALTYPE=6
!           IF(VTYPE.EQ.8) ALTYPE=7
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!           ALENS(ALTYPE,INT(VARABL(I,3)))=V1
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                            OPCALC_TYPE=3
!                             CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!           ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
!     !                       THESE VARIABLES DONE
!                             END IF
!           IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE. &
!           124.AND.VTYPE.LE.149) THEN
!           IF(VTYPE.EQ.11) ALTYPE=41
!           IF(VTYPE.EQ.12) ALTYPE=37
!           IF(VTYPE.EQ.13) ALTYPE=38
!           IF(VTYPE.EQ.14) ALTYPE=39
!           IF(VTYPE.EQ.15) ALTYPE=40
!           IF(VTYPE.EQ.16) ALTYPE=118
!           IF(VTYPE.EQ.17) ALTYPE=119
!           IF(VTYPE.EQ.18) ALTYPE=120
!           IF(VTYPE.EQ.19) ALTYPE=114
!           IF(VTYPE.EQ.20) ALTYPE=115
!           IF(VTYPE.EQ.21) ALTYPE=46
!           IF(VTYPE.EQ.22) ALTYPE=47
!           IF(VTYPE.EQ.23) ALTYPE=48
!           IF(VTYPE.EQ.24) ALTYPE=49
!           IF(VTYPE.EQ.25) ALTYPE=50
!           IF(VTYPE.EQ.75) ALTYPE=43
!           IF(VTYPE.EQ.124) ALTYPE=71
!           IF(VTYPE.EQ.125) ALTYPE=72
!           IF(VTYPE.EQ.126) ALTYPE=73
!           IF(VTYPE.EQ.127) ALTYPE=74
!           IF(VTYPE.EQ.128) ALTYPE=75
!           IF(VTYPE.EQ.129) ALTYPE=81
!           IF(VTYPE.EQ.130) ALTYPE=82
!           IF(VTYPE.EQ.131) ALTYPE=83
!           IF(VTYPE.EQ.132) ALTYPE=84
!           IF(VTYPE.EQ.133) ALTYPE=85
!           IF(VTYPE.EQ.134) ALTYPE=116
!           IF(VTYPE.EQ.135) ALTYPE=86
!           IF(VTYPE.EQ.136) ALTYPE=87
!           IF(VTYPE.EQ.137) ALTYPE=78
!           IF(VTYPE.EQ.138) ALTYPE=79
!           IF(VTYPE.EQ.139) ALTYPE=80
!           IF(VTYPE.EQ.140) ALTYPE=89
!           IF(VTYPE.EQ.141) ALTYPE=11
!           IF(VTYPE.EQ.142) ALTYPE=10
!           IF(VTYPE.EQ.143) ALTYPE=90
!           IF(VTYPE.EQ.144) ALTYPE=91
!           IF(VTYPE.EQ.145) ALTYPE=92
!           IF(VTYPE.EQ.146) ALTYPE=93
!           IF(VTYPE.EQ.147) ALTYPE=94
!           IF(VTYPE.EQ.148) ALTYPE=95
!           IF(VTYPE.EQ.149) ALTYPE=98
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!           ALENS(ALTYPE,INT(VARABL(I,3)))=V1
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                           OPCALC_TYPE=3
!                           CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!           ALENS(ALTYPE,INT(VARABL(I,3)))=VARABL(I,4)
!                             END IF
!           IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
!           ALTYPE=VTYPE-249
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!     !     SET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
!           ISURF=INT(VARABL(I,3))
!           DEFGR1=ALENS(103,ISURF)
!           DEFGR2=ALENS(104,ISURF)
!           DEFGR3=ALENS(105,ISURF)
!           DEFGR4=ALENS(106,ISURF)
!           DEFGR5=ALENS(107,ISURF)
!           DEFGR6=0.0D0
!           DEFGR7=ALENS(109,ISURF)
!           DEFGR8=0.0D0
!           ACTNUM=ALTYPE
!           NEWDEFVAL=V1
!           ERR1=.FALSE.
!           ERR2=.FALSE.
!           CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                           OPCALC_TYPE=3
!                           CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!     !     SET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
!           ISURF=INT(VARABL(I,3))
!           DEFGR1=ALENS(103,ISURF)
!           DEFGR2=ALENS(104,ISURF)
!           DEFGR3=ALENS(105,ISURF)
!           DEFGR4=ALENS(106,ISURF)
!           DEFGR5=ALENS(107,ISURF)
!           DEFGR6=0.0D0
!           DEFGR7=ALENS(109,ISURF)
!           DEFGR8=0.0D0
!           ACTNUM=ALTYPE
!           NEWDEFVAL=VARABL(I,4)
!           ERR1=.FALSE.
!           ERR2=.FALSE.
!           CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
!                             END IF
!           IF(VTYPE.EQ.150) THEN
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!     !     SET THE APPRORIATE REGISTER VALUE
!           ISURF=INT(VARABL(I,3))
!           OREG=GPREG(ISURF)
!           GPREG(ISURF)=V1
!     !     CALCULATE OPERANDS AND LOAD THEM
!                           OPCALC_TYPE=3
!                           CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     RESTORE THE REGISTER VALUE
!           ISURF=INT(VARABL(I,3))
!           GPREG(ISURF)=OREG
!     !     LOOP TO NEXT DERIVATIVE
!     !     DERIVATIVES DONE FOR VARIABLE I
!                             END IF
!           IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
!     !     SPECIAL SURFACE COEFFICIENTS
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!           FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                            OPCALC_TYPE=3
!                             CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!           FTFL01((VTYPE-26),INT(VARABL(I,3)))=VARABL(I,4)
!     !                       SPECIAL SURFACE COEFICIENTS DONE
!                             END IF
!           IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
!     !     SPECIAL SURFACE COEFFICIENTS
!     !     NEW VALUE IS:
!           V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!           FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
!     !     UPDATE THE LENS
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
!     !     CALCULATE OPERANDS AND LOAD THEM
!                            OPCALC_TYPE=3
!                             CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!     !     FOR EACH OPERAND, CALCULATE AND STORE THE DERIVATIVE
!                             DO J=1,OPCNT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     DERIVATIVES DONE FOR VARIABLE I
!     !     RESTORE THE LENS
!           FTFL01((VTYPE-27),INT(VARABL(I,3)))=VARABL(I,4)
!     !                       SPECIAL SURFACE COEFICIENTS DONE
!                             END IF
!                                     ELSE
!     !     CONFIGS VARIABLE
!     !     VCFG IS THE CONFIG NUMBER
!           VCFG=INT(VARABL(I,2))
!     !     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
!           VTYPE=INT(VARABL(I,1))
!     !
!     !     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
!     !     CONFIG, DO AN EOS AND EVALUATE ALL OPERANDS
!     !
!     !     THE OLD VARIABLE VALUE IS:
!                     OLDV1=VARABL(I,4)
!     !     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
!     !     OLDVA1 IS THE VALUE WE WILL USE IN THE RESTORATION PROCESS
!     !     AFTER THE DERIVATIVE IS CALCULATED
!                     VADD=INT(VARABL(I,14))
!           IF(CFADD(VADD,1).EQ.1.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
!           IF(CFADD(VADD,1).EQ.9.AND.OLDV1.NE.0.0D0) OLDV1=1.0D0/OLDV1
!                    N1=OLDV1
!                     CALL AUXNTA
!                    OLDAV1=AN1
!     !     THE NEW VARAIBLE VALUE IS JUST
!                     V1=VARABL(I,4)+(DINMUL*VARABL(I,8))
!           IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
!           IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
!     !     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
!                    N1=V1
!                     CALL AUXNTA
!                    AV1=AN1
!     !
!     !     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
!     !     VARIABLE IS FOUND IS:
!                     VADD=INT(VARABL(I,14))
!     !
!           IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR. &
!           CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123 &
!           .OR.CFADD(VADD,1).EQ.141) THEN
!           CFVAL(VADD,2)=V1
!           CFCHAR(VADD,2)=AV1
!                           ELSE
!           CFVAL(VADD,1)=V1
!           CFCHAR(VADD,1)=AV1
!                           END IF
!     !     NOW UPDATE THE CONFIG ARRAY
!     !     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
!     !     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
!     !     STUFF IT THERE.
!     !     NOW UPDATE THE CONFIG ARRAY
!           IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR. &
!           CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123 &
!           .OR.CFADD(VADD,1).EQ.141) THEN
!           CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7)) &
!           =AV1(1:23)
!                            ELSE
!           CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5)) &
!           =AV1(1:23)
!                            END IF
!     !
!     !     NOW EVALUATE THE OPERANDS
!                            OPCALC_TYPE=3
!                            CALL OPCALC
!           IF(F28.EQ.0) RETURN
!                             CALL OPLOAD
!           IF(F28.EQ.0) RETURN
!                             DO J=1,OPCNT
!     !     CALC THE DERIVATIVE FOR OPERAND J AND STORE IT
!           DERIV(J,I)=(OPERND(J,7))*(OPERND(J,4)-OLDOP(J,4))/ &
!           (DINMUL*VARABL(I,8))
!           DEREXT=.TRUE.
!     !     LOOP TO NEXT DERIVATIVE
!                             END DO
!     !     NOW IN THE SAME PLACE WHERE AV1 WAS STUFFED, STUFF OLDAV1
!                     VADD=INT(VARABL(I,14))
!           IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR. &
!           CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123 &
!           .OR.CFADD(VADD,1).EQ.141) THEN
!           CFVAL(VADD,2)=OLDV1
!           CFCHAR(VADD,2)=OLDAV1
!     !     NOW UPDATE THE CONFIG ARRAY
!           CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7)) &
!           =OLDAV1(1:23)
!                                             ELSE
!           CFVAL(VADD,1)=OLDV1
!           CFCHAR(VADD,1)=OLDAV1
!     !     NOW UPDATE THE CONFIG ARRAY
!           CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5)) &
!           =OLDAV1(1:23)
!                                              END IF
    
!     !     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
!     !     FINISHED WITH A CONFIG VARIABLE
!                             END IF
!     !     LOOP TO NEXT VARIABL
!                             END DO
!     !     WE JUST DID THE LAST CALCULATION AND WE WANT TO
!     !     RESTORE THE ORIGINAL OPERANDS
!                          OPERND(1:OPCNT,1:20)=OLDOP(1:OPCNT,1:20)
!     !     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
!                             F6=1
!                             F1=0
!                             F22=0
!                            LNSTYP=2
!                            CALL LNSEOS
    
!     !     THE DERIVATIVE MATRIX IS FORMED
!                            return
!                            END


end module