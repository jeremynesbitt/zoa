module optim_functions
    use GLOBALS, only: long
    use handlers, only: updateTerminalLog
    use type_utils

contains

subroutine aut_debug()
    use kdp_utils, only: OUTKDP
    use DATLEN, only: PFAC
    
    use DATMAI
    use type_utils
    real(kind=long) :: fmtOld, fmtTst, fmtLow, m, iDamp, fmtDamp
    integer, parameter :: maxIter = 10
    real(kind=long), dimension(maxIter) :: fmtArr
    logical :: autConverge
    integer :: i, endCode
    real(kind=long), dimension(2001) :: tmpRMS, tmpTHI


    ! ********************
    ! Old way
    ! Default merit function is spot diagram for now
    call PROCESKDP('MERIT; RMS,,,1;EOS')

    ! ! Temp - print out statement
    ! call outKDP("Iteration 0")
    ! call PROCESKDP("FMT")    
    ! call PROCESKDP("SUR SA")


    ! TEMP Code:  spit out data for offline debugging
    tmpTHI = -100 + .1 * [(i, i=0,2001-1)]
    print *, "tmpTHI is ", tmpTHI
    do i=1,2001
        call PROCESKDP('U L; CHG 3; TH '//real2str(tmpTHI(i),2)//';EOS')
        tmpRMS(i) = SQRT(getMeritFunction())
    end do
    ! PRINT out for copying from terminal
    do i=1,2001
        print *, real2str(tmpTHI(i))//','//real2str(tmpRMS(i))
    end do
end subroutine

subroutine aut_go()
    use kdp_utils, only: OUTKDP
    use DATLEN, only: PFAC
    use DATSUB, only: VARABL
    use DATMAI
    use type_utils
    real(kind=long) :: fmtOld, fmtTst, fmtLow, m, iDamp, fmtDamp
    integer, parameter :: maxIter = 500
    real(kind=long), dimension(maxIter) :: fmtArr
    logical :: autConverge
    integer :: i, endCode
    integer, parameter :: nV = 1000
    real(kind=long), dimension(nV) :: oldVars



    ! ********************
    ! Old way
    ! Default merit function is spot diagram for now
    !call PROCESKDP('MERIT; RMS,,,1;EOS')

    ! ! Temp - print out statement
    ! call outKDP("Iteration 0")
    ! call PROCESKDP("FMT")    
    ! call PROCESKDP("SUR SA")



    ! !call PROCESKDP('OPSPOT RECT; OPRECT, 20')
    ! call PROCESKDP('ITER; PFIND;ITER')

    ! ! Temp - print out statement
    ! call outKDP("Iteration 1")
    ! call PROCESKDP("FMT")    
    ! call PROCESKDP("SUR SA")
    ! End Old way
    ! *****************************


    ! Brute force way.  Pseudocode.
    ! Compute Merit function (Fold)
    ! for each iteration
    ! Compute new merit function (F) for current damping factor
    ! Is F < Fold?
    ! yes - decrease damping factor by m (m between 2..5)
    ! compute F.  Pick damping factor that gives lowest F?
    ! no - increase damping factor by m, compute F
    ! test for convergence
    ! if convergence test passes, exit loop
    ! if it fails, then go to next iteration

    ! Prepare for loop
    autConverge = .FALSE.
    !PFAC = 1E-7 ! Damping factor
    m = 2 ! Scaling factor for damping factor
    fmtLow = getMeritFunction()
    fmtOld = fmtLow
    call updateTerminalLog("Cycle number 0", "black")
    call updateTerminalLog("Error Function = "//real2str(fmtLow), "black")
    call PROCESKDP("SUR SA")
    call updateTerminalLog("Cycle number 0", "black")
    call updateTerminalLog("Error Function = "//real2str(fmtLow), "black")

    do i=1,maxIter  
        oldVars = VARABL(1:nV,4)
        fmtDamp = getMeritFunction()  
        call runIter(2,0,.FALSE.)
        fmtTst = getMeritFunction()
        call LogTermDebug("At start of iter i merit is "//real2str(fmtDamp))
        call LogTermDebug("At start of iter i after solvit merit is "//real2str(fmtTst))
        if (fmtTst < fmtLow) then
            ! This is clearly a mess, but okay for testing
            fmtOld = fmtLow
            fmtLow = fmtTst
            fmtTst = fmtOld
            PFAC = PFAC/m
        else ! went the wrong way
            
            PFAC = PFAC*m
            call restoreLensFromVars(oldVars)
            !call PROCESKDP("RESTORE")
            fmtTst = getMeritFunction()
            !call runIter(2,0,.FALSE.)
            call LogTermDebug("Printing lens arter restore")
            call PROCESKDP("SUR SA")
        end if

        call LogTermDebug("For Convergence Test, comparing "//real2str(fmtTst,4))
        call LogTermDebug("To "//real2str(fmtLow,4))

        autConverge = testAutConvergence(fmtLow, fmtTst, i, endCode)

        ! Log stuff
        call updateTerminalLog("Cycle number "//int2str(i), "black")
        call updateTerminalLog("Error Function = "//real2str(fmtTst)// "(change = "// &
        & real2str((fmtTst-fmtOld)/fmtOld)//")", "black")        
        call PROCESKDP("SUR SA")
        if (autConverge) then 
            !call LogTermDebug("Ending iteration.  TODO:  Add endCode support")
            call updateTerminalLog("Ending Auto algorithm.  Reached acceptable stopping point", "black")
            return
        end if        
    end do


end subroutine


! This was extracted from SOLVIT and added because the KDP command RESTORE wasn't working as I think it should
! when optimizing with multiple variables

subroutine restoreLensFromVars(oldVars)
         use type_utils
         use DATSUB
         use DATMAI
         use DATLEN
         use DATMAC
         
!
      IMPLICIT NONE
      real(kind=long), dimension(:) :: oldVars
      CHARACTER OOLDWQ*8
      LOGICAL ITDER,SILENT!
      INTEGER SSN,SM,NP2,MP,N,J,I,L,M,VTYPE,ALTYPE,VADD,VCFG &
      ,VN1,MAXCNT,IV1,VN,ALLOERR,IID,JJD, II!
      INTEGER ISURF!
      REAL*8 NEWDEFVAL,PFACSCL!
      COMMON/DEFVALCOM/NEWDEFVAL!
        REAL*8 &
        X(1:100000),WT,V1,MAX,VTEMP,OLDCUR,NEWCUR
      DIMENSION WT(:)
      ALLOCATABLE :: WT
      LOGICAL LVAL
      CHARACTER AV1*23


        LOGICAL ERR1,ERR2

      REAL*8 W,V,BTB,BTG,DIAGSUM,WMAX,WMIN
      DIMENSION W(:),V(:,:),BTB(:,:),BTG(:)
      ALLOCATABLE :: W,V,BTB,BTG

      include "DATCFG.INC"

      VARABL(1:size(oldVars),4) = oldVars


!     NOW APPLY THE CHANGE VECTOR TO THE LENS
!**********************************************************************
                        DO I=1,VBCNT
!     FIRST, IF THE VARIABLE IS A CONFIG 1 VARIABLE
!     NEXT IF THE VARIABLE IS IN A CONFIG OTHER THAN 1
      IF(VARABL(I,2).EQ.1.0D0) THEN
!     NON-CONFIGS VARIABLE MEANING CONFIG 1
!     THIS IS A LENS LEVEL VARIABLE CHANGE
!     GET THE DATA TYPE NUMBER OF THE VARIABLE
      VTYPE=INT(VARABL(I,1))
!                          CURVATURE
      IF(VTYPE.EQ.2.OR.VTYPE.EQ.1) THEN
!     SURFACE CURVATURE
!     NEW VALUE IS:
      V1=VARABL(I,4)
                      ALENS(1,INT(VARABL(I,3)))=V1
!                       CURVATURE DONE
                        END IF
      IF(VTYPE.EQ.10.OR.VTYPE.EQ.9) THEN
!     SURFACE TORIC CURVATURE
!     NEW VALUE IS:
      V1=VARABL(I,4)
                      ALENS(24,INT(VARABL(I,3)))=V1
!                       TORIC CURVATURE DONE
                        END IF
      IF(VTYPE.GE.3.AND.VTYPE.LE.8) THEN
      IF(VTYPE.EQ.3) ALTYPE=3
      IF(VTYPE.EQ.4) ALTYPE=2
      IF(VTYPE.EQ.5) ALTYPE=4
      IF(VTYPE.EQ.6) ALTYPE=5
      IF(VTYPE.EQ.7) ALTYPE=6
      IF(VTYPE.EQ.8) ALTYPE=7
!     NEW VALUE IS:
      V1=VARABL(I,4)
      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
!                       THESE VARIABLES DONE
                        END IF
      IF(VTYPE.GE.11.AND.VTYPE.LE.25.OR.VTYPE.EQ.75.OR.VTYPE.GE. &
      124.AND.VTYPE.LE.149) THEN
      IF(VTYPE.EQ.11) ALTYPE=41
      IF(VTYPE.EQ.12) ALTYPE=37
      IF(VTYPE.EQ.13) ALTYPE=38
      IF(VTYPE.EQ.14) ALTYPE=39
      IF(VTYPE.EQ.15) ALTYPE=40
      IF(VTYPE.EQ.16) ALTYPE=118
      IF(VTYPE.EQ.17) ALTYPE=119
      IF(VTYPE.EQ.18) ALTYPE=120
      IF(VTYPE.EQ.19) ALTYPE=114
      IF(VTYPE.EQ.20) ALTYPE=115
      IF(VTYPE.EQ.21) ALTYPE=46
      IF(VTYPE.EQ.22) ALTYPE=47
      IF(VTYPE.EQ.23) ALTYPE=48
      IF(VTYPE.EQ.24) ALTYPE=49
      IF(VTYPE.EQ.25) ALTYPE=50
      IF(VTYPE.EQ.75) ALTYPE=43
      IF(VTYPE.EQ.124) ALTYPE=71
      IF(VTYPE.EQ.125) ALTYPE=72
      IF(VTYPE.EQ.126) ALTYPE=73
      IF(VTYPE.EQ.127) ALTYPE=74
      IF(VTYPE.EQ.128) ALTYPE=75
      IF(VTYPE.EQ.129) ALTYPE=81
      IF(VTYPE.EQ.130) ALTYPE=82
      IF(VTYPE.EQ.131) ALTYPE=83
      IF(VTYPE.EQ.132) ALTYPE=84
      IF(VTYPE.EQ.133) ALTYPE=85
      IF(VTYPE.EQ.134) ALTYPE=116
      IF(VTYPE.EQ.135) ALTYPE=86
      IF(VTYPE.EQ.136) ALTYPE=87
      IF(VTYPE.EQ.137) ALTYPE=78
      IF(VTYPE.EQ.138) ALTYPE=79
      IF(VTYPE.EQ.139) ALTYPE=80
      IF(VTYPE.EQ.140) ALTYPE=89
      IF(VTYPE.EQ.141) ALTYPE=11
      IF(VTYPE.EQ.142) ALTYPE=10
      IF(VTYPE.EQ.143) ALTYPE=90
      IF(VTYPE.EQ.144) ALTYPE=91
      IF(VTYPE.EQ.145) ALTYPE=92
      IF(VTYPE.EQ.146) ALTYPE=93
      IF(VTYPE.EQ.147) ALTYPE=94
      IF(VTYPE.EQ.148) ALTYPE=95
      IF(VTYPE.EQ.149) ALTYPE=98
!     NEW VALUE IS:
      V1=VARABL(I,4)
                      ALENS(ALTYPE,INT(VARABL(I,3)))=V1
                        END IF
      IF(VTYPE.EQ.150) THEN
!     NEW VALUE IS:
      V1=VARABL(I,4)
      GPREG(INT(VARABL(I,3)))=V1
                        END IF
      IF(VTYPE.GE.250.AND.VTYPE.LE.4218) THEN
      ALTYPE=VTYPE-249
!     NEW VALUE IS:
      V1=VARABL(I,4)
!     RESET THE APPRORIATE ARRAY VALUE IN THE DEFORMABLE SURFACE
      ISURF=INT(VARABL(I,3))
      DEFGR1=ALENS(103,ISURF)
      DEFGR2=ALENS(104,ISURF)
      DEFGR3=ALENS(105,ISURF)
      DEFGR4=ALENS(106,ISURF)
      DEFGR5=ALENS(107,ISURF)
      DEFGR6=0.0D0
      DEFGR7=ALENS(109,ISURF)
      DEFGR8=0.0D0
      ACTNUM=ALTYPE
      NEWDEFVAL=V1
      ERR1=.FALSE.
      ERR2=.FALSE.
      CALL DEFGRIDS(6,ISURF,ERR1,ERR2)
                        END IF
      IF(VTYPE.GE.27.AND.VTYPE.LE.74) THEN
!     SPECIAL SURFACE COEFFICIENTS
!     NEW VALUE IS:
      V1=VARABL(I,4)
      FTFL01((VTYPE-26),INT(VARABL(I,3)))=V1
!                       SPECIAL SURFACE COEFICIENTS DONE
                        END IF
      IF(VTYPE.GE.76.AND.VTYPE.LE.123) THEN
!     SPECIAL SURFACE COEFFICIENTS
!     NEW VALUE IS:
      V1=VARABL(I,4)
      FTFL01((VTYPE-27),INT(VARABL(I,3)))=V1
!                       SPECIAL SURFACE COEFICIENTS DONE
                        END IF
!
                                ELSE
!     CONFIGS VARIABLE
!     VCFG IS THE CONFIG NUMBER
      VCFG=INT(VARABL(I,2))
!     VTYPE IS THE VARIABLE TYPE NUMBER AS USED IN THE VARIABLES ARRAYS
      VTYPE=INT(VARABL(I,1))
!
!     FOR VARIABLE I, APPLY THE SPECIFIED CHANGE TO THE SPECIFIED
!     CONFIG
!
!     THE NEW VARAIBLE VALUE IS JUST
      V1=VARABL(I,4)
!     CONVERT THIS VALUE TO A CHARACTER*23 CHARACTER VARIABLE
                VADD=INT(VARABL(I,14))
      IF(CFADD(VADD,1).EQ.1.AND.V1.NE.0.0D0) V1=1.0D0/V1
      IF(CFADD(VADD,1).EQ.9.AND.V1.NE.0.0D0) V1=1.0D0/V1
                CALL AUXNTA
!
!     THE POSITION IN THE CFADD,CFVAL AND CFCHAR ARRAYS WHERE THIS
!     VARIABLE IS FOUND IS:
!
      IF(CFADD(VADD,1).GE.27.AND.CFADD(VADD,1).LE.74.OR. &
      CFADD(VADD,1).GE.76.AND.CFADD(VADD,1).LE.123 &
      .OR.CFADD(VADD,1).EQ.141) THEN
      CFVAL(VADD,2)=V1
      CFCHAR(VADD,2)=AV1
!     NOW UPDATE THE CONFIG ARRAY
      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,6):CFADD(VADD,7)) &
      =AV1(1:23)
                      ELSE
      CFVAL(VADD,1)=V1
      CFCHAR(VADD,1)=AV1
!     NOW UPDATE THE CONFIG ARRAY
      CONFG(CFADD(VADD,3),CFADD(VADD,9))(CFADD(VADD,4):CFADD(VADD,5)) &
      =AV1(1:23)
                      END IF
!     NOW LOOK UP WHERE THIS CHARACTER REPRESENTATION OF THE NEW VALUE
!     SHOULD BE STUFFED INTO THE CONFIG ARRAYS CONFG AND
!     STUFF IT THERE.
!
!     NOW LOOP BACK AND REPEAT FOR THE NEXT VARIABLE.
!     FINISHED WITH A CONFIG VARIABLE
                        END IF
!     LOOP TO NEXT VARIABL
                        END DO
!     UPDATE THE LENS OR THE PERMANENT LENS WILL BE ******!
                        CALL FIXDEFORMFILE
                        F6=1
                        F1=0
                        F22=0
                       LNSTYP=2
                       CALL LNSEOS

end subroutine


subroutine runIter(IFUNCTION, ICHK, ITERROR)
    use DATMAI
    use DATSUB, only: DEREXT

    implicit none 
    integer :: IFUNCTION, ICHK
    logical :: ITERROR  

    F28=1
    WC='SV'
    WQ='        '
    WS=' '
    W1=0.0D0
    W2=0.0D0
    W3=0.0D0
    W4=0.0D0
    W5=0.0D0
    DF1=1
    DF2=1
    DF3=1
    DF4=1
    DF5=1
    S1=0
    S2=0
    S3=0
    S4=0
    S5=0
    SN=0
    STI=0
    SQ=0
    SST=0

    DEREXT = .FALSE.
                      
    CALL ITER(IFUNCTION,ICHK,ITERROR)
    F28=0
end subroutine    


function testAutConvergence(fmtOld, fmtTst, i, endCode) result(autConverge)
    real(kind=long), intent(in) :: fmtOld, fmtTst
    integer, intent(in) :: i
    integer, intent(inout) :: endCode
    logical :: autConverge
    real(kind=long) :: tstVal, smallChange

    autConverge = .FALSE.
    smallChange = .0000001
    !tstVal = (fmtTst-fmtOld)/fmtOld
    tstVal = (fmtTst-fmtOld)/fmtOld

    ! For now force min 5 iterations.  will fix this with an inner loop after multi-var testing
    if (i <500) return

    !For now only support it being not too different.  Eventually add more options
    if (DABS(tstVal) < smallChange) then
        autConverge = .TRUE.
    end if

end function

function getMeritFunction() result(fmt)
    use DATSUB
    use DATMAI, only: F28, KILOPT
    use kdp_utils, only: OUTKDP
    real(kind=long) :: fmt
    F28=1
    OPCALC_TYPE=3
       CALL OPCALC
    IF(F28.EQ.0) RETURN
       CALL OPLOAD
    IF(F28.EQ.0) RETURN
     IF(KILOPT) THEN
        call OUTKDP('SOME OPERANDS ARE NOT CALCULABLE', 1)
        call OUTKDP('THE CURRENT FIGURE OF MERIT IS MEANINGLESS.', 1)
        RETURN
      END IF
    FMTFLG=.TRUE.
    !     PROCEED WITH ACTION FOR COMMAND

    fmt=0.0D0
    DO I=1,OPCNT
    fmt=fmt+(OPERND(I,14)**2)
    END DO


end function

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