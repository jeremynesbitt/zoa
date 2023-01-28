
      ! STI is 1 when ? is after command
      LOGICAL FUNCTION COMMANDINFOCHECK(OUTTXT)

        IMPLICIT NONE

        INTEGER STI
        CHARACTER(LEN=*), INTENT(IN) :: OUTTXT
        CHARACTER(LEN=140) :: OUTLYNE

        COMMON/KUERRY/STI
        COMMON/REPLWRIT/OUTLYNE

        COMMANDINFOCHECK = .FALSE.

        IF (STI.EQ.1) THEN
           COMMANDINFOCHECK = .TRUE.
           OUTLYNE=OUTTXT
           CALL SHOWIT(1)
        END IF

        END FUNCTION

      ! LOGICAL FUNCTION  NUMERICINPUTCHECK
      !
      !   IMPLICIT NONE
      !
      !   INCLUDE "DATMAI.INC"
      !
      !   IF(SN.EQ.1.OR.SQ.EQ.1)
      !
      !
      !
      ! END FUNCTION

      LOGICAL FUNCTION STRINGINPUTCHECK(CMDSTR)

         IMPLICIT NONE

         CHARACTER(LEN=*), INTENT(IN) :: CMDSTR

        ! Globals
        CHARACTER(LEN=140) :: OUTLYNE

        INTEGER SB1,SB2,SC1,SC2,SQ,SST,S1,S2,S3,S4,S5,
     1  DF1,DF2,DF3,DF4,DF5,SN,STI
C
        REAL*8 W1,W2,W3,W4,W5
C
        COMMON/CNTLNM/W1,W2,W3,W4,W5,SB1,SB2,SC1,SC2,SQ,SST,
     1  S1,S2,S3,S4,S5,DF1,DF2,DF3,DF4,DF5,SN

        COMMON/REPLWRIT/OUTLYNE
         STRINGINPUTCHECK = .FALSE.
         IF(SST.EQ.1.OR.SQ.EQ.1) THEN
           STRINGINPUTCHECK = .TRUE.
           OUTLYNE = CMDSTR // " TAKES NO QUALIFIER OR STRING INPUT"
           CALL SHOWIT(1)
           OUTLYNE='RE-ENTER COMMAND'
           CALL SHOWIT(1)
           CALL MACFAL
         END IF

      END FUNCTION

      LOGICAL FUNCTION CHECKMAXFLOATINPUTS(CMDSTR, MAXNUM)

         IMPLICIT NONE

         CHARACTER(LEN=*), INTENT(IN) :: CMDSTR
         INTEGER, INTENT(IN) :: MAXNUM

        ! Globals
        CHARACTER(LEN=140) :: OUTLYNE

        INTEGER SB1,SB2,SC1,SC2,SQ,SST,S1,S2,S3,S4,S5,
     1  DF1,DF2,DF3,DF4,DF5,SN,STI
C
        REAL*8 W1,W2,W3,W4,W5
C
        COMMON/CNTLNM/W1,W2,W3,W4,W5,SB1,SB2,SC1,SC2,SQ,SST,
     1  S1,S2,S3,S4,S5,DF1,DF2,DF3,DF4,DF5,SN

        COMMON/REPLWRIT/OUTLYNE
        CHECKMAXFLOATINPUTS = .TRUE.
        SELECT CASE (MAXNUM)

         CASE (2)
              IF(S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) THEN
                CHECKMAXFLOATINPUTS = .FALSE.
                OUTLYNE = CMDSTR //
     1  "ONLY TAKES NUMERIC WORD #1 AND #2 INPUT"
             END IF

        END SELECT

        IF (.NOT.CHECKMAXFLOATINPUTS) THEN
          CALL SHOWIT(1)
          OUTLYNE='RE-ENTER COMMAND'
          CALL SHOWIT(1)
          CALL MACFAL
        END IF

      END FUNCTION

      subroutine getInputStringLength(CVFILENAME, CVFILENAMELENGTH)

        implicit none

        character(len=80) :: CVFILENAME
        integer, intent(inout) :: CVFILENAMELENGTH
        integer :: I

        CVFILENAMELENGTH = 0
        DO I=80,1,-1
              IF(CVFILENAME(I:I).NE.' ') THEN
                CVFILENAMELENGTH=I
                EXIT
              END IF
        END DO


      end subroutine
