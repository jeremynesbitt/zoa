module zoa_file_handler

      !INTEGER ID_SYSTEM
      !COMMON/SYSTEMID/ID_SYSTEM

    contains

      function getFileSep() result(res)

        character(len=1) :: res
          !Windows
          IF(ID_SYSTEM.EQ.3.OR.ID_SYSTEM.EQ.4) THEN
            res = '\'
          else !MacOS or Linux
            res = '/'
          end if
      end function

      function getZoaPath() result(path)

        character(len=255) :: path, basepath
        integer :: tstifdef

! Cannot indent these c directives
       PRINT *, "About to enter ifdef part... "
! #ifdef MACOS
! tstifdef = 1
! #elif
! tstifdef = 0
! #endif
! PRINT *, "tstifdef is ", tstifdef
            path = ''

#ifdef MACOS
            call get_environment_variable("HOME", basepath)
            path = trim(basepath)//'/Library/Application Support/Zoa/'
            PRINT *, "Path for files is ", trim(path)
#endif
         PRINT *, "Path for files is ", trim(path)
        end function

        subroutine clear_file(fName)
          use GLOBALS
          IMPLICIT NONE

          character(len=*) :: fName

         OPEN(UNIT=16,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED', &
        &  FILE=fName,STATUS='UNKNOWN')
        CLOSE(16)

      end subroutine

      function open_mac_dat() result(fileID)

!       THE INITIAL VALUES STORED IN MAC.DAT ARE:
!
!               MNAME IS THE MACRO NAME
!               M1 IS THE MACRO LOCK
!               M2 IS THE MACRO LENGTH
!               M3 IS THE OCCUPANCY FLAG
!                               = 0 FOR BLANK MACRO
!                               = 1 FOR NON-BLANK MACRO
!               STAMP is ???

       ! Example of read call
       ! READ(UNIT=20,REC=I,ERR=10) MNAME,M1,M2,NEXTM3,STAMP
       ! M3=M3+NEXTM3


        USE GLOBALS
        implicit none


        integer :: fileID
        integer :: recLength
        character(len=8) :: MNAME
        character(len=20)  :: STAMP
          INTEGER :: M1, M2, M3

        INCLUDE 'DATMAI.INC'

        inquire(iolength=recLength) MNAME, M1, M2, M3, STAMP

        fileID = 20

        OPEN(UNIT=fileID,ACCESS='DIRECT', &
     &    FILE=trim(LIBMAC)//'MAC.DAT',FORM= &
     &    'UNFORMATTED',RECL=recLength,STATUS='UNKNOWN')

      end function


end module
