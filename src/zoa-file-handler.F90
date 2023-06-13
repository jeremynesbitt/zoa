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

      function open_macro_file(fileName) result(fileID)
         use GLOBALS
        implicit none

        integer :: fileID, rec30, L
        character(len=*) :: fileName
        INCLUDE 'DATMAI.INC'
        INCLUDE 'DATMAC.INC'

        fileID = 30
        L = 1

        inquire(iolength=rec30) MACCW(L),MACQW(L),MACSTR(L),MACNW(1,L),   &
     &  MACNW(2,L),MACNW(3,L),MACNW(4,L),MACNW(5,L),MACSTA(1,L),          &
     &  MACSTA(2,L),MACSTA(3,L),MACSTA(4,L),MACSTA(5,L),MACSTA(6,L),      &
     &  MACSTA(7,L),MACSTA(8,L),MACSTA(9,L),MACSTA(10,L),MACSTA(11,L),    &
     &  MACSTA(12,L),MACSTA(13,L),MACSTA(14,L),MACSTA(15,L),MACSTA(16,L), &
     &  MACSTA(17,L),MACSTA(18,L),MACSTA(19,L),MACSTA(20,L)


        OPEN(UNIT=fileID,ACCESS='DIRECT',FILE=trim(LIBMAC)//fileName,FORM= &
     &  'UNFORMATTED',RECL=rec30,STATUS='UNKNOWN')

      end function


end module
