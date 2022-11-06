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
#ifdef MACOS
            call get_environment_variable("HOME", basepath)
            path = trim(basepath)//'/Library/Application Support/Zoa/'
            PRINT *, "Path for files is ", trim(path)
#elif

            path = ''
            PRINT *, "Path for files is ", trim(path)
#endif

        end function

        subroutine clear_file(fName)
          use GLOBALS
          IMPLICIT NONE

          character(len=*) :: fName

         OPEN(UNIT=16,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED', &
        &  FILE=basePath//fName,STATUS='UNKNOWN')
        CLOSE(16)

      end subroutine


end module
