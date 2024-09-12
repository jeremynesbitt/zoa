! Created this module to try and consolidate file io for this program
!
module zoa_file_handler
      use iso_c_binding
      use gtk_sup, only: convert_c_string

      !INTEGER ID_SYSTEM
      !COMMON/SYSTEMID/ID_SYSTEM

      character(len=1024) :: codevdir
      character(len=1024) :: savresDir
      character(len=1024) :: currSaveDir

      integer :: ID_SYSTEM = -1000
      integer, parameter :: ID_OS_WINDOWS = 3
      integer, parameter :: ID_OS_MAC = 1
      integer, parameter :: ID_OS_LINUX = 2

      interface

      function get_macos_bundle_dir() bind(c)
        import :: c_ptr, c_char
        type(c_ptr) :: get_macos_bundle_dir
        !character(kind=c_char), dimension(*) :: file_name
      end function
 
      
      function browser_open_url (url) bind(c)
        import :: c_int, c_char
        character(kind=c_char), dimension(*) :: url
        integer(c_int) :: browser_open_url
      end function
      end interface  

    contains

      subroutine openHelpFile()
        implicit none
        character(len=1024) :: helpfilePath
        integer(kind=c_int) :: browserResult
#ifdef MACOS
      type(c_ptr) :: ptr_macbundledir
      character(len=1024) :: str_bundle_dir
      
#endif

#ifdef WINDOWS
     helpfilePath = trim(getZoaPath())//'help'// &
     & getFileSep()//'html'//getFileSep()//'index.html'

     call LogTermFOR("Loc is "//trim(helpfilePath))   
     browserResult =  browser_open_url(trim(helpfilePath))


#endif

#ifdef MACOS
      ptr_macbundledir = get_macos_bundle_dir()
      print *, "Ptr Loc is ", LOC(ptr_macbundledir)
      call convert_c_string(ptr_macbundledir, str_bundle_dir)
      helpfilePath = 'file:'//getFileSep()//getFileSep()//getFileSep()// &
      & trim(str_bundle_dir)//getFileSep()//'Resources'//getFileSep()// &
      & 'help'//getFileSep()//'html'//getFileSep()//'index.html'

      browserResult =  browser_open_url(trim(helpfilePath))
      !call LogTermFOR("Browser Result is "//int2str(browserResult))
#endif      




      end subroutine

      function isSPOFileOpen(fileTest) result(res)
        include "DATMAI.INC"
        character(len=*) :: fileTest
        logical :: res
        res = .FALSE.
        INQUIRE(FILE=trim(LIBSPO)//fileTest,OPENED=res)
      end function

      function doesFileExist(fileTest) result(res)
        character(len=*) :: fileTest
        logical :: res
        res = .FALSE.
        INQUIRE(FILE=fileTest,EXIST=res)
        
      end function

      function getFileSep() result(res)



        character(len=1) :: res

          !Windows
          IF(ID_SYSTEM.EQ.ID_OS_WINDOWS) THEN
            res = '\'
          else !MacOS or Linux
            res = '/'
          end if
          !PRINT *, "In getFileSep ID_SYSTEM = ", ID_SYSTEM
      end function

      function getZoaPath() result(path)
        use iso_c_binding, only: c_char, c_null_char
        use g, only: g_setenv

        character(len=255) :: path, basepath
        character(len=1024) :: pwd
        character(kind=c_char, len=1224) :: PLPLOT_LIB, XDG_DATA_DIR
        integer :: tstifdef, setenv_result

! Cannot indent these c directives
       !PRINT *, "About to enter ifdef part... "

            path = ''

#ifdef MACOS
            !PRINT *, "MACOS IFDEF LOOP ACTIVATED!"
            call get_environment_variable("HOME", basepath)
            path = trim(basepath)//'/Library/Application Support/Zoa/'
            !PRINT *, "Path for files is ", trim(path)
            ID_SYSTEM = ID_OS_MAC
            ! Set PlPlot data directory
            PLPLOT_LIB = trim(path)//'plplot'
            setenv_result = g_setenv("PLPLOT_LIB"//c_null_char, trim(PLPLOT_LIB)//c_null_char, 1)
            !PRINT *, "setenv_result = ", setenv_result
            call get_environment_variable("PWD", pwd)
            !call LogTermFOR("PWD is "//trim(pwd))
            call get_environment_variable("PLPLOT_LIB", pwd)
            
            

#endif

#ifdef WINDOWS
           !PRINT *, "WINDOWS IFDEF LOOP ACTIVATED!"
           call get_environment_variable("APPDATA", basepath)
           path = trim(basepath)//'\Zoa\'
           ID_SYSTEM = ID_OS_WINDOWS
           !PRINT *, "Path for files is ", trim(path)
           PLPLOT_LIB = trim(path)//'plplot'
           setenv_result = g_setenv("PLPLOT_LIB"//c_null_char, trim(PLPLOT_LIB)//c_null_char, 1)
           !PRINT *, "setenv_result = ", setenv_result
           call get_environment_variable("PLPLOT_LIB", pwd)
           !PRINT *, "Basepath is ", trim(pwd)      
           XDG_DATA_DIR = trim(path)//'share' !\glib-2.0\schemas'
           setenv_result = g_setenv("XDG_DATA_DIRS"//c_null_char, trim(XDG_DATA_DIR)//c_null_char, 1)
           !PRINT *, "setenv_result = ", setenv_result
           call get_environment_variable("XDG_DATA_DIRS", pwd)
           !PRINT *, "XDG Path is ", trim(pwd)       
#endif

#ifdef LINUX
          ID_SYSTEM = ID_OS_LINUX
          path = './Library/'
#endif



        ! Since this method essentially serves as an initialization
        ! add this here.  Should probably go somewhere else.
        codevdir = trim(path)//getFileSep()//'CodeV'//getFileSep()
        savresDir = trim(path)//getFileSep()//'Projects'//getFileSep()

        currSaveDir = savresDir

        !PRINT *, "Set ID_SYSTEM in getZoaPath to ", ID_SYSTEM

        end function

        function getRestoreFilePath(fName) result(fullPath)
          !use handlers, only: updateTerminalLog
          implicit none
          character(len=*) :: fName
          character(len=1024) :: fullPath

          fullPath = trim(savresDir)//fName
          if (doesFileExist(trim(fullPath))) then
            return
          else
            call LogTermFOR("Error:  File does not exist "//trim(fullPath))
            !call updateTerminalLog("Error:  File does not exist "//trim(fullPath), "red")
            fullPath = ""
            return
          end if
            

        end function


        function open_file_to_sav_lens(fName, dirName) result(fID)
          use gtk_hl_dialog
          use iso_c_binding, only:  c_null_char
          implicit none
          character(len=*) :: fName
          character(len=*), optional :: dirName

          integer :: fID, stat, resp
          character(len=80), dimension(2) :: msg
          character(len=2048) :: fullPath

          fID = 1111
          ! This is to be stored in the savresDir
          if (present(dirName)) then
            fullPath = dirName//getFileSep()//fName
          else
            fullPath = trim(getSaveDirectory())//fName
          end if

          call LogTermFOR('FUll Path is '//trim(fullPath))
          ! First check if file exists
          if (doesFileExist(trim(fullPath))) then
            ! Ask user if they want to overwrite
            msg(1) = "Do you want to overwrite" 
            msg(2) = fName//" ?" 
            !msg(2) = "File "//fName//" ?"
            resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
            & "Warning"//c_null_char)    
            if (resp == GTK_RESPONSE_YES) then  
              call LogTermFOR("Clearing file")  
              call delete_file(trim(fullPath))
            else
              call LogTermFOR("Save cancelled to avoid overwriting previous file.")
            end if            
          end if
          
          
          open(unit=fID, iostat=stat, file=trim(fullPath), &
          & status='new', action="write")
          if (stat /= 0) fID=0 ! Error
          

        end function

        subroutine delete_file(fName)
          implicit none
          character(len=*) :: fName
          integer :: stat

           open(unit=1234, iostat=stat, file=fName, status='old')
           if (stat == 0) close(1234, status='delete')

        end subroutine

        subroutine clear_file(fName)
          use GLOBALS
          IMPLICIT NONE

          character(len=*) :: fName

         OPEN(UNIT=16,ACCESS='SEQUENTIAL',BLANK='NULL',FORM='FORMATTED', &
        &  FILE=fName,STATUS='UNKNOWN')
        CLOSE(16)

      end subroutine

      subroutine process_zoa_file(fileName)
        implicit none
        character(len=*) :: fileName
        integer :: n, ios
        character(len=256) :: line

             
        open(unit=99, file=trim(fileName), iostat=ios)
        if ( ios /= 0 ) stop "Error opening file "
    
        n = 0
    
        do
            read(99, '(A)', iostat=ios) line
            if (ios /= 0) then 
              !call LogtermFOR("End of file")
              close(99)
              return
            else
              call LogTermFOR("Processing "//trim(line))
              call PROCESKDP(trim(line))
            end if
            n = n + 1
        end do      
        

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

      subroutine saveCommandHistoryToFile(command_history, command_index)
        implicit none
        character(len=*), intent(in) :: command_history(:)
        integer :: command_index
        character(len=1024) :: filePath
        logical :: existFlag
        integer :: recLen, i

        filePath = trim(getZoaPath())//'command_history.dat'

        inquire(file=filePath,EXIST=existFlag)
        if(existFlag) call clear_file(filePath)

        inquire(iolength=recLen) command_history(1)


        OPEN(UNIT=16,ACCESS='DIRECT',FILE=trim(filePath), FORM='UNFORMATTED', &
        & RECL=recLen,STATUS='UNKNOWN')

        if(command_index.GT.size(command_history)) command_index = size(command_history)

        do i=1, command_index
          write(unit=16, rec=i) command_history(i)
        end do
        close(UNIT=16)

      end subroutine

      subroutine readCommandHistoryFromFile(command_history, command_index)

        implicit none
        character(len=*), intent(inout) :: command_history(:)
        integer, intent(inout) :: command_index
        character(len=1024) :: filePath
        logical :: existFlag
        integer :: recLen, i

        filePath = trim(getZoaPath())//getFileSep()//'command_history.dat'

        inquire(file=filePath,EXIST=existFlag)
        if(existFlag) then

          inquire(iolength=recLen) command_history(1)
          OPEN(UNIT=16,ACCESS='DIRECT',FILE=trim(filePath), FORM='UNFORMATTED', &
          & RECL=recLen,STATUS='UNKNOWN')

        do i=1, size(command_history)
          read(unit=16, rec=i, err=77) command_history(i)
        end do


        !PRINT *, "Command History is ", command_history
77      PRINT *, "End of file is record ", i
        command_index = i
        close(unit=16)
        return
        end if
      end subroutine

subroutine setCodeVDir(newDir)
  implicit none
  character(len=*) :: newDir

  codevdir = newDir

end subroutine

function getCodeVDir() result(res)
  use GLOBALS, only : basePath
  implicit none
  character(len=250) :: res

  !codevdir = trim(basePath)//'CodeV'//getFileSep()
  res = codevdir


end function

function getZemaxDir() result(res)
  use GLOBALS, only : basePath
  implicit none
  character(len=250) :: res

  !codevdir = trim(basePath)//'CodeV'//getFileSep()
  res = trim(basePath)//'Zemax'


end function

function getProjectDir() result(res)
  use GLOBALS, only : basePath
  implicit none
  character(len=250) :: res

  !codevdir = trim(basePath)//'CodeV'//getFileSep()
  ! TODO:  Add a ui so users can set this themselves
  res = trim(basePath)//'Projects'//getFileSep()


end function

function addFileSepIfNeeded(filePath) result(adjFilePath)
  character(len=*) :: filePath
  character(len=len(filePath)+1) :: adjFilePath
  integer :: k

  adjFilePath = filePath
  k = len(filePath)
  if (filePath(k:k) /= getFileSep()) then
     adjFilePath = filePath//getFileSep()
  end if


end function

subroutine setSaveDirectory(saveDir) 
  character(len=*) :: saveDir
  
  currSaveDir = trim(addFileSepIfNeeded(saveDir))
  
end subroutine

function getSaveDirectory() result(saveDir)
  character(len=1024) :: saveDir

  saveDir = currSaveDir
  
end function


function getFileNameFromPath(fileName) result(res)
  implicit none
  character(len=*) :: fileName
  character(len=1500) :: res
  integer :: slashLoc

  slashLoc = index(fileName, getFileSep(), BACK=.TRUE.)

  res = fileName(slashLoc+1:len(fileName))

end function

function getPermMacroDir() result(permDir)
  ! Eventually want to support user changing directories
  ! For now use this fcn to put all directory changes in
  ! one place
  character(len=500) :: permDir
  permDir = trim(getZoaPath())//'PERMAC'//getFileSep()

end function

function genOutputLineWithSpacing(blnk, p1, p2, p3, p4) result(outStr)
  implicit none
  character(len=*) :: blnk
  character(len=*) :: p1, p2
  character(len=*), optional :: p3, p4
  character(len=1024) :: outStr
  
  
  outStr = p1//blnk//p2

  if(present(p3)) outStr = trim(outStr)//blnk//p3
  if(present(p4)) outStr = trim(outStr)//blnk//p4

end function

!subroutine exportCurrentSessionToFile(fName)
!  character(len=*) :: fName

  ! FOr now, pseudocode
   ! writeLine('LEN')
   ! writeLine('TIT '//'//getLensSystemTitle()//')
   ! writeLine(getPupilCmd)
   ! writeLine(getFieldDefinition)
   ! writeLine(getWavelength)
   ! do i=1, numSurfaces
   ! writeLine(surfaceData)
   ! end do
   ! writeLine('GO')

!end subroutine

end module
