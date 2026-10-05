! Created this module to try and consolidate file io for this program
!
module zoa_file_handler
      use iso_c_binding
      use gtk_sup, only: convert_c_string

      !INTEGER ID_SYSTEM
      !COMMON/SYSTEMID/ID_SYSTEM

      character(len=1024) :: codevdir
      character(len=1024) :: savresDir
      character(len=1024) :: currSaveDir, tempDir
      character(len=1024) :: macroDir
      character(len=1024) :: glassCatalogDirOverride = ''

      integer :: ID_SYSTEM = -1000
      integer, parameter :: ID_OS_WINDOWS = 3
      integer, parameter :: ID_OS_MAC = 1
      integer, parameter :: ID_OS_LINUX = 2

      ! Search path: extra folders searched for lenses (RES), macros
      ! (macro:), CODE V / Zemax imports and the new-lens template, after the
      ! folder that kind of file normally lives in.  Each folder may hold the
      ! files directly or in Projects/, Macros/ and CodeV/ subfolders (the
      ! install's layout).  Entries come from the ZOA_SEARCH_PATH environment
      ! variable, the SearchPath= preference lines, or the SEARCHPATH command.
      integer, parameter :: MAX_SEARCH_DIRS = 32
      integer, parameter :: SP_ENV = 1      ! ZOA_SEARCH_PATH (not saved)
      integer, parameter :: SP_PREF = 2     ! preferences / SEARCHPATH ADD (saved)
      integer, parameter :: SP_SESSION = 3  ! this run only (test runner)
      character(len=1024) :: searchDirs(MAX_SEARCH_DIRS) = ''
      integer :: searchOrigin(MAX_SEARCH_DIRS) = 0
      integer :: nSearchDirs = 0
      ! getZoaPath sets the default asset folders on its first call only, so
      ! later calls cannot undo the preferences or a test's isolation.
      logical :: assetDirsInitialized = .false.
      ! Set by the test runner: preferences are neither read nor written.
      logical :: preferencesIsolated = .false.

      ! Recursion depth counter for process_zoa_file: incremented on open,
      ! decremented on close.  Gating headless per-line flushes on depth==1
      ! prevents mid-file snapshots during nested RES loads.
      integer :: zoa_file_depth = 0

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

      subroutine openHelpFile(fileName)
        use GLOBALS, only: basePath
        use zoa_output, only: zoa_emit
        implicit none
        character(len=1024) :: helpfilePath
        integer(kind=c_int) :: browserResult
        character(len=*) :: fileName
#ifdef MACOS
      type(c_ptr) :: ptr_macbundledir
      character(len=1024) :: str_bundle_dir
      
#endif

#ifdef WINDOWS
     helpfilePath = trim(basePath)//'help'// &
     & getFileSep()//'html'//getFileSep()//fileName
     browserResult = browser_open_url(trim(helpfilePath)//c_null_char)
     if (browserResult == 0) call zoa_emit('Unable to open help: '//trim(helpfilePath), 'red')


#endif

#ifdef MACOS
      ptr_macbundledir = get_macos_bundle_dir()
      call convert_c_string(ptr_macbundledir, str_bundle_dir)
      helpfilePath = 'file:'//getFileSep()//getFileSep()//getFileSep()// &
      & trim(str_bundle_dir)//getFileSep()//'Resources'//getFileSep()// &
      & 'help'//getFileSep()//'html'//getFileSep()//fileName

      browserResult = browser_open_url(trim(helpfilePath)//c_null_char)
#endif      




      end subroutine

      function isSPOFileOpen(fileTest) result(res)
        use DATMAI
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
        use platform_io, only: configure_plplot_runtime
        use iso_c_binding, only: c_char, c_null_char
        use g, only: g_setenv

        character(len=255) :: path, basepath
        character(len=1024) :: pwd
        character(kind=c_char, len=1224) :: PLPLOT_LIB, XDG_DATA_DIR
        integer :: tstifdef, setenv_result

! Cannot indent these c directives
       !PRINT *, "About to enter ifdef part... "

            path = ''
            call configure_plplot_runtime()

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
        ! add this here.  Should probably go somewhere else.  Only on the
        ! first call: it is also called later just for the base path.
        if (.not. assetDirsInitialized) then
          codevdir  = trim(path)//'CodeV'//getFileSep()
          savresDir = trim(path)//'Projects'//getFileSep()
          tempDir   = trim(path)//'Temp'//getFileSep()
          macroDir  = trim(path)//'MACROS'//getFileSep()

          currSaveDir = savresDir
          call loadSearchPathFromEnv()
          assetDirsInitialized = .true.
        end if

        !PRINT *, "Set ID_SYSTEM in getZoaPath to ", ID_SYSTEM

        end function

        function getRestoreFilePath(fName) result(fullPath)
          !use handlers, only: updateTerminalLog
          implicit none
          character(len=*) :: fName
          character(len=1024) :: fullPath

          fullPath = findDataFile('Projects', fName)
          if (len_trim(fullPath) > 0) then
            return
          else
            fullPath = trim(savresDir)//fName
            call LogTermFOR("Error:  File does not exist "//trim(fullPath))
            !call updateTerminalLog("Error:  File does not exist "//trim(fullPath), "red")
            fullPath = ""
            return
          end if
            

        end function

        function open_file_to_sav_lens(fName, dirName, overwriteFlag, fullPathOut) result(fID)
          use gtk_hl_dialog
          use iso_c_binding, only:  c_null_char
          use GLOBALS, only: HEADLESS_MODE
          implicit none
          character(len=*) :: fName
          character(len=*), optional :: dirName
          logical, optional :: overwriteFlag
          character(len=*), intent(out), optional :: fullPathOut

          integer :: fID, stat, resp
          character(len=80), dimension(2) :: msg
          character(len=2048) :: fullPath

          fID = 1111
          if (present(fullPathOut)) fullPathOut = ''
          ! This is to be stored in the savresDir
          if (present(dirName)) then
            if (dirName(len_trim(dirName):len_trim(dirName)) == getFileSep()) then
              fullPath = trim(dirName)//trim(fName)
            else
              fullPath = trim(dirName)//getFileSep()//trim(fName)
            end if
          else
            fullPath = trim(getSaveDirectory())//fName
          end if

          if (present(fullPathOut)) fullPathOut = trim(fullPath)

          ! PRINT (not LogTermFOR): absolute paths must stay out of captured
          ! test output; unit 6 is suppressed in the test runner.
          print *, 'Full Path is '//trim(fullPath)
          ! First check if file exists
          if (doesFileExist(trim(fullPath))) then
            if(present(overwriteFlag)) then
              ! write file and return
              open(unit=fID, iostat=stat, file=trim(fullPath), &
              & status='old', action="write")
              if (stat /= 0) fID=0 ! Error
              return
            end if
              
            ! Ask user if they want to overwrite.  In headless mode default to
            ! yes without asking -- creating the GTK dialog with no display
            ! segfaulted (any headless SAV over an existing file crashed).
            ! (zoa_ui_callbacks%query_yes_no would be cleaner but creates a
            ! module cycle: zoa_ui_callbacks -> plot_setting_manager ->
            ! mod_lens_data_manager -> zoa_file_handler.)
            if (HEADLESS_MODE) then
              ! Silent (no "Clearing file" log): the message would make test
              ! output depend on whether the save file already exists.
              call delete_file(trim(fullPath))
            else
              msg(1) = "Do you want to overwrite"
              msg(2) = fName//" ?"
              resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
              & "Warning"//c_null_char)
              if (resp == GTK_RESPONSE_YES) then
                call LogTermFOR("Clearing file")
                call delete_file(trim(fullPath))
              else
                call LogTermFOR("Save cancelled to avoid overwriting previous file.")
              end if
            end if
          end if
          
          
          open(unit=fID, iostat=stat, file=trim(fullPath), &
          & status='new', action="write")
          if (stat /= 0) fID=0 ! Error
          

        end function

        ! Companion .zin path for a .zoa lens path (or vice versa): same
        ! directory/basename, extension swapped. If zoaPath has no extension
        ! (no '.' after the last path separator), the extension is appended.
        function zinPathFromZoa(zoaPath) result(zinPath)
          implicit none
          character(len=*), intent(in) :: zoaPath
          character(len=2048) :: zinPath
          integer :: dotLoc, sepLoc

          dotLoc = index(zoaPath, '.', BACK=.TRUE.)
          sepLoc = index(zoaPath, getFileSep(), BACK=.TRUE.)

          if (dotLoc > sepLoc) then
            zinPath = zoaPath(1:dotLoc)//'zin'
          else
            zinPath = trim(zoaPath)//'.zin'
          end if
        end function

        function zoaPathFromZin(zinPath) result(zoaPath)
          implicit none
          character(len=*), intent(in) :: zinPath
          character(len=2048) :: zoaPath
          integer :: dotLoc, sepLoc

          dotLoc = index(zinPath, '.', BACK=.TRUE.)
          sepLoc = index(zinPath, getFileSep(), BACK=.TRUE.)

          if (dotLoc > sepLoc) then
            zoaPath = zinPath(1:dotLoc)//'zoa'
          else
            zoaPath = trim(zinPath)//'.zoa'
          end if
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

      subroutine process_zoa_file(fileName, printOnly)
        use GLOBALS, only: HEADLESS_MODE
        use zoa_output, only: zoa_invoke_replot_flush
        implicit none
        character(len=*) :: fileName
        logical, optional :: printOnly
        integer :: n, ios, iu
        character(len=1024) :: line

        open(newunit=iu, file=trim(fileName), iostat=ios)
        if ( ios /= 0 ) stop "Error opening file "

        zoa_file_depth = zoa_file_depth + 1
        n = 0

        do
            read(iu, '(A)', iostat=ios) line
            if (ios /= 0) then
              close(iu)
              zoa_file_depth = zoa_file_depth - 1
              return
            end if

            ! Strip inline ! comments, respecting single-quoted strings.
            ! e.g.  TIT 'OSDlens'  ! note title in quotes  →  TIT 'OSDlens'
            block
              integer :: k
              logical :: inQuote
              inQuote = .FALSE.
              do k = 1, len_trim(line)
                if (line(k:k) == "'") inQuote = .not. inQuote
                if (line(k:k) == '!' .and. .not. inQuote) then
                  line = line(1:k-1)
                  exit
                end if
              end do
            end block

            if (len_trim(line) == 0) cycle

            if (present(printOnly)) then
              call LogTermFOR(trim(line))
            else
              call PROCESKDP(trim(line))
            end if
            n = n + 1

            ! After each processed line at top level, drain any pending replot so
            ! that lens-modifying commands record an undo snapshot before the next
            ! command runs.  Gated on HEADLESS_MODE so the GUI load path is unchanged.
            if (zoa_file_depth == 1 .and. HEADLESS_MODE) call zoa_invoke_replot_flush()
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
        use DATMAI
        implicit none


        integer :: fileID
        integer :: recLength
        character(len=8) :: MNAME
        character(len=20)  :: STAMP
          INTEGER :: M1, M2, M3


        inquire(iolength=recLength) MNAME, M1, M2, M3, STAMP

        fileID = 20

        OPEN(UNIT=fileID,ACCESS='DIRECT', &
     &    FILE=trim(LIBMAC)//'MAC.DAT',FORM= &
     &    'UNFORMATTED',RECL=recLength,STATUS='UNKNOWN')

      end function

      function open_macro_file(fileName) result(fileID)
        ! Binary macro system removed - this function is dead code
        implicit none
        integer :: fileID
        character(len=*) :: fileName
        fileID = -1
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
  character(len=1024) :: res

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
  implicit none
  character(len=250) :: res

  res = trim(savresDir)


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


function getTempDirectory() result(outDir)
  character(len=1024) :: outDir

  outDir = tempDir

end function

! Return the filename (not full path) for the current-lens autosave.
! In headless/test mode use a separate file so that running tests does
! not overwrite the user's interactive session state.
function getCurrentLensFileName() result(fname)
  use GLOBALS, only: HEADLESS_MODE
  character(len=32) :: fname
  if (HEADLESS_MODE) then
    fname = 'templens.zoa'
  else
    fname = 'currlens.zoa'
  end if
end function

function getFileNameFromPath(fileName) result(res)
  implicit none
  character(len=*) :: fileName
  character(len=1500) :: res
  integer :: slashLoc

  slashLoc = index(fileName, getFileSep(), BACK=.TRUE.)

  res = fileName(slashLoc+1:len(fileName))

end function

function getMacroDir() result (outDir)

  character(len=1024) :: outDir

  outDir = macroDir

end function

function getPermMacroDir() result(permDir)
  character(len=500) :: permDir
  permDir = trim(getZoaPath())//'PERMAC'//getFileSep()

end function

! =========================================================================
! Directory setters — used by the Preferences dialog
! =========================================================================

subroutine setMacroDir(dir)
  character(len=*), intent(in) :: dir
  macroDir = trim(addFileSepIfNeeded(dir))
end subroutine

subroutine setTempDir(dir)
  character(len=*), intent(in) :: dir
  tempDir = trim(addFileSepIfNeeded(dir))
end subroutine

subroutine setProjectDir(dir)
  character(len=*), intent(in) :: dir
  call setSaveDirectory(dir)
  savresDir = currSaveDir
end subroutine

subroutine setGlassCatalogDir(dir)
  character(len=*), intent(in) :: dir
  glassCatalogDirOverride = trim(addFileSepIfNeeded(dir))
end subroutine

function getGlassCatalogDir() result(res)
  use DATMAI
  character(len=1024) :: res
  if (len_trim(glassCatalogDirOverride) > 0) then
    res = glassCatalogDirOverride
  else
    res = LIBGLA
  end if
end function

! Apply the glass catalog override to LIBGLA after INITKDP has run.
! INITKDP unconditionally sets LIBGLA from basePath, so we override
! it here if the user has set a custom glass catalog directory.
subroutine applyGlassCatalogDirFromPrefs()
  use DATMAI
  if (len_trim(glassCatalogDirOverride) > 0) then
    LIBGLA = glassCatalogDirOverride
  end if
end subroutine

! Check whether a directory path exists (INQUIRE works for directories
! on macOS, Linux, and Windows when not trailing-slash-terminated).
function doesDirectoryExist(dirPath) result(res)
  character(len=*), intent(in) :: dirPath
  logical :: res
  character(len=len(dirPath)) :: testPath
  integer :: k
  ! Strip trailing separator for INQUIRE compatibility
  testPath = trim(dirPath)
  k = len_trim(testPath)
  if (k > 1 .and. (testPath(k:k) == '/' .or. testPath(k:k) == '\')) then
    testPath = testPath(1:k-1)
  end if
  INQUIRE(FILE=trim(testPath), EXIST=res)
end function

! =========================================================================
! Search path (see searchDirs above)
! =========================================================================

! Add a folder (no-op if already listed).  ok is false when the list is full.
subroutine addSearchDir(dir, origin, ok)
  character(len=*), intent(in) :: dir
  integer, intent(in) :: origin
  logical, intent(out), optional :: ok
  character(len=1024) :: d
  integer :: i
  if (present(ok)) ok = .true.
  if (len_trim(dir) == 0) return
  d = trim(addFileSepIfNeeded(trim(adjustl(dir))))
  do i = 1, nSearchDirs
    if (trim(searchDirs(i)) == trim(d)) return
  end do
  if (nSearchDirs >= MAX_SEARCH_DIRS) then
    if (present(ok)) ok = .false.
    return
  end if
  nSearchDirs = nSearchDirs + 1
  searchDirs(nSearchDirs) = d
  searchOrigin(nSearchDirs) = origin
end subroutine

! Remove entry i (1-based); ok is false when there is no such entry.
subroutine removeSearchDir(i, ok)
  integer, intent(in) :: i
  logical, intent(out) :: ok
  ok = (i >= 1 .and. i <= nSearchDirs)
  if (.not. ok) return
  searchDirs(i:nSearchDirs-1) = searchDirs(i+1:nSearchDirs)
  searchOrigin(i:nSearchDirs-1) = searchOrigin(i+1:nSearchDirs)
  searchDirs(nSearchDirs) = ''
  searchOrigin(nSearchDirs) = 0
  nSearchDirs = nSearchDirs - 1
end subroutine

! The 1-based index of dir in the list, or 0.
function searchDirIndex(dir) result(idx)
  character(len=*), intent(in) :: dir
  integer :: idx, i
  character(len=1024) :: d
  idx = 0
  if (len_trim(dir) == 0) return
  d = trim(addFileSepIfNeeded(trim(adjustl(dir))))
  do i = 1, nSearchDirs
    if (trim(searchDirs(i)) == trim(d)) then
      idx = i
      return
    end if
  end do
end function

! Remove every entry of the given origin.
subroutine clearSearchDirs(origin)
  integer, intent(in) :: origin
  integer :: i
  logical :: ok
  i = 1
  do while (i <= nSearchDirs)
    if (searchOrigin(i) == origin) then
      call removeSearchDir(i, ok)
    else
      i = i + 1
    end if
  end do
end subroutine

! ZOA_SEARCH_PATH: folders separated by ';' (any platform) or, except on
! Windows where ':' follows a drive letter, ':'.
subroutine loadSearchPathFromEnv()
  character(len=4096) :: env
  integer :: stat, i, start
  logical :: sep
  call get_environment_variable('ZOA_SEARCH_PATH', env, status=stat)
  if (stat /= 0 .or. len_trim(env) == 0) return
  start = 1
  do i = 1, len_trim(env) + 1
    if (i > len_trim(env)) then
      sep = .true.
    else
      sep = env(i:i) == ';'
#ifndef WINDOWS
      sep = sep .or. env(i:i) == ':'
#endif
    end if
    if (sep) then
      if (i > start) call addSearchDir(env(start:i-1), SP_ENV)
      start = i + 1
    end if
  end do
end subroutine

! Locate a data file of one kind ('Projects', 'Macros' or 'CodeV'): first in
! the folder that kind normally lives in (the project, macro or CODE V
! folder), then in each search-path folder -- in its kind subfolder, then
! directly.  name may contain subfolders ('Bentley/Bentley4p2.zoa'), with
! either separator.  Returns '' when the file is nowhere.
function findDataFile(kind, name) result(path)
  character(len=*), intent(in) :: kind, name
  character(len=1024) :: path
  character(len=1024) :: nm, home
  character(len=16) :: sub(2)
  integer :: i, k, nsub

  nm = adjustl(name)
  do i = 1, len_trim(nm)
    if (nm(i:i) == '/' .or. nm(i:i) == '\') nm(i:i) = getFileSep()
  end do

  select case (kind)
  case ('Projects')
    home = savresDir
    sub(1) = 'Projects'; nsub = 1
  case ('Macros')
    home = macroDir
    sub(1) = 'Macros'; sub(2) = 'MACROS'; nsub = 2
  case ('CodeV')
    home = trim(addFileSepIfNeeded(trim(codevdir)))
    sub(1) = 'CodeV'; nsub = 1
  case default
    home = ''
    nsub = 0
  end select

  path = trim(home)//trim(nm)
  if (len_trim(home) > 0 .and. doesFileExist(trim(path))) return

  do i = 1, nSearchDirs
    do k = 1, nsub
      path = trim(searchDirs(i))//trim(sub(k))//getFileSep()//trim(nm)
      if (doesFileExist(trim(path))) return
    end do
    path = trim(searchDirs(i))//trim(nm)
    if (doesFileExist(trim(path))) return
  end do
  path = ''
end function

! =========================================================================
! Preferences persistence — simple key=value text file in basePath
! =========================================================================

subroutine savePreferences()
  use GLOBALS, only: basePath, zoa_threads
  implicit none
  character(len=1024) :: filePath
  integer :: funit, i

  ! A test run (isolated) never writes the user's preferences.
  if (preferencesIsolated) return
  filePath = trim(basePath)//'preferences.ini'
  funit = 99
  open(unit=funit, file=trim(filePath), status='replace', action='write', form='formatted')
  write(funit, '(A)') 'ProjectDir='//trim(savresDir)
  write(funit, '(A)') 'TempDir='//trim(tempDir)
  write(funit, '(A)') 'MacroDir='//trim(macroDir)
  write(funit, '(A)') 'GlassDir='//trim(glassCatalogDirOverride)
  write(funit, '(A,I0)') 'Threads=', zoa_threads
  do i = 1, nSearchDirs
    if (searchOrigin(i) == SP_PREF) write(funit, '(A)') 'SearchPath='//trim(searchDirs(i))
  end do
  close(funit)
end subroutine

subroutine loadPreferences()
  use GLOBALS, only: basePath, HEADLESS_MODE, zoa_threads
  implicit none
  character(len=1024) :: filePath, line, key, val
  integer :: funit, ios, eq, nthr

  if (preferencesIsolated) return
  filePath = trim(basePath)//'preferences.ini'
  funit = 98
  open(unit=funit, file=trim(filePath), status='old', action='read', &
       form='formatted', iostat=ios)
  if (ios /= 0) return   ! no file yet — use defaults

  do
    read(funit, '(A)', iostat=ios) line
    if (ios /= 0) exit
    line = adjustl(line)
    if (len_trim(line) == 0) cycle
    eq = index(line, '=')
    if (eq < 2) cycle
    key = line(1:eq-1)
    val = line(eq+1:len_trim(line))
    select case (trim(key))
    case ('ProjectDir')
      if (len_trim(val) > 0) then
        ! Migrate the old generated default without rewriting custom/UNC paths.
        if (trim(val) == trim(basePath)//getFileSep()//'Projects'//getFileSep()) &
          val = trim(basePath)//'Projects'//getFileSep()
        savresDir   = trim(val)
        currSaveDir = trim(val)
      end if
    case ('TempDir')
      if (len_trim(val) > 0) tempDir = trim(val)
    case ('MacroDir')
      if (len_trim(val) > 0) macroDir = trim(val)
    case ('GlassDir')
      glassCatalogDirOverride = trim(val)
    case ('SearchPath')
      call addSearchDir(trim(val), SP_PREF)
    case ('Threads')
      ! The headless runner (tests, zoa_server) ignores the user's thread
      ! preference so results never depend on it; THREADS sets it there.
      if (.not. HEADLESS_MODE) then
        read(val, *, iostat=ios) nthr
        if (ios == 0 .and. nthr >= 0) zoa_threads = nthr
      end if
    end select
  end do

  close(funit)
end subroutine

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

subroutine getListofFilesInDirectory(dirName, extension, outList, iL)

  use g
  use gtk
  use iso_c_binding
  use gtk_sup
  use strings

  implicit none

  character(len=*), intent(in) :: dirName, extension
  character(len=*), dimension(:), intent(inout) :: outList
  integer :: iL ! Number of items in array

  type(c_ptr) :: dir, fptr
  integer(c_int) :: dirTest
  character(len=200) :: fpath
  logical :: boolLoop
  integer :: extLoc


  dir = g_dir_open(dirName//c_null_char, 0_c_int, c_null_ptr)
  iL = 0
  boolLoop = .TRUE.
  do while (boolLoop)
    fptr = g_dir_read_name(dir)
    if (.not.c_associated(fptr)) then 
      boolLoop = .FALSE.
    else
      call convert_c_string(fptr, fpath)
      !print *, "fullpath is ", trim(pwd)//'/'//fpath
      dirTest = g_file_test(dirName//getFileSep()//trim(fpath)//getFileSep(), G_FILE_TEST_IS_DIR)
      !print *, "dirTest is ", dirTest
      if (dirTest == 0) then
        extLoc = index(trim(fpath), extension, .TRUE.)
        print *, "Len of trim(fpath) is ", len_trim(fpath)
        print *, "Len of extension is ", len(extension)
        if (len_trim(fpath)-extLoc + 1 == len(extension)) then ! Finally found what we are looking for!
          iL = iL+1
          outList(iL:iL) = trim(fpath)
        print *, "File is ", trim(fpath)
        end if 
      end if
    end if   
    
  end do  


end subroutine

! Like getListofFilesInDirectory but also recurses one level into subdirectories.
! Files in subdirectories are returned as "subdir/filename.ext".
! iL must be initialised to 0 by the caller before the first (non-recursive) call.
recursive subroutine getListofFilesInDirectoryRecursive(dirName, extension, outList, iL, prefix)

  use g
  use gtk
  use iso_c_binding
  use gtk_sup
  use strings

  implicit none

  character(len=*), intent(in)                   :: dirName, extension
  character(len=*), dimension(:), intent(inout)  :: outList
  integer,                        intent(inout)  :: iL
  character(len=*), intent(in),   optional       :: prefix

  type(c_ptr)          :: dir, fptr
  integer(c_int)       :: dirTest
  character(len=200)   :: fpath
  character(len=1024)  :: subDirPath, entryPrefix
  logical              :: boolLoop
  integer              :: extLoc

  dir = g_dir_open(trim(dirName)//c_null_char, 0_c_int, c_null_ptr)
  if (.not. c_associated(dir)) return

  boolLoop = .TRUE.
  do while (boolLoop)
    fptr = g_dir_read_name(dir)
    if (.not. c_associated(fptr)) then
      boolLoop = .FALSE.
    else
      call convert_c_string(fptr, fpath)
      ! Test whether this entry is itself a directory
      dirTest = g_file_test(trim(dirName)//getFileSep()//trim(fpath)//getFileSep(), &
                            G_FILE_TEST_IS_DIR)
      if (dirTest /= 0) then
        ! It is a directory — recurse into it with an updated prefix
        subDirPath  = trim(dirName)//getFileSep()//trim(fpath)
        if (present(prefix)) then
          entryPrefix = trim(prefix)//trim(fpath)//getFileSep()
        else
          entryPrefix = trim(fpath)//getFileSep()
        end if
        call getListofFilesInDirectoryRecursive(trim(subDirPath), extension, &
                                                outList, iL, prefix=trim(entryPrefix))
      else
        ! It is a file — add it if the extension matches
        extLoc = index(trim(fpath), extension, .TRUE.)
        if (len_trim(fpath) - extLoc + 1 == len(extension)) then
          iL = iL + 1
          if (present(prefix)) then
            outList(iL:iL) = trim(prefix)//trim(fpath)
          else
            outList(iL:iL) = trim(fpath)
          end if
        end if
      end if
    end if
  end do

  call g_dir_close(dir)

end subroutine

!Prototype func to eventually translate to list all files
!in a dir with a specific extension
subroutine printFilesInCurrentDirectory()

  use g
  use gtk
  use iso_c_binding
  use gtk_sup

  implicit none

  type(c_ptr) :: dir, fptr
  integer(c_int) :: dirTest
  character(len=200) :: fpath
  character(len=1024) :: pwd
  logical :: boolLoop

  dir = g_dir_open(".", 0_c_int, c_null_ptr)
  boolLoop = .TRUE.
  call get_environment_variable("PWD", pwd)
  print *, "PWD is", pwd
  do while (boolLoop)
    fptr = g_dir_read_name(dir)
    if (.not.c_associated(fptr)) then 
      boolLoop = .FALSE.
    else
      call convert_c_string(fptr, fpath)
      !print *, "fullpath is ", trim(pwd)//'/'//fpath
      dirTest = g_file_test(trim(pwd)//getFileSep()//trim(fpath)//getFileSep(), G_FILE_TEST_IS_DIR)
      !print *, "dirTest is ", dirTest
      if (dirTest == 0) then
        print *, "File is ", trim(fpath) 
      end if
    end if   
    
  end do
  
end subroutine

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
