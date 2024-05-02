! The purpose of this is to have a baseline with a bunch of commands being run
! to catch issues during development
subroutine TestCommands
  use GLOBALS
  use kdp_utils, only: OUTKDP
  use zoa_file_handler, only: delete_file, getZoaPath, doesFileExist
  implicit none
  !include "DATMAI.INC"

  TEST_MODE = .TRUE.

  ! Delete test output.  This needs to match  the file in the dumpto file.
  if (doesFileExist(trim(getZoaPath())//'testOutput.txt')) then
     call delete_file(trim(getZoaPath())//'testOutput.txt')
  end if


  ! if (WQ.EQ.'MANGIN') then
  !   CALL PROCESKDP("MAB3 ALL")
  !   CALL PROCESKDP('MAB3')
  !   CALL PROCESKDP('MAB3 I 2')
  !   CALL PROCESKDP('MAB3 2')
  ! else
  CALL PROCESKDP('res OSDasdoubletaxis')
  CALL PROCESKDP('FIR')


  ! CALL PROCESKDP('MFL')
  ! CALL PROCESKDP('LIB P')
  ! CALL PROCESKDP('GLASSP')
  ! CALL PROCESKDP('GLASSP SCHOTT')

  ! CALL PROCESKDP('CV2PRG LithoKotaro.seq')
  ! CALL PROCESKDP('FOB 0')
  ! CALL PROCESKDP('RAY 1 0')
  ! CALL PROCESKDP('CAPFN')
  ! CALL PROCESKDP('FITZERN')
  ! CALL PROCESKDP('LISTZERN')

  ! ! Lens Library
  ! CALL PROCESKDP('LSTAT')
  ! CALL PROCESKDP('LIB GET 5')
  ! CALL PROCESKDP('LIB GET 2')

  !end if

  !CALL PROCESSKDP('SHO RMSOPD')



  !CALL PROCESKDP('AST 0')
  !CALL PROCESKDP('PLTAST')
  !CALL PROCESKDP('POWSYM')


! MACRODMP

end subroutine

subroutine dumpToFile(iptStr)
  use zoa_file_handler 
  use GLOBALS
  implicit none
  character(len=*) :: iptStr
  character(len=256) :: dbgFileName
  character(len=512) :: tmpFile
  logical itsopen 
  
  dbgFileName = trim(getZoaPath())//'testOutput.txt'
  !PRINT *, "dbgFileName is ", dbgFileName

  inquire(unit=3578, opened=itsopen) 
  if (itsopen) then
    close(3578, status='KEEP')
  end if

    
    open(unit=3578,file=trim(dbgFileName),position='append',form='formatted')
    write(3578, *) iptStr 
    close(3578, status='KEEP')

    
  ! if (itsopen.eqv..FALSE. ) then 
  !   !call LogTermFOR("Opening Test Output (so meta)")
  !   if (doesFileExist(trim(dbgFileName))) then
  !     call delete_file(trim(dbgFileName))
  !   end if

  !   open(unit=3578,file=trim(dbgFileName), status='new', action="write")
  !         ! First check if file exists

  ! end if

  ! !call LogTermFOR("Writing Output String"//iptStr)\
  ! !write(tmpFile, *), trim(dbgFileName)
  ! PRINT *, 'FIle Name is ', trim(dbgFileName)
  ! !PRINT *, "Am I crazy?"
  ! PRINT *, 'About to writev', iptStr
  ! write(3578,*), trim(iptStr)

  ! self%recNo = self%recNo + 1
  ! inquire(unit=self%fileID, opened=fileOpen)
  ! inquire(iolength=recL) logTxt
  ! close(self%fileID, status='KEEP')
  ! open(unit=self%fileID,file=self%logFileName,position='append',form='formatted')
  ! write(self%fileID, *) logTxt

end subroutine