module zoa_logger

type zoaLogger

  character(len=255)  :: basePath
  character(len=355)  :: logFileName
  integer             :: fileID
  integer             :: recNo
  ! The log file is connected on its own unit (NEWUNIT, negative) for the
  ! logger's lifetime; isOpen replaces the old "fileID <= 0" guard.
  logical             :: isOpen = .false.


contains
  procedure, public :: logText
  procedure, public :: logTextWithReal
  procedure, public :: logTextWithNum
  procedure, public :: logTextWithInt
  procedure, private :: writeLogToDisk

  FINAL :: closeLogFile
end type


interface zoaLogger
  module procedure :: zoaLogger_constructor
end interface


contains

type(zoaLogger) function zoaLogger_constructor(path) result(self)


    character(len=255) :: path
    integer :: ios
    self%basePath = path
    self%logFileName = trim(self%basePath)//'zoalogger.log'
    self%recNo = 0
    PRINT *, "Create Logger here: ", self%logFileName
    ! NEWUNIT: a fixed unit (it was 1) is shared with legacy code -- BMPJN
    ! opens and closes unit 1 -- which would close or redirect the log.
    open(newunit=self%fileID, access='sequential', file=self%logFileName, &
         status='replace', form='formatted', iostat=ios)
    self%isOpen = (ios == 0)

end function

subroutine writeLogToDisk(self, logTxt)
  class(zoaLogger) :: self
  character(len=*), intent(in) :: logTxt
  logical :: fileOpen
  integer :: ios

  if (.not. self%isOpen) return

  self%recNo = self%recNo + 1
  ! The file stays connected; flush puts each record on disk at once (what
  ! the old close + reopen-to-append per record achieved).  That reopen cost
  ! a file open and close per record -- cheap on macOS, but ~7 ms each on
  ! Windows, where the optimizer's thousands of logged commands made
  ! optim_extra take 95-135 s instead of 7-9 s.  Reconnect only if the unit
  ! was somehow closed.
  inquire(unit=self%fileID, opened=fileOpen)
  if (.not. fileOpen) then
    open(newunit=self%fileID, file=self%logFileName, position='append', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
      self%isOpen = .false.
      return
    end if
  end if
  write(self%fileID, *) logTxt
  flush(self%fileID)

  PRINT *, logTxt

end subroutine

subroutine logTextWithNum(self, logTxt, value)
    use ISO_FORTRAN_ENV
    class(zoaLogger) :: self
    character(len=*), intent(in) :: logTxt
    class(*), intent(in) :: value

    select type(value)
    type is (real(real64))
      call self%logTextWithReal(logTxt, value)
    type is (integer)
      call self%logTextWithInt(logTxt, value)
    end select


  end subroutine

subroutine logTextWithInt(self, logTxt, value)
    class(zoaLogger) :: self
    character(len=*), intent(in) :: logTxt
    integer, intent(in) :: value
    character(len=140) :: tmpTxt

    WRITE(tmpTxt, *) logTxt, value

    call self%writeLogToDisk(tmpTxt)

end subroutine

subroutine logTextWithReal(self, logTxt, realVar)
    class(zoaLogger) :: self
    character(len=*), intent(in) :: logTxt
    real*8, intent(in) :: realVar

    character(len=140) :: tmpTxt

    WRITE(tmpTxt, *) logTxt, realVar

    call self%writeLogToDisk(tmpTxt)


end subroutine


subroutine logText(self, logTxt)
  class(zoaLogger) :: self
  character(len=*), intent(in) :: logTxt

  call self%writeLogToDisk(logTxt)

end subroutine

 subroutine closeLogFile(self)
  type(zoaLogger) :: self
  PRINT *, "Zoa Logger Destructor Being Called!"
  ! Never close here: "logger = zoaLogger(path)" finalizes the constructor's
  ! result, which shares the unit with the logger being assigned.
  !close(self%fileID)

end subroutine

end module
