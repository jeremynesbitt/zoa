module zoa_logger

type zoaLogger

  character(len=255)  :: basePath
  character(len=355)  :: logFileName
  integer             :: fileID
  integer             :: recNo


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
    self%basePath = path
    self%logFileName = trim(self%basePath)//'zoalogger.log'
    self%fileID = 1
    self%recNo = 0
    PRINT *, "Create Logger here: ", self%logFileName
    open(unit=self%fileID,access='sequential',file=self%logFileName,status='replace',form='formatted')

end function

subroutine writeLogToDisk(self, logTxt)
  class(zoaLogger) :: self
  character(len=*), intent(in) :: logTxt
  logical :: fileOpen
  integer :: recL

  self%recNo = self%recNo + 1
  inquire(unit=self%fileID, opened=fileOpen)
  inquire(iolength=recL) logTxt
  close(self%fileID, status='KEEP')
  open(unit=self%fileID,file=self%logFileName,position='append',form='formatted')
  write(self%fileID, *) logTxt

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
  !close(self%fileID)

end subroutine

end module
