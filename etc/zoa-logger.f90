module zoa_logger

type zoaLogger

  character(len=255)  :: basePath
  character(len=355)  :: logFileName
  integer             :: fileID


contains
  procedure, public :: logText
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

    open(unit=self%fileID,file=self%logFileName,status='replace',form='formatted')

end function

subroutine logText(self, logTxt)
  class(zoaLogger) :: self
  character(len=*) :: logTxt

  write(self%fileID, *) logTxt
  PRINT *, logTxt

end subroutine

 subroutine closeLogFile(self) 
  type(zoaLogger) :: self
  PRINT *, "Zoa Logger Destructor Being Called!"
  close(self%fileID)

end subroutine

end module
