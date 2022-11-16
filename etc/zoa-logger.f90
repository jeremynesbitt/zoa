module zoa_logger

type zoaLogger

  character(len=255)  :: basePath
  character(len=355)  :: logFileName
  integer             :: fileID
  integer             :: recNo


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
    self%recNo = 0
    PRINT *, "Create Logger here: ", self%logFileName
    open(unit=self%fileID,access='sequential',file=self%logFileName,status='replace',form='formatted')

end function

subroutine logText(self, logTxt)
  class(zoaLogger) :: self
  character(len=*), intent(in) :: logTxt
  logical :: fileOpen
  integer :: recL

  self%recNo = self%recNo + 1
  inquire(unit=self%fileID, opened=fileOpen)
  inquire(iolength=recL) logTxt
  close(self%fileID, status='KEEP')
  !open(unit=self%fileID,file=self%logFileName,access='append',recl=recL,form='formatted')
    open(unit=self%fileID,file=self%logFileName,access='append',form='formatted')
     !    OPEN(UNIT=fileID,ACCESS='DIRECT',FILE=filePath,
     ! 1  FORM='UNFORMATTED',RECL=rec11,STATUS='UNKNOWN')
     !    READ(UNIT=fileID,REC=1) TOTAL
     !    CLOSE(UNIT=fileID, STATUS='KEEP')
  !write(self%fileID, rec=self%recNo) logTxt
  write(self%fileID, *) logTxt
  !PRINT *, "LOG OPEN FLAG IS ", fileOpen
  PRINT *, "LOGTXT IS ", logTxt


end subroutine

 subroutine closeLogFile(self)
  type(zoaLogger) :: self
  PRINT *, "Zoa Logger Destructor Being Called!"
  !close(self%fileID)

end subroutine

end module
