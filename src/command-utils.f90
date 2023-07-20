! Goal of this module is some simple refactoring of the command
! input verification that is in many of the KDP modules.  Hope to
! improve code readability and make it easier to provide input checking for
! new commands
! JN 6/25/23
!
! Cheat sheet how KDP saves parsed command input
! WS - Alphanumeric string (len=80)

module command_utils


  integer, parameter :: ID_CMD_ONLY = 1
  integer, parameter :: ID_CMD_QUAL = 2
  integer, parameter :: ID_CMD_ALPHA = 3
  integer, parameter :: ID_CMD_QUAL_ALPHA = 4
  integer, parameter :: ID_CMD_NUM = 5
  integer, parameter :: ID_CMD_NUM_ALPHA = 6
  integer, parameter :: ID_CMD_QUAL_NUM = 7
  integer, parameter :: ID_CMD_QUAL_NUM_ALPHA = 8
  integer, parameter :: ID_CMD_HELP = 9
  integer, parameter :: ID_CMD_QUAL_HELP = 10

  integer, parameter :: ID_NUM_CMD_INPUT_TYPES = 10

  ! Mimic existing structure for now; use methods for improved readability
  type command_parser
    !logical :: SST
    !character(len=80) :: WS

  contains
    procedure :: hasAlphaNumericInput
  !  procedure :: hasQualifierWord

  end type

contains

  function hasAlphaNumericInput(self) result(flag)
    implicit none
    class(command_parser) :: self
    include "DATMAI.INC"
    logical :: flag


  end function

  function checkCommandInput(ID_CMD_TYPE, qual_words, qual_only_err_msg) result (goodInput)
    ! qual_words - character array of words that are acceptable for this command
    !  EG for LIB qual words are PUT and GET
    ! qual_only_err_msg - override error message when input error is encountered
    !use handlers, only: updateTerminalLog

    implicit none
    integer, intent(in) :: ID_CMD_TYPE ! only required argument
    character(len=*), optional :: qual_words(:)
    character(len=*), optional :: qual_only_err_msg
    type(command_parser) :: cp
    logical :: goodInput

    include "DATMAI.INC"

    select case (ID_CMD_TYPE)
    case (ID_CMD_ONLY)

    case (ID_CMD_QUAL)

    case (ID_CMD_ALPHA)



    case default


    end select


  end function

  subroutine parseCommandIntoTokens(cmdInput, tokens, numTokens, tokenLen)
    implicit none
    character(len=*) :: cmdInput
    character(len=80) :: tokens(40)
    integer :: numTokens, fst, lst, i
    integer, optional  :: tokenLen(40)
    character(len=150) :: subString
  !call checkCommandInput(typeCode, allowableQualWords)
  ! Type:  QualWord+N_nums
  include "DATMAI.INC"

  PRINT *, "Alphanumeric string is ", cmdInput

  if (.not.present(tokenLen))  PRINT *, "Tokenlen doesn't exist!"

  !Test String Tokenizer
  subString = cmdInput
  fst = INDEX(subString, ' ', BACK=.FALSE.)
  lst = INDEX(subString, ' ', BACK=.TRUE.)
  i = 1
  PRINT *, "fst is ", fst
  PRINT *, "lst is ", lst
  do while (fst > 1)
     fst = INDEX(subString, ' ', BACK=.FALSE.)
     lst = INDEX(subString, ' ', BACK=.TRUE.)
       PRINT *, "fst is ", fst
       PRINT *, "lst is ", lst
     tokens(i) = subString(1:fst-1)
     PRINT *, "token is ", tokens(i)
     if(present(tokenLen)) tokenLen(i) = fst-1
     i = i+1
     if (fst<len(cmdInput)) subString = subString(fst+1:len(cmdInput))
  end do

  PRINT *, "tokens ", tokens(1:i-2)
  !if(present(tokenLen) PRINT *, "Token Length = ", tokenLen(1:i-2)
  numTokens = i-2

  end subroutine

  function cmdOptionExists(cmdTst) result(cmdExists)
    implicit none
    character(len=*) :: cmdTst
    logical :: cmdExists
    integer :: cmdLoc = 0

    include "DATMAI.INC"

    cmdLoc = index(WS, cmdTst)
    cmdExists = .FALSE.
    if (cmdLoc>0) cmdExists = .TRUE.

  end function

  function getCmdInputValue(cmdToGet) result(cmdVal)
    implicit none
    character(len=*) :: cmdToGet
    real :: cmdVal
    character(len=80) :: tokens(40)
    integer :: numTokens, fst,  i
  !call checkCommandInput(typeCode,
    include "DATMAI.INC"

    PRINT *, "About to Parse ", WS
    call parseCommandIntoTokens(trim(WS), tokens, numTokens)
    PRINT *, "After parseCommandIntoTokens"

    PRINT *, "cmdToGet is ", cmdToGet
    fst = 0
    do i=1,numTokens
      PRINT *, "Looking at token ", tokens(i)
      fst = index(tokens(i), cmdToGet)
      PRINT *, "fst is ", fst
      if (fst>0) then
        PRINT *, "Convert token to real ", tokens(i+1)
        read(tokens(i+1), *)  cmdVal
        return
      end if
    end do

  end function

  subroutine removeLeadingBlanks(strCand)
    implicit none
    character(len=*), intent(inout) :: strCand
    character(len=:), allocatable :: subString
    integer :: locBlank

    PRINT *, "Orig strCand is ", strCand
    subString = trim(strCand)
    locBlank = index(subString, " ")
    PRINT *, "locBlank is ", locBlank
    do while (locBlank.EQ.1)
      subString = subString(2:len(strCand))
      locBlank = index(subString, " ")
      PRINT *, "in loop locBlank is ", locBlank
    end do
    PRINT *, "out of loop locBlank is ", locBlank
    strCand = trim(subString)
    PRINT *, "Now strCand is ", strCand

  end subroutine



end module
