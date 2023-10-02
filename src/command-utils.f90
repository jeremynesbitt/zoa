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

  function checkCommandInput(ID_CMD_TYPE, qual_words, qual_only_err_msg, max_num_terms) result (goodInput)
    ! qual_words - character array of words that are acceptable for this command
    !  EG for LIB qual words are PUT and GET
    ! qual_only_err_msg - override error message when input error is encountered
    !use handlers, only: updateTerminalLog
    use kdp_utils

    implicit none
    integer, intent(in) :: ID_CMD_TYPE(:) ! only required argument
    integer, optional :: max_num_terms 
    character(len=*), optional :: qual_words(:)
    character(len=*), optional :: qual_only_err_msg
    type(command_parser) :: cp
    logical :: goodInput
    integer :: i, numValidTypes, cmdToProcess

    include "DATMAI.INC"

    goodInput = .TRUE. ! Innocent until proven guilty

    if (size(ID_CMD_TYPE) > 1) then
      ! Some commands support multiple types.  But only one type can exist at a time.  So check if two types aren't mixed
      numValidTypes = 0
      do i=1,size(ID_CMD_TYPE)
        if (isCmdType(ID_CMD_TYPE(i))) then 
           numValidTypes = numValidTypes + 1
           cmdToProcess = ID_CMD_TYPE(i)
        end if
      end do
      ! TODO:  Need to add a flag to tell this fcn that some input is required before enabling this.
      ! if (numValidTypes == 0)
      !    goodInput = .FALSE.
      !    CALL OUTKDP("Error:  No Valid Inpus for command " //INPUT)

      ! end if
      if (numValidTypes > 1) then
        goodInput = .FALSE.
        ! TODO:  Add some 
        call OUTKDP("Error:  Mixing of command types.  Please enter command again")
        return
      end if
    else
      cmdToProcess = ID_CMD_TYPE(1)
    end if


    select case (cmdToProcess)
    case (ID_CMD_ONLY)

    case (ID_CMD_QUAL)
      numValidTypes = 0
      if (present(qual_words)) then 
        do i=1,size(qual_words)
          if (trim(qual_words(i)).EQ.WQ) numValidTypes = numValidTypes+1

        end do
        if (numValidTypes.EQ.0) then 
          goodInput = .FALSE.
          if((present(qual_only_err_msg))) then
            call OUTKDP(trim(qual_only_err_msg))
          else
            call OUTKDP("No valid qualifier words found.")
            call OUTKDP("Please reenter command")
          end if
          RETURN
        end if

      end if
    
    case (ID_CMD_NUM)
      if (present(max_num_terms)) then
       
      select case (max_num_terms) 

      case (1)
        if(SST.EQ.1.OR.S2.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call OUTKDP("Command takes no string or > 1 numeric input ")
          RETURN        
        end if

      case (2)
        if(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call OUTKDP("Command takes no string or > 2 numeric input ")
          RETURN        
        end if

      case (3)
        if(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call OUTKDP("Command takes no string or > 3 numeric input ")
          RETURN        
        end if  
      case (4)
        if(SST.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call OUTKDP("Command takes no string or > 4 numeric input ")
          RETURN        
        end if   
      end select
    end if             
    case (ID_CMD_ALPHA)



    case default


    end select


  end function

  function isCmdType(ID_CMD_TYPE) result(typeExists)

    implicit none
    logical :: typeExists
    integer :: ID_CMD_TYPE

    include "DATMAI.INC"

    typeExists = .FALSE.
    select case (ID_CMD_TYPE)

    case (ID_CMD_ONLY)

    case (ID_CMD_QUAL)
      IF(SQ.EQ.1) typeExists = .TRUE.

    case (ID_CMD_ALPHA)

    case (ID_CMD_NUM)
      IF (S1.EQ.1) typeExists = .TRUE.



    case default
      typeExists = .FALSE.

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
