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

  type input_parsed_data

      real :: inputNums(5)
      integer :: maxNums
      character(len=10) :: inputQualWord


      contains
      procedure :: initialize => init_parsed_data
      

  end type

  type(input_parsed_data) :: currInputData

  ! Mimic existing structure for now; use methods for improved readability
  type command_parser
    !logical :: SST
    !character(len=80) :: WS



  contains
    procedure :: hasAlphaNumericInput
  !  procedure :: hasQualifierWord

  end type

contains

  subroutine report_error_and_fail(message, show_code)
    use DATMAI
    implicit none
    character(len=*), intent(in) :: message
    integer, intent(in) :: show_code
    integer :: start_pos, split_pos
    character(len=1) :: lf
    EXTERNAL SHOWIT
    EXTERNAL MACFAL

    lf = new_line('a')
    start_pos = 1

    do
      split_pos = find_message_break(message(start_pos:))
      if (split_pos == 0) then
        call show_message_line(message(start_pos:), show_code)
        exit
      end if

      call show_message_line(message(start_pos:start_pos + split_pos - 2), show_code)

      if (message(start_pos + split_pos - 1:start_pos + split_pos - 1) == lf) then
        start_pos = start_pos + split_pos
      else
        start_pos = start_pos + split_pos + 1
      end if

      if (start_pos > len(message)) then
        call show_message_line('', show_code)
        exit
      end if
    end do

    call MACFAL

  contains

    integer function find_message_break(text)
      character(len=*), intent(in) :: text
      integer :: actual_newline, slash_newline

      actual_newline = index(text, lf)
      slash_newline = index(text, '\n')

      if (actual_newline == 0) then
        find_message_break = slash_newline
      else if (slash_newline == 0) then
        find_message_break = actual_newline
      else
        find_message_break = min(actual_newline, slash_newline)
      end if
    end function

    subroutine show_message_line(line, code)
      character(len=*), intent(in) :: line
      integer, intent(in) :: code

      OUTLYNE = line
      call SHOWIT(code)
    end subroutine

  end subroutine

  logical function has_string_input()
    use DATMAI
    implicit none
    has_string_input = SST .EQ. 1
  end function

  logical function has_qualifier_input()
    use DATMAI
    implicit none
    has_qualifier_input = SQ .EQ. 1
  end function

  logical function is_command_query()
    use DATMAI
    implicit none
    is_command_query = STI .EQ. 1
  end function

  logical function has_numeric_input(word_number)
    use DATMAI
    implicit none
    integer, intent(in) :: word_number
    select case (word_number)
    case (1)
      has_numeric_input = S1 .EQ. 1
    case (2)
      has_numeric_input = S2 .EQ. 1
    case (3)
      has_numeric_input = S3 .EQ. 1
    case (4)
      has_numeric_input = S4 .EQ. 1
    case (5)
      has_numeric_input = S5 .EQ. 1
    case default
      has_numeric_input = .FALSE.
    end select
  end function

  logical function is_default_numeric(word_number)
    use DATMAI
    implicit none
    integer, intent(in) :: word_number
    select case (word_number)
    case (1)
      is_default_numeric = DF1 .EQ. 1
    case (2)
      is_default_numeric = DF2 .EQ. 1
    case (3)
      is_default_numeric = DF3 .EQ. 1
    case (4)
      is_default_numeric = DF4 .EQ. 1
    case (5)
      is_default_numeric = DF5 .EQ. 1
    case default
      is_default_numeric = .TRUE.
    end select
  end function

  logical function has_numeric_input_after(max_word)
    implicit none
    integer, intent(in) :: max_word

    has_numeric_input_after = .FALSE.
    if (max_word < 1 .and. has_numeric_input(1)) has_numeric_input_after = .TRUE.
    if (max_word < 2 .and. has_numeric_input(2)) has_numeric_input_after = .TRUE.
    if (max_word < 3 .and. has_numeric_input(3)) has_numeric_input_after = .TRUE.
    if (max_word < 4 .and. has_numeric_input(4)) has_numeric_input_after = .TRUE.
    if (max_word < 5 .and. has_numeric_input(5)) has_numeric_input_after = .TRUE.
  end function

  logical function has_any_numeric_input()
    implicit none

    has_any_numeric_input = has_numeric_input_after(0)
  end function

  logical function reject_string_input(command_name)
    implicit none
    character(len=*), intent(in) :: command_name

    reject_string_input = has_string_input()
    if (reject_string_input) then
      call report_error_and_fail(command_name//' TAKES NO STRING INPUT\nRE-ENTER COMMAND', 1)
    end if
  end function

  logical function reject_numeric_input_after(command_name, max_word)
    implicit none
    character(len=*), intent(in) :: command_name
    integer, intent(in) :: max_word
    character(len=80) :: numeric_range

    reject_numeric_input_after = has_numeric_input_after(max_word)
    if (reject_numeric_input_after) then
      select case (max_word)
      case (0)
        numeric_range = 'NUMERIC INPUT'
      case (1)
        numeric_range = 'NUMERIC WORD #2 THROUGH #5 INPUT'
      case (2)
        numeric_range = 'NUMERIC WORD #3 THROUGH #5 INPUT'
      case (3)
        numeric_range = 'NUMERIC WORD #4 OR #5 INPUT'
      case (4)
        numeric_range = 'NUMERIC WORD #5 INPUT'
      case default
        numeric_range = 'NUMERIC INPUT'
      end select
      call report_error_and_fail(command_name//' TAKES NO '//trim(numeric_range)//'\nRE-ENTER COMMAND', 1)
    end if
  end function

  logical function require_numeric_input(command_name, word_number)
    implicit none
    character(len=*), intent(in) :: command_name
    integer, intent(in) :: word_number
    character(len=80) :: word_label

    require_numeric_input = is_default_numeric(word_number)
    if (require_numeric_input) then
      select case (word_number)
      case (1)
        word_label = 'NUMERIC WORD #1'
      case (2)
        word_label = 'NUMERIC WORD #2'
      case (3)
        word_label = 'NUMERIC WORD #3'
      case (4)
        word_label = 'NUMERIC WORD #4'
      case (5)
        word_label = 'NUMERIC WORD #5'
      case default
        word_label = 'NUMERIC INPUT'
      end select
      call report_error_and_fail(command_name//' REQUIRES EXPLICIT '//trim(word_label)//' INPUT\nRE-ENTER COMMAND', 1)
    end if
  end function

  logical function reject_qualifier_or_numeric_input(command_name)
    implicit none
    character(len=*), intent(in) :: command_name

    reject_qualifier_or_numeric_input = has_qualifier_input() .or. has_any_numeric_input()
    if (reject_qualifier_or_numeric_input) then
      call report_error_and_fail(command_name//' TAKES NO EXPLICIT INPUT\nRE-ENTER COMMAND', 1)
    end if
  end function

  function hasAlphaNumericInput(self) result(flag)
    use DATMAI
    implicit none
    class(command_parser) :: self
    logical :: flag


  end function

  function checkCommandInput(ID_CMD_TYPE, qual_words, qual_only_err_msg, max_num_terms) result (goodInput)
    ! qual_words - character array of words that are acceptable for this command
    !  EG for LIB qual words are PUT and GET
    ! qual_only_err_msg - override error message when input error is encountered

    !use kdp_utils
    use DATMAI
    implicit none
    integer, intent(in) :: ID_CMD_TYPE(:) ! only required argument
    integer, optional :: max_num_terms 
    character(len=*), optional :: qual_words(:)
    character(len=*), optional :: qual_only_err_msg
    type(command_parser) :: cp
    logical :: goodInput
    integer :: i, numValidTypes, cmdToProcess

    call currInputData%initialize()

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
        call LogTermFOR("Error:  Mixing of command types.  Please enter command again")
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
            call LogTermFOR(trim(qual_only_err_msg))
          else
            call LogTermFOR("No valid qualifier words found.")
            call LogTermFOR("Please reenter command")
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
          call LogTermFOR("Command takes no string or > 1 numeric input ")
          RETURN        
        end if

      case (2)
        if(SST.EQ.1.OR.S3.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call LogTermFOR("Command takes no string or > 2 numeric input ")
          RETURN        
        end if

      case (3)
        if(SST.EQ.1.OR.S4.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call LogTermFOR("Command takes no string or > 3 numeric input ")
          RETURN        
        end if  
      case (4)
        if(SST.EQ.1.OR.S5.EQ.1) then
          goodInput = .FALSE.
          call LogTermFOR("Command takes no string or > 4 numeric input ")
          RETURN        
        end if   
      end select
    end if             
    case (ID_CMD_ALPHA)



    case default


    end select


  end function

  function isCmdType(ID_CMD_TYPE) result(typeExists)
    use DATMAI
    implicit none
    logical :: typeExists
    integer :: ID_CMD_TYPE
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

  subroutine parseCommandIntoTokens(cmdInput, tokens, numTokens, iptDelim, tokenLen)
    implicit none
    character(len=*) :: cmdInput
    character(len=80) :: tokens(40)
    integer :: numTokens, fst, lst, i, j, abslst
    character(len=1), optional :: iptDelim
    character(len=1) :: delim
    integer, optional  :: tokenLen(40)
    character(len=:), allocatable :: subString
    integer :: tot

  PRINT *, "Alphanumeric string is ", cmdInput

  allocate(character(len=len(cmdInput)) :: subString)

  if (.not.present(tokenLen))  PRINT *, "Tokenlen doesn't exist!"

  if (.not.present(iptDelim))  then 
     PRINT *, "Using blank as delimiter "
     delim = " "
  else
     delim = iptDelim
     
  end if

  !PRINT *, "delim is ", delim
  !Test String Tokenizer
  subString = cmdInput
  fst = INDEX(subString, delim, BACK=.FALSE.)
  abslst = INDEX(subString, delim, BACK=.TRUE.)

  !print *, "fst is ", fst
  !print *, "abslst is ", abslst
  !print *, "len of cmdInput is ", len(cmdInput)

  !print *, "End of string is ", cmdInput(abslst:len(cmdInput))
  
  ! Deal with case of one or two tokens.  I'm sure there is a more elegant way to do this
  ! but this seems to work
  if (fst==abslst) then
    if (fst==0) then
       numTokens = 1
       tokens(numTokens) = subString
    else
      if (abslst < len(cmdInput)) then
        numTokens = 2
        tokens(1) = subString(1:fst-1)
        tokens(2) = subString(abslst+1:len(cmdInput))
      end if
    end if

  else
    i = 1 
    tot = 0

  ! Keep on extracting tokens until we run out of delimiters or find that fst=last (end of tokens)  

  do while (fst > 1)
     fst = INDEX(subString, delim, BACK=.FALSE.)
     lst = INDEX(subString, delim, BACK=.TRUE.)
       !PRINT *, "i is ", i
       !PRINT *, "fst is ", fst
       !PRINT *, "lst is ", lst

     !if (fst==lst) then
       ! We have the last token.  Need to exit loop
     !  tokens(i) = subString(1:fst-2)
     !  fst = 0 ! exit loop
       ! This is to compensate for i = i +1 at end of loop.  The best thing I can say is that
       ! this code works, but it is far from elegant
     !  i = i - 1
     !else  
       tokens(i) = subString(1:fst-1)
       tot = tot + fst
       if (tot == abslst) then
        ! We have reached the last token.  Exit here 
        if (abslst < len(cmdInput)) then
          i = i + 1
          tokens(i) = cmdInput(tot+1:len(cmdInput))
          numTokens = i
          return
        else
          numTokens = 1
          return
       end if
      end if
     !end if
     !PRINT *, "token is ", tokens(i)
     !PRINT *, "subString before removing token(i) is ", subString
     !PRINT *, "tot is ", tot
     !PRINT *, "remaining cmd is ", cmdInput(tot+1:len(cmdInput))
     

     if(present(tokenLen)) tokenLen(i) = fst-1
     i = i+1
     if (fst<len(cmdInput)) subString = subString(fst+1:len(cmdInput))
     ! Skip consecutive delimiters so "CMD   ARG" parses the same as "CMD ARG"
     do while (len(subString) > 0 .and. subString(1:1) == delim)
       subString = subString(2:len(subString))
     end do
     !PRINT *, "subString is ", subString
  end do

    numTokens = i-1


  end if

  ! Compact out empty tokens produced by consecutive delimiters (e.g. "RMD      S1")
  block
    integer :: src, dst
    dst = 0
    do src = 1, numTokens
      if (len_trim(tokens(src)) > 0) then
        dst = dst + 1
        tokens(dst) = tokens(src)
      end if
    end do
    numTokens = dst
  end block

  if (numTokens > 0 ) PRINT *, "tokens ", tokens(1:numTokens)
  !if(present(tokenLen) PRINT *, "Token Length = ", tokenLen(1:i-2)


  end subroutine

  function cmdOptionExists(cmdTst) result(cmdExists)
    use DATMAI
    implicit none
    character(len=*) :: cmdTst
    logical :: cmdExists
    integer :: cmdLoc = 0
    cmdLoc = index(WS, cmdTst)
    cmdExists = .FALSE.
    if (cmdLoc>0) cmdExists = .TRUE.

  end function

  function getCmdInputValue(cmdToGet) result(cmdVal)
    use DATMAI
    implicit none
    character(len=*) :: cmdToGet
    real :: cmdVal
    character(len=80) :: tokens(40)
    integer :: numTokens, fst,  i
  !call checkCommandInput(typeCode,
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

  subroutine init_parsed_data(self)
    use DATMAI
    class(input_parsed_data) :: self
     self%inputNums(1) = W1
     self%inputNums(2) = W2
     self%inputNums(3) = W3
     self%inputNums(4) = W4
     self%inputNums(5) = W5
     if(S1.eq.0.and.S2.eq.0.and.S3.eq.0.and.S4.eq.0.and.S5.eq.0) then 
       self%maxNums = 0
     else if (S2.eq.0.and.S3.eq.0.and.S4.eq.0.and.S5.eq.0) then 
      self%maxNums = 1
     else if(S3.eq.0.and.S4.eq.0.and.S5.eq.0) then 
      self%maxNums = 2
     else if(S4.eq.0.and.S5.eq.0) then 
      self%maxNums = 3
     else if(S5.eq.0) then 
      self%maxNums = 4
     else if(S5.ne.0) then 
      self%maxNums = 5
     end if
     
     ! TODO:  only populate this if WQ is valid for this cmd
     self%inputQualWord = WQ
     



  end subroutine

  function getQualWord() result(rslt)
    character(len=10) :: rslt

    rslt = currInputData%inputQualWord

  end function  

  function getInputNumber(index) result(rslt)
    use iso_fortran_env, only: real64
    real(kind=real64) :: rslt

    rslt = currInputData%inputNums(index)

  end function

  function removeQuotes(iptStr) result(outStr)
    character(len=*) :: iptStr
    character(len=80) :: outStr
    integer :: lQ, rQ
    lQ = INDEX(iptStr, '"', BACK=.FALSE.)
    rQ = INDEX(iptStr, '"', BACK=.TRUE.)
    if(lQ.NE.rQ.AND.rQ.GT.lQ) then
      outStr = iptStr(lQ+1:rQ-1)
    else
      outStr = iptStr ! No quotes found
    end if

  end function
  ! This is a special parsing as the title is enlcosed in single quotes
  ! If this is in more commands abstract this
  function parseTitleCommand() result(title)
    use strings
    use DATMAI
    implicit none
    character(len=80) :: title
    character(len=80) :: restOfString

    character(len=5) :: cmdStr
    integer :: lenInput, lQ, rQ
    integer :: blankLoc

    lenInput = len(trim(INPUT))

    title = ""
    blankLoc = INDEX(trim(INPUT), ' ', BACK=.FALSE.)
    cmdStr = INPUT(1:blankLoc)
    restOfString = INPUT(blankLoc+1:lenInput)

    if(uppercase(cmdStr).EQ.'TIT'.OR.uppercase(cmdStr).EQ.'TITLE') then 
      ! We have the right command to parse
      lQ = INDEX(restOfString, '''', BACK=.FALSE.)
      rQ = INDEX(restOfString, '''', BACK=.TRUE.)
      if(lQ.NE.rQ.AND.rQ.GT.lQ) then
        title = restOfString(lQ+1:rQ-1)
      else
        return ! No valid title found
      end if
    else
      return ! No valid title found
    end if

  end function

  function isInputNumber(tstStr) result(boolResult)
    implicit none
    character(len=*) :: tstStr
    logical :: boolResult
    integer :: ierror, rerror, intVal

    
    real(kind=8) :: realVal8

    read(tstStr, '(i10)' , iostat=ierror) intval
    read(tstStr, *, iostat=rerror) realVal8

    boolResult = .FALSE.

    if (ierror == 0 .OR. rerror == 0) boolResult = .TRUE.

  end function

end module

subroutine report_error_and_fail(message, show_code)
  use command_utils, only: report_error_and_fail_impl => report_error_and_fail
  implicit none
  character(len=*), intent(in) :: message
  integer, intent(in) :: show_code

  call report_error_and_fail_impl(message, show_code)
end subroutine
