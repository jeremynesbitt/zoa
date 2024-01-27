module mod_lens_editor_settings
type lens_editor_settings
     integer changed
     integer numSurfaces
     !real, ALLOCATABLE radii(:), thicknesses(:)



contains
    procedure, public, pass(self) :: lens_editor_is_changed

  end type lens_editor_settings



interface lens_editor_settings
  module procedure :: lens_editor_settings_constructor
end interface lens_editor_settings

contains

type(lens_editor_settings) function lens_editor_settings_constructor() result(self)
    self%changed = 0
    call getOpticalSystemLastSurface(self%numSurfaces)

    self%numSurfaces = self%numSurfaces + 1 ! Since surface number starts with 022

end function lens_editor_settings_constructor


function lens_editor_is_changed(self) result(flag)
  class(lens_editor_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function lens_editor_is_changed




end module mod_lens_editor_settings


! This module is to interpret input commands and turn them into settings to be used during
! plotting 
module plotSettingParser

  !eg PLTOPD n64 w1 f3 si p0 azi30 alt60
  ! have a type for each plot type
  ! eg for wavelength
  ! type of ui control (spin button)
  ! default, min, max
  ! unique ID
  ! prefix
  ! 
  ! How to use
  ! addSetting to command.  Eg setting_manager%addSetting(ID_WAVELENGTH)
  ! it would ideally be able to:
  ! get data from input command to see if value should be updated
  ! update command 
  ! interact with tab to populate ui when needed
  ! eg commands
  ! setting_manager%initialize(INPUT)
  ! setting_manager%addCommand(Wavelength)
  ! setting_manager%finalize(objIdx)
  


type setting_parser
   ! The current design is that all possible settings are listed here
   ! and in the initialize routing they are looked for from the input tokens.
   ! if the type receives a request to get a setting, then it assumes that it needs to
   ! be in the command and adds it to the token list if it isn't there.
    
   character(len=80)  :: tokens(40)
   integer :: wavelengthIdx, fieldIdx, numTokens, density
   integer :: surf, plotType, azi, alt




 contains
 procedure, public, pass(self) :: initialize  
 procedure, private, pass(self) :: getIntFromToken
 procedure, public, pass(self) :: checkForIntToken
 procedure, public, pass(self) :: checkForStrToken
 procedure, private, pass(self) :: addToken
 procedure, private, pass(self) :: addTokenStr
 procedure, public, pass(self) :: getCommand


end type

contains
subroutine initialize(self, tokens)
  class(setting_parser), intent(inout) :: self
  character(len=*), dimension(:) :: tokens
  integer :: i


  do i=1,size(tokens)

    self%tokens(i) = tokens(i)
  end do
  self%numTokens = size(tokens)
  PRINT *, "numTokens is ", self%numTokens
  PRINT *, "len of tkens is  ", len(tokens)


end subroutine

function getCommand(self) result(strCmd)
  implicit none
  class(setting_parser) :: self
  character(len=240) :: strCmd
  integer :: i

  strCmd = self%tokens(1)
  do i=2,self%numTokens
    strCmd = trim(strCmd)//' '//self%tokens(i)
  end do


end function


function checkForIntToken(self, prefix, defVal) result(val)
  !If token exists, return value
  !If not, add token and return default value
  use type_utils, only: str2int
  implicit none

  class(setting_parser), intent(inout) :: self
  character(len=*) :: prefix
  integer :: defVal
  integer :: val, i
  character(len=:), allocatable :: strTest
  logical :: addNewToken

  allocate(character(len=len(self%tokens(1))) :: strTest)
  val = defVal ! This is the default value for no result
  addNewToken = .TRUE.

  do i=1, self%numTokens
    strTest = self%tokens(i)
    if (strTest(1:len(prefix)) == prefix) then
    addNewToken = .FALSE.
    val = str2int(strTest(len(prefix)+1:len(trim(strTest))))
    end if
  end do

  if (addNewToken) then
    call self%addToken(prefix, defVal)
  end if

end function


function checkForStrToken(self, prefix, defVal) result(val)
  !If token exists, return value
  !If not, add token and return default value
  implicit none

  class(setting_parser), intent(inout) :: self
  character(len=*) :: prefix
  character(len=*) :: defVal
  character(len=10) :: val
  integer :: i
  character(len=:), allocatable :: strTest
  logical :: addNewToken

  allocate(character(len=len(self%tokens(1))) :: strTest)
  val = defVal ! This is the default value for no result
  addNewToken = .TRUE.

  do i=1, self%numTokens
    strTest = self%tokens(i)
    if (strTest(1:len(prefix)) == prefix) then
    addNewToken = .FALSE.
    val = strTest(len(prefix)+1:len(trim(strTest)))
    end if
  end do

  if (addNewToken) then
    call self%addTokenStr(prefix, trim(defVal))
  end if

end function


function getIntFromToken(self, prefix) result(val)
  use type_utils, only: str2int
  implicit none

  class(setting_parser), intent(in) :: self
  character(len=*) :: prefix
  integer :: val, i
  character(len=:), allocatable :: strTest

  allocate(character(len=len(self%tokens(1))) :: strTest)
  val = -1 ! This is the default value for no result

  do i=1, self%numTokens
    strTest = self%tokens(i)
    if (strTest(1:len(prefix)) == prefix) then
    val = str2int(strTest(len(prefix)+1:len(trim(strTest))))
    end if
  end do

end function

subroutine addTokenStr(self, prefix, strVal)
  use type_utils, only: int2str
  implicit none
  class(setting_parser), intent(inout) :: self
  character(len=*) :: prefix
  character(len=*) :: strVal

  self%numTokens = self%numTokens+1
  self%tokens(self%numTokens) = prefix // strVal

end subroutine

subroutine addToken(self, prefix, intVal)
  use type_utils, only: int2str
  implicit none
  class(setting_parser), intent(inout) :: self
  character(len=*) :: prefix
  integer :: intVal

  self%numTokens = self%numTokens+1
  self%tokens(self%numTokens) = prefix // trim(int2str(intVal))

end subroutine


end module