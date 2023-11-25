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
 procedure, public, pass(self) :: getWavelength
 procedure, public, pass(self) :: getField
 procedure, public, pass(self) :: getDensity
 procedure, private, pass(self) :: setWavelength
 procedure, private, pass(self) :: getIntFromToken
 procedure, private, pass(self) :: setField
 procedure, public, pass(self) :: setDensity

 procedure, private, pass(self) :: addToken
 procedure, public, pass(self) :: getCommand


end type

contains
subroutine initialize(self, tokens)
  class(setting_parser), intent(inout) :: self
  character(len=*), dimension(:) :: tokens

  self%tokens = tokens
  self%numTokens = size(tokens)
  PRINT *, "numTokens is ", self%numTokens
  PRINT *, "len of tkens is  ", len(tokens)


  call self%setWavelength()
  PRINT *, "Wavelength Index is ", self%wavelengthIdx
  call self%setField()

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


function getIntFromToken(self, prefix) result(val)
  use kdp_utils, only: str2int
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

subroutine addToken(self, prefix, intVal)
  use kdp_utils, only: int2str
  implicit none
  class(setting_parser), intent(inout) :: self
  character(len=*) :: prefix
  integer :: intVal

  self%numTokens = self%numTokens+1
  self%tokens(self%numTokens) = prefix // trim(int2str(intVal))

end subroutine

! For the get methods, 
function getWavelength(self) result(val)
  implicit none
  class(setting_parser), intent(inout) :: self
  integer:: val

  val = self%wavelengthIdx

  if (self%getIntFromToken('w') == -1) then
    call self%addToken('w', self%wavelengthIdx)
  end if

end function

function getField(self) result(val)
  implicit none
  class(setting_parser), intent(inout) :: self
  integer:: val

  val = self%fieldIdx

  if (self%getIntFromToken('f') == -1) then
    call self%addToken('f', self%fieldIdx)
  end if

end function

function getDensity(self) result(val)
  implicit none
  class(setting_parser), intent(inout) :: self
  integer:: val

  val = self%density

  if (self%getIntFromToken('n') == -1) then
    call self%addToken('n', self%density)
  end if

end function

!!!!!!!!!!!!!!!!
! Set routines !
!!!!!!!!!!!!!!!!

subroutine setField(self)
  use global_widgets, only: sysConfig
  implicit none
  class(setting_parser), intent(inout) :: self
  integer :: wF

  self%fieldIdx = sysConfig%numFields ! TODO:  Better default way?
  wF = self%getIntFromToken('f')
  if(wF.GT.0.and.wF.LT.(sysConfig%numFields+1)) then
    self%fieldIdx = wF
  end if


end subroutine

subroutine setDensity(self, defDensity)
  use global_widgets, only: sysConfig
  implicit none
  class(setting_parser), intent(inout) :: self
  integer, optional :: defDensity
  integer :: wN

  if (present(defDensity)) then
    self%density = defDensity 
  else
    self%density = 1 ! ??
  end if

  wN = self%getIntFromToken('n')
  PRINT *, "in setDensity wN is ", wN
  if(wN.GT.0) then
    self%density = wN
  end if

end subroutine

subroutine setWavelength(self)
  use global_widgets, only: sysConfig
  implicit none
  class(setting_parser), intent(inout) :: self
  integer :: wL

  self%wavelengthIdx = sysConfig%refWavelengthIndex ! TODO:  Better default way?
  wL = self%getIntFromToken('w')
  if(wL.GT.0.and.wL.LT.(sysConfig%numWavelengths+1)) then
    self%wavelengthIdx = wL
  end if


end subroutine

end module