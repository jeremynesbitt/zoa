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


type setting_parser
   character(len=80)  :: tokens(40)
   integer :: wavelengthIdx, fieldIdx, numTokens



 contains
 procedure, public, pass(self) :: initialize  
 !procedure, public, pass(self) :: getWavelength
 !procedure, public, pass(self) :: getField
 procedure, private, pass(self) :: setWavelength
 !procedure, private, pass(self) :: setField

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
  !call self%setField()

end subroutine

subroutine setWavelength(self)
  use global_widgets, only: sysConfig
  use kdp_utils, only: str2int
  class(setting_parser), intent(inout) :: self
  character(len=:), allocatable :: strTest
  integer :: i, wC

  allocate(character(len=len(self%tokens(1))) :: strTest)

  do i=1, self%numTokens
    strTest = self%tokens(i)
    PRINT *, "strTest is ", strTest
    self%wavelengthIdx = sysConfig%refWavelengthIndex
    if (strTest(1:1) == 'w') then
    wC = str2int(strTest(2:len(trim(strTest))))
      !PRINT *, "wC is ", wC
      !PRINT *, "Num Wave is ", (sysConfig%numWavelengths+1)
      !PRINT *, "Cond 1 is " (wC.GT.0)
      !PRINT *, "Cond 2 is " (wC.LT.(sysConfig%numWavelengths+1))

      if (wC.GT.0.and.wC.LT.(sysConfig%numWavelengths+1)) then
        PRINT *, "about to set wavelength index to ", wC
        self%wavelengthIdx = wC
      else
        self%wavelengthIdx = sysConfig%refWavelengthIndex

      end if

    end if

  end do

end subroutine

end module