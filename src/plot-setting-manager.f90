    ! have a type for each plot type
  ! eg for wavelength
  ! type of ui control (spin button)
  ! default, min, max
  ! unique ID
  ! prefix
  
  ! setting_manager%initialize(INPUT)
  ! setting_manager%addCommand(Wavelength)
  ! setting_manager%finalize(objIdx)
module plot_setting_manager
    use plotSettingParser
    use zoa_ui

    integer, parameter :: SETTING_WAVELENGTH = 1750
    integer, parameter :: SETTING_FIELD = 2
    integer, parameter :: SETTING_DENSITY = 1751
    integer, parameter :: SETTING_ZERNIKE = 4


    type plot_setting
      integer ::  ID, uitype
      real :: min, max, default
      character(len=3) :: prefix !For command parsing
      character(len=80) :: label
      character(len=10) :: defaultStr
      character(len=80) :: cmd ! The Command used to change the value.  Eg SETWV or SETDENS
      character(len=80) :: fullCmd ! command + value to change.  eg SETWV 1


      contains
       procedure, public, pass(self) :: initialize => init_setting
       procedure, public, pass(self) :: init_setting_new
       procedure, public, pass(self) :: initializeStr
       procedure, public, pass(self) :: initializeStr_new
         


    end type

    type zoaplot_setting_manager
    type(plot_setting), dimension(16) :: ps
    integer :: numSettings
    character(len=140) :: baseCmd
    type(setting_parser) :: sp
    integer :: plotNum 

    contains
    procedure, public, pass(self) :: initialize => init_plotSettingManager
    procedure, public, pass(self) :: init_plotSettingManager_new
    procedure, public, pass(self) :: addWavelengthSetting
    procedure, public, pass(self) :: addWavelengthSetting_new
    procedure, public, pass(self) :: updateWavelengthSetting_new
    procedure, public, pass(self) :: getWavelengthSetting_new
    procedure, public, pass(self) :: generatePlotCommand


    procedure, public, pass(self) :: addFieldSetting
    procedure, public, pass(self) :: addDensitySetting
    procedure, public, pass(self) :: addDensitySetting_new   
    procedure, public, pass(self) :: getDensitySetting_new     
    procedure, public, pass(self) :: updateDensitySetting_new    
    procedure, public, pass(self) :: addZernikeSetting
    procedure, public, pass(self) :: addZernikeSetting_new
    procedure, public, pass(self) :: updateZernikeSetting_new
    procedure, public, pass(self) :: getZernikeSetting_min_and_max


    procedure, public, pass(self) :: updateSetting_new

    !procedure, public, pass(self) :: addZernikeSetting

    procedure, public, pass(self) :: finalize


    end type


contains

    subroutine init_setting(self, ID_SETTING, label, default, min, max, prefix, ID_UITYPE)
        class (plot_setting) :: self
        integer :: ID_SETTING, ID_UITYPE
        character(len=*) :: label, prefix
        real :: default, min, max

        self%ID = ID_SETTING
        self%uitype = ID_UITYPE
        self%label = label
        self%default = default
        self%min = min
        self%max = max
        self%prefix = prefix

    end subroutine

    subroutine init_setting_new(self, ID_SETTING, label, default, min, max, cmd, fullCmd, ID_UITYPE)
      class (plot_setting) :: self
      integer :: ID_SETTING, ID_UITYPE
      character(len=*) :: label, cmd, fullCmd
      real :: default, min, max

      self%ID = ID_SETTING
      self%uitype = ID_UITYPE
      self%label = label
      self%default = default
      self%min = min
      self%max = max
      self%cmd= cmd
      self%fullCmd = fullCmd

  end subroutine


    subroutine initializeStr(self, ID_SETTING, label, default, prefix, ID_UITYPE)
        class (plot_setting) :: self
        integer :: ID_SETTING, ID_UITYPE
        character(len=*) :: label, default, prefix
  
        self%ID = ID_SETTING
        self%uitype = ID_UITYPE
        self%label = label
        self%default = 0.0
        self%defaultStr = default
        self%min = -1
        self%max = -1
        self%prefix = prefix

    end subroutine
    
    subroutine initializeStr_new(self, ID_SETTING, label, default, cmd, ID_UITYPE)
      implicit none
      class (plot_setting) :: self
      integer :: ID_SETTING, ID_UITYPE
      character(len=*) :: label, default, cmd

      self%ID = ID_SETTING
      self%uitype = ID_UITYPE
      self%label = label
      self%default = 0.0
      self%defaultStr = default
      self%min = -1
      self%max = -1
      self%cmd = cmd 
      self%fullCmd = cmd//" "//default

  end subroutine
  


    subroutine init_plotSettingManager(self, strCmd)
        use plotSettingParser
        use command_utils, only:  parseCommandIntoTokens

        class(zoaplot_setting_manager) :: self
        character(len=*) :: strCmd
        
        character(len=80) :: tokens(40)
        integer :: numTokens        

        PRINT *, "INPUT is ", strCmd

        self%numSettings = 0
        

       call parseCommandIntoTokens(trim(strCmd), tokens, numTokens, " ") 
       call self%sp%initialize(tokens(1:numTokens))
       PRINT *, "New CMD is ", self%sp%getCommand()


    end subroutine


    subroutine init_plotSettingManager_new(self, strCmd)
      !use plotSettingParser
      !use command_utils, only:  parseCommandIntoTokens

      class(zoaplot_setting_manager) :: self
      character(len=*) :: strCmd

      self%numSettings = 0
      self%baseCmd = strCmd
      ! For now is always set outside of psm
      self%plotNum = -1
      
  end subroutine



    function addWavelengthSetting(self) result(lambda)
        use global_widgets, only: sysConfig
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: lambda

        lambda = self%sp%checkForIntToken('w', sysConfig%refWavelengthIndex)

        self%numSettings = self%numSettings + 1
        PRINT *, "numWavelengths is ", real(sysConfig%numWavelengths)
        call self%ps(self%numSettings)%initialize(SETTING_WAVELENGTH, & 
        & "Wavelength", real(lambda),1.0,real(sysConfig%numWavelengths), 'w', UITYPE_SPINBUTTON)


      end function   

      subroutine addWavelengthSetting_new(self) 
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: lambda

        lambda = sysConfig%refWavelengthIndex
        self%numSettings = self%numSettings + 1
        PRINT *, "numWavelengths is ", real(sysConfig%numWavelengths)
        call self%ps(self%numSettings)%init_setting_new(SETTING_WAVELENGTH, & 
        & "Wavelength", real(lambda),1.0,real(sysConfig%numWavelengths), &
        & "SETWV", "SETWV "//trim(int2str(lambda)), UITYPE_SPINBUTTON)


      end subroutine 

      subroutine updateWavelengthSetting_new(self, newIdx) 
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: newIdx
        integer :: i

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_WAVELENGTH) then
            call LogTermFOR("Found setting and changing to " //int2str(newIdx))

            self%ps(i)%fullCmd = trim("SETWV "//int2str(newIdx))
          end if
        end do

      end subroutine 

      function getWavelengthSetting_new(self) result(wvIdx)
        use global_widgets, only: sysConfig
        use type_utils, only: str2int
        use strings
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: wvIdx
        integer :: i
        character(len=80) :: tokens(40)
        integer :: numTokens

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_WAVELENGTH) then
            call parse(trim(self%ps(i)%fullCmd), ' ', tokens, numTokens) 
            wvIdx = str2int(tokens(2))
          end if
        end do

      end function



      function addFieldSetting(self) result(val)
        use global_widgets, only: sysConfig
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val

        val = self%sp%checkForIntToken('f', sysConfig%numFields)
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_FIELD, & 
        & "Field", real(val),1.0,real(sysConfig%numFields), 'f', UITYPE_SPINBUTTON)

      
      end function 

      function addZernikeSetting(self, defVal) result(val)
        use global_widgets, only: sysConfig
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        character(len=*) :: defVal
        character(len=10) :: val

        val = self%sp%checkForStrToken('c', defVal)
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initializeStr(SETTING_ZERNIKE, & 
        & "Zernike Coefficients", val, 'c', UITYPE_ENTRY)

      
      end function       


      subroutine addZernikeSetting_new(self, defVal) 
        use global_widgets, only: sysConfig
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        character(len=*) :: defVal
        character(len=10) :: val

        val = defVal
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initializeStr_new(SETTING_ZERNIKE, & 
        & "Zernike Coefficients", val, "SETZERNC", UITYPE_ENTRY)

      
      end subroutine             
     
      subroutine updateZernikeSetting_new(self, newVal) 
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        character(len=*) :: newVal
        integer :: i

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_ZERNIKE) then
            self%ps(i)%defaultStr = newVal
            self%ps(i)%fullCmd = self%ps(i)%cmd//" "//newVal
          end if
        end do

      end subroutine 

      subroutine getZernikeSetting_min_and_max(self, minZ, maxZ)
        use type_utils, only: str2int, int2str
        implicit none
        class(zoaplot_setting_manager) :: self
        integer, intent(inout) ::minZ, maxZ
        integer :: locE, i

        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_ZERNIKE) then
            locE = index(self%ps(i)%defaultStr, '..')
            minZ = str2int(self%ps(i)%defaultStr(1:locE-1))
            maxZ = str2int(self%ps(i)%defaultStr(locE+2:len(self%ps(i)%defaultStr)))

            return
          end if
        end do


      end subroutine


      subroutine addDensitySetting_new(self, defaultVal, minVal, maxVal) 
        use type_utils, only: int2str
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val, defaultVal, minVal, maxVal

        val = defaultVal
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%init_setting_new(SETTING_DENSITY, & 
        & "Density", real(val),real(minVal),real(maxVal), &
        & "SETDENS", "SETDENS "//trim(int2str(defaultVal)), UITYPE_SPINBUTTON)
      
      end subroutine  

      
      subroutine updateSetting_new(self, setting_code, newVal)
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        class(*), intent(in) :: newVal
        integer :: setting_code
        !integer :: newVal
        integer :: i

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == setting_code) then

            select type(newVal)
              type is (integer)

                self%ps(i)%fullCmd = trim(self%ps(i)%cmd)// &
                & " "//trim(int2str(newVal))
              type is (character(*))
                self%ps(i)%defaultStr = newVal
                self%ps(i)%fullCmd = trim(self%ps(i)%cmd)// &
                & " "//newVal       
                call LogTermFOR("Updated Char val to "//self%ps(i)%defaultStr)     
            end select
          end if
        
        end do

      end subroutine 

      subroutine updateDensitySetting_new(self, newVal) 
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: newVal
        integer :: i

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_DENSITY) then
            self%ps(i)%fullCmd = trim("SETDENS "//int2str(newVal))
          end if
        end do

      end subroutine 

      function addDensitySetting(self, defaultVal, minVal, maxVal) result(val)
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val, defaultVal, minVal, maxVal

        val = self%sp%checkForIntToken('n', defaultVal)
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_DENSITY, & 
        & "Density", real(val),real(minVal),real(maxVal), 'n', UITYPE_SPINBUTTON)

      
      end function  
      
      function getDensitySetting_new(self) result(denVal)
        use global_widgets, only: sysConfig
        use type_utils, only: str2int
        use strings
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: denVal
        integer :: i
        character(len=80) :: tokens(40)
        integer :: numTokens

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_DENSITY) then
            print *, "density cmd is ", self%ps(i)%fullCmd
            call parse(trim(self%ps(i)%fullCmd), ' ', tokens, numTokens) 
            denVal = str2int(tokens(2))
          end if
        end do

      end function      

    ! Tmp to allow for compile w/o circular deps  
    subroutine finalize(self, objIdx, inputCmd)
      use iso_c_binding, only: c_null_char
      use type_utils, only: int2str
      implicit none
        character(len=*) :: inputCmd
        integer :: objIdx
        integer :: i
        class(zoaplot_setting_manager) :: self      

      end subroutine
      


    function generatePlotCommand(self) result(strOut)
      class(zoaplot_setting_manager):: self
      integer :: i
      character(len=1024) :: strOut

      strOut = trim(self%baseCmd)
      do i=1,self%numSettings
        strOut = trim(strOut) // " ; "//self%ps(i)%fullCmd
      end do

      strOut = trim(strOut) // " ; GO"


    end function



end module