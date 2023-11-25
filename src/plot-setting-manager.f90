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

    integer, parameter :: SETTING_WAVELENGTH = 1
    integer, parameter :: SETTING_FIELD = 2
    integer, parameter :: SETTING_DENSITY = 3
    integer, parameter :: UITYPE_SPINBUTTON = 1

    type plot_setting
      integer ::  ID, uitype
      real :: min, max, default
      character(len=3) :: prefix !For command parsing
      character(len=80) :: label

      contains
       procedure, public, pass(self) :: initialize => init_setting
         


    end type

    type zoaplot_setting_manager
    type(plot_setting), dimension(16) :: ps
    integer :: numSettings
    type(setting_parser) :: sp

    contains
    procedure, public, pass(self) :: initialize => init_plotSettingManager
    procedure, public, pass(self) :: addWavelength
    procedure, public, pass(self) :: addFieldSetting
    procedure, public, pass(self) :: addDensitySetting
    procedure, public, pass(self) :: finalize
    procedure, public, pass(self) :: getWavelengthSetting
    procedure, public, pass(self) :: getFieldSetting
    procedure, public, pass(self) :: getDensitySetting





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
       PRINT *, "Wavelength is ", self%sp%getWavelength()
       PRINT *, "New CMD is ", self%sp%getCommand()


    end subroutine

    subroutine addFieldSetting(self, fldIdx)
        use global_widgets, only: sysConfig
        class(zoaplot_setting_manager) :: self
        integer :: fldIdx

        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_FIELD, & 
        & "Field", real(fldIdx),1.0,real(sysConfig%numFields), 'f', UITYPE_SPINBUTTON)


    end subroutine

    subroutine addDensitySetting(self, n, minVal, maxVal)
        use global_widgets, only: sysConfig
        class(zoaplot_setting_manager) :: self
        integer :: n, minVal, maxVal

        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_DENSITY, & 
        & "Density", real(n),real(minVal),real(maxVal), 'n', UITYPE_SPINBUTTON)


    end subroutine    

    subroutine addWavelength(self, lambda)
        use global_widgets, only: sysConfig
        class(zoaplot_setting_manager) :: self
        integer :: lambda

        self%numSettings = self%numSettings + 1
        PRINT *, "numWavelengths is ", real(sysConfig%numWavelengths)
        call self%ps(self%numSettings)%initialize(SETTING_WAVELENGTH, & 
        & "Wavelength", real(lambda),1.0,real(sysConfig%numWavelengths), 'w', UITYPE_SPINBUTTON)


    end subroutine

    function getWavelengthSetting(self) result(val)
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val

        val = self%sp%getWavelength()
        call self%addWavelength(val)
      
      end function   

      function getFieldSetting(self) result(val)
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val

        val = self%sp%getField()
        call self%addFieldSetting(val)

      
      end function         

      function getDensitySetting(self, defaultVal, minVal, maxVal) result(val)
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val, defaultVal, minVal, maxVal

        call self%sp%setDensity(defaultVal)

        val = self%sp%getDensity()
        call self%addDensitySetting(val, minVal, maxVal)

      
      end function   

    subroutine finalize(self, objIdx, inputCmd)
        use handlers, only: zoaTabMgr
        use iso_c_binding, only: c_null_char
        implicit none

        character(len=*) :: inputCmd
        integer :: objIdx
        integer :: i
        class(zoaplot_setting_manager) :: self

        zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
        do i=1,self%numSettings

        select case (self%ps(i)%uitype)

        case(UITYPE_SPINBUTTON)
        call zoaTabMgr%tabInfo(objIdx)%tabObj%addSpinButton_runCommand_2( & 
        & self%ps(i)%label, self%ps(i)%default, self%ps(i)%min, self%ps(i)%max, 1, &
        & trim(self%ps(i)%prefix))
        !"Number of Field Points", &
        !& 10.0, 1.0, 20.0, 1, "NUMPTS"//c_null_char)
        !call zoaTabMgr%tabInfo(objIdx)%tabObj%addSpinButton_runCommand("Test2", 1.0, 0.0, 10.0, 1, "")

        end select 
        end do
  
        


    end subroutine



end module