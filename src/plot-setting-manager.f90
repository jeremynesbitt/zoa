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
    integer, parameter :: SETTING_DENSITY = 3
    integer, parameter :: SETTING_ZERNIKE = 4


    type plot_setting
      integer ::  ID, uitype
      real :: min, max, default
      character(len=3) :: prefix !For command parsing
      character(len=80) :: label
      character(len=10) :: defaultStr
      character(len=80) :: cmd

      contains
       procedure, public, pass(self) :: initialize => init_setting
       procedure, public, pass(self) :: init_setting_new
       procedure, public, pass(self) :: initializeStr
         


    end type

    type zoaplot_setting_manager
    type(plot_setting), dimension(16) :: ps
    integer :: numSettings
    character(len=140) :: baseCmd
    type(setting_parser) :: sp

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
    procedure, public, pass(self) :: addZernikeSetting

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

    subroutine init_setting_new(self, ID_SETTING, label, default, min, max, cmd, ID_UITYPE)
      class (plot_setting) :: self
      integer :: ID_SETTING, ID_UITYPE
      character(len=*) :: label, cmd
      real :: default, min, max

      self%ID = ID_SETTING
      self%uitype = ID_UITYPE
      self%label = label
      self%default = default
      self%min = min
      self%max = max
      self%cmd = cmd

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
        & trim("SETWV "//int2str(lambda)), UITYPE_SPINBUTTON)


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
            call LogTermFOR("Found setting!")
            self%ps(i)%cmd = trim("SETWV "//int2str(newIdx))
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
            call parse(trim(self%ps(i)%cmd), ' ', tokens, numTokens) 
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
     

      function addDensitySetting(self, defaultVal, minVal, maxVal) result(val)
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val, defaultVal, minVal, maxVal

        val = self%sp%checkForIntToken('n', defaultVal)
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_DENSITY, & 
        & "Density", real(val),real(minVal),real(maxVal), 'n', UITYPE_SPINBUTTON)

      
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
      

    ! subroutine finalize(self, objIdx, inputCmd)
    !     use handlers, only: zoaTabMgr
    !     use iso_c_binding, only: c_null_char
    !     use type_utils, only: int2str
    !     implicit none

    !     character(len=*) :: inputCmd
    !     integer :: objIdx
    !     integer :: i
    !     class(zoaplot_setting_manager) :: self


    !     zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
    !     do i=1,self%numSettings

    !     select case (self%ps(i)%uitype)

    !     case(UITYPE_SPINBUTTON)
    !     call zoaTabMgr%tabInfo(objIdx)%tabObj%addSpinButton_runCommand_new( & 
    !     & trim(int2str(self%ps(i)%ID)), self%ps(i)%default, self%ps(i)%min, self%ps(i)%max, 1, &
    !     & trim(self%ps(i)%prefix))
    !     !"Number of Field Points", &
    !     !& 10.0, 1.0, 20.0, 1, "NUMPTS"//c_null_char)
    !     !call zoaTabMgr%tabInfo(objIdx)%tabObj%addSpinButton_runCommand("Test2", 1.0, 0.0, 10.0, 1, "")

    !     case(UITYPE_ENTRY)
    !     call zoaTabMgr%tabInfo(objIdx)%tabObj%addEntry_runCommand( &
    !     & self%ps(i)%label, self%ps(i)%defaultStr, trim(self%ps(i)%prefix))   

    !     end select 
    !     end do




    ! end subroutine

    function generatePlotCommand(self) result(strOut)
      class(zoaplot_setting_manager):: self
      integer :: i
      character(len=1024) :: strOut

      strOut = trim(self%baseCmd)
      call LogTermFOR("Str Out in genPlotCommand is "//strOut)
      do i=1,self%numSettings
        strOut = trim(strOut) // " ; "//self%ps(i)%cmd
        call LogTermFOR("Str Out in loop is "//strOut)
      end do

      strOut = trim(strOut) // " ; GO"


    end function



end module