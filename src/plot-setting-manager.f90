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
    use zoa_ui
    use kdp_data_types, only: idText

    !TODO:  move all this to zoa_ui 
    integer, parameter :: SETTING_WAVELENGTH = 1750
    integer, parameter :: SETTING_FIELD = 2
    integer, parameter :: SETTING_DENSITY = 1751
    integer, parameter :: SETTING_ZERNIKE = 4


    type plot_setting
      integer ::  ID, uitype
      real :: min, max, default
      character(len=3) :: prefix !For command parsing
      type(idText), allocatable :: set(:) ! options for a combo box
      character(len=80) :: label
      character(len=10) :: defaultStr
      character(len=80) :: cmd ! The Command used to change the value.  Eg SETWV or SETDENS
      character(len=80) :: fullCmd ! command + value to change.  eg SETWV 1


      contains
       procedure, public, pass(self) :: initialize => init_setting
       procedure, public, pass(self) :: initializeStr

    end type

    type zoaplot_setting_manager
    type(plot_setting), dimension(16) :: ps
    integer :: numSettings
    character(len=140) :: baseCmd
    integer :: plotNum 

    ! With current design, number of seetings here will be huges, as it has
    ! to include all possible settings in every plot
    ! maye be possible to use subtypes to make it more readable, but 
    ! not sure it is worth the effort...
    contains
    procedure, public, pass(self) :: initialize => init_plotSettingManager
    procedure, public, pass(self) :: addWavelengthSetting
    procedure, public, pass(self) :: updateWavelengthSetting
    procedure, public, pass(self) :: getWavelengthSetting
    procedure, public, pass(self) :: getFieldSetting
    procedure, public, pass(self) :: generatePlotCommand
    procedure, public, pass(self) :: getSettingValueByCode

    procedure, public, pass(self) :: addFieldSetting
    procedure, public, pass(self) :: addDensitySetting   
    procedure, public, pass(self) :: getDensitySetting     
    procedure, public, pass(self) :: updateDensitySetting    
    procedure, public, pass(self) :: addZernikeSetting
    procedure, public, pass(self) :: updateZernikeSetting
    procedure, public, pass(self) :: getZernikeSetting_min_and_max
    procedure, public, pass(self) :: addGenericSetting

    ! Spot Diagram Settings
    procedure, public, pass(self) :: addSpotDiagramSettings
    procedure, public, pass(self) :: addSpotCalculationSetting
    procedure, public, pass(self) :: getSpotDiagramSettings

    ! Lens Draw Settings
    procedure, public, pass(self) :: addLensDrawSettings
    procedure, public, pass(self) :: addLensDrawOrientationSettings   
    procedure, public, pass(self) :: addLensDrawScaleSettings    
    procedure, public, pass(self) :: getLensDrawSettings
    procedure, public, pass(self) :: addPlotManipToolbarSettings

    procedure, public, pass(self) :: addAstigSettings
    procedure, public, pass(self) :: getAstigSettings

    !RMS Settings
    procedure, public, pass(self) :: addRMSFieldSettings
    procedure, public, pass(self) :: getRMSFieldSettings

    procedure, public, pass(self) :: updateSetting

    !procedure, public, pass(self) :: addZernikeSetting

    procedure, public, pass(self) :: finalize


    end type



contains


    subroutine init_setting(self, ID_SETTING, label, default, min, max, cmd, fullCmd, ID_UITYPE, set)
      use type_utils, only: real2str
      class (plot_setting) :: self
      integer :: ID_SETTING, ID_UITYPE
      character(len=*) :: label, cmd, fullCmd
      type(idText), optional :: set(:)
      real :: default, min, max

      self%ID = ID_SETTING
      self%uitype = ID_UITYPE
      self%label = label
      self%default = default
      self%min = min
      self%max = max
      self%cmd= cmd
      self%fullCmd = fullCmd
      if(present(set)) then
        self%set = set
      end if

  end subroutine

    subroutine addLensDrawSettings(self)
      use zoa_ui
      use type_utils, only: int2str, real2str
      use mod_lens_data_manager, only: ldm
      implicit none
      class (zoaplot_setting_manager) :: self

      call self%addLensDrawOrientationSettings()

      ! Add indvidual settings 
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_NUM_FIELD_RAYS, & 
      & "Num Rays Per Feild", real(7),1.0,real(19), &
      & "NUMRAYS ", "NUMRAYS "//trim(int2str(7)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENS_FIRSTSURFACE, & 
      & "First Surface", real(0),0.0,real(ldm%getLastSurf()), &
      & "DRAWSI", "DRAWSI "//trim(int2str(20)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENS_LASTSURFACE, & 
      & "Last Surface", real(ldm%getLastSurf()),real(1.0),real(ldm%getLastSurf()), &
      & "DRAWSF ", "DRAWSF "//trim(int2str(ldm%getLastSurf())), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_ELEVATION, & 
      & "Elevation", real(26.2),real(0.0),real(360.0), &
      & "ELEV", "ELEV "//trim(real2str(26.2)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_AZIMUTH, & 
      & "Aziumuth", real(232.2),real(0.0),real(360.0), &
      & "AZI", "AZI "//trim(real2str(232.2)), UITYPE_SPINBUTTON)       

      call self%addLensDrawScaleSettings()

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_AUTOSCALE_VALUE, & 
      & "Manual Scale Factor", 0.045,real(0.0),real(10000.0), &
      & "SSI", "SSI "//trim(real2str(0.045,5)), UITYPE_SPINBUTTON)             


      ! Toolbar settings
      call self%addPlotManipToolbarSettings()

    end subroutine

    subroutine addPlotManipToolbarSettings(self)
      use type_utils, only: real2str
      class(zoaplot_setting_manager) :: self

      !For now just test x and y offset
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_OFFSET_X, & 
      & "Manual Scale Factor", real(0.0),real(-10000.0),real(10000.0), &
      & "XOFF", "XOFF "//trim(real2str(0.0)), UITYPE_TOOLBAR)    
     
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_OFFSET_Y, & 
      & "Manual Scale Factor", real(0.0),real(-10000.0),real(10000.0), &
      & "YOFF", "YOFF "//trim(real2str(0.0)), UITYPE_TOOLBAR)          

    end subroutine



    subroutine addLensDrawOrientationSettings(self)
      implicit none
      class (zoaplot_setting_manager) :: self
      type(idText) :: set(4)


      ! Move over stuff from ui-spot.  Perhaps this should be a subtype or submodule?
      set(1)%text = "YZ - Plane Layout"
      set(1)%id = ID_LENSDRAW_YZ_PLOT_ORIENTATION
    
      set(2)%text = "XZ - Plane Layout"
      set(2)%id = ID_LENSDRAW_XZ_PLOT_ORIENTATION
    
      set(3)%text = "XY - Plane Layout"
      set(3)%id = ID_LENSDRAW_XY_PLOT_ORIENTATION
 
      set(4)%text = "Orthographic"
      set(4)%id = ID_LENSDRAW_ORTHO_PLOT_ORIENTATION      


      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_PLOT_ORIENTATION, & 
      & "Plot Orientation", real(ID_LENSDRAW_YZ_PLOT_ORIENTATION),0.0,0.0, &
      & "ORIENT", "ORIENT YZ", UITYPE_COMBO, set=set)
      

    end subroutine

    subroutine addLensDrawScaleSettings(self)
      implicit none
      class (zoaplot_setting_manager) :: self
      type(idText) :: set(2)


      ! Move over stuff from ui-spot.  Perhaps this should be a subtype or submodule?
      set(1)%text = "AutoScale"
      set(1)%id = ID_LENSDRAW_AUTOSCALE
    
      set(2)%text = "Manual Scale"
      set(2)%id = ID_LENSDRAW_MANUALSCALE
    
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_SCALE, & 
      & "Auto or Manual Scale", real(ID_LENSDRAW_AUTOSCALE),0.0,0.0, &
      & "SSI", "SSI 1", UITYPE_COMBO, set=set)
      

    end subroutine    

    subroutine getLensDrawSettings(self, plotOrient, numRays, Si, Sf, elev, azi, scaleChoice, scaleFactor)
      implicit none
      class(zoaplot_setting_manager) :: self 
      integer, intent(inout) :: plotOrient, numRays, Si, Sf, scaleChoice
      real, intent(inout) :: elev, azi, scaleFactor

      plotOrient = self%getSettingValueByCode(ID_LENSDRAW_PLOT_ORIENTATION)
      numRays = self%getSettingValueByCode(ID_LENSDRAW_NUM_FIELD_RAYS)
      Si = self%getSettingValueByCode(ID_LENS_FIRSTSURFACE)
      Sf = self%getSettingValueByCode(ID_LENS_LASTSURFACE)
      elev = self%getSettingValueByCode(ID_LENSDRAW_ELEVATION)
      azi = self%getSettingValueByCode(ID_LENSDRAW_AZIMUTH)
      scaleChoice = self%getSettingValueByCode(ID_LENSDRAW_SCALE)
      scaleFactor = self%getSettingValueByCode(ID_LENSDRAW_AUTOSCALE_VALUE)
      
    end subroutine

    subroutine addSpotDiagramSettings(self)
      
      use zoa_ui
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self

     

      call self%addFieldSetting()
      call self%addWavelengthSetting()
      call self%addSpotCalculationSetting()
     

      ! Add indvidual settings 
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_SPOT_RECT_GRID, & 
      & "Rectangular Grid (nxm)", real(20),1.0,real(300), &
      & "RECTDENS", "RECTDENS "//trim(int2str(20)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_SPOT_RAND_NUMRAYS, & 
      & "Number of Rays (random only)", real(2000),1.0,real(100000000), &
      & "NUMRAYS", "NUMRAYS "//trim(int2str(2000)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_SPOT_RING_NUMRINGS, & 
      & "Number of Rings (ring only)", real(20),1.0,real(50), &
      & "NUMRAYS", "NUMRAYS "//trim(int2str(20)), UITYPE_SPINBUTTON)      
      
    end subroutine

    subroutine addGenericSetting(self, ID_CODE, label, default, min, max, baseCmd, fullCmd, UI_TYPE)
      use zoa_ui
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self
      integer :: ID_CODE, UI_TYPE
      character(len=*) :: label, baseCmd, fullCmd
      real :: default, min, max

      if (UI_TYPE /= UITYPE_ENTRY) then
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_CODE, & 
      & label, default,min,max, &
      & baseCmd, fullCmd, UI_TYPE)
      else 
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initializeStr(ID_CODE, & 
        & label, fullCmd, baseCmd, UI_TYPE)             
      end if


    end subroutine

    ! Since there is a lot of custom settings, write a method to get all settings
    subroutine getSpotDiagramSettings(self, idxField, idxLambda, idxSpotCalcMethod, nRect, nRand, nRing)
      use zoa_ui
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self
      integer, intent(inout) :: idxField, idxLambda, idxSpotCalcMethod
      integer, intent(inout) :: nRect, nRand, nRing

      idxField = self%getSettingValueByCode(SETTING_FIELD)
      idxLambda = self%getSettingValueByCode(SETTING_WAVELENGTH)
      
      idxSpotCalcMethod = self%getSettingValueByCode(ID_SPOT_TRACE_ALGO)
      !call LogTermFOR("In getSpotDiagramSettings idxSpotCalcMethod is "// &
      !&int2str(idxSpotCalcMethod))
      nRect = self%getSettingValueByCode(ID_SPOT_RECT_GRID)
      nRand = self%getSettingValueByCode(ID_SPOT_RAND_NUMRAYS)
      nRing = self%getSettingValueByCode(ID_SPOT_RING_NUMRINGS)

    end subroutine
    
    subroutine addSpotCalculationSetting(self)
      use kdp_data_types, only: idText
      implicit none
      class (zoaplot_setting_manager) :: self
      type(idText) :: spotTrace(3)

      ! Move over stuff from ui-spot.  Perhaps this should be a subtype or submodule?
      spotTrace(1)%text = "Rectangle"
      spotTrace(1)%id = ID_SPOT_RECT
    
      spotTrace(2)%text = "Ring"
      spotTrace(2)%id = ID_SPOT_RING
    
      spotTrace(3)%text = "Random"
      spotTrace(3)%id = ID_SPOT_RAND      

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_SPOT_TRACE_ALGO, & 
      & "Spot Tracing Method", real(ID_SPOT_RECT),0.0,0.0, &
      & "TRAC", "TRAC RECT", UITYPE_COMBO, set=spotTrace)

      !call LogTermFOR("Successfully Initialized Combo Box Settings")



      ! call self%settings%addListBoxTextID("Spot Tracing Method", spotTrace, &
      ! & c_funloc(callback_spot_settings), c_loc(TARGET_SPOT_TRACE_ALGO), &
      ! & spot_struct_settings%currSpotRaySetting)      


    end subroutine

    subroutine addAstigSettings(self)
      use kdp_data_types, only: idText
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self
      type(idText) :: set(2)
   
      set(1)%text = "Y FIELD"
      set(1)%id = ID_AST_FIELD_Y
    
      set(2)%text = "X FIELD"
      set(2)%id = ID_AST_FIELD_X
    
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_AST_FIELDXY, & 
      & "Field Selection", real(ID_AST_FIELD_Y),0.0,0.0, &
      & "ASTFLD ", "ASTFLD Y", UITYPE_COMBO, set=set)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_NUMPOINTS, & 
      & "Number of Points", real(10.0),1.0,50.0, &
      & "NUMPTS ", "NUMPTS "//int2str(10), UITYPE_SPINBUTTON)



    end subroutine

    subroutine getAstigSettings(self, idxFieldXY, numPts)
      use zoa_ui
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self
      integer, intent(inout) :: idxFieldXY, numPts

      idxFieldXY = INT(self%getSettingValueByCode(ID_AST_FIELDXY))
      numPts = INT(self%getSettingValueByCode(ID_NUMPOINTS))
      

    end subroutine

    subroutine addRMSFieldSettings(self)
      use kdp_data_types, only: idText
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self
      type(idText) :: set(2)
   
      set(1)%text = "Spot Size"
      set(1)%id = ID_RMS_DATA_SPOT
    
      set(2)%text = "Wavefront Error"
      set(2)%id = ID_RMS_DATA_WAVE
    
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_RMS_DATA_TYPE, & 
      & "Data", real(ID_RMS_DATA_WAVE),0.0,0.0, &
      & "RMSDATA ", "RMSDATA WAVE", UITYPE_COMBO, set=set)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_NUMPOINTS, & 
      & "Number of Points", real(10.0),1.0,50.0, &
      & "NUMPTS ", "NUMPTS "//int2str(10), UITYPE_SPINBUTTON)

      call self%addWavelengthSetting()


    end subroutine

    subroutine getRMSFieldSettings(self, iData, iLambda, numPoints)
      use zoa_ui
      use type_utils, only: int2str
      implicit none
      class (zoaplot_setting_manager) :: self
      integer, intent(inout) :: iData, iLambda, numPoints
   
      iData = self%getSettingValueByCode(ID_RMS_DATA_TYPE)
      iLambda = self%getSettingValueByCode(SETTING_WAVELENGTH)
      numPoints = self%getSettingValueByCode(ID_NUMPOINTS)
    
    end subroutine    

    
    subroutine initializeStr(self, ID_SETTING, label, default, cmd, ID_UITYPE)
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
      !use plotSettingParser
      !use command_utils, only:  parseCommandIntoTokens

      class(zoaplot_setting_manager) :: self
      character(len=*) :: strCmd

      self%numSettings = 0
      self%baseCmd = strCmd
      ! For now is always set outside of psm
      self%plotNum = -1
      
  end subroutine

      subroutine addWavelengthSetting(self) 
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: lambda

        lambda = sysConfig%refWavelengthIndex
        self%numSettings = self%numSettings + 1
        PRINT *, "numWavelengths is ", real(sysConfig%numWavelengths)
        call self%ps(self%numSettings)%initialize(SETTING_WAVELENGTH, & 
        & "Wavelength", real(lambda),1.0,real(sysConfig%numWavelengths), &
        & "SETWV", "SETWV "//trim(int2str(lambda)), UITYPE_SPINBUTTON)


      end subroutine 

      

      subroutine updateWavelengthSetting(self, newIdx) 
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: newIdx
        integer :: i

        !TODO:  Add error checking
        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_WAVELENGTH) then
            !call LogTermFOR("Found setting and changing to " //int2str(newIdx))
            self%ps(i)%default = real(newIdx)
            print *, "About to call int2str in update wv setting"
            self%ps(i)%fullCmd = trim("SETWV "//int2str(newIdx))
          end if
        end do

      end subroutine 


      function getFieldSetting(self) result(idxFld)
        use type_utils, only: str2int
        use strings
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: idxFld
        
        idxFld = INT(self%getSettingValueByCode(SETTING_FIELD))

      end function

      function getWavelengthSetting(self) result(wvIdx)
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



      subroutine addFieldSetting(self) 
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val
        integer :: fldPoint

        fldPoint = 1 ! Default to first field
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_FIELD, & 
        & "Field Point", real(fldPoint),1.0,real(sysConfig%numFields), &
        & "SETFLD", "SETFLD "//trim(int2str(fldPoint)), UITYPE_SPINBUTTON)


      
      end subroutine

      subroutine addZernikeSetting(self, defVal) 
        use global_widgets, only: sysConfig
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        character(len=*) :: defVal
        character(len=10) :: val

        val = defVal
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initializeStr(SETTING_ZERNIKE, & 
        & "Zernike Coefficients", val, "SETZERNC", UITYPE_ENTRY)

      
      end subroutine             
     
      subroutine updateZernikeSetting(self, newVal) 
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


      subroutine addDensitySetting(self, defaultVal, minVal, maxVal) 
        use type_utils, only: int2str
        implicit none
        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val, defaultVal, minVal, maxVal

        val = defaultVal
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_DENSITY, & 
        & "Density", real(val),real(minVal),real(maxVal), &
        & "SETDENS", "SETDENS "//trim(int2str(defaultVal)), UITYPE_SPINBUTTON)
      
      end subroutine  

      function getSettingValueByCode(self, setting_code) result(val)
        use global_widgets, only: sysConfig
        use type_utils, only: int2str
        implicit none

        class(zoaplot_setting_manager) :: self
        integer :: setting_code
        real :: val
        integer :: i

        do i=1,self%numSettings
          if (self%ps(i)%ID == setting_code) then
            val = self%ps(i)%default
            return
          end if
        end do

      end function


      subroutine updateSetting(self, setting_code, newVal)
        use global_widgets, only: sysConfig
        use type_utils, only: int2str, real2str
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
                !call LogTermFOR("Upating Setting int value")
                !call LogTermFOR("New value is "//int2str(newVal))
                self%ps(i)%default = real(newVal)
                self%ps(i)%fullCmd = trim(self%ps(i)%cmd)// &
                & " "//trim(int2str(newVal))
                !call LogTermFOR("Setting Code is "//int2str(setting_code))
                !call LogTermFOR("Defauls is "//int2str(INT(self%ps(i)%default)))
              type is (character(*))
                self%ps(i)%defaultStr = newVal
                self%ps(i)%fullCmd = trim(self%ps(i)%cmd)// &
                & " "//newVal       
                !call LogTermFOR("Updated Char val to "//self%ps(i)%defaultStr) 
                type is (double precision)
                self%ps(i)%default = real(newVal)
                self%ps(i)%fullCmd = trim(self%ps(i)%cmd)// &
                & " "//trim(real2str(newVal))                  
                type is (real)
                self%ps(i)%default = real(newVal)
                self%ps(i)%fullCmd = trim(self%ps(i)%cmd)// &
                & " "//trim(real2str(newVal))                    
            end select
          end if
        
        end do

      end subroutine 

      subroutine updateDensitySetting(self, newVal) 
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

      
      function getDensitySetting(self) result(denVal)
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