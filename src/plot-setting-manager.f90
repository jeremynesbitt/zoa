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
    use type_utils
    use mod_zin_io
    implicit none

    !TODO:  move all this to zoa_ui 
    integer, parameter :: SETTING_WAVELENGTH = 1750
    integer, parameter :: SETTING_FIELD = 2
    integer, parameter :: SETTING_DENSITY = 1751
    ! FIE plot: manual x-axis half-width for the field-curvature and distortion
    ! panels (0 = autoscale).
    integer, parameter :: SETTING_AST_MAX = 1752
    integer, parameter :: SETTING_DST_MAX = 1753
    ! PMA (OPD) plot: plot type (surface map or Zernike bar chart) and how many
    ! Zernike terms to show.  FITZERN always fits 37 Fringe terms; ZFR selects
    ! how many of them are displayed.
    integer, parameter :: SETTING_PMA_PLOTTYPE = 1754
    integer, parameter :: SETTING_PMA_NZERN    = 1755
    integer, parameter :: ID_PMA_SUR = 1756
    integer, parameter :: ID_PMA_BAR = 1757
    integer, parameter :: PMA_MAX_ZERN = 37
    ! PLTRMS: reference sphere centre -- the legacy RSPH global (REFLOC).
    ! The option ids themselves live in zoa-ui.f90 with the other ID_* values.
    integer, parameter :: SETTING_RSPH = 1758
    ! Spot diagram: overlay the Airy disk
    integer, parameter :: SETTING_AIRY = 1759
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

      ! Coupled (compound) settings: one command carries several settings.
      !   OWNER  -> coupledIDs lists the child setting IDs it also sets
      !             (e.g. ORIENT owns ELEV, AZI); its fullCmd is the whole
      !             "ORIENT YZ ELEV 26.2 AZI 232.2" string.
      !   CHILD  -> ownerID is the owner's setting ID.  A child is NOT emitted
      !             on its own in generatePlotCommand (the owner carries it).
      integer, allocatable :: coupledIDs(:)
      integer :: ownerID = -1


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
    
    procedure :: addWavelengthComboSetting, getWavelengthComboSetting
    procedure :: addScaleSetting
    procedure, public, pass(self) :: getFieldSetting
    procedure, public, pass(self) :: generatePlotCommand
    procedure, public, pass(self) :: getSettingValueByCode
    procedure, public, pass(self) :: getCommandByCode
    procedure, public, pass(self) :: buildCoupledCmd

    procedure, public, pass(self) :: addFieldSetting
    procedure, public, pass(self) :: addDensitySetting   
    procedure, public, pass(self) :: getDensitySetting     
    procedure, public, pass(self) :: updateDensitySetting    
    procedure, public, pass(self) :: addZernikeSetting
    procedure, public, pass(self) :: updateZernikeSetting
    procedure, public, pass(self) :: getZernikeSetting_min_and_max
    procedure, public, pass(self) :: getZernikeSetting_list
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
    procedure, public, pass(self) :: addPMASettings
    procedure, public, pass(self) :: getPMASettings
    procedure, public, pass(self) :: addNumPointsSetting

    !RMS Settings
    procedure, public, pass(self) :: addRMSFieldSettings
    procedure, public, pass(self) :: getRMSFieldSettings
    procedure, public, pass(self) :: getRSPHSetting
    procedure, public, pass(self) :: getAirySetting

    procedure, public, pass(self) :: updateSetting, addPowerOfTwoImageSetting, getPowerOfTwoImageSetting
    procedure, public, pass(self) :: applySettingCommand

    procedure, public, pass(self) :: saveToBinary => psm_save_binary
    procedure, public, pass(self) :: loadFromBinary => psm_load_binary

    end type



contains


    subroutine init_setting(self, ID_SETTING, label, default, min, max, cmd, fullCmd, ID_UITYPE, set)

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

      use mod_lens_data_manager, only: ldm

      class (zoaplot_setting_manager) :: self

      call self%addLensDrawOrientationSettings()

      ! Add indvidual settings 
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_NUM_FIELD_RAYS, & 
      & "Num Rays Per Field", real(7),1.0,real(19), &
      & "NUMRAYS ", "NUMRAYS "//trim(int2str(7)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENS_FIRSTSURFACE, & 
      & "First Surface", real(0),0.0,real(ldm%getLastSurf()), &
      & "DRAWSI", "DRAWSI "//trim(int2str(0)), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENS_LASTSURFACE, & 
      & "Last Surface", real(ldm%getLastSurf()),real(1.0),real(ldm%getLastSurf()), &
      & "DRAWSF ", "DRAWSF "//trim(int2str(ldm%getLastSurf())), UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_ELEVATION, &
      & "Elevation", real(26.2),real(0.0),real(360.0), &
      & "ELEV", "ELEV "//trim(real2str(26.2)), UITYPE_SPINBUTTON)
      self%ps(self%numSettings)%ownerID = ID_LENSDRAW_PLOT_ORIENTATION   ! coupled to ORIENT

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_AZIMUTH, &
      & "Aziumuth", real(232.2),real(0.0),real(360.0), &
      & "AZI", "AZI "//trim(real2str(232.2)), UITYPE_SPINBUTTON)
      self%ps(self%numSettings)%ownerID = ID_LENSDRAW_PLOT_ORIENTATION   ! coupled to ORIENT

      ! Now that orientation + its children exist, build the coupled ORIENT cmd
      ! ("ORIENT YZ ELEV 26.2 AZI 232.2") so it round-trips on the first replot.
      call self%buildCoupledCmd(ID_LENSDRAW_PLOT_ORIENTATION)

      call self%addLensDrawScaleSettings()

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_LENSDRAW_AUTOSCALE_VALUE, & 
      & "Manual Scale Factor", 0.045,real(0.0),real(10000.0), &
      & "SSI", "SSI "//trim(real2str(0.045,5)), UITYPE_SPINBUTTON)             


      ! Toolbar settings
      call self%addPlotManipToolbarSettings()

    end subroutine

    subroutine addPlotManipToolbarSettings(self)
  
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
      & "ORIENT", "", UITYPE_COMBO, set=set)
      ! ORIENT is a coupled command: it also carries elevation and azimuth.
      self%ps(self%numSettings)%coupledIDs = [ID_LENSDRAW_ELEVATION, ID_LENSDRAW_AZIMUTH]


    end subroutine

    subroutine addLensDrawScaleSettings(self)

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

    subroutine addScaleSetting(self)

      class(zoaplot_setting_manager) :: self

      call self%addGenericSetting(SETTING_SCALE, 'Scale', 0.0, 0.0, 1000.0, 'SSI', 'SSI 0', UITYPE_SPINBUTTON)       

    end subroutine

    subroutine addSpotDiagramSettings(self)
      
      class (zoaplot_setting_manager) :: self
      type(idText) :: airySet(2)

     

      call self%addFieldSetting()
      call self%addWavelengthComboSetting()
      call self%addSpotCalculationSetting()
      call self%addScaleSetting()
     

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

      ! Airy disk overlay, off by default.
      airySet(1)%text = "No (OFF)"
      airySet(1)%id   = ID_AIRY_OFF
      airySet(2)%text = "Yes (ON)"
      airySet(2)%id   = ID_AIRY_ON

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(SETTING_AIRY, &
      & "Draw Airy Radius", real(ID_AIRY_OFF),0.0,0.0, &
      & "AIRY ", "AIRY OFF", UITYPE_COMBO, set=airySet)

    end subroutine

    subroutine addGenericSetting(self, ID_CODE, label, default, min, max, baseCmd, fullCmd, UI_TYPE)

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

    function getPowerOfTwoImageSetting(self) result(N)
      class (zoaplot_setting_manager) :: self
      integer :: N, idx

      ! 1 = 8, 2= 16, etc
      idx = self%getSettingValueByCode(ID_DENSITY_POWER_OF_TWO)
      ! TODO:  Integrate this with the set in a better way
      select case (idx)
      case (ID_16x16)
        N = 16
      case(ID_32x32)
        N = 32
      case(ID_64x64)
        N = 64
      case(ID_128x128)
        N = 128
      case(ID_256x256)
        N = 256
      case(ID_512x512)
        N = 512
      end select

    end function

    ! Since there is a lot of custom settings, write a method to get all settings
    subroutine getSpotDiagramSettings(self, idxField, idxLambda, idxSpotCalcMethod, nRect, nRand, nRing, plotScale)
      class (zoaplot_setting_manager) :: self
      integer, intent(inout) :: idxField, idxLambda, idxSpotCalcMethod
      integer, intent(inout) :: nRect, nRand, nRing
      real, intent(inout) :: plotScale

      idxField = self%getSettingValueByCode(SETTING_FIELD)
      idxLambda = self%getSettingValueByCode(ID_SETTING_WAVELENGTH_COMBO)
      
      idxSpotCalcMethod = self%getSettingValueByCode(ID_SPOT_TRACE_ALGO)
      !call LogTermFOR("In getSpotDiagramSettings idxSpotCalcMethod is "// &
      !&int2str(idxSpotCalcMethod))
      nRect = self%getSettingValueByCode(ID_SPOT_RECT_GRID)
      nRand = self%getSettingValueByCode(ID_SPOT_RAND_NUMRAYS)
      nRing = self%getSettingValueByCode(ID_SPOT_RING_NUMRINGS)
      plotScale = self%getSettingValueByCode(SETTING_SCALE)

    end subroutine

    function convertPowerOfTwoIndex(inVal, defVal) result(outVal)
      ! A more elegant way could be done (eg by dividing N times until 
      ! we get 2 and outVal should be N - 3), but this shoudl work...
      integer :: inVal, outVal
      integer, optional :: defVal

      select case (inVal)
      case (16)
        outVal = 1
      case (32)
        outVal = 2
      case (64)
        outVal = 3
      case(128)
        outVal = 4
      case (256)
        outVal = 5
      case (512)
        outVal = 6
      case default
        if(present(defVal)) outVal = defVal
      end select 


    end function

    subroutine addPowerOfTwoImageSetting(self, tgtVal, minVal, maxVal)

      class (zoaplot_setting_manager) :: self
      integer :: minVal, maxVal, minSet, maxSet, tgtVal, tgtSet
      type(idText) :: spotTrace(6)

      spotTrace(1)%text = "16x16"
      spotTrace(1)%id   = ID_16x16
      spotTrace(2)%text = "32x32"
      spotTrace(2)%id   = ID_32x32  
      spotTrace(3)%text = "64x64"
      spotTrace(3)%id   = ID_64x64
      spotTrace(4)%text = "128x128"
      spotTrace(4)%id   = ID_128x128 
      spotTrace(5)%text = "256x256"
      spotTrace(5)%id   = ID_256x256 
      spotTrace(6)%text = "512x512"
      spotTrace(6)%id   = ID_512x512   
      
      ! Need to go from N (eg 32) to the index in the set.  
      minSet = convertPowerOfTwoIndex(minVal, defval=1)
      maxSet = convertPowerOfTwoIndex(maxVal, defval=6)
      tgtSet = convertPowerOfTwoIndex(tgtVal, defval=1)
 

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_DENSITY_POWER_OF_TWO, & 
      & "Image Size (NxN)", real(spotTrace(tgtSet)%id),0.0,0.0, &
      & "NRD", "NRD "//trim(int2str(tgtVal)), UITYPE_COMBO, set=spotTrace(minSet:maxSet))      


    end subroutine
    
    subroutine addSpotCalculationSetting(self)
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

      ! Manual x-axis scale for the two panels; 0 keeps the autoscale.  The
      ! value is the half-width: AST .01 plots field curvature over -.01..+.01.
      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(SETTING_AST_MAX, &
      & "Max Astigmatism (0 for autoscale)", 0.0, 0.0, 1.0e6, &
      & "AST", "AST 0", UITYPE_SPINBUTTON)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(SETTING_DST_MAX, &
      & "Max Distortion in % (0 for autoscale)", 0.0, 0.0, 1.0e6, &
      & "DST", "DST 0", UITYPE_SPINBUTTON)

    end subroutine

    subroutine getAstigSettings(self, idxFieldXY, numPts, astMax, dstMax)

      class (zoaplot_setting_manager) :: self
      integer, intent(inout) :: idxFieldXY, numPts
      real, intent(out) :: astMax, dstMax

      idxFieldXY = INT(self%getSettingValueByCode(ID_AST_FIELDXY))
      numPts = INT(self%getSettingValueByCode(ID_NUMPOINTS))
      astMax = self%getSettingValueByCode(SETTING_AST_MAX)
      dstMax = self%getSettingValueByCode(SETTING_DST_MAX)

    end subroutine

    ! PMA (OPD) plot: plot type and Zernike term count.  The option labels
    ! carry the command code in parentheses so "PLO SUR" / "PLO BAR" resolve
    ! through applySettingCommand while the dropdown stays readable.
    subroutine addPMASettings(self)

      class (zoaplot_setting_manager) :: self
      type(idText) :: set(2)

      set(1)%text = "Surface Map (SUR)"
      set(1)%id = ID_PMA_SUR

      set(2)%text = "Zernike Bar Chart (BAR)"
      set(2)%id = ID_PMA_BAR

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(SETTING_PMA_PLOTTYPE, &
      & "Plot Type", real(ID_PMA_SUR), 0.0, 0.0, &
      & "PLO", "PLO SUR", UITYPE_COMBO, set=set)

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(SETTING_PMA_NZERN, &
      & "Number of Zernikes to Fit (default 37)", real(PMA_MAX_ZERN), 1.0, real(PMA_MAX_ZERN), &
      & "ZFR", "ZFR "//int2str(PMA_MAX_ZERN), UITYPE_SPINBUTTON)

    end subroutine

    ! "Number of Points" across the field (NUMPTS), as used by the vs-field
    ! plots.  Distinct from addDensitySetting (SETDENS), which is the pupil
    ! grid handed to CAPFN.
    subroutine addNumPointsSetting(self, defaultVal, minVal, maxVal)

      class (zoaplot_setting_manager) :: self
      integer, intent(in) :: defaultVal, minVal, maxVal

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(ID_NUMPOINTS, &
      & "Number of Points", real(defaultVal), real(minVal), real(maxVal), &
      & "NUMPTS ", "NUMPTS "//int2str(defaultVal), UITYPE_SPINBUTTON)

    end subroutine

    subroutine getPMASettings(self, plotType, nZern)

      class (zoaplot_setting_manager) :: self
      integer, intent(out) :: plotType, nZern

      plotType = INT(self%getSettingValueByCode(SETTING_PMA_PLOTTYPE))
      nZern    = INT(self%getSettingValueByCode(SETTING_PMA_NZERN))
      ! FITZERN always fits PMA_MAX_ZERN terms; keep the display count in range.
      if (nZern < 1) nZern = 1
      if (nZern > PMA_MAX_ZERN) nZern = PMA_MAX_ZERN

    end subroutine

    subroutine addRMSFieldSettings(self)

      class (zoaplot_setting_manager) :: self
      type(idText) :: set(2), set3(3)
   
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

      ! Pupil sampling used at each field point: an NxN grid.  16 is KDP's
      ! own default CAPFN grid, so the wavefront numbers are unchanged from
      ! when this was hard-coded.
      call self%addDensitySetting(16, 4, 128)

      ! Reference sphere centre (the legacy RSPH global).  Only meaningful for
      ! the wavefront data type -- spot size is measured from ray positions and
      ! is unaffected -- but shown for both.
      set3(1)%text = "Chief Ray (CHIEF)"
      set3(1)%id   = ID_RSPH_CHIEF
      set3(2)%text = "No Tilt (NOTILT)"
      set3(2)%id   = ID_RSPH_NOTILT
      set3(3)%text = "Best Focus (BEST)"
      set3(3)%id   = ID_RSPH_BEST

      self%numSettings = self%numSettings + 1
      call self%ps(self%numSettings)%initialize(SETTING_RSPH, &
      & "Reference", real(ID_RSPH_CHIEF),0.0,0.0, &
      & "RSPH ", "RSPH CHIEF", UITYPE_COMBO, set=set3)

      call self%addWavelengthSetting()


    end subroutine

    subroutine getRMSFieldSettings(self, iData, iLambda, numPoints)

      class (zoaplot_setting_manager) :: self
      integer, intent(inout) :: iData, iLambda, numPoints

      iData = self%getSettingValueByCode(ID_RMS_DATA_TYPE)
      iLambda = self%getSettingValueByCode(SETTING_WAVELENGTH)
      numPoints = self%getSettingValueByCode(ID_NUMPOINTS)

    end subroutine

    ! .true. when the spot diagram should overlay the Airy disk.
    function getAirySetting(self) result(drawAiry)
      class (zoaplot_setting_manager) :: self
      logical :: drawAiry

      drawAiry = (INT(self%getSettingValueByCode(SETTING_AIRY)) == ID_AIRY_ON)
    end function

    ! REFLOC value for this plot's Reference setting; defaults to the chief
    ! ray for a psm that has no such setting.
    function getRSPHSetting(self) result(refLocVal)
      class (zoaplot_setting_manager) :: self
      integer :: refLocVal

      refLocVal = refLocFromID(INT(self%getSettingValueByCode(SETTING_RSPH)))
    end function

    ! Option id -> the REFLOC code the legacy wavefront routines expect.
    function refLocFromID(idVal) result(refLocVal)
      integer, intent(in) :: idVal
      integer :: refLocVal

      select case (idVal)
      case (ID_RSPH_NOTILT)
        refLocVal = 3
      case (ID_RSPH_BEST)
        refLocVal = 4
      case default
        refLocVal = 1   ! chief ray
      end select
    end function

    
    subroutine initializeStr(self, ID_SETTING, label, default, cmd, ID_UITYPE)

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

      class(zoaplot_setting_manager) :: self
      character(len=*) :: strCmd

      self%numSettings = 0
      self%baseCmd = strCmd
      ! For now is always set outside of psm
      self%plotNum = -1
      
  end subroutine

      subroutine addWavelengthSetting(self) 
        use global_widgets, only: sysConfig

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


      subroutine addWavelengthComboSetting(self)
        use global_widgets, only: sysConfig

        class(zoaplot_setting_manager) :: self
        type(idText), dimension(sysConfig%numWavelengths+1) :: set
        integer :: lambda, i

        do i=1,sysConfig%numWavelengths
          set(i)%text = int2str(i)
          set(i)%id = wlIndices(i)
        end do         
        set(i)%text = 'All'
        set(i)%id = wlIndices(11)
    
          self%numSettings = self%numSettings + 1
          call self%ps(self%numSettings)%initialize(ID_SETTING_WAVELENGTH_COMBO, & 
          & "Wavelength", real(wlIndices(11)),0.0,0.0, &
          & "SETWV ", "SETWV ALL", UITYPE_COMBO, set=set)


      end subroutine

      function getWavelengthComboSetting(self) result (idxWL)
        class(zoaplot_setting_manager) :: self
        integer :: idxWL

        idxWL = INT(self%getSettingValueByCode(ID_SETTING_WAVELENGTH_COMBO))

      end function

      function getFieldSetting(self) result(idxFld)
        use strings

        class(zoaplot_setting_manager) :: self
        integer :: idxFld
        
        idxFld = INT(self%getSettingValueByCode(SETTING_FIELD))

      end function

      function getWavelengthSetting(self) result(wvIdx)
        use global_widgets, only: sysConfig
        use strings

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

      ! Parse the Zernike setting into an explicit list of term indices.
      ! Supports both notations:
      !   "5..9"        -> [5,6,7,8,9]   (inclusive range)
      !   "9,16,25,36"  -> [9,16,25,36]  (explicit list; spaces also allowed)
      ! Returns a 0-length array if the setting is missing/blank.  Out-of-range
      ! terms (<=0) are dropped so a bad token can't index past the coeff array.
      subroutine getZernikeSetting_list(self, zlist)
        use strings, only: parse
        class(zoaplot_setting_manager) :: self
        integer, allocatable, intent(out) :: zlist(:)
        integer :: locD, i, a, b, k, n
        character(len=80) :: s
        character(len=20) :: tokens(40)
        integer :: numTokens

        do i=1,self%numSettings
          if (self%ps(i)%ID == SETTING_ZERNIKE) then
            s = trim(adjustl(self%ps(i)%defaultStr))
            locD = index(s, '..')
            if (locD > 0) then
              a = str2int(s(1:locD-1))
              b = str2int(s(locD+2:len_trim(s)))
              if (b < a) then; k=a; a=b; b=k; end if
              n = 0
              if (a >= 1) n = b-a+1
              if (n < 0) n = 0
              allocate(zlist(n))
              do k=1,n
                zlist(k) = a+k-1
              end do
            else
              call parse(s, ', ', tokens, numTokens)
              n = 0
              do k=1,numTokens
                if (str2int(trim(tokens(k))) >= 1) n = n + 1
              end do
              allocate(zlist(n))
              n = 0
              do k=1,numTokens
                if (str2int(trim(tokens(k))) >= 1) then
                  n = n + 1
                  zlist(n) = str2int(trim(tokens(k)))
                end if
              end do
            end if
            return
          end if
        end do

        allocate(zlist(0))
      end subroutine


      subroutine addDensitySetting(self, defaultVal, minVal, maxVal)

        class(zoaplot_setting_manager), intent(inout) :: self
        integer:: val, defaultVal, minVal, maxVal

        val = defaultVal
        self%numSettings = self%numSettings + 1
        call self%ps(self%numSettings)%initialize(SETTING_DENSITY, & 
        & "Density", real(val),real(minVal),real(maxVal), &
        & "SETDENS", "SETDENS "//trim(int2str(defaultVal)), UITYPE_SPINBUTTON)
      
      end subroutine  

      ! Apply "<KEYWORD> <value>" to whichever setting owns KEYWORD.
      !
      ! Every plot setting already carries the command keyword it emits (ps%cmd),
      ! so a single lookup here serves any setting rather than needing a
      ! bespoke zoaCmds handler per keyword -- the omission of which is what
      ! made SETFLD/TRAC/RECTDENS/NRD answer "INVALID CMD LEVEL COMMAND" and
      ! silently do nothing.
      !
      ! Accepts either form a combo setting can appear in: the option text
      ! ("TRAC RECT", as first generated) or its numeric id ("TRAC 2.00000",
      ! as rewritten by updateSetting once the value is changed).
      subroutine applySettingCommand(self, keyword, valueStr, found)
        use command_utils, only: isInputNumber
        use strings, only: uppercase
        use type_utils, only: str2real8
        class(zoaplot_setting_manager), intent(inout) :: self
        character(len=*), intent(in) :: keyword, valueStr
        logical, intent(out) :: found
        integer :: i, k

        found = .FALSE.
        do i = 1, self%numSettings
          if (uppercase(trim(self%ps(i)%cmd)) /= uppercase(trim(keyword))) cycle
          found = .TRUE.

          if (isInputNumber(trim(valueStr))) then
            call self%updateSetting(self%ps(i)%ID, str2real8(trim(valueStr)))
            return
          end if

          ! Non-numeric: match the text against this setting's combo options --
          ! either the full option text, or a short code the option carries in
          ! trailing parentheses, eg "Surface Map (SUR)" matches SUR.  That lets
          ! a dropdown show a readable label while the command uses the code.
          if (allocated(self%ps(i)%set)) then
            do k = 1, size(self%ps(i)%set)
              if (uppercase(trim(self%ps(i)%set(k)%text)) == uppercase(trim(valueStr)) .or. &
                  uppercase(trim(parenCode(self%ps(i)%set(k)%text))) == uppercase(trim(valueStr))) then
                call self%updateSetting(self%ps(i)%ID, real(self%ps(i)%set(k)%ID, kind(1.0d0)))
                return
              end if
            end do
          end if

          ! Otherwise keep it as text (settings whose value really is a string).
          call self%updateSetting(self%ps(i)%ID, trim(valueStr))
          return
        end do

      contains

        ! The code inside a trailing "(...)" of an option label, or blank.
        function parenCode(label) result(code)
          character(len=*), intent(in) :: label
          character(len=len(label)) :: code
          integer :: lp, rp
          code = ' '
          rp = index(label, ')', BACK=.TRUE.)
          lp = index(label, '(', BACK=.TRUE.)
          if (lp > 0 .and. rp > lp + 1) code = label(lp+1:rp-1)
        end function parenCode

      end subroutine applySettingCommand

      function getSettingValueByCode(self, setting_code) result(val)
        use global_widgets, only: sysConfig

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

      ! Mirror of getSettingValueByCode, returning the setting's command keyword
      ! (e.g. 'ELEV').  Used to tag a child's value inside a coupled command, so
      ! the order of coupledIDs is cosmetic, not a parsing contract.
      function getCommandByCode(self, setting_code) result(cmdStr)
        class(zoaplot_setting_manager) :: self
        integer :: setting_code
        character(len=80) :: cmdStr
        integer :: i
        cmdStr = ''
        do i=1,self%numSettings
          if (self%ps(i)%ID == setting_code) then
            cmdStr = trim(self%ps(i)%cmd)
            return
          end if
        end do
      end function

      ! Rebuild the fullCmd of a coupled OWNER from the live values of itself and
      ! its children, e.g. "ORIENT YZ ELEV 26.2 AZI 232.2".  Children are tagged
      ! with their own command keyword (getCommandByCode) so generation and
      ! parsing share one source of truth (coupledIDs) and need no order contract.
      subroutine buildCoupledCmd(self, ownerID)
        class(zoaplot_setting_manager) :: self
        integer, intent(in) :: ownerID
        integer :: i, k, cid, oIdx
        character(len=140) :: str
        character(len=80)  :: tcmd
        real :: tval

        oIdx = -1
        do i=1,self%numSettings
          if (self%ps(i)%ID == ownerID) oIdx = i
        end do
        if (oIdx < 0) return
        if (.not. allocated(self%ps(oIdx)%coupledIDs)) return

        ! Owner keyword + owner value (combo owners render as a short text code)
        if (self%ps(oIdx)%uitype == UITYPE_COMBO) then
          str = trim(self%ps(oIdx)%cmd)//" "//trim(orientCode(int(self%ps(oIdx)%default)))
        else
          tval = self%ps(oIdx)%default
          str = trim(self%ps(oIdx)%cmd)//" "//trim(real2str(tval))
        end if

        ! Each child as "TAG value".  Use temporaries -- do NOT pass a function
        ! result directly to real2str's class(*) dummy (crashes under gfortran).
        do k=1,size(self%ps(oIdx)%coupledIDs)
          cid  = self%ps(oIdx)%coupledIDs(k)
          tcmd = self%getCommandByCode(cid)
          tval = self%getSettingValueByCode(cid)
          str  = trim(str)//" "//trim(tcmd)//" "//trim(real2str(tval))
        end do

        self%ps(oIdx)%fullCmd = trim(str)
      end subroutine

      ! Lens-draw orientation id <-> short command code (YZ/XZ/XY/Ortho).
      function orientCode(id) result(code)
        integer, intent(in) :: id
        character(len=8) :: code
        select case (id)
          case (ID_LENSDRAW_XZ_PLOT_ORIENTATION);    code = 'XZ'
          case (ID_LENSDRAW_XY_PLOT_ORIENTATION);    code = 'XY'
          case (ID_LENSDRAW_ORTHO_PLOT_ORIENTATION); code = 'ORTHO'
          case default;                              code = 'YZ'
        end select
      end function

      ! Case-insensitive: CLI args arrive uppercased by the command processor,
      ! but GUI callers may pass mixed case.
      function orientId(code) result(id)
        character(len=*), intent(in) :: code
        integer :: id
        integer :: j, c
        character(len=8) :: uc
        uc = ' '
        do j = 1, min(len_trim(code), len(uc))
          c = ichar(code(j:j))
          if (c >= 97 .and. c <= 122) c = c - 32
          uc(j:j) = char(c)
        end do
        select case (trim(uc))
          case ('XZ');    id = ID_LENSDRAW_XZ_PLOT_ORIENTATION
          case ('XY');    id = ID_LENSDRAW_XY_PLOT_ORIENTATION
          case ('ORTHO'); id = ID_LENSDRAW_ORTHO_PLOT_ORIENTATION
          case default;   id = ID_LENSDRAW_YZ_PLOT_ORIENTATION
        end select
      end function


      subroutine updateSetting(self, setting_code, newVal)
        use global_widgets, only: sysConfig

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
            ! If this setting participates in a coupled command, rebuild the
            ! owner's fullCmd from all current values (overrides the simple
            ! "cmd value" set above for an owner; refreshes the owner for a child).
            if (self%ps(i)%ownerID >= 0) call self%buildCoupledCmd(self%ps(i)%ownerID)
            if (allocated(self%ps(i)%coupledIDs)) call self%buildCoupledCmd(self%ps(i)%ID)
          end if

        end do

      end subroutine

      subroutine updateDensitySetting(self, newVal)
        use global_widgets, only: sysConfig

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
        use strings

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

    function generatePlotCommand(self) result(strOut)
      class(zoaplot_setting_manager):: self
      integer :: i
      character(len=1024) :: strOut

      strOut = trim(self%baseCmd)
      do i=1,self%numSettings
        ! Children of a coupled command are carried by the owner's fullCmd -- skip
        if (self%ps(i)%ownerID >= 0) cycle
        if (len_trim(self%ps(i)%fullCmd) > 0) then
          strOut = trim(strOut) // " ; "//self%ps(i)%fullCmd
        end if
      end do

      strOut = trim(strOut) // " ; GO"


    end function

    ! -----------------------------------------------------------------
    ! .zin binary serialization (WP1 -- pure data, no GTK).
    ! -----------------------------------------------------------------

    subroutine psm_save_binary(self, unit)
      use iso_fortran_env, only: int32
      class(zoaplot_setting_manager), intent(in) :: self
      integer, intent(in) :: unit
      integer :: i, k, nSet, nCoupled

      write(unit) int(self%numSettings, int32)
      call zin_write_str(unit, self%baseCmd)
      write(unit) int(self%plotNum, int32)

      do i = 1, self%numSettings
        write(unit) int(self%ps(i)%ID, int32)
        write(unit) int(self%ps(i)%uitype, int32)
        write(unit) self%ps(i)%min
        write(unit) self%ps(i)%max
        write(unit) self%ps(i)%default
        call zin_write_str(unit, self%ps(i)%prefix)
        call zin_write_str(unit, self%ps(i)%label)
        call zin_write_str(unit, self%ps(i)%defaultStr)
        call zin_write_str(unit, self%ps(i)%cmd)
        call zin_write_str(unit, self%ps(i)%fullCmd)
        write(unit) int(self%ps(i)%ownerID, int32)

        if (allocated(self%ps(i)%set)) then
          nSet = size(self%ps(i)%set)
        else
          nSet = 0
        end if
        write(unit) int(nSet, int32)
        do k = 1, nSet
          write(unit) int(self%ps(i)%set(k)%ID, int32)
          call zin_write_str(unit, self%ps(i)%set(k)%text)
        end do

        if (allocated(self%ps(i)%coupledIDs)) then
          nCoupled = size(self%ps(i)%coupledIDs)
        else
          nCoupled = 0
        end if
        write(unit) int(nCoupled, int32)
        do k = 1, nCoupled
          write(unit) int(self%ps(i)%coupledIDs(k), int32)
        end do
      end do
    end subroutine psm_save_binary

    subroutine psm_load_binary(self, unit, ios)
      use iso_fortran_env, only: int32
      class(zoaplot_setting_manager), intent(inout) :: self
      integer, intent(in) :: unit
      integer, intent(out) :: ios
      integer(int32) :: n32
      integer :: i, k, nSet, nCoupled

      ios = 0

      read(unit, iostat=ios) n32; if (ios /= 0) return
      self%numSettings = int(n32)
      call zin_read_str(unit, self%baseCmd, ios); if (ios /= 0) return
      read(unit, iostat=ios) n32; if (ios /= 0) return
      self%plotNum = int(n32)

      do i = 1, self%numSettings
        read(unit, iostat=ios) n32; if (ios /= 0) return
        self%ps(i)%ID = int(n32)
        read(unit, iostat=ios) n32; if (ios /= 0) return
        self%ps(i)%uitype = int(n32)
        read(unit, iostat=ios) self%ps(i)%min; if (ios /= 0) return
        read(unit, iostat=ios) self%ps(i)%max; if (ios /= 0) return
        read(unit, iostat=ios) self%ps(i)%default; if (ios /= 0) return
        call zin_read_str(unit, self%ps(i)%prefix, ios); if (ios /= 0) return
        call zin_read_str(unit, self%ps(i)%label, ios); if (ios /= 0) return
        call zin_read_str(unit, self%ps(i)%defaultStr, ios); if (ios /= 0) return
        call zin_read_str(unit, self%ps(i)%cmd, ios); if (ios /= 0) return
        call zin_read_str(unit, self%ps(i)%fullCmd, ios); if (ios /= 0) return
        read(unit, iostat=ios) n32; if (ios /= 0) return
        self%ps(i)%ownerID = int(n32)

        if (allocated(self%ps(i)%set)) deallocate(self%ps(i)%set)
        read(unit, iostat=ios) n32; if (ios /= 0) return
        nSet = int(n32)
        if (nSet > 0) then
          allocate(self%ps(i)%set(nSet))
          do k = 1, nSet
            read(unit, iostat=ios) n32; if (ios /= 0) return
            self%ps(i)%set(k)%ID = int(n32)
            call zin_read_str(unit, self%ps(i)%set(k)%text, ios); if (ios /= 0) return
          end do
        end if

        if (allocated(self%ps(i)%coupledIDs)) deallocate(self%ps(i)%coupledIDs)
        read(unit, iostat=ios) n32; if (ios /= 0) return
        nCoupled = int(n32)
        if (nCoupled > 0) then
          allocate(self%ps(i)%coupledIDs(nCoupled))
          do k = 1, nCoupled
            read(unit, iostat=ios) n32; if (ios /= 0) return
            self%ps(i)%coupledIDs(k) = int(n32)
          end do
        end if
      end do
    end subroutine psm_load_binary

end module