submodule (codeV_commands) mod_plot
implicit none
contains
    !## cmd:      PLO
    !## syntax:   PLO SUR | BAR
    !## category: Plot Settings
    !## desc:     PMA plot type: SUR = OPD surface map (default), BAR = bar chart
    !##           of the Fringe Zernike coefficients fitted to the wavefront.
    !##
    !## cmd:      ZFR
    !## syntax:   ZFR n
    !## category: Plot Settings
    !## desc:     PMA: number of Zernike terms shown in the BAR chart and Data
    !##           tab, 1-37 (default 37). The fit itself is always 37 terms.
    !##
    !## cmd:      AST
    !## syntax:   AST x
    !## category: Plot Settings
    !## desc:     FIE plot: field-curvature x-axis spans -x..+x (0 = autoscale).
    !##           The legacy astigmatism-table command is now ASTK.
    !##
    !## cmd:      DST
    !## syntax:   DST x
    !## category: Plot Settings
    !## desc:     FIE plot: distortion x-axis spans -x%..+x% (0 = autoscale).
    !##
    !## cmd:      NUMPTS
    !## syntax:   NUMPTS n
    !## category: Plot Settings
    !## desc:     Set the number of field samples for a vs-field plot.
    !##
    !## cmd:      ASTFLD
    !## syntax:   ASTFLD X | Y
    !## category: Plot Settings
    !## desc:     Select the field direction for the astigmatism plot.
    !##
    !## cmd:      TRAC
    !## syntax:   TRAC RECT | RAND | RING
    !## category: Plot Settings
    !## desc:     Set the spot-diagram ray-trace pattern for the active plot.
    !##
    !## cmd:      AIRY
    !## syntax:   AIRY ON | OFF
    !## category: Plot Settings
    !## desc:     Spot diagram: overlay the Airy disk on every field point.
    !##           Draws a black circle of radius 1.22*lambda*F/#, using the
    !##           working (image-space) F-number.  Off by default.  While it
    !##           is on the plot is scaled squarely so the disk is a circle.
    !##
    !## cmd:      NUMRINGS
    !## syntax:   NUMRINGS n
    !## category: Plot Settings
    !## desc:     Spot diagram: number of rings when the tracing method is
    !##           RING, 1-50 (default 20).  The pattern is evenly spaced radii
    !##           with six more rays on each successive ring.
    !##
    !## cmd:      RECTDENS
    !## syntax:   RECTDENS n
    !## category: Plot Settings
    !## desc:     Set the spot-diagram rectangular grid density for the active plot.
    !##
    !## cmd:      RSPH
    !## syntax:   RSPH [CHIEF | NOTILT | BEST]
    !## category: Plot Settings
    !## desc:     Reference sphere centre for wavefront calculations.
    !##           Inside a plot (e.g. PLTRMS) it sets that plot's own
    !##           Reference setting, applied only while the plot computes.
    !##           At the top level it sets the global default, as the legacy
    !##           KDP command did.  With no qualifier it reports the current
    !##           global setting.
    !##
    ! RSPH is context-aware: within a plot loop it is a per-plot setting (the
    ! plot brackets REFLOC around its own calculation and restores it), and at
    ! the top level it sets the legacy global directly.  It is registered as a
    ! zoaCmd, so the front door reaches this handler before the legacy
    ! NAMES/CMDER route -- the global branch below replicates what
    ! WAVSPOT4's RSPH did, rather than trying to feed the legacy parse globals.
    module procedure setReferenceSphere

        use strings, only: parse
        use DATLEN, only: REFLOC
        use DATSPD, only: DLLX, DLLY, DLLZ
        use plot_setting_manager, only: SETTING_RSPH

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, newRefLoc
        logical :: found

        call parse(trim(iptStr), ' ', tokens, numTokens)

        ! Bare RSPH, or the legacy query form "RSPH ?": report the current
        ! global setting, with the reference-sphere displacements the legacy
        ! command showed for the non-chief cases.
        if (numTokens < 2 .or. trim(tokens(2)) == '?') then
            select case (REFLOC)
            case (3)
                call zoa_emit("Reference sphere centre removes tilt (NOTILT)", "black")
                call zoa_emit("  X displacement = "//trim(real2str(DLLX)), "black")
                call zoa_emit("  Y displacement = "//trim(real2str(DLLY)), "black")
            case (4)
                call zoa_emit("Reference sphere centre removes tilt and focus (BEST)", "black")
                call zoa_emit("  X displacement = "//trim(real2str(DLLX)), "black")
                call zoa_emit("  Y displacement = "//trim(real2str(DLLY)), "black")
                call zoa_emit("  Z displacement = "//trim(real2str(DLLZ)), "black")
            case default
                call zoa_emit("Reference sphere centre lies on the chief ray (CHIEF)", "black")
            end select
            return
        end if

        ! Inside a plot: hand it to the active plot's setting manager.
        if (cmd_loop /= 0) then
            call curr_psm%applySettingCommand(trim(tokens(1)), trim(tokens(2)), found)
            if (found) return
            ! Plot has no Reference setting -- fall through and treat it as the
            ! global command, so RSPH still does something predictable.
        end if

        newRefLoc = -1
        select case (trim(tokens(2)))
        case ('CHIEF')
            newRefLoc = 1
        case ('NOTILT')
            newRefLoc = 3
        case ('BEST')
            newRefLoc = 4
        end select

        if (newRefLoc < 0) then
            call zoa_emit("RSPH takes CHIEF, NOTILT or BEST", "red")
            return
        end if

        REFLOC = newRefLoc
        DLLX = 0.0D0
        DLLY = 0.0D0
        DLLZ = 0.0D0

        select case (newRefLoc)
        case (3)
            call zoa_emit("Reference sphere centre will remove tilt", "black")
        case (4)
            call zoa_emit("Reference sphere centre will remove tilt and focus", "black")
        case default
            call zoa_emit("Reference sphere centre will lie on the chief ray", "black")
        end select

    end procedure

    ! Generic handler for plot settings whose keyword needs no bespoke logic.
    ! The active plot's setting manager already knows which setting owns the
    ! keyword, so one handler serves them all -- register any new setting
    ! keyword here rather than writing another near-identical routine.
    module procedure setPlotSettingGeneric

        use strings, only: parse

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: found

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            call curr_psm%applySettingCommand(trim(tokens(1)), trim(tokens(2)), found)
            if (.not. found) then
                call zoa_emit(trim(tokens(1))//" is not a setting of the active plot", "red")
            end if
        end if

    end procedure

    !## cmd:      SETFLD
    !## syntax:   SETFLD n
    !## category: Plot Settings
    !## desc:     Set the field point for the active plot (MTF, PSF, OPD).
    !##
    ! addFieldSetting (plot-setting-manager) has always emitted "SETFLD n" into
    ! the plot command, but no command was ever registered for it: every replot
    ! of an MTF/PSF/OPD plot answered "INVALID CMD LEVEL COMMAND", and changing
    ! the field point in those plots' settings did nothing.
    module procedure setPlotFieldPoint

        use command_utils, only: isInputNumber
        use plot_setting_manager, only: SETTING_FIELD
        use global_widgets, only: sysConfig

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, fld

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                ! Clamp to the current lens: a stored command (replot, or a
                ! restored .zin) can name a field the new lens does not have.
                fld = str2int(trim(tokens(2)))
                if (fld < 1) fld = 1
                if (fld > sysConfig%numFields) fld = sysConfig%numFields
                call curr_psm%updateSetting(SETTING_FIELD, fld)
            end if
        end if

    end procedure

    !## cmd:      SETWV
    !## syntax:   SETWV n
    !## category: Plot Settings
    !## desc:     Set the wavelength index for the active plot.
    !##
    module procedure setPlotWavelength
        
        use command_utils, only: isInputNumber

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 
       
        !TODO:  Add error checking (min and max wavelength within range)
        if (numTokens  == 2) then
            if (isInputNumber(tokens(2))) then
                call curr_psm%updateWavelengthSetting(str2int(trim(tokens(2))))
            end if
        end if
        
    end procedure

    !## cmd:      SETDENS
    !## syntax:   SETDENS n
    !## category: Plot Settings
    !## desc:     Set the sampling density for the active plot.
    !##
    module procedure setPlotDensity
        
        use command_utils, only: isInputNumber
     
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 
       
        !TODO:  Add error checking (min and max wavelength within range)
        if (numTokens  == 2) then
            if (isInputNumber(tokens(2))) then
                call curr_psm%updateDensitySetting(str2int(tokens(2)))
            end if
        end if
        
    end procedure  
    !## cmd:      SETZERNC
    !## syntax:   SETZERNC 5..9 | 9,16,25
    !## category: Plot Settings
    !## desc:     Set which Zernike terms the Zernike plot shows (range or list).
    !##
    module procedure setPlotZernikeCoefficients
        
        use command_utils, only: isInputNumber

        implicit none


        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

        !TODO:  Add error checking (min and max wavelength within range)
        if (numTokens  == 2) then
            if (.NOT.isInputNumber(tokens(2))) then
                call curr_psm%updateZernikeSetting(trim(tokens(2)))
            end if
        end if
        
    end procedure 

    !## cmd:      ZERN_TST
    !## syntax:   ZERN_TST ; [SETZERNC ...] ; GO
    !## category: Plotting
    !## desc:     Zernike-coefficient-vs-field plot.
    !##
    module procedure ZERN_TST
        !use ui_spot, only: spot_struct_settings, spot_settings
       ! use mod_plotopticalsystem

        implicit none
        type(zoaplot_setting_manager) :: psm

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        call psm%initialize(trim(iptStr))
        cmd_loop = ZERN_LOOP

        if (numTokens  == 2) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, ID_PLOTTYPE_ZERN_VS_FIELD)
            ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
            ! here but don't have a better soultion right now
           if (plotExists) return
        end if

        ! Set up settings
        call psm%addWavelengthSetting()
        ! Field points across the sweep (NUMPTS) and the pupil grid handed to
        ! CAPFN (SETDENS).  These used to be conflated: one "Density" setting
        ! was read as the field count while CAPFN ran bare and silently
        ! inherited whatever grid the previous CAPFN caller (eg PMA at 64) had
        ! left in KDP's CAPDEF global.
        call psm%addNumPointsSetting(10, 8, 21)
        call psm%addDensitySetting(32, 16, 512)
        call psm%addZernikeSetting("5..9")

        curr_psm = psm


    end procedure 


    ! psuedocode for checking plot input
    ! if user entered PX
    !    check if PX plot exists already
    !    if no, create new psm
    ! if user did not enter PX
    !   create new default settings
    ! in all cases update cmd_loop to plot loop

    module procedure execVIE
        !use ui_spot, only: spot_struct_settings, spot_settings
       ! use mod_plotopticalsystem

        implicit none
        type(zoaplot_setting_manager) :: psm

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        call psm%initialize(trim(iptStr))
        cmd_loop = VIE_LOOP

        !call LogTermFOR("About ot check VIE for existing plot")
        if (numTokens  == 2) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, ID_PLOTTYPE_LENSDRAW)
            !ßcall LogTermFOR("VIE plot exists is "//bool2str(plotExists))
            ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
            ! here but don't have a better soultion right now
           if (plotExists) return
        end if
        call psm%addLensDrawSettings()
        ! Set up settings
        !call psm%addWavelengthSetting()
        !call psm%addDensitySetting(10, 8, 21)
        curr_psm = psm
    end procedure  

    !## cmd:      PLTRMS
    !## syntax:   PLTRMS
    !## category: Plotting
    !## desc:     RMS wavefront/spot vs field plot.
    !##
    module procedure execRMSPlot
        implicit none

        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        !call psm%addDensitySetting(64,8,128)
        !1call psm%addFieldSetting()
        call psm%addRMSFieldSettings()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_RMSFIELD, psm)
        if(boolResult .EQV. .FALSE.) then
            call zoa_emit("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if

    end procedure    

    !## cmd:      PLOTTHO
    !## syntax:   PLOTTHO
    !## category: Plotting
    !## desc:     Third-order (Seidel) aberration bar chart.
    !##
    module procedure execSeidelBarChart
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        !call psm%addDensitySetting(64,8,128)
        !1call psm%addFieldSetting()
        call psm%addWavelengthSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_SEIDEL, psm)
        if(boolResult .EQV. .FALSE.) then
            call zoa_emit("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if

    end procedure    

    !## cmd:      PSF
    !## syntax:   PSF
    !## category: Plotting
    !## desc:     Point-spread-function plot.
    !##
    module procedure execPSF
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        call psm%addDensitySetting(64,8,128)
        call psm%addFieldSetting()
        call psm%addWavelengthSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_PSF, psm)
        if(boolResult .EQV. .FALSE.) then
            call zoa_emit("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if

    end procedure       
    
    !## cmd:      MTF
    !## syntax:   MTF
    !## category: Plotting
    !## desc:     Modulation-transfer-function plot.
    !##
    module procedure execMTF
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm
        real :: maxFreq


        call psm%initialize(trim(iptStr))
        !call psm%addDensitySetting(16,8,128)
        call psm%addPowerOfTwoImageSetting(16,16,128)

        call psm%addFieldSetting()
        call psm%addWavelengthSetting()
        maxFreq = getDefaultMaxFrequency()
        call psm%addGenericSetting(SETTING_MAX_FREQUENCY, 'Maximum Frequency [lp]', maxFreq, &
        & 0.0, 100000.0, 'MFR', 'MFR '//trim(real2str(maxFreq)), UITYPE_SPINBUTTON) 
        call psm%addGenericSetting(SETTING_FREQUENCY_INTERVAL, 'Frequency Interval [lp]', maxFreq/100.0, &
        & 0.0, 100000.0, 'IFR', 'IFR '//trim(real2str(maxFreq/100.0)), UITYPE_SPINBUTTON)          
        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_MTF, psm)
        if(boolResult .EQV. .FALSE.) then
            call zoa_emit("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if

    end procedure       

end submodule