submodule (codeV_commands) mod_codev_plots
use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                       kdp_lens_end, kdp_chg, kdp_lens_cmd
implicit none
contains

    module function adjustImageFocus(x) result(f)
        use DATMAI, only: REG

        implicit none

        double precision, intent(in) :: x
        double precision :: f

        call PROCESKDP('THI SI '//real2str(x))
        call PROCESKDP('FOB 0; CAPFN; SHO RMSOPD')
        f = REG(9)
    end function adjustImageFocus

    !## cmd:      BES
    !## syntax:   BES
    !## category: Lens System Commands
    !## desc:     Find the best-focus image thickness (minimum RMS).
    !##
    module procedure findBestFocus
        use global_widgets, only: curr_par_ray_trace, sysConfig
        use command_utils, only : isInputNumber
        use DATLEN, only: COLRAY
        use algos

        implicit none

        character(len=80) :: tokens(40)
        real(kind=real64) :: relAngle
        double precision :: newThi, result
        integer :: numTokens, i, jj, numRays
        integer, allocatable :: fields(:)
        character(len=5) :: plotOrientation
        logical :: goodCmd, yzFlag

        call LogTermFOR("FindBestFocus plumbing works!")
        call tstBrent()

        result = brent(-50*1d0, 0*1d0, 50*1d0, adjustImageFocus, 1E-6*1d0, newThi)
        call LogTermFOR("RMS at best image focus is "//real2str(result,3))
        call PROCESKDP('THI SI '//real2str(newThi))
    end procedure findBestFocus

    !## cmd:      FAN
    !## syntax:   FAN
    !## category: Plotting
    !## desc:     Ray-fan plot (used within a plot loop).
    !##
    module procedure execFAN
        use command_utils, only : isInputNumber
        use global_widgets, only: sysConfig
        use DATLEN, only: COLRAY

        implicit none

        character(len=80) :: tokens(40)
        real(kind=real64) :: relAngle
        integer :: numTokens, ii, jj, numRays
        integer, allocatable :: fields(:)
        character(len=5) :: plotOrientation
        logical :: goodCmd, yzFlag

        goodCmd = .FALSE.
        yzFlag = .TRUE.

        select case(cmd_loop)
        case(VIE_LOOP)
        case(DRAW_LOOP)
            call LogTermFOR("Executing Custom Cmd "//iptStr)
            call parse(trim(iptStr), ' ', tokens, numTokens)

            do ii=2,numTokens
                if (isInputNumber(tokens(ii))) then
                    numRays = str2int(tokens(ii))
                    call LogTermFOR("Added numRays "//int2str(numRays))
                end if
                if (tokens(ii)(1:1) == 'f' .OR. tokens(ii)(1:1) == 'F') then
                    call LogTermFOR("Setting Fields from input "//tokens(ii))
                    if (isInputNumber(tokens(ii)(2:len(tokens)))) then
                        allocate(fields(1))
                        fields(1) = str2int(tokens(ii)(2:len(tokens)))
                        call LogTermFOR("Set field index to "//int2str(fields(1)))
                    end if
                end if
                if (tokens(ii) == 'YZ') yzFlag = .TRUE.
                if (tokens(ii) == 'XZ') yzFlag = .FALSE.
            end do

            if (.not. allocated(fields)) then
                allocate(fields(sysConfig%numFields))
                fields = (/(jj,jj=1,sysConfig%numFields)/)
            end if

            do ii=1,size(fields)
                COLRAY = sysConfig%fieldColorCodes(fields(ii))
                call PROCESKDP("FOB "//real2str(sysConfig%relativeFields(2,fields(ii))) &
                & //real2str(sysConfig%relativeFields(1,fields(ii))))
                do jj=1,numRays
                    relAngle = -1.0d0 + (jj-1)*(2.0d0)/(numRays-1)
                    if (yzFlag) then
                    else
                    end if
                end do
            end do
        case default
            call zoa_emit("FAN not used at base level", "black")
        end select
    end procedure execFAN

    module function checkForExistingPlot(tokens, psm, plot_code) result(plotExists)
        use zoa_ui_callbacks, only: query_existing_plot

        implicit none

        character(len=*), dimension(:) :: tokens
        type(zoaplot_setting_manager), intent(inout) :: psm
        integer, intent(in) :: plot_code
        logical :: plotExists
        integer, allocatable :: plotNum(:)

        plotExists = .FALSE.
        plotNum = cmd_parser_get_int_input_for_prefix('P', tokens)
        call query_existing_plot(plot_code, plotNum(1), curr_psm, plotExists)
        if (.not. plotExists) then
            psm%plotNum = plotNum(1)
        end if
    end function checkForExistingPlot

    module function initiatePlotLoop(iptStr, plot_code, psm) result(boolResult)
        implicit none

        character(len=*) :: iptStr
        integer :: plot_code
        type(zoaplot_setting_manager) :: psm
        logical :: boolResult
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        boolResult = .FALSE.
        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2 .AND. tokens(2)(1:1) == 'P') then
            plotExists = checkForExistingPlot(tokens(1:2), psm, plot_code)
            if (plotExists) then
                cmd_loop = plot_code
                boolResult = .TRUE.
                return
            end if
            cmd_loop = plot_code
            boolResult = .TRUE.
            curr_psm = psm
            return
        end if

        if (numTokens == 1) then
            cmd_loop = plot_code
            boolResult = .TRUE.
            curr_psm = psm
            return
        end if
    end function initiatePlotLoop

    !## cmd:      SSI
    !## syntax:   SSI x
    !## category: Plot Settings
    !## desc:     Set the scale of the active plot.
    !##
    module procedure setPlotScale
        use command_utils

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                select case(cmd_loop)
                case (ID_PLOTTYPE_RIM)
                    call curr_psm%updateSetting(SETTING_SCALE, str2real8(tokens(2)))
                case (SPO_LOOP)
                    call curr_psm%updateSetting(SETTING_SCALE, str2real8(tokens(2)))
                end select
            end if
        end if
    end procedure setPlotScale

    !## cmd:      MFR
    !## syntax:   MFR x
    !## category: Plot Settings
    !## desc:     Set the maximum frequency for the MTF plot.
    !##
    module procedure updateMaxFrequency
        use command_utils

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                select case(cmd_loop)
                case (ID_PLOTTYPE_MTF)
                    call curr_psm%updateSetting(SETTING_MAX_FREQUENCY, str2real8(tokens(2)))
                case default
                    call zoa_emit("Error:  This cmd must be entered after MTF and before GO to update value", "red")
                end select
            end if
        end if
    end procedure updateMaxFrequency

    !## cmd:      IFR
    !## syntax:   IFR x
    !## category: Plot Settings
    !## desc:     Set the frequency interval for the MTF plot.
    !##
    module procedure updateFrequencyInterval
        use command_utils

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                select case(cmd_loop)
                case (ID_PLOTTYPE_MTF)
                    call curr_psm%updateSetting(SETTING_FREQUENCY_INTERVAL, str2real8(tokens(2)))
                case default
                    call zoa_emit("Error:  This cmd must be entered after MTF and before GO to update value", "red")
                end select
            end if
        end if
    end procedure updateFrequencyInterval

    !## cmd:      RIM
    !## syntax:   RIM
    !## category: Plotting
    !## desc:     Ray-aberration (rim-ray fan) plot.
    !##
    module procedure execRayAberrationPlot
        implicit none

        logical :: boolResult
        type(zoaplot_setting_manager) :: psm

        call psm%initialize(trim(iptStr))
        call psm%addDensitySetting(64,8,128)
        call psm%addGenericSetting(SETTING_SCALE, 'Scale', 0.0, 0.0, 1000.0, 'SSI', 'SSI 0', UITYPE_SPINBUTTON)
        call psm%addWavelengthComboSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_RIM, psm)
        if (.not. boolResult) then
            call zoa_emit("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if
    end procedure execRayAberrationPlot

    !## cmd:      PMA
    !## syntax:   PMA
    !## category: Plotting
    !## desc:     Optical-path-difference (wavefront map) plot.
    !##
    module procedure execPMAPlot
        implicit none

        logical :: boolResult
        type(zoaplot_setting_manager) :: psm

        call psm%initialize(trim(iptStr))
        call psm%addDensitySetting(64,8,128)
        call psm%addFieldSetting()
        call psm%addWavelengthSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_OPD, psm)
        if (.not. boolResult) then
            call zoa_emit("Error in input. Should be either PMA or PMA PX, where X is plot num", "red")
        end if
    end procedure execPMAPlot

    !## cmd:      FIE
    !## syntax:   FIE
    !## category: Plotting
    !## desc:     Astigmatic field-curvature and distortion plot.
    !##
    module procedure execAstigFieldCurvDistPlot
        implicit none

        logical :: boolResult
        type(zoaplot_setting_manager) :: psm

        call psm%initialize(trim(iptStr))
        call psm%addAstigSettings()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_AST, psm)
        if (.not. boolResult) then
            call zoa_emit("Error in input. Should be either ASTFCDIST or ASTFCDIST PX, where X is plot num", "red")
        end if
    end procedure execAstigFieldCurvDistPlot

    !## cmd:      TOW
    !## syntax:   TOW ; ... ; GO
    !## category: Optimization
    !## desc:     Enter the tolerancing/operand-weighting loop.
    !##
    module procedure execTOW
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists
        type(zoaplot_setting_manager) :: psm

        if (cmd_loop == 0) then
            call parse(trim(iptStr), ' ', tokens, numTokens)
            call psm%initialize(trim(iptStr))
            cmd_loop = TOW_LOOP
            cmdTOW = ''

            if (numTokens == 2) then
                plotExists = checkForExistingPlot(tokens(1:2), psm, ID_TOW_TAB)
                if (plotExists) return
            end if
            curr_psm = psm
        end if
    end procedure execTOW

    !## cmd:      AUT
    !## syntax:   AUT ; <constraints/operands> ; GO
    !## category: Optimization
    !## desc:     Run local optimization; define operands and constraints inside the loop.
    !##
    module procedure execAUT
        use optim_types, only: optim

        if (cmd_loop == 0) then
            cmd_loop = AUT_LOOP
            optim%imp = .01_long
        else
            call zoa_emit("Cannot enter AUT loop as in another command loop", "red")
        end if
    end procedure execAUT

    !## cmd:      TAR
    !## syntax:   TAR ; ... ; GO
    !## category: Optimization
    !## desc:     Enter the optimization target loop (set operand targets/weights).
    !##
    module procedure execTAR
        implicit none

        if (cmd_loop == 0) then
            cmd_loop = TAR_LOOP
        else
            call zoa_emit("Cannot enter TAR loop as in another command loop", "red")
        end if
    end procedure execTAR

    !## cmd:      SPO
    !## syntax:   SPO v [w]
    !## category: Optimization
    !## desc:     RMS spot-size operand (weighted objective term).
    !##
    module procedure execSPO
        use command_utils, only : isInputNumber
        use optim_types

        implicit none

        type(zoaplot_setting_manager) :: psm
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (cmd_loop == AUT_LOOP .OR. cmd_loop == TAR_LOOP .OR. cmd_loop == CON_UPDATE_LOOP) then
            ! SPO [target [weight]] -> objective term (same form the unified
            ! parser accepts for the other evaluator names).  In the UPD CON
            ! loop the entry at idxConUpdate is UPDATED in place (this is the
            ! AUTUI edit path -- without it an SPO edit fell through to the
            ! spot-PLOT branch below instead of updating the merit entry).
            block
                real(long) :: t, w
                t = 0.0_long
                w = 1.0_long
                if (numTokens >= 2) then
                    if (isInputNumber(trim(tokens(2)))) t = str2real8(trim(tokens(2)))
                end if
                if (numTokens >= 3) then
                    if (isInputNumber(trim(tokens(3)))) w = str2real8(trim(tokens(3)))
                end if
                if (cmd_loop == CON_UPDATE_LOOP) then
                    call addMeritEntry('SPO', ID_ROLE_OBJECTIVE, t, weight=w, idxToUpdate=idxConUpdate)
                else
                    call addMeritEntry('SPO', ID_ROLE_OBJECTIVE, t, weight=w)
                end if
            end block
            return
        end if

        call psm%initialize(trim(iptStr))
        cmd_loop = SPO_LOOP

        if (numTokens == 2) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, ID_PLOTTYPE_SPOT_NEW)
            if (plotExists) return
        end if
        call psm%addSpotDiagramSettings()
        curr_psm = psm
    end procedure execSPO

    module procedure execSPO_old
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2) then
            if (tokens(2) == 'P1') then
                cmd_loop = SPO_LOOP
                return
            end if
        end if
        cmd_loop = SPO_LOOP
    end procedure execSPO_old

    !## cmd:      CIR
    !## syntax:   CIR [Sk] r
    !## category: Apertures
    !## desc:     Set a circular clear aperture of radius r on surface Sk (CIR EDG for an edge aperture).
    !##
    module procedure execCIR
        use command_utils, only : isInputNumber
        use type_utils, only: str2real8
        use mod_lens_data_manager, only: ldm
        use zoa_ui_callbacks, only: notify_replot

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, surfNum

        call parse(trim(iptStr), ' ', tokens, numTokens)

        ! Edge (physical) aperture.  Two forms, mirroring CIR:
        !   CIR EDG <value>      -> current surface (surface pointer), used in .zoa
        !   CIR EDG Sk <value>   -> explicit surface k
        if (trim(tokens(2)) == 'EDG') then
            block
                character(len=80) :: valStr
                if (numTokens == 3) then
                    surfNum = ldm%getSurfacePointer()
                    valStr  = tokens(3)
                else if (numTokens == 4 .and. isSurfCommand(trim(tokens(3)))) then
                    surfNum = getSurfNumFromSurfCommand(trim(tokens(3)))
                    valStr  = tokens(4)
                else
                    call zoa_emit("Error: use 'CIR EDG <value>' or 'CIR EDG Sk <value>'", "red")
                    return
                end if
                if (.not. isInputNumber(trim(valStr))) then
                    call zoa_emit("Error: unable to interpret edge aperture value '" &
                        & //trim(valStr)//"'", "red")
                    return
                end if
                call ldm%setEdgeSemiAperture(surfNum, str2real8(trim(valStr)))
                call notify_replot()
            end block
            return
        end if

        select case (numTokens)
        case (2)
            if (isInputNumber(trim(tokens(2)))) then
                call kdp_silent_begin()
                call kdp_lens_begin()
                call kdp_lens_cmd('CLAP', w1=str2real8(trim(tokens(2))), w2=0.0d0, &
                &                 w3=0.0d0, w4=str2real8(trim(tokens(2))))
                call kdp_lens_end()
                call kdp_silent_end()
            else
                call zoa_emit("Error: unable to intepret number for input argument "//trim(tokens(2)), "red")
                return
            end if
        case (3)
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
                if (isInputNumber(trim(tokens(3)))) then
                    call kdp_silent_begin()
                    call kdp_lens_begin()
                    call kdp_chg(surfNum)
                    call kdp_lens_cmd('CLAP', w1=str2real8(trim(tokens(3))))
                    call kdp_lens_end()
                    call kdp_silent_end()
                else
                    call zoa_emit("Error: unable to intepret number for input argument "//trim(tokens(3)), "red")
                    return
                end if
            else
                call zoa_emit("Error: unable to intepret surface number for input argument "//trim(tokens(2)), "red")
                return
            end if
        end select
    end procedure execCIR

    ! NUMRAYS/DRAWSI/DRAWSF/ELEV/AZI/ORIENT all dispatch here.  The keyword
    ! selects which lens-draw setting on curr_psm to update.  Only meaningful
    ! inside a VIE ; ... ; GO loop (where curr_psm is the lens-draw psm).
    !## cmd:      AZI
    !## syntax:   AZI a
    !## category: Plot Settings
    !## desc:     Set the azimuth angle of the 3D lens view.
    !##
    !## cmd:      DRAWSF
    !## syntax:   DRAWSF Sk
    !## category: Plot Settings
    !## desc:     Set the last surface drawn in the lens view.
    !##
    !## cmd:      DRAWSI
    !## syntax:   DRAWSI Sk
    !## category: Plot Settings
    !## desc:     Set the first surface drawn in the lens view.
    !##
    !## cmd:      ELEV
    !## syntax:   ELEV a
    !## category: Plot Settings
    !## desc:     Set the elevation angle of the 3D lens view.
    !##
    !## cmd:      NUMRAYS
    !## syntax:   NUMRAYS n
    !## category: Plot Settings
    !## desc:     Set the number of rays drawn in the lens view.
    !##
    !## cmd:      ORIENT
    !## syntax:   ORIENT ...
    !## category: Plot Settings
    !## desc:     Set the orientation (elevation/azimuth) of the lens view.
    !##
    module procedure adjustVieSettings
        use type_utils, only: str2int, str2real8
        use plot_setting_manager, only: orientId
        use iso_fortran_env, only: real64
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, k

        if (cmd_loop /= VIE_LOOP) return

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens < 2) return

        select case (trim(tokens(1)))
        case ('NUMRAYS')
            call curr_psm%updateSetting(ID_LENSDRAW_NUM_FIELD_RAYS, str2int(trim(tokens(2))))
        case ('DRAWSI')
            call curr_psm%updateSetting(ID_LENS_FIRSTSURFACE, str2int(trim(tokens(2))))
        case ('DRAWSF')
            call curr_psm%updateSetting(ID_LENS_LASTSURFACE, str2int(trim(tokens(2))))
        case ('ELEV')
            call curr_psm%updateSetting(ID_LENSDRAW_ELEVATION, real(str2real8(trim(tokens(2))), real64))
        case ('AZI')
            call curr_psm%updateSetting(ID_LENSDRAW_AZIMUTH, real(str2real8(trim(tokens(2))), real64))
        case ('ORIENT')
            ! Coupled: ORIENT <orient> [TAG value]...  The orientation is set,
            ! then each trailing "TAG value" pair is re-dispatched as its own
            ! child command (ELEV/AZI), so order is irrelevant and partial input
            ! (e.g. just "ORIENT Ortho") works.
            call curr_psm%updateSetting(ID_LENSDRAW_PLOT_ORIENTATION, orientId(trim(tokens(2))))
            do k = 3, numTokens-1, 2
                call adjustVieSettings(trim(tokens(k))//" "//trim(tokens(k+1)))
            end do
        end select
    end procedure adjustVieSettings

    ! ZRN WAV [fi] [wj] [zk] [dN]
    ! Fit the OPD error to Zernike polynomials (the same CAPFN + FITZERN path the
    ! Zernike-vs-Field plot uses) for field i, wavelength j, zoom k, at an NxN
    ! pupil density, and print a 36-row table (coefficient number, value in waves).
    ! Defaults: f1, reference wavelength, zoom 1, N=64.  Only the WAV qualifier is
    ! implemented for now; other qualifiers/fields/wavelengths come later.
    !## cmd:      ZRN
    !## syntax:   ZRN WAV [fi] [wj] [zk] [dN]
    !## category: Analysis
    !## desc:     Fit the OPD error to Zernike polynomials and print a 36-term coefficient table.
    !##
    module procedure execZRN
        use strings, only: parse
        use command_utils, only: isInputNumber
        use global_widgets, only: sysConfig
        use kdp_utils, only: OUTKDP
        use iso_fortran_env, only: real64
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, i
        integer :: fieldIdx, wavIdx, zoomIdx, densN
        character(len=200) :: fobStr
        character(len=80)  :: lineStr
        real(real64) :: X(1:96)
        COMMON/SOLU/X

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens < 2 .or. trim(tokens(2)) /= 'WAV') then
            call zoa_emit("ZRN: usage ZRN WAV [fi] [wj] [zk] [dN] "// &
            &             "(only the WAV qualifier is supported for now)", "red")
            return
        end if

        ! Defaults: field 1, reference wavelength, zoom 1, 64x64 pupil.
        fieldIdx = 1
        wavIdx   = sysConfig%getRefWavelengthIndex()
        zoomIdx  = 1
        densN    = 64

        ! Prefixed args f<i> w<j> z<k> d<N> (the front door upper-cases the line).
        do i=3,numTokens
            if (.not. isInputNumber(trim(tokens(i)(2:)))) cycle
            select case (tokens(i)(1:1))
            case ('F'); fieldIdx = str2int(trim(tokens(i)(2:)))
            case ('W'); wavIdx   = str2int(trim(tokens(i)(2:)))
            case ('Z'); zoomIdx  = str2int(trim(tokens(i)(2:)))
            case ('D'); densN    = str2int(trim(tokens(i)(2:)))
            end select
        end do

        if (fieldIdx < 1 .or. fieldIdx > sysConfig%numFields) then
            call zoa_emit("ZRN: field "//trim(int2str(fieldIdx))//" out of range (1.."// &
            &             trim(int2str(sysConfig%numFields))//")", "red")
            return
        end if
        if (wavIdx < 1 .or. wavIdx > sysConfig%numWavelengths) then
            call zoa_emit("ZRN: wavelength "//trim(int2str(wavIdx))//" out of range (1.."// &
            &             trim(int2str(sysConfig%numWavelengths))//")", "red")
            return
        end if
        if (densN < 2) then
            call zoa_emit("ZRN: pupil density N must be >= 2", "red")
            return
        end if

        ! Select zoom, aim at the field, compute the wavefront on the NxN pupil
        ! grid, and fit Zernikes at the requested wavelength.
        call PROCESKDP("POS "//trim(int2str(zoomIdx)))
        write(fobStr, *) "FOB ", sysConfig%relativeFields(2,fieldIdx), ' ', &
        &                        sysConfig%relativeFields(1,fieldIdx)
        call PROCESKDP(trim(fobStr))
        call PROCESKDP("CAPFN, "//trim(int2str(densN)))
        call PROCESKDP("FITZERN, "//trim(int2str(wavIdx)))

        ! X(1:96) holds the fit coefficients (COMMON/SOLU/X, same as LISTZERN).
        call OUTKDP("Zernike Coefficients (waves)  --  field "//trim(int2str(fieldIdx))// &
        &           ", wavelength "//trim(int2str(wavIdx))//", zoom "//trim(int2str(zoomIdx))// &
        &           ", pupil "//trim(int2str(densN))//"x"//trim(int2str(densN)))
        call OUTKDP("   Coeff             Value")
        do i=1,36
            write(lineStr, '(4X,I3,4X,F18.8)') i, X(i)
            call OUTKDP(trim(lineStr))
        end do

    end procedure execZRN

end submodule mod_codev_plots
