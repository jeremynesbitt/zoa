submodule (codeV_commands) mod_codev_plots
use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                       kdp_lens_end, kdp_chg, kdp_lens_cmd, kdp_fob, kdp_capfn, kdp_get
implicit none
contains

    module function adjustImageFocus(x) result(f)
        use DATMAI, only: REG

        implicit none

        double precision, intent(in) :: x
        double precision :: f

        call PROCESKDP('THI SI '//real2str(x))
        ! Explicit grid (KDP's default 16) so the focus search is independent of
        ! whatever CAPFN grid a plot last left in KDP's CAPDEF global.
        call kdp_fob(0.0d0)
        call kdp_capfn(16)
        f = kdp_get('RMSOPD', show=.true.)
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
        call LogTermUser("RMS at best image focus is "//real2str(result,3))
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
                call kdp_fob(sysConfig%relativeFields(2,fields(ii)), &
                &            sysConfig%relativeFields(1,fields(ii)))
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
    !## desc:     Ray-aberration (rim-ray fan) plot.  WFR Y plots the wave aberration (OPD, waves) instead.
    !##
    module procedure execRayAberrationPlot
        implicit none

        logical :: boolResult
        type(zoaplot_setting_manager) :: psm

        call psm%initialize(trim(iptStr))
        call psm%addDensitySetting(64,8,128)
        call psm%addGenericSetting(SETTING_SCALE, 'Scale', 0.0, 0.0, 1000.0, 'SSI', 'SSI 0', UITYPE_SPINBUTTON)
        call psm%addWavelengthComboSetting()
        call psm%addRimWfrSetting()

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
        ! CAPFN pupil grid: KDP needs NRD >= 16 (a smaller one gave no map
        ! and PMA crashed reading it); SETDENS below 16 snaps up to 16.
        call psm%addDensitySetting(64,16,128)
        call psm%addFieldSetting()
        call psm%addWavelengthSetting()
        call psm%addPMASettings()

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
            ! IMP is NOT reset here (it was: a value set with IMP or the
            ! optimizer window's General Settings tab was lost at every
            ! AUT;GO).  It returns to IMP_DEFAULT when a lens is loaded.
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
    !## cmd:      SPO
    !## syntax:   SPO [FLD | THF] ; [options] ; GO
    !## category: Plotting
    !## desc:     Spot diagram.  SPO / SPO FLD: one spot per field (or the chosen field),
    !##           at focus FFO.  SPO THF: the chosen field and wavelength at each focus
    !##           position FFO, FFO+IFO, ... (NFO of them) on one scale, in a row or a
    !##           column (ORI H|V).  The rays are moved to each focus along their slopes.
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

        ! SPO [FLD | THF] [Pn]: FLD (the default) a spot per field at focus
        ! FFO; THF one field at each focus position FFO, FFO+IFO, ... (NFO).
        ! THF tabs are their own plot type, so their numbers P1, P2 ...
        ! are separate from the field plots'.
        block
            use plot_functions, only: isPlotNumToken
            integer :: k, plotCode
            logical :: thf
            character(len=80) :: pTok
            thf = .false.
            pTok = ''
            do k = 2, numTokens
                select case (trim(tokens(k)))
                case ('FLD'); thf = .false.
                case ('THF'); thf = .true.
                case default
                    if (isPlotNumToken(trim(tokens(k)))) then
                        pTok = tokens(k)
                    else
                        call zoa_emit("Error:  SPO: expected SPO [FLD|THF], not "//trim(tokens(k)), "red")
                        return
                    end if
                end select
            end do
            plotCode = ID_PLOTTYPE_SPOT_NEW
            if (thf) plotCode = ID_PLOTTYPE_SPOT_THF

            call psm%initialize(trim(iptStr))
            cmd_loop = SPO_LOOP
            if (len_trim(pTok) > 0) then
                plotExists = checkForExistingPlot([character(len=80) :: tokens(1), pTok], psm, plotCode)
                if (plotExists) return
            end if
            call psm%addSpotDiagramSettings(throughFocus=thf)
            curr_psm = psm
        end block
    end procedure execSPO

    ! SPO options claimed only inside SPO: ORI H|V, the direction of the
    ! row of focus positions in SPO THF (the dropdown sends 1 or 2).
    !## cmd:      ANN
    !## syntax:   ANN RMS|SPO [Y|N]
    !## category: Plot Settings
    !## desc:     SPO: annotate each spot with its RMS size (ANN RMS, on by default)
    !##           and its full diameter, 100% of the rays about the centroid (ANN
    !##           SPO, off by default).  ANN LEN / ANN INI are not available.
    !##
    module procedure spoSetOption
        use plot_setting_manager, only: SETTING_SPOT_ORI, ID_SPOT_ORI_H, ID_SPOT_ORI_V, &
        &   SETTING_SPOT_ANN_RMS, SETTING_SPOT_ANN_SPO, ID_ANN_YES, ID_ANN_NO
        use plot_functions, only: spotIsThroughFocus
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, n

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (trim(tokens(1)) == 'ANN') then
            n = 0
            if (numTokens == 2) then
                n = ID_ANN_YES
            else if (numTokens == 3) then
                select case (trim(tokens(3)))
                case ('Y', 'YES', '1', '1.0'); n = ID_ANN_YES
                case ('N', 'NO', '2', '2.0');  n = ID_ANN_NO
                end select
            end if
            if (n == 0 .or. numTokens < 2) then
                call zoa_emit("Error:  SPO: expected ANN RMS|SPO [Y|N]", "red")
                return
            end if
            select case (trim(tokens(2)))
            case ('RMS'); call curr_psm%updateSetting(SETTING_SPOT_ANN_RMS, n)
            case ('SPO'); call curr_psm%updateSetting(SETTING_SPOT_ANN_SPO, n)
            case ('LEN', 'INI')
                if (n == ID_ANN_YES) call zoa_emit("SPO: ANN "//trim(tokens(2))// &
                &   " is not available (no lens-name or date annotation)", "red")
            case default
                call zoa_emit("Error:  SPO: expected ANN RMS|SPO [Y|N]", "red")
            end select
            return
        end if
        if (.not. spotIsThroughFocus(curr_psm)) then
            call zoa_emit("Error:  ORI applies to SPO THF (focus positions in a row or column)", "red")
            return
        end if
        n = 0
        if (numTokens == 2) then
            select case (trim(tokens(2)))
            case ('H', 'HOR', '1', '1.0'); n = ID_SPOT_ORI_H
            case ('V', 'VER', '2', '2.0'); n = ID_SPOT_ORI_V
            end select
        end if
        if (n == 0) then
            call zoa_emit("Error:  SPO: expected ORI H|V", "red")
            return
        end if
        call curr_psm%updateSetting(SETTING_SPOT_ORI, n)
    end procedure spoSetOption

    !## cmd:      MULTIPLOT
    !## syntax:   MULTIPLOT rows cols ['title'] [Pn]
    !## category: Plotting
    !## desc:     Start a page of subplots (rows x cols cells, up to 6 x 6).  Each analysis that follows (RIM, SPO, MTF, PSF, ... ; GO) fills the next cell, or the one SUBPLOT chose; a GO with no analysis open draws the page.  The page redraws when the lens changes.
    !##
    module procedure execMULTIPLOT
        use plot_composer, only: comp_active, comp_begin, comp_set_page
        use plot_functions, only: multiplot_page_begin, isPlotNumToken
        use command_utils, only: isInputNumber
        implicit none
        character(len=1024) :: tokens(40), rest
        character(len=200) :: title
        character(len=1100) :: script
        integer :: numTokens, k, q1, q2, rows, cols, plotNum, objIdx
        logical :: replot

        if (comp_active()) then
            call zoa_emit("Error:  MULTIPLOT: a page is already open; finish it with GO", "red")
            return
        end if
        if (cmd_loop /= 0) then
            call zoa_emit("Error:  MULTIPLOT: close the open command with GO first", "red")
            return
        end if

        ! the title: the first quoted string (any case)
        title = ''
        rest = iptStr
        q1 = scan(iptStr, "'"//'"')
        if (q1 > 0) then
            q2 = index(iptStr(q1+1:), iptStr(q1:q1))
            if (q2 == 0) then
                call zoa_emit("Error:  MULTIPLOT: the title has no closing quote", "red")
                return
            end if
            title = iptStr(q1+1:q1+q2-1)
            rest = iptStr(1:q1-1)//' '//iptStr(q1+q2+1:)
        end if

        call parse(trim(rest), ' ', tokens, numTokens)
        rows = 0
        cols = 0
        plotNum = -1
        do k = 2, numTokens
            if (isPlotNumToken(trim(tokens(k)))) then
                ! (P then digits, checked by isPlotNumToken)
                read(tokens(k)(2:len_trim(tokens(k))), *) plotNum
            else if (isInputNumber(trim(tokens(k))) .and. rows == 0) then
                rows = nint(str2real8(trim(tokens(k))))
            else if (isInputNumber(trim(tokens(k))) .and. cols == 0) then
                cols = nint(str2real8(trim(tokens(k))))
            else
                rows = -1
            end if
        end do
        if (rows < 1 .or. rows > 6 .or. cols < 1 .or. cols > 6) then
            call zoa_emit("Error:  expected MULTIPLOT rows cols ['title'] (1 to 6 rows and columns)", "red")
            return
        end if

        call multiplot_page_begin(plotNum, objIdx, replot)
        write(script, '(A,I0,1X,I0)') 'MULTIPLOT ', rows, cols
        if (len_trim(title) > 0) script = trim(script)//" '"//trim(title)//"'"
        if (plotNum >= 1) script = trim(script)//' P'//trim(int2str(plotNum))
        if (.not. comp_begin(rows, cols, trim(script), trim(title))) then
            call zoa_emit("Error:  MULTIPLOT: pages nested too deeply", "red")
            return
        end if
        call comp_set_page(objIdx, plotNum, replot)
    end procedure execMULTIPLOT

    !## cmd:      SUBPLOT
    !## syntax:   SUBPLOT r c [rspan cspan]
    !## category: Plotting
    !## desc:     In a MULTIPLOT page: the next analysis fills the cell at row r, column c (spanning rspan rows and cspan columns).
    !##
    module procedure execSUBPLOT
        use plot_composer, only: comp_active, comp_set_slot
        use command_utils, only: isInputNumber
        implicit none
        character(len=80) :: tokens(40)
        character(len=120) :: msg
        integer :: numTokens, k, v(4)

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (.not. comp_active()) then
            call zoa_emit("Error:  SUBPLOT is used in a MULTIPLOT page", "red")
            return
        end if
        if (numTokens /= 3 .and. numTokens /= 5) then
            call zoa_emit("Error:  expected SUBPLOT r c [rspan cspan]", "red")
            return
        end if
        v = 1
        do k = 2, numTokens
            if (.not. isInputNumber(trim(tokens(k)))) then
                call zoa_emit("Error:  expected SUBPLOT r c [rspan cspan]", "red")
                return
            end if
            v(k-1) = nint(str2real8(trim(tokens(k))))
        end do
        if (.not. comp_set_slot(v(1), v(2), v(3), v(4), msg)) call zoa_emit("Error:  "//trim(msg), "red")
    end procedure execSUBPLOT

    ! The GO that closes a MULTIPLOT page (no analysis open): draw it.  Its
    ! script -- everything from MULTIPLOT to here -- becomes the tab's
    ! command, which redraws the page on a lens change.
    module procedure multiplotGo
        use plot_composer, only: comp_script, comp_get_page, comp_page, comp_end, comp_count
        use plot_functions, only: multiplot_page_end
        implicit none
        character(len=:), allocatable :: script
        integer :: objIdx, plotNum
        logical :: replot

        script = comp_script()//'; GO'
        call comp_get_page(objIdx, plotNum, replot)
        if (comp_count() == 0) call zoa_emit("MULTIPLOT: the page has no subplots", "red")
        if (len(script) > 1040) then
            call zoa_emit("MULTIPLOT: the page's commands are longer than 1040 characters; "// &
            &             "it is drawn but cannot be redrawn", "red")
            call multiplot_page_end(comp_page(), objIdx, replot, plotNum, ' ')
        else
            call multiplot_page_end(comp_page(), objIdx, replot, plotNum, script)
        end if
        call comp_end()
    end procedure multiplotGo

    !## cmd:      WFR
    !## syntax:   WFR Y|N
    !## category: Plot Settings
    !## desc:     RIM: plot the wave aberration (OPD in waves) instead of the transverse ray aberration.
    !##
    ! (the settings dropdown sends its entry id: 1 = No, 2 = Yes)
    module procedure rimSetOption
        use plot_setting_manager, only: SETTING_RIM_WFR, ID_RIM_WFR_NO, ID_RIM_WFR_YES
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, n

        call parse(trim(iptStr), ' ', tokens, numTokens)
        n = 0
        if (numTokens == 2) then
            select case (trim(tokens(2)))
            case ('Y', 'YES', '2', '2.0'); n = ID_RIM_WFR_YES
            case ('N', 'NO', '1', '1.0');  n = ID_RIM_WFR_NO
            end select
        end if
        if (n == 0) then
            call zoa_emit("Error:  RIM: expected WFR Y|N", "red")
            return
        end if
        call curr_psm%updateSetting(SETTING_RIM_WFR, n)
    end procedure rimSetOption

    !## cmd:      LSA
    !## syntax:   LSA [Y|N|x]
    !## category: Plot Settings
    !## desc:     FIE: add the longitudinal spherical aberration panel (x: its +-x scale).
    !##
    !## cmd:      ZFO
    !## syntax:   ZFO Y|N
    !## category: Plot Settings
    !## desc:     FIE: field curves measured from the axial focus instead of the image surface.
    !##
    ! FIE options: LSA [Y|N|x], ZFO Y|N, FFD N (full field display: not yet)
    module procedure fieSetOption
        use plot_setting_manager, only: SETTING_FIE_LSA, SETTING_FIE_ZFO, ID_FIE_ZFO_NO, ID_FIE_ZFO_YES
        use command_utils, only: isInputNumber
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens
        character(len=8) :: a

        call parse(trim(iptStr), ' ', tokens, numTokens)
        a = ''
        if (numTokens >= 2) a = tokens(2)
        if (numTokens > 2) then
            call zoa_emit("Error:  expected "//trim(tokens(1))//" Y|N"// &
            &             merge("|x", "  ", trim(tokens(1)) == 'LSA'), "red")
            return
        end if
        select case (trim(tokens(1)))
        case ('LSA')
            select case (trim(a))
            case ('', 'Y', 'YES'); call curr_psm%updateSetting(SETTING_FIE_LSA, -1.0)
            case ('N', 'NO');      call curr_psm%updateSetting(SETTING_FIE_LSA, 0.0)
            case default
                ! (nested: no short-circuit .and. -- str2real8 of a word aborts)
                if (isInputNumber(trim(a))) then
                    if (str2real8(trim(a)) >= -1.0_long) then
                        call curr_psm%updateSetting(SETTING_FIE_LSA, str2real8(trim(a)))
                        return
                    end if
                end if
                call zoa_emit("Error:  expected LSA Y|N|x (x > 0: the plotted maximum)", "red")
            end select
        case ('ZFO')
            select case (trim(a))
            case ('', 'Y', 'YES', '2', '2.0'); call curr_psm%updateSetting(SETTING_FIE_ZFO, ID_FIE_ZFO_YES)
            case ('N', 'NO', '1', '1.0');      call curr_psm%updateSetting(SETTING_FIE_ZFO, ID_FIE_ZFO_NO)
            case default; call zoa_emit("Error:  expected ZFO Y|N", "red")
            end select
        case ('FFD')
            select case (trim(a))
            case ('N', 'NO')
            case default
                call zoa_emit("FFD: the full-field display is not available yet (FFD N)", "red")
            end select
        end select
    end procedure fieSetOption

    !## cmd:      FRE
    !## syntax:   FRE x
    !## category: Plot Settings
    !## desc:     MTF THF: the spatial frequency whose modulation is plotted through focus.
    !##
    !## cmd:      PLO FOC
    !## syntax:   PLO FOC [Y|N]  |  PLO FRE [Y|N]
    !## category: Plot Settings
    !## desc:     Inside MTF: PLO FOC Y charts the modulation at one frequency
    !##           (FRE) through the focus positions FFO/IFO/NFO (same as MTF THF);
    !##           PLO FRE Y (or PLO FOC N) charts it against frequency, the default.
    !##           The field and NRD choices made so far are kept.
    !##
    ! MTF options claimed only inside MTF: NRD n (pupil grid 16, 32, 64 or
    ! 128; the settings dropdown sends its entry id, also accepted), PLO
    ! FOC|FRE (switch between the frequency and through-focus plots) and, in
    ! MTF THF, FRE x.
    module procedure mtfSetOption
        use plot_setting_manager, only: SETTING_MTF_FRE, SETTING_FIELD
        use command_utils, only: isInputNumber
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, n
        logical :: wantThf

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (trim(tokens(1)) == 'PLO') then
            ! PLO FOC [Y|N] / PLO FRE [Y|N]: which of the two plots
            wantThf = (cmd_loop == ID_PLOTTYPE_MTF_THF)
            n = 0
            if (numTokens >= 2 .and. numTokens <= 3) then
                if (numTokens == 2) tokens(3) = 'Y'
                select case (trim(tokens(3)))
                case ('Y', 'YES', 'N', 'NO')
                    select case (trim(tokens(2)))
                    case ('FOC')
                        n = 1; wantThf = (tokens(3)(1:1) == 'Y')
                    case ('FRE')
                        n = 1
                        if (tokens(3)(1:1) == 'Y') then
                            wantThf = .false.
                        else if (cmd_loop == ID_PLOTTYPE_MTF) then
                            call zoa_emit("MTF: PLO FRE N leaves nothing to plot; use PLO FOC Y "// &
                            &             "for the through-focus plot", "red")
                            return
                        end if
                    end select
                end select
            end if
            if (n == 0) then
                call zoa_emit("Error:  MTF: expected PLO FOC [Y|N] or PLO FRE [Y|N]", "red")
                return
            end if
            if (wantThf .eqv. (cmd_loop == ID_PLOTTYPE_MTF_THF)) return
            block
                real :: fld, nrd
                fld = curr_psm%getSettingValueByCode(SETTING_FIELD)
                nrd = curr_psm%getSettingValueByCode(ID_DENSITY_POWER_OF_TWO)
                if (wantThf) then
                    call execMTF('MTF THF')
                else
                    call execMTF('MTF')
                end if
                call curr_psm%updateSetting(SETTING_FIELD, fld)
                call curr_psm%updateSetting(ID_DENSITY_POWER_OF_TWO, nrd)
            end block
            return
        end if
        if (trim(tokens(1)) == 'NRD') then
            n = 0
            if (numTokens == 2) then
                if (isInputNumber(trim(tokens(2)))) n = nint(str2real8(trim(tokens(2))))
            end if
            select case (n)
            case (16);  n = ID_16x16
            case (32);  n = ID_32x32
            case (64);  n = ID_64x64
            case (128); n = ID_128x128
            case (ID_16x16:ID_128x128)
            case default; n = 0
            end select
            if (n == 0) then
                call zoa_emit("Error:  MTF: expected NRD 16, 32, 64 or 128", "red")
            else
                call curr_psm%updateSetting(ID_DENSITY_POWER_OF_TWO, n)
            end if
            return
        end if
        if (numTokens == 2) then
            if (isInputNumber(trim(tokens(2)))) then
                if (str2real8(trim(tokens(2))) > 0.0_long) then
                    call curr_psm%updateSetting(SETTING_MTF_FRE, str2real8(trim(tokens(2))))
                    return
                end if
            end if
        end if
        call zoa_emit("Error:  MTF THF: expected FRE frequency (greater than 0)", "red")
    end procedure mtfSetOption

    !## cmd:      DIS
    !## syntax:   DIS [INT] [Y|N]
    !## category: Plot Settings
    !## desc:     PMA: colour map (DIS Y, the default), interferogram fringes
    !##           (DIS INT Y, one fringe per wave) or none (DIS N, for contours alone).
    !##           PSF: DIS Y is accepted (the plot is always drawn).
    !##
    !## cmd:      CON
    !## syntax:   CON [SUP] [Y|N]
    !## category: Plot Settings
    !## desc:     PMA: contour lines over the map, numbered (CON Y) or not (CON SUP Y).
    !##
    !## cmd:      ADW
    !## syntax:   ADW [TIL|FOC] x_tilt [y_tilt [focus [piston]]]
    !## category: Plot Settings
    !## desc:     PMA: add tilt, focus (rho^2) and piston to the wavefront, in waves
    !##           at the pupil edge; TIL removes the best-fit tilt first, FOC the
    !##           best-fit tilt and focus.  ADW 0 turns it off.
    !##
    !## cmd:      RAN COL
    !## syntax:   RAN COL SPE | RGB [r [g [b]]]
    !## category: Plot Settings
    !## desc:     PMA: colour scale -- spectral rainbow (SPE, the default) or a ramp
    !##           from black to the colour r g b (0..1 each; RGB 1 1 1 is grey).
    !##
    !## cmd:      LIS
    !## syntax:   LIS [Y|N]
    !## category: Plot Settings
    !## desc:     PMA, PSF, MTF: accepted for CODE V compatibility; the listing is
    !##           the plot's Data tab, which is always written.
    !##
    ! Options claimed by the dispatcher inside PMA (DIS CON ADW RAN LIS PLO
    ! ZFR) and PSF/MTF (LIS, and DIS for PSF).  Combo settings arrive from the
    ! settings panel as their ids (DIS 2, CON 3).
    module procedure pmaSetOption
        use plot_setting_manager, only: SETTING_PMA_DIS, SETTING_PMA_CON, SETTING_PMA_ADW, &
        &   SETTING_PMA_COL, ID_PMA_DIS_MAP, ID_PMA_DIS_INT, ID_PMA_DIS_NONE, &
        &   ID_PMA_CON_NO, ID_PMA_CON_YES, ID_PMA_CON_SUP
        use command_utils, only: isInputNumber
        implicit none
        character(len=80) :: tokens(40), q, yn
        integer :: numTokens, i, n
        character(len=200) :: rest
        logical :: isPMA

        call parse(trim(iptStr), ' ', tokens, numTokens)
        isPMA = (cmd_loop == ID_PLOTTYPE_OPD)
        select case (trim(tokens(1)))
        case ('LIS')
            if (numTokens > 2) then
                call zoa_emit("Error:  expected LIS [Y|N]", "red"); return
            end if
            if (numTokens == 2) then
                if (.not. isYesNo(tokens(2))) then
                    call zoa_emit("Error:  expected LIS [Y|N]", "red")
                end if
            end if
        case ('DIS')
            ! DIS [INT] [Y|N], or the combo's id
            q = ''; yn = 'Y'
            do i = 2, numTokens
                if (trim(tokens(i)) == 'INT') then
                    q = 'INT'
                else
                    yn = tokens(i)
                end if
            end do
            if (.not. isPMA) then
                if (len_trim(q) > 0 .or. .not. isYesNo(yn)) then
                    call zoa_emit("Error:  PSF: expected DIS [Y|N]", "red")
                else if (yn(1:1) == 'N') then
                    call zoa_emit("PSF: the plot is always drawn (DIS N ignored)", "red")
                end if
                return
            end if
            n = 0
            if (numTokens == 2 .and. isInputNumber(trim(tokens(2)))) then
                n = nint(str2real8(trim(tokens(2))))
                if (n < ID_PMA_DIS_MAP .or. n > ID_PMA_DIS_NONE) n = 0
            else if (numTokens <= 3 .and. isYesNo(yn)) then
                if (yn(1:1) == 'N') then
                    n = ID_PMA_DIS_NONE
                    if (len_trim(q) > 0) n = ID_PMA_DIS_MAP
                else
                    n = ID_PMA_DIS_MAP
                    if (len_trim(q) > 0) n = ID_PMA_DIS_INT
                end if
            end if
            if (n == 0) then
                call zoa_emit("Error:  PMA: expected DIS [INT] [Y|N]", "red"); return
            end if
            call curr_psm%updateSetting(SETTING_PMA_DIS, n)
        case ('CON')
            q = ''; yn = 'Y'
            do i = 2, numTokens
                if (trim(tokens(i)) == 'SUP') then
                    q = 'SUP'
                else
                    yn = tokens(i)
                end if
            end do
            n = 0
            if (numTokens == 2 .and. isInputNumber(trim(tokens(2)))) then
                n = nint(str2real8(trim(tokens(2))))
                if (n < ID_PMA_CON_NO .or. n > ID_PMA_CON_SUP) n = 0
            else if (numTokens <= 3 .and. isYesNo(yn)) then
                if (yn(1:1) == 'N') then
                    n = ID_PMA_CON_NO
                    if (len_trim(q) > 0) n = ID_PMA_CON_YES   ! CON SUP N: numbers back on
                else
                    n = ID_PMA_CON_YES
                    if (len_trim(q) > 0) n = ID_PMA_CON_SUP
                end if
            end if
            if (n == 0) then
                call zoa_emit("Error:  PMA: expected CON [SUP] [Y|N]", "red"); return
            end if
            call curr_psm%updateSetting(SETTING_PMA_CON, n)
        case ('ADW')
            ! ADW [TIL|FOC] x_tilt [y_tilt [focus [piston]]]
            rest = ''
            n = 0
            do i = 2, numTokens
                if (i == 2 .and. (trim(tokens(2)) == 'TIL' .or. trim(tokens(2)) == 'FOC')) then
                    rest = trim(tokens(2))
                else if (isInputNumber(trim(tokens(i)))) then
                    n = n + 1
                    rest = trim(rest)//' '//trim(tokens(i))
                else
                    n = 99
                end if
            end do
            if (n < 1 .or. n > 4) then
                call zoa_emit("Error:  PMA: expected ADW [TIL|FOC] x_tilt [y_tilt [focus [piston]]]", "red")
                return
            end if
            call curr_psm%updateSetting(SETTING_PMA_ADW, trim(adjustl(rest)))
        case ('RAN')
            n = 0
            if (numTokens >= 3) then
                if (trim(tokens(2)) == 'COL') then
                    if (trim(tokens(3)) == 'SPE' .and. numTokens == 3) then
                        n = 1; rest = 'SPE'
                    else if (trim(tokens(3)) == 'RGB' .and. numTokens <= 6) then
                        n = 1; rest = 'RGB'
                        do i = 4, numTokens
                            if (.not. isInputNumber(trim(tokens(i)))) then
                                n = 0
                            else if (str2real8(trim(tokens(i))) < 0.0_long .or. &
                            &        str2real8(trim(tokens(i))) > 1.0_long) then
                                n = 0
                            else
                                rest = trim(rest)//' '//trim(tokens(i))
                            end if
                        end do
                    end if
                else if (trim(tokens(2)) == 'SCA') then
                    call zoa_emit("PMA: RAN SCA is not available yet (the colour scale spans the data)", "red")
                    return
                end if
            end if
            if (n == 0) then
                call zoa_emit("Error:  PMA: expected RAN COL SPE or RAN COL RGB [r [g [b]]] (0..1)", "red")
                return
            end if
            call curr_psm%updateSetting(SETTING_PMA_COL, trim(rest))
        case ('PLO')
            ! CODE V PLO Y|N is the oblique projection plot; Zoa's PLO SUR|BAR
            ! chooses the map or the Zernike bar chart.
            if (numTokens == 2) then
                if (isYesNo(tokens(2))) then
                    if (tokens(2)(1:1) == 'Y') call zoa_emit( &
                    &   "PMA: the oblique projection plot is not available; the map is drawn", "red")
                    return
                end if
            end if
            call setPlotSettingGeneric(iptStr)
        case ('ZFR')
            ! ZFR [EXP|EXS] n
            if (numTokens == 3) then
                if (trim(tokens(2)) == 'EXP' .or. trim(tokens(2)) == 'EXS') then
                    call zoa_emit("PMA: ZFR "//trim(tokens(2))//": the fit is to the pupil map's "// &
                    &   "own grid; the qualifier is ignored", "red")
                    call setPlotSettingGeneric('ZFR '//trim(tokens(3)))
                    return
                end if
            end if
            call setPlotSettingGeneric(iptStr)
        end select

    contains

        logical function isYesNo(t)
            character(len=*), intent(in) :: t
            select case (trim(t))
            case ('Y', 'YES', 'N', 'NO'); isYesNo = .true.
            case default; isYesNo = .false.
            end select
        end function isYesNo

    end procedure pmaSetOption

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

        ! Obscuration: CIR OBS [Sk] r -- shared with REX/ELX/ADX/...
        if (any(tokens(2:max(2,numTokens)) == 'OBS')) then
            call setApertureValue(iptStr)
            return
        end if

        ! Edge (physical) aperture.  Two forms, mirroring CIR:
        !   CIR EDG <value>      -> current surface (surface pointer), used in .zoa
        !   CIR EDG Sk <value>   -> explicit surface k
        ! (CODE V's order, CIR Sk EDG r, is accepted too)
        if (numTokens == 4) then
            if (trim(tokens(3)) == 'EDG' .and. isSurfCommand(trim(tokens(2)))) then
                tokens(3) = tokens(2); tokens(2) = 'EDG'
            end if
        end if
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
                ! No Sk: the current surface (the one being built in a .zoa;
                ! KDP's own SURF lags behind after an SI line).
                call kdp_silent_begin()
                call kdp_lens_begin()
                call kdp_chg(ldm%getSurfacePointer())
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
        ! Clamp the drawn surface range to the CURRENT lens.  A stored plot
        ! command (replot, or a restored .zin) can carry a range from a larger
        ! lens -- e.g. "DRAWSF 61" replayed after a macro's LEN NEW left three
        ! surfaces -- which made the drawing code walk off the end of the lens
        ! and emit "SURFACE NUMBER ... IS BEYOND LEGAL RANGE" once per ray.
        case ('DRAWSI')
            call curr_psm%updateSetting(ID_LENS_FIRSTSURFACE, clampToLens(str2int(trim(tokens(2)))))
        case ('DRAWSF')
            ! "DRAWSF I" (CODE V's image surface), or the image surface's
            ! number, means "to the image": stored as I, so a replot after
            ! surfaces are added draws them too.  Any other surface stays put.
            block
                use mod_lens_data_manager, only: ldm
                integer :: sf
                if (trim(tokens(2)) == 'I') then
                    sf = ldm%getLastSurf()
                else
                    sf = clampToLens(str2int(trim(tokens(2))))
                end if
                call curr_psm%updateSetting(ID_LENS_LASTSURFACE, sf)
                if (sf == ldm%getLastSurf()) call curr_psm%setSettingCommand(ID_LENS_LASTSURFACE, 'DRAWSF I')
            end block
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

    contains

        ! Restrict a surface index to [0, last surface of the current lens].
        integer function clampToLens(s) result(sc)
            use mod_lens_data_manager, only: ldm
            integer, intent(in) :: s
            integer :: lastS
            lastS = ldm%getLastSurf()
            sc = s
            if (sc < 0) sc = 0
            if (lastS >= 0 .and. sc > lastS) sc = lastS
        end function clampToLens

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
        call kdp_fob(sysConfig%relativeFields(2,fieldIdx), sysConfig%relativeFields(1,fieldIdx))
        call kdp_capfn(densN)
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

    ! Diagnostic self-check for the .zin binary serializers.  Builds small
    ! synthetic zoaplot / zoaPlot3d / multiplot / zoaplot_setting_manager
    ! objects, round-trips them through a temp .zin file, and compares every
    ! field for an exact match.  Headless-safe: no GTK calls anywhere in this
    ! path.  Test hook only -- not a user-facing command.
    module procedure execZINTEST
        use kdp_utils, only: OUTKDP
        use zoa_plot, only: zoaplot, zoaPlot3d, multiplot, POS_UPPER_RIGHT, MAX_PLOT_SERIES
        use plplot, only: pl_test_flt
        use plot_setting_manager, only: zoaplot_setting_manager, plot_setting
        use mod_zin_io, only: zin_write_header, zin_read_header, &
        &                     zin_write_str, zin_read_str_alloc
        use zoa_file_handler, only: getTempDirectory, delete_file
        use iso_c_binding, only: c_associated
        use, intrinsic :: iso_c_binding, only: c_null_ptr
        implicit none

        type(zoaplot) :: p1
        type(zoaPlot3d) :: p3
        type(multiplot) :: mplt, mpltL
        type(zoaplot_setting_manager) :: psm, psmL
        class(zoaplot), pointer :: pp1L
        class(zoaplot), pointer :: pp3Lbase

        real :: x(5), y(5), x2(5), y2(5)
        real :: gx(4), gy(4), gz(16)
        character(len=30) :: legendArr(1)
        character(len=200) :: fname
        integer :: unit, ios, i, numTabsOut
        logical :: ok
        integer :: failCount
        integer, parameter :: BIGLEN = 200000
        character(len=:), allocatable :: bigText, bigTextL

        failCount = 0

        ! Non-uniform so a truncated or short read cannot accidentally match.
        allocate(character(len=BIGLEN) :: bigText)
        do i = 1, BIGLEN
            bigText(i:i) = achar(33 + mod(i, 90))
        end do

        x  = [1.0, 2.0, 3.0, 4.0, 5.0]
        y  = [1.0, 4.0, 9.0, 16.0, 25.0]
        x2 = [1.0, 2.0, 3.0, 4.0, 5.0]
        y2 = [5.0, 4.0, 3.0, 2.0, 1.0]

        call p1%initialize(c_null_ptr, x, y, xlabel="X Axis", ylabel="Y Axis", title="ZINTEST Plot")
        call p1%addXYPlot(x2, y2)
        legendArr(1) = "Series A"
        call p1%addLegend(legendArr)
        call p1%addText("hello zintest", POS_UPPER_RIGHT)
        call p1%setYScale(2.5_pl_test_flt)
        call p1%setXScale(1.5_pl_test_flt)
        call p1%addScaleBar(POS_UPPER_RIGHT)

        ! Series capacity: plotdatalist is fixed size, and overrunning it used
        ! to abort the program (ZRNFLD with 10+ terms).  Adding past the cap
        ! must be refused, not fatal, and must not corrupt numSeries.
        block
            type(zoaplot) :: pcap
            integer :: s
            call pcap%initialize(c_null_ptr, x, y, xlabel="X", ylabel="Y", title="cap")
            do s = 1, MAX_PLOT_SERIES + 5
                call pcap%addXYPlot(x2, y2)
            end do
            call ckI("seriesCap.numSeries", MAX_PLOT_SERIES, pcap%numSeries)
        end block

        gx = [1.0, 2.0, 3.0, 4.0]
        gy = [1.0, 2.0, 3.0, 4.0]
        gz = [(real(i), i = 1, 16)]
        call p3%init3d(c_null_ptr, gx, gy, gz, 4, 4, xlabel="gx", ylabel="gy", title="ZINTEST 3D")

        call mplt%initialize(c_null_ptr, 1, 2)
        call mplt%addBottomPanel("Big Label", "Little Label", "Legend Text")
        mplt%m_title = "ZINTEST Multiplot"
        call mplt%set(1, 1, p1)
        call mplt%set(1, 2, p3)

        call psm%initialize("ZINTEST P1")
        call psm%addLensDrawSettings()

        fname = trim(getTempDirectory())//'zintest.zin'

        open(newunit=unit, file=trim(fname), access='stream', form='unformatted', &
             status='replace', iostat=ios)
        if (ios /= 0) then
            call OUTKDP("ZINTEST: FAIL cannot open temp file for write")
            return
        end if
        call zin_write_header(unit, 1)
        call psm%saveToBinary(unit)
        call mplt%saveToBinary(unit)
        ! A Data-tab table (a dense PMA pupil map is ~200 KB) must survive
        ! whole: fixed-size buffers used to cut the restored tab off
        ! mid-number at 65536 characters.
        call zin_write_str(unit, bigText)
        close(unit)

        open(newunit=unit, file=trim(fname), access='stream', form='unformatted', &
             status='old', iostat=ios)
        if (ios /= 0) then
            call OUTKDP("ZINTEST: FAIL cannot reopen temp file for read")
            return
        end if

        call zin_read_header(unit, numTabsOut, ok)
        if (.not. ok) then
            call OUTKDP("ZINTEST: FAIL header not ok")
            failCount = failCount + 1
        else
            call ckI("header.numTabs", 1, numTabsOut)
        end if

        call psmL%loadFromBinary(unit, ios)
        if (ios /= 0) then
            call OUTKDP("ZINTEST: FAIL psm load ios")
            failCount = failCount + 1
        end if

        call mpltL%loadFromBinary(unit, ios)
        if (ios /= 0) then
            call OUTKDP("ZINTEST: FAIL mplt load ios")
            failCount = failCount + 1
        end if

        call zin_read_str_alloc(unit, bigTextL, ios)
        if (ios /= 0) then
            call OUTKDP("ZINTEST: FAIL long string load ios")
            failCount = failCount + 1
        else
            call ckI("longString.len", len(bigText), len(bigTextL))
            if (len(bigText) == len(bigTextL)) then
                if (bigText /= bigTextL) then
                    call OUTKDP("ZINTEST: FAIL longString content mismatch")
                    failCount = failCount + 1
                end if
            end if
        end if

        close(unit)
        call delete_file(trim(fname))

        ! ---- compare zoaplot_setting_manager ----
        call ckI("psm.numSettings", psm%numSettings, psmL%numSettings)
        call ckS("psm.baseCmd", psm%baseCmd, psmL%baseCmd)
        call ckI("psm.plotNum", psm%plotNum, psmL%plotNum)
        if (psm%numSettings == psmL%numSettings) then
            do i = 1, psm%numSettings
                call ckPlotSetting(i, psm%ps(i), psmL%ps(i))
            end do
        end if

        ! ---- compare multiplot scalars ----
        call ckI("mplt.m_rows", mplt%m_rows, mpltL%m_rows)
        call ckI("mplt.m_cols", mplt%m_cols, mpltL%m_cols)
        call ckS("mplt.m_title", mplt%m_title, mpltL%m_title)
        call ckL("mplt.m_hasTitle", mplt%m_hasTitle, mpltL%m_hasTitle)
        call ckI("mplt.width", mplt%width, mpltL%width)
        call ckI("mplt.height", mplt%height, mpltL%height)
        call ckL("mplt.hasBottomPanel", mplt%hasBottomPanel, mpltL%hasBottomPanel)
        call ckS("mplt.bottomPanelBigLabel", mplt%bottomPanelBigLabel, mpltL%bottomPanelBigLabel)
        call ckS("mplt.bottomPanelLittleLabel", mplt%bottomPanelLittleLabel, mpltL%bottomPanelLittleLabel)
        call ckS("mplt.bottomPanelLegend", mplt%bottomPanelLegend, mpltL%bottomPanelLegend)

        ! ---- compare cell (1,1): basic zoaplot ----
        pp1L => mpltL%get(1, 1)
        if (.not. associated(pp1L)) then
            call OUTKDP("ZINTEST: FAIL mplt cell(1,1) not associated")
            failCount = failCount + 1
        else
            call ckZoaplotBase("p1", p1, pp1L)
            ! Regression guard: a deserialized plot must carry a NULL drawing
            ! area.  It is never serialized (it is a live GTK handle) and the
            ! owning multiplot assigns the real one before drawing; if it comes
            ! back as garbage, drawPlot's g_object_get_data(self%area, ...)
            ! segfaults on the first cell -- which crashed Zoa on startup while
            ! restoring plots from a .zin.
            if (c_associated(pp1L%area)) then
                call OUTKDP("ZINTEST: FAIL loaded plot has non-null area")
                failCount = failCount + 1
            end if
        end if

        ! ---- compare cell (1,2): zoaPlot3d ----
        pp3Lbase => mpltL%get(1, 2)
        if (.not. associated(pp3Lbase)) then
            call OUTKDP("ZINTEST: FAIL mplt cell(1,2) not associated")
            failCount = failCount + 1
        else
            select type (pp3Lbase)
            type is (zoaPlot3d)
                call ckZoaplotBase("p3", p3%zoaplot, pp3Lbase%zoaplot)
                call ckI("p3.numSeries(3d)", p3%numSeries, pp3Lbase%numSeries)
                if (p3%numSeries == pp3Lbase%numSeries) then
                    do i = 1, p3%numSeries
                        call ckPlotdata3d(i, p3%plotdatalist3d(i), pp3Lbase%plotdatalist3d(i))
                    end do
                end if
            class default
                call OUTKDP("ZINTEST: FAIL mplt cell(1,2) wrong dynamic type")
                failCount = failCount + 1
            end select
        end if

        if (failCount == 0) then
            call OUTKDP("ZINTEST: PASS")
            call OUTKDP("ZINTEST: stats psm.numSettings="//trim(int2str(psm%numSettings))// &
            &           " p1.numSeries="//trim(int2str(p1%numSeries))// &
            &           " p3.numSeries="//trim(int2str(p3%numSeries)))
        else
            call OUTKDP("ZINTEST: "//trim(int2str(failCount))//" FAILURE(S)")
        end if

    contains

        subroutine ckI(name, a, b)
            character(len=*), intent(in) :: name
            integer, intent(in) :: a, b
            if (a /= b) then
                call OUTKDP("ZINTEST: FAIL "//trim(name))
                failCount = failCount + 1
            end if
        end subroutine ckI

        subroutine ckL(name, a, b)
            character(len=*), intent(in) :: name
            logical, intent(in) :: a, b
            if (a .neqv. b) then
                call OUTKDP("ZINTEST: FAIL "//trim(name))
                failCount = failCount + 1
            end if
        end subroutine ckL

        subroutine ckS(name, a, b)
            character(len=*), intent(in) :: name, a, b
            if (trim(a) /= trim(b)) then
                call OUTKDP("ZINTEST: FAIL "//trim(name))
                failCount = failCount + 1
            end if
        end subroutine ckS

        subroutine ckR(name, a, b)
            character(len=*), intent(in) :: name
            real, intent(in) :: a, b
            if (a /= b) then
                call OUTKDP("ZINTEST: FAIL "//trim(name))
                failCount = failCount + 1
            end if
        end subroutine ckR

        subroutine ckR8(name, a, b)
            character(len=*), intent(in) :: name
            real(pl_test_flt), intent(in) :: a, b
            if (a /= b) then
                call OUTKDP("ZINTEST: FAIL "//trim(name))
                failCount = failCount + 1
            end if
        end subroutine ckR8

        subroutine ckRArr(name, a, b)
            character(len=*), intent(in) :: name
            real, intent(in) :: a(:), b(:)
            if (size(a) /= size(b)) then
                call OUTKDP("ZINTEST: FAIL "//trim(name)//" size")
                failCount = failCount + 1
            else if (any(a /= b)) then
                call OUTKDP("ZINTEST: FAIL "//trim(name)//" values")
                failCount = failCount + 1
            end if
        end subroutine ckRArr

        subroutine ckR8Arr(name, a, b)
            character(len=*), intent(in) :: name
            real(pl_test_flt), intent(in) :: a(:), b(:)
            if (size(a) /= size(b)) then
                call OUTKDP("ZINTEST: FAIL "//trim(name)//" size")
                failCount = failCount + 1
            else if (any(a /= b)) then
                call OUTKDP("ZINTEST: FAIL "//trim(name)//" values")
                failCount = failCount + 1
            end if
        end subroutine ckR8Arr

        subroutine ckZoaplotBase(tag, a, b)
            character(len=*), intent(in) :: tag
            class(zoaplot), intent(in) :: a, b
            integer :: k

            if (allocated(a%x) .neqv. allocated(b%x)) then
                call OUTKDP("ZINTEST: FAIL "//trim(tag)//".x allocated mismatch")
                failCount = failCount + 1
            else if (allocated(a%x)) then
                call ckRArr(trim(tag)//".x", a%x, b%x)
            end if

            if (allocated(a%y) .neqv. allocated(b%y)) then
                call OUTKDP("ZINTEST: FAIL "//trim(tag)//".y allocated mismatch")
                failCount = failCount + 1
            else if (allocated(a%y)) then
                call ckRArr(trim(tag)//".y", a%y, b%y)
            end if

            call ckS(trim(tag)//".title", a%title, b%title)
            call ckS(trim(tag)//".xlabel", a%xlabel, b%xlabel)
            call ckS(trim(tag)//".ylabel", a%ylabel, b%ylabel)
            call ckS(trim(tag)//".labelFontColor", a%labelFontColor, b%labelFontColor)
            call ckS(trim(tag)//".xPlotCode", a%xPlotCode, b%xPlotCode)
            call ckS(trim(tag)//".yPlotCode", a%yPlotCode, b%yPlotCode)
            call ckL(trim(tag)//".useGridLines", a%useGridLines, b%useGridLines)
            call ckI(trim(tag)//".dataColorCode", a%dataColorCode, b%dataColorCode)
            call ckI(trim(tag)//".numSeries", a%numSeries, b%numSeries)

            if (a%numSeries == b%numSeries) then
                do k = 1, a%numSeries
                    if (allocated(a%plotdatalist(k)%x) .neqv. allocated(b%plotdatalist(k)%x)) then
                        call OUTKDP("ZINTEST: FAIL "//trim(tag)//".plotdatalist.x allocated mismatch")
                        failCount = failCount + 1
                    else if (allocated(a%plotdatalist(k)%x)) then
                        call ckRArr(trim(tag)//".plotdatalist.x", a%plotdatalist(k)%x, b%plotdatalist(k)%x)
                        call ckRArr(trim(tag)//".plotdatalist.y", a%plotdatalist(k)%y, b%plotdatalist(k)%y)
                    end if
                    call ckI(trim(tag)//".plotdatalist.dataColorCode", &
                    &        a%plotdatalist(k)%dataColorCode, b%plotdatalist(k)%dataColorCode)
                    call ckI(trim(tag)//".plotdatalist.lineStyleCode", &
                    &        a%plotdatalist(k)%lineStyleCode, b%plotdatalist(k)%lineStyleCode)
                end do
            end if

            call ckL(trim(tag)//".useLegend", a%useLegend, b%useLegend)
            call ckI(trim(tag)//".numLegendNames", a%numLegendNames, b%numLegendNames)
            if (a%numLegendNames == b%numLegendNames) then
                do k = 1, a%numLegendNames
                    call ckS(trim(tag)//".legendNames", a%legendNames(k), b%legendNames(k))
                end do
            end if

            call ckL(trim(tag)//".addTextToPlot", a%addTextToPlot, b%addTextToPlot)
            call ckI(trim(tag)//".numTextLabels", a%numTextLabels, b%numTextLabels)
            if (a%numTextLabels == b%numTextLabels) then
                do k = 1, a%numTextLabels
                    call ckS(trim(tag)//".textLabels", a%textLabels(k), b%textLabels(k))
                    call ckI(trim(tag)//".textLabelPositions", a%textLabelPositions(k), b%textLabelPositions(k))
                end do
            end if

            call ckL(trim(tag)//".manualYScale", a%manualYScale, b%manualYScale)
            call ckR8(trim(tag)//".yScale", a%yScale, b%yScale)
            call ckL(trim(tag)//".manualXScale", a%manualXScale, b%manualXScale)
            call ckR8(trim(tag)//".xScale", a%xScale, b%xScale)

            call ckS(trim(tag)//".xPlotCodes", a%xPlotCodes, b%xPlotCodes)
            call ckS(trim(tag)//".yPlotcodes", a%yPlotcodes, b%yPlotcodes)

            call ckL(trim(tag)//".drawScale", a%drawScale, b%drawScale)
            call ckR(trim(tag)//".scaleRatio", a%scaleRatio, b%scaleRatio)
            call ckI(trim(tag)//".scalePos", a%scalePos, b%scalePos)
        end subroutine ckZoaplotBase

        subroutine ckPlotdata3d(idx, a, b)
            use zoa_plot, only: plotdata3d
            integer, intent(in) :: idx
            type(plotdata3d), intent(in) :: a, b
            character(len=40) :: tag

            write(tag, '(A,I0,A)') "p3.plotdatalist3d(", idx, ")"
            call ckR8Arr(trim(tag)//".x", a%x, b%x)
            call ckR8Arr(trim(tag)//".y", a%y, b%y)
            call ckR8Arr(trim(tag)//".z", a%z, b%z)
            call ckI(trim(tag)//".dataColorCode", a%dataColorCode, b%dataColorCode)
            call ckI(trim(tag)//".lineStyleCode", a%lineStyleCode, b%lineStyleCode)
            call ckI(trim(tag)//".xpts", a%xpts, b%xpts)
            call ckI(trim(tag)//".ypts", a%ypts, b%ypts)
        end subroutine ckPlotdata3d

        subroutine ckPlotSetting(idx, a, b)
            integer, intent(in) :: idx
            type(plot_setting), intent(in) :: a, b
            character(len=40) :: tag
            integer :: k

            write(tag, '(A,I0,A)') "psm.ps(", idx, ")"
            call ckI(trim(tag)//".ID", a%ID, b%ID)
            call ckI(trim(tag)//".uitype", a%uitype, b%uitype)
            call ckR(trim(tag)//".min", a%min, b%min)
            call ckR(trim(tag)//".max", a%max, b%max)
            call ckR(trim(tag)//".default", a%default, b%default)
            call ckS(trim(tag)//".prefix", a%prefix, b%prefix)
            call ckS(trim(tag)//".label", a%label, b%label)
            call ckS(trim(tag)//".defaultStr", a%defaultStr, b%defaultStr)
            call ckS(trim(tag)//".cmd", a%cmd, b%cmd)
            call ckS(trim(tag)//".fullCmd", a%fullCmd, b%fullCmd)
            call ckI(trim(tag)//".ownerID", a%ownerID, b%ownerID)

            if (allocated(a%set) .neqv. allocated(b%set)) then
                call OUTKDP("ZINTEST: FAIL "//trim(tag)//".set allocated mismatch")
                failCount = failCount + 1
            else if (allocated(a%set)) then
                call ckI(trim(tag)//".set.size", size(a%set), size(b%set))
                if (size(a%set) == size(b%set)) then
                    do k = 1, size(a%set)
                        call ckI(trim(tag)//".set.ID", a%set(k)%ID, b%set(k)%ID)
                        call ckS(trim(tag)//".set.text", a%set(k)%text, b%set(k)%text)
                    end do
                end if
            end if

            if (allocated(a%coupledIDs) .neqv. allocated(b%coupledIDs)) then
                call OUTKDP("ZINTEST: FAIL "//trim(tag)//".coupledIDs allocated mismatch")
                failCount = failCount + 1
            else if (allocated(a%coupledIDs)) then
                call ckI(trim(tag)//".coupledIDs.size", size(a%coupledIDs), size(b%coupledIDs))
                if (size(a%coupledIDs) == size(b%coupledIDs)) then
                    do k = 1, size(a%coupledIDs)
                        call ckI(trim(tag)//".coupledIDs", a%coupledIDs(k), b%coupledIDs(k))
                    end do
                end if
            end if
        end subroutine ckPlotSetting

    end procedure execZINTEST

    !## cmd:      RAD
    !## syntax:   RAD ; [NRD n] ; [PER p1 p2 ...] ; GO
    !## category: Plotting
    !## desc:     Geometric radial (encircled) energy, CODE V's RAD option: plots the energy
    !##           enclosed vs radius for each field and lists the radius enclosing each
    !##           percentage, about the polychromatic spot's centroid.  NRD rays across the
    !##           pupil (default 20); PER up to 10 percentages (default 10, 20, .. 100).
    !##
    module procedure execRAD
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm
        call psm%initialize(trim(iptStr))
        call psm%addRADSettings()
        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_RAD, psm)
        if (.not. boolResult) call zoa_emit("Error:  expected RAD or RAD Pn", "red")
    end procedure execRAD

    ! PER p1 [p2 ..]: the RAD plot's tabulated percentages.
    module procedure radSetPER
        use command_utils, only: isInputNumber
        use plot_setting_manager, only: SETTING_RAD_PER
        implicit none
        character(len=80) :: tokens(40)
        character(len=200) :: lst
        integer :: numTokens, i
        real(long) :: v

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens < 2 .or. numTokens > 11) then
            call zoa_emit("Error:  RAD: expected PER p1 [p2 ...] (up to 10)", "red"); return
        end if
        lst = ''
        do i = 2, numTokens
            if (.not. isInputNumber(trim(tokens(i)))) then
                call zoa_emit("Error:  RAD: bad percentage "//trim(tokens(i)), "red"); return
            end if
            v = str2real8(trim(tokens(i)))
            if (v <= 0.0_long .or. v > 100.0_long) then
                call zoa_emit("Error:  RAD: percentages must be in (0, 100]", "red"); return
            end if
            lst = trim(lst)//' '//trim(tokens(i))
        end do
        call curr_psm%updateSetting(SETTING_RAD_PER, trim(adjustl(lst)))
    end procedure radSetPER

    ! NRD n inside a RAD plot: rays across the pupil (outside RAD, NRD is the
    ! legacy KDP command; the dispatcher only sends it here inside RAD).
    module procedure radSetNRD
        use command_utils, only: isInputNumber
        use plot_setting_manager, only: SETTING_RAD_NRD
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens /= 2 .or. .not. isInputNumber(trim(tokens(2)))) then
            call zoa_emit("Error:  RAD: expected NRD n", "red"); return
        end if
        if (str2int(trim(tokens(2))) < 2) then
            call zoa_emit("Error:  RAD: NRD must be at least 2", "red"); return
        end if
        call curr_psm%updateSetting(SETTING_RAD_NRD, str2int(trim(tokens(2))))
    end procedure radSetNRD

    !## cmd:      WAV
    !## syntax:   WAV; [NOM Y] [BES Y] [THR Y] [RFO Y] [NRD n] [WTF w1 ..] [CPL g1 ..]; GO
    !## category: Analysis
    !## desc:     RMS wavefront error and Strehl ratio per field, at nominal and/or best composite focus, and through focus (CODE V WAV).
    !##
    module procedure execWAV
        use wavefront_analysis, only: wav_reset
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 1) then
            call zoa_emit("Error:  expected WAV (then its options and GO)", "red"); return
        end if
        call wav_reset()
        cmd_loop = WAV_LOOP
    end procedure execWAV

    ! The WAV options (claimed by the dispatcher only inside WAV).
    module procedure wavSetOption
        use command_utils, only: isInputNumber
        use wavefront_analysis, only: wav_set_mode, wav_set_rfo, wav_set_nrd, &
                                      wav_set_wtf, wav_set_cpl
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, i, ivals(40)
        real(long) :: vals(40)
        logical :: on

        call parse(trim(iptStr), ' ', tokens, numTokens)
        select case (trim(tokens(1)))
        case ('BES', 'NOM', 'THR', 'RFO')
            on = .true.
            if (numTokens >= 2) then
                select case (trim(tokens(2)))
                case ('Y', 'YES'); on = .true.
                case ('N', 'NO');  on = .false.
                case default
                    call zoa_emit("Error:  WAV: expected "//trim(tokens(1))//" Y|N", "red"); return
                end select
            end if
            if (numTokens > 2) then
                call zoa_emit("Error:  WAV: expected "//trim(tokens(1))//" Y|N", "red"); return
            end if
            if (tokens(1) == 'RFO') then
                call wav_set_rfo(on)
            else
                call wav_set_mode(trim(tokens(1)), on)
            end if
        case ('NRD')
            if (numTokens /= 2 .or. .not. isInputNumber(trim(tokens(2)))) then
                call zoa_emit("Error:  WAV: expected NRD n", "red"); return
            end if
            i = str2int(trim(tokens(2)))
            if (i < 4 .or. i > 512) then
                call zoa_emit("Error:  WAV: NRD must be 4..512", "red"); return
            end if
            call wav_set_nrd(i)
        case ('WTF', 'CPL')
            if (numTokens < 2) then
                call zoa_emit("Error:  WAV: expected "//trim(tokens(1))//" v1 [v2 ..] (one per field)", "red")
                return
            end if
            do i = 2, numTokens
                if (.not. isInputNumber(trim(tokens(i)))) then
                    call zoa_emit("Error:  WAV: bad value "//trim(tokens(i)), "red"); return
                end if
                vals(i-1) = str2real8(trim(tokens(i)))
                ivals(i-1) = nint(vals(i-1))
            end do
            if (tokens(1) == 'WTF') then
                if (any(vals(1:numTokens-1) < 0.0_long)) then
                    call zoa_emit("Error:  WAV: field weights must be >= 0", "red"); return
                end if
                call wav_set_wtf(vals, numTokens-1)
            else
                if (any(ivals(1:numTokens-1) < 1)) then
                    call zoa_emit("Error:  WAV: CPL groups are numbered from 1", "red"); return
                end if
                call wav_set_cpl(ivals, numTokens-1)
            end if
        end select
    end procedure wavSetOption

    !## cmd:      FMA
    !## syntax:   FMA; [FFD RWE|RSD|DST [Y]] [NFX n] [NFY n] [XMI v] [XMA v] [YMI v] [YMA v] [NRD n] [LIS Y]; GO
    !## category: Analysis
    !## desc:     Field map: RMS wavefront, RMS spot diameter or distortion on a grid of field points (CODE V FMA FFD).
    !##
    module procedure execFMA
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm
        call psm%initialize(trim(iptStr))
        call psm%addFMASettings()
        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_FMA, psm)
        if (.not. boolResult) call zoa_emit("Error:  expected FMA", "red")
    end procedure execFMA

    ! The FMA options (claimed by the dispatcher only inside FMA).  The
    ! settings panel sends combo choices as their ids (FFD 2, LIS 1), so
    ! those are accepted too.
    module procedure fmaSetOption
        use command_utils, only: isInputNumber
        use plot_setting_manager, only: SETTING_FMA_FFD, SETTING_FMA_NFX, SETTING_FMA_NFY, &
            SETTING_FMA_NRD, SETTING_FMA_XMI, SETTING_FMA_XMA, SETTING_FMA_YMI, &
            SETTING_FMA_YMA, SETTING_FMA_LIS
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, n, code, lo, hi

        call parse(trim(iptStr), ' ', tokens, numTokens)
        select case (trim(tokens(1)))
        case ('FFD')
            n = 0
            if (numTokens >= 2) then
                select case (trim(tokens(2)))
                case ('RWE', '1', '1.0'); n = 1
                case ('RSD', '2', '2.0'); n = 2
                case ('DST', '3', '3.0'); n = 3
                case ('AST', 'DSC', 'BDE', 'WPO', 'CTD', 'CCD', 'MTF', 'MTQ', 'GMT', 'GMQ')
                    call zoa_emit("Error:  FMA: FFD "//trim(tokens(2))// &
                    &  " is not available yet (RWE, RSD or DST)", "red"); return
                end select
            end if
            if (numTokens >= 3) then
                if (tokens(3) /= 'Y' .and. tokens(3) /= 'YES') n = 0
            end if
            if (n == 0 .or. numTokens > 3) then
                call zoa_emit("Error:  FMA: expected FFD RWE|RSD|DST [Y]", "red"); return
            end if
            call curr_psm%updateSetting(SETTING_FMA_FFD, n)
        case ('LIS')
            n = 0
            if (numTokens == 1) n = 2
            if (numTokens == 2) then
                select case (trim(tokens(2)))
                case ('Y', 'YES', '2', '2.0'); n = 2
                case ('N', 'NO', '1', '1.0');  n = 1
                end select
            end if
            if (n == 0) then
                call zoa_emit("Error:  FMA: expected LIS Y|N", "red"); return
            end if
            call curr_psm%updateSetting(SETTING_FMA_LIS, n)
        case ('NFX', 'NFY', 'NRD')
            if (numTokens /= 2 .or. .not. isInputNumber(trim(tokens(2)))) then
                call zoa_emit("Error:  FMA: expected "//trim(tokens(1))//" n", "red"); return
            end if
            n = nint(str2real8(trim(tokens(2))))
            select case (trim(tokens(1)))
            case ('NFX'); code = SETTING_FMA_NFX; lo = 2; hi = 41
            case ('NFY'); code = SETTING_FMA_NFY; lo = 2; hi = 41
            case default; code = SETTING_FMA_NRD; lo = 4; hi = 128
            end select
            if (n < lo .or. n > hi) then
                call zoa_emit("Error:  FMA: "//trim(tokens(1))//" must be "// &
                &  trim(int2str(lo))//".."//trim(int2str(hi)), "red"); return
            end if
            call curr_psm%updateSetting(code, n)
        case ('XMI', 'XMA', 'YMI', 'YMA')
            if (numTokens /= 2) then
                call zoa_emit("Error:  FMA: expected "//trim(tokens(1))//" value|AUTO", "red"); return
            end if
            if (.not. isInputNumber(trim(tokens(2))) .and. tokens(2) /= 'AUTO') then
                call zoa_emit("Error:  FMA: bad value "//trim(tokens(2)), "red"); return
            end if
            select case (trim(tokens(1)))
            case ('XMI'); code = SETTING_FMA_XMI
            case ('XMA'); code = SETTING_FMA_XMA
            case ('YMI'); code = SETTING_FMA_YMI
            case default; code = SETTING_FMA_YMA
            end select
            call curr_psm%updateSetting(code, trim(tokens(2)))
        end select
    end procedure fmaSetOption

    !## cmd:      FOO
    !## syntax:   FOO; [SUR Sk|Si..j] [BOU Y|N] [NRD n] [RAR f] [AZI min max incr] [DAP Y|N] [ORI x y] [LIS Y]; GO
    !## category: Analysis
    !## desc:     Footprint plot: every field's beam where it crosses the designated surfaces, with the aperture outline (CODE V FOO).
    !##
    module procedure execFOO
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm
        call psm%initialize(trim(iptStr))
        call psm%addFOOSettings()
        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_FOO, psm)
        if (.not. boolResult) call zoa_emit("Error:  expected FOO", "red")
    end procedure execFOO

    ! The FOO options (claimed by the dispatcher only inside FOO).  Combo
    ! settings arrive from the settings panel as their ids (BOU 2, DAP 1).
    module procedure fooSetOption
        use command_utils, only: isInputNumber
        use plot_setting_manager, only: SETTING_FOO_SUR, SETTING_FOO_BOU, SETTING_FOO_NRD, &
            SETTING_FOO_RAR, SETTING_FOO_DAP, SETTING_FOO_LIS, SETTING_FOO_ORI, SETTING_FOO_AZI
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, n, i, s0, s1, code
        real(kind=real64) :: v(3)

        call parse(trim(iptStr), ' ', tokens, numTokens)
        select case (trim(tokens(1)))
        case ('SUR')
            if (numTokens /= 2) then
                call zoa_emit("Error:  FOO: expected SUR Sk or SUR Si..j", "red"); return
            end if
            if (.not. parseSurfaceSpec(trim(tokens(2)), s0, s1)) return
            call curr_psm%updateSetting(SETTING_FOO_SUR, trim(tokens(2)))
        case ('BOU', 'DAP', 'LIS')
            n = 0
            if (numTokens == 1) n = 2
            if (numTokens == 2) then
                select case (trim(tokens(2)))
                case ('Y', 'YES'); n = 2
                case ('N', 'NO');  n = 3
                case ('1', '1.0'); n = 1
                case ('2', '2.0'); n = 2
                case ('3', '3.0'); n = 3
                end select
            end if
            ! DAP and LIS are No/Yes combos (ids 1/2); BOU has Default/Yes/No
            if (tokens(1) /= 'BOU' .and. n == 3) then
                if (tokens(2)(1:1) == 'N') then
                    n = 1
                else
                    n = 0
                end if
            end if
            if (n == 0) then
                call zoa_emit("Error:  FOO: expected "//trim(tokens(1))//" Y|N", "red"); return
            end if
            select case (trim(tokens(1)))
            case ('BOU'); code = SETTING_FOO_BOU
            case ('DAP'); code = SETTING_FOO_DAP
            case default; code = SETTING_FOO_LIS
            end select
            call curr_psm%updateSetting(code, n)
        case ('NRD')
            if (numTokens /= 2) then
                call zoa_emit("Error:  FOO: expected NRD n|AUTO", "red"); return
            end if
            if (tokens(2) /= 'AUTO') then
                if (.not. isInputNumber(trim(tokens(2)))) then
                    call zoa_emit("Error:  FOO: expected NRD n|AUTO", "red"); return
                end if
                n = nint(str2real8(trim(tokens(2))))
                if (n < 2 .or. n > 200) then
                    call zoa_emit("Error:  FOO: NRD must be 2..200", "red"); return
                end if
                tokens(2) = int2str(n)
            end if
            call curr_psm%updateSetting(SETTING_FOO_NRD, trim(tokens(2)))
        case ('RAR')
            if (numTokens /= 2 .or. .not. isInputNumber(trim(tokens(2)))) then
                call zoa_emit("Error:  FOO: expected RAR fraction", "red"); return
            end if
            v(1) = str2real8(trim(tokens(2)))
            if (v(1) <= 0.0_real64 .or. v(1) > 0.1_real64) then
                call zoa_emit("Error:  FOO: RAR must be in (0, 0.1]", "red"); return
            end if
            call curr_psm%updateSetting(SETTING_FOO_RAR, trim(tokens(2)))
        case ('AZI', 'ORI')
            n = merge(3, 2, tokens(1) == 'AZI')
            if (numTokens /= n + 1) then
                if (n == 3) then
                    call zoa_emit("Error:  FOO: expected AZI min max increment (degrees)", "red")
                else
                    call zoa_emit("Error:  FOO: expected ORI x y", "red")
                end if
                return
            end if
            do i = 1, n
                if (.not. isInputNumber(trim(tokens(i+1)))) then
                    call zoa_emit("Error:  FOO: bad value "//trim(tokens(i+1)), "red"); return
                end if
                v(i) = str2real8(trim(tokens(i+1)))
            end do
            if (n == 3) then
                if (v(3) <= 0.0_real64 .or. v(2) <= v(1) .or. (v(2) - v(1))/v(3) > 3600.0_real64) then
                    call zoa_emit("Error:  FOO: AZI needs max > min and 0 < increment (at most 3600 azimuths)", "red")
                    return
                end if
                call curr_psm%updateSetting(SETTING_FOO_AZI, &
                &  trim(tokens(2))//' '//trim(tokens(3))//' '//trim(tokens(4)))
            else
                call curr_psm%updateSetting(SETTING_FOO_ORI, trim(tokens(2))//' '//trim(tokens(3)))
            end if
        case default
            call zoa_emit("Error:  FOO: "//trim(tokens(1))//" is not available yet", "red")
        end select
    end procedure fooSetOption

    !## cmd:      TRA
    !## syntax:   TRA; [FUL Y|N] [NRD n]; GO
    !## category: Analysis
    !## desc:     Transmission analysis (CODE V TRA): per field and wavelength, the mean
    !##           transmission of the rays reaching the image -- surface losses (coatings,
    !##           else bare Fresnel), bulk absorption from the glass catalogs, and both.
    !##           FUL Y lists every field in full; NRD sets the pupil grid (default 20).
    !##
    module procedure execTRA
        use transmission_analysis, only: tra_reset
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 1) then
            call zoa_emit("Error:  expected TRA (then its options and GO)", "red"); return
        end if
        call tra_reset()
        cmd_loop = TRA_LOOP
    end procedure execTRA

    module procedure traSetOption
        use transmission_analysis, only: tra_set_option
        implicit none
        character(len=80) :: tokens(40)
        character(len=200) :: msg
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        call tra_set_option(tokens, numTokens, msg)
        if (len_trim(msg) > 0) call zoa_emit("Error:  "//trim(msg), "red")
    end procedure traSetOption

    !## cmd:      FAB
    !## syntax:   FAB; [SUR Y] [IND Y] [SPA Y] [SAG [Sk] [max incr ref delta min]] [BES Y] [FRI Y] [SLO Y]; GO
    !## category: Analysis
    !## desc:     Fabrication data: element (or surface) listing, index and spacer tables, aspheric sag tables (CODE V FAB).
    !##
    module procedure execFAB
        use fab_data, only: fab_reset
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 1) then
            call zoa_emit("Error:  expected FAB (then its options and GO)", "red"); return
        end if
        call fab_reset()
        cmd_loop = FAB_LOOP
    end procedure execFAB

    ! The FAB options (claimed by the dispatcher only inside FAB).  SAG's
    ! numbers are max_ht incr_ht ref_curv delta_excess min_ht, each a value
    ! or No (keep the default).
    module procedure fabSetOption
        use command_utils, only: isInputNumber
        use fab_data, only: fab_set_flag, fab_set_sag
        use mod_lens_data_manager, only: ldm
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, i, k, s0, s1, surf
        real(kind=real64) :: par(5)
        logical :: has(5), on

        call parse(trim(iptStr), ' ', tokens, numTokens)
        select case (trim(tokens(1)))
        case ('SUR', 'IND', 'SPA', 'BES', 'FRI', 'SLO')
            on = .true.
            if (numTokens == 2) then
                select case (trim(tokens(2)))
                case ('Y', 'YES'); on = .true.
                case ('N', 'NO');  on = .false.
                case default
                    call zoa_emit("Error:  FAB: expected "//trim(tokens(1))//" Y|N", "red"); return
                end select
            else if (numTokens > 2) then
                call zoa_emit("Error:  FAB: expected "//trim(tokens(1))//" Y|N", "red"); return
            end if
            call fab_set_flag(trim(tokens(1)), on)
        case ('SAG')
            surf = -1
            i = 2
            if (numTokens >= 2) then
                if (tokens(2)(1:1) == 'S') then
                    if (.not. parseSurfaceSpec(trim(tokens(2)), s0, s1)) return
                    if (s1 /= s0) then
                        call zoa_emit("Error:  FAB: SAG takes one surface", "red"); return
                    end if
                    if (s0 < 1 .or. s0 >= ldm%getLastSurf()) then
                        call zoa_emit("Error:  FAB: SAG needs a surface between S1 and SI-1", "red"); return
                    end if
                    surf = s0
                    i = 3
                end if
            end if
            par = 0.0_real64; has = .false.
            if (numTokens - i + 1 > 5) then
                call zoa_emit("Error:  FAB: SAG [Sk] [max incr ref delta min]", "red"); return
            end if
            do k = 1, numTokens - i + 1
                if (tokens(i+k-1) == 'NO' .or. tokens(i+k-1) == 'N') cycle
                if (.not. isInputNumber(trim(tokens(i+k-1)))) then
                    call zoa_emit("Error:  FAB: bad SAG value "//trim(tokens(i+k-1)), "red"); return
                end if
                par(k) = str2real8(trim(tokens(i+k-1)))
                has(k) = .true.
            end do
            if ((has(1) .and. par(1) <= 0.0_real64) .or. (has(2) .and. par(2) <= 0.0_real64) .or. &
            &   (has(5) .and. par(5) < 0.0_real64)) then
                call zoa_emit("Error:  FAB: SAG heights and increment must be positive", "red"); return
            end if
            call fab_set_sag(surf, par, has)
        case default
            call zoa_emit("Error:  FAB: "//trim(tokens(1))//" is not available yet", "red")
        end select
    end procedure fabSetOption

    !## cmd:      CAN
    !## syntax:   CAN
    !## category: Utilities
    !## desc:     Cancel the open option (a plot, AUT, ...) without running it (CODE V CAN).
    !##
    module procedure cancelOption
        implicit none
        if (cmd_loop == 0) then
            call zoa_emit("CAN: no option is open", "black")
            return
        end if
        cmd_loop = 0
        block
            use optim_types, only: idxConUpdate
            idxConUpdate = 0
        end block
        call zoa_emit("Option cancelled", "black")
    end procedure cancelOption

end submodule mod_codev_plots
