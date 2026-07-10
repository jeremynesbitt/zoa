submodule (codeV_commands) mod_codev_editops
implicit none
contains

    module procedure updateVarCodes
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: processResult
        integer :: s0, sf, dotLoc

        processResult = .FALSE.
        call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 3) then
            dotLoc = index(tokens(2), '..')
            if (dotLoc > 0) then
                if (isInputNumber(tokens(2)(2:dotLoc-1)) .and. &
                & isInputNumber(tokens(2)(dotLoc+2:len(tokens(2))))) then
                    s0 = str2int(tokens(2)(2:dotLoc-1))
                    sf = str2int(tokens(2)(dotLoc+2:len(tokens(2))))
                    processResult = .TRUE.
                else
                    call zoa_emit("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            else
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
                if (surfNum .ne. -1) then
                    s0 = surfNum
                    sf = surfNum
                    processResult = .TRUE.
                else
                    call zoa_emit("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            end if
        end if

        if (processResult) then
            if (isInputNumber(trim(tokens(3)))) then
                print *, "ABout to update Var with ", str2int(trim(tokens(3)))
                call ldm%updateOptimVars(trim(tokens(1)), s0, sf, str2int(trim(tokens(3))))
                call updateOptimVarsNew(trim(tokens(1)), s0, sf, str2int(trim(tokens(3))))
            end if
        else
            call zoa_emit("Error:  Variable code must be number "//trim(tokens(3)), "red")
        end if
    end procedure updateVarCodes

    module procedure updateConstraint
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: processResult
        integer :: s0, sf, dotLoc
        character(len=256) :: normalStr
        integer :: ci, ni

        processResult = .FALSE.

        if (cmd_loop == AUT_LOOP .OR. cmd_loop == TAR_LOOP .OR. cmd_loop == CON_UPDATE_LOOP) then
            normalStr = ''
            ni = 1
            do ci = 1, len_trim(iptStr)
                select case (iptStr(ci:ci))
                case ('=', '<', '>')
                    normalStr(ni:ni) = ' '; ni = ni + 1
                    normalStr(ni:ni) = iptStr(ci:ci); ni = ni + 1
                    normalStr(ni:ni) = ' '; ni = ni + 1
                case default
                    normalStr(ni:ni) = iptStr(ci:ci); ni = ni + 1
                end select
            end do
            call parse(trim(normalStr), ' ', tokens, numTokens)

            if (numTokens == 3 .AND. isInputNumber(trim(tokens(3)))) then
                if (trim(tokens(2)) == '=' .OR. trim(tokens(2)) == '>' .OR. trim(tokens(2)) == '<') then
                    if (cmd_loop == CON_UPDATE_LOOP) then
                        call addConstraint(trim(tokens(1)), str2real8(tokens(3)), trim(tokens(2)), idxConUpdate)
                    else
                        call addConstraint(trim(tokens(1)), str2real8(tokens(3)), trim(tokens(2)))
                    end if
                else
                    call zoa_emit("Error:  Unable to parse number for third token ", "red")
                end if
            else
                call zoa_emit("Error:  Unable to parse number for third token ", "red")
            end if
        else
            call zoa_emit("Error:  Can only set constraint in AUT loop! ", "red")
        end if
    end procedure updateConstraint

    module procedure execNBR
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult
        integer, allocatable :: surfaces(:)

        call parse(trim(iptStr), ' ', tokens, numTokens)
        boolResult = .FALSE.
        if (numTokens > 2) then
            select case(trim(tokens(2)))
            case('ELE')
                if (tokens(3)(1:1) == 'S') then
                    boolResult = cmd_parser_get_integer_range(tokens(3)(2:len(tokens(3))), surfaces)
                    if (boolResult) then
                        if (cmd_loop == VIE_LOOP) then
                            call LogTermFOR("Upating Surfaces in ld_settings")
                            call curr_psm%updateSetting(ID_LENS_FIRSTSURFACE, surfaces(1))
                            call curr_psm%updateSetting(ID_LENS_LASTSURFACE, surfaces(size(surfaces)))
                        end if
                    end if
                end if
            case default
                boolResult = .FALSE.
            end select
        end if
        if (.not. boolResult) then
            call zoa_emit("Unable to parse command.  Expect NBR ELE Si..j Got " // trim(iptStr), "black")
        end if
    end procedure execNBR

    module procedure execCLI
        use global_widgets, only: curr_lens_data
        use command_utils, only: isInputNumber
        use kdp_data_types, only: check_clear_apertures
        use mod_lens_data_manager, only: ldm
        use DATLEN
        implicit none

        integer :: i
        character(len=80) :: tokens(40)
        character(len=4)  :: surfTxt
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        call LogTermFOR("Calling check_clear_apetures")
        ! Size the typed surface array for the current lens, then fill each
        ! surface's clap%auto_* with ray-traced extents.
        call ldm%load_surfaces_from_alens()
        call check_clear_apertures(curr_lens_data, ldm%surfaces)
        call zoa_emit(blankStr(7)//"Y-FAN"//blankStr(5)//"X-FAN", "black")
        do i=2,curr_lens_data%num_surfaces
            surfTxt = blankStr(2)//trim(int2str(i-1))
            if (i==1) surfTxt = "OBJ"
            if (i==curr_lens_data%ref_stop) surfTxt = "STO"
            if (i==curr_lens_data%num_surfaces) surfTxt = "IMG"
            call zoa_emit(trim(surfTxt)//blankStr(2)// &
            & trim(real2str(ldm%surfaces(i-1)%s%clap%auto_semi_y))//blankStr(5)// &
            & trim(real2str(ldm%surfaces(i-1)%s%clap%auto_semi_x)), "black")
        end do
    end procedure execCLI

    module procedure insertSurf
        use command_utils, only: isInputNumber
        use mod_lens_data_manager, only: ldm
        use global_widgets, only: curr_lens_data
        implicit none

        integer :: surfNum, i, s0, sf, dotLoc
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: movePIM
        integer :: pimSurf

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            dotLoc = index(tokens(2), '..')
            if (dotLoc > 0) then
                if (isInputNumber(tokens(2)(2:dotLoc-1)) .AND. &
                & isInputNumber(tokens(2)(dotLoc+2:len(tokens(2))))) then
                    s0 = str2int(tokens(2)(2:dotLoc-1))
                    sf = str2int(tokens(2)(dotLoc+2:len(tokens(2))))
                    do i=s0,sf
                        ! num_surfaces shifts up by 1 each iteration, so check against current value
                        movePIM = (i == curr_lens_data%num_surfaces - 1) .AND. &
                                & ldm%isPIMSolveOnSurf(i - 1)
                        pimSurf = i - 1
                        call executeCodeVLensUpdateCommand('INSK, '//trim(int2str(i)), &
                        & exitLensUpdate=.TRUE., refreshAll=.TRUE.)
                        if (movePIM) then
                            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(pimSurf))//'; TSD; GO')
                            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(i))//'; PY, 0; GO')
                        end if
                    end do
                else
                    call zoa_emit("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            else
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
                if (surfNum .NE. -1) then
                    movePIM = (surfNum == curr_lens_data%num_surfaces - 1) .AND. &
                            & ldm%isPIMSolveOnSurf(surfNum - 1)
                    call executeCodeVLensUpdateCommand('INSK, '//trim(int2str(surfNum)), &
                    & exitLensUpdate=.TRUE., refreshAll=.TRUE.)
                    if (movePIM) then
                        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum - 1))//'; TSD; GO')
                        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; PY, 0; GO')
                    end if
                else
                    call zoa_emit("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            end if
        end if
    end procedure insertSurf

    module procedure setDim
        use command_utils
        logical :: inputCheck

        inputCheck = checkCommandInput([ID_CMD_QUAL], qual_words=['M', 'C', 'I'], &
        &qual_only_err_msg="DIM Takes only M(mm), C(cm), or I(inches) as input")

        select case (getQualWord())
        case ('M')
            call executeCodeVLensUpdateCommand("UNITS MM")
        case ('C')
            call executeCodeVLensUpdateCommand("UNITS CM")
        case ('I')
            call executeCodeVLensUpdateCommand("UNITS IN")
        end select
    end procedure setDim

    ! Shared "the lens is being replaced" reset: every entry point that loads or
    ! creates a lens (LEN NEW, CV2PRG, ZMX2PRG, RES/.zoa restore) funnels through
    ! here so per-lens extra state is cleared ONE way -- by running the
    ! newlens.zoa template macro (DCON ALL, DEL VIG, DEL APE SA, then a minimal
    ! template lens), exactly what LEN NEW has always done.  Deliberate
    ! exception: undo restore_snapshot keeps its minimal direct resets (it must
    ! not touch the undo baseline, and snapshots fully encode the state).
    module procedure resetToNewLensTemplate
        use mod_lens_data_manager
        use zoom_manager, only: zoom_reset
        implicit none

        integer :: ios, fID
        character(len=200) :: line

        ! A new lens starts single-config; clear any prior zoom before loading.
        ! (Per-field vignetting is cleared by the DEL VIG line in newlens.zoa;
        ! clear + edge apertures by its DEL APE SA; constraints by DCON ALL.)
        call zoom_reset()
        ! newunit (not a hardcoded unit): this also runs nested inside
        ! process_zoa_file when a script line triggers a lens load.
        open(newunit=fID, file=trim(basePath)//'Macros/newlens.zoa', iostat=ios)
        if (ios /= 0) stop "Error opening file "

        do
            read(fID, '(A)', iostat=ios) line
            if (ios /= 0) then
                exit
            else
                call PROCESKDP(trim(line))
            end if
        end do
        close(unit=fID)
        ldm%vars(:,:) = 100
        ! Rebuild the typed surface store from the freshly-loaded template so it
        ! does not carry stale surfaces from a previous lens.  Without this, a
        ! per-surface geometry refresh on the *next* lens can act on a leftover
        ! store, making an otherwise-identical macro non-idempotent on re-run.
        call ldm%load_surfaces_from_alens()
    end procedure resetToNewLensTemplate

    module procedure newLens
        use undo_manager, only: undo_reset_baseline
        implicit none

        call resetToNewLensTemplate()
        ! A new lens replaces the system: reset undo history with it as baseline.
        call undo_reset_baseline()
    end procedure newLens

    module procedure setLensTitle
        use command_utils
        call executeCodeVLensUpdateCommand('LI '// parseTitleCommand())
    end procedure setLensTitle

    module procedure processFileComment
    end procedure processFileComment

    module procedure setFieldWeights
        call zoa_emit("Field Weights Command "//trim(iptStr)//" Not supported", "black")
    end procedure setFieldWeights

    module procedure setWavelengthWeights
        use global_widgets, only: sysConfig
        implicit none

        integer :: i
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        do i=2,numTokens
            call sysConfig%setSpectralWeights(i-1, real(str2real8(trim(tokens(i))),8))
        end do
    end procedure setWavelengthWeights

    module procedure setField
        use command_utils, only : parseCommandIntoTokens
        use kdp_utils, only: inLensUpdateLevel
        use global_widgets, only: sysConfig
        implicit none

        integer :: i, numFields
        character(len=80) :: tokens(40)
        integer :: numTokens
        real(kind=real64), allocatable :: absFields(:)
        integer, parameter :: X_COL = 1
        integer, parameter :: Y_COL = 2
        integer :: FLD_COL

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (tokens(1).EQ.'YAN'.OR.tokens(1).EQ.'YOB'.OR.tokens(1).EQ.'YIM') FLD_COL = Y_COL
        if (tokens(1).EQ.'XAN'.OR.tokens(1).EQ.'XOB'.OR.tokens(1).EQ.'XIM') FLD_COL = X_COL

        call sysConfig%setFieldTypeFromString(trim(tokens(1)))
        numFields = numTokens-1
        allocate(absFields(numFields))
        do i=1,numFields
            absFields(i) = str2real8(trim(tokens(i+1)))
        end do
        call sysConfig%setNumFields(numFields)
        call sysConfig%setAbsoluteFields(absFields, FLD_COL)
    end procedure setField

    module procedure setWavelength
        use global_widgets, only: sysConfig
        implicit none

        integer :: i, numWavelengths
        character(len=80) :: tokens(40)
        integer :: numTokens
        real(kind=real64) :: wvReal
        logical :: CVERROR
        character(len=1024) :: outStr
        character(len=1024) :: strWL

        call parse(trim(iptStr), ' ', tokens, numTokens)
        numWavelengths = numTokens-1

        if (numTokens <= 6) then
            outStr = 'WV, '
            do i=2,numTokens
                call ATODCODEV(tokens(i)(1:23), wvReal, CVERROR)
                write(strWL, '(D23.15)') wvReal/1000.0_long
                outStr = trim(outStr)//' '//trim(strWL)
                call sysConfig%setSpectralWeights(i-1, 1.0D0)
            end do
            if (numTokens < 6) then
                do i=numTokens+1,6
                    outStr = trim(outStr)//' 0.0'
                end do
            end if
            call executeCodeVLensUpdateCommand(trim(outStr))
        end if
    end procedure setWavelength

    module procedure setSurfaceCodeVStyle
        use mod_lens_data_manager
        use command_utils, only : parseCommandIntoTokens
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

        select case(numTokens)
        case (1)
            call zoa_emit("No info given besides surface identifier!  Please try again", "red")
        case (2)
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; RD, ' // trim(tokens(2)))
        case (3)
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; RD, ' // trim(tokens(2))//";TH, "//trim(tokens(3)))
        case (4)
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            if (.not. isSpecialGlass(trim(tokens(4)))) then
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; RD, ' // trim(tokens(2))//";TH, "// &
                & trim(tokens(3))//'; '//trim(getSetGlassText(trim(tokens(4)))))
            else
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; RD, ' // trim(tokens(2))//";TH, "// &
                & trim(tokens(3))//';' // trim(tokens(4)))
            end if
        end select
    end procedure setSurfaceCodeVStyle

    module procedure scaleSystem
        use command_utils, only : isInputNumber
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 3) then
            select case(tokens(2))
            case('EFL')
                if (isInputNumber(tokens(3))) then
                    call PROCESSILENT('SC FY, '//trim(tokens(3))// ", 0")
                else
                    call zoa_emit("Error!  Could not convert 3rd token to numeric value", "red")
                end if
            end select
        else
            call zoa_emit("Error!  SCA format is SCA VAR VAL.  Eg SCA EFL 50", "red")
        end if
    end procedure scaleSystem

    module function getDefaultMaxFrequency() result(maxFreq)
        use DATSPD
        real :: FREQ1, FREQ2, maxFreq
        logical :: ERROR
        ERROR = .FALSE.
        call CUTTOFF(FREQ1, FREQ2, ERROR)
        if (ERROR) then
            call zoa_emit('ERROR IN OBJECT/IMAGE SPACE FREQUENCY RELATIONSHIP', "red")
            return
        end if
        if (SPACEBALL .EQ. 1) then
            maxFreq = FREQ2
        else
            maxFreq = FREQ1
        end if
        print *, "Max Frequency is ", maxFreq
    end function getDefaultMaxFrequency

    module procedure updateOptimImprovementGoal
        use command_utils
        use optim_types, only: optim

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2 .AND. isInputNumber(trim(tokens(2)))) then
            optim%imp = str2real8(trim(tokens(2)))
        else
            call zoa_emit('Error:  Expect IMP r, where r is a number', "red")
        end if
    end procedure updateOptimImprovementGoal

    module procedure updateRMSPlotType
        use command_utils
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2) then
            select case(cmd_loop)
            case (ID_PLOTTYPE_RMSFIELD)
                if (lowercase(tokens(2)) == 'spot') then
                    call curr_psm%updateSetting(ID_RMS_DATA_TYPE, ID_RMS_DATA_SPOT)
                end if
                if (lowercase(tokens(2)) == 'wave') then
                    call curr_psm%updateSetting(ID_RMS_DATA_TYPE, ID_RMS_DATA_WAVE)
                end if
            end select
        end if
    end procedure updateRMSPlotType

    module procedure aut_ui
        use iso_c_binding, only: c_associated
        use optimizer_ui
        use global_widgets, only: optimizer_window, my_window
        use globals, only: HEADLESS_MODE
        if (HEADLESS_MODE) then
            call zoa_emit("AUTUI requires GUI", "red")
            return
        end if
        if (.not. c_associated(optimizer_window)) then
            call optimizer_ui_new(my_window)
        end if
    end procedure aut_ui

    module procedure updateDatabase
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numtokens < 2) then
            call zoa_emit('Error:  Expect UPD X, where X is the type of data to update', "red")
        else
            select case(trim(tokens(2)))
            case('CON')
                cmd_loop = CON_UPDATE_LOOP
            end select
        end if
    end procedure updateDatabase

    module procedure changeDatabase
        use optim_types
        use command_utils, only: isInputNumber
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2 .AND. isInputNumber(trim(tokens(2)))) then
            select case(cmd_loop)
            case(CON_UPDATE_LOOP)
                idxConUpdate = str2int(trim(tokens(2)))
            case default
                call zoa_emit('CHA Error: Not in update loop', "red")
            end select
        else
            call zoa_emit('Error:  Expect CHA r, where r is a number', "red")
        end if
    end procedure changeDatabase

    module procedure evaluateCmd
        real(kind=long) :: result
        result = evalfunc(iptStr(4:len_trim(iptStr)), .TRUE.)
    end procedure evaluateCmd

    module procedure evalFunc
        use data_registers, only: getData
        call getData(iptStr, res)
        if (present(logResult)) then
            if (logResult) then
                call LogTermFOR(real2str(res))
            end if
        end if
    end procedure evalFunc

    module procedure listConstraints
        use optim_types, only: nC, constraintsInUse
        use type_utils, only: real2str
        use kdp_utils, only: OUTKDP
        implicit none
        integer :: i
        character(len=1) :: conTypeStr
        character(len=80) :: outStr

        if (nC == 0) then
            call OUTKDP('No constraints defined')
            return
        end if

        call OUTKDP('  #   NAME   TYPE        TARGET')
        call OUTKDP('  -   ----   ----   -----------')
        do i = 1, nC
            conTypeStr = constraintsInUse(i)%getConstraintTypeAsText()
            write(outStr, '(I3, 3X, A4, 3X, A1, 3X, A)') i, constraintsInUse(i)%name, conTypeStr, trim(real2str(constraintsInUse(i)%targ))
            call OUTKDP(trim(outStr))
        end do
    end procedure listConstraints

    module procedure deleteConstraints
        use optim_types, only: optim
        use kdp_utils, only: OUTKDP
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2 .and. trim(tokens(2)) == 'ALL') then
            call optim%removeAllConstraints()
            call OUTKDP('All constraints removed')
        else
            call OUTKDP('Usage: DCON ALL')
        end if
    end procedure deleteConstraints

    module procedure execSET
        use mod_lens_data_manager, only: ldm
        use global_widgets, only: sysConfig
        use type_utils, only: str2real8
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, fld
        real(kind=real64) :: vuy, vly, vux, vlx

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens >= 2 .and. trim(tokens(2)) == 'CAP') then
            ! SET CAP: auto-assign clear apertures to all surfaces from real ray tracing.
            ! SETCLAP is a CMD-level command handled in CMDER, so call it directly via
            ! PROCESKDP (do NOT wrap in 'U L'). Then refresh the typed surface objects so
            ! the lens editor reflects the new ALENS clap data.
            call PROCESKDP('SETCLAP REAL')
            call ldm%load_surfaces_from_alens()
        else if (numTokens >= 2 .and. trim(tokens(2)) == 'VIG') then
            ! SET VIG <field> <vuy> <vly> <vux> <vlx>: per-field vignetting factors.
            ! Missing factors default to 0 (no vignetting on that edge).
            if (numTokens < 3) then
                call zoa_emit("Usage: SET VIG <field> [vuy] [vly] [vux] [vlx]", "red")
                return
            end if
            fld = nint(str2real8(tokens(3)))
            if (fld < 1 .or. fld > sysConfig%numFields) then
                call zoa_emit("SET VIG: field index out of range", "red")
                return
            end if
            vuy = 0.0_real64; vly = 0.0_real64; vux = 0.0_real64; vlx = 0.0_real64
            if (numTokens >= 4) vuy = str2real8(tokens(4))
            if (numTokens >= 5) vly = str2real8(tokens(5))
            if (numTokens >= 6) vux = str2real8(tokens(6))
            if (numTokens >= 7) vlx = str2real8(tokens(7))
            call sysConfig%setVignetting(fld, vuy, vly, vux, vlx)
        else
            call zoa_emit("Unknown SET subcommand. Try: SET CAP, SET VIG", "red")
        end if
    end procedure execSET

end submodule mod_codev_editops
