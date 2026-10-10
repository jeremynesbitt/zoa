submodule (codeV_commands) mod_codev_editops
use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                       kdp_lens_end, kdp_chg, kdp_lens_cmd
use iso_fortran_env, only: real64
implicit none
contains

    !## cmd:      CCY
    !## syntax:   CCY Sk n | CCY Si..j n
    !## category: Optimization
    !## desc:     Set the YZ-curvature variable code on surface(s).
    !##
    !## cmd:      GLC
    !## syntax:   GLC Sk n
    !## category: Optimization
    !## desc:     Set the glass variable code on surface(s).
    !##
    !## cmd:      KC
    !## syntax:   KC Sk n
    !## category: Optimization
    !## desc:     Set the conic-constant variable code on surface(s).
    !##
    !## cmd:      THC
    !## syntax:   THC Sk n | THC Si..j n
    !## category: Optimization
    !## desc:     Set the thickness variable code on surface(s).
    !##
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

    ! Unified merit-entry parser, dispatched for every registered evaluator
    ! name (EFL, TCO, TAS, PTB, IMC, SAS; SPO arrives via execSPO) inside the
    ! AUT/TAR/UPD CON loops:
    !   NAME = v | NAME > v | NAME < v   -> hard constraint
    !   NAME v [w]                       -> objective term, target v, weight w
    !                                       (default 1); minimized as
    !                                       w*(value-v)**2
    !## cmd:      EFL
    !## syntax:   EFL = v | EFL v [w]
    !## category: Optimization
    !## desc:     Effective focal length merit entry (constraint with =, or operand with target v, weight w).
    !##
    !## cmd:      IMC
    !## syntax:   IMC = v | IMC v [w]
    !## category: Optimization
    !## desc:     Image-clearance / distance merit entry.
    !##
    !## cmd:      PTB
    !## syntax:   PTB = v | PTB v [w]
    !## category: Optimization
    !## desc:     Petzval-blur merit entry.
    !##
    !## cmd:      PTZ
    !## syntax:   PTZ = v | PTZ v [w]
    !## category: Optimization
    !## desc:     Petzval-curvature merit entry (1/R of the Petzval surface, lens units^-1).
    !##
    !## cmd:      SAS
    !## syntax:   SAS = v | SAS v [w]
    !## category: Optimization
    !## desc:     Transverse spherical aberration merit entry.
    !##
    !## cmd:      TAS
    !## syntax:   TAS = v | TAS v [w]
    !## category: Optimization
    !## desc:     Tangential-astigmatism merit entry.
    !##
    !## cmd:      TCO
    !## syntax:   TCO = v | TCO v [w]
    !## category: Optimization
    !## desc:     Transverse coma merit entry.
    !##
    module procedure updateConstraint
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        character(len=256) :: normalStr
        integer :: ci, ni
        real(long) :: w

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

            if (numTokens == 3 .AND. .not. isInputNumber(trim(tokens(2)))) then
                ! Constraint form: NAME <op> value
                if ((trim(tokens(2)) == '=' .OR. trim(tokens(2)) == '>' .OR. trim(tokens(2)) == '<') &
                &   .AND. isInputNumber(trim(tokens(3)))) then
                    if (cmd_loop == CON_UPDATE_LOOP) then
                        call addConstraint(trim(tokens(1)), str2real8(tokens(3)), trim(tokens(2)), idxConUpdate)
                    else
                        call addConstraint(trim(tokens(1)), str2real8(tokens(3)), trim(tokens(2)))
                    end if
                else
                    call zoa_emit("Error:  Expect NAME = value (or > <), or NAME target [weight]", "red")
                end if
            else if (numTokens >= 2 .AND. isInputNumber(trim(tokens(2)))) then
                ! Objective form: NAME target [weight]
                w = 1.0_long
                if (numTokens >= 3 .AND. isInputNumber(trim(tokens(3)))) w = str2real8(tokens(3))
                if (cmd_loop == CON_UPDATE_LOOP) then
                    call addMeritEntry(trim(tokens(1)), ID_ROLE_OBJECTIVE, str2real8(tokens(2)), &
                    &                  weight=w, idxToUpdate=idxConUpdate)
                else
                    call addMeritEntry(trim(tokens(1)), ID_ROLE_OBJECTIVE, str2real8(tokens(2)), weight=w)
                end if
            else
                call zoa_emit("Error:  Expect NAME = value (or > <), or NAME target [weight]", "red")
            end if
        else
            call zoa_emit("Error:  Can only set constraint in AUT loop! ", "red")
        end if
    end procedure updateConstraint

    !## cmd:      NBR
    !## syntax:   NBR ELE Si..j
    !## category: Plot Settings
    !## desc:     Set the surface range for the lens drawing.
    !##
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

    ! General-constraint settings (MXT/MNT/MNE/MNA/MAE): global limits applied
    ! automatically to every VARIABLE thickness at AUT;GO.  Inside the AUT/TAR
    ! loop:  "MXT 14.0" sets, bare "MXT" prints the current value.
    !## cmd:      MAE
    !## syntax:   MAE X
    !## category: Optimization
    !## desc:     Set the minimum edge air spacing (general constraint).
    !##
    !## cmd:      MNA
    !## syntax:   MNA X
    !## category: Optimization
    !## desc:     Set the minimum axial air spacing (general constraint).
    !##
    !## cmd:      MNE
    !## syntax:   MNE X
    !## category: Optimization
    !## desc:     Set the minimum element edge thickness (general constraint).
    !##
    !## cmd:      MNT
    !## syntax:   MNT X
    !## category: Optimization
    !## desc:     Set the minimum element center thickness (general constraint).
    !##
    !## cmd:      MXT
    !## syntax:   MXT X
    !## category: Optimization
    !## desc:     Set the maximum element center thickness (general constraint, inside AUT).
    !##
    module procedure updateGeneralConstraint
        use command_utils, only : isInputNumber
        use optim_types, only: optim
        use type_utils, only: real2str, str2real8
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        real(long) :: v
        logical :: setIt

        if (cmd_loop /= AUT_LOOP .AND. cmd_loop /= TAR_LOOP) then
            call zoa_emit("Error:  General constraints (MXT/MNT/MNE/MNA/MAE) are set inside the AUT loop", "red")
            return
        end if

        call parse(trim(iptStr), ' ', tokens, numTokens)
        setIt = .FALSE.
        if (numTokens >= 2) then
            if (isInputNumber(trim(tokens(2)))) then
                v = str2real8(tokens(2))
                setIt = .TRUE.
            end if
        end if

        select case (trim(tokens(1)))
        case('MXT')
            if (setIt) then
                optim%mxt = v
            else
                call zoa_emit("MXT (max element thickness) = "//trim(real2str(optim%mxt)), "black")
            end if
        case('MNT')
            if (setIt) then
                optim%mnt = v
            else
                call zoa_emit("MNT (min element thickness) = "//trim(real2str(optim%mnt)), "black")
            end if
        case('MNE')
            if (setIt) then
                optim%mne = v
            else
                call zoa_emit("MNE (min edge thickness) = "//trim(real2str(optim%mne)), "black")
            end if
        case('MNA')
            if (setIt) then
                optim%mna = v
            else
                call zoa_emit("MNA (min axial air spacing) = "//trim(real2str(optim%mna)), "black")
            end if
        case('MAE')
            if (setIt) then
                optim%mae = v
            else
                call zoa_emit("MAE (min air spacing at edge) = "//trim(real2str(optim%mae)), "black")
            end if
        end select
    end procedure updateGeneralConstraint

    !## cmd:      GENCON
    !## syntax:   GENCON YES|NO
    !## category: Optimization
    !## desc:     Turn the general constraints (MXT/MNT/MNE/MNA/MAE) on or off; bare GENCON queries. Set inside AUT or UPD CON.
    !##
    module procedure execGENCON
        use optim_types, only: optim
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        if (cmd_loop /= AUT_LOOP .AND. cmd_loop /= TAR_LOOP) then
            call zoa_emit("Error:  GENCON is set inside the AUT loop (or UPD CON)", "red")
            return
        end if

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens < 2) then
            if (optim%genConOn) then
                call zoa_emit("GENCON YES (general constraints MXT/MNT/MNE/MNA/MAE are on)", "black")
            else
                call zoa_emit("GENCON NO (general constraints MXT/MNT/MNE/MNA/MAE are off)", "black")
            end if
            return
        end if

        select case (trim(tokens(2)))
        case ('YES', 'Y', 'ON')
            optim%genConOn = .true.
        case ('NO', 'N', 'OFF')
            optim%genConOn = .false.
        case default
            call zoa_emit("Error:  GENCON takes YES or NO", "red")
        end select
    end procedure execGENCON

    ! ETH: list every gap's center and edge thickness.  Edge thickness is
    ! computed from the typed surfaces' sag() methods at the gap's evaluation
    ! height (max of the two surfaces' semi-diameters: explicit CIR EDG if
    ! set, else the ray-traced auto extent).  Also the verification vehicle
    ! for the optimizer's MNE/MAE general constraints.
    !## cmd:      ETH
    !## syntax:   ETH Sk X
    !## category: Apertures
    !## desc:     Set the edge thickness aperture used by the MNE/MAE general constraints.
    !##
    module procedure execETH
        use global_widgets, only: curr_lens_data
        use kdp_data_types, only: check_clear_apertures
        use mod_lens_data_manager, only: ldm
        use kdp_utils, only: OUTKDP
        use type_utils, only: int2str, real2str
        implicit none

        integer :: k, lastSurf
        real(real64) :: rho, edge
        character(len=8) :: gapTyp
        character(len=120) :: outStr

        ! Refresh the typed store + auto apertures (same pattern as CLI).
        call ldm%load_surfaces_from_alens()
        call check_clear_apertures(curr_lens_data, ldm%surfaces)

        lastSurf = ldm%getLastSurf()
        if (lastSurf < 2) then
            call OUTKDP('ETH: no gaps to list')
            return
        end if

        call OUTKDP('GAP        TYPE      CENTER        EVAL HT       EDGE')
        do k = 1, lastSurf-1
            if (ldm%isGlassSurf(k)) then
                gapTyp = 'GLASS'
            else
                gapTyp = 'AIR'
            end if
            rho  = max(ldm%getEvalSemiDia(k), ldm%getEvalSemiDia(k+1))
            edge = ldm%edge_thickness(k, rho)
            write(outStr, '(A, 2X, A8, 2X, A12, 2X, A12, 2X, A12)') &
            &  'S'//trim(int2str(k))//'-S'//trim(int2str(k+1)), gapTyp, &
            &  trim(real2str(ldm%getSurfThi(k), 5)), trim(real2str(rho, 5)), &
            &  trim(real2str(edge, 5))
            call OUTKDP(trim(outStr))
        end do
    end procedure execETH

    !## cmd:      CLI
    !## syntax:   CLI
    !## category: Apertures
    !## desc:     Check/refresh the clear apertures.
    !##
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

    !## cmd:      INS
    !## syntax:   INS Sk | INS Si..j
    !## category: Surface Parameters
    !## desc:     Insert one or more new surfaces before surface k (or over the range i..j).
    !##
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
                        call kdp_silent_begin()
                        call kdp_lens_begin()
                        call kdp_lens_cmd('INSK', w1=real(i, real64))
                        call kdp_lens_end(refreshAll=.TRUE.)
                        call kdp_silent_end()
                        if (movePIM) then
                            call kdp_silent_begin()
                            call kdp_lens_begin()
                            call kdp_chg(pimSurf)
                            call kdp_lens_cmd('TSD')
                            call kdp_lens_end()
                            call kdp_lens_begin()
                            call kdp_chg(i)
                            call kdp_lens_cmd('PY', w1=0.0d0)
                            call kdp_lens_end()
                            call kdp_silent_end()
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
                    call kdp_silent_begin()
                    call kdp_lens_begin()
                    call kdp_lens_cmd('INSK', w1=real(surfNum, real64))
                    call kdp_lens_end(refreshAll=.TRUE.)
                    call kdp_silent_end()
                    if (movePIM) then
                        call kdp_silent_begin()
                        call kdp_lens_begin()
                        call kdp_chg(surfNum - 1)
                        call kdp_lens_cmd('TSD')
                        call kdp_lens_end()
                        call kdp_lens_begin()
                        call kdp_chg(surfNum)
                        call kdp_lens_cmd('PY', w1=0.0d0)
                        call kdp_lens_end()
                        call kdp_silent_end()
                    end if
                else
                    call zoa_emit("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            end if
        end if
    end procedure insertSurf

    !## cmd:      DIM
    !## syntax:   DIM M|C|I
    !## category: System Parameters
    !## desc:     Set the lens units: M (mm), C (cm), or I (inches).
    !##
    module procedure setDim
        ! Self-tokenizing (parse the raw line) rather than reading the DATMAI
        ! qualifier global via getQualWord -- so this works when the front door
        ! dispatches it before PRO3 has run.
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens < 2) then
            call zoa_emit("DIM takes only M(mm), C(cm), or I(inches) as input", "red")
            return
        end if

        select case (trim(tokens(2)))
        case ('M', 'C', 'I')
            call kdp_silent_begin()
            call kdp_lens_begin()
            select case (trim(tokens(2)))
            case ('M'); call kdp_lens_cmd('UNITS', wq='MM')
            case ('C'); call kdp_lens_cmd('UNITS', wq='CM')
            case ('I'); call kdp_lens_cmd('UNITS', wq='IN')
            end select
            call kdp_lens_end()
            call kdp_silent_end()
        case default
            call zoa_emit("DIM takes only M(mm), C(cm), or I(inches) as input", "red")
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
        use zoa_file_handler, only: findDataFile, getFileSep
        implicit none

        integer :: ios, fID
        character(len=200) :: line
        character(len=1024) :: templatePath

        ! A new lens starts single-config; clear any prior zoom before loading.
        ! (Per-field vignetting is cleared by the DEL VIG line in newlens.zoa;
        ! clear + edge apertures by its DEL APE SA; constraints by DCON ALL.)
        call zoom_reset()
        ! the default edge aperture belongs to the lens (saved as DDR EDG):
        ! a new or loaded lens starts with no margin
        sysConfig%defaultEdgeScaleFactor = 1.0_long
        ! newunit (not a hardcoded unit): this also runs nested inside
        ! process_zoa_file when a script line triggers a lens load.
        ! The template: the macro folder, then the search path, then the
        ! install's Macros folder.
        templatePath = findDataFile('Macros', 'newlens.zoa')
        if (len_trim(templatePath) == 0) &
            templatePath = trim(basePath)//'Macros'//getFileSep()//'newlens.zoa'
        open(newunit=fID, file=trim(templatePath), iostat=ios)
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
        use zoa_ui_callbacks, only: notify_close_all_tabs
        use zoa_file_handler, only: zoa_file_depth
        implicit none

        ! A new lens invalidates every open plot, so offer to discard them --
        ! the same prompt CV2PRG/ZMX2PRG show.  Automatically a no-op headless
        ! (the callback is unregistered) and when no plots are open
        ! (closeAllTabs returns immediately).
        !
        ! Only for a LEN NEW the user actually drove: typed at the prompt
        ! (zoa_file_depth == 0) or run from a macro.  A LEN NEW that is just the
        ! first line of a .zoa lens file being restored is structural -- that
        ! path manages tabs itself (closing them and restoring from the .zin
        ! companion), and prompting there would fire on every RES.
        if (zoa_file_depth == 0 .or. in_macro_load) then
            call notify_close_all_tabs("You are about to open a new " //&
            &"lens system.  This will invalidate all plots.   " //&
            &"Press yes to close them.")
        end if

        call resetToNewLensTemplate()
        ! A new lens replaces the system: reset undo history with it as baseline.
        call undo_reset_baseline()
    end procedure newLens

    !## cmd:      TIT
    !## syntax:   TIT 'text'
    !## category: System Parameters
    !## desc:     Set the lens title.
    !##
    module procedure setLensTitle
        ! Extract the quoted title from this command's own text (iptStr),
        ! not the raw INPUT parse global.  Quoted-string case is preserved on
        ! both dispatch paths now (the front door's fold and PRO2's UPPER both
        ! protect quotes), so iptStr carries the title verbatim.
        implicit none
        character(len=80) :: restOfString, title
        integer :: blankLoc, lQ, rQ

        blankLoc = index(trim(iptStr), ' ', BACK=.FALSE.)
        if (blankLoc == 0) return                 ! no argument
        restOfString = iptStr(blankLoc+1:len_trim(iptStr))
        lQ = index(restOfString, '''', BACK=.FALSE.)
        rQ = index(restOfString, '''', BACK=.TRUE.)
        if (lQ /= rQ .and. rQ > lQ) then
            title = restOfString(lQ+1:rQ-1)
            ! SLI sets LI=WS (with SST=1), so the title goes straight into the
            ! string field -- and typed avoids PRO3 having to re-tokenize a
            ! multi-word title out of the command text.
            call kdp_silent_begin()
            call kdp_lens_begin()
            call kdp_lens_cmd('LI', ws=trim(title))
            call kdp_lens_end()
            call kdp_silent_end()
        end if
    end procedure setLensTitle

    !## cmd:      !
    !## syntax:   ! text
    !## category: Utilities
    !## desc:     Comment line (ignored); used in .zoa files.
    !##
    module procedure processFileComment
    end procedure processFileComment

    !## cmd:      WTF
    !## syntax:   WTF s1 [s2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the field weights.
    !##
    module procedure setFieldWeights
        call zoa_emit("Field Weights Command "//trim(iptStr)//" Not supported", "black")
    end procedure setFieldWeights

    !## cmd:      WTW
    !## syntax:   WTW s1 [s2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the spectral (wavelength) weights.
    !##
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

    !## cmd:      XAN
    !## syntax:   XAN a1 [a2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the X field angles (degrees).
    !##
    !## cmd:      XOB
    !## syntax:   XOB h1 [h2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the X object heights.
    !##
    !## cmd:      YAN
    !## syntax:   YAN a1 [a2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the Y field angles (degrees).
    !##
    !## cmd:      YIM
    !## syntax:   YIM h1 [h2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the Y paraxial image heights (field specification).
    !##
    !## cmd:      YOB
    !## syntax:   YOB h1 [h2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the Y object heights.
    !##
    module procedure setField
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

    !## cmd:      WL
    !## syntax:   WL w1 [w2 ...]
    !## category: Fields & Wavelengths
    !## desc:     Set the system wavelengths (nm); up to 5 values.
    !##
    module procedure setWavelength
        use global_widgets, only: sysConfig
        implicit none

        integer :: i, numWavelengths
        character(len=80) :: tokens(40)
        integer :: numTokens
        real(kind=real64) :: wvReal
        logical :: CVERROR
        real(kind=real64) :: wv(5)
        character(len=32)  :: wvStr

        call parse(trim(iptStr), ' ', tokens, numTokens)
        numWavelengths = numTokens-1

        if (numTokens <= 6) then
            wv = 0.0_real64
            do i=2,numTokens
                call ATODCODEV(tokens(i)(1:23), wvReal, CVERROR)
                ! Preserve the exact wavelength the legacy WV text path produced
                ! (D23.15 round-trip): this diffraction intermediate is
                ! ill-conditioned to 1 ULP, so quantizing here keeps results
                ! byte-identical while still eliminating the PROCESKDP dispatch.
                write(wvStr, '(D23.15)') wvReal/1000.0_long
                read(wvStr, *) wv(i-1)
                call sysConfig%setSpectralWeights(i-1, 1.0D0)
            end do
            call kdp_silent_begin()
            call kdp_lens_begin()
            call kdp_lens_cmd('WV', w1=wv(1), w2=wv(2), w3=wv(3), w4=wv(4), w5=wv(5))
            call kdp_lens_end()
            call kdp_silent_end()
        end if
    end procedure setWavelength

    !## cmd:      S
    !## syntax:   S [rd th glass]
    !## category: Surface Parameters
    !## desc:     Advance to / add the next surface (optionally set radius, thickness, glass).
    !##
    !## cmd:      SI
    !## syntax:   SI [rd th glass]
    !## category: Surface Parameters
    !## desc:     Select the image surface.
    !##
    !## cmd:      SO
    !## syntax:   SO [rd th glass]
    !## category: Surface Parameters
    !## desc:     Select the object surface (S0).
    !##
    module procedure setSurfaceCodeVStyle
        use mod_lens_data_manager
        use command_utils, only: isInputNumber
        use global_widgets, only: curr_lens_data
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 1) then
            call zoa_emit("No info given besides surface identifier!  Please try again", "red")
            return
        end if
        if (.not. isInputNumber(trim(tokens(2)))) then
            call zoa_emit("Expect numeric radius, e.g. S3 -78", "red"); return
        end if
        if (numTokens >= 3 .and. .not. isInputNumber(trim(tokens(3)))) then
            call zoa_emit("Expect numeric thickness, e.g. S3 -78 2.5", "red"); return
        end if

        surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_chg(surfNum)
        call kdp_lens_cmd('RD', w1=str2real8(trim(tokens(2))))
        if (numTokens >= 3) call kdp_lens_cmd('TH', w1=str2real8(trim(tokens(3))))
        if (numTokens == 4) then
            if (.not. isSpecialGlass(trim(tokens(4)))) then
                call applyGlassText(trim(getSetGlassText(trim(tokens(4)))))
            else
                call kdp_lens_cmd(trim(tokens(4)))
            end if
        end if
        call kdp_lens_end(refreshSurf=surfNum)
        call kdp_silent_end()
        ! During a .zoa load the surrounding lens level is already open, so the
        ! transaction above is nested and does NOT fire the post-EOS refresh that
        ! CONTRO runs after every parsed command.  Sync curr_lens_data here so the
        ! next bare-S (getSurfNumFromSurfCommand) counts surfaces from fresh data.
        call curr_lens_data%update()
    end procedure setSurfaceCodeVStyle

    !## cmd:      SCA
    !## syntax:   SCA EFL X
    !## category: Lens System Commands
    !## desc:     Scale the system (e.g. SCA EFL 50 scales to an EFL of 50).
    !##
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
        use iso_fortran_env, only: real64
        ! CUTTOFF (WAVSPOT5.f90) is an external routine with real64 outputs.
        ! These were default REAL, called with no interface: CUTTOFF wrote 8
        ! bytes into each 4-byte actual, corrupting the adjacent stack (ERROR
        ! among it).  The interface makes any such mismatch a compile error.
        interface
            subroutine CUTTOFF(FREQ1, FREQ2, ERROR)
                import real64
                real(real64) :: FREQ1, FREQ2
                logical :: ERROR
            end subroutine CUTTOFF
        end interface
        real(real64) :: FREQ1, FREQ2
        real :: maxFreq
        logical :: ERROR
        ERROR = .FALSE.
        ! Always define the result: the error path used to return with maxFreq
        ! never assigned, so a failed CUTTOFF produced a garbage/zero maximum
        ! frequency that was then baked into the plot command as "MFR 0.00000".
        maxFreq = 0.0
        call CUTTOFF(FREQ1, FREQ2, ERROR)
        if (ERROR) then
            call zoa_emit('ERROR IN OBJECT/IMAGE SPACE FREQUENCY RELATIONSHIP', "red")
            return
        end if
        if (SPACEBALL .EQ. 1) then
            maxFreq = real(FREQ2)
        else
            maxFreq = real(FREQ1)
        end if
    end function getDefaultMaxFrequency

    !## cmd:      IMP
    !## syntax:   IMP X
    !## category: Optimization
    !## desc:     Set the optimizer improvement goal.
    !##
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

    !## cmd:      RMSDATA
    !## syntax:   RMSDATA WAVE|SPOT
    !## category: Plot Settings
    !## desc:     Choose the data type for the RMS-vs-field plot.
    !##
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

    !## cmd:      AUTUI
    !## syntax:   AUTUI
    !## category: Optimization
    !## desc:     Open the optimizer setup window (merit operands, constraints, variables).
    !##
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

    !## cmd:      UPD
    !## syntax:   UPD CON
    !## category: Optimization
    !## desc:     Enter an update loop to edit a data set (e.g. UPD CON for constraints).
    !##
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

    !## cmd:      CHA
    !## syntax:   CHA n ; <value>
    !## category: Optimization
    !## desc:     Change entry n inside the current UPD loop.
    !##
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

    !## cmd:      EVA
    !## syntax:   EVA name
    !## category: Analysis
    !## desc:     Evaluate a merit operand by name and print its value.
    !##
    module procedure evaluateCmd
        real(kind=long) :: result
        result = evalfunc(iptStr(4:len_trim(iptStr)), .TRUE.)
    end procedure evaluateCmd

    ! A merit operand name (EFL, SPO, ...) is evaluated directly from the
    ! evaluator registry.  Anything else is run as a command and read back
    ! from the scalar data register (the ray-data commands fill it).  When
    ! neither yields a value the result is 0 and an error is reported --
    ! never an undefined value.
    module procedure evalFunc
        use data_registers, only: getData
        use optim_types, only: evaluators, isNameInEvaluatorList
        use strings, only: uppercase
        character(len=:), allocatable :: what
        integer :: idx
        logical :: ok

        what = trim(adjustl(iptStr))
        res = 0.0_long
        idx = 0
        if (len(what) > 0) idx = isNameInEvaluatorList(uppercase(what))
        if (idx > 0) then
            res = evaluators(idx)%func()
            ok = .true.
        else if (len(what) > 0) then
            call getData(what, res, ok)
        else
            ok = .false.
        end if
        if (.not. ok) then
            call zoa_emit("Error:  EVA cannot evaluate '"//what//"'", "red")
            return
        end if
        if (present(logResult)) then
            if (logResult) then
                call LogTermFOR(real2str(res))
            end if
        end if
    end procedure evalFunc

    ! List the whole merit function: objective terms and constraints, with
    ! their role.  The # is the entry's position in the unified list (the
    ! index UPD CON; CHA n edits).
    !## cmd:      LCON
    !## syntax:   LCON
    !## category: Optimization
    !## desc:     List the current merit operands and constraints.
    !##
    module procedure listConstraints
        use optim_types, only: nM, meritInUse
        use type_utils, only: real2str
        use kdp_utils, only: OUTKDP
        implicit none
        integer :: i
        character(len=1) :: conTypeStr
        character(len=10) :: roleStr
        character(len=12) :: weightStr
        character(len=100) :: outStr

        if (nM == 0) then
            call OUTKDP('No merit entries (operands or constraints) defined')
            call printGeneralConstraintFooter()
            return
        end if

        call OUTKDP('  #   NAME   ROLE         TYPE        TARGET   WEIGHT')
        call OUTKDP('  -   ----   ----------   ----   -----------   ------')
        do i = 1, nM
            if (meritInUse(i)%role == ID_ROLE_CONSTRAINT) then
                roleStr = 'Constraint'
                conTypeStr = meritInUse(i)%getConstraintTypeAsText()
                weightStr = ''
            else
                roleStr = 'Operand'
                conTypeStr = ' '
                weightStr = trim(real2str(meritInUse(i)%weight))
            end if
            write(outStr, '(I3, 3X, A4, 3X, A10, 3X, A1, 3X, A14, 3X, A)') &
            &  i, meritInUse(i)%name, roleStr, conTypeStr, &
            &  trim(real2str(meritInUse(i)%targ)), trim(weightStr)
            call OUTKDP(trim(outStr))
        end do
        call printGeneralConstraintFooter()
    end procedure listConstraints

    ! One-line summary of the general-constraint settings (they apply to
    ! variable thicknesses only; see updateGeneralConstraint).
    subroutine printGeneralConstraintFooter()
        use optim_types, only: optim
        use type_utils, only: real2str
        use kdp_utils, only: OUTKDP
        implicit none

        if (.not. optim%genConOn) then
            call OUTKDP('General constraints: OFF (GENCON NO)')
            return
        end if
        call OUTKDP('General (variable thicknesses): MXT '//trim(real2str(optim%mxt))// &
        &  '  MNT '//trim(real2str(optim%mnt))//'  MNE '//trim(real2str(optim%mne))// &
        &  '  MNA '//trim(real2str(optim%mna))//'  MAE '//trim(real2str(optim%mae)))
    end subroutine

    !## cmd:      DCON
    !## syntax:   DCON n
    !## category: Optimization
    !## desc:     Delete constraint/operand number n.
    !##
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

    !## cmd:      SET
    !## syntax:   SET <option> ...
    !## category: Utilities
    !## desc:     Set a system option (e.g. SET CAP, SET VIG).
    !##
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
