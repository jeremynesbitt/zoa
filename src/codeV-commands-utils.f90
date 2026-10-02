submodule (codeV_commands) mod_codev_utils
implicit none
contains

    module procedure executeGo
        use global_widgets, only: ioConfig
        use kdp_utils, only: inLensUpdateLevel
        use plot_functions, only: mtf_go, psf_go, vie_go, spo_go, seidel_go, ast_go, pma_go, rayaberration_go, rmsfield_go, zern_go
        use optim_functions, only: aut_go
        use tow_functions, only: tow_go

        if (cmd_loop == ID_PLOTTYPE_MTF) then
            call mtf_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ID_PLOTTYPE_PSF) then
            call psf_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == AUT_LOOP) then
            call aut_go()
            cmd_loop = 0
        end if
        if (cmd_loop == TAR_LOOP) then
            cmd_loop = 0
            return
        end if
        if (cmd_loop == CON_UPDATE_LOOP) then
            ! UPD CON; CHA n; <line>; GO -- the GO only closes the update
            ! loop.  Without this the loop state leaked past the command and
            ! every later input still ran "inside" UPD CON.
            cmd_loop = 0
            return
        end if
        if (cmd_loop == TOW_LOOP) then
            cmd_loop = 0
            call curr_psm%addGenericSetting(999, "Command", real(999), -1.0, -1.0, ' ', trim(cmdTOW), UITYPE_ENTRY)
            call tow_go(curr_psm, trim(cmdTOW))
        end if
        if (cmd_loop == VIE_LOOP) then
            if (inLensUpdateLevel()) then
                call LogTermFOR("Will not draw in lens update level")
                return
            end if
            cmd_loop = DRAW_LOOP
            call vie_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == SPO_LOOP) then
            call spo_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ID_PLOTTYPE_SEIDEL) then
            call seidel_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ID_PLOTTYPE_AST) then
            call ast_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ID_PLOTTYPE_OPD) then
            call pma_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ID_PLOTTYPE_RIM) then
            call rayaberration_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ID_PLOTTYPE_RMSFIELD) then
            call rmsfield_go(curr_psm)
            cmd_loop = 0
        end if
        if (cmd_loop == ZERN_LOOP) then
            call zern_go(curr_psm)
            cmd_loop = 0
        end if
        if (inLensUpdateLevel()) then
            call PROCESKDP('EOS')
        end if
    end procedure executeGo

    module procedure setLens
        call PROCESKDP('LENS')
        call PROCESKDP('WV, 0.635')
        call PROCESKDP('UNITS MM')
        call PROCESKDP('SAY, 10.0')
        call PROCESKDP('CV, 0.0')
        call PROCESKDP('TH, 0.10E+21')
        call PROCESKDP('AIR')
        call PROCESKDP('CV, 0.0')
        call PROCESKDP('TH, 10.0')
        call PROCESKDP('REFS')
        call PROCESKDP('ASTOP')
        call PROCESKDP('AIR')
        call PROCESKDP('CV, 0.0')
        call PROCESKDP('TH, 1.0')
        call PROCESKDP('EOS')
        call PROCESKDP('U L')
    end procedure setLens

    !## cmd:      FLY
    !## syntax:   FLY Si..j
    !## category: Lens System Commands
    !## desc:     Flip (reverse) the given range of surfaces.
    !##
    module procedure flipSurfaces
        use global_widgets, only: sysConfig
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens
        integer, allocatable :: surfs(:)
        integer :: i, j, iSto, iStoNew
        real :: symPlane

        call parse(trim(iptStr), ' ', tokens, numTokens)
        surfs = cmd_parser_get_int_input_for_prefix('s', tokens(1:numTokens))

        if (size(surfs) > 1) then
            call PROCESSILENT('FLIP '//trim(int2str(surfs(1)))//","//trim(int2str(surfs(size(surfs)))))
        else
            call zoa_emit("Error!  Must have at least two surfaces to flip", "red")
        end if

        iSto = ldm%getStopSurf()
        if (iSto >= surfs(1) .AND. iSto <= surfs(size(surfs))) then
            symPlane = getSymmetryPlane(surfs)
            iStoNew = INT(symPlane-iSto+symPlane)
            call execSTO('STO S'//trim(int2str(iStoNew)))
        end if
    end procedure flipSurfaces

    module procedure getSymmetryPlane
        midPoint = size(surfs)/2.0 + 0.5
    end procedure getSymmetryPlane

    module procedure isInputSurfaceParameter
        use pickup_manager, only: pickup_j_from_cli
        ! A parameter is PIK-able iff it has a CLI name in the pickup-kind
        ! table (RDY, THI, GLA, K, A..I).
        boolResult = (pickup_j_from_cli(trim(iptStr)) /= 0)
    end procedure isInputSurfaceParameter

    module procedure setPickup
        use pickup_manager, only: pickup_j_from_cli, PIKUP_KINDS
        use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                               kdp_lens_end, kdp_chg, kdp_lens_cmd
        use iso_fortran_env, only: real64
        integer :: jIdx

        ! CLI param name -> pickup kind via the table (RDY, THI, GLA, K, A..I).
        jIdx = pickup_j_from_cli(param1)
        if (jIdx == 0) return

        ! PIKUP <qual>, <src>[, <scale>, <offset>] driven typed: qual->WQ, source
        ! surface->W1, scale/offset->W2/W3 (full real64, no real2str).  The GLASS
        ! kind takes no scale/offset.
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_chg(si)
        if (trim(PIKUP_KINDS(jIdx)%qual) == 'GLASS') then
            call kdp_lens_cmd('PIKUP', wq='GLASS', w1=real(sj, real64))
        else
            call kdp_lens_cmd('PIKUP', wq=trim(PIKUP_KINDS(jIdx)%qual), &
            &                 w1=real(sj, real64), w2=scale, w3=offset)
        end if
        call kdp_lens_end()
        call kdp_silent_end()
    end procedure setPickup

    module procedure parsePickupInput
        use command_utils, only : isInputNumber
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, si, sj

        call parse(trim(iptStr), ' ', tokens, numTokens)
        select case(numTokens)
        case (:3)
            call zoa_emit("Error!  Not enough arguments!  Format is PIK PARAM Si [PARAM] Sj [sf off]", "red")
        case (4)
            si = getSurfNumFromSurfCommand(trim(tokens(3)))
            sj = getSurfNumFromSurfCommand(trim(tokens(4)))
            if (si == -1 .or. sj == -1) then
                call zoa_emit("Error!  can't parse surfaces from arguments 3 and 4", "red")
                return
            end if
            if (isInputSurfaceParameter(trim(tokens(2)))) then
                call setPickup(trim(tokens(2)), si,sj, 1.0_long, 0.0_long)
            else
                call zoa_emit("Error!  Cannot parse parameter "//trim(tokens(2))//" to known surface parameter", "red")
            end if
        case (5,6)
            sj = getSurfNumFromSurfCommand(trim(tokens(4)))
            if (sj == -1) then
                si = getSurfNumFromSurfCommand(trim(tokens(3)))
                sj = getSurfNumFromSurfCommand(trim(tokens(5)))
                if (si == -1 .or. sj == -1) then
                    call zoa_emit("Error!  can't parse surfaces from arguments 3 and 5", "red")
                    return
                else
                    if (isInputSurfaceParameter(trim(tokens(2))) .AND. isInputSurfaceParameter(trim(tokens(4)))) then
                        if (numTokens == 6 .and. isInputNumber(trim(tokens(6)))) then
                            call setPickup(trim(tokens(2)), si,sj, str2real8(trim(tokens(6))), 0.0_long, trim(tokens(5)))
                        else
                            call setPickup(trim(tokens(2)), si,sj, 1.0_long, 0.0_long, trim(tokens(5)))
                        end if
                    else
                        call zoa_emit("Error!  Cannot parse parameter "//trim(tokens(2))//" or "//trim(tokens(5))//" to known surface parameter", "red")
                    end if
                end if
            else
                si = getSurfNumFromSurfCommand(trim(tokens(3)))
                if (si == -1) then
                    call zoa_emit("Error!  can't parse surface from arguments 3 ", "red")
                    return
                else
                    if (isInputSurfaceParameter(trim(tokens(2))) .AND. isInputNumber(trim(tokens(5)))) then
                        call setPickup(trim(tokens(2)), si,sj, str2real8(trim(tokens(5))), 0.0_long)
                    else
                        call zoa_emit("Error!  Cannot parse parameter "//trim(tokens(2))//" or "//trim(tokens(5))//" to known surface parameter", "red")
                    end if
                end if
            end if
        case (7)
            si = getSurfNumFromSurfCommand(trim(tokens(3)))
            sj = getSurfNumFromSurfCommand(trim(tokens(5)))
            if (si == -1 .or. sj == -1) then
                call zoa_emit("Error!  can't parse surfaces from arguments 3 and 5", "red")
                return
            else
                if (isInputSurfaceParameter(trim(tokens(2))) .AND. isInputSurfaceParameter(trim(tokens(4)))) then
                    if (isInputNumber(trim(tokens(6))) .AND. isInputNumber(trim(tokens(7)))) then
                        call setPickup(trim(tokens(2)), si,sj, str2real8(trim(tokens(6))), str2real8(trim(tokens(7))), trim(tokens(5)))
                    else
                        call zoa_emit("Error!  Cannot parse scale or offset parameter", "red")
                    end if
                else
                    call zoa_emit("Error!  Cannot parse parameter "//trim(tokens(2))//" or "//trim(tokens(5))//" to known surface parameter", "red")
                end if
            end if
        end select
    end procedure parsePickupInput

    module procedure processZoaFileInput
        use zoa_file_handler
        use mod_lens_data_manager, only: ldm
        use undo_manager, only: undo_reset_baseline
        implicit none
        integer :: locStr, locDot, i
        character(len=1024) :: fileName
        character(len=1) :: fileSep
        logical :: savedMacroFlag

        fileSep = getFileSep()

        ! Accept both "macro:" and legacy "zoa_macro:" prefixes (case-insensitive).
        locStr = index(uppercase(iptStr), 'MACRO:')
        if (locStr .ne. 0) then
            fileName = iptStr(locStr + len('MACRO:') : len_trim(iptStr))
            ! Normalize path separators
            do i = 1, len_trim(fileName)
                if (fileName(i:i) == '/' .or. fileName(i:i) == '\') fileName(i:i) = fileSep
            end do
            locDot = index(fileName, '.')
            if (locDot == 0) fileName = trim(fileName)//'.zoa'
            fileName = trim(getMacroDir())//trim(fileName)
            if (doesFileExist(trim(fileName))) then
                if (present(printOnly)) then
                    call process_zoa_file(trim(fileName), printOnly=.TRUE.)
                else
                    ! Macros are NOT forced through the new-lens reset: a macro
                    ! may be pure analysis operating on the CURRENT lens system.
                    ! A lens-defining macro starts with LEN NEW (the Bentley
                    ! macros do), and that command performs the shared
                    ! newlens.zoa reset + undo-baseline itself.
                    ! Flag the macro so that LEN NEW prompts to discard open
                    ! plots (save/restore, so a macro calling a macro nests).
                    savedMacroFlag = in_macro_load
                    in_macro_load = .TRUE.
                    call process_zoa_file(trim(fileName))
                    in_macro_load = savedMacroFlag
                end if
            end if
        else
            fileName = trim(iptStr)
            locDot = index(fileName, '.')
            if (locDot == 0) fileName = trim(fileName)//'.zoa'
            fileName = getRestoreFilePath(trim(fileName))
            if (doesFileExist(trim(fileName))) then
                if (present(printOnly)) then
                    call process_zoa_file(trim(fileName), printOnly=.TRUE.)
                else
                    call loadLensFromZoaPath(trim(fileName))
                end if
            end if
        end if
    end procedure processZoaFileInput

    ! Shared lens-restore sequence for every user-facing .zoa load (RES, RESAUTO,
    ! File > Open).  fullPath is the already-resolved path to the file.
    module procedure loadLensFromZoaPath
        use zoa_file_handler, only: process_zoa_file, doesFileExist, zinPathFromZoa
        use mod_lens_data_manager, only: ldm
        use undo_manager, only: undo_reset_baseline
        use zoa_ui_callbacks, only: notify_load_zin, notify_close_all_tabs
        implicit none
        character(len=2048) :: zinPath
        logical :: hasZin

        zinPath = zinPathFromZoa(trim(fullPath))
        hasZin  = doesFileExist(trim(zinPath))

        ! Loading a different lens invalidates every open plot, so offer to
        ! discard them -- the same prompt LEN NEW and CV2PRG/ZMX2PRG show.  Ask
        ! BEFORE the lens is replaced, so the plots on screen still match the
        ! lens being discarded.  Automatically a no-op headless (callback
        ! unregistered) and when no plots are open.
        !
        ! Skipped when the lens has a .zin companion: that path replaces the
        ! plots wholesale below (closes all, then restores the saved set), so
        ! asking whether to keep the old lens's plots would be meaningless.
        if (.not. hasZin) then
            call notify_close_all_tabs("You are about to open a new " //&
            &"lens system.  This will invalidate all plots.   " //&
            &"Press yes to close them.")
        end if

        ! Saved .zoa files begin with LEN NEW, which performs the newlens.zoa
        ! reset itself.  Only pre-reset a file that does not, so the template
        ! (DCON ALL / DEL VIG / DEL APE SA ...) runs exactly once per load.
        if (.not. zoaFileStartsWithLenNew(fullPath)) call resetToNewLensTemplate()
        call process_zoa_file(trim(fullPath))
        call ldm%load_surfaces_from_alens()
        ! A user load replaces the lens: reset the undo history with it as baseline.
        call undo_reset_baseline()
        ! If a companion .zin plot file exists alongside the lens, tell the GUI
        ! to load it (no-op in headless mode / when no hook is registered).
        if (hasZin) call notify_load_zin(trim(zinPath))
    end procedure loadLensFromZoaPath

    ! True when the first non-blank, non-comment line of a .zoa file is LEN NEW.
    logical function zoaFileStartsWithLenNew(path) result(res)
        use strings, only: uppercase
        implicit none
        character(len=*), intent(in) :: path
        character(len=256) :: line
        integer :: fID, ios
        res = .false.
        open(newunit=fID, file=trim(path), status='old', action='read', iostat=ios)
        if (ios /= 0) return
        do
            read(fID, '(A)', iostat=ios) line
            if (ios /= 0) exit
            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '!') cycle
            res = (uppercase(line(1:7)) == 'LEN NEW')
            exit
        end do
        close(fID)
    end function zoaFileStartsWithLenNew

    !## cmd:      PRT
    !## syntax:   PRT file
    !## category: File I/O
    !## desc:     Print the contents of a text file to the terminal.
    !##
    module procedure printFile
        use zoa_file_handler
        use command_utils, only: isInputNumber
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, locStr, locDot
        character(len=256) :: fileName

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2) then
            call processZoaFileInput(trim(tokens(2)), printOnly=.TRUE.)
        else
            call zoa_emit("Error!  Expect two inputs, eg PRT file or PRT zoa_macro:file", "red")
        end if
    end procedure printFile

    !## cmd:      CX
    !## syntax:   CX
    !## category: Analysis
    !## desc:     Print chief-ray X data.
    !##
    !## cmd:      CY
    !## syntax:   CY
    !## category: Analysis
    !## desc:     Print chief-ray Y data.
    !##
    module procedure getRayData
        use DATLEN, only: RAYRAY
        use data_registers, only: setData

        character(len=LEN(iptStr)) :: savStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        integer, allocatable :: fields(:), wavelengths(:), surfaces(:)
        real(kind=real64) :: relApeX, relApeY

        relApeX = 0.0
        relApeY = 0.0
        call parse(trim(iptStr), ' ', tokens, numTokens)
        savStr = iptStr

        fields      = cmd_parser_get_int_input_for_prefix('f', tokens(1:numTokens))
        wavelengths = cmd_parser_get_int_input_for_prefix('w', tokens(1:numTokens))
        surfaces    = cmd_parser_get_int_input_for_prefix('s', tokens(1:numTokens))
        call cmd_parser_get_real_pair(tokens(1:numTokens), relApeX, relApeY, real1Bounds=[-1.0,1.0], real2Bounds=[-1.0,1.0])

        if (size(fields) == 1 .and. size(wavelengths) == 1 .and. size(surfaces) == 1) then
            call PROCESSILENT("RSI f"//trim(int2str(fields(1)))//" w"//trim(int2str(wavelengths(1)))// &
            & " "//trim(real2str(relApeX))//" "//trim(real2str(relApeY)))
            select case(trim(tokens(1)))
            case('CY')
                call setData(savStr, RAYRAY(5,surfaces(1)))
                print *, "Data stored is ", RAYRAY(5,surfaces(1))
            case('CX')
                call setData(savStr, RAYRAY(4,surfaces(1)))
                print *, "Data stored is ", RAYRAY(4,surfaces(1))
            end select
        else
            call zoa_emit("Error:  Was unable to parse intput to get only one surface, field point and wavelength","red")
        end if
    end procedure getRayData

    !## cmd:      TERM
    !## syntax:   TERM
    !## category: Utilities
    !## desc:     Reset terminal output to the default view.
    !##
    module procedure execTERM
        use global_widgets, only: ioConfig
        use zoa_ui, only: ID_TERMINAL_DEFAULT
        use GLOBALS, only: HEADLESS_MODE
        if (HEADLESS_MODE) return
        call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
        call zoa_emit("Terminal output redirected to default", "black")
    end procedure execTERM

    !## cmd:      THO
    !## syntax:   THO
    !## category: Analysis
    !## desc:     List the third-order (Seidel) aberration coefficients.
    !##
    module procedure execTHO
        use global_widgets, only: sysConfig
        implicit none
        interface
            subroutine MMAB3_NEW(YFLAG, idxWV, printTable)
                logical, intent(in) :: YFLAG
                integer, intent(in) :: idxWV
                logical, optional, intent(in) :: printTable
            end subroutine
        end interface
        call MMAB3_NEW(.TRUE., sysConfig%refWavelengthIndex, .TRUE.)
    end procedure execTHO

    !## cmd:      RAYREF
    !## syntax:   RAYREF
    !## category: Analysis
    !## desc:     Compute the per-field reference rays (R1..R5).
    !##
    module procedure execRAYREF
        use mod_reference_rays, only: refRays, NUM_REF_RAYS
        use type_utils, only: real2str, int2str
        use iso_fortran_env, only: real64
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, i, k
        real(real64) :: xi, yi
        logical :: ok
        character(len=6), parameter :: rayLbl(NUM_REF_RAYS) = &
            ['R1CHF ', 'R2UPR ', 'R3LWR ', 'R4+XSG', 'R5-XSG']

        call parse(trim(iptStr), ' ', tokens, numTokens)

        ! RAYREF [BUILD] : (re)build the store.  RAYREF LIST : dump it as text.
        if (numTokens >= 2 .and. trim(tokens(2)) == 'LIST') then
            if (.not. refRays%valid .or. refRays%getNumFields() < 1) then
                call zoa_emit("RAYREF: no reference rays stored. Run RAYREF BUILD first.", "red")
                return
            end if
            call zoa_emit("Reference rays: "//trim(int2str(refRays%getNumFields()))//" field(s)", "black")
            do i = 1, refRays%getNumFields()
                call zoa_emit("F"//trim(int2str(i))//"  x="// &
                    trim(real2str(refRays%fields(i)%xfrac,4))//" y="// &
                    trim(real2str(refRays%fields(i)%yfrac,4))//"  wl="// &
                    trim(int2str(refRays%fields(i)%wavelength)), "black")
                do k = 1, NUM_REF_RAYS
                    call refRays%getImagePoint(i, k, xi, yi, ok)
                    call zoa_emit("  "//rayLbl(k)//"  ok="//logChar(refRays%fields(i)%ray(k)%traced_ok)// &
                        " vig="//logChar(refRays%isVignetted(i,k))// &
                        "  Ximg="//trim(real2str(xi,4))//" Yimg="//trim(real2str(yi,4)), "black")
                end do
            end do
        else
            call refRays%populate()
            if (refRays%valid) then
                call zoa_emit("Reference rays built for "// &
                    trim(int2str(refRays%getNumFields()))//" field(s).", "black")
            else
                call zoa_emit("RAYREF: no field points defined; nothing built.", "red")
            end if
        end if

    contains
        pure function logChar(b) result(c)
            logical, intent(in) :: b
            character(len=1) :: c
            c = 'F'
            if (b) c = 'T'
        end function
    end procedure execRAYREF

    !## cmd:      TRACECMP
    !## syntax:   TRACECMP [n] [w] [BRIEF]
    !## category: Diagnostics
    !## desc:     Parity check of the ray trace engine against the legacy tracer.
    !##           Traces an n x n pupil grid (default 8) at wavelength slot w
    !##           (default: reference wavelength) for the current lens and field
    !##           with both tracers and compares status codes and the per-surface
    !##           ray data.  Run FOB first.  BRIEF prints only deterministic
    !##           counts and the PASS/FAIL/SKIP verdict.
    !##
    module procedure execTRACECMP
        use DATLEN, only: NEWOBJ, NEWIMG, REFEXT, RAYRAY, RAYCOD, RELX, RELY, &
                          WWQ, WW1, WW2, WW3, WW4, WW5, WVN, CACOCH, MSG, STOPP, &
                          ANAAIM, NOCOAT, GRASET, DXFSET, RAYEXT
        use mod_system, only: sys_wl_ref
        use mod_ray_trace_engine, only: trace_context, ray_request, ray_result, &
                                        trace_ray, RR_N, RAY_OK
        use mod_ray_trace_builder, only: build_trace_context
        use zoa_output, only: zoa_emit
        implicit none

        ! Largest scaled difference |engine - legacy| / max(1,|legacy|) that
        ! still counts as agreement; ~1e5 x double epsilon, i.e. allows
        ! different-but-equivalent operation ordering, not algorithmic drift.
        real(real64), parameter :: PARITY_TOL = 1.0e-9_real64
        integer, parameter :: NCMP = 25
        character(len=12), parameter :: slotName(NCMP) = [character(len=12) :: &
            'X', 'Y', 'Z', 'L', 'M', 'N', 'OPL', 'LEN', 'COSI', 'COSIP', &
            'UX', 'UY', 'LN', 'MN', 'NN', 'XOLD', 'YOLD', 'ZOLD', 'LOLD', &
            'MOLD', 'NOLD', 'OPL_TOTAL', 'RV', 'POSRAY', 'ENERGY']

        character(len=80) :: tokens(40)
        character(len=200) :: line
        integer :: numTokens, i, ios, n, iwl, iy, ix, k, s, ntot, nok, nfail
        integer :: nStatMis, nBoth, ir, worstSlot
        integer :: codeHist(0:99), nOther
        logical :: brief, nSet, wSet
        real(real64) :: delfob, px, py, dv, rv, worstVal, px0
        real(real64) :: slotMax(NCMP), slotPx(NCMP), slotPy(NCMP)
        integer :: slotSurf(NCMP)

        ! saved legacy state
        logical :: sANAAIM, sNOCOAT, sGRASET, sDXFSET, sMSG, sRAYEXT, sSPDTRA
        integer :: sCACOCH, sSTOPP, sRAYCOD(2)
        character(len=8) :: sWWQ
        real(real64) :: sWW1, sWW2, sWW3, sWW4, sWW5, sWVN, sRELX, sRELY
        real(real64), allocatable :: sRAYRAY(:,:)
        logical :: SPDTRA
        common /SPRA1/ SPDTRA

        type(trace_context) :: ctx
        type(ray_request) :: req
        type(ray_result) :: res
        integer, allocatable :: legCode(:), legSurf(:)
        real(real64), allocatable :: legRR(:,:,:)

        call parse(trim(iptStr), ' ', tokens, numTokens)

        brief = .false.
        nSet = .false.
        wSet = .false.
        n = 8
        iwl = nint(sys_wl_ref())
        do i = 2, numTokens
            if (trim(tokens(i)) == 'BRIEF') then
                brief = .true.
            else
                read(tokens(i), *, iostat=ios) dv
                if (ios /= 0) then
                    call zoa_emit("TRACECMP: cannot parse '"//trim(tokens(i))//"'", "red")
                    return
                end if
                if (.not. nSet) then
                    n = nint(dv); nSet = .true.
                else if (.not. wSet) then
                    iwl = nint(dv); wSet = .true.
                end if
            end if
        end do
        if (n < 1 .or. n > 200) then
            call zoa_emit("TRACECMP: grid size must be 1..200", "red")
            return
        end if
        if (iwl < 1 .or. iwl > 10) then
            call zoa_emit("TRACECMP: wavelength slot must be 1..10", "red")
            return
        end if

        if (.not. REFEXT) then
            call zoa_emit("TRACECMP: no chief ray exists - run FOB first", "red")
            return
        end if

        ntot = n*n
        allocate(legCode(ntot), legSurf(ntot))
        allocate(legRR(NCMP, NEWOBJ:NEWIMG, ntot))
        allocate(sRAYRAY(size(RAYRAY,1), 0:ubound(RAYRAY,2)))

        ! ---- save every global the legacy trace touches ----
        sRAYRAY = RAYRAY
        sRAYCOD = RAYCOD
        sRELX = RELX; sRELY = RELY
        sWWQ = WWQ
        sWW1 = WW1; sWW2 = WW2; sWW3 = WW3; sWW4 = WW4; sWW5 = WW5
        sWVN = WVN; sCACOCH = CACOCH; sSTOPP = STOPP; sMSG = MSG
        sANAAIM = ANAAIM; sNOCOAT = NOCOAT; sGRASET = GRASET; sDXFSET = DXFSET
        sRAYEXT = RAYEXT; sSPDTRA = SPDTRA

        ! ---- legacy trace of the grid (same setup as COMPAP) ----
        delfob = 2.0_real64/real(n, real64)
        k = 0
        do iy = 0, n-1
            do ix = 0, n-1
                k = k + 1
                py = (-1.0_real64 + delfob/2.0_real64) + real(iy, real64)*delfob
                px = (-1.0_real64 + delfob/2.0_real64) + real(ix, real64)*delfob
                WWQ = 'CAOB'
                WW1 = py
                WW2 = px
                WW3 = real(iwl, real64)
                WVN = real(iwl, real64)
                CACOCH = 1
                SPDTRA = .true.
                MSG = .false.
                STOPP = 0
                ANAAIM = .false.
                WW4 = 1.0_real64
                NOCOAT = .false.
                GRASET = .false.
                DXFSET = .false.
                call RAYTRA2
                legCode(k) = RAYCOD(1)
                legSurf(k) = RAYCOD(2)
                legRR(:, :, k) = RAYRAY(1:NCMP, NEWOBJ:NEWIMG)
            end do
        end do

        ! ---- restore ----
        RAYRAY = sRAYRAY
        RAYCOD = sRAYCOD
        RELX = sRELX; RELY = sRELY
        WWQ = sWWQ
        WW1 = sWW1; WW2 = sWW2; WW3 = sWW3; WW4 = sWW4; WW5 = sWW5
        WVN = sWVN; CACOCH = sCACOCH; STOPP = sSTOPP; MSG = sMSG
        ANAAIM = sANAAIM; NOCOAT = sNOCOAT; GRASET = sGRASET; DXFSET = sDXFSET
        RAYEXT = sRAYEXT; SPDTRA = sSPDTRA

        ! ---- legacy summary ----
        nok = count(legCode == 0)
        nfail = ntot - nok
        codeHist = 0
        nOther = 0
        do k = 1, ntot
            if (legCode(k) /= 0) then
                if (legCode(k) >= 0 .and. legCode(k) <= 99) then
                    codeHist(legCode(k)) = codeHist(legCode(k)) + 1
                else
                    nOther = nOther + 1
                end if
            end if
        end do

        write(line, '(A,I0,A,I0,A,I0,A)') 'TRACECMP: grid ', n, 'x', n, ' (', ntot, ' rays)'
        write(line, '(A,A,I0)') trim(line), ', wavelength slot ', iwl
        call zoa_emit(trim(line), "black")
        write(line, '(A,I0,A,I0,A,I0)') 'LEGACY: traced ', ntot, ', ok ', nok, ', failed ', nfail
        call zoa_emit(trim(line), "black")
        do i = 0, 99
            if (codeHist(i) > 0) then
                write(line, '(A,I0,A,I0)') '  LEGACY FAIL CODE ', i, ': ', codeHist(i)
                call zoa_emit(trim(line), "black")
            end if
        end do
        if (nOther > 0) then
            write(line, '(A,I0)') '  LEGACY FAIL CODE (other): ', nOther
            call zoa_emit(trim(line), "black")
        end if

        ! ---- engine ----
        call build_trace_context(ctx)
        if (.not. ctx%supported) then
            call zoa_emit('ENGINE NOT SUPPORTED: '//trim(ctx%reason), "black")
            call zoa_emit('TRACECMP: SKIP', "black")
            return
        end if
        call zoa_emit('ENGINE SUPPORTED', "black")

        nStatMis = 0
        nBoth = 0
        slotMax = 0.0_real64
        slotPx = 0.0_real64
        slotPy = 0.0_real64
        slotSurf = 0
        k = 0
        do iy = 0, n-1
            do ix = 0, n-1
                k = k + 1
                req%py = (-1.0_real64 + delfob/2.0_real64) + real(iy, real64)*delfob
                req%px = (-1.0_real64 + delfob/2.0_real64) + real(ix, real64)*delfob
                req%iwl = iwl
                req%weight = 1.0_real64
                call trace_ray(ctx, req, res)
                if (res%status /= legCode(k)) then
                    nStatMis = nStatMis + 1
                    cycle
                end if
                if (res%status /= RAY_OK) cycle
                nBoth = nBoth + 1
                do s = NEWOBJ, NEWIMG
                    do ir = 1, NCMP
                        rv = legRR(ir, s, k)
                        dv = abs(res%rr(ir, s) - rv)/max(1.0_real64, abs(rv))
                        if (dv > slotMax(ir)) then
                            slotMax(ir) = dv
                            slotPx(ir) = req%px
                            slotPy(ir) = req%py
                            slotSurf(ir) = s
                        end if
                    end do
                end do
            end do
        end do

        write(line, '(A,I0)') 'STATUS MISMATCHES: ', nStatMis
        call zoa_emit(trim(line), "black")

        worstVal = 0.0_real64
        worstSlot = 0
        do ir = 1, NCMP
            if (slotMax(ir) > worstVal) then
                worstVal = slotMax(ir)
                worstSlot = ir
            end if
        end do

        if (.not. brief) then
            write(line, '(A,I0,A)') 'Compared ', nBoth, ' rays OK in both tracers'
            call zoa_emit(trim(line), "black")
            do ir = 1, NCMP
                if (slotMax(ir) > 0.0_real64) then
                    write(line, '(A,I0,1X,A,A,ES10.3,A,F8.4,A,F8.4,A,I0)') 'SLOT ', ir, &
                        trim(slotName(ir)), ' max scaled diff ', slotMax(ir), &
                        ' at px=', slotPx(ir), ' py=', slotPy(ir), ' surf ', slotSurf(ir)
                    call zoa_emit(trim(line), "black")
                end if
            end do
        end if

        if (nStatMis == 0 .and. worstVal <= PARITY_TOL) then
            call zoa_emit('TRACECMP: PASS', "black")
        else
            if (worstSlot > 0 .and. worstVal > PARITY_TOL) then
                write(line, '(A,I0,1X,A)') 'TRACECMP: FAIL (worst slot ', worstSlot, trim(slotName(worstSlot))//')'
            else
                line = 'TRACECMP: FAIL'
            end if
            call zoa_emit(trim(line), "black")
        end if
    end procedure execTRACECMP

end submodule mod_codev_utils
