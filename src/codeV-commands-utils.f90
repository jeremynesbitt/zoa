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

    !## cmd:      ENGINETEST
    !## syntax:   ENGINETEST [BRIEF]
    !## category: Diagnostics
    !## desc:     Unit test of the pure surface-placement transforms against the
    !##           legacy ones.  For the current lens, runs a fixed set of rays
    !##           through legacy TRNSF2 and place_into_surface at every surface
    !##           NEWOBJ+1..NEWIMG, and the same points through BAKONE/FORONEL
    !##           and their ports on surface NEWOBJ+1, comparing results bit for
    !##           bit.  Also runs every surface's clear aperture / obscuration
    !##           check (legacy CACHEK) against its port, check_apertures, over a
    !##           grid of points, comparing the return code and every value CACHEK
    !##           leaves behind.  BRIEF prints only counts and verdicts.
    !##
    module procedure execENGINETEST
        use DATLEN, only: NEWOBJ, NEWIMG, R_X, R_Y, R_Z, R_L, R_M, R_N, R_I, &
                          R_TX, R_TY, R_TZ, RAYCOD, STOPP, MSG, AIMTOL, &
                          X0, Y0, XT, YT, NP, IPOLYX, IPOLYY
        use mod_surface_placement, only: surface_placement, place_into_surface, &
                                         back_to_object, forward_from_object, &
                                         pivot_normal_needed
        use mod_surface_apertures, only: surface_apertures, check_apertures
        use mod_surface, only: set_surf_clap_type, set_surf_clap_dim, set_surf_clap_tilt, &
                               set_surf_coat_type, set_surf_cobs_poly, &
                               set_surf_cobs_ape_type, set_surf_cobs_ape_data, &
                               set_surf_cobs_era_type, set_surf_cobs_era_data
        use mod_ray_trace_builder, only: placement_of, apertures_of
        use zoa_output, only: zoa_emit
        use iso_fortran_env, only: int64
        implicit none

        integer, parameter :: NRAY = 20
        character(len=80) :: tokens(40)
        character(len=200) :: line
        character(len=8), parameter :: rname(3) = [character(len=8) :: 'TRNSF2', 'BAKONE', 'FORONEL']
        integer :: numTokens, i, k, s, ncase(3), nbad(3), nobj1, nglob
        logical :: brief
        real(real64) :: pos(3, NRAY), dirv(3, NRAY), raw(3, NRAY), nrm
        real(real64) :: a(6), b(6), maxd(3), d
        real(real64) :: sRX, sRY, sRZ, sRL, sRM, sRN, sTX, sTY, sTZ
        integer :: sRI
        real(real64) :: jz, pl, pm, pn
        type(surface_placement) :: pp, pc
        type(surface_placement), allocatable :: pl_all(:)
        character(len=60) :: failName

        ! ---- CACHEK test state ----
        ! The legacy COMMON blocks CACHEK writes or reads (layouts as in CACHEK).
        integer :: l_caeras, l_coeras
        real(real64) :: l_ls
        common /CACO/ l_caeras, l_coeras, l_ls
        integer :: l_spd1, l_spd2
        common /SPRA2/ l_spd1, l_spd2
        logical :: l_nocobs
        common /PSFCOBS/ l_nocobs
        integer, parameter :: NGRID = 41, MAXPTS_T = 4500
        integer :: cc_cases, cc_bad, cc_blk6, cc_blk7, cc_surfs, cc_ipoly
        real(real64) :: cc_pts(2, MAXPTS_T)
        integer :: cc_np
        type(surface_apertures) :: cc_ap
        ! saved legacy globals
        integer :: sSTOP, sRAYCOD(2), sCAERAS, sCOERAS, sSPD1, sSPD2, sNP
        real(real64) :: sLS, sAIMTOL, sX0, sY0, sXT(200), sYT(200)
        logical :: sMSG, sNOCOBS

        ! ---- HITSUR test tallies ----
        integer :: hs_cases, hs_skip, hs_bad, hs_c4, hs_c20, hs_dum, hs_refl, hs_rv, hs_surfs, hs_gates

        call parse(trim(iptStr), ' ', tokens, numTokens)
        brief = .false.
        do i = 2, numTokens
            if (trim(tokens(i)) == 'BRIEF') then
                brief = .true.
            else
                call zoa_emit("ENGINETEST: unknown argument '"//trim(tokens(i))//"'", "red")
                return
            end if
        end do

        ! Fixed deterministic inputs: positions spread over +-12 incl. off-axis x
        ! and y, and a z offset; directions from fixed raw vectors, normalized.
        do k = 1, NRAY
            pos(1, k) = 0.75_real64*real(mod(k*7, 11) - 5, real64)
            pos(2, k) = 0.6_real64*real(mod(k*5, 13) - 6, real64)
            pos(3, k) = 0.1_real64*real(mod(k*3, 7) - 3, real64)
            raw(1, k) = 0.07_real64*real(mod(k*3, 9) - 4, real64)
            raw(2, k) = 0.05_real64*real(mod(k*4, 11) - 5, real64)
            raw(3, k) = 1.0_real64
        end do
        ! a few hand-picked extremes
        pos(:, 1) = [0.0_real64, 0.0_real64, 0.0_real64]
        raw(:, 1) = [0.0_real64, 0.0_real64, 1.0_real64]
        pos(:, 2) = [5.0_real64, 0.0_real64, 0.0_real64]
        raw(:, 2) = [0.0_real64, 0.0_real64, 1.0_real64]
        pos(:, 3) = [0.0_real64, 5.0_real64, 0.0_real64]
        raw(:, 3) = [0.0_real64, 0.0_real64, 1.0_real64]
        raw(:, 4) = [0.3_real64, 0.0_real64, 1.0_real64]
        raw(:, 5) = [0.0_real64, 0.3_real64, 1.0_real64]
        raw(:, 6) = [0.25_real64, -0.35_real64, 1.0_real64]
        raw(:, 7) = [-0.5_real64, 0.4_real64, 0.8_real64]
        do k = 1, NRAY
            nrm = sqrt(raw(1, k)**2 + raw(2, k)**2 + raw(3, k)**2)
            dirv(:, k) = raw(:, k)/nrm
        end do

        nobj1 = NEWOBJ + 1
        allocate(pl_all(NEWOBJ:NEWIMG))
        do s = NEWOBJ, NEWIMG
            pl_all(s) = placement_of(s)
        end do

        ! save every legacy global we write
        sRX = R_X; sRY = R_Y; sRZ = R_Z; sRL = R_L; sRM = R_M; sRN = R_N
        sRI = R_I; sTX = R_TX; sTY = R_TY; sTZ = R_TZ

        ncase = 0
        nbad = 0
        maxd = 0.0_real64

        ! ---- TRNSF2 vs place_into_surface ----
        do s = nobj1, NEWIMG
            do k = 1, NRAY
                R_X = pos(1, k); R_Y = pos(2, k); R_Z = pos(3, k)
                R_L = dirv(1, k); R_M = dirv(2, k); R_N = dirv(3, k)
                R_I = s
                call TRNSF2
                a = [R_X, R_Y, R_Z, R_L, R_M, R_N]
                b = [pos(:, k), dirv(:, k)]
                call place_into_surface(pl_all(s-1), pl_all(s), b(1), b(2), b(3), b(4), b(5), b(6))
                call cmp6(1)
            end do
        end do

        ! ---- BAKONE vs back_to_object (surface NEWOBJ+1) ----
        do k = 1, NRAY
            R_TX = pos(1, k); R_TY = pos(2, k); R_TZ = pos(3, k)
            call BAKONE
            a(1:3) = [R_TX, R_TY, R_TZ]
            a(4:6) = 0.0_real64
            b(1:3) = pos(:, k)
            b(4:6) = 0.0_real64
            call back_to_object(pl_all(nobj1), pl_all(NEWOBJ)%thickness, b(1), b(2), b(3))
            call cmp6(2)
        end do

        ! ---- FORONEL vs forward_from_object (surface NEWOBJ+1) ----
        pl = 0.0_real64; pm = 0.0_real64; pn = 1.0_real64
        if (pivot_normal_needed(pl_all(nobj1))) then
            ! the one piece of FORONEL that is surface geometry: SAGINT's normal
            call SAGINT(nobj1, pl_all(nobj1)%pivot_x, pl_all(nobj1)%pivot_y, jz, pl, pm, pn)
        end if
        do k = 1, NRAY
            R_TX = pos(1, k); R_TY = pos(2, k); R_TZ = pos(3, k)
            call FORONEL
            a(1:3) = [R_TX, R_TY, R_TZ]
            a(4:6) = 0.0_real64
            b(1:3) = pos(:, k)
            b(4:6) = 0.0_real64
            call forward_from_object(pl_all(nobj1), pl, pm, pn, b(1), b(2), b(3))
            call cmp6(3)
        end do

        ! ---- CACHEK vs check_apertures ----
        sSTOP = STOPP; sRAYCOD = RAYCOD; sCAERAS = l_caeras; sCOERAS = l_coeras
        sSPD1 = l_spd1; sSPD2 = l_spd2; sLS = l_ls; sAIMTOL = AIMTOL; sMSG = MSG
        sNOCOBS = l_nocobs; sX0 = X0; sY0 = Y0; sNP = NP; sXT = XT(1:200); sYT = YT(1:200)
        cc_cases = 0; cc_bad = 0; cc_blk6 = 0; cc_blk7 = 0; cc_surfs = 0; cc_ipoly = 0
        do s = nobj1, NEWIMG - 1
            cc_ap = apertures_of(s)
            if (cc_ap%clap_type /= 0 .or. cc_ap%cobs_type /= 0) cc_surfs = cc_surfs + 1
            call run_cachek_surface(s)
        end do
        ! IPOLY (type 6) shapes can only be loaded from an IPOLYnn.DAT file in the
        ! working directory, so no script can set them up.  Cover those branches
        ! by temporarily writing synthetic polygons into the first surface of the
        ! legacy store, running the same comparison, and restoring it.
        ! (the first such surface without the footblock flag, which would skip it)
        do s = nobj1, NEWIMG - 1
            cc_ap = apertures_of(s)
            if (cc_ap%footblok_flag /= 1) then
                call run_synthetic_ipoly(s)
                exit
            end if
        end do
        STOPP = sSTOP; RAYCOD = sRAYCOD; l_caeras = sCAERAS; l_coeras = sCOERAS
        l_spd1 = sSPD1; l_spd2 = sSPD2; l_ls = sLS; AIMTOL = sAIMTOL; MSG = sMSG
        l_nocobs = sNOCOBS; X0 = sX0; Y0 = sY0; NP = sNP; XT(1:200) = sXT; YT(1:200) = sYT

        ! ---- HITSUR vs hit_and_interact ----
        call run_hitsur()

        ! restore
        R_X = sRX; R_Y = sRY; R_Z = sRZ; R_L = sRL; R_M = sRM; R_N = sRN
        R_I = sRI; R_TX = sTX; R_TY = sTY; R_TZ = sTZ

        write(line, '(A,I0,A,I0)') 'ENGINETEST: surfaces ', NEWIMG - NEWOBJ + 1, ', input rays ', NRAY
        call zoa_emit(trim(line), "black")
        ! per-surface flag summary (integers only), so a reader of the output can
        ! see which TRNSF2/BAKONE/FORONEL branches the lens exercises
        line = 'TILT FLAGS:'
        do s = NEWOBJ, NEWIMG
            write(line, '(A,1X,I0)') trim(line), pl_all(s)%tilt_flag
        end do
        call zoa_emit(trim(line), "black")
        line = 'DECENTER FLAGS:'
        do s = NEWOBJ, NEWIMG
            write(line, '(A,1X,I0)') trim(line), pl_all(s)%decenter_flag
        end do
        call zoa_emit(trim(line), "black")
        nglob = 0
        do s = NEWOBJ, NEWIMG
            if (pl_all(s)%global_dx /= 0.0_real64 .or. pl_all(s)%global_dy /= 0.0_real64 .or. &
                pl_all(s)%global_dz /= 0.0_real64 .or. pl_all(s)%global_alpha /= 0.0_real64 .or. &
                pl_all(s)%global_beta /= 0.0_real64 .or. pl_all(s)%global_gamma /= 0.0_real64) &
                nglob = nglob + 1
        end do
        write(line, '(A,I0,A,L1)') 'SURFACES WITH GLOBAL DATA: ', nglob, &
            ', FORONEL PIVOT PATH: ', pivot_normal_needed(pl_all(nobj1))
        call zoa_emit(trim(line), "black")
        failName = ''
        do i = 1, 3
            if (brief) then
                write(line, '(A,A,I0,A,I0)') trim(rname(i)), ': cases ', ncase(i), &
                    ', not bit-identical ', nbad(i)
            else
                write(line, '(A,A,I0,A,I0,A,ES10.3)') trim(rname(i)), ': cases ', ncase(i), &
                    ', not bit-identical ', nbad(i), ', max abs diff ', maxd(i)
            end if
            call zoa_emit(trim(line), "black")
            if (nbad(i) > 0 .and. len_trim(failName) == 0) failName = rname(i)
        end do
        write(line, '(A,I0,A,I0)') 'CACHEK: cases ', cc_cases, ', mismatches ', cc_bad
        call zoa_emit(trim(line), "black")
        if (.not. brief) then
            write(line, '(A,I0,A,I0,A,I0,A,I0)') 'CACHEK DETAIL: surfaces with apertures ', cc_surfs, &
                ', blocked by clap ', cc_blk6, ', blocked by cobs ', cc_blk7, ', synthetic cases ', cc_ipoly
            call zoa_emit(trim(line), "black")
        end if
        write(line, '(A,I0,A,I0,A,I0)') 'HITSUR: cases ', hs_cases, ', skipped ', hs_skip, &
            ', mismatches ', hs_bad
        call zoa_emit(trim(line), "black")
        if (.not. brief) then
            write(line, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A,I0)') 'HITSUR DETAIL: surfaces compared ', hs_surfs, &
                ', code 4 ', hs_c4, ', code 20 ', hs_c20, ', dummy ', hs_dum, ', reflection ', hs_refl, &
                ', reversed ', hs_rv, ', gate checks ', hs_gates
            call zoa_emit(trim(line), "black")
        end if
        if (cc_bad > 0 .and. len_trim(failName) == 0) failName = 'CACHEK'
        if (hs_bad > 0 .and. len_trim(failName) == 0) failName = 'HITSUR'
        if (len_trim(failName) == 0) then
            call zoa_emit('ENGINETEST: PASS', "black")
        else
            call zoa_emit('ENGINETEST: FAIL ('//trim(failName)//')', "black")
        end if

    contains

        ! compare a(1:6) (legacy) with b(1:6) (port) bitwise; accumulate for routine r
        subroutine cmp6(r)
            integer, intent(in) :: r
            integer :: j
            logical :: bad
            ncase(r) = ncase(r) + 1
            bad = .false.
            do j = 1, 6
                if (transfer(a(j), 0_int64) /= transfer(b(j), 0_int64)) then
                    bad = .true.
                    d = abs(a(j) - b(j))
                    if (d > maxd(r) .or. d /= d) maxd(r) = d
                end if
            end do
            if (bad) nbad(r) = nbad(r) + 1
        end subroutine cmp6

        ! Run every test point and mode on surface s, whose apertures are in the
        ! legacy store.
        subroutine run_cachek_surface(sf)
            integer, intent(in) :: sf
            real(real64) :: span, dm, v(2), w(2), scl(5), px, py
            integer :: ix, iy, ir, isx, isy, iv, iw, isc, k, m
            real(real64) :: tols(2)
            real(real64) :: rec(6, 4)

            cc_ap = apertures_of(sf)
            ! the four aperture records: clap, cobs, clap erase, cobs erase
            rec = 0.0_real64
            rec(1:5, 1) = cc_ap%clap_dim
            rec(1:5, 2) = cc_ap%cobs_dim(1:5)
            rec(1:5, 3) = cc_ap%clap_erase_dim(1:5)
            rec(1:5, 4) = cc_ap%cobs_erase_dim(1:5)
            ! legacy layout: dims 1,2 are the sizes, 3 = y decenter, 4 = x decenter
            dm = 0.0_real64
            do ir = 1, 4
                dm = max(dm, abs(rec(1, ir)) + abs(rec(3, ir)) + abs(rec(4, ir)))
                dm = max(dm, abs(rec(2, ir)) + abs(rec(3, ir)) + abs(rec(4, ir)))
            end do
            if (dm == 0.0_real64) dm = 10.0_real64
            span = 1.5_real64*dm

            ! grid
            cc_np = 0
            do iy = 0, NGRID - 1
                do ix = 0, NGRID - 1
                    cc_np = cc_np + 1
                    cc_pts(1, cc_np) = -span + 2.0_real64*span*real(ix, real64)/real(NGRID - 1, real64)
                    cc_pts(2, cc_np) = -span + 2.0_real64*span*real(iy, real64)/real(NGRID - 1, real64)
                end do
            end do
            ! points on the nominal edges and corners of each record (+- tiny)
            scl = [1.0_real64, 1.0_real64 + 1.0e-8_real64, 1.0_real64 - 1.0e-8_real64, &
                   1.0_real64 + 1.0e-6_real64, 1.0_real64 - 1.0e-6_real64]
            do ir = 1, 4
                v = [rec(1, ir), rec(2, ir)]
                w = [rec(1, ir), rec(2, ir)]
                do iv = 1, 2
                    do iw = 1, 2
                        do isx = -1, 1
                            do isy = -1, 1
                                do isc = 1, 5
                                    if (cc_np + 1 > MAXPTS_T) exit
                                    cc_np = cc_np + 1
                                    cc_pts(1, cc_np) = rec(4, ir) + real(isx, real64)*v(iv)*scl(isc)
                                    cc_pts(2, cc_np) = rec(3, ir) + real(isy, real64)*w(iw)*scl(isc)
                                end do
                            end do
                        end do
                    end do
                end do
            end do
            tols = [sAIMTOL, 1.0e-3_real64]

            do k = 1, cc_np
                px = cc_pts(1, k); py = cc_pts(2, k)
                do m = 1, 2
                    ! plain checks, all three modes
                    call one_case(sf, px, py, 0.0_real64, 0.0_real64, 0.0_real64, 0, .false., tols(m))
                    call one_case(sf, px, py, 0.0_real64, 0.0_real64, 0.0_real64, 1, .false., tols(m))
                    call one_case(sf, px, py, 0.0_real64, 0.0_real64, 0.0_real64, 2, .false., tols(m))
                    ! arbitrary offsets and extra tilt (the JK1/JK2/JK3 path)
                    call one_case(sf, px, py, 0.3_real64, -0.2_real64, 7.5_real64, 0, .false., tols(m))
                    call one_case(sf, px, py, -0.15_real64, 0.25_real64, -12.0_real64, 1, .false., tols(m))
                    call one_case(sf, px, py, 0.1_real64, 0.05_real64, 20.0_real64, 2, .false., tols(m))
                end do
                call one_case(sf, px, py, 0.0_real64, 0.0_real64, 0.0_real64, 0, .true., sAIMTOL)
                ! the MULTCLAP / MULTCOBS entries, as the CACOCH loop feeds them
                do m = 1, cc_ap%multi_clap_n
                    call one_case(sf, px, py, cc_ap%multi_clap(1, m), cc_ap%multi_clap(2, m), &
                                  cc_ap%multi_clap(3, m), 1, .false., sAIMTOL)
                end do
                do m = 1, cc_ap%multi_cobs_n
                    call one_case(sf, px, py, cc_ap%multi_cobs(1, m), cc_ap%multi_cobs(2, m), &
                                  cc_ap%multi_cobs(3, m), 2, .false., sAIMTOL)
                end do
            end do
        end subroutine run_cachek_surface

        ! One CACHEK call and one check_apertures call from identical sentinel
        ! state; compare everything either leaves behind.
        subroutine one_case(sf, px, py, j1, j2, j3, cac, nocop, tol)
            integer, intent(in) :: sf, cac
            real(real64), intent(in) :: px, py, j1, j2, j3, tol
            logical, intent(in) :: nocop
            real(real64) :: jk1, jk2, jk3
            integer :: c_code, c_fs, c_stopp, c_caeras, c_coeras, c_s1, c_s2
            real(real64) :: c_ls
            logical :: bad

            jk1 = j1; jk2 = j2; jk3 = j3
            ! legacy
            R_X = px; R_Y = py; R_Z = 0.0_real64; R_I = sf
            STOPP = -7; l_caeras = -9; l_coeras = -9; l_ls = -99.0_real64
            l_spd1 = -5; l_spd2 = -5; RAYCOD = -3
            MSG = .false.; AIMTOL = tol; l_nocobs = nocop
            call CACHEK(jk1, jk2, jk3, cac)
            ! port, from the same sentinels
            c_stopp = -7; c_caeras = -9; c_coeras = -9; c_ls = -99.0_real64
            c_s1 = -5; c_s2 = -5
            call check_apertures(cc_ap, px, py, jk1, jk2, jk3, cac, tol, nocop, &
                                 c_code, c_fs, c_stopp, c_ls, c_caeras, c_coeras, c_s1, c_s2)
            cc_cases = cc_cases + 1
            bad = RAYCOD(1) /= c_code .or. RAYCOD(2) /= c_fs .or. STOPP /= c_stopp .or. &
                  l_caeras /= c_caeras .or. l_coeras /= c_coeras .or. &
                  l_spd1 /= c_s1 .or. l_spd2 /= c_s2 .or. &
                  transfer(l_ls, 0_int64) /= transfer(c_ls, 0_int64)
            if (bad) cc_bad = cc_bad + 1
            if (RAYCOD(1) == 6) cc_blk6 = cc_blk6 + 1
            if (RAYCOD(1) == 7) cc_blk7 = cc_blk7 + 1
            AIMTOL = sAIMTOL
        end subroutine one_case

        ! Synthetic aperture sets written into the legacy store of surface sf,
        ! for shapes no script can reach.  Runs the comparison for each set and
        ! restores the surface.
        !   set 1: irregular polygons (type 6, loaded from IPOLYnn.DAT files in
        !          real use) for clap, clap erase, cobs and cobs erase
        !   set 2: a polygon CLAP ERASE (type 5).  "CLAP POLYE" stores its data and
        !          then always rejects it (legacy test: NW2 must be 0 for type 5,
        !          the message says 3..200), so the command can never leave a
        !          usable polygon erase behind.
        subroutine run_synthetic_ipoly(sf)
            integer, intent(in) :: sf
            type(surface_apertures) :: keep
            integer :: j, k, iset
            real(real64) :: cx
            ! vertex radius per slot: clap, clap erase, cobs, cobs erase (the erase
            ! polygons are smaller than the cobs, bigger than the clap, so both
            ! "erased" and "not erased" points occur)
            real(real64), parameter :: polyscale(4) = [1.0_real64, 1.5_real64, 2.0_real64, 0.9_real64]

            keep = apertures_of(sf)
            do iset = 1, 2
                call restore_surface(sf, keep)
                if (iset == 1) then
                    ! polygon k: a skewed pentagon, scaled per slot
                    do k = 1, 4
                        do j = 1, 5
                            cx = polyscale(k)
                            IPOLYX(j, sf, k) = cx*cos(1.2566370614359172_real64*real(j - 1, real64) + 0.3_real64*real(k, real64))
                            IPOLYY(j, sf, k) = 0.8_real64*cx*sin(1.2566370614359172_real64*real(j - 1, real64) + 0.3_real64*real(k, real64))
                        end do
                        IPOLYX(6:200, sf, k) = 0.0_real64
                        IPOLYY(6:200, sf, k) = 0.0_real64
                    end do
                    call set_surf_clap_type(sf, 6)
                    call set_surf_clap_dim(sf, 1, 1.0_real64)
                    call set_surf_clap_dim(sf, 2, 5.0_real64)
                    call set_surf_clap_dim(sf, 3, 0.1_real64)
                    call set_surf_clap_dim(sf, 4, -0.05_real64)
                    call set_surf_clap_dim(sf, 5, 0.0_real64)
                    call set_surf_clap_tilt(sf, 4.0_real64)
                    call set_surf_cobs_ape_type(sf, 6)
                    call set_surf_cobs_ape_data(sf, 1, 1.0_real64)
                    call set_surf_cobs_ape_data(sf, 2, 5.0_real64)
                    call set_surf_cobs_ape_data(sf, 3, 0.05_real64)
                    call set_surf_cobs_ape_data(sf, 4, 0.02_real64)
                    call set_surf_cobs_ape_data(sf, 5, 0.0_real64)
                    call set_surf_cobs_ape_data(sf, 6, 0.1_real64)
                    call set_surf_coat_type(sf, 6)
                    call set_surf_cobs_poly(sf, 1, 1.0_real64)
                    call set_surf_cobs_poly(sf, 2, 5.0_real64)
                    call set_surf_cobs_poly(sf, 3, -0.1_real64)
                    call set_surf_cobs_poly(sf, 4, 0.1_real64)
                    call set_surf_cobs_poly(sf, 5, 0.0_real64)
                    call set_surf_cobs_poly(sf, 6, 3.0_real64)
                    call set_surf_cobs_era_type(sf, 6)
                    call set_surf_cobs_era_data(sf, 1, 1.0_real64)
                    call set_surf_cobs_era_data(sf, 2, 5.0_real64)
                    call set_surf_cobs_era_data(sf, 3, 0.0_real64)
                    call set_surf_cobs_era_data(sf, 4, 0.1_real64)
                    call set_surf_cobs_era_data(sf, 5, 0.0_real64)
                    call set_surf_cobs_era_data(sf, 6, 0.2_real64)
                else
                    ! small circular clap whose polygon erase region reaches past it
                    call set_surf_clap_type(sf, 1)
                    call set_surf_clap_dim(sf, 1, 1.0_real64)
                    call set_surf_clap_dim(sf, 2, 1.0_real64)
                    call set_surf_clap_dim(sf, 3, 0.0_real64)
                    call set_surf_clap_dim(sf, 4, 0.0_real64)
                    call set_surf_clap_dim(sf, 5, 0.0_real64)
                    call set_surf_clap_tilt(sf, 0.0_real64)
                    call set_surf_cobs_ape_type(sf, 5)
                    call set_surf_cobs_ape_data(sf, 1, 1.4_real64)
                    call set_surf_cobs_ape_data(sf, 2, 6.0_real64)
                    call set_surf_cobs_ape_data(sf, 3, 0.1_real64)
                    call set_surf_cobs_ape_data(sf, 4, -0.1_real64)
                    call set_surf_cobs_ape_data(sf, 5, 0.0_real64)
                    call set_surf_cobs_ape_data(sf, 6, 0.3_real64)
                end if
                k = cc_cases
                call run_cachek_surface(sf)
                cc_ipoly = cc_ipoly + cc_cases - k
            end do
            call restore_surface(sf, keep)
        end subroutine run_synthetic_ipoly

        ! Write a saved surface_apertures back into the legacy store of surface sf.
        subroutine restore_surface(sf, keep)
            integer, intent(in) :: sf
            type(surface_apertures), intent(in) :: keep
            integer :: j
            call set_surf_clap_type(sf, keep%clap_type)
            do j = 1, 5
                call set_surf_clap_dim(sf, j, keep%clap_dim(j))
            end do
            call set_surf_clap_tilt(sf, keep%clap_tilt)
            call set_surf_coat_type(sf, keep%cobs_type)
            call set_surf_cobs_ape_type(sf, keep%clap_erase_type)
            call set_surf_cobs_era_type(sf, keep%cobs_erase_type)
            do j = 1, 6
                call set_surf_cobs_poly(sf, j, keep%cobs_dim(j))
                call set_surf_cobs_ape_data(sf, j, keep%clap_erase_dim(j))
                call set_surf_cobs_era_data(sf, j, keep%cobs_erase_dim(j))
            end do
            IPOLYX(1:200, sf, 1:4) = keep%ipoly_x
            IPOLYY(1:200, sf, 1:4) = keep%ipoly_y
        end subroutine restore_surface

        ! ----------------------------------------------------------------
        ! HITSUR (legacy) against hit_and_interact (port), surface by surface.
        ! For every surface NEWOBJ+1..NEWIMG that the port supports, a fixed set
        ! of incoming rays (given in that surface's frame, as TRNSF2 leaves them)
        ! is run through both from identical sentinel state, for several
        ! wavelength slots, several RV/RVSTART/REVSTR combinations, both signs of
        ! the starting z, and (on surface NEWOBJ+1) two aim points.  Every value
        ! either side leaves behind must agree bit for bit.  Unsupported surfaces
        ! are counted as skipped (the port must also decline them untouched).
        ! ----------------------------------------------------------------
        subroutine run_hitsur()
            use DATLEN, only: PHASE, DUM, INTERS, SEC, OLDL, OLDM, OLDN, LN, MN, NN, &
                              COSI, COSIP, RV, RVSTART, REVSTR, WVN, SURTOL, &
                              R_XAIM, R_YAIM, R_ZAIM, R_L0, R_M0, R_N0, HOE_DO_IT
            use mod_ray_trace_builder, only: build_trace_context
            use mod_ray_trace_engine, only: trace_context, trace_surface
            use mod_surface_interaction, only: hit_state, hit_and_interact, hit_supported, &
                                               surface_optics, HIT_UNSUPPORTED, GLASS_PERFECT, &
                                               GLASS_IDEAL
            integer, parameter :: NRH = 26
            logical :: l_tir
            common /RIT/ l_tir
            type(trace_context) :: ctx
            type(hit_state) :: st0, stl, stp, sv
            real(real64) :: hx(NRH), hy(NRH), hl(NRH), hm(NRH), hs(NRH)
            real(real64) :: aimx(2), aimy(2), aimz(2), z0, wv, hn
            logical :: svDUM(0:499), svMSG, wl_ok(3), c_revs(4), c_rv0(4), c_rvs(4), sv_revstr
            real(real64) :: svXAIM, svYAIM, svZAIM, svWVN
            integer :: i, iw, ist, iz, ir, ia, naim, status, igate, gsurf
            logical :: ok
            type(surface_optics) :: go
            type(trace_surface) :: nosurf
            real(real64) :: gwv

            hs_cases = 0; hs_skip = 0; hs_bad = 0; hs_c4 = 0; hs_c20 = 0
            hs_dum = 0; hs_refl = 0; hs_rv = 0; hs_surfs = 0; hs_gates = 0

            ! x, y, l, m and the sign of n (+1 forward, -1 backward) of the rays
            hx = [0.0d0, 0.1d0, 0.0d0, 2.0d0, -3.0d0, 1.0d0, 5.0d0, 0.0d0, 8.0d0, 0.0d0, 0.0d0, &
                  0.0d0, 0.0d0, 1.0d0, -2.0d0, 0.0d0, 0.0d0, 0.5d0, 3.0d0, 0.0d0, 0.0d0, 2.0d0, &
                  -1.0d0, 0.0d0, 0.001d0, 12.0d0]
            hy = [0.0d0, 0.0d0, 0.1d0, 1.0d0, 2.0d0, -4.0d0, 0.0d0, 5.0d0, -6.0d0, 0.0d0, 0.0d0, &
                  0.0d0, 0.0d0, 1.0d0, 0.0d0, 3.0d0, 0.0d0, 0.5d0, -3.0d0, 0.0d0, 0.0d0, 2.0d0, &
                  4.0d0, 0.0d0, 0.001d0, 12.0d0]
            hl = [0.0d0, 0.0d0, 0.01d0, -0.1d0, 0.1d0, 0.2d0, 0.0d0, 0.0d0, -0.3d0, 0.5d0, 0.0d0, &
                  0.7d0, 0.0d0, 0.7d0, -0.85d0, 0.3d0, 0.6d0, 0.2d0, 0.3d0, 0.95d0, -0.95d0, 0.99d0, &
                  0.1d0, 0.0d0, 1.0d-4, 0.05d0]
            hm = [0.0d0, 0.01d0, 0.0d0, 0.05d0, -0.2d0, 0.1d0, 0.0d0, 0.0d0, 0.3d0, 0.0d0, 0.5d0, &
                  0.0d0, 0.8d0, 0.5d0, 0.2d0, -0.9d0, 0.6d0, 0.1d0, 0.1d0, 0.1d0, -0.1d0, 0.0d0, &
                  0.97d0, -0.9d0, 1.0d-4, 0.05d0]
            hs = 1.0d0
            hs(18) = -1.0d0; hs(19) = -1.0d0
            aimx = [0.4d0, -1.5d0]; aimy = [-0.3d0, 2.0d0]; aimz = [0.05d0, 0.12d0]
            ! REVSTR, RV, RVSTART combinations
            c_revs = [.false., .true., .false., .true.]
            c_rv0 = [.false., .true., .true., .false.]
            c_rvs = [.false., .true., .false., .true.]

            call build_trace_context(ctx)
            if (.not. allocated(ctx%surf)) then
                hs_skip = NEWIMG - NEWOBJ
                return
            end if

            ! save every legacy global we touch
            call get_globals(sv)
            svDUM = DUM; svMSG = MSG; svXAIM = R_XAIM; svYAIM = R_YAIM; svZAIM = R_ZAIM
            svWVN = WVN; sv_revstr = REVSTR
            MSG = .false.

            ! wavelength slots whose index is defined (nonzero) on every surface
            do iw = 1, 3
                wl_ok(iw) = .true.
                do i = NEWOBJ, NEWIMG
                    if (ctx%surf(i)%optics%index(iw) == 0.0_real64) wl_ok(iw) = .false.
                end do
            end do

            do i = NEWOBJ + 1, NEWIMG
                if (.not. hit_supported(ctx%surf(i)%geom, ctx%surf(i)%optics, 1.0_real64)) then
                    ! out of scope: the port must decline without touching anything
                    stp = sv
                    stp%x = 1.5_real64
                    call hit_and_interact(ctx%surf(i)%geom, ctx%surf(i)%optics, ctx%surf(i-1)%optics, &
                                          i, NEWOBJ, NEWIMG, 1.0_real64, SURTOL, .false., &
                                          0.0_real64, 0.0_real64, 0.0_real64, stp, status)
                    ok = status == HIT_UNSUPPORTED .and. stp%x == 1.5_real64
                    hs_skip = hs_skip + 1
                    if (.not. ok) hs_bad = hs_bad + 1
                    cycle
                end if
                hs_surfs = hs_surfs + 1
                naim = 1
                if (i == NEWOBJ + 1) naim = 2
                do iw = 1, 3
                    if (.not. wl_ok(iw)) cycle
                    wv = real(iw, real64)
                    do ist = 1, 4
                        do iz = 1, 2
                            do ir = 1, NRH
                                do ia = 1, naim
                                    hn = hs(ir)*sqrt(1.0_real64 - hl(ir)**2 - hm(ir)**2)
                                    z0 = -2.0_real64
                                    if (iz == 2) z0 = 2.0_real64
                                    if (hs(ir) < 0.0_real64) z0 = -z0
                                    hs_cases = hs_cases + 1

                                    ! incoming state with sentinels in everything the
                                    ! legacy routines might leave alone
                                    st0 = hit_state()
                                    st0%x = hx(ir); st0%y = hy(ir); st0%z = z0
                                    st0%l = hl(ir); st0%m = hm(ir); st0%n = hn
                                    st0%ln = -31.0_real64; st0%mn = -32.0_real64; st0%nn = -33.0_real64
                                    st0%cosi = -41.0_real64; st0%cosip = -42.0_real64
                                    st0%l0 = -51.0_real64; st0%m0 = -52.0_real64; st0%n0 = -53.0_real64
                                    st0%oldl = -61.0_real64; st0%oldm = -62.0_real64; st0%oldn = -63.0_real64
                                    st0%phase = -71.0_real64
                                    st0%rv = c_rv0(ist); st0%rvstart = c_rvs(ist)
                                    st0%tir = .true.
                                    st0%dum = (mod(hs_cases, 2) == 0)
                                    st0%inters = 77; st0%sec = 78; st0%stopp = -7
                                    st0%raycod = -3
                                    st0%spdcd1 = -5; st0%spdcd2 = -5
                                    st0%hoe_do_it = -9

                                    ! legacy
                                    R_I = i
                                    call put_globals(st0)
                                    WVN = wv; REVSTR = c_revs(ist)
                                    R_XAIM = aimx(ia); R_YAIM = aimy(ia); R_ZAIM = aimz(ia)
                                    call HITSUR
                                    call get_globals(stl)

                                    ! port, from the same state
                                    stp = st0
                                    call hit_and_interact(ctx%surf(i)%geom, ctx%surf(i)%optics, &
                                                          ctx%surf(i-1)%optics, i, NEWOBJ, NEWIMG, wv, &
                                                          SURTOL, c_revs(ist), aimx(ia), aimy(ia), &
                                                          aimz(ia), stp, status)
                                    if (status /= 0 .or. .not. same_state(stl, stp)) hs_bad = hs_bad + 1
                                    if (stl%raycod(1) == 4) hs_c4 = hs_c4 + 1
                                    if (stl%raycod(1) == 20) hs_c20 = hs_c20 + 1
                                    if (stl%raycod(1) == 0 .and. stl%dum) hs_dum = hs_dum + 1
                                    if (ctx%surf(i)%optics%index(iw)*ctx%surf(i-1)%optics%index(iw) < 0.0_real64) &
                                        hs_refl = hs_refl + 1
                                    if (stl%raycod(1) == 0 .and. stl%rv) hs_rv = hs_rv + 1
                                end do
                            end do
                        end do
                    end do
                end do
            end do

            ! Synthetic gate pass: take the first supported surface and break one
            ! condition at a time on a copy; the port must decline each one and
            ! leave its state alone.
            gsurf = -1
            do i = NEWOBJ + 1, NEWIMG
                if (hit_supported(ctx%surf(i)%geom, ctx%surf(i)%optics, 1.0_real64)) then
                    gsurf = i
                    exit
                end if
            end do
            if (gsurf > 0) then
                do igate = 1, 12
                    go = ctx%surf(gsurf)%optics
                    gwv = 1.0_real64
                    select case (igate)
                    case (1);  go%special_type = 12
                    case (2);  go%array_parity = 1
                    case (3);  go%paraxial = 1
                    case (4);  go%diffraction_flag = 1
                    case (5);  go%glass_class = GLASS_PERFECT
                    case (6);  go%glass_class = GLASS_IDEAL
                    case (7);  go%ray_error = 0.5_real64
                    case (8);  go%clap_dim4 = 13.0_real64
                    case (9);  go%typed_valid = .false.
                    case (10); gwv = 0.0_real64
                    case (11); gwv = 11.0_real64
                    case (12); continue
                    end select
                    stp = sv
                    stp%x = 1.5_real64
                    if (igate == 12) then
                        call hit_and_interact(nosurf%geom, go, ctx%surf(gsurf-1)%optics, gsurf, NEWOBJ, &
                                              NEWIMG, gwv, SURTOL, .false., 0.0_real64, 0.0_real64, &
                                              0.0_real64, stp, status)
                    else
                        call hit_and_interact(ctx%surf(gsurf)%geom, go, ctx%surf(gsurf-1)%optics, gsurf, &
                                              NEWOBJ, NEWIMG, gwv, SURTOL, .false., 0.0_real64, &
                                              0.0_real64, 0.0_real64, stp, status)
                    end if
                    hs_gates = hs_gates + 1
                    if (status /= HIT_UNSUPPORTED .or. stp%x /= 1.5_real64) hs_bad = hs_bad + 1
                end do
            end if

            ! restore
            call put_globals(sv)
            DUM = svDUM; MSG = svMSG; R_XAIM = svXAIM; R_YAIM = svYAIM; R_ZAIM = svZAIM
            WVN = svWVN; REVSTR = sv_revstr
        end subroutine run_hitsur

        ! Load the legacy globals described by hit_state into st (DUM(R_I) from the
        ! current R_I).
        subroutine get_globals(st)
            use DATLEN, only: PHASE, DUM, INTERS, SEC, OLDL, OLDM, OLDN, LN, MN, NN, &
                              COSI, COSIP, RV, RVSTART, R_L0, R_M0, R_N0, HOE_DO_IT
            use mod_surface_interaction, only: hit_state
            type(hit_state), intent(out) :: st
            logical :: l_tir
            common /RIT/ l_tir
            st%x = R_X; st%y = R_Y; st%z = R_Z; st%l = R_L; st%m = R_M; st%n = R_N
            st%ln = LN; st%mn = MN; st%nn = NN; st%cosi = COSI; st%cosip = COSIP
            st%l0 = R_L0; st%m0 = R_M0; st%n0 = R_N0
            st%oldl = OLDL; st%oldm = OLDM; st%oldn = OLDN
            st%phase = PHASE; st%rv = RV; st%rvstart = RVSTART; st%tir = l_tir
            st%dum = DUM(R_I)
            st%inters = INTERS; st%sec = SEC; st%stopp = STOPP; st%raycod = RAYCOD
            st%spdcd1 = l_spd1; st%spdcd2 = l_spd2; st%hoe_do_it = HOE_DO_IT
        end subroutine get_globals

        ! Write st into the legacy globals (DUM(R_I) for the current R_I; callers
        ! set R_I first when it matters, so put_globals(st0) is followed by R_I = i
        ! and the DUM sentinel is stored below).
        subroutine put_globals(st)
            use DATLEN, only: PHASE, DUM, INTERS, SEC, OLDL, OLDM, OLDN, LN, MN, NN, &
                              COSI, COSIP, RV, RVSTART, R_L0, R_M0, R_N0, HOE_DO_IT
            use mod_surface_interaction, only: hit_state
            type(hit_state), intent(in) :: st
            logical :: l_tir
            common /RIT/ l_tir
            R_X = st%x; R_Y = st%y; R_Z = st%z; R_L = st%l; R_M = st%m; R_N = st%n
            LN = st%ln; MN = st%mn; NN = st%nn; COSI = st%cosi; COSIP = st%cosip
            R_L0 = st%l0; R_M0 = st%m0; R_N0 = st%n0
            OLDL = st%oldl; OLDM = st%oldm; OLDN = st%oldn
            PHASE = st%phase; RV = st%rv; RVSTART = st%rvstart; l_tir = st%tir
            DUM(R_I) = st%dum
            INTERS = st%inters; SEC = st%sec; STOPP = st%stopp; RAYCOD = st%raycod
            l_spd1 = st%spdcd1; l_spd2 = st%spdcd2; HOE_DO_IT = st%hoe_do_it
        end subroutine put_globals

        logical function same_state(a, b)
            use mod_surface_interaction, only: hit_state
            type(hit_state), intent(in) :: a, b
            same_state = same_r(a%x, b%x) .and. same_r(a%y, b%y) .and. same_r(a%z, b%z) .and. &
                         same_r(a%l, b%l) .and. same_r(a%m, b%m) .and. same_r(a%n, b%n) .and. &
                         same_r(a%ln, b%ln) .and. same_r(a%mn, b%mn) .and. same_r(a%nn, b%nn) .and. &
                         same_r(a%cosi, b%cosi) .and. same_r(a%cosip, b%cosip) .and. &
                         same_r(a%l0, b%l0) .and. same_r(a%m0, b%m0) .and. same_r(a%n0, b%n0) .and. &
                         same_r(a%oldl, b%oldl) .and. same_r(a%oldm, b%oldm) .and. same_r(a%oldn, b%oldn) .and. &
                         same_r(a%phase, b%phase) .and. &
                         (a%rv .eqv. b%rv) .and. (a%rvstart .eqv. b%rvstart) .and. &
                         (a%tir .eqv. b%tir) .and. (a%dum .eqv. b%dum) .and. &
                         a%inters == b%inters .and. a%sec == b%sec .and. a%stopp == b%stopp .and. &
                         all(a%raycod == b%raycod) .and. a%spdcd1 == b%spdcd1 .and. &
                         a%spdcd2 == b%spdcd2 .and. a%hoe_do_it == b%hoe_do_it
        end function same_state

        logical function same_r(p, q)
            real(real64), intent(in) :: p, q
            same_r = transfer(p, 0_int64) == transfer(q, 0_int64)
        end function same_r
    end procedure execENGINETEST

end submodule mod_codev_utils
