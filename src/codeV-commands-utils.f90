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
            call LogTermFOR("Existing SPO Loop")
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
            call LogTermFOR("Existing Zern Loop")
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

    module procedure flipSurfaces
        use global_widgets, only: sysConfig
        use command_utils, only : parseCommandIntoTokens, isInputNumber
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
        boolResult = .FALSE.
        if (iptStr == 'RDY' .or. iptStr == 'THI' .or. iptStr == 'GLA') then
            boolResult = .TRUE.
        end if
    end procedure isInputSurfaceParameter

    module procedure setPickup
        character(len=6) :: kParam
        logical :: scaleOffset

        scaleOffset = .TRUE.
        select case (param1)
        case('RDY')
            kParam = 'RD'
        case('THI')
            kParam = 'TH'
        case('GLA')
            kParam = 'GLASS'
            scaleOffset = .FALSE.
        end select

        if (scaleOffset) then
            call executeCodeVLensUpdateCommand("CHG "//trim(int2str(si))//";PIKUP "//trim(kParam)//","// &
            & trim(int2str(sj))//","//trim(real2str(scale))//","//trim(real2str(offset)))
        else
            call executeCodeVLensUpdateCommand("CHG "//trim(int2str(si))//";PIKUP "//trim(kParam)//","//trim(int2str(sj)))
        end if
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
        implicit none
        integer :: locStr, locDot, i
        character(len=1024) :: fileName
        character(len=1) :: fileSep

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
                    call process_zoa_file(trim(fileName))
                    call ldm%load_surfaces_from_alens()
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
                    call process_zoa_file(trim(fileName))
                    call ldm%load_surfaces_from_alens()
                end if
            end if
        end if
    end procedure processZoaFileInput

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

    module procedure execTERM
        use global_widgets, only: ioConfig
        use zoa_ui, only: ID_TERMINAL_DEFAULT
        use GLOBALS, only: HEADLESS_MODE
        if (HEADLESS_MODE) return
        call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
        call zoa_emit("Terminal output redirected to default", "black")
    end procedure execTERM

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

end submodule mod_codev_utils
