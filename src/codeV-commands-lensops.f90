submodule (codeV_commands) mod_codev_lensops
implicit none
contains

    module procedure execSTO
        use command_utils, only : parseCommandIntoTokens
        use mod_lens_data_manager, only: ldm

        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        surfNum = -1
        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens == 2) then
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            else
                call zoa_emit("STO Should have a surface identifier (S0, Sk, Si, SA)", "red")
                return
            end if
        else
            surfNum = ldm%getSurfacePointer()
        end if

        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; ASTOP; REFS')
    end procedure execSTO

    module procedure execRestore
        implicit none

        character(len=160) :: tokens(40)
        integer :: numTokens, locStr, locDot
        character(len=256) :: fileName

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            print *, "Tokens(2) is ", trim(tokens(2))
            call processZoaFileInput(trim(tokens(2)))
        else
            call zoa_emit("Error!  Expect two inputs, eg RES file or RES macro:file", "red")
        end if
    end procedure execRestore

    module procedure execRestore_old
        use zoa_file_handler
        use command_utils, only : parseCommandIntoTokens
        use zoa_file_handler, only: open_file_to_sav_lens, doesFileExist
        use zoa_ui_callbacks, only: query_confirm, query_yes_no, notify_close_all_tabs
        implicit none

        character(len=256) :: fName
        character(len=80) :: tokens(40)
        character(len=1024) :: fullPath
        integer :: numTokens, locDot
        logical :: confirmed, save_lens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        if (numTokens == 2) then
            fName = trim(tokens(2))
            locDot = index(fName, '.')
            if (locDot == 0) fName = trim(fName)//'.zoa'
            call LogTermFOR("File name to restore is "//trim(fName))
            fullPath = getRestoreFilePath(trim(fName))

            if (TEST_MODE .eqv. .FALSE. .and. doesFileExist(trim(fullPath))) then
                call query_confirm("You are about to start a new lens system. Are you sure?", "Warning", confirmed)
                if (confirmed) then
                    call query_yes_no("Do you want to save current lens before loading?", "Warning", save_lens)
                    if (save_lens) call PROCESKDP('LIB PUT')
                    call notify_close_all_tabs("Loading new lens")
                else
                    call zoa_emit("New Lens Process Cancelled", "black")
                end if
            end if

            if (len(trim(fullPath)) > 1) then
                call process_zoa_file(fullPath)
            else
                call zoa_emit("File Not found in known directory!  Please try again or use UI", "red")
            end if
        else
            call zoa_emit("No file given to restore!  Please try again", "red")
        end if
    end procedure execRestore_old

    module procedure exportLensToCodeV
        use zoa_file_handler
        use zoa_ui_callbacks, only: query_save_file
        use global_widgets, only: sysConfig, curr_lens_data
        use optim_types, only: optim
        use mod_lens_data_manager

        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, fID
        character(len=500) :: fileName
        character(len=500) :: cdir
        logical :: fileSelected

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            fileName = trim(tokens(2))
        else
            fileName = ''
        end if

        call query_save_file(fileName, cdir, trim(getCodeVDir()), "*.seq", "CodeV File", fileSelected)

        if (fileSelected) then
            print *, "fileName is ", trim(getFileNameFromPath(fileName))
            print *, "fileDirectory is ", trim(cdir)

            fID = open_file_to_sav_lens(trim(getFileNameFromPath(fileName)), dirName=trim(cdir), overwriteFlag=.TRUE.)
            if (fID /= 0) then
                call sysConfig%genSaveOutputText(fID)
                call ldm%genSaveOutputText(fID)
                call optim%genSaveOutputText(fID)
                close(fID)
            else
                call LogTermFOR("Error!  fID is "//int2str(fID))
            end if
        end if
    end procedure exportLensToCodeV

    module procedure exportLensToZemax
        character(len=256) :: fName
        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID
    end procedure exportLensToZemax

    module procedure execSAV
        use global_widgets, only: sysConfig, curr_lens_data
        use command_utils, only : parseCommandIntoTokens
        use zoa_file_handler, only: open_file_to_sav_lens, getTempDirectory, getCurrentLensFileName
        use optim_types, only: optim
        use zoom_manager, only: zoom_genSaveOutputText
        use mod_lens_data_manager
        implicit none

        character(len=256) :: fName
        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID

        dirName = ''
        call parse(trim(iptStr), ' ', tokens, numTokens)

        call LogTermFOR("Number of tokens is "//int2str(numTokens))
        select case(numTokens)
        case (1)
            fName = getCurrentLensFileName()
            call zoa_emit("File name to save is "//trim(getTempDirectory())//trim(fName), "black")
            fID = open_file_to_sav_lens(fName, dirName=getTempDirectory(), overwriteFlag=.TRUE.)
        case (2)
            fName = trim(tokens(2))
            locDot = index(fName, '.')
            if (locDot == 0) fName = trim(fName)//'.zoa'
            call zoa_emit("File name to save is "//trim(fName), "black")
            fID = open_file_to_sav_lens(fName)
        end select

        if (fID /= 0) then
            call sysConfig%genSaveOutputText(fID)
            call ldm%genSaveOutputText(fID)
            call optim%genSaveOutputText(fID)
            call zoom_genSaveOutputText(fID)
            close(fID)
        else
            call LogTermFOR("Error!  fiD is "//int2str(fID))
        end if
    end procedure execSAV

    module procedure execSaveSessionToFile
        use zoa_ui_callbacks, only: notify_write_tab_state
        use global_widgets, only: sysConfig, curr_lens_data
        use command_utils, only : parseCommandIntoTokens
        use zoa_file_handler, only: open_file_to_sav_lens
        use mod_lens_data_manager, only: ldm
        implicit none

        character(len=256) :: fName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 1) then
            fName = trim(tokens(2))
            locDot = index(fName, '.')
            if (locDot == 0) fName = trim(fName)//'.zoaenv'
            call LogTermFOR("File name to save is "//trim(fName))
        else
            fName = 'default.zoaenv'
        end if
        fID = open_file_to_sav_lens(fName)
        if (fID /= 0) then
            call sysConfig%genSaveOutputText(fID)
            call ldm%genSaveOutputText(fID)
            call notify_write_tab_state(fID)
            close(fID)
        else
            call LogTermFOR("Error!  fID is "//int2str(fID))
        end if
    end procedure execSaveSessionToFile

    module procedure execSetWavelengthIndex
        use command_utils, only : parseCommandIntoTokens
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens == 2) then
            call executeCodeVLensUpdateCommand('CW '//trim(tokens(2)))
        else
            call zoa_emit("No Wavelength Index Input.  Please try again", "red")
            return
        end if
    end procedure execSetWavelengthIndex

    module procedure execRMD
        use command_utils, only : parseCommandIntoTokens
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(iptStr, tokens, numTokens, ' ')

        if (isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (numTokens > 2) then
                select case(trim(tokens(3)))
                case ('REFL')
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; REFL')
                case ('REFR')
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; AIR')
                case ('TIR')
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; REFLTIRO')
                end select
            else
                call zoa_emit("No Surface Modifier Selected.  Please try again", "red")
            end if
        else
            call zoa_emit("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if
    end procedure execRMD

    module procedure execSetCodeVCmd
        use command_utils, only : parseCommandIntoTokens
        use global_widgets, only: curr_lens_data, curr_par_ray_trace
        use DATMAI
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')
        if (numTokens > 1) then
            select case(trim(tokens(2)))
            case('MAG')
                if (numTokens > 2) then
                    call executeCodeVLensUpdateCommand('CHG 0;TH '// &
                    real2str(curr_par_ray_trace%getObjectThicknessToSetParaxialMag(str2real8(trim(tokens(3))), curr_lens_data)))
                else
                    call zoa_emit("No Mag Value specified.  Please try again", "red")
                end if
            end select
        end if
    end procedure execSetCodeVCmd

    module procedure setMagSolve
        use command_utils, only: isInputNumber
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                call executeCodeVLensUpdateCommand('CHG 0; REDSLV '//trim(tokens(2)), exitLensUpdate=.TRUE.)
            else
                call zoa_emit("Error!  Unable to parse value "//trim(tokens(2))//" into number", "red")
            end if
        else
            call zoa_emit("Error!  Expecting RED X where X is the desired reduction factor", "red")
        end if
    end procedure setMagSolve

    module procedure deleteStuff
        use command_utils, only: isInputNumber
        use global_widgets, only: curr_lens_data
        use optim_types
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens > 1) then
            select case(trim(tokens(2)))
            case('PIM')
                call zoa_emit("Deleting PIM", "blue")
                surfNum = curr_lens_data%num_surfaces - 2
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; TSD ;GO')
            case('SOL')
                if (numTokens > 2) then
                    call zoa_emit("Deleting Solve", "blue")
                    select case(trim(tokens(3)))
                    case('CUY')
                        if (isSurfCommand(trim(tokens(4)))) then
                            surfNum = getSurfNumFromSurfCommand(trim(tokens(4)))
                            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; CSDY ;GO')
                        end if
                    end select
                else
                    call zoa_emit("No Angle Solve Specified.  Please try again", "red")
                end if
            case('CON')
                if (numTokens == 3 .AND. isInputNumber(tokens(3))) then
                    call deleteConstraint(str2int(tokens(3)))
                else
                    call zoa_emit("Improper format detected.  Expect DEL CON ID", "red")
                end if
            end select
        end if
    end procedure deleteStuff

    module procedure setEPD
        use command_utils
        implicit none

        if (checkCommandInput([ID_CMD_NUM], max_num_terms=1)) then
            call executeCodeVLensUpdateCommand('SAY '//real2str(getInputNumber(1)/2.0))
        end if
    end procedure setEPD

    module procedure setParaxialImageSolve
        use global_widgets, only: curr_lens_data

        integer :: surfNum

        surfNum = curr_lens_data%num_surfaces - 2
        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))//'; PY, 0; GO')
    end procedure setParaxialImageSolve

    module function getSetGlassText(strInput) result(strOut)
        use command_utils, only : isInputNumber
        use glass_manager, only: parseModelGlassEntry, gdb
        use zoa_output, only: zoa_emit

        character(len=*) :: strInput
        character(len=1024) :: strOut
        real(kind=real64) :: nd, vd
        integer :: colonLoc, underscoreLoc, catalogIdx
        character(len=20) :: modelLabel
        character(len=40) :: glassName, catalogName, altGlassName

        colonLoc     = index(strInput, ':')
        underscoreLoc = index(strInput, '_')

        if (colonLoc > 0) then
            ! n:v format: e.g. "1.415:47.0" -> nd=1.415, vd=47.0
            read(strInput(1:colonLoc-1), *) nd
            read(strInput(colonLoc+1:len_trim(strInput)), *) vd
            ! Build xyz.abc label to match existing model glass convention
            write(modelLabel, '(I0,".",I0)') nint((nd - 1.0_real64)*1000), nint(vd*10)
            strOut = 'MODEL D'//trim(modelLabel)//','//real2str(nd)//','//real2str(vd)
        else if (isInputNumber(strInput)) then
            call LogTermFOR("Model Glass Entered! "//strInput)
            call parseModelGlassEntry(strInput, nd, vd)
            strOut = 'MODEL D'//strInput//','//real2str(nd)//','//real2str(vd)
            call LogTermFOR("STROUT is "//trim(strOut))
        else if (underscoreLoc > 1) then
            ! GLASSNAME_CATALOG format (e.g. NSK16_SCHOTT -> SCHOTT N-SK16)
            glassName   = strInput(1:underscoreLoc-1)
            catalogName = strInput(underscoreLoc+1:len_trim(strInput))
            if (gdb%isNameInCatalog(trim(catalogName), catalogIdx)) then
                if (gdb%isGlassInCatalog(trim(glassName), catalogIdx)) then
                    strOut = trim(catalogName)//' '//trim(glassName)
                else if (glassName(1:1) == 'N' .and. &
                         gdb%isGlassInCatalog('N-'//trim(glassName(2:)), catalogIdx)) then
                    ! e.g. NSK16 -> N-SK16 within the named catalog
                    strOut = trim(catalogName)//' N-'//trim(glassName(2:))
                else if (gdb%isGlassInAnyCatalog(trim(glassName))) then
                    ! glass not in named catalog but exists elsewhere
                    strOut = 'GLAK '//trim(glassName)
                else if (glassName(1:1) == 'N' .and. &
                         gdb%isGlassInAnyCatalog('N-'//trim(glassName(2:)))) then
                    ! e.g. NSK16_SCHOTT -> N-SK16 found in another catalog (e.g. SCH2000)
                    altGlassName = 'N-'//trim(glassName(2:))
                    strOut = 'GLAK '//trim(altGlassName)
                else
                    call zoa_emit(trim(strInput)//' not found in any catalog', 'red')
                    strOut = 'GLAK '//trim(strInput)
                end if
            else
                call zoa_emit(trim(strInput)//' not found in any catalog', 'red')
                strOut = 'GLAK '//trim(strInput)
            end if
        else
            ! Plain glass name: if starts with N, also try N-xxx variant
            if (strInput(1:1) == 'N' .and. &
                .not. gdb%isGlassInAnyCatalog(trim(strInput)) .and. &
                gdb%isGlassInAnyCatalog('N-'//trim(strInput(2:)))) then
                altGlassName = 'N-'//trim(strInput(2:))
                strOut = 'GLAK '//trim(altGlassName)
            else if (.not. gdb%isGlassInAnyCatalog(trim(strInput))) then
                call zoa_emit(trim(strInput)//' not found in any catalog', 'red')
                strOut = 'GLAK '//trim(strInput)
            else
                strOut = 'GLAK '//trim(strInput)
            end if
        end if
    end function getSetGlassText

    module procedure execEDI
        use command_utils, only : parseCommandIntoTokens
        use globals, only: HEADLESS_MODE
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        if (numTokens < 2) then
            call zoa_emit("EDI requires a subcommand (e.g. EDI PREF)", "red")
            return
        end if

        select case (trim(tokens(2)))
        case ('PREF')
            if (HEADLESS_MODE) then
                call zoa_emit("EDI PREF requires GUI", "red")
            else
                block
                    use ui_preferences
                    use global_widgets, only: preferences_window, my_window
                    use iso_c_binding, only: c_associated
                    if (.not. c_associated(preferences_window)) then
                        call preferences_new(my_window)
                    end if
                end block
            end if
        case default
            call zoa_emit("Unknown EDI subcommand: "//trim(tokens(2)), "red")
        end select
    end procedure execEDI

    module procedure execRESAUTO
        use zoa_file_handler, only: getTempDirectory, getCurrentLensFileName, process_zoa_file
        implicit none
        character(len=1024) :: fullPath

        fullPath = trim(getTempDirectory())//getCurrentLensFileName()
        call zoa_emit("Restoring from: "//trim(fullPath), "black")
        call process_zoa_file(trim(fullPath))
    end procedure execRESAUTO

end submodule mod_codev_lensops
