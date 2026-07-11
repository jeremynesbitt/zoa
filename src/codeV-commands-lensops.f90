submodule (codeV_commands) mod_codev_lensops
implicit none
contains

    module procedure execSTO
        use mod_lens_data_manager, only: ldm

        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        surfNum = -1
        call parse(trim(iptStr), ' ', tokens, numTokens)
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
        use strings, only: parse
        use zoa_file_handler, only: open_file_to_sav_lens, doesFileExist
        use zoa_ui_callbacks, only: query_confirm, query_yes_no, notify_close_all_tabs
        implicit none

        character(len=256) :: fName
        character(len=80) :: tokens(40)
        character(len=1024) :: fullPath
        integer :: numTokens, locDot
        logical :: confirmed, save_lens

        call parse(trim(iptStr), ' ', tokens, numTokens)

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
        use zoa_output, only: zoa_emit

        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, fID
        character(len=500) :: fileName
        character(len=500) :: cdir
        logical :: fileSelected
        integer :: locDot

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens >= 2) then
            ! Headless/direct path: filename given on command line
            fileName = trim(tokens(2))
            locDot = index(fileName, '.')
            if (locDot == 0) fileName = trim(fileName)//'.seq'
            fID = open_file_to_sav_lens(trim(fileName), dirName=trim(getCodeVDir()), overwriteFlag=.TRUE.)
            if (fID /= 0) then
                call sysConfig%genSaveOutputText(fID)
                call ldm%genSaveOutputText(fID)
                call optim%genSaveOutputText(fID)
                close(fID)
                call zoa_emit("Saved CODE V file "//trim(fileName), "black")
            else
                call LogTermFOR("Error!  fID is "//int2str(fID))
            end if
        else
            ! GUI dialog path: no filename given
            fileName = ''
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
                    call zoa_emit("Saved CODE V file "//trim(getFileNameFromPath(fileName)), "black")
                else
                    call LogTermFOR("Error!  fID is "//int2str(fID))
                end if
            end if
        end if
    end procedure exportLensToCodeV

    module procedure exportLensToZemax
        use zoa_file_handler, only: open_file_to_sav_lens, getCodeVDir
        use global_widgets, only: sysConfig
        use mod_lens_data_manager, only: ldm
        use DATLEN, only: GLANAM
        use kdp_data_types, only: FIELD_OBJECT_ANGLE_DEG, FIELD_OBJECT_HEIGHT
        use zoa_output, only: zoa_emit
        use iso_fortran_env, only: real64

        character(len=256) :: fName
        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID
        integer :: k, lastSurf, stopSurf, ii
        real(real64) :: curv, thi, conic
        character(len=13) :: glassName
        logical :: isMirror
        real(real64) :: enpd, fieldY, fieldX

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens < 2) then
            call zoa_emit("Usage: ZOA2ZMX <filename>", "red")
            return
        end if

        fName = trim(tokens(2))
        locDot = index(fName, '.')
        if (locDot == 0) fName = trim(fName)//'.zmx'

        fID = open_file_to_sav_lens(trim(fName), dirName=trim(getCodeVDir()), overwriteFlag=.TRUE.)
        if (fID == 0) then
            call zoa_emit("ZOA2ZMX: could not open file "//trim(fName), "red")
            return
        end if

        lastSurf = ldm%getLastSurf()
        stopSurf = ldm%getStopSurf()

        ! Header — NAME triggers PROCESKDP('LENS') in ZMX2PRG; no VERS needed
        write(fID, '(A)') 'NAME '//trim(sysConfig%lensTitle)
        write(fID, '(A)') 'UNIT MM'
        ! refApertureValue(2) holds the full diameter (saved as EPD);
        ! ZMX2PRG converts ENPD back via SAY = ENPD/2
        enpd = sysConfig%refApertureValue(2)
        write(fID, '(A,1X,G20.10)') 'ENPD', enpd

        ! Field type
        if (sysConfig%currFieldID == FIELD_OBJECT_ANGLE_DEG) then
            write(fID, '(A)') 'FTYP 0 0'
        else
            write(fID, '(A)') 'FTYP 1 0'
        end if

        ! Field values
        fieldX = sysConfig%refFieldValue(1)
        fieldY = sysConfig%refFieldValue(2)
        ! XFLN line
        write(fID, '(A)', advance='no') 'XFLN'
        do ii = 1, sysConfig%numFields
            write(fID, '(1X,G16.8)', advance='no') fieldX * sysConfig%relativeFields(1,ii)
        end do
        write(fID, '()')
        ! YFLN line
        write(fID, '(A)', advance='no') 'YFLN'
        do ii = 1, sysConfig%numFields
            write(fID, '(1X,G16.8)', advance='no') fieldY * sysConfig%relativeFields(2,ii)
        end do
        write(fID, '()')

        ! Wavelengths (in µm — that's already what wavelengths() stores)
        write(fID, '(A)', advance='no') 'WAVL'
        do ii = 1, sysConfig%numWavelengths
            write(fID, '(1X,G14.7)', advance='no') sysConfig%wavelengths(ii)
        end do
        write(fID, '()')

        write(fID, '(A,1X,I0)') 'PWAV', sysConfig%refWavelengthIndex

        ! Surfaces
        do k = 0, lastSurf
            write(fID, '(A,1X,I0)') 'SURF', k
            if (k == stopSurf) write(fID, '(A)') '  STOP'
            write(fID, '(A)') '  TYPE STANDARD'

            curv = ldm%getSurfCurv(k)
            write(fID, '(A,1X,G20.12,A)') '  CURV', curv, '   0  0  0'

            conic = ldm%getConicConstant(k)
            if (conic /= 0.0_real64) then
                write(fID, '(A,1X,G20.12)') '  CONI', conic
            end if

            ! Glass (only for non-last surface: glass goes on the surface before the next medium)
            if (ldm%isGlassSurf(k)) then
                glassName = GLANAM(k,2)
                ! Detect mirror: GLANAM(k,2) is REFL, REFLTIRO, or REFLTIR
                isMirror = (glassName(1:4) == 'REFL')
                if (isMirror) then
                    write(fID, '(A)') '  GLAS MIRROR'
                else
                    write(fID, '(A)') '  GLAS '//trim(glassName)
                end if
            end if

            thi = ldm%getSurfThi(k)
            if (thi > 1.0e10_real64) then
                write(fID, '(A)') '  DISZ INFINITY'
            else
                write(fID, '(A,1X,G20.12)') '  DISZ', thi
            end if
        end do

        close(fID)
        call zoa_emit("Saved Zemax file "//trim(fName), "black")
    end procedure exportLensToZemax

    module procedure execSAV
        use global_widgets, only: sysConfig, curr_lens_data
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
            ! Emit only the filename; absolute paths must stay out of captured
            ! test output (full path still goes to the terminal via print).
            print *, "File name to save is "//trim(getTempDirectory())//trim(fName)
            call zoa_emit("File name to save is "//trim(fName), "black")
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
        use strings, only: parse
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2) then
            call executeCodeVLensUpdateCommand('CW '//trim(tokens(2)))
        else
            call zoa_emit("No Wavelength Index Input.  Please try again", "red")
            return
        end if
    end procedure execSetWavelengthIndex

    module procedure execRMD
        use strings, only: parse
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)

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
        use strings, only: parse
        use global_widgets, only: curr_lens_data, curr_par_ray_trace
        use DATMAI
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        ! trim() copy so parse never touches the global INPUT buffer, even
        ! transiently (parse compacts its argument in place, then restores).
        call parse(trim(INPUT), ' ', tokens, numTokens)
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

    !## cmd:      DEL
    !## syntax:   DEL VIG | APE SA | PIM
    !## category: General
    !## desc:     Allows for deleting user defined parameters of the system, such as 
    !##           Vignetting, Apertures, and Solves 
    !##           
    !##    
    module procedure deleteStuff
        use command_utils, only: isInputNumber
        use global_widgets, only: curr_lens_data, sysConfig
        use mod_lens_data_manager, only: ldm
        use zoa_ui_callbacks, only: notify_replot
        use optim_types
        use solve_manager, only: solve_del_cmd, SLV_YZ_THI_SLOT, &
        &                        SLV_YZ_CURV_SLOT, SLV_XZ_CURV_SLOT
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
                ! DEL SOL <verb> Sk : delete the solve of that class on surface Sk.
                !   CUY -> CSDY (YZ curvature), CUX -> CSDX (XZ curvature),
                !   THI -> TSD (thickness).  Verb->delete-word via solve_manager.
                if (numTokens > 3 .and. isSurfCommand(trim(tokens(4)))) then
                    block
                        integer :: delSlot
                        select case(trim(tokens(3)))
                        case('CUY'); delSlot = SLV_YZ_CURV_SLOT
                        case('CUX'); delSlot = SLV_XZ_CURV_SLOT
                        case('THI'); delSlot = SLV_YZ_THI_SLOT
                        case default; delSlot = -1
                        end select
                        if (delSlot /= -1) then
                            call zoa_emit("Deleting Solve", "blue")
                            surfNum = getSurfNumFromSurfCommand(trim(tokens(4)))
                            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                            & '; '//trim(solve_del_cmd(delSlot))//' ;GO')
                        else
                            call zoa_emit("Unknown solve type.  Expect DEL SOL CUY|CUX|THI Sk", "red")
                        end if
                    end block
                else
                    call zoa_emit("No Angle Solve Specified.  Please try again", "red")
                end if
            case('CON')
                if (numTokens == 3 .AND. isInputNumber(tokens(3))) then
                    call deleteConstraint(str2int(tokens(3)))
                else
                    call zoa_emit("Improper format detected.  Expect DEL CON ID", "red")
                end if
            case('VIG')
                ! DEL VIG: clear all per-field vignetting factors.
                call sysConfig%resetVignetting()
                call notify_replot()
            case('APE')
                ! DEL APE SA: delete clear apertures AND edge apertures on all
                ! surfaces (SA = "surfaces, all").  Silent, like DEL VIG, so it can
                ! be used in reset macros (newlens.zoa) without polluting output.
                if (numTokens >= 3 .and. trim(tokens(3)) == 'SA') then
                    call ldm%deleteAllApertures()
                    call notify_replot()
                else
                    call zoa_emit("Expected 'DEL APE SA'", "red")
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
        use strings, only: parse
        use globals, only: HEADLESS_MODE
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)

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
        use undo_manager, only: undo_reset_baseline
        use mod_lens_data_manager, only: ldm
        implicit none
        character(len=1024) :: fullPath

        fullPath = trim(getTempDirectory())//getCurrentLensFileName()
        ! Emit only the filename; absolute paths must stay out of captured
        ! test output (full path still goes to the terminal via print).
        print *, "Restoring from: "//trim(fullPath)
        call zoa_emit("Restoring from: "//trim(getCurrentLensFileName()), "black")
        ! The lens is being replaced: same shared newlens.zoa reset + finalize
        ! as RES (processZoaFileInput), so session restore clears the same
        ! state the same way as every other lens-load path.
        call resetToNewLensTemplate()
        call process_zoa_file(trim(fullPath))
        call ldm%load_surfaces_from_alens()
        call undo_reset_baseline()
    end procedure execRESAUTO

end submodule mod_codev_lensops
