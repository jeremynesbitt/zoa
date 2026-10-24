submodule (codeV_commands) mod_codev_lensops
use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                       kdp_lens_end, kdp_chg, kdp_lens_cmd
implicit none
contains

    !## cmd:      STO
    !## syntax:   STO Sk
    !## category: Surface Parameters
    !## desc:     Make surface Sk the aperture stop.
    !##
    !## cmd:      STOP
    !## syntax:   STOP Sk
    !## category: Surface Parameters
    !## desc:     Make surface Sk the aperture stop (alias of STO).
    !##
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

        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_chg(surfNum)
        call kdp_lens_cmd('ASTOP')
        call kdp_lens_cmd('REFS')
        call kdp_lens_end()
        call kdp_silent_end()
    end procedure execSTO

    !## cmd:      RES
    !## syntax:   RES file
    !## category: File I/O
    !## desc:     Restore (load) a lens from a .zoa file (RES macro:file loads from a macro).
    !## desc:     A companion .zin plot file with the same name is loaded too, if present.
    !##
    module procedure execRestore
        implicit none

        character(len=160) :: tokens(40)
        integer :: numTokens, locStr, locDot
        character(len=256) :: fileName

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens == 2) then
            call processZoaFileInput(trim(rawCommandWord(2, tokens(2))))
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

    !## cmd:      ZOA2CV
    !## syntax:   ZOA2CV file
    !## category: File I/O
    !## desc:     Export the current lens to a CODE V sequence (.seq) file.
    !##
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
                call sysConfig%genSaveOutputText(fID, forCodeV=.true.)
                call ldm%genSaveOutputText(fID)
                call sysConfig%genPostLensSaveText(fID, forCodeV=.true.)
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
                    call sysConfig%genSaveOutputText(fID, forCodeV=.true.)
                    call ldm%genSaveOutputText(fID)
                    call sysConfig%genPostLensSaveText(fID, forCodeV=.true.)
                    call optim%genSaveOutputText(fID)
                    close(fID)
                    call zoa_emit("Saved CODE V file "//trim(getFileNameFromPath(fileName)), "black")
                else
                    call LogTermFOR("Error!  fID is "//int2str(fID))
                end if
            end if
        end if
    end procedure exportLensToCodeV

    !## cmd:      ZOA2ZMX
    !## syntax:   ZOA2ZMX file
    !## category: File I/O
    !## desc:     Export the current lens to a Zemax (.zmx) file.
    !##
    module procedure exportLensToZemax
        use zoa_file_handler, only: open_file_to_sav_lens, getCodeVDir
        use global_widgets, only: sysConfig
        use mod_lens_data_manager, only: ldm
        use DATLEN, only: GLANAM
        use mod_surface, only: surf_clap_type, surf_clap_dim
        use kdp_data_types, only: FIELD_OBJECT_ANGLE_DEG, FIELD_OBJECT_HEIGHT, APER_IMAGE_FNO, &
                                  APER_OBJECT_NA, APER_STOP_SURFACE
        use mod_system, only: sys_say
         use zoa_output, only: zoa_emit
         use type_utils, only: compactReal
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
        ! The aperture, as the Zemax type ZMX2PRG reads back: OBNA (object
        ! NA), FLOA (float by stop size), otherwise ENPD -- the entrance
        ! pupil diameter, 2*SAY (refApertureValue holds the NA or F/# for
        ! the other types; it used to be written as ENPD for all of them).
        select case (sysConfig%currApertureID)
        case (APER_OBJECT_NA)
            write(fID, '(A,1X,G20.10)') 'OBNA', sysConfig%refApertureValue(2)
        case (APER_STOP_SURFACE)
            write(fID, '(A)') 'FLOA'
        case default
            enpd = 2.0d0*sys_say()
            write(fID, '(A,1X,G20.10)') 'ENPD', enpd
        end select

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
        ! Field weights: FWGT (older OpticStudio) and FWGN (newer); Zemax
        ! files carry both.
        write(fID, '(A)', advance='no') 'FWGT'
        do ii = 1, sysConfig%numFields
            write(fID, '(1X,A)', advance='no') trim(compactReal(sysConfig%fieldWeights(ii)))
        end do
        write(fID, '()')
        write(fID, '(A)', advance='no') 'FWGN'
        do ii = 1, sysConfig%numFields
            write(fID, '(1X,A)', advance='no') trim(compactReal(sysConfig%fieldWeights(ii)))
        end do
        write(fID, '()')

        ! Wavelengths (in µm — that's already what wavelengths() stores)
        write(fID, '(A)', advance='no') 'WAVL'
        do ii = 1, sysConfig%numWavelengths
            write(fID, '(1X,G14.7)', advance='no') sysConfig%wavelengths(ii)
        end do
        write(fID, '()')
        ! Wavelength weights
        write(fID, '(A)', advance='no') 'WWGT'
        do ii = 1, sysConfig%numWavelengths
            write(fID, '(1X,A)', advance='no') trim(compactReal(sysConfig%spectralWeights(ii)))
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

            ! Circular clear aperture (needed by FLOA, the stop-size float),
            ! as ZMX2PRG reads it back: CLAP 0 <radius> 0.  Other shapes are
            ! not exported.
            if (surf_clap_type(k) == 1.0d0) then
                write(fID, '(A,1X,G20.12,A)') '  CLAP 0', surf_clap_dim(k, 1), ' 0'
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

    !## cmd:      SAV
    !## syntax:   SAV file
    !## category: File I/O
    !## desc:     Save the current lens to a .zoa file. A companion .zin
    !## desc:     plot file (same name, .zin extension) is written too when
    !## desc:     any plots are open.
    !##
    module procedure execSAV
        use zoa_file_handler, only: getTempDirectory, getCurrentLensFileName
        implicit none

        character(len=256) :: fName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot

        call parse(trim(iptStr), ' ', tokens, numTokens)

        call LogTermFOR("Number of tokens is "//int2str(numTokens))
        select case(numTokens)
        case (1)
            fName = getCurrentLensFileName()
            call saveLensSystem(trim(fName), dirName=getTempDirectory(), overwrite=.TRUE.)
        case (2)
            fName = trim(rawCommandWord(2, tokens(2)))
            locDot = index(fName, '.')
            if (locDot == 0) fName = trim(fName)//'.zoa'
            call saveLensSystem(trim(fName))
        end select
    end procedure execSAV

    !## cmd:      (internal) saveLensSystem
    !## desc:     Reusable lens-save entry point used by SAV and (future)
    !## desc:     timed autosave. Writes the .zoa lens text and notifies the
    !## desc:     GUI to save the companion .zin plot file alongside it.
    module procedure saveLensSystem
        use global_widgets, only: sysConfig
        use zoa_file_handler, only: open_file_to_sav_lens, zinPathFromZoa
        use optim_types, only: optim
        use zoom_manager, only: zoom_genSaveOutputText
        use zoa_ui_callbacks, only: notify_save_zin
        use mod_lens_data_manager
        implicit none

        integer :: fID
        logical :: doForceOverwrite, beQuiet
        character(len=2048) :: fullPath

        doForceOverwrite = .FALSE.
        if (present(overwrite)) doForceOverwrite = overwrite
        beQuiet = .FALSE.
        if (present(quiet)) beQuiet = quiet

        if (.not. beQuiet) then
            if (present(dirName)) then
                ! PRINT (not LogTermFOR): absolute paths must stay out of
                ! captured test output; unit 6 is suppressed in the test runner.
                print *, "File name to save is "//trim(dirName)//trim(fName)
            end if
            call zoa_emit("File name to save is "//trim(fName), "black")
        end if

        if (present(dirName)) then
            if (doForceOverwrite) then
                fID = open_file_to_sav_lens(fName, dirName=dirName, overwriteFlag=.TRUE., &
                                             fullPathOut=fullPath)
            else
                fID = open_file_to_sav_lens(fName, dirName=dirName, fullPathOut=fullPath)
            end if
        else
            if (doForceOverwrite) then
                fID = open_file_to_sav_lens(fName, overwriteFlag=.TRUE., fullPathOut=fullPath)
            else
                fID = open_file_to_sav_lens(fName, fullPathOut=fullPath)
            end if
        end if

        if (fID /= 0) then
            call sysConfig%genSaveOutputText(fID)
            call ldm%genSaveOutputText(fID)
            call sysConfig%genPostLensSaveText(fID)
            call optim%genSaveOutputText(fID)
            call zoom_genSaveOutputText(fID)
            close(fID)
            call notify_save_zin(zinPathFromZoa(trim(fullPath)))
        else
            call LogTermFOR("Error!  fiD is "//int2str(fID))
        end if
    end procedure saveLensSystem

    !## cmd:      SAVESESS
    !## syntax:   SAVESESS file
    !## category: File I/O
    !## desc:     Save the current session (lens plus settings) to a file.
    !##
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
            call sysConfig%genPostLensSaveText(fID)
            call notify_write_tab_state(fID)
            close(fID)
        else
            call LogTermFOR("Error!  fID is "//int2str(fID))
        end if
    end procedure execSaveSessionToFile

    !## cmd:      REF
    !## syntax:   REF n
    !## category: Fields & Wavelengths
    !## desc:     Set the reference (control) wavelength to index n.
    !##
    module procedure execSetWavelengthIndex
        use strings, only: parse
        use command_utils, only: isInputNumber
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2 .and. isInputNumber(trim(tokens(2)))) then
            call kdp_silent_begin()
            call kdp_lens_begin()
            call kdp_lens_cmd('CW', w1=str2real8(trim(tokens(2))))
            call kdp_lens_end()
            call kdp_silent_end()
        else
            call zoa_emit("No Wavelength Index Input.  Please try again", "red")
            return
        end if
    end procedure execSetWavelengthIndex

    !## cmd:      RMD
    !## syntax:   RMD Sk REFL|REFR|TIR
    !## category: Surface Parameters
    !## desc:     Set the surface Sk mode: reflector, refractor (air), or TIR reflector.
    !##
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
                case ('REFL', 'REFR', 'TIR')
                    call kdp_silent_begin()
                    call kdp_lens_begin()
                    call kdp_chg(surfNum)
                    select case(trim(tokens(3)))
                    case ('REFL'); call kdp_lens_cmd('REFL')
                    case ('REFR'); call kdp_lens_cmd('AIR')
                    case ('TIR');  call kdp_lens_cmd('REFLTIRO')
                    end select
                    call kdp_lens_end()
                    call kdp_silent_end()
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
                    call kdp_silent_begin()
                    call kdp_lens_begin()
                    call kdp_chg(0)
                    call kdp_lens_cmd('TH', w1=curr_par_ray_trace%getObjectThicknessToSetParaxialMag( &
                    &                          str2real8(trim(tokens(3))), curr_lens_data))
                    call kdp_lens_end()
                    call kdp_silent_end()
                else
                    call zoa_emit("No Mag Value specified.  Please try again", "red")
                end if
            end select
        end if
    end procedure execSetCodeVCmd

    !## cmd:      RED
    !## syntax:   RED f
    !## category: Lens System Commands
    !## desc:     Set an object-thickness reduction solve for paraxial magnification f.
    !##
    module procedure setMagSolve
        use command_utils, only: isInputNumber
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                call kdp_silent_begin()
                call kdp_lens_begin()
                call kdp_chg(0)
                call kdp_lens_cmd('REDSLV', w1=str2real8(trim(tokens(2))))
                call kdp_lens_end()
                call kdp_silent_end()
            else
                call zoa_emit("Error!  Unable to parse value "//trim(tokens(2))//" into number", "red")
            end if
        else
            call zoa_emit("Error!  Expecting RED X where X is the desired reduction factor", "red")
        end if
    end procedure setMagSolve

    !## cmd:      DEL
    !## syntax:   DEL PIM | DEL SOL <verb> Sk | DEL CON n | DEL VIG
    !## category: Lens System Commands
    !## desc:     Delete user-defined system data: PIM/thickness solves, angle/curvature
    !##           solves (DEL SOL CUY|CUX|THI Sk), a constraint (DEL CON n), or vignetting.
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
                call kdp_silent_begin()
                call kdp_lens_begin()
                call kdp_chg(surfNum)
                call kdp_lens_cmd('TSD')
                call kdp_lens_end()
                call kdp_silent_end()
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
                            call kdp_silent_begin()
                            call kdp_lens_begin()
                            call kdp_chg(surfNum)
                            call kdp_lens_cmd(trim(solve_del_cmd(delSlot)))
                            call kdp_lens_end()
                            call kdp_silent_end()
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
            case('PIK')
                call deletePickups()
            case('DDA', 'DAR', 'BEN', 'REV')
                call deleteDecenters()
            case default
                ! DEL Sk / DEL Si..j: delete surfaces (CODE V).  Highest first
                ! so the lower numbers in a range still name the same surfaces.
                if (tokens(2)(1:1) == 'S' .and. len_trim(tokens(2)) > 1) then
                    call deleteSurfaces(trim(tokens(2)(2:)))
                else
                    call zoa_emit("Unknown DEL option "//trim(tokens(2))// &
                    &  ".  Expect DEL Sk, DEL Si..j, DEL CON n, DEL SOL, DEL PIM, DEL VIG or DEL APE SA", "red")
                end if
            end select
        else
            call zoa_emit("Expect DEL Sk, DEL Si..j, DEL CON n, DEL SOL, DEL PIM, DEL VIG or DEL APE SA", "red")
        end if

    contains

        ! DEL DDA Sk|Si..j: remove the decenters and tilts.  DEL DAR|BEN|REV
        ! Sk|Si..j: make a decenter-and-return / bend / reversed surface a
        ! plain decentered one (values kept).
        subroutine deleteDecenters()
            use mod_surface, only: surf_tilt_flag
            integer :: s, s0, s1, want
            if (numTokens /= 3) then
                call zoa_emit("Expected DEL "//trim(tokens(2))//" Sk|Si..j", "red")
                return
            end if
            if (.not. parseSurfaceSpec(tokens(3), s0, s1)) return
            select case (trim(tokens(2)))
            case ('DAR'); want = 5
            case ('BEN'); want = 4
            case ('REV'); want = 7
            case default; want = 0
            end select
            call kdp_silent_begin()
            call kdp_lens_begin()
            do s = max(s0, 1), s1
                if (want == 0) then
                    call kdp_chg(s)
                    if (surf_tilt_flag(s) /= 0) call kdp_lens_cmd('TILTD')
                    call kdp_lens_cmd('DEC', w1=0.0_real64, w2=0.0_real64, w3=0.0_real64)
                else if (surf_tilt_flag(s) == want) then
                    call kdp_chg(s)
                    call kdp_lens_cmd('TILT')
                end if
            end do
            call kdp_lens_end()
            call kdp_silent_end()
            call notify_replot()
        end subroutine

        ! DEL PIK [XXX] [Sk|Si..j] (CODE V): delete all pickups, or those for
        ! parameter XXX, on the given surfaces (default all).  Zoom (Zk) and
        ! coefficient (Ck) ranges are not supported (no zoom pickups in Zoa).
        subroutine deletePickups()
            use pickup_manager, only: PIKUP_KINDS, NUM_PICKUP_KINDS, &
                                      pickup_j_from_cli, pickup_on_surf
            use iso_fortran_env, only: real64
            integer, allocatable :: surfs(:)
            integer :: k, it, s, j, jWant, s0, s1, nDel
            character(len=80) :: t

            jWant = 0
            s0 = 0; s1 = ldm%getLastSurf()
            do it = 3, numTokens
                t = tokens(it)
                if (t(1:1) == 'S' .and. len_trim(t) > 1 .and. &
                &   (isInputNumber(t(2:2)) .or. trim(t) == 'SI' .or. trim(t) == 'SO')) then
                    if (trim(t) == 'SI') then
                        s0 = ldm%getLastSurf(); s1 = s0
                    else if (trim(t) == 'SO') then
                        s0 = 0; s1 = 0
                    else if (index(t, '..') > 0) then
                        if (.not. cmd_parser_get_integer_range(trim(t(2:)), surfs)) then
                            call zoa_emit("Error:  bad surface range "//trim(t), "red")
                            return
                        end if
                        s0 = minval(surfs); s1 = maxval(surfs)
                    else if (isInputNumber(trim(t(2:)))) then
                        s0 = str2int(trim(t(2:))); s1 = s0
                    else
                        call zoa_emit("Error:  bad surface "//trim(t), "red")
                        return
                    end if
                else if (t(1:1) == 'Z' .or. t(1:1) == 'C') then
                    call zoa_emit("DEL PIK: zoom/coefficient ranges ("//trim(t)// &
                    &  ") are not supported", "red")
                    return
                else
                    jWant = pickup_j_from_cli(trim(t))
                    if (jWant == 0) then
                        call zoa_emit("Error:  "//trim(t)//" is not a pickup parameter", "red")
                        return
                    end if
                end if
            end do
            s0 = max(s0, 0); s1 = min(s1, ldm%getLastSurf())

            nDel = 0
            call kdp_silent_begin()
            call kdp_lens_begin()
            do s = s0, s1
                do k = 1, NUM_PICKUP_KINDS
                    j = PIKUP_KINDS(k)%j
                    if (jWant /= 0 .and. j /= jWant) cycle
                    if (.not. pickup_on_surf(s, j)) cycle
                    call kdp_chg(s)
                    call kdp_lens_cmd('PIKD', wq=trim(PIKUP_KINDS(k)%qual), &
                    &                 w1=real(s, real64))
                    nDel = nDel + 1
                end do
            end do
            call kdp_lens_end()
            call kdp_silent_end()
            if (nDel == 0) call zoa_emit("DEL PIK: no matching pickups", "black")
        end subroutine

        subroutine deleteSurfaces(spec)
            use iso_fortran_env, only: real64
            character(len=*), intent(in) :: spec
            integer, allocatable :: surfs(:)
            integer :: i, s0, s1, lastSurf

            if (index(spec, '..') > 0) then
                if (.not. cmd_parser_get_integer_range(spec, surfs)) then
                    call zoa_emit("Error:  bad surface range S"//spec//".  Expect DEL Si..j", "red")
                    return
                end if
                s0 = minval(surfs); s1 = maxval(surfs)
            else if (isInputNumber(spec)) then
                s0 = str2int(spec); s1 = s0
            else
                call zoa_emit("Error:  bad surface S"//spec//".  Expect DEL Sk or DEL Si..j", "red")
                return
            end if

            lastSurf = ldm%getLastSurf()
            if (s0 < 1 .or. s1 > lastSurf-1) then
                call zoa_emit("Error:  can only delete surfaces S1..S"//trim(int2str(lastSurf-1))// &
                &  " (not the object or image surface)", "red")
                return
            end if

            call kdp_silent_begin()
            call kdp_lens_begin()
            do i = s1, s0, -1
                call kdp_chg(i)
                call kdp_lens_cmd('DELK')
            end do
            call kdp_lens_end()
            call kdp_silent_end()
        end subroutine

    end procedure deleteStuff

    !## cmd:      EPD
    !## syntax:   EPD X
    !## category: System Parameters
    !## desc:     Set the entrance pupil diameter to X.
    !##
    module procedure setEPD
        ! Self-tokenizing (parse the raw line) rather than reading the DATMAI
        ! parse globals via getInputNumber -- so this handler works when
        ! dispatched by the front door, before PRO3 has run.  Same net effect:
        ! SAY (semi-aperture) = EPD / 2.
        use command_utils, only: isInputNumber
        use type_utils, only: str2real8, real2str
        use kdp_data_types, only: releaseImageFNoHold
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                ! EPD replaces a held image-space F/# (FNO), which would
                ! otherwise resize the pupil straight back over this value.
                call releaseImageFNoHold()
                call kdp_silent_begin()
                call kdp_lens_begin()
                ! CODE V's EPD is one circular pupil: both semi-apertures.
                ! (Until the SAY handler's lost IFs were restored, SAY set
                ! SAX too, which this relied on.)
                call kdp_lens_cmd('SAY', w1=str2real8(trim(tokens(2)))/2.0d0)
                call kdp_lens_cmd('SAX', w1=str2real8(trim(tokens(2)))/2.0d0)
                call kdp_lens_end()
                call kdp_silent_end()
            else
                call zoa_emit("Error! Unable to parse EPD value "//trim(tokens(2))//" into a number", "red")
            end if
        else
            call zoa_emit("Error! Expecting 'EPD X' where X is the entrance pupil diameter", "red")
        end if
    end procedure setEPD

    ! CODE V's FNO: the system aperture given as the paraxial image-space
    ! F/number, -1/(2u') of the marginal ray.  It is held: the entrance pupil
    ! is resized to keep it as the lens changes, until another aperture
    ! (EPD, ...) replaces it.  Engine: the aperture is first made an explicit
    ! semi-aperture (SAY, clearing NAO / float / object F/#), then FNBY and
    ! FNBX HLD hold the F/#.  "FNO" or "FNO ?" reports the current value.
    !## cmd:      FNO
    !## syntax:   FNO [f | ?]
    !## category: System Data
    !## desc:     Set (and hold) the image-space F/number, or report it.
    !##
    module procedure setFNO
        use command_utils, only: isInputNumber
        use type_utils, only: str2real8, real2str
        use mod_system, only: sys_say, sys_last_surf, sys_fno_flag_y
        use DATLEN, only: PXTRAY
        implicit none
        character(len=80) :: tokens(40)
        character(len=40) :: numStr
        integer :: numTokens
        real(real64) :: fno, u

        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens == 1 .or. (numTokens == 2 .and. trim(tokens(2)) == '?')) then
            u = PXTRAY(2, int(sys_last_surf()))
            if (abs(u) <= 1.0d-15) then
                call zoa_emit("FNO: the marginal ray leaves the lens parallel (afocal); no F/number", "red")
                return
            end if
            fno = -1.0d0/(2.0d0*u)
            if (sys_fno_flag_y() == -1.0d0) then
                call zoa_emit("FNO "//trim(real2str(fno, 6))//"  (held)", "black")
            else
                call zoa_emit("FNO "//trim(real2str(fno, 6)), "black")
            end if
        else if (numTokens == 2 .and. isInputNumber(tokens(2))) then
            fno = str2real8(trim(tokens(2)))
            if (fno <= 0.0d0) then
                call zoa_emit("Error! FNO must be greater than zero", "red")
                return
            end if
            write(numStr, '(G0)') fno
            call kdp_silent_begin()
            call kdp_lens_begin()
            call kdp_lens_cmd('SAY', w1=sys_say())
            call kdp_lens_end()
            call KDP_EXEC('FNBY HLD, '//trim(numStr))
            call KDP_EXEC('FNBX HLD, '//trim(numStr))
            ! The adjustment reaches the paraxial trace at once but the stored
            ! semi-aperture (and so real rays, VIE) only at the next EOS.
            call kdp_lens_begin()
            call kdp_lens_end()
            call kdp_silent_end()
        else
            call zoa_emit("Error! Expecting 'FNO X' where X is the image-space F/number", "red")
        end if
    end procedure setFNO

    ! CODE V's NAO: the system aperture given as the object-space numerical
    ! aperture (finite object distance only).  Replaces a held FNO.  Engine:
    ! NAOY and NAOX.  "NAO" or "NAO ?" reports the current value.
    !## cmd:      NAO
    !## syntax:   NAO [na | ?]
    !## category: System Data
    !## desc:     Set the object-space numerical aperture, or report it.
    !##
    module procedure setNAO
        use command_utils, only: isInputNumber
        use type_utils, only: str2real8, real2str
        use mod_system, only: sys_naoy, sys_na_set
        use mod_surface, only: surf_thickness
        use kdp_data_types, only: releaseImageFNoHold
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens
        real(real64) :: na

        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens == 1 .or. (numTokens == 2 .and. trim(tokens(2)) == '?')) then
            if (sys_na_set() == 0.0d0) then
                call zoa_emit("NAO is not set (the aperture is not defined by the object NA)", "black")
            else
                call zoa_emit("NAO "//trim(real2str(sys_naoy(), 6)), "black")
            end if
        else if (numTokens == 2 .and. isInputNumber(tokens(2))) then
            na = str2real8(trim(tokens(2)))
            if (na <= 0.0d0 .or. na >= 1.0d0) then
                call zoa_emit("Error! NAO must be between 0 and 1", "red")
                return
            end if
            if (abs(surf_thickness(0)) >= 1.0d10) then
                call zoa_emit("Error! NAO needs a finite object distance (the object is at infinity); use EPD or FNO", "red")
                return
            end if
            call releaseImageFNoHold()
            call kdp_silent_begin()
            call kdp_lens_begin()
            call kdp_lens_cmd('NAOY', w1=na)
            call kdp_lens_cmd('NAOX', w1=na)
            call kdp_lens_end()
            call kdp_silent_end()
        else
            call zoa_emit("Error! Expecting 'NAO X' where X is the object-space numerical aperture", "red")
        end if
    end procedure setNAO

    ! The system aperture floats with the stop: the entrance pupil is whatever
    ! the stop surface's clear aperture admits (Zemax "Float By Stop Size";
    ! CODE V has no equivalent).  Replaces a held FNO.  Engine: SAY FLOAT.
    !## cmd:      FLOAT
    !## syntax:   FLOAT
    !## category: System Data
    !## desc:     Make the entrance pupil float with the stop surface's clear aperture.
    !##
    module procedure setFLOAT
        use kdp_data_types, only: releaseImageFNoHold
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens /= 1) then
            call zoa_emit("Error! FLOAT takes no arguments", "red")
            return
        end if
        call releaseImageFNoHold()
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_lens_cmd('SAY', wq='FLOAT')
        call kdp_lens_end()
        call kdp_silent_end()
    end procedure setFLOAT

    module procedure setParaxialImageSolve
        use global_widgets, only: curr_lens_data

        integer :: surfNum

        surfNum = curr_lens_data%num_surfaces - 2
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_chg(surfNum)
        call kdp_lens_cmd('PY', w1=0.0d0)
        call kdp_lens_end()
        call kdp_silent_end()
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
            ! Plain glass name.  A legacy Schott name (SK16, F2, BK7, ...) that has
            ! a modern N- equivalent resolves to that SCH2000 entry -- the
            ! long-standing behavior (SK16 -> N-SK16_SCH2000) -- rather than to
            ! the first catalog in search order, which is the obsolete SCHOTT table.
            if (gdb%isNameInCatalog('SCH2000', catalogIdx) .and. &
                gdb%isGlassInCatalog('N-'//trim(strInput), catalogIdx)) then
                strOut = 'SCH2000 N-'//trim(strInput)
            ! Otherwise, if it starts with N, also try the N-xxx spelling
            else if (strInput(1:1) == 'N' .and. &
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

    ! Dispatch a glass command produced by getSetGlassText typed instead of
    ! through PROCESKDP.  Two forms:
    !   MODEL D<label>,<nd>,<vd>  (SGLASS: WQ=D<label>, W1=nd, W2=vd)
    !   <CATALOG> <glassname>     (GLSCAT: WC=catalog word, WS=glass name;
    !                              GLAK is one such catalog word)
    ! Re-parsing the text (rather than passing full-precision nd/vd) keeps the
    ! exact real2str values getSetGlassText already committed to, so output is
    ! byte-identical while the legacy parser is bypassed.
    module procedure applyGlassText
        use strings, only: parse
        implicit none
        character(len=80) :: tk(40)
        integer :: nt

        call parse(trim(glassCmd), ' ,', tk, nt)
        if (nt < 2) return
        if (trim(tk(1)) == 'MODEL') then
            if (nt < 4) return
            call kdp_lens_cmd('MODEL', wq=trim(tk(2)), &
            &                 w1=str2real8(trim(tk(3))), w2=str2real8(trim(tk(4))))
        else
            call kdp_lens_cmd(trim(tk(1)), ws=trim(tk(2)))
        end if
    end procedure applyGlassText

    !## cmd:      EDI
    !## syntax:   EDI PREF
    !## category: Utilities
    !## desc:     Open an editor (EDI PREF opens preferences).
    !##
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

    !## cmd:      RESAUTO
    !## syntax:   RESAUTO
    !## category: File I/O
    !## desc:     Restore the most recently auto-saved lens.
    !## desc:     A companion .zin plot file with the same name is loaded too, if present.
    !##
    module procedure execRESAUTO
        use zoa_file_handler, only: getTempDirectory, getCurrentLensFileName
        implicit none
        character(len=1024) :: fullPath

        fullPath = trim(getTempDirectory())//getCurrentLensFileName()
        ! Emit only the filename; absolute paths must stay out of captured
        ! test output (full path still goes to the terminal via print).
        print *, "Restoring from: "//trim(fullPath)
        call zoa_emit("Restoring from: "//trim(getCurrentLensFileName()), "black")
        ! Same shared lens-load sequence as RES, so session restore clears the
        ! same state the same way as every other lens-load path.
        call loadLensFromZoaPath(trim(fullPath))
    end procedure execRESAUTO

    !## cmd:      XDE
    !## syntax:   XDE|YDE|ZDE|ADE|BDE|CDE|XOD|YOD|ZOD [Sk|Si..j] value
    !## category: Tilts and Decenters
    !## desc:     Decenter (XDE/YDE/ZDE, lens units) or tilt (ADE/BDE/CDE, degrees) of the
    !##           new axis at surface Sk (default: the current surface).  CODE V order:
    !##           decenters first, then ADE, BDE, CDE.  XOD/YOD/ZOD: the point (relative
    !##           to the surface origin) the tilt is applied about.
    !##
    module procedure setDecenterValue
        use command_utils, only: isInputNumber
        use mod_lens_data_manager, only: ldm
        use zoa_ui_callbacks, only: notify_replot
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, s, s0, s1
        character(len=8) :: kdpWord
        real(kind=real64) :: v
        logical :: hasSurf

        call parse(trim(iptStr), ' ', tokens, numTokens)
        select case (trim(tokens(1)))
        case ('XDE'); kdpWord = 'XD'
        case ('YDE'); kdpWord = 'YD'
        case ('ZDE'); kdpWord = 'ZD'
        case ('ADE'); kdpWord = 'ALPHA'
        case ('BDE'); kdpWord = 'BETA'
        case ('CDE'); kdpWord = 'GAMMA'
        case default; kdpWord = 'PIVOT'     ! XOD/YOD/ZOD
        end select

        hasSurf = (numTokens == 3)
        if (numTokens < 2 .or. numTokens > 3) then
            call zoa_emit("Error:  expected "//trim(tokens(1))//" [Sk|Si..j] value", "red")
            return
        end if
        if (.not. isInputNumber(trim(tokens(numTokens)))) then
            call zoa_emit("Error:  "//trim(tokens(1))//": bad value "//trim(tokens(numTokens)), "red")
            return
        end if
        v = str2real8(trim(tokens(numTokens)))
        if (hasSurf) then
            if (.not. parseSurfaceSpec(tokens(2), s0, s1)) return
        end if

        call kdp_silent_begin()
        call kdp_lens_begin()
        if (hasSurf) then
            do s = s0, s1
                call kdp_chg(s)
                call applyValue(s)
            end do
        else
            ! No Sk: the current surface (the one being built in a .zoa).
            call kdp_chg(ldm%getSurfacePointer())
            call applyValue(ldm%getSurfacePointer())
        end if
        call kdp_lens_end()
        call kdp_silent_end()
        call notify_replot()

    contains

        subroutine applyValue(sk)
            use mod_surface, only: surf_pivot_x, surf_pivot_y, surf_pivot_z, &
            &                      surf_focus_dx, surf_focus_dy, surf_focus_dz
            integer, intent(in) :: sk
            real(kind=real64) :: p(3), d(3)
            if (trim(kdpWord) /= 'PIVOT') then
                call kdp_lens_cmd(trim(kdpWord), w1=v)
                return
            end if
            ! KDP's PIVOT sets all three coordinates and clears the surface's
            ! decenters: change one coordinate, then put the decenters back.
            p = [surf_pivot_x(sk), surf_pivot_y(sk), surf_pivot_z(sk)]
            d = [surf_focus_dx(sk), surf_focus_dy(sk), surf_focus_dz(sk)]
            select case (trim(tokens(1)))
            case ('XOD'); p(1) = v
            case ('YOD'); p(2) = v
            case default; p(3) = v
            end select
            call kdp_lens_cmd('PIVOT', w1=p(1), w2=p(2), w3=p(3))
            if (d(1) /= 0.0_real64) call kdp_lens_cmd('XD', w1=d(1))
            if (d(2) /= 0.0_real64) call kdp_lens_cmd('YD', w1=d(2))
            if (d(3) /= 0.0_real64) call kdp_lens_cmd('ZD', w1=d(3))
        end subroutine
    end procedure setDecenterValue

    !## cmd:      DAR
    !## syntax:   DAR|BEN|REV [Sk|Si..j] | RET [Sk] Sj
    !## category: Tilts and Decenters
    !## desc:     Decenter type of surface Sk (default: the current surface):
    !##           DAR decenter-and-return, BEN decenter-and-bend (mirrors), REV reversed
    !##           decenter, RET return to the coordinate system of surface Sj.
    !##           DEL DDA|DAR|BEN|REV Sk|Si..j removes them.
    !##
    module procedure setDecenterType
        use command_utils, only: isInputNumber
        use mod_lens_data_manager, only: ldm
        use zoa_ui_callbacks, only: notify_replot
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, s, s0, s1, retSurf
        logical :: hasSurf

        call parse(trim(iptStr), ' ', tokens, numTokens)

        retSurf = -1
        if (trim(tokens(1)) == 'RET') then
            ! RET [Sk] Sj
            if (numTokens < 2 .or. numTokens > 3) then
                call zoa_emit("Error:  expected RET [Sk] Sj", "red")
                return
            end if
            block
                integer :: r0, r1
                if (.not. parseSurfaceSpec(tokens(numTokens), r0, r1)) return
                if (r0 /= r1) then
                    call zoa_emit("Error:  RET returns to a single surface Sj", "red")
                    return
                end if
                retSurf = r0
            end block
            hasSurf = (numTokens == 3)
            if (hasSurf) then
                if (.not. parseSurfaceSpec(tokens(2), s0, s1)) return
            end if
        else
            if (numTokens > 2) then
                call zoa_emit("Error:  expected "//trim(tokens(1))//" [Sk|Si..j]", "red")
                return
            end if
            hasSurf = (numTokens == 2)
            if (hasSurf) then
                if (.not. parseSurfaceSpec(tokens(2), s0, s1)) return
            end if
        end if

        call kdp_silent_begin()
        call kdp_lens_begin()
        if (hasSurf) then
            do s = s0, s1
                call kdp_chg(s)
                call applyType()
            end do
        else
            call kdp_chg(ldm%getSurfacePointer())
            call applyType()
        end if
        call kdp_lens_end()
        call kdp_silent_end()
        call notify_replot()

    contains

        ! KDP's TILT with a qualifier and no values keeps the surface's angles.
        subroutine applyType()
            if (retSurf >= 0) then
                call kdp_lens_cmd('TILT', wq='RET', w1=real(retSurf, real64))
            else
                call kdp_lens_cmd('TILT', wq=trim(tokens(1)))
            end if
        end subroutine
    end procedure setDecenterType

    !## cmd:      REX
    !## syntax:   REX|REY|ELX|ELY|ADX|ADY|ARO [OBS] [Sk|Si..j] value
    !## category: Apertures
    !## desc:     Clear aperture (or, with OBS, obscuration) of surface Sk (default: the
    !##           current surface): REX/REY rectangle and ELX/ELY ellipse X/Y semi-widths,
    !##           ADX/ADY its decenter, ARO its rotation (degrees).  CIR OBS r is a
    !##           circular obscuration.
    !##
    module procedure setApertureValue
        use command_utils, only: isInputNumber
        use mod_lens_data_manager, only: ldm, get_aperture_spec
        use zoa_ui_callbacks, only: notify_replot
        implicit none

        character(len=80) :: tokens(40), cmd
        integer :: numTokens, i, s, s0, s1, nArg
        character(len=80) :: args(3)
        logical :: obs, hasSurf
        real(kind=real64) :: v

        call parse(trim(iptStr), ' ', tokens, numTokens)
        cmd = tokens(1)
        obs = .false.
        nArg = 0
        do i = 2, numTokens
            if (trim(tokens(i)) == 'OBS') then
                obs = .true.
            else if (nArg < 3) then
                nArg = nArg + 1
                args(nArg) = tokens(i)
            else
                nArg = 4
            end if
        end do
        if (nArg < 1 .or. nArg > 2) then
            call zoa_emit("Error:  expected "//trim(cmd)//" [OBS] [Sk|Si..j] value", "red")
            return
        end if
        if (.not. isInputNumber(trim(args(nArg)))) then
            call zoa_emit("Error:  "//trim(cmd)//": bad value "//trim(args(nArg)), "red")
            return
        end if
        v = str2real8(trim(args(nArg)))
        hasSurf = (nArg == 2)
        if (hasSurf) then
            if (.not. parseSurfaceSpec(args(1), s0, s1)) return
        else
            s0 = ldm%getSurfacePointer(); s1 = s0
        end if
        if (s0 < 1 .and. s1 < 1) then
            call zoa_emit("Error:  the object surface takes no aperture", "red")
            return
        end if

        call kdp_silent_begin()
        call kdp_lens_begin()
        do s = max(s0, 1), s1
            call applyAperture(s)
        end do
        call kdp_lens_end()
        call kdp_silent_end()
        call notify_replot()

    contains

        ! Change one property of surface sk's aperture and re-send the whole
        ! KDP CLAP/COBS (it takes every value at once).
        subroutine applyAperture(sk)
            integer, intent(in) :: sk
            integer :: shape
            real(kind=real64) :: ys, xs, yd, xd, tilt
            character(len=4) :: word

            call get_aperture_spec(sk, obs, shape, ys, xs, yd, xd, tilt)
            select case (trim(cmd))
            case ('CIR')
                shape = 1; ys = v; xs = v
            case ('REX', 'REY', 'ELX', 'ELY')
                ! A new rectangle/ellipse starts square/round from this value.
                if ((cmd(1:1) == 'R' .and. shape /= 2) .or. (cmd(1:1) == 'E' .and. shape /= 3)) then
                    if (shape == 1) then
                        xs = ys
                    else
                        xs = v; ys = v
                    end if
                end if
                shape = merge(2, 3, cmd(1:1) == 'R')
                if (cmd(3:3) == 'X') then
                    xs = v
                else
                    ys = v
                end if
            case default
                if (shape < 1 .or. shape > 3) then
                    call zoa_emit("Error:  "//trim(cmd)//": surface "//trim(int2str(sk))// &
                    &  " has no circular, rectangular or elliptical "// &
                    &  trim(merge('obscuration', 'aperture   ', obs)), "red")
                    return
                end if
                select case (trim(cmd))
                case ('ADX'); xd = v
                case ('ADY'); yd = v
                case default; tilt = v
                end select
            end select

            word = merge('COBS', 'CLAP', obs)
            call kdp_chg(sk)
            select case (shape)
            case (1)
                if (obs) then
                    call kdp_lens_cmd(word, w1=ys, w2=yd, w3=xd)
                else
                    call kdp_lens_cmd(word, w1=ys, w2=yd, w3=xd, w4=ys)
                end if
            case (2)
                call kdp_lens_cmd(word, wq='RECT', w1=ys, w2=xs, w3=yd, w4=xd)
            case (3)
                call kdp_lens_cmd(word, wq='ELIP', w1=ys, w2=xs, w3=yd, w4=xd)
            end select
            call kdp_lens_cmd(word, wq='TILT', w1=tilt)
        end subroutine
    end procedure setApertureValue

    !## cmd:      TIN
    !## syntax:   TIN|CIY [Sk|Si..j] increment
    !## category: Surface Parameters
    !## desc:     Add increment to the thickness (TIN) or Y curvature (CIY) of surface Sk
    !##           (default: the current surface).
    !##
    module procedure incrementSurfValue
        use command_utils, only: isInputNumber
        use mod_lens_data_manager, only: ldm
        use mod_surface, only: surf_toric_flag
        use zoa_ui_callbacks, only: notify_replot
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, s, s0, s1
        real(kind=real64) :: v

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens < 2 .or. numTokens > 3) then
            call zoa_emit("Error:  expected "//trim(tokens(1))//" [Sk|Si..j] increment", "red")
            return
        end if
        if (.not. isInputNumber(trim(tokens(numTokens)))) then
            call zoa_emit("Error:  "//trim(tokens(1))//": bad value "//trim(tokens(numTokens)), "red")
            return
        end if
        v = str2real8(trim(tokens(numTokens)))
        if (numTokens == 3) then
            if (.not. parseSurfaceSpec(tokens(2), s0, s1)) return
        else
            s0 = ldm%getSurfacePointer(); s1 = s0
        end if
        call kdp_silent_begin()
        call kdp_lens_begin()
        do s = s0, s1
            if (trim(tokens(1)) == 'CIY' .and. surf_toric_flag(s) /= 0) then
                call zoa_emit("Error:  CIY: surface "//trim(int2str(s))//" is toric (not supported)", "red")
                cycle
            end if
            call kdp_chg(s)
            if (trim(tokens(1)) == 'TIN') then
                call kdp_lens_cmd('TH', wq='DELT', w1=v)
            else
                call kdp_lens_cmd('CV', wq='DELT', w1=v)
            end if
        end do
        call kdp_lens_end()
        call kdp_silent_end()
        call notify_replot()
    end procedure incrementSurfValue

    !## cmd:      BRE
    !## syntax:   BRE Sk
    !## category: Surface Parameters
    !## desc:     Break the cemented interface at Sk: insert, ahead of Sk, a surface of the
    !##           same shape (curvature, conic, asphere) with zero air space after it.
    !##
    module procedure breakSurface
        use mod_surface, only: surf_curvature, surf_conic, surf_asphere_coeff
        use mod_lens_data_manager, only: ldm
        use zoa_ui_callbacks, only: notify_replot
        use type_utils, only: compactReal
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, k, s0, s1, i
        real(kind=real64) :: cv, cc, a(10)
        character(len=16) :: sk
        ! CODE V letters for orders 4..18 (index = order/2)
        character(len=1), parameter :: aLetter(10) = [' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', ' ']

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens /= 2) then
            call zoa_emit("Error:  expected BRE Sk", "red")
            return
        end if
        if (.not. parseSurfaceSpec(tokens(2), s0, s1)) return
        k = s0
        if (s0 /= s1 .or. k < 1 .or. k >= ldm%getLastSurf()) then
            call zoa_emit("Error:  BRE needs one surface between S1 and the last surface", "red")
            return
        end if
        cv = surf_curvature(k)
        cc = surf_conic(k)
        do i = 1, 10
            a(i) = surf_asphere_coeff(k, 2*i)
        end do
        ! INS Sk: a new air surface, zero thickness, ahead of Sk (pickups,
        ! solves and zoom data renumber as for INS).
        call insertSurf('INS S'//trim(int2str(k)))
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_chg(k)
        call kdp_lens_cmd('CV', w1=cv)
        call kdp_lens_end(refreshSurf=k)
        call kdp_silent_end()
        ! Conic and asphere terms through their own commands (they also set
        ! KDP's asphere flag; bare AD/CCK leave the terms untraced).
        sk = ' S'//trim(int2str(k))//' '
        if (cc /= 0.0_real64) call updateConicConstant('K'//trim(sk)//' '//trim(compactReal(cc, 17)))
        do i = 2, 9
            if (a(i) /= 0.0_real64) call updateAsphereTerms(aLetter(i)//trim(sk)//' '// &
            &  trim(compactReal(a(i), 17)))
        end do
        if (a(1) /= 0.0_real64 .or. a(10) /= 0.0_real64) call zoa_emit( &
        &  "Warning:  BRE: 2nd/20th-order asphere terms are not copied", "red")
        call notify_replot()
    end procedure breakSurface

    !## cmd:      AFC
    !## syntax:   AFC [Yes|No]
    !## category: System Data
    !## desc:     Afocal (Yes) or focal (No) evaluation mode; bare AFC shows the mode.
    !##
    module procedure setAfocal
        use mod_system, only: sys_mode
        use zoa_ui_callbacks, only: notify_replot
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens
        character(len=8) :: q

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 1) then
            if (nint(sys_mode()) == 3 .or. nint(sys_mode()) == 4) then
                call zoa_emit("AFC YES (afocal)", "black")
            else
                call zoa_emit("AFC NO (focal)", "black")
            end if
            return
        end if
        select case (trim(tokens(2)))
        case ('YES', 'Y'); q = 'AFOCAL'
        case ('NO', 'N');  q = 'FOCAL'
        case default
            call zoa_emit("Error:  expected AFC Yes|No", "red")
            return
        end select
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_lens_cmd('MODE', wq=trim(q))
        call kdp_lens_end()
        call kdp_silent_end()
        call notify_replot()
    end procedure setAfocal

    !## cmd:      WRL
    !## syntax:   WRL [filename]
    !## category: Lens Files
    !## desc:     Write the lens as CODE V commands (filename.seq); same as ZOA2CV.
    !##
    module procedure writeLensSeq
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens
        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens >= 2) then
            call exportLensToCodeV('ZOA2CV '//trim(iptStr(index(iptStr, ' ')+1:)))
        else
            call exportLensToCodeV('ZOA2CV')
        end if
    end procedure writeLensSeq

    !## cmd:      PKL
    !## syntax:   PKL [Sk|Si..j]
    !## category: Surface Parameters
    !## desc:     List the pickups on the given surfaces (default all).
    !##
    module procedure listPickups
        use pickup_manager, only: PIKUP_KINDS, NUM_PICKUP_KINDS, pickup_get
        use mod_lens_data_manager, only: ldm
        use type_utils, only: compactReal
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, s, s0, s1, k, src, n
        real(kind=real64) :: scale, offset
        character(len=200) :: line
        character(len=8) :: nm
        character(len=32) :: sc

        call parse(trim(iptStr), ' ', tokens, numTokens)
        s0 = 0; s1 = ldm%getLastSurf()
        if (numTokens >= 2) then
            if (.not. parseSurfaceSpec(tokens(2), s0, s1)) return
        end if
        n = 0
        do s = s0, s1
            do k = 1, NUM_PICKUP_KINDS
                if (.not. pickup_get(s, PIKUP_KINDS(k)%j, src, scale, offset)) cycle
                if (n == 0) call zoa_emit("Surf  Param     From   Scale            Offset", "black")
                n = n + 1
                nm = PIKUP_KINDS(k)%cli
                if (len_trim(nm) == 0) nm = PIKUP_KINDS(k)%qual
                write(line, '(A4,2X,A8,2X,A5)') 'S'//trim(int2str(s)), nm, 'S'//trim(int2str(src))
                if (trim(nm) /= 'GLA') then
                    sc = compactReal(scale, 15)
                    line = trim(line)//'  '//sc(1:16)//' '//trim(compactReal(offset, 15))
                end if
                call zoa_emit(trim(line), "black")
            end do
        end do
        if (n == 0) call zoa_emit("No pickups", "black")
    end procedure listPickups

    !## cmd:      ZLI
    !## syntax:   ZLI
    !## category: Zoom
    !## desc:     List the zoom (multi-configuration) data; same as a bare ZOO.
    !##
    module procedure listZoom
        use zoom_manager, only: zoom_list
        implicit none
        call zoom_list()
    end procedure listZoom

    !## cmd:      DEZ
    !## syntax:   DEZ k
    !## category: Zoom
    !## desc:     Dezoom: keep zoom position k as the (single-configuration) lens and
    !##           remove the zoom data.
    !##
    module procedure dezoom
        use zoom_manager, only: zoom_switch, zoom_reset, zoom_num_configs, zoom_current_config
        use command_utils, only: isInputNumber
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, k

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens /= 2 .or. .not. isInputNumber(trim(tokens(2)))) then
            call zoa_emit("Error:  expected DEZ k", "red")
            return
        end if
        k = str2int(trim(tokens(2)))
        if (k < 1 .or. k > zoom_num_configs()) then
            call zoa_emit("Error:  DEZ: position "//trim(int2str(k))//" out of range (1.."// &
            &  trim(int2str(zoom_num_configs()))//")", "red")
            return
        end if
        if (k /= zoom_current_config()) call zoom_switch(k)
        call zoom_reset()
        call zoa_emit("Dezoomed to position "//trim(int2str(k)), "black")
    end procedure dezoom

    !## cmd:      GSC
    !## syntax:   GSC [Sj]
    !## category: Tilts and Decenters
    !## desc:     List every surface's global position (X Y Z) and orientation (ADE BDE CDE,
    !##           degrees) relative to surface Sj (default S1).
    !##
    module procedure listGlobalSurfaces
        use DATLEN, only: VERTEX
        use mod_lens_data_manager, only: ldm
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, s, j, j1
        real(kind=real64) :: d(3,3), al, be, ga, cb
        real(kind=real64), parameter :: r2d = 57.29577951308232_real64
        character(len=200) :: line

        call parse(trim(iptStr), ' ', tokens, numTokens)
        j = 1
        if (numTokens >= 2) then
            if (.not. parseSurfaceSpec(tokens(2), j, j1)) return
        end if
        call PROCESKDP('GLOBAL '//trim(int2str(j)))
        call zoa_emit("Global coordinates relative to surface "//trim(int2str(j)), "black")
        call zoa_emit("Surf          X               Y               Z         ADE        BDE        CDE", "black")
        do s = 0, ldm%getLastSurf()
            d = reshape(VERTEX(4:12, s), [3, 3], order=[2, 1])
            ! Angles from the rotation matrix, as KDP's DISP does.
            be = asin(max(-1.0_real64, min(1.0_real64, -d(3,1))))
            cb = cos(be)
            if (abs(cb) > 1.0e-12_real64) then
                al = atan2(d(3,2)/cb, d(3,3)/cb)
                ga = atan2(-d(2,1)/cb, d(1,1)/cb)
            else
                al = atan2(d(1,2), d(1,3)); ga = 0.0_real64
            end if
            write(line, '(A4,3F16.6,3F11.5)') 'S'//trim(int2str(s)), clean(VERTEX(1, s)), &
            &  clean(VERTEX(2, s)), clean(VERTEX(3, s)), clean(al*r2d), clean(be*r2d), clean(ga*r2d)
            call zoa_emit(trim(line), "black")
        end do
    contains
        ! No "-0.000000" for round-off.
        real(kind=real64) function clean(x)
            real(kind=real64), intent(in) :: x
            clean = x
            if (abs(x) < 5.0e-7_real64) clean = 0.0_real64
        end function
    end procedure listGlobalSurfaces

    !## cmd:      RSL
    !## syntax:   RSL [INF | DEF | radius]
    !## category: System Data
    !## desc:     OPD reference sphere: DEF (default) radius from the exit pupil for each
    !##           field; INF flat reference; radius = that radius for every field (positive:
    !##           sphere before the image).  Bare RSL shows the setting.
    !##
    module procedure setRefSphereLocation
        use ref_sphere_settings, only: rsl_set, rsl_mode, rsl_value, RSL_MODE_DEF, RSL_MODE_INF, RSL_MODE_VALUE
        use command_utils, only: isInputNumber
        use type_utils, only: compactReal
        use zoa_ui_callbacks, only: notify_replot, notify_refresh_sysconfig
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 1) then
            select case (rsl_mode())
            case (RSL_MODE_INF);   call zoa_emit("RSL INF", "black")
            case (RSL_MODE_VALUE); call zoa_emit("RSL "//trim(compactReal(rsl_value(), 15)), "black")
            case default;     call zoa_emit("RSL DEF", "black")
            end select
            return
        end if
        if (numTokens > 2) then
            call zoa_emit("Error:  expected RSL [INF|DEF|radius]", "red")
            return
        end if
        select case (trim(tokens(2)))
        case ('INF'); call rsl_set(RSL_MODE_INF)
        case ('DEF'); call rsl_set(RSL_MODE_DEF)
        case default
            if (.not. isInputNumber(trim(tokens(2)))) then
                call zoa_emit("Error:  expected RSL [INF|DEF|radius]", "red")
                return
            end if
            if (str2real8(trim(tokens(2))) == 0.0_real64) then
                call zoa_emit("Error:  RSL radius must not be zero", "red")
                return
            end if
            call rsl_set(RSL_MODE_VALUE, str2real8(trim(tokens(2))))
        end select
        call notify_replot()
        ! Not a lens update, so the System Config panel is not refreshed by
        ! the usual end-of-update hook: refresh it here (no-op if closed).
        call notify_refresh_sysconfig()
    end procedure setRefSphereLocation

end submodule mod_codev_lensops
