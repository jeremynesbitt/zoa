! Lens module commands (CODE V "Lens Module (Eikonal-based lens)"): MOD,
! MFL, MRD, MFF, MBF, MEN, MFD, MED, MWL, MCO.  The data is the typed store in
! module lens_module; the optics are in lens_module_math.
submodule (codeV_commands) mod_codev_lens_module
use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                       kdp_lens_end, kdp_chg, kdp_lens_cmd
implicit none

contains

    !## cmd:      MOD
    !## syntax:   MOD Sk  |  MOD
    !## category: Surface Parameters
    !## desc:     Make surfaces Sk and Sk+1 a lens module: a "black box" lens
    !##           given by its first-order properties (MFL, MFF, MBF), perfect
    !##           for an object at infinity, with the aberrations set by MCO.
    !##           Both surfaces are made flat and the gap air.  MOD alone lists
    !##           the modules.  CHR/ACH and finite conjugates (MRD) are not yet
    !##           supported.
    !##
    !## cmd:      MFL
    !## syntax:   MFL Sk f
    !## category: Surface Parameters
    !## desc:     Lens module focal length (required).
    !##
    !## cmd:      MFF
    !## syntax:   MFF Sk d
    !## category: Surface Parameters
    !## desc:     Lens module front vertex to front focal point (default -MFL).
    !##
    !## cmd:      MBF
    !## syntax:   MBF Sk d
    !## category: Surface Parameters
    !## desc:     Lens module rear vertex to rear focal point (default +MFL).
    !##
    !## cmd:      MEN
    !## syntax:   MEN Sk d
    !## category: Surface Parameters
    !## desc:     Lens module front vertex to entrance pupil (default 0).
    !##
    !## cmd:      MFD
    !## syntax:   MFD Sk [IMG] value
    !## category: Surface Parameters
    !## desc:     Lens module maximum field: object-space angle in degrees, or
    !##           the image height with IMG.  Needed for MCO.
    !##
    !## cmd:      MED
    !## syntax:   MED Sk d
    !## category: Surface Parameters
    !## desc:     Lens module entrance pupil diameter.  Needed for MCO.
    !##
    !## cmd:      MWL
    !## syntax:   MWL Sk w1 w2 w3
    !## category: Surface Parameters
    !## desc:     Lens module wavelengths (nm) of the MCO colour coefficients
    !##           C1-C3 and C6-C8 (default 656.27 587.56 486.13).
    !##
    !## cmd:      MCO
    !## syntax:   MCO Sk Cj value
    !## category: Surface Parameters
    !## desc:     Lens module aberration, in waves (reference wavelength) at the
    !##           edge of the pupil and field: C11 spherical, C12 coma, C13
    !##           astigmatism, C14 distortion, C15 field curvature; C1-C3 axial
    !##           and C6-C8 lateral colour at the MWL wavelengths.
    !##
    !## cmd:      MRD
    !## syntax:   MRD Sk 0
    !## category: Surface Parameters
    !## desc:     Lens module reduction ratio; only 0 (object at infinity) is
    !##           supported so far.
    !##
    module procedure execLensModule
        use strings, only: parse
        use lens_module, only: lm_store, lm_params, LM_FIELD_IMG, lm_warned
        use mod_lens_data_manager, only: ldm
        use type_utils, only: str2int, int2str
        implicit none

        character(len=80) :: tokens(40)
        character(len=8) :: word
        integer :: numTokens, k, j, last, ios
        real(real64) :: v(3)

        call parse(trim(iptStr), ' ', tokens, numTokens)
        word = tokens(1)
        last = ldm%getLastSurf()

        if (word == 'MOD' .and. numTokens == 1) then
            call listModules()
            return
        end if
        if (numTokens < 2 .or. .not. isSurfCommand(trim(tokens(2)))) then
            call zoa_emit(trim(word)//": usage: "//trim(word)//" Sk ...", "red")
            return
        end if
        k = getSurfNumFromSurfCommand(trim(tokens(2)))

        if (word == 'MOD') then
            if (numTokens > 2) then
                if (tokens(3) == 'CHR' .or. tokens(3) == 'ACH') then
                    call zoa_emit("MOD: CHR/ACH lens modules are not supported yet; "// &
                    &             "use MWL and MCO C1-C8 for colour", "red")
                    return
                end if
            end if
            if (k < 1 .or. k+1 >= last) then
                call zoa_emit("MOD: a lens module needs surfaces Sk and Sk+1 between "// &
                &             "the object and the image (Sk from 1 to "//trim(int2str(last-2))//")", "red")
                return
            end if
            if (lm_store(k-1)%active .or. lm_store(k+1)%active) then
                call zoa_emit("MOD: S"//trim(int2str(k))//" or S"//trim(int2str(k+1))// &
                &             " already belongs to a lens module", "red")
                return
            end if
            if (.not. lm_store(k)%active) then
                lm_store(k) = lm_params()
                lm_store(k)%active = .true.
            end if
            lm_warned = .false.
            ! the module's surfaces are flat, with air between them
            call kdp_silent_begin()
            call kdp_lens_begin()
            call kdp_chg(k)
            call kdp_lens_cmd('CV', w1=0.0_real64)
            call kdp_lens_cmd('AIR')
            call kdp_chg(k+1)
            call kdp_lens_cmd('CV', w1=0.0_real64)
            call kdp_lens_end(refreshAll=.true.)
            call kdp_silent_end()
            return
        end if

        if (.not. lm_store(k)%active) then
            call zoa_emit(trim(word)//": S"//trim(int2str(k))//" is not a lens module (MOD S"// &
            &             trim(int2str(k))//" first)", "red")
            return
        end if

        select case (word)
        case ('MFL', 'MRD', 'MFF', 'MBF', 'MEN', 'MED')
            if (.not. getReal(3, v(1))) return
            select case (word)
            case ('MFL')
                if (v(1) == 0.0_real64) then
                    call zoa_emit("MFL: the focal length must not be 0", "red"); return
                end if
                lm_store(k)%efl = v(1)
            case ('MRD')
                if (v(1) /= 0.0_real64) then
                    call zoa_emit("MRD: only an object at infinity (MRD 0) is supported so far", "red")
                    return
                end if
            case ('MFF'); lm_store(k)%mff = v(1); lm_store(k)%mff_set = .true.
            case ('MBF'); lm_store(k)%mbf = v(1); lm_store(k)%mbf_set = .true.
            case ('MEN'); lm_store(k)%men = v(1)
            case ('MED')
                if (v(1) <= 0.0_real64) then
                    call zoa_emit("MED: the entrance pupil diameter must be positive", "red"); return
                end if
                lm_store(k)%med = v(1)
            end select
        case ('MFD')
            if (numTokens >= 4 .and. (tokens(3) == 'OBJ' .or. tokens(3) == 'IMG')) then
                if (tokens(3) == 'OBJ') then
                    call zoa_emit("MFD: OBJ (object height) needs a finite object, which "// &
                    &             "lens modules do not support yet", "red")
                    return
                end if
                if (.not. getReal(4, v(1))) return
                lm_store(k)%mfd_kind = LM_FIELD_IMG
            else
                if (.not. getReal(3, v(1))) return
                lm_store(k)%mfd_kind = 0
            end if
            lm_store(k)%mfd = v(1)
        case ('MWL')
            if (numTokens < 5) then
                call zoa_emit("MWL: usage: MWL Sk w1 w2 w3 (nm)", "red"); return
            end if
            do j = 1, 3
                if (.not. getReal(2+j, v(j))) return
            end do
            lm_store(k)%mwl = v
        case ('MCO')
            if (numTokens < 4 .or. tokens(3)(1:1) /= 'C') then
                call zoa_emit("MCO: usage: MCO Sk Cj value", "red"); return
            end if
            read(tokens(3)(2:), *, iostat=ios) j
            if (ios /= 0) j = -1
            if (j == 27) then
                call zoa_emit("MCO: C27 (pupil spherical aberration) is not supported yet", "red")
                return
            end if
            if (.not. any(j == [1, 2, 3, 6, 7, 8, 11, 12, 13, 14, 15])) then
                call zoa_emit("MCO: coefficients are C1-C3, C6-C8 (colour) and C11-C15", "red")
                return
            end if
            if (.not. getReal(4, v(1))) return
            lm_store(k)%c(j) = v(1)
        end select
        lm_warned = .false.
        call refreshLens()

    contains

        logical function getReal(i, x) result(ok)
            integer, intent(in) :: i
            real(real64), intent(out) :: x
            integer :: ios2
            ok = .false.
            x = 0.0_real64
            if (i > numTokens) then
                call zoa_emit(trim(word)//": missing value", "red"); return
            end if
            read(tokens(i), *, iostat=ios2) x
            if (ios2 /= 0) then
                call zoa_emit(trim(word)//": '"//trim(tokens(i))//"' is not a number", "red"); return
            end if
            ok = .true.
        end function

    end procedure execLensModule

    ! Re-run the paraxial trace (EOS) so first-order data and plots follow.
    subroutine refreshLens()
        call kdp_silent_begin()
        call kdp_lens_begin()
        call kdp_lens_end(refreshAll=.true.)
        call kdp_silent_end()
    end subroutine

    subroutine listModules()
        use lens_module, only: lm_store, LM_FIELD_IMG
        use mod_lens_data_manager, only: ldm
        use type_utils, only: int2str, real2str
        use kdp_utils, only: OUTKDP
        integer :: s, j
        logical :: found
        character(len=200) :: line
        found = .false.
        do s = 0, ldm%getLastSurf()
            if (.not. lm_store(s)%active) cycle
            found = .true.
            associate (p => lm_store(s))
            call OUTKDP('LENS MODULE S'//trim(int2str(s))//'-S'//trim(int2str(s+1)))
            line = '  MFL '//f6(p%efl)
            if (p%mff_set) line = trim(line)//'   MFF '//f6(p%mff)
            if (p%mbf_set) line = trim(line)//'   MBF '//f6(p%mbf)
            line = trim(line)//'   MEN '//f6(p%men)
            call OUTKDP(trim(line))
            if (p%mfd_kind == LM_FIELD_IMG) then
                line = '  MFD IMG '//f6(p%mfd)
            else
                line = '  MFD '//f6(p%mfd)//' deg'
            end if
            line = trim(line)//'   MED '//f6(p%med)// &
            &      '   MWL '//trim(real2str(p%mwl(1), 2))//' '//trim(real2str(p%mwl(2), 2))// &
            &      ' '//trim(real2str(p%mwl(3), 2))
            call OUTKDP(trim(line))
            line = '  MCO'
            do j = 1, 15
                if (p%c(j) /= 0.0_real64) line = trim(line)//'  C'//trim(int2str(j))//' '// &
                &                               f6(p%c(j))
            end do
            if (line == '  MCO') line = '  MCO (none)'
            call OUTKDP(trim(line))
            end associate
        end do
        if (.not. found) call OUTKDP('No lens modules')

    contains

        function f6(x) result(s)
            real(real64), intent(in) :: x
            character(len=:), allocatable :: s
            character(len=32) :: buf
            write(buf, '(F0.6)') x
            s = trim(adjustl(buf))
            if (s(1:1) == '.') s = '0'//s
            if (s(1:2) == '-.') s = '-0'//s(2:)
        end function
    end subroutine

end submodule mod_codev_lens_module
