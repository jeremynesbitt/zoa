! CODE V glass catalog commands (Lens System Setup: Glass Catalog Data).
submodule (codeV_commands) mod_codev_glass
use mod_kdp_api, only: kdp_lens_begin, kdp_lens_end, kdp_silent_begin, kdp_silent_end
implicit none

! PRV ... END entry state: the PWL wavelengths (um) and glasses defined
real(real64) :: prvWl(21)
integer :: prvNWl = 0, prvNew = 0
contains

    !## cmd:      CSO
    !## syntax:   CSO [catalog ...]
    !## category: Glass Catalogs
    !## desc:     Sets the catalog search order: a glass named without its
    !##           catalog is looked up in these catalogs first, in this order
    !##           (the other catalogs follow alphabetically).  CSO alone
    !##           restores the default order (LIBGLA/catalog-order.txt);
    !##           CSO ? shows the current order.  The order lasts for the
    !##           session.
    !##
    module procedure execCSO
        use strings, only: parse
        use glass_catalog, only: gc_set_search_order, gc_find_catalog, &
                                 gc_catalog_name, gc_num_catalogs
        implicit none
        character(len=80) :: tokens(200)
        character(len=32) :: names(200)
        character(len=1024) :: line
        integer :: numTokens, i, ic

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 1) then
            call gc_set_search_order(names, 0)
        else if (trim(tokens(2)) /= '?') then
            do i = 2, numTokens
                ic = gc_find_catalog(trim(tokens(i)))
                if (ic == 0) then
                    call zoa_emit('CSO: no glass catalog '//trim(tokens(i))// &
                                  '; search order not changed', 'red')
                    return
                end if
                names(i-1) = gc_catalog_name(ic)     ! (an alias's catalog)
            end do
            call gc_set_search_order(names, numTokens-1)
        end if
        line = 'Catalog search order:'
        do ic = 1, gc_num_catalogs()
            line = trim(line)//' '//trim(gc_catalog_name(ic))
        end do
        call zoa_emit(trim(line), 'black')
    end procedure execCSO

    !## cmd:      GLI
    !## syntax:   GLI [catalog]
    !## category: Glass Catalogs
    !## desc:     Lists the catalog glasses' refractive indices at the lens
    !##           wavelengths, one line per glass ('-' outside a glass's
    !##           wavelength range).  Without a catalog, every catalog in
    !##           search order (CSO).
    !##
    module procedure execGLI
        call glassListing(iptStr, 'GLI')
    end procedure execGLI

    !## cmd:      TLI
    !## syntax:   TLI [catalog]
    !## category: Glass Catalogs
    !## desc:     Lists the catalog glasses' internal transmittance data
    !##           (wavelength, transmittance, sample thickness), as given in
    !##           the catalog file (AGF IT lines).
    !##
    module procedure execTLI
        call glassListing(iptStr, 'TLI')
    end procedure execTLI

    !## cmd:      MLI
    !## syntax:   MLI [catalog]
    !## category: Glass Catalogs
    !## desc:     Lists the catalog glasses' other data: glass code, nd, vd,
    !##           status, density, thermal expansion, partial dispersion
    !##           deviation, relative cost and the climate, stain, acid,
    !##           alkali and phosphate resistance codes ('-' where the
    !##           catalog gives none).
    !##
    module procedure execMLI
        call glassListing(iptStr, 'MLI')
    end procedure execMLI

    !## cmd:      GPR
    !## syntax:   GPR [catalog [glass]]
    !## category: Glass Catalogs
    !## desc:     Lists all data (GLI, TLI and MLI) for one glass, or for the
    !##           glasses of a catalog, or of every catalog.
    !##
    module procedure execGPR
        call glassListing(iptStr, 'GPR')
    end procedure execGPR

    ! GLI/TLI/MLI/GPR: the glasses named by the command, then each
    ! listing for them.
    subroutine glassListing(iptStr, what)
        use strings, only: parse
        use glass_catalog, only: gc_find_catalog, gc_find_glass, gc_num_catalogs, &
                                 gc_catalog_name, gc_catalog_size
        character(len=*), intent(in) :: iptStr, what
        character(len=80) :: tokens(10)
        integer :: numTokens, ic, ic1, ic2, ig, ig1, ig2

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 2 .and. what /= 'GPR') then
            call zoa_emit(what//' takes at most a catalog name', 'red')
            return
        end if
        if (numTokens > 3) then
            call zoa_emit('GPR takes at most a catalog and a glass name', 'red')
            return
        end if
        ic1 = 1; ic2 = gc_num_catalogs()
        if (numTokens >= 2) then
            ic1 = gc_find_catalog(trim(tokens(2)))
            if (ic1 == 0) then
                if (trim(tokens(2)) == 'PRV') then
                    call zoa_emit(what//': the lens has no private glasses (PRV)', 'red')
                    return
                end if
                call zoa_emit(what//': no glass catalog '//trim(tokens(2)), 'red')
                return
            end if
            ic2 = ic1
        end if
        if (numTokens == 3) then
            if (.not. gc_find_glass(ic1, trim(tokens(3)), ig1, byCode=.true.)) then
                call zoa_emit('GPR: no glass '//trim(tokens(3))//' in catalog '// &
                              trim(gc_catalog_name(ic1)), 'red')
                return
            end if
        end if

        if (what == 'GLI' .or. what == 'GPR') call indexHeader()
        do ic = ic1, ic2
            ig1 = 1; ig2 = gc_catalog_size(ic)
            if (numTokens == 3) then
                if (.not. gc_find_glass(ic, trim(tokens(3)), ig1, byCode=.true.)) cycle
                ig2 = ig1
            end if
            do ig = ig1, ig2
                select case (what)
                case ('GLI'); call indexLine(ic, ig)
                case ('TLI'); call transLines(ic, ig)
                case ('MLI'); call otherLine(ic, ig, ic == ic1 .and. ig == ig1)
                case ('GPR')
                    if (.not. (ic == ic1 .and. ig == ig1)) call indexHeader()
                    call indexLine(ic, ig)
                    call transLines(ic, ig)
                    call otherLine(ic, ig, .true.)
                end select
            end do
        end do
    end subroutine glassListing

    ! The lens wavelengths (um) with a nonzero weight.
    subroutine lensWavelengths(wl, n)
        use mod_system, only: sys_wavelength, sys_wl_weight
        real(real64), intent(out) :: wl(10)
        integer, intent(out) :: n
        integer :: j
        n = 0
        do j = 1, 10
            if (sys_wavelength(j) <= 0.0d0 .or. sys_wl_weight(j) == 0.0d0) cycle
            n = n + 1
            wl(n) = sys_wavelength(j)
        end do
    end subroutine lensWavelengths

    subroutine indexHeader()
        real(real64) :: wl(10)
        integer :: n, j
        character(len=400) :: line
        character(len=14) :: col
        call lensWavelengths(wl, n)
        line = 'CATALOG    GLASS                CODE'
        do j = 1, n
            write(col, '(F12.2,A)') wl(j)*1000.0d0, 'nm'
            line(45+14*(j-1):) = col
        end do
        call zoa_emit(trim(line), 'black')
    end subroutine indexHeader

    subroutine indexLine(ic, ig)
        use glass_catalog, only: glass_record, gc_glass, gc_index, gc_catalog_name
        integer, intent(in) :: ic, ig
        type(glass_record) :: g
        real(real64) :: wl(10)
        integer :: n, j
        character(len=400) :: line
        character(len=14) :: col
        g = gc_glass(ic, ig)
        call lensWavelengths(wl, n)
        line = leftCols(gc_catalog_name(ic), g%name, g%code)
        do j = 1, n
            if (wl(j) < g%lmin - 1.0d-9 .or. wl(j) > g%lmax + 1.0d-9) then
                col = repeat(' ', 13)//'-'
            else
                write(col, '(F14.6)') gc_index(ic, ig, wl(j))
            end if
            line(45+14*(j-1):) = col
        end do
        call zoa_emit(trim(line), 'black')
    end subroutine indexLine

    subroutine transLines(ic, ig)
        use glass_catalog, only: glass_record, gc_glass, gc_catalog_name
        integer, intent(in) :: ic, ig
        type(glass_record) :: g
        character(len=200) :: line
        integer :: k
        g = gc_glass(ic, ig)
        line = trim(gc_catalog_name(ic))//' '//trim(g%name)// &
               ': internal transmittance, '//trim(int2str(g%nit))//' points'
        call zoa_emit(trim(line), 'black')
        if (g%nit == 0) return
        call zoa_emit('  WAVELENGTH_UM   TRANSMITTANCE   THICKNESS_MM', 'black')
        do k = 1, g%nit
            write(line, '(F15.4,F16.4,F15.2)') g%it(1,k), g%it(2,k), g%it(3,k)
            call zoa_emit(trim(line), 'black')
        end do
    end subroutine transLines

    subroutine otherLine(ic, ig, header)
        use glass_catalog, only: glass_record, gc_glass, gc_catalog_name
        integer, intent(in) :: ic, ig
        logical, intent(in) :: header
        type(glass_record) :: g
        character(len=400) :: line
        character(len=10) :: st
        integer :: k
        character(len=9), parameter :: STATUS(0:4) = &
            [character(len=9) :: 'standard', 'preferred', 'obsolete', 'special', 'melt']
        if (header) then
            line = 'CATALOG    GLASS                CODE'
            line(45:) = '        ND       VD  STATUS       DENSITY  TCE-30/70     DPGF'// &
                        '    COST  CR  FR  SR  AR  PR'
            call zoa_emit(trim(line), 'black')
        end if
        g = gc_glass(ic, ig)
        st = '-'
        if (g%status >= 0 .and. g%status <= 4) st = STATUS(g%status)
        line = leftCols(gc_catalog_name(ic), g%name, g%code)
        line(45:) = num(g%nd, '(F10.6)', g%nd > 0.0d0)//num(g%vd, '(F9.3)', g%vd > 0.0d0)// &
               '  '//st//num(g%ed(3), '(F10.3)', g%ed(3) > 0.0d0)// &
               num(g%ed(1), '(F11.2)', g%ed(1) /= 0.0d0)// &
               num(g%ed(4), '(F9.4)', g%ed(4) /= 0.0d0)// &
               num(g%od(1), '(F8.2)', g%od(1) >= 0.0d0)
        do k = 2, 6
            line = trim(line)//num(g%od(k), '(I4)', g%od(k) >= 0.0d0)
        end do
        call zoa_emit(trim(line), 'black')
    end subroutine otherLine

    function leftCols(cat, name, code) result(s)
        character(len=*), intent(in) :: cat, name, code
        character(len=44) :: s
        character(len=11) :: c
        character(len=21) :: n
        character(len=12) :: k
        c = cat; n = name; k = code
        s = c//n//k
    end function leftCols

    ! x written with fmt (an I format rounds it), or a '-' right-aligned in
    ! the same width when the catalog gives no value
    function num(x, fmt, known) result(s)
        real(real64), intent(in) :: x
        character(len=*), intent(in) :: fmt
        logical, intent(in) :: known
        character(len=:), allocatable :: s
        character(len=32) :: t
        integer :: w
        if (fmt(2:2) == 'I') then
            write(t, fmt) nint(x)
        else
            write(t, fmt) x
        end if
        w = len_trim(t)
        if (.not. known) t = repeat(' ', w - 1)//'-'
        s = t(1:w)
    end function num

    !## cmd:      PRV
    !## syntax:   PRV
    !## category: Glass Catalogs
    !## desc:     Starts private glass entries, which belong to the lens and
    !##           are found before any catalog glass of the same name:
    !##             PWL wavelength_nm ...       wavelengths for the next glasses
    !##             'label' index ...           a glass's index at each PWL wavelength
    !##             END                         ends the entries
    !##           One wavelength gives a constant index; more give a cubic
    !##           spline over their range.  DEL PRV ALL|'label' removes them.
    !##
    module procedure execPRV
        use strings, only: parse
        implicit none
        character(len=80) :: tokens(4)
        integer :: numTokens
        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 1) then
            call zoa_emit('PRV takes no input; the glasses follow on the next lines, then END', 'red')
            return
        end if
        prvNWl = 0
        prvNew = 0
        cmd_loop = PRV_LOOP
    end procedure execPRV

    ! One line inside PRV ... END.
    module procedure prvLine
        use strings, only: parse
        use glass_catalog, only: gc_prv_define
        implicit none
        character(len=80) :: tokens(30)
        character(len=80) :: label
        integer :: numTokens, i, ios, L
        real(real64) :: v(21)

        call parse(trim(iptStr), ' ,', tokens, numTokens)
        if (numTokens < 1) return
        select case (trim(tokens(1)))
        case ('END')
            cmd_loop = 0
            call zoa_emit('PRV: '//trim(int2str(prvNew))//' private glass(es) defined', 'black')
            call refreshLensGlasses()
            return
        case ('PWL')
            if (numTokens < 2 .or. numTokens > 22) then
                call zoa_emit('PWL takes 1 to 21 wavelengths (nm)', 'red')
                return
            end if
            do i = 2, numTokens
                read(tokens(i), *, iostat=ios) v(i-1)
                if (ios /= 0 .or. v(i-1) <= 0.0d0) then
                    call zoa_emit('PWL: '//trim(tokens(i))//' is not a wavelength (nm)', 'red')
                    return
                end if
            end do
            prvNWl = numTokens - 1
            prvWl(1:prvNWl) = v(1:prvNWl)/1000.0d0
            return
        end select

        ! 'label' index ... (one index per PWL wavelength)
        label = tokens(1)
        L = len_trim(label)
        if (L >= 2 .and. label(1:1) == "'" .and. label(L:L) == "'") label = label(2:L-1)
        L = len_trim(label)
        if (L == 0 .or. L > 13) then
            call zoa_emit('PRV: a glass label is 1 to 13 characters: '//trim(tokens(1)), 'red')
            return
        end if
        if (numTokens >= 2) then
            select case (trim(tokens(2)))
            case ('MLT', 'LAU', 'GML', 'GMS', 'SLM', 'CAU', 'HAR')
                call zoa_emit('PRV: '//trim(tokens(2))//' glass definitions are not supported '// &
                              'yet; give the indices at the PWL wavelengths', 'red')
                return
            end select
        end if
        if (prvNWl == 0) then
            call zoa_emit('PRV: give the wavelengths (PWL) before '//trim(label), 'red')
            return
        end if
        if (numTokens - 1 /= prvNWl) then
            call zoa_emit('PRV: '//trim(label)//' needs '//trim(int2str(prvNWl))// &
                          ' indices, one per PWL wavelength', 'red')
            return
        end if
        do i = 2, numTokens
            read(tokens(i), *, iostat=ios) v(i-1)
            if (ios /= 0 .or. v(i-1) <= 0.0d0) then
                call zoa_emit('PRV: '//trim(tokens(i))//' is not an index', 'red')
                return
            end if
        end do
        call gc_prv_define(trim(label), prvWl(1:prvNWl), v(1:prvNWl), prvNWl)
        prvNew = prvNew + 1
    end procedure prvLine

    ! A line of a PRV block in a CODE V file being imported (CV2PRG): the
    ! PRV command's parser, without its command loop or the lens refresh.
    module procedure prvImportLine
        implicit none
        character(len=1024) :: w
        done = .false.
        w = adjustl(line)
        if (trim(w) == 'PRV') then
            prvNWl = 0
            prvNew = 0
        else if (trim(w) == 'END') then
            done = .true.
        else
            call prvLine(trim(w))
        end if
    end procedure prvImportLine

    ! DEL PRV ALL | DEL PRV 'label'
    module procedure prvDelete
        use strings, only: parse
        use glass_catalog, only: gc_prv_clear, gc_prv_delete
        use DATLEN, only: GLANAM
        use mod_system, only: sys_last_surf
        implicit none
        character(len=80) :: tokens(4), label
        integer :: numTokens, L, i
        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens < 3) then
            call zoa_emit('DEL PRV needs ALL or a glass label', 'red')
            return
        end if
        if (trim(tokens(3)) == 'ALL') then
            label = ''
            call gc_prv_clear()
        else
            label = tokens(3)
            L = len_trim(label)
            if (L >= 2 .and. label(1:1) == "'" .and. label(L:L) == "'") label = label(2:L-1)
            if (.not. gc_prv_delete(trim(label))) then
                call zoa_emit('DEL PRV: no private glass '//trim(label), 'red')
                return
            end if
        end if
        ! surfaces that used a deleted glass are left without one
        do i = 0, int(sys_last_surf())
            if (GLANAM(i,1) /= 'PRV') cycle
            if (len_trim(label) > 0 .and. GLANAM(i,2) /= label) cycle
            call zoa_emit('DEL PRV: surface S'//trim(int2str(i))//' used '// &
                trim(GLANAM(i,2))//'; its indices are now 1.0', 'red')
        end do
        call refreshLensGlasses()
    end procedure prvDelete


    ! Recompute the lens's indices after its private glasses changed.
    subroutine refreshLensGlasses()
        use DATMAI, only: F1, F22
        if (F1 /= 1) return
        call kdp_silent_begin()
        call kdp_lens_begin()
        F22 = 1                     ! re-resolve every surface's glass at EOS
        call kdp_lens_end()
        call kdp_silent_end()
    end subroutine refreshLensGlasses

end submodule mod_codev_glass
