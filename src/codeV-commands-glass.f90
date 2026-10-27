! CODE V glass catalog commands (Lens System Setup: Glass Catalog Data).
submodule (codeV_commands) mod_codev_glass
implicit none
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
            if (wl(j) < g%lmin .or. wl(j) > g%lmax) then
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

end submodule mod_codev_glass
