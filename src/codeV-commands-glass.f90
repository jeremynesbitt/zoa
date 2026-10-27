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

end submodule mod_codev_glass
