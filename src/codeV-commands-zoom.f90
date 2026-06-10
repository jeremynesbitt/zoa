submodule (codeV_commands) mod_codev_zoom
implicit none
contains

    ! ZOO            -> list the zoom table
    ! ZOO n          -> set the number of configurations
    ! ZOO <p> Sk v.. -> define a per-config operand (p = THI/RDY/CUY/GLA/K), one value per config
    ! ZOO PIM        -> per-config paraxial image solve (re-applied on each switch)
    module procedure execZOO
        use command_utils, only: parseCommandIntoTokens, isInputNumber
        use type_utils,    only: str2int
        use zoom_manager,  only: zoom_set_count, zoom_define_operand, zoom_list
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens, surf, nVals
        character(len=8) :: param

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        if (numTokens <= 1) then
            call zoom_list()
            return
        end if

        ! ZOO n : set number of configurations
        if (isInputNumber(trim(tokens(2)))) then
            call zoom_set_count(str2int(trim(tokens(2))))
            return
        end if

        param = trim(tokens(2))

        ! ZOO PIM : solve operand, no surface/values
        if (trim(param) == 'PIM') then
            call zoom_define_operand('PIM', 0, tokens(1:0), 0)
            return
        end if

        if (numTokens < 4) then
            call zoa_emit("Usage: ZOO <param> Sk v1 v2 ... vn   (or: ZOO n, ZOO PIM, ZOO)", "red")
            return
        end if
        if (.not. isSurfCommand(trim(tokens(3)))) then
            call zoa_emit("ZOO: expected a surface (e.g. S3), got '"//trim(tokens(3))//"'", "red")
            return
        end if
        surf  = getSurfNumFromSurfCommand(trim(tokens(3)))
        nVals = numTokens - 3
        call zoom_define_operand(trim(param), surf, tokens(4:numTokens), nVals)
    end procedure execZOO

    ! POS n -> switch the active configuration to n
    module procedure execPOS
        use command_utils, only: parseCommandIntoTokens, isInputNumber
        use type_utils,    only: str2int
        use zoom_manager,  only: zoom_switch
        implicit none
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens < 2 .or. .not. isInputNumber(trim(tokens(2)))) then
            call zoa_emit("Usage: POS <config number>", "red")
            return
        end if
        call zoom_switch(str2int(trim(tokens(2))))
    end procedure execPOS

end submodule mod_codev_zoom
