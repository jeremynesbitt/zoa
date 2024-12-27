submodule (codeV_commands) mod_plot
implicit none
contains
module procedure setPlotWavelength
        
        use command_utils, only: isInputNumber

        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 
       
        !TODO:  Add error checking (min and max wavelength within range)
        if (numTokens  == 2) then
            if (isInputNumber(tokens(2))) then
                print *, "about to call psm update wv setting new ", trim(tokens(2))
                call curr_psm%updateWavelengthSetting(str2int(trim(tokens(2))))
                !call curr_psm%updateWavelengthSetting(str2int(tokens(2)))
                call LogTermFOR("Finished Updating Wv")
            end if
        end if
        
    end procedure
end submodule