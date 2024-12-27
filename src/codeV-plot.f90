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

    module procedure setPlotDensity
        
        use command_utils, only: isInputNumber
     
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 
       
        !TODO:  Add error checking (min and max wavelength within range)
        if (numTokens  == 2) then
            if (isInputNumber(tokens(2))) then
                call curr_psm%updateDensitySetting(str2int(tokens(2)))
                !call curr_psm%updateWavelengthSetting(str2int(tokens(2)))
                call LogTermFOR("Finished Updating Density")
            end if
        end if
        
    end procedure  
    module procedure setPlotZernikeCoefficients
        
        use command_utils, only: isInputNumber

        implicit none


        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        print *, "numTokens is ", numTokens
       
        !TODO:  Add error checking (min and max wavelength within range)
        if (numTokens  == 2) then
            if (.NOT.isInputNumber(tokens(2))) then
                call curr_psm%updateZernikeSetting(trim(tokens(2)))
                !call curr_psm%updateWavelengthSetting(str2int(tokens(2)))
                call LogTermFOR("Finished Updating Zernike")
            end if
        end if
        
    end procedure 

    module procedure ZERN_TST
        !use ui_spot, only: spot_struct_settings, spot_settings
       ! use mod_plotopticalsystem

        implicit none
        type(zoaplot_setting_manager) :: psm

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        call psm%initialize(trim(iptStr))
        cmd_loop = ZERN_LOOP

        if (numTokens  == 2) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, ID_PLOTTYPE_ZERN_VS_FIELD)
            ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
            ! here but don't have a better soultion right now
           if (plotExists) return
        end if

        ! Set up settings
        call psm%addWavelengthSetting()
        call psm%addDensitySetting(10, 8, 21)
        call psm%addZernikeSetting("5..9")

        curr_psm = psm


    end procedure 
end submodule