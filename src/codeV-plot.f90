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


    ! psuedocode for checking plot input
    ! if user entered PX
    !    check if PX plot exists already
    !    if no, create new psm
    ! if user did not enter PX
    !   create new default settings
    ! in all cases update cmd_loop to plot loop

    module procedure execVIE
        !use ui_spot, only: spot_struct_settings, spot_settings
       ! use mod_plotopticalsystem

        implicit none
        type(zoaplot_setting_manager) :: psm

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        call psm%initialize(trim(iptStr))
        cmd_loop = VIE_LOOP

        !call LogTermFOR("About ot check VIE for existing plot")
        if (numTokens  == 2) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, ID_PLOTTYPE_LENSDRAW)
            !ßcall LogTermFOR("VIE plot exists is "//bool2str(plotExists))
            ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
            ! here but don't have a better soultion right now
           if (plotExists) return
        end if
        call psm%addLensDrawSettings()
        ! Set up settings
        !call psm%addWavelengthSetting()
        !call psm%addDensitySetting(10, 8, 21)
        curr_psm = psm
    end procedure  

    module procedure execRMSPlot
        implicit none

        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        !call psm%addDensitySetting(64,8,128)
        !1call psm%addFieldSetting()
        call psm%addRMSFieldSettings()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_RMSFIELD, psm)
        if(boolResult .EQV. .FALSE.) then
            call updateTerminalLog("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if

    end procedure    

    module procedure execSeidelBarChart
        implicit none
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        !call psm%addDensitySetting(64,8,128)
        !1call psm%addFieldSetting()
        call psm%addWavelengthSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_SEIDEL, psm)
        if(boolResult .EQV. .FALSE.) then
            call updateTerminalLog("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if

    end procedure    
end submodule