! THis module contains Code V style commands and translates them to KDP style commands
! TODO:
! Convert all subs here to new implemenation that accepts string input and zoa_cmd
! Add useful things to zoa_cmd.  Some ideas
! An identifier to describe the type of command (or commands) it is.  FOr example,
! a lens update command or a plot command or a command that updates a plot
! Or adding a description for help that explains the command
! or an identifier that explains the type of command it is for error checking (eg 
! command should be of format CMD Sk ACTION)
! Other items
! Should redirect output from UI to somewhere else (fake element) when calling KDP cmds
! from codeV cmds.  For example, if the user enters in THI SO 125, in the ui will see
! U L; CHO 0; TH 125; EOS.  While this is helpful for developers, not so for regular users
! So add some logic that if not in debug mode, in exec codeV cmd, start by redirect output to 
! fake textView, print code v cmd, then when done redirect output to window

module codeV_commands
    use zoa_ui
    use iso_fortran_env, only: real64
    use plot_setting_manager, only: zoaplot_setting_manager
    use type_utils
    use handlers, only: updateTerminalLog, zoatabMgr, my_window
    use strings
    
    implicit none
   !ifort will not compile the common interfaces unless I do it this way
    ! I had wanted to do a module with interfaces only.  gfortran was okay with it
    ! but not ifort.  
    include "codeV-interfaces.INC"


    type zoa_cmd
      character(len=8) :: cmd
      procedure (cmdImplementation), pointer, nopass :: execFunc

    end type

    abstract interface
    subroutine cmdImplementation (iptStr)
       import zoa_cmd
       !class(zoa_cmd) :: self
       character(len=*) ::iptStr
       !real, intent (in) :: z
    end subroutine cmdImplementation 
 end interface    


    character(len=4), dimension(500) :: surfCmds
    type(zoa_cmd), dimension(601) :: zoaCmds

    type(zoaplot_setting_manager)  :: curr_psm
    character(len=10024) :: cmdTOW

    integer :: cmd_loop = 0
    integer, parameter :: VIE_LOOP = 1
    integer, parameter :: DRAW_LOOP = 2 ! While plot is being drawn
    integer, parameter :: SPO_LOOP = 3
    integer, parameter :: ZERN_LOOP = 4
    integer, parameter :: TOW_LOOP = 5
    integer, parameter :: AUT_LOOP = 6
    integer, parameter :: TAR_LOOP = 7


    contains

    subroutine initializeCmds()
        ! This is called when the program is initialized (currently INITKDP.FOR)
        use iso_c_binding, only: c_null_ptr
        use global_widgets, only: ioConfig
        use hl_gtk_zoa, only : hl_zoa_text_view_new

        integer :: i
        character(len=1), dimension(8) :: evenAsphereTerms = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']

        ! Initialize null ptr textView to dump KDP print statements when needed
        call ioConfig%registerTextView(c_null_ptr, ID_TERMINAL_KDPDUMP)

        zoaCmds(1)%cmd = "RMD"
        zoaCmds(1)%execFunc => execRMD
        !zoaCmds(2)%cmd = 'WL'
        !zoaCmds(2)%execFunc => setWavelength
        do i = 1,499
            zoaCmds(i)%cmd = 'S'//trim(int2str(i))
            zoaCmds(i)%execFunc => setSurfaceCodeVStyle    
        end do        
        zoaCmds(500)%cmd = "SAV"
        zoaCmds(500)%execFunc => execSAV
        zoaCmds(501)%cmd = "REF"
        zoaCmds(501)%execFunc => execSetWavelengthIndex  
        zoaCmds(502)%cmd = "RES"
        zoaCmds(502)%execFunc => execRestore 
        zoaCmds(503)%cmd = "VIE"
        zoaCmds(503)%execFunc => execVie    
        zoaCmds(504)%cmd = "SUR"
        zoaCmds(504)%execFunc => execSUR                               
        zoaCmds(505)%cmd = "STO"
        zoaCmds(505)%execFunc => execSTO    
        zoaCmds(506)%cmd = "S"
        zoaCmds(506)%execFunc => setSurfaceCodeVStyle    
        zoaCmds(507)%cmd = "SO"
        zoaCmds(507)%execFunc => setSurfaceCodeVStyle   
        zoaCmds(508)%cmd = 'WL'
        zoaCmds(508)%execFunc => setWavelength    
        zoaCmds(509)%cmd = 'STOP'
        zoaCmds(509)%execFunc => execSTO  
        zoaCmds(510)%cmd = 'YAN'
        zoaCmds(510)%execFunc => setField
        zoaCmds(511)%cmd = 'XAN'
        zoaCmds(511)%execFunc => setField                                    
        zoaCmds(512)%cmd = 'CIR'
        zoaCmds(512)%execFunc => execCIR  
        zoaCmds(513)%cmd = 'SI'
        zoaCmds(513)%execFunc => setSurfaceCodeVStyle 
        zoaCmds(514)%cmd = 'WTW'
        zoaCmds(514)%execFunc => setWavelengthWeights          
        zoaCmds(515)%cmd = 'WTF'
        zoaCmds(515)%execFunc => setFieldWeights
        zoaCmds(516)%cmd = 'YOB'
        zoaCmds(516)%execFunc => setField
        zoaCmds(517)%cmd = 'XOB'
        zoaCmds(517)%execFunc => setField     
        zoaCmds(518)%cmd = 'INS'
        zoaCmds(518)%execFunc => insertSurf                         
        zoaCmds(519)%cmd = 'FAN'
        zoaCmds(519)%execFunc => execFAN        
        zoaCmds(520)%cmd = 'RSI'
        zoaCmds(520)%execFunc => execRSI         
        zoaCmds(521)%cmd = 'NBR'
        zoaCmds(521)%execFunc => execNBR      
        zoaCmds(522)%cmd = 'FIO'
        zoaCmds(522)%execFunc => execFIO   
        zoaCmds(523)%cmd = 'BES'
        zoaCmds(523)%execFunc => findBestFocus     
        zoaCmds(524)%cmd = 'SPO'
        zoaCmds(524)%execFunc => execSPO   
        zoaCmds(525)%cmd = 'ZERN_TST'
        zoaCmds(525)%execFunc => ZERN_TST     
        zoaCmds(526)%cmd = 'SETWV'
        zoaCmds(526)%execFunc => setPlotWavelength 
        zoaCmds(527)%cmd = 'SETDENS'
        zoaCmds(527)%execFunc => setPlotDensity     
        zoaCmds(528)%cmd = 'SETZERNC'
        zoaCmds(528)%execFunc => setPlotZernikeCoefficients    
        zoaCmds(529)%cmd = 'TOW'
        zoaCmds(529)%execFunc => execTOW   
        zoaCmds(530)%cmd = 'FIE'
        zoaCmds(530)%execFunc => execAstigFieldCurvDistPlot                              
        zoaCmds(531)%cmd = 'PMA'
        zoaCmds(531)%execFunc => execPMAPlot                              
        zoaCmds(532)%cmd = 'SSI'
        zoaCmds(532)%execFunc => setPlotScale     
        zoaCmds(533)%cmd = 'RIM'
        zoaCmds(533)%execFunc => execRayAberrationPlot                                    
        zoaCmds(534)%cmd = 'THO'
        zoaCmds(534)%execFunc => execSeidelBarChart
        zoaCmds(535)%cmd = 'PLTRMS'
        zoaCmds(535)%execFunc => execRMSPlot
        zoaCmds(536)%cmd = 'XOFF'
        zoaCmds(536)%execFunc => execXOFF
        zoaCmds(537)%cmd = 'YOFF'
        zoaCmds(537)%execFunc => execYOFF                
        zoaCmds(538)%cmd = 'SAVESESS'
        zoaCmds(538)%execFunc => execSaveSessionToFile                                       
        zoaCmds(539)%cmd = 'CLI'
        zoaCmds(539)%execFunc => execCLI              
        zoaCmds(540)%cmd = 'YIM'
        zoaCmds(540)%execFunc => setField        
        zoaCmds(541)%cmd = 'IND'
        zoaCmds(541)%execFunc => printRefractiveIndices  
        zoaCmds(542)%cmd = 'AUT'
        zoaCmds(542)%execFunc => execAUT 
        !zoaCmds(543)%cmd = 'FRZ'
        !zoaCmds(543)%execFunc => freezeParams 
        zoaCmds(544)%cmd = 'THC'
        zoaCmds(544)%execFunc => updateVarCodes    
        zoaCmds(545)%cmd = 'CCY'
        zoaCmds(545)%execFunc => updateVarCodes           
        zoaCmds(546)%cmd = 'EFL'
        zoaCmds(546)%execFunc => updateEFLConstraint                         
        zoaCmds(547)%cmd = 'TAR'
        zoaCmds(547)%execFunc => execTAR     
        zoaCmds(548)%cmd = 'FRZ'
        zoaCmds(548)%execFunc => execFreeze    
        zoaCmds(549)%cmd = 'TCO'
        zoaCmds(549)%execFunc => updateTCOConstraint   
        zoaCmds(550)%cmd = 'FLY'
        zoaCmds(550)%execFunc => flipSurfaces    
        zoaCmds(551)%cmd = 'SCA'
        zoaCmds(551)%execFunc => scaleSystem    
        zoaCmds(552)%cmd = 'PIK'
        zoaCmds(552)%execFunc => parsePickupInput 
        zoaCmds(553)%cmd = 'RED'
        zoaCmds(553)%execFunc => setMagSolve           
        zoaCmds(554)%cmd = 'TAS'
        zoaCmds(554)%execFunc => updateTCOConstraint     
        zoaCmds(555)%cmd = 'IMC'
        zoaCmds(555)%execFunc => updateConstraint    
        zoaCmds(556)%cmd = 'PTB'
        zoaCmds(556)%execFunc => updateConstraint   
        zoaCmds(557)%cmd = 'PRT'
        zoaCmds(557)%execFunc => printFile    
        zoaCmds(558)%cmd = 'THI'
        zoaCmds(558)%execFunc => setThickness
        zoaCmds(559)%cmd = 'KC' ! TODO:  Verify this is the correct value
        zoaCmds(559)%execFunc => updateVarCodes                            
        zoaCmds(560)%cmd = 'GLA'
        zoaCmds(560)%execFunc => setGlass
        zoaCmds(561)%cmd = 'CUY'
        zoaCmds(561)%execFunc => setCurvature  
        zoaCmds(562)%cmd = 'DEL'
        zoaCmds(562)%execFunc => deleteStuff   
        zoaCmds(563)%cmd = 'RDY'
        zoaCmds(563)%execFunc => setRadius    
        zoaCmds(564)%cmd = 'TIT'
        zoaCmds(564)%execFunc => setLensTitle
        zoaCmds(565)%cmd = 'DIM'
        zoaCmds(565)%execFunc => setDim                        
        zoaCmds(566)%cmd = 'PSF'
        zoaCmds(566)%execFunc => execPSF    
        zoaCmds(567)%cmd = 'MTF'
        zoaCmds(567)%execFunc => execMTF       
        zoaCmds(568)%cmd = 'MFR'
        zoaCmds(568)%execFunc => updateMaxFrequency         
        zoaCmds(569)%cmd = 'IFR'
        zoaCmds(569)%execFunc => updateFrequencyInterval                             
        zoaCmds(570)%cmd = 'CY'
        zoaCmds(570)%execFunc => getRayData    
        zoaCmds(571)%cmd = 'CX'
        zoaCmds(571)%execFunc => getRayData    
        zoaCmds(572)%cmd = 'ASP'
        zoaCmds(572)%execFunc => execAsphere                                            
        do i=1,8
            zoaCmds(572+i)%cmd      = evenAsphereTerms(i)
            zoaCmds(572+i)%execFunc => updateAsphereTerms
        end do        
        zoaCmds(581)%cmd = 'K'          
        zoaCmds(581)%execFunc => updateConicConstant  
        zoaCmds(582)%cmd = '!'          
        zoaCmds(582)%execFunc => processFileComment          
        zoaCmds(583)%cmd = 'ZOA2CV'
        zoaCmds(583)%execFunc => exportLensToCodeV  
        zoaCmds(584)%cmd = 'ZOA2ZMX'
        zoaCmds(584)%execFunc => exportLensToZemax           

        
    end subroutine

    function startCodeVLensUpdateCmd(iptCmd) result(boolResult)
        use GLOBALS, only:  currentCommand

        character(len=*) :: iptCmd
        integer :: ii
        logical :: boolResult

        boolResult = .FALSE.

    
        do ii=1,size(zoaCmds)
        if (iptCmd == zoaCmds(ii)%cmd) then
            if (cmd_loop == TOW_LOOP .AND. iptCmd /= 'GO') then
                if (len(trim(cmdTOW)) == 0) then
                    cmdTOW = currentCommand
                else
                    cmdTOW = trim(cmdTOW)//'; '//currentCommand
                end if
                boolResult = .TRUE.
                return
            end if
                
            !PRINT *, "About to crash with fcn pointer?"
            call zoaCmds(ii)%execFunc(currentCommand)
            boolResult = .TRUE.
            return
        end if
        end do

        select case (iptCmd)
           
        case('GO')
            CALL executeGo()
            boolResult = .TRUE.
            return
         
        case ('PIM')
            call setParaxialImageSolve()
            boolResult = .TRUE.
            return     
        case ('EPD')
            call setEPD()
            boolResult = .TRUE.
            return   
        ! case ('CUY')
        !     call setCurvature()
        !     boolResult = .TRUE.
        !     return             
            
        case ('SETC')
            call execSetCodeVCmd()
            boolResult = .TRUE.
            return              

        end select

        ! Handle Sk separately
        ! IF(isSurfCommand(iptCmd)) then
        !     CALL setSurfaceCodeVStyle(iptCmd)
        !     return
        !   END IF            
              
    end function
     
    ! Would like to move these cmd_parser to command-utils, but there is a circulat
    ! dependency due to updateTerminalLog I need to solve.  Sigh.
    subroutine cmd_parser_get_real_pair(tokens, real1, real2, real1Bounds, real2Bounds) 
    ! Look for 2 and only 2 inputs that don't have a string character in them    
        use command_utils, only: isInputNumber
        implicit none
        character(len=*), dimension(:) :: tokens
        real(kind=real64) :: real1, real2
        real(kind=real64) :: tmpVal
        real, dimension(2), optional :: real1Bounds
        real, dimension(2), optional :: real2Bounds
        integer :: i, numRealInputs 

        numRealInputs = 0

        if(size(tokens) > 1) then
            do i=2,size(tokens)
                if (isInputNumber(tokens(i))) then
                    numRealInputs = numRealInputs + 1
                    select case(numRealInputs)
                    case (1)
                        if (present(real1Bounds)) then
                            tmpVal = str2real8(tokens(i))
                            if (tmpVal >= real1Bounds(1) .AND. tmpVal <= real1Bounds(2)) then
                                real1 = tmpVal
                            else
                                call updateTerminalLog("Error:  Real 1 Input Outside Bounds "//trim(tokens(i)), "red")
                                return
                            end if
                        else
                            real1 = str2real8(tokens(i))
                        end if

                    case (2)
                        if (present(real2Bounds)) then
                            tmpVal = str2real8(tokens(i))
                            if (tmpVal >= real1Bounds(1) .AND. tmpVal <= real1Bounds(2)) then
                                real2 = tmpVal
                            else
                                call updateTerminalLog("Error:  Real 2 Input Outside Bounds "//trim(tokens(i)), "red")
                                return
                            end if
                        else
                            real2 = str2real8(tokens(i))
                        end if                        
                      
                    case default
                        call updateTerminalLog("Warning:  Detected more than two valid inputs.  Ignoring "//trim(tokens(i)), "red")

                    end select
                end if
            end do
        end if        

    
    end subroutine

    function cmd_parser_get_integer_range(iptStr, intArr) result(goodResult)
        use command_utils, only: isInputNumber
        implicit none
        logical :: goodResult
        character(len=*) :: iptStr
        integer, allocatable :: intArr(:)
        integer :: i, s0, sf, dotLoc

        goodResult = .FALSE.
        PRINT *, "CHecking input string ", iptStr
        ! Check for ellipsis
        dotLoc = index(iptStr,'..') 
        if(dotLoc > 0) then
            ! Require input to be i..k

            if(isInputNumber(iptStr(2:dotLoc-1)).AND. &
            &  isInputNumber(iptStr(dotLoc+2:len(iptStr)))) then
               goodResult = .TRUE.
               s0 = str2int(iptStr(1:dotLoc-1))
               sf = str2int(iptStr(dotLoc+2:len(iptStr)))
               intArr = (/ (i,i=s0,sf)/)
            else
                call updateTerminalLog("Error:  Could not convert input to X..Y "//iptStr, "red")
            end if
        else ! No dots found
            if(isInputNumber(iptStr)) then
            goodResult = .TRUE.
            allocate(intArr(1))
            intArr(1) = str2int(iptStr)
            end if
        end if


    end function
    

    function cmd_parser_get_int_input_for_prefix(prefix, tokens) result(intArr)
        use global_widgets, only: sysConfig
        use command_utils, only: isInputNumber

        implicit none
        character(len=*) :: prefix
        character(len=*), dimension(:) :: tokens
        integer, allocatable :: intArr(:)
        integer :: i
        logical :: userDefined 

        userDefined = .FALSE.
        
        if(size(tokens) > 1) then
            do i=2,size(tokens)
                if (uppercase(tokens(i)(1:len(prefix))) == uppercase(prefix) ) then
                    userDefined = cmd_parser_get_integer_range(tokens(i)(2:len(tokens(i))), intArr)

                    !if (isInputNumber(tokens(i)(2:len(tokens)))) then
                    !    userDefined = .TRUE.
                    !    allocate(fields(1))
                    !    fields = str2int(tokens(i)(2:len(tokens)))
                    !end if
                end if
                !call LogTermFOR("Tokens "// tokens(i))

            end do
        end if

        !Default is all fields
        if (.NOT.userDefined) then

            select case (prefix)

            case ('f')
                intArr =  (/ (i,i=1,sysConfig%numFields)/)
            case ('w')
                intArr =  (/ (i,i=1,sysConfig%numWavelengths)/)
            case default
                allocate(intArr(1))
                intArr(1) = 0

            end select
        end if




    end function

    subroutine execXOFF(iptStr)
        use command_utils, only: isInputNumber

        implicit none
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens
        INTEGER VIEXOF,VIEYOF, VIEROT
  
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT


        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens==2) then
            if (isInputNumber(tokens(2))) then
                VIEXOF = INT(str2real8(tokens(2)))
            end if
        end if

    end subroutine

    subroutine execYOFF(iptStr)
        use command_utils, only: isInputNumber

        implicit none
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens
        INTEGER VIEXOF,VIEYOF, VIEROT
  
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT


        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens==2) then
            if (isInputNumber(tokens(2))) then
                VIEYOF =  INT(str2real8(tokens(2)))
            end if
        end if

    end subroutine

    subroutine execFreeze(iptStr)
        
        use optim_types, only: optim
        

        implicit none

        character(len=*) :: iptStr     
        character(len=80) :: tokens(40)
        integer :: numTokens
        
        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if(numTokens == 2) then
            if (tokens(2) == 'SA') then
                call optim%freezeAllSurfaces()
            end if
            if (tokens(2) == 'CON') then
                call optim%removeAllConstraints()
            end if

        else
            call updateTerminalLog("Error!  FRZ must include a qualifier, such as SA to freeze control variables", "red")
        end if
        

    end subroutine

    ! This is a very important command that performs ray traces for a user defined
    ! number of field points, wavelengths, and relative aperture locations
    ! The output depends on the cmd level
    ! For normal operation a table will be output with the results
    ! If in VIE level it will save the rays intersection coordinates for use with plooting
    ! Format RSI fi..k wi..k relApeX relApeY
    subroutine execRSI(iptStr)
        use global_widgets, only: sysConfig
        use command_utils, only : parseCommandIntoTokens, isInputNumber

        implicit none        

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        integer, allocatable :: fields(:), wavelengths(:)
        real(kind=real64) :: relApeX, relApeY
        integer :: i, j
        
        ! Defaults
        relApeX = 0.0
        relApeY = 0.0

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        
        fields = cmd_parser_get_int_input_for_prefix('f', tokens(1:numTokens))
        wavelengths = cmd_parser_get_int_input_for_prefix('w', tokens(1:numTokens))
        call cmd_parser_get_real_pair(tokens(1:numTokens), relApeX, relApeY, &
        & real1Bounds=[-1.0,1.0], real2Bounds=[-1.0,1.0])
        !fields = cmd_parser_get_fields(tokens(1:numTokens))
        do i=1,size(fields)
            call LogTermFOR("RSI fields are "//int2str(fields(i)))

        end do
        do i=1,size(wavelengths)
            call LogTermFOR("RSI wavelengths are "//int2str(wavelengths(i)))
        end do

        call LogTermFOR("Relative X Aperture is " // real2str(relApeX))
        call LogTermFOR("Relative Y Aperture is " // real2str(relApeY))


        ! Now that we have inputs, trace all rays needed.
        do i = 1,size(wavelengths)
        do j = 1,size(fields)
            !SAVE_KDP(1)=SAVEINPT(1)
            
            ! TODO:  Put COLRAY value into sysConfig (current field Color)
            !COLRAY = sysConfig%fieldColorCodes(fields(j))
            !WRITE(INPUT, *) "FOB ", )
            CALL PROCESKDP("FOB "//trim(real2str(sysConfig%relativeFields(2,fields(j)))))
            !REST_KDP(1)=RESTINPT(1)
            
  
      ! Add loop for wavelengths      
      ! By definition RSI only traces a ray of a single angle
            !SAVE_KDP(1)=SAVEINPT(1)
            CALL PROCESKDP("RAY "//real2str(relApeY)// &
            & " "//real2str(relApeX)//" "//int2str(wavelengths(i)))      
            CALL PROCESKDP("PRXYZ ALL")
            !REST_KDP(1)=RESTINPT(1)
    end do ! fields
   end do  !  wavelengths        

        
    end subroutine

    subroutine execFIO(iptStr)
        use GLOBALS, only: long
        use global_widgets, only: curr_par_ray_trace, curr_lens_data, sysConfig
        use kdp_utils

        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        character(len=3), dimension(4) :: colHeaders
        real(kind=long), dimension(4,curr_lens_data%num_surfaces):: dataArray

        print *, "Size of dataArray is ", size(dataArray,1)
        dataArray(1,:) = curr_par_ray_trace%marginal_ray_height
        dataArray(2,:) = curr_par_ray_trace%marginal_ray_angle        
        dataArray(3,:) = curr_par_ray_trace%chief_ray_height
        dataArray(4,:) = curr_par_ray_trace%chief_ray_angle

        colHeaders(1) = "HMY"
        colHeaders(2) = "UMY"
        colHeaders(3) = "HCY"
        colHeaders(4) = "UCY"

        call OUTKDP('   '//trim(sysConfig%lensTitle))
        call logDataVsSurface(dataArray, colHeaders)
      
        ! call updateTerminalLog("UMY"//blankStr(5)//"HMY"//blankStr(5)//"UCY"//blankStr(5)//"HCY", "black")
        ! do i=1,curr_lens_data%num_surfaces
        !     call updateTerminalLog(trim(real2str(curr_par_ray_trace%marginal_ray_angle(i)))//blankStr(2)// &
        !     &                      trim(real2str(curr_par_ray_trace%marginal_ray_height(i)))//blankStr(2)// &
        !     &                      trim(real2str(curr_par_ray_trace%chief_ray_angle(i)))//blankStr(2)// &
        !     &                      trim(real2str(curr_par_ray_trace%chief_ray_height(i))), "black" )


        ! end do

    end subroutine

    subroutine printRefractiveIndices(iptStr)
        use GLOBALS, only: long
        use global_widgets, only: curr_par_ray_trace, curr_lens_data, sysConfig
        use kdp_utils
        use mod_lens_data_manager
 

        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        character(len=3), dimension(4) :: colHeaders
        real(kind=long), dimension(4,curr_lens_data%num_surfaces):: dataArray
        integer :: i,j
        character(len=1024) :: outStr

        print *, "Size of dataArray is ", size(dataArray,1)
        dataArray(1,:) = curr_par_ray_trace%marginal_ray_height
        dataArray(2,:) = curr_par_ray_trace%marginal_ray_angle        
        dataArray(3,:) = curr_par_ray_trace%chief_ray_height
        dataArray(4,:) = curr_par_ray_trace%chief_ray_angle

        colHeaders(1) = "HMY"
        colHeaders(2) = "UMY"
        colHeaders(3) = "HCY"
        colHeaders(4) = "UCY"

        call OUTKDP('   '//trim(sysConfig%lensTitle))
        call OUTKDP('REFRACTIVE INDICES')
        outStr = '   GLASS CODE'
        do j=1,sysConfig%numWavelengths
            outStr = trim(outStr)//blankStr(6)//real2str(1000.0*sysConfig%getWavelength(j),2)
        end do
        call OUTKDP(trim(outStr))
        do i=0,ldm%getLastSurf()
            if (ldm%isGlassSurf(i)) then
                outStr = blankStr(3)//ldm%getGlassName(i)
                do j=1,sysConfig%numWavelengths
                    outStr=trim(outStr)//blankStr(5)//real2str(ldm%getSurfIndex(i,j))
                end do
                call outKDP(trim(outStr))
            end if
        end do




    end subroutine



    ! Todo:  move this somewhere once it is properly written
    function adjustImageFocus(x) result(f)

       use DATMAI, only: REG
       implicit none
       double precision :: f
       double precision, intent(in):: x


       call PROCESKDP('THI SI '//real2str(x))
       call PROCESKDP('FOB 0; CAPFN; SHO RMSOPD')
       f = REG(9)


    end function


    subroutine findBestFocus(iptStr)
        use global_widgets, only: curr_par_ray_trace, sysConfig
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use DATLEN, only: COLRAY
        use algos
    
        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        real(kind=real64) :: relAngle
        double precision :: newThi, result
        integer :: numTokens, i, jj, numRays
        integer, allocatable :: fields(:)
        character(len=5) :: plotOrientation
        logical :: goodCmd, yzFlag

        call LogTermFOR("FindBestFocus plumbing works!")
        !call tstNewtonRaphson()
        call tstBrent()

        ! For now hard code image surface.  TODO:  Abstract this to other surfaces
        result = brent(-50*1d0, 0*1d0, 50*1d0, adjustImageFocus, 1E-6*1d0, newThi)
        call LogTermFOR("RMS at best image focus is "//real2str(result,3))
        call PROCESKDP('THI SI '//real2str(newThi))

        !do ii=1,sysConfig%numFields


        !end do
      
    end subroutine

    ! Options
    ! FAN AB N - N rays for all fields in AB Plane 
    ! FAN N  - N rays for all fields in YZ (default) plane 
    ! FAN AB N Fi - N Rayls for field i in AB plane
    ! TODO:  Fix this iwth new plot infra
    subroutine execFAN(iptStr)
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use global_widgets, only:  sysConfig
        use DATLEN, only: COLRAY
    
        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        real(kind=real64) :: relAngle
        integer :: numTokens, ii, jj, numRays
        integer, allocatable :: fields(:)
        character(len=5) :: plotOrientation
        logical :: goodCmd, yzFlag

        

        goodCmd = .FALSE.
        yzFlag = .TRUE.

        ! If user enters this during a VIE loop (in between VIE and GO)
        ! then add command list
        ! Then when draw is issued, it will come back here and translate
        ! entry to a series of RSI commands

        select case(cmd_loop)
        case(VIE_LOOP)
            ! Blindly add.  will check it for goodness later
              !goodCmd = .TRUE.
              !call LogTermFOR("Addint Custom Cmd "//iptStr)
              !call ld_settings%addCustomRayCmd(iptStr)
   
        case(DRAW_LOOP)
            call LogTermFOR("Executing Custom Cmd "//iptStr)
            ! Execute command.  Have to parse it to get options.
            call parse(trim(iptStr), ' ', tokens, numTokens) 
            ! TODO:  Is there a better way to parse this to reuse code?
            
            do ii=2,numTokens
                ! Assume bare number is rays.  TODO check limits
                if (isInputNumber(tokens(ii))) then
                    numRays = str2int(tokens(ii))
                    call LogTermFOR("Added numRays "//int2str(numRays))
                end if
                ! If there is a fi value, assume a single field point
                if (tokens(ii)(1:1) == 'f' .OR. tokens(ii)(1:1) == 'F') then
                    call LogTermFOR("Setting Fields from input "//tokens(ii))
                    if (isInputNumber(tokens(ii)(2:len(tokens)))) then
                        allocate(fields(1))
                        fields(1) = str2int(tokens(ii)(2:len(tokens)))
                        call LogTermFOR("Set field index to "//int2str(fields(1)))
                    end if
                end if
                ! Look for XZ or YZ to check which rays to plot
                if (tokens(ii) == 'YZ') yzFlag = .TRUE.
                if (tokens(ii)  == 'XZ') yzFlag = .FALSE.
            end do
                   if(.not.allocated(fields)) then
                    !Assume default of all fields
                    allocate(fields(sysConfig%numFields))
                       fields =  (/ (jj,jj=1,sysConfig%numFields)/)
                end if


                   ! Finally ready to set fields and plot.  TODO:  Convert to RSI command? 
                   do ii=1,size(fields)
                    COLRAY = sysConfig%fieldColorCodes(fields(ii))
                    call PROCESKDP("FOB "//real2str(sysConfig%relativeFields(2,fields(ii))) &
                    & //real2str(sysConfig%relativeFields(1,fields(ii))))    
                      do jj=1,numRays
                        relAngle = -1.0d0 + (jj-1)*(2.0d0)/(numRays-1)
                        if(yzFlag) then
                          !call VIE_NEW_TRACERAY(0.0d0, relAngle, sysConfig%refWavelengthIndex, ld_settings)
                        else
                          !  call VIE_NEW_TRACERAY(relAngle, 0.0d0,sysConfig%refWavelengthIndex, ld_settings)
                        end if

                      end do
                    end do
                case default
                    ! Do nothing
                call updateTerminalLog("FAN not used at base level", "black")
                end select


    end subroutine


    ! Started to wrtie this but it doesn't seem to simpify things too much
    function checkForExistingPlot(tokens, psm, plot_code) result (plotExists)

        implicit none

        logical :: plotExists
        character(len=*), dimension(:) :: tokens
        type(zoaplot_setting_manager), intent(inout) :: psm
        integer, intent(in) :: plot_code
        integer, allocatable :: plotNum(:) 
        integer :: objIdx

        plotExists = .FALSE.
        plotNum = cmd_parser_get_int_input_for_prefix('P', tokens)
        plotExists = zoatabMgr%doesPlotExist_new(plot_code, objIdx, plotNum(1))
        if (plotExists) then
            ! TODO:  Replace with abstract call
            curr_psm  = zoaTabMgr%tabInfo(objIdx)%tabObj%psm
            !cmd_loop = ZERN_LOOP       
            else
                psm%plotNum = plotNum(1)
        end if
        

    end function

    function initiatePlotLoop(iptStr, PLOT_CODE, psm) result(boolResult)

        implicit none

        logical :: boolResult
        integer :: PLOT_CODE
        character(len=*) :: iptStr
        type(zoaplot_setting_manager) :: psm
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        boolResult = .FALSE.
        call parse(trim(iptStr), ' ', tokens, numTokens) 

        
        

        if (numTokens  == 2 .AND. tokens(2)(1:1)=='P' ) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, PLOT_CODE)
            ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
            ! here but don't have a better soultion right now
           if (plotExists) then 
            cmd_loop = PLOT_CODE
            boolResult = .TRUE.
            return
           else ! User entered Px but it doesn't exist.  Ignore PX
            cmd_loop = PLOT_CODE
            boolResult = .TRUE.
            curr_psm = psm
            return
           end if
        end if
        if (numTokens==1) then
            cmd_loop = PLOT_CODE
            boolResult = .TRUE.
            curr_psm = psm
            return
        end if
        ! If we got down here then something went wrong, and boolresult should stay falsse
        

    end function

    subroutine setPlotScale(iptStr)

        use command_utils

        implicit none

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult

       

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if(numTokens ==2 ) then
            if (isInputNumber(tokens(2))) then

            select case(cmd_loop)
            case (ID_PLOTTYPE_RIM)
                call curr_psm%updateSetting(SETTING_SCALE, str2real8(tokens(2)))
            case (SPO_LOOP)
                call curr_psm%updateSetting(SETTING_SCALE, str2real8(tokens(2)))
            end select
        end if
        end if


        !Pseudocode
        ! if (cmd_loop == ID_PLOTTYPE_SPOT) then
        !  psm%setScaleInLensUnits(real(tokens(2)))

        !   end if

    end subroutine


    subroutine updateMaxFrequency(iptStr)

        use command_utils

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult
  

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if(numTokens ==2 ) then
            if (isInputNumber(tokens(2))) then

            select case(cmd_loop)
            case (ID_PLOTTYPE_MTF)
                call curr_psm%updateSetting(SETTING_MAX_FREQUENCY, str2real8(tokens(2)))
            case default
                call updateTerminalLog("Error:  &
                & This cmd must be entered after MTF and before GO to update value", "red")
            end select
        end if
        end if


        !Pseudocode
        ! if (cmd_loop == ID_PLOTTYPE_SPOT) then
        !  psm%setScaleInLensUnits(real(tokens(2)))

        !   end if

    end subroutine

    subroutine updateFrequencyInterval(iptStr)

        use command_utils

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult
  

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if(numTokens ==2 ) then
            if (isInputNumber(tokens(2))) then

            select case(cmd_loop)
            case (ID_PLOTTYPE_MTF)
                call curr_psm%updateSetting(SETTING_FREQUENCY_INTERVAL, str2real8(tokens(2)))
            case default
                call updateTerminalLog("Error:  &
                & This cmd must be entered after MTF and before GO to update value", "red")
            end select
        end if
        end if


        !Pseudocode
        ! if (cmd_loop == ID_PLOTTYPE_SPOT) then
        !  psm%setScaleInLensUnits(real(tokens(2)))

        !   end if

    end subroutine    

    subroutine execRayAberrationPlot(iptStr)
        implicit none
        character(len=*) :: iptStr
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        call psm%addDensitySetting(64,8,128)
        call psm%addGenericSetting(SETTING_SCALE, 'Scale', 0.0, 0.0, 1000.0, 'SSI', 'SSI 0', UITYPE_SPINBUTTON)      
        call psm%addWavelengthComboSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_RIM, psm)
        if(boolResult .EQV. .FALSE.) then
            call updateTerminalLog("Error in input. Should be either RIM or RIM PX, where X is plot num", "red")
        end if


    end subroutine

    subroutine execPMAPlot(iptStr)

        implicit none
        character(len=*) :: iptStr
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        call psm%addDensitySetting(64,8,128)
        call psm%addFieldSetting()
        call psm%addWavelengthSetting()

        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_OPD, psm)
        if(boolResult .EQV. .FALSE.) then
            call updateTerminalLog("Error in input. Should be either PMA or PMA PX, where X is plot num", "red")
        end if


    end subroutine


    subroutine execAstigFieldCurvDistPlot(iptStr)
        implicit none
        character(len=*) :: iptStr
        logical :: boolResult
        type(zoaplot_setting_manager) :: psm


        call psm%initialize(trim(iptStr))
        call psm%addAstigSettings()


        boolResult = initiatePlotLoop(iptStr, ID_PLOTTYPE_AST, psm)
        if(boolResult .EQV. .FALSE.) then
            call updateTerminalLog("Error in input. Should be either ASTFCDIST or ASTFCDIST PX, where X is plot num", "red")
        end if


    end subroutine

    subroutine execTOW(iptStr)

        implicit none
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists
        type(zoaplot_setting_manager) :: psm

        if(cmd_loop == 0) then

        call parse(trim(iptStr), ' ', tokens, numTokens) 

            call psm%initialize(trim(iptStr))
            cmd_loop = TOW_LOOP
            cmdTOW = ''

            if (numTokens  == 2) then
                plotExists = checkForExistingPlot(tokens(1:2), psm, ID_TOW_TAB)
                ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
                ! here but don't have a better soultion right now
            if (plotExists) return
            end if
            curr_psm = psm
        end if
    end subroutine

    subroutine execAUT(iptStr)
        implicit none
        character(len=*) :: iptStr
        
        if (cmd_loop == 0) then

            cmd_loop = AUT_LOOP
        else
            call updateTerminalLog("Cannot enter AUT loop as in another command loop", "red")
        end if


    end subroutine

    subroutine execTAR(iptStr)
        implicit none
        character(len=*) :: iptStr
        
        if (cmd_loop == 0) then

            cmd_loop = TAR_LOOP
        else
            call updateTerminalLog("Cannot enter TAR loop as in another command loop", "red")
        end if


    end subroutine    

    subroutine execSPO(iptStr)
        !use ui_spot, only: spot_struct_settings, spot_settings
       ! use mod_plotopticalsystem

        use command_utils, only : isInputNumber
        use optim_types
        use GLOBALS, only: long

        implicit none
        character(len=*) :: iptStr
        type(zoaplot_setting_manager) :: psm

        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: plotExists

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        ! If we are in a merit update loop add operand and exit
        if(cmd_loop == AUT_LOOP .OR. cmd_loop == TAR_LOOP) then
            if (isInputNumber(trim(tokens(2)))) then 
                call LogTermDebug("About to add operand SPO")
                call addOperand('SPO', str2real8(trim(tokens(2))))
            else
                call addOperand('SPO', 0.0_long)
            end if
            return
        end if

        call psm%initialize(trim(iptStr))
        cmd_loop = SPO_LOOP

        if (numTokens  == 2) then
            plotExists = checkForExistingPlot(tokens(1:2), psm, ID_PLOTTYPE_SPOT_NEW)
            ! If plotExiss then curr_psm is sst so we are good.  Seems like a bad design
            ! here but don't have a better soultion right now
           if (plotExists) return
        end if
        call psm%addSpotDiagramSettings()
        ! Set up settings
        !call psm%addWavelengthSetting()
        !call psm%addDensitySetting(10, 8, 21)
        curr_psm = psm
    end subroutine




    subroutine execSPO_old(iptStr)
        !use ui_spot, only: spot_struct_settings, spot_settings
       ! use mod_plotopticalsystem

        implicit none
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 
        if (numTokens  == 2) then
           if (tokens(2) == 'P1') then
            ! Do not update settings
            cmd_loop = SPO_LOOP
            return
           end if
        end if
        cmd_loop = SPO_LOOP
        !spot_struct_settings = spot_settings()

    end subroutine

    subroutine execCIR(iptStr)

        use command_utils, only : parseCommandIntoTokens, isInputNumber
    
        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens, surfNum

        call parse(trim(iptStr), ' ', tokens, numTokens) 
        select case (numTokens)
        ! CIR VAL for current surface    
        case(2)
        
            ! Error Checking
            if (isInputNumber(trim(tokens(2)))) then    
                call executeCodeVLensUpdateCommand('CLAP, '//trim(tokens(2))//", 0.0, 0.0, "//trim(tokens(2)))
           else
             call updateTerminalLog( &
             & "Error: unable to intepret number for input argument "//trim(tokens(2)), "red")
             return
           end if

        ! CIR Sk VAL   
        case(3)

            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
                if (isInputNumber(trim(tokens(3)))) then    
                  call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; CLAP '//trim(tokens(3))//';GO')
                else 
                    call updateTerminalLog( &
                    & "Error: unable to intepret number for input argument "//trim(tokens(3)), "red")
                    return
                end if
            else 
                call updateTerminalLog( &
                & "Error: unable to intepret surface number for input argument "//trim(tokens(2)), "red")
                return 
            end if                         


        end select




    end subroutine

    subroutine execSTO(iptStr)
        use command_utils, only : parseCommandIntoTokens
        use mod_lens_data_manager, only: ldm
    
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        surfNum = -1 ! Error checking
        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens == 2 ) then
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            else

            call updateTerminalLog("STO Should have a surface identifier (S0, Sk, Si, SA)", "red")
            return
            end if

        else
            ! Use current surface
            surfNum = ldm%getSurfacePointer()

        end if

        ! If we got here, update
        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
        & '; ASTOP; REFS')
    end subroutine

    subroutine execRestore(iptStr)
        implicit none
        character(len=*) :: iptStr

        character(len=160) :: tokens(40)
        integer :: numTokens, locStr, locDot
        character(len=256) :: fileName

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens==2) then
            print *, "Tokens(2) is ", trim(tokens(2))
            call processZoaFileInput(trim(tokens(2)))

        else
            call updateTerminalLog("Error!  Expect two inputs, eg RES file or RES zoa_macro:file", "red")


        end if

    end subroutine

    subroutine execRestore_old(iptStr)
        !use global_widgets, only: curr_lens_data
        use globals, only: TEST_MODE
        use gtk_hl_dialog
        use zoa_file_handler
        use command_utils, only : parseCommandIntoTokens
        use zoa_file_handler, only: open_file_to_sav_lens, doesFileExist
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=80) :: tokens(40)
        character(len=1024) :: fullPath
        integer :: numTokens, locDot
        integer :: resp
        character(len=80), dimension(3) :: msg

        ! Temp vars
        integer :: ios, n
        character(len=200) :: line        

        !call parse(trim(iptStr), ' ', tokens, numTokens)
        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        if (numTokens == 2 ) then
            fName = trim(tokens(2))
            locDot = INDEX(fName, '.')
            if (locDot == 0) then
                fName = trim(fName)//'.zoa'
            end if
            call LogTermFOR("File name to restore is "//trim(fName))
            ! See if the file is in a known directory
            fullPath = getRestoreFilePath(trim(fName))

        ! Only ask user for input if not in test mode
            if (TEST_MODE.eqv..FALSE. .and. doesFileExist(trim(fullPath))) then
                ! Step 1:  Ask user if they are sure
                
                msg(1) ="You are about to start a new lens system"
                msg(2) = "Are you sure?"
                msg(3) = "Press Cancel to abort."   
              
                resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_OK_CANCEL, &
                     & "Warning"//c_null_char)
                if (resp == GTK_RESPONSE_OK) then
                  ! Ask user if they want to save current lens
                  msg(1) = "Do you want to save current lens?"
                  msg(2) = "Yes to add to lens database"
                  msg(3) = "No to throw away"
                  resp = hl_gtk_message_dialog_show(msg, GTK_BUTTONS_YES_NO, &
                  & "Warning"//c_null_char)    
                  if (resp == GTK_RESPONSE_YES) then    
                    ! Add to database
                    call PROCESKDP('LIB PUT')
                  end if ! Yes to save current lens
              
                    ! Final question!  Ask the user if they want to close current tabs
                    call zoatabMgr%closeAllTabs("dummy text at present")
              
                    ! Finally at the new lens process.  
                else 
                    ! If user aborted, log it
                    call updateTerminalLog("New Lens Process Cancelled", "black")
                end if ! add to lens database 
                end if ! Test Mode
                                  

            if (len(trim(fullPath)) > 1) then
                call process_zoa_file(fullPath)
            else
            call updateTerminalLog("File Not found in known directory!  Please try again or use UI", "red")
            end if
                   


        else
            call updateTerminalLog("No file given to restore!  Please try again", "red")

        end if



    end subroutine
      
    subroutine exportLensToCodeV(iptStr)
        use zoa_file_handler
        use ui_dialogs
        use zoa_file_handler
        use global_widgets, only: sysConfig, curr_lens_data
        use optim_types, only: optim
        use mod_lens_data_manager

        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID
        character(len=500) :: fileName
        character(len=500) :: cdir
        character(len=1024) :: currDir
        logical :: fileSelected
    
        integer :: n, ios
        character(len=256) :: line
    
        call parse(trim(iptStr), ' ', tokens, numTokens)

        if(numTokens == 2) then
            fileName = trim(tokens(2))
        else
            fileName = ''
        end if

        fileSelected = ui_new_file(my_window, fileName, cdir, trim(getCodeVDir()), "*.seq", "CodeV File")

        if (fileSelected) then
    
         PRINT *, "fileName is ", trim(getFileNameFromPath(fileName))
         PRINT *, "fileDirectory is is ", trim(cdir)
    
         fID = open_file_to_sav_lens(trim(getFileNameFromPath(fileName)), dirName=trim(cdir), overwriteFlag=.TRUE.)
        
        if (fID /= 0) then
         
           call sysConfig%genSaveOutputText(fID)
           call curr_lens_data%genSaveOutputText(fID)
           call optim%genSaveOutputText(fID)
           close(fID)
        else
            call LogTermFOR("Error!  fiD is "//int2str(fID))
        end if

         !currDir = getSaveDirectory()
         !call setSaveDirectory(trim(cdir))
         ! Here is where I should insert code to save file
    
         !call setSaveDirectory(trim(currDir))
        end if
    


    end subroutine

    subroutine exportLensToZemax(iptStr)
        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID
    end subroutine

    subroutine execSAV(iptStr)
        use global_widgets, only: sysConfig, curr_lens_data
        use command_utils, only : parseCommandIntoTokens
        use zoa_file_handler, only: open_file_to_sav_lens, getTempDirectory
        use optim_types, only: optim
        use mod_lens_data_manager
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=1024) :: dirName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID

        dirName = ''

        call parse(trim(iptStr), ' ', tokens, numTokens)

        call LogTermFOR("Number of tokens is "//int2str(numTokens))
        select case(numTokens)
        case (1) ! No file given save as current lens in temp folder
           fName = 'currlens.zoa'
           call updateTerminalLog("File name to save is "//trim(getTempDirectory())//trim(fName), "black")
           fID = open_file_to_sav_lens(fName, dirName=getTempDirectory(), overwriteFlag=.TRUE.)
        case (2) ! Check for extension and add if needed 
            fName = trim(tokens(2))
            locDot = INDEX(fName, '.')
            if (locDot == 0) then
                fName = trim(fName)//'.zoa'
            end if
            call updateTerminalLog("File name to save is "//trim(fName), "black")
            fID = open_file_to_sav_lens(fName)
        end select

        
        if (fID /= 0) then
         
           call sysConfig%genSaveOutputText(fID)
           call curr_lens_data%genSaveOutputText(fID)
           call optim%genSaveOutputText(fID)
           close(fID)
        else
            call LogTermFOR("Error!  fiD is "//int2str(fID))
        end if


        ! Save Presciption
        !writeToFile('LEN NEW')
        !PRINT *, "Lens Title is ", trim(LI)




    end subroutine


    subroutine execSaveSessionToFile(iptStr)
        use global_widgets, only: sysConfig, curr_lens_data
        use command_utils, only : parseCommandIntoTokens
        use zoa_file_handler, only: open_file_to_sav_lens
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens > 1 ) then
            fName = trim(tokens(2))
            locDot = INDEX(fName, '.')
            if (locDot == 0) then
                fName = trim(fName)//'.zoaenv'
            end if
            call LogTermFOR("File name to save is "//trim(fName))
        else
            fName = 'default.zoaenv'

        end if
        fID = open_file_to_sav_lens(fName)
        if (fID /= 0) then
         
           call sysConfig%genSaveOutputText(fID)
           call curr_lens_data%genSaveOutputText(fID)
           call zoaTabMgr%genSaveOutputText(fID)
           close(fID)
        else
            call LogTermFOR("Error!  fiD is "//int2str(fID))
        end if


        ! Save Presciption
        !writeToFile('LEN NEW')
        !PRINT *, "Lens Title is ", trim(LI)




    end subroutine


    subroutine execSetWavelengthIndex(iptStr)

        use command_utils, only : parseCommandIntoTokens
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        !call LogTermFOR("NumberofTokens is "//trim(int2str(numTokens)))

        if(numTokens == 2) then
            call executeCodeVLensUpdateCommand('CW '//trim(tokens(2)))
        else
            call updateTerminalLog("No Wavelength Index Input.  Please try again", "red")
            return            
        end if

    end subroutine

    ! RMD Sk REFR REFL TIR
    ! Change surface to refraction, reflection tir

    subroutine execRMD(iptStr)
        use command_utils, only : parseCommandIntoTokens
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(iptStr, tokens, numTokens, ' ')

        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if(numTokens > 2) then
                select case(trim(tokens(3)))
                case ('REFL')
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; REFL')
                case ('REFR')
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; AIR')                    

                case ('TIR')
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; REFLTIRO')                    
                    

                end select                          
            else
                call updateTerminalLog("No Surface Modifier Selected.  Please try again", "red")
            end if         
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if  
    end subroutine

    subroutine execSetCodeVCmd()
        use command_utils, only : parseCommandIntoTokens
        use global_widgets, only: curr_lens_data, curr_par_ray_trace     
        use DATMAI
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')
        ! This nested select statements is not sustainable.  Need a more elegant way of parsing this
        ! command and figuring out what commands to translate it to
        if(numTokens > 1 ) then
        select case(trim(tokens(2))) 
            case('MAG') ! FORMAT SET MAX X
                if(numTokens > 2) then
                    call executeCodeVLensUpdateCommand('CHG 0;TH '// &
                    real2str(curr_par_ray_trace%getObjectThicknessToSetParaxialMag( &
                    & str2real8(trim(tokens(3))),curr_lens_data)))                            
                else
                    call updateTerminalLog("No Mag Value specified.  Please try again", "red")
                end if 
                

            end select

        end if
    end subroutine

    subroutine setMagSolve(iptStr)
        use command_utils, only: isInputNumber
        implicit none

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 2) then
            if (isInputNumber(tokens(2))) then
                call executeCodeVLensUpdateCommand('CHG 0; REDSLV '//trim(tokens(2)), exitLensUpdate=.TRUE.) 
            else
                call updateTerminalLog("Error!  Unable to parse value "//trim(tokens(2))//" into number", "red")
            end if   
        else
            call updateTerminalLog("Error!  Expecting RED X where X is the desired reduction factor", "red")
        end if            

        

    end subroutine

    ! Format:  DEL SOL CUY S2
    !          DEL PIM
    subroutine deleteStuff(iptStr)
        use command_utils, only : parseCommandIntoTokens
        use global_widgets, only: curr_lens_data
        implicit none

        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(iptStr, tokens, numTokens, ' ')
        ! This nested select statements is not sustainable.  Need a more elegant way of parsing this
        ! command and figuring out what commands to translate it to
        if(numTokens > 1 ) then
        select case(trim(tokens(2))) 
            case('PIM')
                call updateTerminalLog("Deleting PIM", "blue")
                surfNum = curr_lens_data%num_surfaces - 2
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; TSD '//';GO')          

            case('SOL') ! Delete Solves
                if(numTokens > 2) then
                    call updateTerminalLog("Deleting Solve", "blue")
                    select case(trim(tokens(3)))
                    case('CUY')
                        if (isSurfCommand(trim(tokens(4)))) then
                            surfNum = getSurfNumFromSurfCommand(trim(tokens(4)))
                            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                            & '; CSDY '//';GO')                            
                        end if
                    end select
                else
                    call updateTerminalLog("No Angle Solve Specified.  Please try again", "red")
                    end if 
                

            end select

        end if

    end subroutine

    subroutine setEPD()
        use command_utils
        implicit none

         if(checkCommandInput([ID_CMD_NUM], max_num_terms=1)) then
            call executeCodeVLensUpdateCommand('SAY '//real2str(getInputNumber(1)/2.0))
         end if      



        ! IF(WC.EQ.'EPD') THEN
        !     IF(DF1.EQ.0) W1=W1/2.0D0
        !     WC='SAY'
        !             END IF        

    end subroutine

    subroutine setParaxialImageSolve()
        use global_widgets, only: curr_lens_data
        integer :: surfNum

        ! Get surface before last surface and add solve
        surfNum = curr_lens_data%num_surfaces - 2
        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
        & '; PY, 0; GO') ! go added to make sure we exit update lens level            
       


    end subroutine

    ! This function exists to update text used to set glass, especially to account
    ! for model glass
    function getSetGlassText(strInput) result(strOut)
        use command_utils, only : isInputNumber
        use glass_manager, only: parseModelGlassEntry
        character(len=*) :: strInput
        character(len=1024) :: strOut
        real(kind=real64) :: nd, vd

        if (isInputNumber(strInput)) then ! Assume user entered model glass
            !PRINT *, "Model Glass Entered!"
            call LogTermFOR("Model Glass Entered! "//strInput)
            call parseModelGlassEntry(strInput, nd, vd)
            strOut = 'MODEL D'//strInput//','//real2str(nd)//','//real2str(vd)   
            call LogTermFOR("STROUT is "//trim(strOut))
        else ! Assume it is glass name          
            strOut = 'GLAK ' // strInput
        end if            


    end function




    subroutine updateVarCodes(iptStr)
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: processResult 
        integer :: s0, sf, dotLoc

        processResult = .FALSE.

        call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 3) then
            dotLoc = index(tokens(2),'..') 
            if(dotLoc > 0) then
                ! Assume input is Si..k
                if(isInputNumber(tokens(2)(2:dotLoc-1)).AND. &
                &  isInputNumber(tokens(2)(dotLoc+2:len(tokens(2))))) then
                   s0 = str2int(tokens(2)(2:dotLoc-1))
                   sf = str2int(tokens(2)(dotLoc+2:len(tokens(2))))
                   processResult = .TRUE.
                else
                    call updateTerminalLog("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            else ! No dots found
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (surfNum.NE.-1) then
                s0=surfNum
                sf=surfNum
                processResult = .TRUE.
            else
                call updateTerminalLog("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
            end if
            end if
        end if

        if (processResult) then
            if(isInputNumber(trim(tokens(3)))) then
                call ldm%updateOptimVars(trim(tokens(1)), s0,sf,str2int(trim(tokens(3))))
                ! THis is to revolve a circular dependency - not sure how to resolve this in a good way.             
                call updateOptimVarsNew(trim(tokens(1)),s0,sf,str2int(trim(tokens(3))))
            end if
        else
            call updateTerminalLog("Error:  Variable code must be number "//trim(tokens(3)), "red")
        end if


    end subroutine        


    subroutine updateConstraint(iptStr)
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: processResult 
        integer :: s0, sf, dotLoc

        processResult = .FALSE.

        if(cmd_loop == AUT_LOOP .OR. cmd_loop == TAR_LOOP) then
          call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 3 .AND. isInputNumber(trim(tokens(3)))) then ! The only correct answer here
 
            select case(trim(tokens(2)))
            case('=')
                call addConstraint(trim(tokens(1)), str2real8(tokens(3)), eq=.TRUE.)
            case('>')
                call addConstraint(trim(tokens(1)), str2real8(tokens(3)), eq=.FALSE., lb=.FALSE.,ub=.TRUE.)
            case('<')
                call addConstraint(trim(tokens(1)), str2real8(tokens(3)), eq=.FALSE., lb=.TRUE.,ub=.FALSE.)
            case default
                call updateTerminalLog("Error:  Format should be EFL >,=,< value ", "red")
            end select
        else
            call updateTerminalLog("Error:  Unable to parse number for third token ", "red")
        end if
        else
            call updateTerminalLog("Error:  Can only set constraint in AUT loop! ", "red")

        end if

    end subroutine    

    subroutine updateEFLConstraint(iptStr)
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: processResult 
        integer :: s0, sf, dotLoc

        processResult = .FALSE.

        if(cmd_loop == AUT_LOOP .OR. cmd_loop == TAR_LOOP) then
          call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 3 .AND. isInputNumber(trim(tokens(3)))) then ! The only correct answer here
 
            select case(trim(tokens(2)))
            case('=')
                call addConstraint('EFL', str2real8(tokens(3)), eq=.TRUE.)
            case('>')
                call addConstraint('EFL', str2real8(tokens(3)), eq=.FALSE., lb=.FALSE.,ub=.TRUE.)
            case('<')
                call addConstraint('EFL', str2real8(tokens(3)), eq=.FALSE., lb=.TRUE.,ub=.FALSE.)
            case default
                call updateTerminalLog("Error:  Format should be EFL >,=,< value ", "red")
            end select
        else
            call updateTerminalLog("Error:  Unable to parse number for third token ", "red")
        end if
        else
            call updateTerminalLog("Error:  Can only set constraint in AUT loop! ", "red")

        end if

    end subroutine    

    ! TODO:  Refactor with EFL constraint.  Just use tokens(1) ant that's it?
    subroutine updateTCOConstraint(iptStr)
        use command_utils, only : isInputNumber
        use mod_lens_data_manager
        use optim_types
        implicit none

        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: processResult 
        integer :: s0, sf, dotLoc

        processResult = .FALSE.

        if(cmd_loop == AUT_LOOP .OR. cmd_loop == TAR_LOOP) then
          call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens == 3 .AND. isInputNumber(trim(tokens(3)))) then ! The only correct answer here
 
            select case(trim(tokens(2)))
            case('=')
                call addConstraint(trim(tokens(1)), str2real8(tokens(3)), eq=.TRUE.)
            case('>')
            case('<')
            case default
                call updateTerminalLog("Error:  Format should be EFL >,=,< value ", "red")
            end select
        else
            call updateTerminalLog("Error:  Unable to parse number for third token ", "red")
        end if
        else
            call updateTerminalLog("Error:  Can only set constraint in AUT loop! ", "red")

        end if

    end subroutine        


    ! NBR ELE Si..j only supported
    subroutine execNBR(iptStr)
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult
        integer, allocatable :: surfaces(:)
   

        call parse(trim(iptStr), ' ', tokens, numTokens) 
 
        boolResult = .FALSE.
        if (numTokens > 2) then

        select case(trim(tokens(2)))
        case('ELE')
            if(tokens(3)(1:1) == 'S') then
                boolResult = cmd_parser_get_integer_range(tokens(3)(2:len(tokens(3))), surfaces)
                if (boolResult) then
                    if(cmd_loop == VIE_LOOP) then
                        call LogTermFOR("Upating Surfaces in ld_settings")
                        call curr_psm%updateSetting(ID_LENS_FIRSTSURFACE, surfaces(1))
                        call curr_psm%updateSetting(ID_LENS_LASTSURFACE, surfaces(size(surfaces)))
                    end if
                end if
            end if
        case default    
            boolResult = .FALSE.
        end select
        end if
        if (boolResult .EQV. .FALSE. ) then
            call updateTerminalLog( &
            & "Unable to parse command.  Expect NBR ELE Si..j Got " //trim(iptStr), "black")
        end if



    end subroutine

    ! CLI SA supported only at this time
    subroutine execCLI(iptStr)
        use global_widgets, only: curr_lens_data
        use command_utils, only: isInputNumber
        use kdp_data_types, only: check_clear_apertures
        use DATLEN
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: i
        character(len=80) :: tokens(40)
        character(len=4)  :: surfTxt
        integer :: numTokens
        !include "DATLEN.INC"

        call parse(trim(iptStr), ' ', tokens, numTokens) 
        call LogTermFOR("Calling check_clear_apetures")
        call check_clear_apertures(curr_lens_data)
        call updateTerminalLog(blankStr(7)//"Y-FAN"//blankStr(5)//"X-FAN", "black")
        do i=2,curr_lens_data%num_surfaces
            surfTxt = blankStr(2)//trim(int2str(i-1))
            ! Special Cases
            if(i==1)                           surfTxt = "OBJ"
            if(i==curr_lens_data%ref_stop)     surfTxt = "STO"
            if(i==curr_lens_data%num_surfaces) surfTxt = "IMG"
            call updateTerminalLog(trim(surfTxt)//blankStr(2)// &
            & trim(real2str(curr_lens_data%clearAps(i)%yRad))//blankStr(5)// &
            & trim(real2str(curr_lens_data%clearAps(i)%xRad)), "black")
        end do

    end subroutine

    subroutine insertSurf(iptStr)
        use command_utils, only: isInputNumber
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum, i, s0, sf, dotLoc
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens == 2) then
            ! Treat special case of ..  TODO:  Should this be in getSurfNum(eg return array output?).  Seems messy
            dotLoc = index(tokens(2),'..') 
            if(dotLoc > 0) then
                ! Assume input is Si..k
                PRINT *, "dotLoc-1 is ", dotLoc-1
                if(isInputNumber(tokens(2)(2:dotLoc-1)).AND. &
                &  isInputNumber(tokens(2)(dotLoc+2:len(tokens(2))))) then
                   s0 = str2int(tokens(2)(2:dotLoc-1))
                   sf = str2int(tokens(2)(dotLoc+2:len(tokens(2))))
                   PRINT *, "s0 is ", s0
                   PRINT *, "sf is ", sf
                   do i=s0,sf
                    call executeCodeVLensUpdateCommand('INSK, '//trim(int2str(i)), exitLensUpdate=.TRUE.)                           
                   end do
                else
                    call updateTerminalLog("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
                end if
            else ! No dots found
    

            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (surfNum.NE.-1) then
               call executeCodeVLensUpdateCommand('INSK, '//trim(int2str(surfNum)), exitLensUpdate=.TRUE.)
            else
                call updateTerminalLog("Error:  Incorrect surface number input "//trim(tokens(2)), "red")
            end if
            end if
        end if


    

        !PRINT *, "Inside insertSurf"
        ! TODO:  Add an error check for Sk in checkCommandInput

        ! if (checkCommandInput([ID_CMD_QUAL])) then
        !     surfNum = getSurfNumFromSurfCommand(trim(getQualWord()))
        !     call executeCodeVLensUpdateCommand('INSK, '//trim(int2str(surfNum)))
        ! end if            




    end subroutine



    subroutine setDim(iptStr)
        use command_utils
        character(len=*) :: iptStr
        logical :: inputCheck

         inputCheck = checkCommandInput([ID_CMD_QUAL], qual_words=['M', 'C', 'I'], &
         &qual_only_err_msg="DIM Takes only M(mm), C(cm), or I(inches) as input")

        ! TODO:  Get qual letter and direct to correct command
        ! if (inputCheck) then
            select case (getQualWord())

            case ('M')
                call executeCodeVLensUpdateCommand("UNITS MM")
            case ('C')
                call executeCodeVLensUpdateCommand("UNITS CM")
            case ('I')
                call executeCodeVLensUpdateCommand("UNITS IN")

            end select


    end subroutine

    subroutine newLens 
        use gtk_hl_dialog
        use globals, only: basePath, TEST_MODE
        use mod_lens_data_manager
      
        implicit none  
      
      
        integer :: resp
        character(len=80), dimension(3) :: msg

        ! Temp vars
        integer :: ios, n
        character(len=200) :: line
        


      ! Prototype for getting this from file
      !PRINT *, "attempting to open ",trim(basePath)//'Macros/newlens.zoa'
      open(unit=9, file=trim(basePath)//'Macros/newlens.zoa', iostat=ios)
      if ( ios /= 0 ) stop "Error opening file "
  
      n = 0
  
      do
          read(9, '(A)', iostat=ios) line
          if (ios /= 0) then 
            exit
          else
            !call LogTermFOR("About to exec "//trim(line))
            call PROCESKDP(trim(line))
          end if
          n = n + 1
      end do      
      close(unit=9)

      ! Initialize the vars array to be default (100)
      ldm%vars(:,:) = 100

      ! Since this is called by .zoa files, want to go into lens update mode
      !CALL PROCESSILENT('U L')
      
      
      end subroutine    

      !TIT
      ! todo:  Send title to parseTitleCommand to clean this up a bit
      subroutine setLensTitle(iptStr)
        use command_utils

        character(len=*) :: iptStr

        call executeCodeVLensUpdateCommand('LI '// parseTitleCommand())



      end subroutine

      subroutine processFileComment(iptStr)
        character(len=*) :: iptStr

        ! Do nothing

      end subroutine      

      subroutine setFieldWeights(iptStr)
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        ! This is not implemented. 
        call updateTerminalLog("Field Weights Command "//trim(iptStr)//" &
        & Not supported", "black")

      end subroutine

      subroutine setWavelengthWeights(iptStr)
        use global_widgets, only: sysConfig
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: i
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
    

        do i=2,numTokens
            call sysConfig%setSpectralWeights(i-1,real(str2real8(trim(tokens(i))),4))            
        end do

        end subroutine


      subroutine setField(iptStr)
        ! TODO:  Support things other than YAN
         use command_utils, only : parseCommandIntoTokens
         use kdp_utils, only: inLensUpdateLevel
         use global_widgets, only: sysConfig
         implicit none
 
         !class(zoa_cmd) :: self
         character(len=*) :: iptStr
         integer :: i,numFields
         character(len=80) :: tokens(40)
         integer :: numTokens
         real(kind=real64), allocatable :: absFields(:)

         integer, parameter :: X_COL = 1
         integer, parameter :: Y_COL = 2
         integer :: FLD_COL

 
         !call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
         call parse(trim(iptStr), ' ', tokens, numTokens)

         ! TODO:  Support more field types
         if (tokens(1).EQ.'YAN'.OR.tokens(1).EQ.'YOB'.OR.tokens(1).EQ.'YIM') FLD_COL = Y_COL
         if (tokens(1).EQ.'XAN'.OR.tokens(1).EQ.'XOB'.OR.tokens(1).EQ.'XIM') FLD_COL = X_COL

         call sysConfig%setFieldTypeFromString(trim(tokens(1)))

          numFields = numTokens-1
          allocate(absFields(numFields))
          do i=1,numFields
            absFields(i) = str2real8(trim(tokens(i+1)))
          end do
          call sysConfig%setNumFields(numFields)
          call sysConfig%setAbsoluteFields(absFields, FLD_COL)

          ! Force update of lens system
          ! JN 2/6/25.  This called me a lot of pain.  I added this to force update in UI,
          ! but it wreaked havoc when I was trying to clean up lens input
          !CALL LNSEOS



       end subroutine   
       
      subroutine setWavelength(iptStr)
       !TODO Support inputting up to 10 WL  See CV2PRG.FOR
        use globals,only: long
        use command_utils, only : parseCommandIntoTokens
        use global_widgets, only: sysConfig
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=1024) :: outStr
        integer :: i
        character(len=80) :: tokens(40)
        character(len=1024) :: strWL
        integer :: numTokens
        real(long) :: wlReal
        logical :: CVERROR

        call parse(trim(iptStr), ' ', tokens, numTokens)

        if (numTokens <= 6) then
            outStr = 'WV, '
            do i=2,numTokens
                call ATODCODEV(tokens(i)(1:23), wlReal, CVERROR)
                write(strWL, '(D23.15)') wlReal/1000.0_long
                outStr = trim(outStr)//' '//trim(strWL)
                ! Set spectral weights; assume all equal to 1.0 here
                call sysConfig%setSpectralWeights(i-1, 1.0)               
            end do
        if (numTokens < 6) then
            do i=numTokens+1,6
                outStr = trim(outStr)//' 0.0'
            end do
        end if
            call executeCodeVLensUpdateCommand(trim(outStr))
        end if

      end subroutine      

      !Todo:  put this in a submodule, as this sub will get HUGE eventually
      subroutine executeGo()
        use global_widgets, only: ioConfig
        use kdp_utils, only: inLensUpdateLevel
        use plot_functions
        use optim_functions
        use tow_functions, only: tow_go

        !TODO:  Switch to select case
        if (cmd_loop == ID_PLOTTYPE_MTF) then
            call mtf_go(curr_psm)
            cmd_loop = 0
        end if    
        
        if (cmd_loop == ID_PLOTTYPE_PSF) then
            call psf_go(curr_psm)
            cmd_loop = 0
        end if        

        if (cmd_loop == AUT_LOOP) then
            call aut_go()
            cmd_loop = 0
        end if

        if (cmd_loop == TAR_LOOP) then
            !call aut_go()
            cmd_loop = 0
            return
        end if        

        if (cmd_loop == TOW_LOOP) then
            cmd_loop = 0 ! Need to put this first because when in TOW loop commands are intercepted before 
            ! executed.  
            call curr_psm%addGenericSetting(999, "Command", real(999), -1.0, -1.0, ' ', trim(cmdTOW), UITYPE_ENTRY)
            call tow_go(curr_psm, trim(cmdTOW))

        end if

        if (cmd_loop == VIE_LOOP) then
                ! Hide KDP Commands from user
        
        ! Temp for testing
        ! Do nothing if in update loop
        if (inLensUpdateLevel()) then
            call LogTermFOR("Will not draw in lens update level")
            return
        end if    

        ! Test new way

     

        !call LogTermFOR("IN VIE LOOP ")
        
        !call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
        cmd_loop = DRAW_LOOP
        call vie_go(curr_psm)
        !CALL PROCESKDP('DRAW')
        !call ioConfig%setTextView(ID_TERMINAL_DEFAULT)  
        
        
        
        !Working code for the OLD Wway

        ! cmd_loop = DRAW_LOOP
        ! active_plot = ID_NEWPLOT_LENSDRAW
        ! call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
        ! call VIE_NEW_NEW(ld_settings)
        ! CALL PROCESKDP('DRAW')
        ! call ioConfig%setTextView(ID_TERMINAL_DEFAULT)  


            
        cmd_loop = 0 ! Go back to base level
        end if

        if (cmd_loop == SPO_LOOP) then
            !active_plot = ID_PLOTTYPE_SPOT
            !call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
            !call PROCESKDP("FOB 1; SPD; PLTSPD")
            !call ioConfig%setTextView(ID_TERMINAL_DEFAULT)  
            call LogTermFOR("Existing SPO Loop")
            call spo_go(curr_psm)            
            cmd_loop = 0

        end if

        if (cmd_loop == ID_PLOTTYPE_SEIDEL) then
            call seidel_go(curr_psm)
            cmd_loop = 0
        end if

        if (cmd_loop == ID_PLOTTYPE_AST) then
            call ast_go(curr_psm)
            cmd_loop = 0
        end if

        if (cmd_loop == ID_PLOTTYPE_OPD) then
            call pma_go(curr_psm)
            cmd_loop = 0
        end if

        if (cmd_loop == ID_PLOTTYPE_RIM) then
            call rayaberration_go(curr_psm)
            cmd_loop = 0
        end if

        if (cmd_loop == ID_PLOTTYPE_RMSFIELD) then
            call rmsfield_go(curr_psm)
            cmd_loop = 0
        end if        




        if (cmd_loop == ZERN_LOOP) then
            call LogTermFOR("Existing Zern Loop")
            call zern_go(curr_psm)
            cmd_loop = 0

        end if        



        if (inLensUpdateLevel()) then 
            call PROCESKDP('EOS')
            !call zoatabMgr%replotifneeded()
        end if

      end subroutine

      function isSurfCommand(tstCmd) result(boolResult)
        implicit none
        character(len=*) :: tstCmd
        logical :: boolResult
        integer :: i

        boolResult = .FALSE.
        
        ! Special case:  SO
        if (tstCmd.EQ.'SO') then
            boolResult = .TRUE.
            return
        end if

        do i=1,size(surfCmds)
            surfCmds(i) = 'S'//trim(int2str(i))
            if(tstCmd.EQ.surfCmds(i)) then
                boolResult = .TRUE.
            end if

        end do

        if (tstCmd.EQ.'SI') then
            boolResult = .TRUE.
            return
        end if        

        ! SA for SUR command
        if (tstCmd.EQ.'SA') then
            boolResult = .TRUE.
            return
        end if        





      end function

     ! This is a way to deal with this allocatable atring issue  
!       type string
!       character(len=:), allocatable :: s
!    end type string
! ! create an array of strings where each element is separately allocatable
!    type(string) :: month(4)
!    integer :: j   
!    month(1)%s = 'January'   
!    month(2)%s = 'February'
!    month(3)%s = 'March'
!    month(4)%s = 'April'
!    print *, (month(j)%s, ' ', j=1,size(month))
!    print *, (len(month(j)%s), j=1, size(month))

      function isCodeVCommand(tstCmd) result(boolResult)
        implicit none 

        logical :: boolResult
        character(len=*) :: tstCmd
        type(string) :: tstCmds(17+size(zoaCmds))
        integer :: i


        ! TODO:  Find some better way to do this.  For now, brute force it
        ! codeVCmds = [character(len=4) :: 'YAN', 'TIT', 'WL', 'SO','S','GO', &
        ! &'DIM', 'RDY', 'THI', 'INS', 'GLA', 'PIM', 'EPD', 'CUY', &
        ! & 'DEL', 'RED', 'SETC', 'AAA', 'AAA']
        ! do i=1,size(zoaCmds)
        !    codeVCmds(i+17) = zoaCmds(i)%cmd
        ! end do

        ! This hard coding of cmds is temporary, until I migrate these to new format

        !tstCmds(1)%s = 'YAN'
        !tstCmds(2)%s = 'TIT'
        !tstCmds(3)%s = 'WL'
        tstCmds(6)%s = 'GO'
        tstCmds(7)%s = 'DIM'
        !tstCmds(8)%s = 'RDY'
        !tstCmds(9)%s = 'THI'
        !tstCmds(10)%s = 'INS'
        !tstCmds(11)%s = 'GLA'
        tstCmds(12)%s = 'PIM'
        tstCmds(13)%s = 'EPD'
        !tstCmds(14)%s = 'CUY'
        tstCmds(15)%s = 'DEL'
        !tstCmds(16)%s = 'RED'
        tstCmds(17)%s = 'SETC'
        do i=1,size(zoaCmds)
            tstCmds(17+i)%s = zoaCmds(i)%cmd
        end do



        boolResult = .FALSE.
        do i=1,size(tstCmds)
            if (tstCmds(i)%s.EQ.tstCmd) then
                boolResult = .TRUE.
                return
            end if
        end do
        ! If we've gotten here check if it is a surface command
        boolResult = isSurfCommand(tstCmd)

      end function

      subroutine executeCodeVLensUpdateCommand(iptCmd, debugFlag, exitLensUpdate)
        use kdp_utils, only: inLensUpdateLevel
        use global_widgets, only: ioConfig

        implicit none
        character(len=*) :: iptCmd
        logical, optional :: debugFlag, exitLensUpdate
        logical :: redirectFlag, inUpdate

        if(present(debugFlag)) then 
            redirectFlag = .NOT.debugFlag
        else
            redirectFlag = .TRUE.
        end if

        ! Hide KDP Commands from user
        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)
          
        inUpdate = inLensUpdateLevel()
        if (inUpdate) then              
            call PROCESKDP(iptCmd)
        else
            !call PROCESKDP('U L;'// iptCmd //';EOS')
            ! Update - do not exit lens update level to better support stops
            ! clear apertures, etc 
            call PROCESKDP('U L;'// iptCmd )
        end if
        
        ! If the called asked to exit update, then exit.
        ! If we were not in lens update level, then exit (return to prior state)
        !eosCalled = .FALSE.
        if(present(exitLensUpdate)) then
            if(exitLensUpdate) CALL PROCESKDP('EOS')
        !    if(exitLensUpdate.eqv..TRUE..OR.inUpdate.eqv..FALSE.) CALL PROCESKDP('EOS')
        end if
         if(inUpdate.eqv..FALSE.) CALL PROCESKDP('EOS')
        

    


        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
      end subroutine

      function getSurfNumFromSurfCommand(iptCmd) result(surfNum)
        use mod_lens_data_manager
        use global_widgets, only: curr_lens_data
        use command_utils, only: isInputNumber
        implicit none
        character(len=*) :: iptCmd
        integer :: surfNum, ptrIdx

        !print *, "IPTCMD is ", iptCmd
        !print *, "len of iptCmd is ", len(iptCmd)

        surfnum = -1 ! For error checking

        if(len(iptCmd).EQ.1) then ! It is S, which means we either move to the next surface or add a surface before
                                  ! the last surface.  
            ptrIdx = ldm%getSurfacePointer()
            if (ptrIdx >= ldm%getLastSurf()-1) then
                ! increment surface
                surfNum = curr_lens_data%num_surfaces-1
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                &  "; INSK "//trim(int2str(surfNum)))
            else ! Move pointer to next surface
                call ldm%incrementSurfacePointer()
                surfNum = ptrIdx+1
            end if
            !surfNum = 1
            return
        end if
        if(len(iptCmd).EQ.2) then
            if (iptCmd(2:2).EQ.'O') then ! 'CMD is SO
                surfNum = 0
                return
            end if
            if (iptCmd(2:2).EQ.'I') then ! CMD is SI
                surfNum = curr_lens_data%num_surfaces-1 ! Not sure thsi is correct
                !call LogTermFOR("DEBUG:  Setting SurfNum to "//int2str(surfNum))
                return
            end if
        end if

        if(len(iptCmd).GT.1) then
            if (isInputNumber(iptCmd(2:len(iptCmd)))) surfNum = str2int(iptCmd(2:len(iptCmd)))
        end if




      end function

      subroutine setLens()

            ! Here I am creating a new default lens.
            ! Not sure this is the right thing to do, but for now give it a go
            call PROCESKDP('LENS')
            call PROCESKDP('WV, 0.635')
            call PROCESKDP('UNITS MM')
            call PROCESKDP('SAY, 10.0')
            call PROCESKDP('CV, 0.0')
            call PROCESKDP('TH, 0.10E+21')
            call PROCESKDP('AIR')
            call PROCESKDP('CV, 0.0')
            call PROCESKDP('TH, 10.0')
            call PROCESKDP('REFS')
            call PROCESKDP('ASTOP')
            call PROCESKDP('AIR')
            call PROCESKDP('CV, 0.0')
            call PROCESKDP('TH, 1.0')
            call PROCESKDP('EOS')    
            call PROCESKDP('U L')
      

      end subroutine

    function isSpecialGlass(tstStr) result(boolResult)
        character(len=*) :: tstStr
        logical :: boolResult

        boolResult = .FALSE.

        if(tstStr.EQ.'AIR') boolResult = .TRUE.
        if(tstStr.EQ.'REFL') boolResult = .TRUE.


    end function


      subroutine setSurfaceCodeVStyle(iptStr)
        use mod_lens_data_manager
        use command_utils, only : parseCommandIntoTokens

        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens


        !call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        call parse(trim(iptStr), ' ', tokens, numTokens)

        select case(numTokens)
        case (1)
            call updateTerminalLog("No info given besides surface identifier!  Please try again", "red")
        case (2) ! Curvature only
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2)))              
        case (3) ! Curvature and thickness
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            !if()
            ! KDP Does not allow setting image thickness.  So work around this for now
            ! if (trim(tokens(1)).EQ.'SI') then
            !     call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            !     & '; RD, ' // trim(tokens(2)), .TRUE.)   
            ! else
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2))//";TH, "// &
            & trim(tokens(3)))        
            ! end if    
        case (4) ! Curvature, thickness, and glass
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            if(.not.isSpecialGlass(trim(tokens(4)))) then

            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2))//";TH, "// &
            & trim(tokens(3))//'; '//trim(getSetGlassText(trim(tokens(4))))) 
            else
                ! TODO:  This and the isSpecialGlass function should go somewhere else.
                ! but first need to figure out if I really want to store this info in 
                ! glassnames or create a new array for this info.
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; RD, ' // trim(tokens(2))//";TH, "// &
                & trim(tokens(3))//';' // trim(tokens(4)))                
            end if
        end select
    end subroutine

    subroutine flipSurfaces(iptStr)
    
        use global_widgets, only: sysConfig
        use command_utils, only : parseCommandIntoTokens, isInputNumber

        use mod_lens_data_manager
    
        implicit none        

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        integer, allocatable :: surfs(:)

        integer :: i, j, iSto, iStoNew
        real :: symPlane
        
     
        call parse(trim(iptStr), ' ', tokens, numTokens) 
        
        surfs = cmd_parser_get_int_input_for_prefix('s', tokens(1:numTokens))
        


        if (size(surfs) > 1) then
            call PROCESSILENT('FLIP '//trim(int2str(surfs(1)))//","//trim(int2str(surfs(size(surfs)))))
        else
            call updateTerminalLog("Error!  Must have at least two surfaces to flip", "red")
        end if

        !If stop surface is in list flip does not handle it.  So work it out here
        iSto = ldm%getStopSurf()
        if (iSto >= surfs(1) .AND. iSto <= surfs(size(surfs))) then
        ! Have to flip stop
          symPlane = getSymmetryPlane(surfs)
          iStoNew = INT(symPlane-iSto+symPlane)
          call execSTO('STO S'//trim(int2str(iStoNew)))
        end if

    
    end subroutine
    
    ! THis should go somewhere else, but not sure where so keep it here for now
    function getSymmetryPlane(surfs) result (midPoint)
        implicit none
        integer, dimension(:) :: surfs
        real :: midPoint
        
        !if MOD(size(surfs(2)),2) then 
        ! Odd
           midPoint = size(surfs)/2.0 +0.5
        !else
        ! Even
        !    midPoint = 
        !end if

    end function

    subroutine scaleSystem(iptStr)
        use command_utils, only : isInputNumber

        implicit none        

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        
        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens == 3) then
            select case(tokens(2))
            case('EFL')
                if (isInputNumber(tokens(3))) then
                    call PROCESSILENT('SC FY, '//trim(tokens(3))// ", 0")

                else
                    call updateTerminalLog("Error!  Could not convert 3rd token to numeric value", "red")
                end if
            end select

        else
            call updateTerminalLog("Error!  SCA format is SCA VAR VAL.  Eg SCA EFL 50", "red")
        end if
        
    end subroutine


    function isInputSurfaceParameter(iptStr) result(boolResult)

        implicit none

        character(len=*) :: iptStr
        logical :: boolResult

        boolResult = .FALSE.

        ! For now hard code this
        if (iptStr == 'RDY' .or. iptStr == 'THI' .or. iptStr == 'GLA') then
            boolResult = .TRUE.

        end if

    end function

    subroutine setPickup(param1, si, sj, scale, offset, param2)
        use GLOBALS, only: long

        implicit none
        character(len=*) :: param1
        integer :: si, sj
        real(long) :: scale, offset
        character(len=*), optional :: param2 ! Not currently supported

        character(len=6) :: kParam
        logical :: scaleOffset

        scaleOffset = .TRUE.

        ! Translate 
        select case (param1)
        case('RDY')
            kParam = 'RD'
        case('THI')
            kParam = 'TH'
        case('GLA')
            kParam = 'GLASS'
            scaleOffset = .FALSE.
        end select

        if(scaleOffset) then
            call executeCodeVLensUpdateCommand("CHG "//trim(int2str(si))//";PIKUP "//trim(kParam)//","// &
                & trim(int2str(sj))//","//trim(real2str(scale))//","//trim(real2str(offset)))
        else
            call executeCodeVLensUpdateCommand("CHG "//trim(int2str(si))//";PIKUP "//trim(kParam)//","// &
                & trim(int2str(sj)))
        end if

    end subroutine

    !Format PIK PARAM Si [PARAM] Sj [sf off]
    !Set value of Si based on Sj*sf + off and param2 if it exists

    ! The error checking/parsing here is a mess.  There must be a better way but not confident enough of a design to 
    ! eval this at present

    subroutine parsePickupInput(iptStr)
        use globals, only: long
        use command_utils, only : isInputNumber

        implicit none        

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens, si, sj
        
        call parse(trim(iptStr), ' ', tokens, numTokens) 

        select case(numTokens)
        case (:3) ! < 4
            call updateTerminalLog("Error!  Not enough arguments!  Format is PIK PARAM Si [PARAM] Sj [sf off]", "red")
        case (4) ! Must be PIK PARAM Si Sj.  Scale/offset default (1 0)
            si = getSurfNumFromSurfCommand(trim(tokens(3)))
            sj = getSurfNumFromSurfCommand(trim(tokens(4)))
            if (si == -1 .or. sj == -1) then
                call updateTerminalLog("Error!  can't parse surfaces from arguments 3 and 4", "red")
                return
            end if
            if (isInputSurfaceParameter(trim(tokens(2)))) then
                call setPickup(trim(tokens(2)), si,sj, 1.0_long, 0.0_long)
            else
                call updateTerminalLog("Error!  Cannot parse parameter "//trim(tokens(2))//" to known surface parameter", "red")
            end if
        case(5, 6) ! Can be either PIK PARAM Si PARAM Sj or PIK PARAM Si Sj sf          
            ! The key to figuring out which is token 4
            sj = getSurfNumFromSurfCommand(trim(tokens(4)))
            if (sj == -1) then ! Either err or PIK PARAM Si PARAM Sj
                si = getSurfNumFromSurfCommand(trim(tokens(3)))
                sj = getSurfNumFromSurfCommand(trim(tokens(5)))
                if (si == -1 .or. sj == -1) then
                    call updateTerminalLog("Error!  can't parse surfaces from arguments 3 and 5", "red")
                    return
                else 
                    if (isInputSurfaceParameter(trim(tokens(2))) .AND. isInputSurfaceParameter(trim(tokens(4)))) then
                        if (numTokens == 6 .and. isInputNumber(trim(tokens(6)))) then
                            call setPickup(trim(tokens(2)), si,sj, str2real8(trim(tokens(6))), 0.0_long, trim(tokens(5)))
                        else
                            call setPickup(trim(tokens(2)), si,sj, 1.0_long, 0.0_long, trim(tokens(5)))                            
                        end if
                    else
                        call updateTerminalLog("Error!  Cannot parse parameter "//trim(tokens(2))//" or "//trim(tokens(5))// &
                        & " to known surface parameter", "red")
                    end if                      
                end if
            else ! Either Err or PIK PARAM Si Sj sf
                si = getSurfNumFromSurfCommand(trim(tokens(3)))
                if (si == -1) then
                    call updateTerminalLog("Error!  can't parse surface from arguments 3 ", "red")
                    return
                else ! Still hope it's a proper command
                    if (isInputSurfaceParameter(trim(tokens(2))) .AND. isInputNumber(trim(tokens(5)))) then
                        call setPickup(trim(tokens(2)), si,sj, str2real8(trim(tokens(5))), 0.0_long)
                    else
                        call updateTerminalLog("Error!  Cannot parse parameter "//trim(tokens(2))//" or "//trim(tokens(5))// &
                        & " to known surface parameter", "red")
                    end if     
                end if             

            end if           
        case(7) ! Finally only one option:  PIK PARAM Si PARAM Sj Sf off
            si = getSurfNumFromSurfCommand(trim(tokens(3)))
            sj = getSurfNumFromSurfCommand(trim(tokens(5)))
            if (si == -1 .or. sj == -1) then
                call updateTerminalLog("Error!  can't parse surfaces from arguments 3 and 5", "red")
                return
            else ! Continue with error checking
            if (isInputSurfaceParameter(trim(tokens(2))) .AND. isInputSurfaceParameter(trim(tokens(4)))) then 
                if (isInputNumber(trim(tokens(6))) .AND. isInputNumber(trim(tokens(7)))) then
                    ! Finally got what we wanted!
                    call setPickup(trim(tokens(2)), si,sj, str2real8(trim(tokens(6))), str2real8(trim(tokens(7))), trim(tokens(5)))
                else
                    call updateTerminalLog("Error!  Cannot parse scale or offset parameter", "red")
                end if    
            else
                call updateTerminalLog("Error!  Cannot parse parameter "//trim(tokens(2))//" or "//trim(tokens(5))// &
                & " to known surface parameter", "red")
            end if
                
            end if

        end select

    end subroutine


    subroutine processZoaFileInput(iptStr, printOnly)
        use globals, only: TEST_MODE
        use zoa_file_handler
        implicit none
        character(len=*) :: iptStr
        logical, optional :: printOnly
        integer :: locStr, locDot
        character(len=1024) :: fileName

            ! Look for special code
        locStr = index(iptStr, uppercase('zoa_macro:'))

        if (locStr .ne. 0) then 
            fileName = iptStr(len('zoa_macro:')+1:len_trim(iptStr))
            locDot = INDEX(fileName, '.')
            if (locDot == 0) then
                fileName = trim(fileName)//'.zoa'
            end if
            fileName = trim(getMacroDir())//trim(fileName)
            if (doesFileExist(trim(fileName))) then
            if (present(printOnly)) then
                call process_zoa_file(trim(fileName), printOnly=.TRUE.)
            else 
                call process_zoa_file(trim(fileName))
            end if
            end if ! Does file exist

        else ! Look in main directory
            fileName = trim(iptStr)
            locDot = INDEX(fileName, '.')
            if (locDot == 0) then
                fileName = trim(fileName)//'.zoa'
            end if    

            fileName = getRestoreFilePath(trim(fileName))

            if (doesFileExist(trim(fileName))) then
                if (present(printOnly)) then
                    call process_zoa_file(trim(fileName), printOnly=.TRUE.)
                else
                    call process_zoa_file(trim(trim(fileName)))
                end if   
            end if
                    
        end if        


    end subroutine

    subroutine printFile(iptStr)
        !! Format:  PRT file.  Will output a file (if it exists) to the console
        !  file is either just a file name or with a special prefix to look in a specific folder.
        ! At present, two options are supported.  
        ! PRT file - looks for a file in the project folder
        ! PRT zoa_macro:file.  Looks for file in the macros directory
        use zoa_file_handler
        use command_utils, only: isInputNumber

        implicit none
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens, locStr, locDot
        character(len=256) :: fileName

        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens==2) then
            call processZoaFileInput(trim(tokens(2)), printOnly=.TRUE.)

        else
            call updateTerminalLog("Error!  Expect two inputs, eg PRT file or PRT zoa_macro:file", "red")


        end if

    end subroutine    

    !Format:  CMD sk wk fk rx ry 
    subroutine getRayData(iptStr)
        use DATLEN, only: RAYRAY
        use data_registers, only: setData
        
        character(len=*) :: iptStr
        character(len=LEN(iptStr)) :: savStr
        character(len=80) :: tokens(40)
        integer :: numTokens       
        integer, allocatable :: fields(:), wavelengths(:), surfaces(:)
        real(kind=real64) :: relApeX, relApeY
        
        ! Defaults
        relApeX = 0.0
        relApeY = 0.0

        call parse(trim(iptStr), ' ', tokens, numTokens) 
        savStr = iptStr
        
        fields      = cmd_parser_get_int_input_for_prefix('f', tokens(1:numTokens))
        wavelengths = cmd_parser_get_int_input_for_prefix('w', tokens(1:numTokens))
        surfaces    = cmd_parser_get_int_input_for_prefix('s', tokens(1:numTokens))
        call cmd_parser_get_real_pair(tokens(1:numTokens), relApeX, relApeY, &
        & real1Bounds=[-1.0,1.0], real2Bounds=[-1.0,1.0])

        if (size(fields) ==1 .and. size(wavelengths) == 1 .and. size(surfaces) == 1) then
            ! Call RSI with the same info given.  This is a bit clunky to reconvernt back to str
            ! but for now let's see how it works
            call PROCESSILENT("RSI f"//trim(int2str(fields(1)))// &
            &                    " w"//trim(int2str(wavelengths(1)))// &
            & " "//trim(real2str(relApeX))//" "//trim(real2str(relApeY)))

            select case(trim(tokens(1)))

            case('CY') ! Cosine Y angle
                call setData(savStr, RAYRAY(5,surfaces(1)))
                print *, "Data stored is ", RAYRAY(5,surfaces(1))
            case('CX') ! Cosine Y angle
                call setData(savStr, RAYRAY(4,surfaces(1)))
                print *, "Data stored is ", RAYRAY(4,surfaces(1))                

            end select


        else
            call updateTerminalLog( &
            & "Error:  Was unable to parse intput to get only one surface, field point and wavelength","red")

        end if

    end subroutine


        !        
    !     (iptCmd)
    !     use command_utils, only : checkCommandInput, getInputNumber
    !     character(len=*) :: iptCmd
    !     integer :: surfNum

    !     surfNum = getSurfNumFromSurfCommand(trim(iptCmd))

        
       
    !     if (checkCommandInput([ID_CMD_NUM], max_num_terms=3)) then
    !         call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
    !         & '; RD, ' // real2str(getInputNumber(1))//";TH, "// &
    !         & real2str(getInputNumber(2)))
    !     end if            

    !   end subroutine

    function getDefaultMaxFrequency() result(maxFreq)
        use DATSPD
        real :: FREQ1, FREQ2, maxFreq
        logical :: ERROR
        ERROR=.FALSE.
        CALL CUTTOFF(FREQ1,FREQ2,ERROR)
      IF(ERROR) THEN 
         call updateTerminalLog('ERROR IN OBJECT/IMAGE SPACE FREQUENCY RELATIONSHIP', "red")
         return
      END IF        
!     FREQ1 IS THE IMAGE  SPACE CUTOFF FREQ
!     FREQ2 IS THE OBJECT SPACE CUTOFF FREQ
      IF(SPACEBALL.EQ.1) THEN
         maxFreq=FREQ2
      ELSE
         maxFreq=FREQ1
      END IF

      print *, "Max Frequency is ", maxFreq

    end function

end module
