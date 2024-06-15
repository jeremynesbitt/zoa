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
    use iso_fortran_env, only: real64



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
    type(zoa_cmd), dimension(531) :: zoaCmds

    integer :: cmd_loop = 0
    integer, parameter :: VIE_LOOP = 1
    integer, parameter :: DRAW_LOOP = 2 ! While plot is being drawn

    contains

    subroutine initializeCmds()
        ! This is called when the program is initialized (currently INITKDP.FOR)
        use global_widgets, only: ioConfig
        use hl_gtk_zoa, only : hl_zoa_text_view_new
        use zoa_ui, only: ID_TERMINAL_KDPDUMP
        use type_utils, only: int2str

        integer :: i

        ! Initialize textView to dump KDP print statements when needed
        call ioConfig%registerTextView(hl_zoa_text_view_new(), ID_TERMINAL_KDPDUMP)

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
        zoaCmds(518)%cmd = 'CLI'
        zoaCmds(518)%execFunc => execCLI                            
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

    end subroutine

    function startCodeVLensUpdateCmd(iptCmd) result(boolResult)
        use GLOBALS, only:  currentCommand

        character(len=*) :: iptCmd
        integer :: ii
        logical :: boolResult

        include "DATMAI.INC"

        boolResult = .FALSE.

        ! IF(iptCmd.EQ.'TIT') THEN
        !         CALL setLensTitle()
        !         return
        !       END IF   
        ! IF(iptCmd.EQ.'YAN') THEN
        !         CALL setField('YAN')
        !         return
        !       END IF   
        ! IF(iptCmd.EQ.'WL') THEN
        !         CALL setWavelength()
        !         return
        !       END IF    
        ! IF(iptCmd.EQ.'SO'.OR.iptCmd.EQ.'S') then
        !         CALL setSurfaceCodeVStyle(iptCmd)
        !         return
        !       END IF          
        ! IF(isSurfCommand(iptCmd)) then
        !         CALL setSurfaceCodeVStyle(iptCmd)
        !         return
        !       END IF                         
        ! IF(iptCmd.EQ.'GO') then
        !         CALL executeGo()
        !         return
        !       END IF  
        ! select case (iptCmd)
        
        ! Temp code for interface check
        print *, "iptCmd is ", iptCmd
        print *, "CurrentCommand is ", currentCommand
        do ii=1,size(zoaCmds)
        if (iptCmd == zoaCmds(ii)%cmd) then
            !PRINT *, "About to crash with fcn pointer?"
            call zoaCmds(ii)%execFunc(currentCommand)
            boolResult = .TRUE.
            return
        end if
        end do

        select case (iptCmd)

        ! case('WL')
        !     CALL setWavelength()
        !     boolResult = .TRUE.
        !     return            
        ! case('SO','S')
        !     CALL setSurfaceCodeVStyle(iptCmd)
        !     boolResult = .TRUE.
        !     return            
        case('GO')
            CALL executeGo()
            boolResult = .TRUE.
            return

        case('TIT') 
            CALL setLensTitle()
            boolResult = .TRUE.
            return            
        case ('DIM')
            call setDim()
            boolResult = .TRUE.
            return 
        case ('THI')
            call setThickness()
            boolResult = .TRUE.
            return 
        case ('RDY')
            call setRadius()
            boolResult = .TRUE.
            return  
        ! case ('INS')
        !     call insertSurf()
        !     boolResult = .TRUE.
        !     return      
        case ('GLA')
            call setGlass()
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
        case ('CUY')
            call setCurvature()
            boolResult = .TRUE.
            return             
        case ('DEL')
            call deleteStuff()
            boolResult = .TRUE.
            return                 

        case ('RED')
            call setMagSolve()
            boolResult = .TRUE.
            return  

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
        use type_utils, only: str2real8
        use handlers, only: updateTerminalLog
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
        use type_utils, only: str2int
        use handlers, only: updateTerminalLog
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
        use type_utils, only: str2int
        implicit none
        character(len=*) :: prefix
        character(len=*), dimension(:) :: tokens
        integer, allocatable :: intArr(:)
        integer :: i
        logical :: userDefined 

        userDefined = .FALSE.
        
        if(size(tokens) > 1) then
            do i=2,size(tokens)
                if (tokens(i)(1:len(prefix)) == prefix ) then
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

    ! This is a very important command that performs ray traces for a user defined
    ! number of field points, wavelengths, and relative aperture locations
    ! The output depends on the cmd level
    ! For normal operation a table will be output with the results
    ! If in VIE level it will save the rays intersection coordinates for use with plooting
    ! Format RSI fi..k wi..k relApeX relApeY
    subroutine execRSI(iptStr)
        use global_widgets, only: sysConfig
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use type_utils, only: real2str, int2str
        use handlers, only: updateTerminalLog
        use type_utils, only: int2str
    
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
        use strings
        use global_widgets, only: curr_par_ray_trace
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use type_utils, only: real2str, int2str, str2int, blankStr
        use handlers, only: updateTerminalLog
        use global_widgets, only:  sysConfig
        use mod_plotopticalsystem
        use DATLEN, only: COLRAY
    
        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        real(kind=real64) :: relAngle
        integer :: numTokens, i, jj, numRays
        integer, allocatable :: fields(:)
        character(len=5) :: plotOrientation
        logical :: goodCmd, yzFlag
      
        call updateTerminalLog("UMY"//blankStr(10)//"HMY"//blankStr(10)//"UCY"//blankStr(10)//"HCY", "black")
        do i=1,curr_lens_data%num_surfaces
            call updateTerminalLog(trim(real2str(curr_par_ray_trace%marginal_ray_angle(i)))//blankStr(2)// &
            &                      trim(real2str(curr_par_ray_trace%marginal_ray_height(i)))//blankStr(2)// &
            &                      trim(real2str(curr_par_ray_trace%chief_ray_angle(i)))//blankStr(2)// &
            &                      trim(real2str(curr_par_ray_trace%chief_ray_height(i))), "black" )


        end do

    end subroutine


    ! Todo:  move this somewhere once it is properly written
    function adjustImageFocus(x) result(f)

       use type_utils, only: real2str
       use DATMAI, only: REG
       implicit none
       double precision :: f
       double precision, intent(in):: x


       call PROCESKDP('THI SI '//real2str(x))
       call PROCESKDP('FOB 0; CAPFN; SHO RMSOPD')
       f = REG(9)


    end function


    subroutine findBestFocus(iptStr)
        use strings
        use global_widgets, only: curr_par_ray_trace
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use type_utils, only: real2str, int2str, str2int, blankStr
        use handlers, only: updateTerminalLog
        use global_widgets, only:  sysConfig
        use mod_plotopticalsystem
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
    subroutine execFAN(iptStr)
        use strings
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use type_utils, only: real2str, int2str, str2int
        use handlers, only: updateTerminalLog
        use global_widgets, only:  sysConfig
        use mod_plotopticalsystem
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
              goodCmd = .TRUE.
              call LogTermFOR("Addint Custom Cmd "//iptStr)
              call ld_settings%addCustomRayCmd(iptStr)
   
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
                          call VIE_NEW_TRACERAY(0.0d0, relAngle, sysConfig%refWavelengthIndex, ld_settings)
                        else
                            call VIE_NEW_TRACERAY(relAngle, 0.0d0,sysConfig%refWavelengthIndex, ld_settings)
                        end if

                      end do
                    end do
                case default
                    ! Do nothing
                call updateTerminalLog("FAN not used at base level", "black")
                end select


    end subroutine

    ! Currently inputs are either 
    ! VIE (new plot)
    ! VIE P1 (update existing plot)
    ! In the future when I support multiple plots then P1 will go to PX
    subroutine execVie(iptStr)
        
        use mod_plotopticalsystem
        use strings

        implicit none
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens) 
        PRINT *, "iptStr is ", iptStr
        if (numTokens  == 2) then
           if (tokens(2) == 'P1') then
            ! Do not update lens draw settings
            cmd_loop = VIE_LOOP
            return
           end if
        end if

        !if(len(trim(iptStr)) > 3 ) then
        !    call LogTermFOR("Warning; VIE accepts no input.  This will be ignored: "//trim(iptStr(4:len(iptStr))))
        !end if

        ! Enter into VIE loop.  Once GO is entered, execute plot
        cmd_loop = VIE_LOOP
        ld_settings = lens_draw_settings()

        ! Psuedocode
        ! curr_lens_settings = lens_settings_new
        
    end subroutine

    subroutine execCIR(iptStr)
        use strings
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use type_utils, only: real2str, int2str
        use handlers, only: updateTerminalLog
    
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
                call executeCodeVLensUpdateCommand('CLAP '//trim(tokens(2)),.TRUE.)
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
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
    
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens == 2 ) then
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; ASTOP; REFS')
            else

            call updateTerminalLog("STO Should have a surface identifier (S0, Sk, Si, SA)", "red")
            end if

        else
            call updateTerminalLog("No Surface identifier given!  Please try again", "red")

        end if



    end subroutine

    subroutine execSUR(iptStr)
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str, blankStr, real2str
        use handlers, only: updateTerminalLog
        use global_widgets, only: curr_lens_data
    
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: ii
        character(len=80) :: tokens(40)
        character(len=256) :: fullLine
        character(len=4)  :: surfTxt
        character(len=23) :: radTxt
        character(len=23) :: thiTxt
        character(len=40) :: glaTxt
        integer :: numTokens

        !numSurfaces = curr_lens_data%num_surfaces
        !call LogTermFOR("Num Surfaces is "//trim(int2str(curr_lens_data%num_surfaces)))

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens == 2 ) then
            if (isSurfCommand(trim(tokens(2)))) then
                call logTermFOR("SUR Cmd here!")
                ! SA
                fullLine = blankStr(10)//"RDY"//blankStr(10)//"THI"//blankStr(5)//"RMD"//blankStr(5)//"GLA"
                call updateTerminalLog(trim(fullLine), "black")
                do ii=1,curr_lens_data%num_surfaces
                    surfTxt = blankStr(2)//trim(int2str(ii-1))//":"
                    ! Special Cases
                    if(ii==1)                           surfTxt = "OBJ:"
                    if(ii==curr_lens_data%ref_stop)     surfTxt = "STO:"
                    if(ii==curr_lens_data%num_surfaces) surfTxt = "IMG:"
                    if (curr_lens_data%radii(ii) == 0) then
                        radTxt = 'INFINITY'
                    else
                        radTxt = real2str(curr_lens_data%radii(ii),5)
                    end if
                    if (curr_lens_data%thicknesses(ii) > 1e10) then
                        thiTxt = 'INFINITY'
                    else
                        
                        if (curr_lens_data%thicknesses(ii) < 0) then
                            thiTxt = real2str(curr_lens_data%thicknesses(ii),3)
                        else
                            thiTxt = real2str(curr_lens_data%thicknesses(ii),5)
                        end if

                    end if
                    glaTxt = curr_lens_data%glassnames(ii)
                    !TODO:  make this more elegant
                    if (glaTxt(1:1).NE.' ') then
                    !else
                        glaTxt = trim(curr_lens_data%glassnames(ii))//'_'//trim(curr_lens_data%catalognames(ii))
                    end if 
                    PRINT *, "glaTxt is ", glaTxt(1:4)
                    PRINT *, "glaTxt check is ", (glaTxt(1:4).EQ.'REFL')
                    if (glaTxt(1:4).EQ.'REFL') then
                        glaTxt = ' '
                        fullLine = surfTxt//blankStr(4)//trim(radTxt)// &
                        & blankStr(4)//trim(thiTxt)//blankStr(5)//'REFL' &
                        & //blankStr(5)//trim(glaTxt)         
                    else

                        fullLine = surfTxt//blankStr(4)//trim(radTxt)// &
                        & blankStr(4)//trim(thiTxt)//blankStr(10)// &
                        & trim(glaTxt)
                    end if                                       


                    call updateTerminalLog(trim(fullLine), "black")


                end do



            else

            call updateTerminalLog("SUR Should have a surface identifier (S0, Sk, Si, SA)", "red")
            end if

        else
            call updateTerminalLog("No Surface identifier given!  Please try again", "red")

        end if



    end subroutine

    subroutine execRestore(iptStr)
        !use global_widgets, only: curr_lens_data

        use zoa_file_handler
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        use zoa_file_handler, only: open_file_to_sav_lens
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=80) :: tokens(40)
        character(len=1024) :: fullPath
        integer :: numTokens, locDot

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
            if (len(trim(fullPath)) > 1) then
                call process_zoa_file(fullPath)
            else
            call updateTerminalLog("File Not found in known directory!  Please try again or use UI", "red")
            end if
                   


        else
            call updateTerminalLog("No file given to restore!  Please try again", "red")

        end if



    end subroutine
      

    subroutine execSAV(iptStr)
        use global_widgets, only: sysConfig, curr_lens_data

        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        use zoa_file_handler, only: open_file_to_sav_lens
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        character(len=80) :: tokens(40)
        integer :: numTokens, locDot, fID

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        call LogTermFOR("Number of tokens is "//int2str(numTokens))
        if (numTokens > 1 ) then
            fName = trim(tokens(2))
            locDot = INDEX(fName, '.')
            if (locDot == 0) then
                fName = trim(fName)//'.zoa'
            end if
            call LogTermFOR("File name to save is "//trim(fName))
        else
            fName = 'default.zoa'

        end if
        fID = open_file_to_sav_lens(fName)
        if (fID /= 0) then
         
           call sysConfig%genSaveOutputText(fID)
           call curr_lens_data%genSaveOutputText(fID)
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
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
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
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

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
        use type_utils, only: int2str, str2real8, real2str
        use global_widgets, only: curr_lens_data, curr_par_ray_trace
        use handlers, only: updateTerminalLog        
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

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

    subroutine setMagSolve()
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')

        call executeCodeVLensUpdateCommand('CHG 0; REDSLV '//trim(tokens(2)))          

    end subroutine

    ! Format:  DEL SOL CUY S2
    !          DEL PIM
    subroutine deleteStuff()
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use global_widgets, only: curr_lens_data
        use handlers, only: updateTerminalLog
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')
        ! This nested select statements is not sustainable.  Need a more elegant way of parsing this
        ! command and figuring out what commands to translate it to
        if(numTokens > 1 ) then
        select case(trim(tokens(2))) 
            case('PIM')
                call updateTerminalLog("Deleting PIM", "blue")
                surfNum = curr_lens_data%num_surfaces - 2
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; TSD ')          

            case('SOL') ! Delete Solves
                if(numTokens > 2) then
                    call updateTerminalLog("Deleting Solve", "blue")
                    select case(trim(tokens(3)))
                    case('CUY')
                        if (isSurfCommand(trim(tokens(4)))) then
                            surfNum = getSurfNumFromSurfCommand(trim(tokens(4)))
                            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                            & '; CSDY ')                            
                        end if
                    end select
                else
                    call updateTerminalLog("No Angle Solve Specified.  Please try again", "red")
                    end if 
                

            end select

        end if

    end subroutine

    ! Format:  CUY Sk SOLVETYPE VAL
    subroutine setCurvature()
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')
        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (numTokens > 2) then
               if (isInputNumber(trim(tokens(3)))) then ! FORMAT: CUY Sk VAL
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; CV, ' // trim(tokens(3)))
               else                 
                

               select case (trim(tokens(3)))
               case('UMY')
                PRINT *, "In the right place!  How exciting!!"
                PRINT *, "numTokens is ", numTokens
                if (numTokens > 3 ) then
                    call updateTerminalLog("Give it a try!", "blue")
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; PUY, ' // trim(tokens(4))) 
                end if

               end select 
            end if ! Tokens > 2 loop
            else
                call updateTerminalLog("No Angle Solve Specified.  Please try again", "red")
            end if
         
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if          

    end subroutine

    subroutine setEPD()
        use command_utils
        use type_utils, only: real2str
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
        use type_utils, only: int2str
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
        use type_utils, only: real2str
        use iso_fortran_env, only: real64       
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


    ! format:  GLA Sk GLASSNAME
    subroutine setGlass()
        use command_utils, only : checkCommandInput, getInputNumber, parseCommandIntoTokens, isInputNumber
        use glass_manager, only: parseModelGlassEntry
        use type_utils, only: real2str, int2str
        use handlers, only: updateTerminalLog
        use iso_fortran_env, only: real64

        !character(len=*) :: iptCmd
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

        !call updateTerminalLog("Starting to update GLA ", "blue" )

        call parseCommandIntoTokens(trim(INPUT), tokens, numTokens, ' ')

        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; '//trim(getSetGlassText(trim(tokens(3))))//';GO')        
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if                

        

        PRINT *, "tokens(1) is ", trim(tokens(2))
        PRINT *, "tokens(2) is ", trim(tokens(3))
        

       
        ! if (checkCommandInput([ID_CMD_NUM], max_num_terms=2)) then
        !     surfNum = INT(getInputNumber(1))
        !     call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
        !     & '; RD, ' // real2str(getInputNumber(2)))
        ! end if                    

    end subroutine

    subroutine setThickness()
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')
        PRINT *, "Token is ", trim(tokens(2))
        if(isSurfCommand(trim(tokens(2)))) then
            PRINT *, "Token is ", trim(tokens(2))
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; TH, ' // trim(tokens(3))//';GO')          
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if       

    end subroutine
    
    !Format RDY Sk Val
    subroutine setRadius()
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        include "DATMAI.INC"

        call parseCommandIntoTokens(INPUT, tokens, numTokens, ' ')

        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(3))//';GO')          
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if             
       
    end subroutine

    ! NBR ELE Si..j only supported
    subroutine execNBR(iptStr)
        use strings
        use handlers, only: updateTerminalLog
        use mod_plotopticalsystem, only: ld_settings
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: boolResult
        integer, allocatable :: surfaces(:)
        !include "DATLEN.INC"

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
                        ld_settings%start_surface = surfaces(1)
                        ld_settings%end_surface = surfaces(size(surfaces))
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
        use strings
        use type_utils, only: int2str, real2str, blankStr
        use handlers, only: updateTerminalLog
        use global_widgets, only: curr_lens_data
        use iso_fortran_env, only: real64
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
        use strings
        use type_utils, only: int2str, str2real8, str2int
        use handlers, only: updateTerminalLog
        use iso_fortran_env, only: real64
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



    subroutine setDim()
        use command_utils
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
        use handlers, only: zoatabMgr, updateTerminalLog
        use globals, only: basePath, TEST_MODE
      
        implicit none  
      
      
        integer :: resp
        character(len=80), dimension(3) :: msg

        ! Temp vars
        integer :: ios, n
        character(len=200) :: line
        
        ! Only ask user for input if not in test mode
        if (TEST_MODE.eqv..FALSE.) then
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
          end if
      
            ! Final question!  Ask the user if they want to close current tabs
            call zoatabMgr%closeAllTabs("dummy text at present")
      
            ! Finally at the new lens process.  
      
      
            ! call PROCESKDP('LENS')
            ! call PROCESKDP('WV, 0.635, 0.0, 0.0, 0.0, 0.0')
            ! call PROCESKDP('UNITS MM')
            ! call PROCESKDP('SAY, 10.0')
            ! call PROCESKDP('CV, 0.0')
            ! call PROCESKDP('TH, 0.10E+21')
            ! call PROCESKDP('AIR')
            ! call PROCESKDP('CV, 0.0')
            ! call PROCESKDP('TH, 10.0')
            ! call PROCESKDP('REFS')
            ! call PROCESKDP('ASTOP')
            ! call PROCESKDP('AIR')
            ! call PROCESKDP('CV, 0.0')
            ! call PROCESKDP('TH, 1.0')
            ! call PROCESKDP('EOS')    
      
      else 
        ! If user aborted, log it
        call updateTerminalLog("New Lens Process Cancelled", "black")
      end if
      end if
      

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
            call LogTermFOR("About to exec "//trim(line))
            call PROCESKDP(trim(line))
          end if
          n = n + 1
      end do      
      close(unit=9)
      
      
      
      end subroutine    

      !TIT
      subroutine setLensTitle()
        use command_utils
        use kdp_utils, only: inLensUpdateLevel
        include "DATMAI.INC"

        call executeCodeVLensUpdateCommand('LI '// parseTitleCommand())

        ! if (inLensUpdateLevel()) then
        !     call PROCESKDP('LI '// parseTitleCommand())
        ! else
        !    call PROCESKDP('U L;LI '// parseTitleCommand()//';EOS')
        ! end if

      end subroutine

      subroutine setFieldWeights(iptStr)
        use handlers, only: updateTerminalLog
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        ! This is not implemented. 
        call updateTerminalLog("Field Weights Command "//trim(iptStr)//" &
        & Not supported", "black")

      end subroutine

      subroutine setWavelengthWeights(iptStr)
        use strings
        use type_utils, only: int2str, str2real8
        use handlers, only: updateTerminalLog
        use global_widgets, only: sysConfig
        use iso_fortran_env, only: real64
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
         use type_utils, only: int2str, str2real8
         use handlers, only: updateTerminalLog
         use kdp_utils, only: inLensUpdateLevel
         use global_widgets, only: sysConfig
         use iso_fortran_env, only: real64
         use strings
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

         do i=1,numTokens
            call LogTermFOR("Token "//trim(tokens(i)))
         end do

         ! TODO:  Support more field types
         if (tokens(1).EQ.'YAN'.OR.tokens(1).EQ.'YOB') FLD_COL = Y_COL
         if (tokens(1).EQ.'XAN'.OR.tokens(1).EQ.'XOB') FLD_COL = X_COL

         call sysConfig%setFieldTypeFromString(trim(tokens(1)))

          numFields = numTokens-1
          call LogTermFOR("Numfields is "//int2str(numFields))
          allocate(absFields(numFields))
          do i=1,numFields
            absFields(i) = str2real8(trim(tokens(i+1)))
          end do
          call sysConfig%setAbsoluteFields(absFields, FLD_COL)


         

         ! Exit lens update level if we are there, since KDP doesn't support field updating
         ! at that level
        !  if (inLensUpdateLevel()) call PROCESKDP('EOS')
        !  numFields = numTokens-1
        !  call PROCESKDP('FLDS MAX '//trim(int2str(numFields)))
        !  call LogTermFOR("cmd "//'FLDS MAX '//int2str(numFields))
        !  do i=1,numFields
        !     call PROCESKDP('FLDS '//trim(int2str(i))//' 0 '//trim(tokens(i+1)))
        !     call LogTermFOR("cmd "//'FLDS '//trim(int2str(i))//' 0 '//trim(tokens(i+1)))
            
        !  end do

       end subroutine   
       
       ! Old set field
    !     use command_utils
    !     use type_utils, only: real2str
    !     use kdp_utils, only: inLensUpdateLevel
    !     implicit none

    !     character(len=3) :: strCmd
    !     logical :: inputCheck

    !     PRINT *, "Setting Field"

    !     inputCheck = checkCommandInput([ID_CMD_NUM])
    !     if (inputCheck) then

    
    !     select case (strCmd)
    !     case('YAN')
    !         call executeCodeVLensUpdateCommand('SCY FANG,' // real2str(getInputNumber(1)))
    
    !         ! if (inLensUpdateLevel()) then
    !         !     PRINT *, 'SCY FANG,' // real2str(getInputNumber(1))
    !         !     call PROCESKDP('SCY FANG,' // real2str(getInputNumber(1)))
    !         ! else
    !         !     call PROCESKDP('U L;SCY FANG, '// real2str(getInputNumber(1))//';EOS')
    !         ! end if

    !     end select
    ! end if

    !   end subroutine

      subroutine setWavelength(iptStr)
       !TODO Support inputting up to 10 WL  See CV2PRG.FOR
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: real2str, str2real8, int2str
        use handlers, only: updateTerminalLog
        use global_widgets, only: sysConfig
        use strings
        implicit none

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=1024) :: outStr
        integer :: i
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(trim(iptStr), ' ', tokens, numTokens)
        !call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        call LogTermFOR("setWL numTokens is "//int2str(numTokens))

        if (numTokens <= 6) then
            outStr = 'WV, '
            do i=2,numTokens
                outStr = trim(outStr)//' '//trim(real2str(str2real8(trim(tokens(i)))/1000.0))
                ! Set spectral weights; assume all equal to 1.0 here
                call sysConfig%setSpectralWeights(i-1, 1.0)               
            end do
            call LogTermFOR("Outstr is "//trim(outStr))
            call executeCodeVLensUpdateCommand(trim(outStr))




        end if

      end subroutine      

      subroutine executeGo()
        use global_widgets, only: ioConfig
        !use zoa_ui, only: ID_TERMINAL_DEFAULT, ID_TERMINAL_KDPDUMP
        use zoa_ui
        use handlers, only: zoatabMgr
        use kdp_utils, only: inLensUpdateLevel
        use mod_plotopticalsystem, only: ld_settings



        if (cmd_loop == VIE_LOOP) then
                ! Hide KDP Commands from user
        
        ! Temp for testing
        ! Do nothing if in update loop
        if (inLensUpdateLevel()) then
            call LogTermFOR("Will not draw in lens update level")
            return
        end if    
        cmd_loop = DRAW_LOOP
        active_plot = ID_NEWPLOT_LENSDRAW
        call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
        call VIE_NEW_NEW(ld_settings)
        CALL PROCESKDP('DRAW')
        call ioConfig%setTextView(ID_TERMINAL_DEFAULT)  
        !PRINT *, "After draw loop"
        !call VIE_NEW(1)
        !call LogTermFOR("Done with VIE NEW NEW")
        
        !call PROCESKDP('VIECO')
        ! CALL VIE_NEW(curr_lens_settings)
            
        cmd_loop = 0 ! Go back to base level
        end if



        if (inLensUpdateLevel()) then 
            call PROCESKDP('EOS')
            !call zoatabMgr%replotifneeded()
        end if

      end subroutine

      function isSurfCommand(tstCmd) result(boolResult)
        use type_utils, only: int2str
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
        use type_utils, only: string
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

        tstCmds(1)%s = 'YAN'
        tstCmds(2)%s = 'TIT'
        !tstCmds(3)%s = 'WL'
        tstCmds(6)%s = 'GO'
        tstCmds(7)%s = 'DIM'
        tstCmds(8)%s = 'RDY'
        tstCmds(9)%s = 'THI'
        !tstCmds(10)%s = 'INS'
        tstCmds(11)%s = 'GLA'
        tstCmds(12)%s = 'PIM'
        tstCmds(13)%s = 'EPD'
        tstCmds(14)%s = 'CUY'
        tstCmds(15)%s = 'DEL'
        tstCmds(16)%s = 'RED'
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
        use zoa_ui, only: ID_TERMINAL_DEFAULT, ID_TERMINAL_KDPDUMP

        implicit none
        character(len=*) :: iptCmd
        logical, optional :: debugFlag, exitLensUpdate
        logical :: redirectFlag

        if(present(debugFlag)) then 
            redirectFlag = .NOT.debugFlag
        else
            redirectFlag = .TRUE.
        end if

        ! Hide KDP Commands from user
        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)
          

        if (inLensUpdateLevel()) then              
            call PROCESKDP(iptCmd)
        else
            !call PROCESKDP('U L;'// iptCmd //';EOS')
            ! Update - do not exit lens update level to better support stops
            ! clear apertures, etc 
            call PROCESKDP('U L;'// iptCmd )
        end if

        if(present(exitLensUpdate)) then
            if(exitLensUpdate) CALL PROCESKDP('EOS')
        end if

        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
      end subroutine

      function getSurfNumFromSurfCommand(iptCmd) result(surfNum)
        use type_utils, only: str2int, int2str
        use global_widgets, only: curr_lens_data
        use command_utils, only: isInputNumber
        character(len=*) :: iptCmd
        integer :: surfNum

        !print *, "IPTCMD is ", iptCmd
        !print *, "len of iptCmd is ", len(iptCmd)

        surfnum = -1 ! For error checking

        if(len(iptCmd).EQ.1) then ! It is S, which means we have to add a surface before
                                  ! the last surface.  
            surfNum = curr_lens_data%num_surfaces-1
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            &  "; INSK "//trim(int2str(surfNum)))
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
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        use strings
        
    
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
            & '; RD, ' // trim(tokens(2)), .TRUE.)              
        case (3) ! Curvature and thickness
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            ! KDP Does not allow setting image thickness.  So work around this for now
            if (trim(tokens(1)).EQ.'SI') then
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; RD, ' // trim(tokens(2)), .TRUE.)   
            else
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2))//";TH, "// &
            & trim(tokens(3)), .TRUE.)        
            end if    
        case (4) ! Curvature, thickness, and glass
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            if(.not.isSpecialGlass(trim(tokens(4)))) then

            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2))//";TH, "// &
            & trim(tokens(3))//'; '//trim(getSetGlassText(trim(tokens(4)))), .TRUE.) 
            else
                ! TODO:  This and the isSpecialGlass function should go somewhere else.
                ! but first need to figure out if I really want to store this info in 
                ! glassnames or create a new array for this info.
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; RD, ' // trim(tokens(2))//";TH, "// &
                & trim(tokens(3))//';' // trim(tokens(4)),.TRUE.)                
            end if
        end select
    end subroutine


        !        
    !     (iptCmd)
    !     use command_utils, only : checkCommandInput, getInputNumber
    !     use type_utils, only: real2str, int2str
    !     character(len=*) :: iptCmd
    !     integer :: surfNum

    !     surfNum = getSurfNumFromSurfCommand(trim(iptCmd))

        
       
    !     if (checkCommandInput([ID_CMD_NUM], max_num_terms=3)) then
    !         call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
    !         & '; RD, ' // real2str(getInputNumber(1))//";TH, "// &
    !         & real2str(getInputNumber(2)))
    !     end if            

    !   end subroutine

end module