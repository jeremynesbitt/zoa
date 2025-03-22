submodule (codeV_commands) mod_sur
implicit none
contains
module procedure execSUR
    ! for now support SUR SA only
    ! New code - add abstraction of row titles and new columns of RMD GLA CCY THC GLC
                
        use command_utils, only : parseCommandIntoTokens
        use mod_lens_data_manager
    
        implicit none

        !class(zoa_cmd) :: self
        integer :: ii
        character(len=80) :: tokens(40)
        character(len=256) :: fullLine
        character(len=4)  :: surfTxt
        character(len=23) :: radTxt
        character(len=23) :: thiTxt
        character(len=15) :: glaTxt
        character(len=10) :: rmdTxt
        integer :: numTokens

        !numSurfaces = curr_lens_data%num_surfaces
        !call LogTermFOR("Num Surfaces is "//trim(int2str(curr_lens_data%num_surfaces)))

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')
        if (numTokens == 2 ) then
            if (isSurfCommand(trim(tokens(2)))) then
                !call logTermFOR("SUR Cmd here!")
                ! SA
                fullLine = blankStr(10)//"RDY"//blankStr(10)//"THI"//blankStr(5)//"RMD"//blankStr(10)//"GLA" &
                & //blankStr(10)//"CCY"//blankStr(5)//"THC"//blankStr(5)//"GLC"
                call updateTerminalLog(trim(fullLine), "black")
                ! 12/12/24:  Change this to 0 indexed surfaees
                do ii=0,ldm%getLastSurf()
                    surfTxt = ldm%getSurfName(ii)//':'
                    if (ldm%getSurfRad(ii) == 0) then
                        radTxt = 'INFINITY'
                    else ! Should abstract this with THI
                        if(ldm%getSurfRad(ii) < 0) then
                           radTxt = real2str(ldm%getSurfRad(ii),3)
                        else
                           radTxt = real2str(ldm%getSurfRad(ii),5)
                        end if
                    end if
                    if (ldm%getSurfThi(ii) > 1e10) then
                        thiTxt = 'INFINITY'
                    else
                        
                        if (ldm%getSurfThi(ii) < 0) then
                            thiTxt = real2str(ldm%getSurfThi(ii),3)
                        else
                            thiTxt = real2str(ldm%getSurfThi(ii),5)
                        end if

                    end if
                    glaTxt = ldm%getGlassName(ii)
  

                    if (glaTxt(1:4).EQ.'REFL') then
                        rmdTxt = 'REFL'//blankStr(6)
                    else
                        rmdTxt = blankStr(len(rmdTxt))
                    end if

                    glaTxt = getGlassText(ii)


                    fullLine = surfTxt//blankStr(4)//trim(radTxt)// &
                    & blankStr(4)//trim(thiTxt)//blankStr(5)//rmdTxt &
                    & //glaTxt//ldm%getCCYCodeAsStr(ii)//blankStr(5)//ldm%getTHCCodeAsStr(ii)//blankStr(5)         


                    call updateTerminalLog(trim(fullLine), "black")


                end do



            else

            call updateTerminalLog("SUR Should have a surface identifier (S0, Sk, Si, SA)", "red")
            end if

        else
            call updateTerminalLog("No Surface identifier given!  Please try again", "red")

        end if



    end procedure

    module procedure setThickness
        use command_utils, only : parseCommandIntoTokens
        use DATMAI
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(iptStr, tokens, numTokens, ' ')
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

    end procedure

    ! format:  GLA Sk GLASSNAME
    module procedure setGlass
        use command_utils, only : checkCommandInput, getInputNumber, isInputNumber
        use glass_manager, only: parseModelGlassEntry
        use DATMAI
        use strings

        implicit none
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens


        !call updateTerminalLog("Starting to update GLA ", "blue" )
        call parse(iptStr, ' ', tokens, numTokens)

        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; '//trim(getSetGlassText(trim(tokens(3))))//';GO')        
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if                
               
    end procedure

    function getGlassText(surf) result(glaTxt)
        use type_utils, only: blankStr
        use mod_lens_data_manager
        implicit none
        character(len=15) :: glaTxt
        integer, intent(in) :: surf

        glaTxt = blankStr(len(glaTxt))! Initialize

        glaTxt = ldm%getGlassName(surf)
        !glaTxt = curr_lens_data%glassnames(surf)


        ! if (glaTxt(1:1).NE.' ') then
        ! !else
        !     glaTxt = trim(curr_lens_data%glassnames(surf))//'_'//trim(curr_lens_data%catalognames(surf))
        ! end if 

        if (glaTxt(1:4).EQ.'REFL') then
            glaTxt = blankStr(len(glaTxt))         
        end if   

    end function

    ! Format:  CUY Sk SOLVETYPE VAL
    module procedure setCurvature
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(iptStr, tokens, numTokens, ' ')
        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (numTokens > 2) then
               if (isInputNumber(trim(tokens(3)))) then ! FORMAT: CUY Sk VAL
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; CV, ' // trim(tokens(3))//";GO")
               else                 
                

               select case (trim(tokens(3)))
               case('UMY')
                PRINT *, "In the right place!  How exciting!!"
                PRINT *, "numTokens is ", numTokens
                if (numTokens > 3 ) then
                    call updateTerminalLog("Give it a try!", "blue")
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; PUY, ' // trim(tokens(4))//";GO") 
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

    end procedure

    !Format RDY Sk Val
    module procedure setRadius
        use strings
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(3))//';GO')          
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if             
       
    end procedure

    module procedure execAsphere
        ! This does nothing for now

    end procedure

    module procedure updateAsphereTerms
        use strings
        use DATLEN, only: ALENS
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        character(len=2) :: aspKDP
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)

        ! Copied from CV2PRG (essentially)
        select case(trim(tokens(1)))
        case('A')
           aspKDP =  'AD'
        case('B')
            aspKDP = 'AE'
        case('C')
            aspKDP = 'AF'
        case('D')
            aspKDP = 'AG'
        case('E')
            aspKDP = 'AH'
        case('F')
            aspKDP = 'AI'
        case('G')
            aspKDP = 'AJ'
        case('H')
            aspKDP = 'AK'                       

        end select
        call execTranslatedSurfCmd(iptStr, aspKDP)
        !call executeCodeVLensUpdateCommand(aspKDP//' '//trim(tokens(2)), debugFlag=.TRUE.)
        print *, "tried to execute ", aspKDP//' '//trim(tokens(2))
    end procedure   
    
    ! Format
    ! K Sk Val - update on lens Sk
    ! K Val - update current lens (eg when loading from file)
    ! K Sk - return val on current lens (not currently implemented) 
    module procedure updateConicConstant
        !use DATLEN, only: ALENS
        use command_utils, only: isInputNumber
        use mod_lens_data_manager
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        character(len=3) ::  kdpCmd = 'CC ' ! For future abstraction
        integer :: numTokens

        call execTranslatedSurfCmd(iptStr, 'CC')

        ! call parse(iptStr, ' ', tokens, numTokens)
        
        ! select case (numTokens)

        ! case(2) 
        !     if (isSurfCommand(trim(tokens(2)))) then
        !             surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
        !     else
        !         if (isInputNumber(trim(tokens(2)))) then 
        !             ! Use current surface
        !             surfNum = ldm%getSurfacePointer()                
        !             call executeCodeVLensUpdateCommand(kdpCmd//trim(tokens(2)))
        !             return 
        !         else
        !         call updateTerminalLog("Error! For "//kdpCmd//"expect second argument to be Sk &
        !         & or value to update for current lens pointer surface ", "red")
        !         return
        !         end if
        !     end if

        ! case(3) ! K Sk Val
        !     if (isSurfCommand(trim(tokens(2)))) then
        !         surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))  
        !     end if          
        !     if (isInputNumber(trim(tokens(3)))) then 
        !         call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
        !         & '; '//kdpCmd//trim(tokens(3)))   
        !         return 
        !     end if           
        ! end select



    end procedure

    module procedure updateSurfaceLabel
        call execTranslatedSurfCmd(iptStr, 'LBL')    
    end procedure

    ! Expected Format for iptStr:
    ! cmd Sk Val - update on lens Sk
    ! cmd Val - update current lens (eg when loading from file)
    ! cmd Sk - return val on current lens (not currently implemented) 
    ! kdpCmd - the translated command for cmd
    subroutine execTranslatedSurfCmd(iptStr, kdpCmd)
        use command_utils, only: isInputNumber, removeQuotes
        use mod_lens_data_manager
        
        character(len=*) :: iptStr
        character(len=*) :: kdpCmd
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        
        select case (numTokens)

        case(2) 
            if (isSurfCommand(trim(tokens(2)))) then
                    surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            else
                if (isInputNumber(trim(tokens(2)))) then 
                    ! Use current surface
                    surfNum = ldm%getSurfacePointer()                
                    call executeCodeVLensUpdateCommand(kdpCmd//' '//trim(tokens(2)))
                    return 
                else
                    ! Some commands are not numbers
                    if (trim(kdpCmd) == 'LBL') then 
                        surfNum = ldm%getSurfacePointer()    
                        tokens(2) = removeQuotes(trim(tokens(2)))
                        call executeCodeVLensUpdateCommand(kdpCmd//' '//trim(tokens(2)))   
                        return 
                    end if                       

                ! If not number and no special case then complain    
                call updateTerminalLog("Error! For "//trim(tokens(1))//"expect second argument to be Sk &
                & or value to update for current lens pointer surface ", "red")
                return
                end if
            end if

        case(3) ! K Sk Val
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))  
            end if          
            if (isInputNumber(trim(tokens(3)))) then 
                PRINT *, "ABout to execitue change for "//kdpCmd
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; '//kdpCmd//' '//trim(tokens(3)))   
                return 
            else
                ! Special case
                if (trim(kdpCmd) == 'LBL') then 
                    tokens(3) = removeQuotes(trim(tokens(3)))
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; '//kdpCmd//' '//trim(tokens(3)))                       
                end if                
            end if           
        end select


    end subroutine


end submodule