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

end submodule