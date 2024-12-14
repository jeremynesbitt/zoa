submodule (codeV_commands) mod_sur
implicit none
contains
module procedure execSUR
    ! for now support SUR SA only
    ! New code - add abstraction of row titles and new columns of RMD GLA CCY THC GLC
                
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str, blankStr, real2str
        use handlers, only: updateTerminalLog
        use global_widgets, only: curr_lens_data
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


                    if (glaTxt(1:4).EQ.'REFL') then
                        rmdTxt = 'REFL'//blankStr(6)
                    else
                        rmdTxt = blankStr(len(rmdTxt))
                    end if

                    glaTxt = getGlassText(ii)


                    fullLine = surfTxt//blankStr(4)//trim(radTxt)// &
                    & blankStr(4)//trim(thiTxt)//blankStr(5)//rmdTxt &
                    & //glaTxt//ldm%getCCYCodeAsStr(ii-1)//blankStr(5)//ldm%getTHCCodeAsStr(ii-1)//blankStr(5)         


                    call updateTerminalLog(trim(fullLine), "black")


                end do



            else

            call updateTerminalLog("SUR Should have a surface identifier (S0, Sk, Si, SA)", "red")
            end if

        else
            call updateTerminalLog("No Surface identifier given!  Please try again", "red")

        end if



    end procedure

    function getGlassText(surf) result(glaTxt)
        use global_widgets, only: curr_lens_data
        use type_utils, only: blankStr
        implicit none
        character(len=15) :: glaTxt
        integer, intent(in) :: surf

        glaTxt = blankStr(len(glaTxt))! Initialize


        glaTxt = curr_lens_data%glassnames(surf)


        if (glaTxt(1:1).NE.' ') then
        !else
            glaTxt = trim(curr_lens_data%glassnames(surf))//'_'//trim(curr_lens_data%catalognames(surf))
        end if 

        if (glaTxt(1:4).EQ.'REFL') then
            glaTxt = blankStr(len(glaTxt))         
        end if   

    end function

end submodule