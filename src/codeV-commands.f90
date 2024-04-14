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



    type zoa_cmd
      character(len=8) :: cmd
      procedure (cmdImplementation), pointer, pass(self) :: execFunc

    end type

    abstract interface
    subroutine cmdImplementation (self,iptStr)
       import zoa_cmd
       class(zoa_cmd) :: self
       character(len=*) ::iptStr
       !real, intent (in) :: z
    end subroutine cmdImplementation 
 end interface    

    character(len=4), dimension(500) :: surfCmds
    type(zoa_cmd), dimension(512) :: zoaCmds

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
        zoaCmds(2)%cmd = "SAV"
        zoaCmds(2)%execFunc => execSAV
        zoaCmds(3)%cmd = "REF"
        zoaCmds(3)%execFunc => execSetWavelengthIndex  
        zoaCmds(4)%cmd = "RES"
        zoaCmds(4)%execFunc => execRestore 
        zoaCmds(5)%cmd = "VIE"
        zoaCmds(5)%execFunc => execVie    
        zoaCmds(6)%cmd = "SUR"
        zoaCmds(6)%execFunc => execSUR                                
        zoaCmds(7)%cmd = "STO"
        zoaCmds(7)%execFunc => execSTO    
        zoaCmds(8)%cmd = "S"
        zoaCmds(8)%execFunc => setSurfaceCodeVStyle    
        zoaCmds(9)%cmd = "SO"
        zoaCmds(9)%execFunc => setSurfaceCodeVStyle   
        zoaCmds(10)%cmd = 'WL'
        zoaCmds(10)%execFunc => setWavelength    
        zoaCmds(11)%cmd = 'STOP'
        zoaCmds(11)%execFunc => execSTO  
        zoaCmds(12)%cmd = 'YAN'
        zoaCmds(12)%execFunc => setField                            
        do i = 1,499
            zoaCmds(12+i)%cmd = 'S'//trim(int2str(i))
            zoaCmds(12+i)%execFunc => setSurfaceCodeVStyle    
        end do

    end subroutine

    function startCodeVLensUpdateCmd(iptCmd) result(boolResult)

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
        do ii=1,size(zoaCmds)
        if (iptCmd == zoaCmds(ii)%cmd) then
            !PRINT *, "About to crash with fcn pointer?"
            call zoaCmds(ii)%execFunc(INPUT)
            boolResult = .TRUE.
            return
        end if
        end do

        select case (iptCmd)

        ! case('YAN')
        !     CALL setField('YAN')
        !     boolResult = .TRUE.
        !     return
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
        case ('INS')
            call insertSurf()
            boolResult = .TRUE.
            return      
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

    subroutine execVie(self, iptStr)
        use global_widgets, only: ioConfig
        use zoa_ui, only: ID_TERMINAL_DEFAULT, ID_TERMINAL_KDPDUMP

        implicit none
        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        ! Temp Vars
        Integer :: iStart, iNew
        Real*8 :: rWait, rDT
        ! End Temp Vars

        
        ! Hide KDP Commands from user
        call ioConfig%setTextView(ID_TERMINAL_KDPDUMP) 
        call PROCESKDP('VIECO')
        call ioConfig%setTextView(ID_TERMINAL_DEFAULT)      


    end subroutine

    subroutine execSTO(self, iptStr)
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
    
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        integer :: surfNum
        character(len=80) :: tokens(40)
        character(len=256) :: fullPath
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

    subroutine execSUR(self, iptStr)
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str, blankStr, real2str
        use handlers, only: updateTerminalLog
        use global_widgets, only: curr_lens_data
    
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        integer :: surfNum, ii
        character(len=80) :: tokens(40)
        character(len=256) :: fullLine
        character(len=4)  :: surfTxt
        character(len=10) :: radTxt
        character(len=10) :: thiTxt
        character(len=40) :: glaTxt
        integer :: refStop
        integer :: numTokens, locDot, fID

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

    subroutine execRestore(self, iptStr)
        use global_widgets, only: sysConfig, curr_lens_data
        use zoa_file_handler
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        use zoa_file_handler, only: open_file_to_sav_lens
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        integer :: surfNum
        character(len=80) :: tokens(40)
        character(len=256) :: fullPath
        integer :: numTokens, locDot, fID

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
      

    subroutine execSAV(self, iptStr)
        use global_widgets, only: sysConfig, curr_lens_data

        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        use zoa_file_handler, only: open_file_to_sav_lens
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        integer :: surfNum
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

    subroutine execSetWavelengthIndex(self, iptStr)

        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        integer :: surfNum
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

    subroutine execRMD(self, iptStr)

        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        implicit none

        class(zoa_cmd) :: self
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

        integer :: surfNum
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
        use global_widgets, only: curr_lens_data
        use handlers, only: updateTerminalLog
        implicit none

        integer :: surfNum
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
        logical :: inputCheck

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
        & '; PY, 0')            


    end subroutine


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
        real(kind=real64) :: nd, vd

        include "DATMAI.INC"

        !call updateTerminalLog("Starting to update GLA ", "blue" )

        call parseCommandIntoTokens(trim(INPUT), tokens, numTokens, ' ')

        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (isInputNumber(trim(tokens(3)))) then ! Assume user entered model glass
                !PRINT *, "Model Glass Entered!"
                call LogTermFOR("Model Glass Entered! "//trim(tokens(3)))
                call parseModelGlassEntry(trim(tokens(3)), nd, vd)
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; MODEL D'//trim(tokens(3))//','//real2str(nd)//','//real2str(vd))    
            else ! Assume it is glass name          
            
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; GLAK ' // trim(tokens(3)), .TRUE.)
            end if            
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
            & '; TH, ' // trim(tokens(3)))          
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
            & '; RD, ' // trim(tokens(3)))          
        else
            call updateTerminalLog("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if             
       
    end subroutine

    subroutine insertSurf()
        use command_utils, only : checkCommandInput, getInputNumber, getQualWord
        use type_utils, only: real2str, int2str
        integer :: surfNum

        !PRINT *, "Inside insertSurf"
        ! TODO:  Add an error check for Sk in checkCommandInput

        if (checkCommandInput([ID_CMD_QUAL])) then
            surfNum = getSurfNumFromSurfCommand(trim(getQualWord()))
            call executeCodeVLensUpdateCommand('INSK, '//trim(int2str(surfNum)))
        end if            




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
        use globals, only: basePath
      
        implicit none  
      
      
        integer :: resp
        character(len=80), dimension(3) :: msg

        ! Temp vars
        integer :: ios, n
        character(len=200) :: line
      
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
      
      
            call PROCESKDP('LENS')
            call PROCESKDP('WV, 0.635, 0.0, 0.0, 0.0, 0.0')
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
      
      else 
        ! If user aborted, log it
        call updateTerminalLog("New Lens Process Cancelled", "black")
      end if
      

      ! Prototype for getting this from file
      PRINT *, "attempting to open ",trim(basePath)//'Macros/newlens.zoa'
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

      subroutine setField(self, iptStr)
        ! TODO:  Support things other than YAN
         use command_utils, only : parseCommandIntoTokens
         use type_utils, only: int2str
         use handlers, only: updateTerminalLog
         implicit none
 
         class(zoa_cmd) :: self
         character(len=*) :: iptStr
         character(len=1024) :: outStr
         integer :: i,numFields
         character(len=80) :: tokens(40)
         integer :: numTokens
 
         call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

         numFields = numTokens-1
         call PROCESKDP('FLDSMAX '//int2str(numFields))
         do i=1,numFields
            call PROCESKDP('FLDS '//int2str(i)//' 0 '//trim(tokens(i+1)))
            
         end do

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

      subroutine setWavelength(self, iptStr)
       !TODO Support inputting up to 10 WL  See CV2PRG.FOR
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: real2str, str2real8
        use handlers, only: updateTerminalLog
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=1024) :: outStr
        integer :: i
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        if (numTokens <= 6) then
            outStr = 'WV, '
            do i=2,numTokens
                outStr = trim(outStr)//' '//trim(real2str(str2real8(trim(tokens(i)))/1000.0))

            end do
            call LogTermFOR("Outstr is "//trim(outStr))
            call executeCodeVLensUpdateCommand(trim(outStr))

        end if

      end subroutine      

      subroutine executeGo()
        use kdp_utils, only: inLensUpdateLevel

        if (inLensUpdateLevel()) call PROCESKDP('EOS')

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
        character(len=4), dimension(19) :: codeVCmds
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
        tstCmds(10)%s = 'INS'
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

      subroutine executeCodeVLensUpdateCommand(iptCmd, debugFlag)
        use kdp_utils, only: inLensUpdateLevel
        use global_widgets, only: ioConfig
        use zoa_ui, only: ID_TERMINAL_DEFAULT, ID_TERMINAL_KDPDUMP

        implicit none
        character(len=*) :: iptCmd
        logical, optional :: debugFlag
        logical :: redirectFlag

        if(present(debugFlag)) then 
            redirectFlag = .NOT.debugFlag
        else
            redirectFlag = .TRUE.
        end if

        PRINT *, "redirect flag is ", redirectFlag

        ! Hide KDP Commands from user
        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)

        if (inLensUpdateLevel()) then               
            call PROCESKDP(iptCmd)
        else
            call PROCESKDP('U L;'// iptCmd //';EOS')
        end if

        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
      end subroutine

      function getSurfNumFromSurfCommand(iptCmd) result(surfNum)
        use type_utils, only: str2int, int2str
        use global_widgets, only: curr_lens_data
        character(len=*) :: iptCmd
        integer :: surfNum

        print *, "IPTCMD is ", iptCmd
        print *, "len of iptCmd is ", len(iptCmd)

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
        end if

        if(len(iptCmd).GT.1) then
            surfNum = str2int(iptCmd(2:len(iptCmd)))
            return
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


      subroutine setSurfaceCodeVStyle(self, iptStr)
        use command_utils, only : parseCommandIntoTokens
        use type_utils, only: int2str
        use handlers, only: updateTerminalLog
        
    
        implicit none

        class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=256) :: fName
        integer :: surfNum
        character(len=80) :: tokens(40)
        character(len=256) :: fullPath
        integer :: numTokens

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        select case(numTokens)
        case (1)
            call updateTerminalLog("No info given besides surface identifier!  Please try again", "red")
        case (2) ! Curvature only
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            call LogTermFOR("Cmd to parse is "//trim(iptStr))
        case (3) ! Curvature and thickness
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2))//";TH, "// &
            & trim(tokens(3)))            
        case (4) ! Curvature, thickness, and glass
            surfNum = getSurfNumFromSurfCommand(trim(tokens(1)))
            if(.not.isSpecialGlass(trim(tokens(4)))) then
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(2))//";TH, "// &
            & trim(tokens(3))//'; GLAK ' // trim(tokens(4)))
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