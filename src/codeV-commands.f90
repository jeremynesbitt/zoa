! THis module contains Code V style commands and translates them to KDP style commands

module codeV_commands

    character(len=4), dimension(500) :: surfCmds

    contains

    function startCodeVLensUpdateCmd(iptCmd) result(boolResult)

        character(len=*) :: iptCmd
        logical :: boolResult

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

        select case (iptCmd)

        case('YAN')
            CALL setField('YAN')
            boolResult = .TRUE.
            return
        case('WL')
            CALL setWavelength()
            boolResult = .TRUE.
            return            
        case('SO','S')
            CALL setSurfaceCodeVStyle(iptCmd)
            boolResult = .TRUE.
            return            
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
            call setThickness(iptCmd)
            boolResult = .TRUE.
            return 
        case ('RDY')
            call setRadius(iptCmd)
            boolResult = .TRUE.
            return  
        case ('INS')
            call insertSurf()
            boolResult = .TRUE.
            return                                     
        end select

        ! Handle Sk separately
        IF(isSurfCommand(iptCmd)) then
            CALL setSurfaceCodeVStyle(iptCmd)
            return
          END IF            
              
    end function

    subroutine setThickness(iptCmd)
        use command_utils, only : checkCommandInput, getInputNumber
        use type_utils, only: real2str, int2str
        character(len=*) :: iptCmd
        integer :: surfNum

       
        if (checkCommandInput([ID_CMD_NUM], max_num_terms=2)) then
            surfNum = INT(getInputNumber(1))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; TH, ' // real2str(getInputNumber(2)))
        end if                    

    end subroutine

    subroutine setRadius(iptCmd)
        use command_utils, only : checkCommandInput, getInputNumber
        use type_utils, only: real2str, int2str
        character(len=*) :: iptCmd
        integer :: surfNum

       
        if (checkCommandInput([ID_CMD_NUM], max_num_terms=2)) then
            surfNum = INT(getInputNumber(1))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // real2str(getInputNumber(2)))
        end if                    

    end subroutine

    subroutine insertSurf()
        use command_utils, only : checkCommandInput, getInputNumber, getQualWord
        use type_utils, only: real2str, int2str
        integer :: surfNum

        PRINT *, "Inside insertSurf"

        if (checkCommandInput([ID_CMD_QUAL])) then
            surfNum = getSurfNumFromSurfCommand(trim(getQualWord()))
            PRINT *, "About to try cmd ", 'INSK, '//trim(int2str(surfNum))
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
            call PROCESKDP(trim(line))
          end if
          n = n + 1
      end do      
      
      
      
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

      subroutine setField(strCmd)
        use command_utils
        use type_utils, only: real2str
        use kdp_utils, only: inLensUpdateLevel
        implicit none

        character(len=3) :: strCmd
        logical :: inputCheck

        PRINT *, "Setting Field"

        inputCheck = checkCommandInput([ID_CMD_NUM])
        if (inputCheck) then

    
        select case (strCmd)
        case('YAN')
            call executeCodeVLensUpdateCommand('SCY FANG,' // real2str(getInputNumber(1)))
    
            ! if (inLensUpdateLevel()) then
            !     PRINT *, 'SCY FANG,' // real2str(getInputNumber(1))
            !     call PROCESKDP('SCY FANG,' // real2str(getInputNumber(1)))
            ! else
            !     call PROCESKDP('U L;SCY FANG, '// real2str(getInputNumber(1))//';EOS')
            ! end if

        end select
    end if

      end subroutine

      subroutine setWavelength()
        !TODO Support inputting up to 10 WL  See CV2PRG.FOR
        use command_utils
        use type_utils, only: real2str
        use kdp_utils, only: inLensUpdateLevel
        implicit none

        logical :: inputCheck


        inputCheck = checkCommandInput([ID_CMD_NUM])
        if (inputCheck) then
            
            if (inLensUpdateLevel()) then               
                call PROCESKDP('WV, ' // real2str(getInputNumber(1)/1000.0))
            else
                call PROCESKDP('U L;WV, ' // real2str(getInputNumber(1)/1000.0)//';EOS')
            end if

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

        
        do i=1,size(surfCmds)
            surfCmds(i) = 'S'//trim(int2str(i))
            if(tstCmd.EQ.surfCmds(i)) then
                boolResult = .TRUE.
            end if

        end do



      end function

      function isCodeVCommand(tstCmd) result(boolResult)
        logical :: boolResult
        character(len=*) :: tstCmd
        character(len=3), dimension(10) :: codeVCmds
        integer :: i


        ! TODO:  Find some better way to do this.  For now, brute force it
        codeVCmds = [character(len=3) :: 'YAN', 'TIT', 'WL', 'SO','S','GO', 'DIM', 'RDY', 'THI', 'INS']
        boolResult = .FALSE.
        do i=1,size(codeVCmds)
            if (tstCmd.EQ.codeVCmds(i)) then
                boolResult = .TRUE.
                return
            end if
        end do
        ! If we've gotten here check if it is a surface command
        boolResult = isSurfCommand(tstCmd)

      end function

      subroutine executeCodeVLensUpdateCommand(iptCmd)
        use kdp_utils, only: inLensUpdateLevel
        implicit none
        character(len=*) :: iptCmd
        if (inLensUpdateLevel()) then               
            call PROCESKDP(iptCmd)
        else
            call PROCESKDP('U L;'// iptCmd //';EOS')
        end if
      end subroutine

      function getSurfNumFromSurfCommand(iptCmd) result(surfNum)
        use type_utils, only: str2int
        character(len=*) :: iptCmd
        integer :: surfNum

        print *, "IPTCMD is ", iptCmd
        print *, "len of iptCmd is ", len(iptCmd)

        if(len(iptCmd).EQ.1) then ! It is S, which is S1
            surfNum = 1
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


      subroutine setSurfaceCodeVStyle(iptCmd)
        use command_utils, only : checkCommandInput, getInputNumber
        use type_utils, only: real2str, int2str
        character(len=*) :: iptCmd
        integer :: surfNum

        surfNum = getSurfNumFromSurfCommand(trim(iptCmd))

        
       
        if (checkCommandInput([ID_CMD_NUM], max_num_terms=2)) then
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // real2str(getInputNumber(1))//";TH, "// &
            & real2str(getInputNumber(2)))
        end if            

      end subroutine

end module