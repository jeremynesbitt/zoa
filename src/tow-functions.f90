module tow_functions
    use iso_c_binding, only:  c_ptr, c_null_char
    use global_widgets, only: ioConfig
    use handlers, only: zoatabMgr
    use plot_setting_manager

    
contains


subroutine initializeGoDataTab(psm,plot_code, plotName, replot, objIdx) 

    use type_utils, only: int2str

    implicit none
    type(zoaplot_setting_manager) :: psm
    integer :: plot_code
    character(len=*) :: plotName

    character(len=1024) :: tabName, inputCmd
    integer :: pIdx
    integer, intent(out) :: objIdx
    logical, intent(out) :: replot


    pIdx = psm%plotNum
    inputCmd = trim(psm%generatePlotCommand())
    replot = .FALSE.
    if (pIdx /= -1 ) then
       replot = zoatabMgr%doesPlotExist_new(plot_code, objIdx, pIdx)
    end if


    !replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx)
    if (replot) then
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      call zoatabMgr%clearDataTab(objIdx)
     else
      pIdx = zoatabMgr%getNumberOfPlotsByCode(plot_code)
     
      psm%plotNum = pIdx+1 ! Noreplot so this is the next num

      !TODO:  Fix this.  need to check if basecmd is multiple pieces or not
      psm%baseCmd = trim(psm%baseCmd)//" P"//int2str(psm%plotNum)
      inputCmd = trim(psm%generatePlotCommand())
      tabName = plotName
      if  (psm%plotNum > 1) then
        tabName = trim(tabName)//" "//int2str(psm%plotNum)
      end if  
      
      objIdx = zoatabMgr%addDataTab(plot_code, &
      & trim(tabName)//c_null_char)
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
    end if


  end subroutine

  subroutine finalizeGoDataTab(psm, replot, objIdx)

    implicit none
    type(zoaplot_setting_manager) :: psm

    integer :: objIdx
    logical :: replot

    if(replot .EQV. .FALSE. ) then 
      call zoaTabMgr%finalize_with_psm(objIdx, psm)
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
    end if

  end subroutine

! At present, this will generate a zoadatatab, execute all of the commands in cmdTOW, and then redirect
! output to standard dialog
! eventually will look for special commands and generate settings for them
subroutine tow_go(psm, cmdTOW)
    use zoa_ui
    use type_utils, only: int2str, str2int
    use plot_functions, only: getTabTextView

    IMPLICIT NONE

    type(zoaplot_setting_manager) :: psm
    character(len=*) :: cmdTOW

    integer :: objIdx
    logical :: replot


    call initializeGoDataTab(psm,ID_TOW_TAB, "Output Data", replot, objIdx)
    
    call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    ! TODO:  This does not support mutiple windows.  Need to fix this after initial testing
    call LogTermDebug("About to process"//cmdTOW)
    CALL PROCESKDP(cmdTOW)
    call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

    call finalizeGoDataTab(psm, replot, objIdx)


end subroutine

end module