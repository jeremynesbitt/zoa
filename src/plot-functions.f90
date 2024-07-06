! collection of functions to call during "GO" to finish off plots
! Description of current plot design
! It is a lock and key design.  
! To create a new plot:
! Add a key in zoa-ui as an integer constant parameter.  Needs to be unique compared to other plots
!


module plot_functions


    contains


subroutine zern_go(psm)

    USE GLOBALS
    use command_utils
    use handlers, only: zoatabMgr, updateTerminalLog
    use global_widgets, only:  sysConfig
    use zoa_ui
    use zoa_plot
    use iso_c_binding, only:  c_ptr, c_null_char
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use plot_setting_manager
    use DATMAI


    IMPLICIT NONE

    character(len=23) :: ffieldstr
    character(len=1024) :: inputCmd
    integer :: ii, objIdx, minZ, maxZ, lambda
    integer :: maxPlotZ = 9, numTermsToPlot
    integer :: numPoints = 10
    integer :: pIdx
    logical :: replot
    type(multiplot) :: mplt
    type(zoaplot) :: zernplot
    type(c_ptr) :: canvas
    type(zoaplot_setting_manager) :: psm
    character(len=80) :: tabName


    character(len=5), allocatable :: zLegend(:)

    REAL, allocatable :: xdat(:), ydat(:,:)
    


    REAL*8 X(1:96)
    COMMON/SOLU/X


    numPoints = psm%getDensitySetting_new()
    call psm%getZernikeSetting_min_and_max(minZ, maxZ)
    numTermsToPlot = maxZ-minZ+1
    !TODO:  Should support a rank one array to set Zernikes
    ! Like this for just minZ and MaxZ
    ! do i = 1,numTermsToPlot
    !     zlist(i) = minZ-1+i
    ! end do
    ! doing this would allow for mor complex entrys by user, such as 4,9,15,25 

    call LogTermFOR("MinZ "//trim(int2str(minZ)))
    call LogTermFOR("MaxZ "//trim(int2str(maxZ)))

    !minZ = 5
    !maxZ = 9
    !numTermsToPlot = 5    
    

    lambda = psm%getWavelengthSetting_new()
    inputCmd = trim(psm%generatePlotCommand())
    
    !inputCmd = trim(psm%sp%getCommand())      

    ! Compute Values
    allocate(xdat(numPoints))
    allocate(ydat(numPoints,maxZ-minZ+1))
    allocate(zLegend(size(ydat,2)))


    do ii = 0, numPoints-1
      xdat(ii+1) = REAL(ii)/REAL(numPoints-1)
      write(ffieldstr, *) xdat(ii+1)
      CALL PROCESKDP("FOB "// ffieldstr)
      CALL PROCESKDP("CAPFN")
      write(ffieldstr, *) lambda
      CALL PROCESKDP("FITZERN, "//ffieldstr)

      !CALL PROCESKDP("SHO RMSOPD")
      xdat(ii+1) = REAL(xdat(ii+1)*sysConfig%refFieldValue(2))
      ydat(ii+1,1:numTermsToPlot) = real(X(minZ:maxZ),4)
    end do

  
    ! Prep PLot
    canvas = hl_gtk_drawing_area_new(size=[1200,500], &
    & has_alpha=FALSE)
   
    call mplt%initialize(canvas, 1,1)
   
    call zernplot%initialize(c_null_ptr, xdat,ydat(:,1), &
    & xlabel=trim(sysConfig%getFieldText())//c_null_char, &
    & ylabel="Coefficient [waves]"//c_null_char, &
    & title='Zernike Coefficients vs Field'//c_null_char)
    zLegend(1) = 'Z'//trim(int2str(minZ))
    do ii=2,numTermsToPlot
      call zernplot%addXYPlot(xdat, ydat(:,ii))
      call zernplot%setDataColorCode(2+ii)
      zLegend(ii) = 'Z'//trim(int2str(minZ+ii-1))
    end do

    call zernplot%addLegend(zLegend)
    call mplt%set(1,1,zernplot)

    call logDataVsField(xdat, ydat, zLegend)


    ! Check for Mulitple Plots

    pIdx = psm%plotNum

    replot = .FALSE.
    if (pIdx /= -1 ) then
       replot = zoatabMgr%doesPlotExist_new(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx, pIdx)
    end if


    !replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx)
    if (replot) then
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)
     else
      pIdx = zoatabMgr%getNumberOfPlotsByCode(ID_PLOTTYPE_ZERN_VS_FIELD)
     
      psm%plotNum = pIdx+1 ! Noreplot so this is the next num

      !TODO:  Fix this.  need to check if basecmd is multiple pieces or not
      psm%baseCmd = trim(psm%baseCmd)//" P"//int2str(psm%plotNum)
      tabName = "Zernike vs Field" 
      if  (psm%plotNum > 1) then
        tabName = trim(tabName)//" "//int2str(psm%plotNum)
      end if  
      objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_ZERN_VS_FIELD, &
      & trim(tabName)//c_null_char, mplt)

      call zoaTabMgr%finalize_with_psm_new(objIdx, psm, trim(inputCmd))
      call zoaTabMgr%finalizeNewPlotTab(objIdx)

    end if

end subroutine


subroutine vie_go(psm)
    USE GLOBALS
    use command_utils
    use handlers, only: updateTerminalLog
    use global_widgets, only:  sysConfig
    use zoa_ui
    use zoa_plot
    use iso_c_binding, only:  c_ptr, c_null_char
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use plot_setting_manager
    use DATMAI
    use g
    use handlers, only: zoatabMgr

    implicit none
    type(zoaplot_setting_manager) :: psm
    integer :: plot_code = ID_PLOTTYPE_LENSDRAW
    character(len=20) :: plotName = 'Lens Drawing'

    character(len=1024) :: inputCmd, tabName
    integer :: objIdx, pIdx
    logical :: replot


    call vie_psm(psm)



    pIdx = psm%plotNum
    inputCmd = trim(psm%generatePlotCommand())
    replot = .FALSE.
    if (pIdx /= -1 ) then
       replot = zoatabMgr%doesPlotExist_new(plot_code, objIdx, pIdx)
    end if


    !replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx)
    if (replot) then
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      call zoatabMgr%updateKDPPlotTab(objIdx)
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
      objIdx = zoatabMgr%addKDPPlotTab(plot_code, &
      & trim(tabName)//c_null_char)

      !objIdx = zoatabMgr%addGenericMultiPlotTab(plot_code, &
      !& trim(tabName)//c_null_char, mplt)

      call zoaTabMgr%finalize_with_psm_new(objIdx, psm, trim(inputCmd))
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
    end if

    


end subroutine


subroutine spo_go(psm)

    USE GLOBALS
    use command_utils
    use handlers, only: updateTerminalLog
    use global_widgets, only:  sysConfig
    use zoa_ui
    use zoa_plot
    use iso_c_binding, only:  c_ptr, c_null_char
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use plot_setting_manager
    use DATMAI
    use g


    IMPLICIT NONE

    type(multiplot) :: mplt
    type(zoaplot) :: xyscat1
    type(c_ptr) :: canvas
    type(c_ptr) ::  isurface
    type(zoaplot_setting_manager) :: psm

    integer :: iField, iLambda, iMethod, nRect, nRand, nRing


    call psm%getSpotDiagramSettings(iField, iLambda, iMethod, nRect, nRand, nRing)

    ! Hopefully temporary 
    call PROCESKDP(trim(getKDPSpotPlotCommand(iField, iLambda, iMethod, nRect, nRand, nRing)))

    ! Prep PLot
    canvas = hl_gtk_drawing_area_new(size=[700,500], &
    & has_alpha=FALSE)
    
    ! Todo:  change initialization to sepcify size, and then don't need to 
    ! call gtk_drawing_area
    call mplt%initialize(canvas, 1,1)

    ! TODO:  remove dependency on canvas here after checking it doesn't break anything.
    call xyscat1%initialize(c_null_ptr, REAL(pack(DSPOTT(1,:), &
    &                                   DSPOTT(1,:) /= 0 .and. DSPOTT(2,:) /=0)), &
    &                                   REAL(pack(DSPOTT(2,:), &
    &                                   DSPOTT(1,:) /= 0 .and. DSPOTT(2,:) /=0)), &
    !call xyscat1%initialize(c_null_ptr, REAL(DSPOTT(1,:)), &
    !&                                   REAL(DSPOTT(2,:)), &
    & xlabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    & ylabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    !& xlabel=' (x)'//c_null_char, ylabel='(y)'//c_null_char, &
    & title='Spot Diagram'//c_null_char)
    call xyscat1%setLineStyleCode(-1)


    call mplt%set(1,1,xyscat1)

    call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_SPOT_NEW, "Spot Diagram")

end subroutine


function getKDPSpotPlotCommand(iField, iLambda, iSpotCalcMethod, nGrid, nRand, nRing) result(plotCmd)
    use type_utils, only: int2str
    use zoa_ui
    use global_widgets, only: sysConfig
    implicit none
    integer, intent(in) :: iField, iLambda, iSpotCalcMethod
    integer, intent(in) :: nGrid, nRand, nRing

    character(len=80) :: charFLD
    character(len=80) :: charTrace
    character(len=1024) :: plotCmd
    integer :: i
  
    include "DATSP1.INC"

    WRITE(charFLD, *) "FOB ", &
    & sysConfig%relativeFields(2,iField) &
    & , ' ' , sysConfig%relativeFields(1,iField)
  
    call LogTermFOR("iSpotCalcMethod is "//int2str(iSpotCalcMethod))
    select case (iSpotCalcMethod)
    case (ID_SPOT_RAND)
      charTrace = "SPOT RAND;RANNUM "//trim(int2str(nRand))
    case (ID_SPOT_RECT)
      charTrace = "SPOT RECT;RECT "//trim(int2str(nGrid))
    case (ID_SPOT_RING)
      ! This is a bit of a hack.  redistribute ring number and rays per ring
      ! using KDP vars.  this should probably be moved to this type eventually
      do i=1,nRing
            RINGRAD(i) = (REAL(i)/nRing)*1D0
            RINGPNT(i) = INT(RINGRAD(i)*360)
      end do
     
      charTrace = "SPOT RING;RINGS "//int2str(nRing)

    end select
    plotCmd = trim(charFLD)//'; '//trim(charTrace)//";SPD "//trim(int2str(iLambda))
    call LogTermFOR("Plot Cmd is "//trim(plotCmd))
    PRINT *, "Plot command is ", trim(plotCMD)
  
  
end function

! This sub checks for whether a replot is needed or whether 
! this is a new plot, 
! If new plot, andcalls the finalize subs in 
! Zoa tab manager
subroutine finalizeGoPlot(mplt, psm, plot_code, plotName)
    use zoa_plot    
    use plot_setting_manager
    use handlers, only: zoatabMgr
    use type_utils, only: int2str

    implicit none
    type(multiplot) :: mplt
    type(zoaplot_setting_manager) :: psm
    integer :: plot_code
    character(len=*) :: plotName

    character(len=1024) :: inputCmd, tabName
    integer :: objIdx, pIdx
    logical :: replot

    pIdx = psm%plotNum
    inputCmd = trim(psm%generatePlotCommand())
    replot = .FALSE.
    if (pIdx /= -1 ) then
       replot = zoatabMgr%doesPlotExist_new(plot_code, objIdx, pIdx)
    end if


    !replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx)
    if (replot) then
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)
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
      objIdx = zoatabMgr%addGenericMultiPlotTab(plot_code, &
      & trim(tabName)//c_null_char, mplt)

      call zoaTabMgr%finalize_with_psm_new(objIdx, psm, trim(inputCmd))
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
    end if



end subroutine
  

end module