! collection of functions to call during "GO" to finish off plots
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
    integer :: maxPlotZ = 9, numTermsToPlot, locE
    integer :: numPoints = 10
    integer :: pIdx
    logical :: replot
    type(multiplot) :: mplt
    type(zoaplot) :: zernplot
    type(c_ptr) :: canvas
    type(zoaplot_setting_manager) :: psm
    character(len=10) :: zernTxt
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
    call LogTermFOR("inputCmd is "//inputCmd)
    !inputCmd = trim(psm%sp%getCommand())      

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
      ydat(ii+1,1:numTermsToPlot) = X(minZ:maxZ)
    end do

  

   
    canvas = hl_gtk_drawing_area_new(size=[1200,500], &
    & has_alpha=FALSE)
   
    call mplt%initialize(canvas, 1,1)
   
    call zernplot%initialize(c_null_ptr, xdat,ydat(:,1), &
    & xlabel=trim(sysConfig%getFieldText())//c_null_char, &
    & ylabel="Coefficient [waves]"//c_null_char, &
    & title='Zernike Coefficients vs Field'//c_null_char)
    zLegend(1) = 'Z'//trim(int2str(minZ))
    do ii=2,numTermsToPlot
      PRINT *, "ii is ", ii
      call zernplot%addXYPlot(xdat, ydat(:,ii))
      PRINT *, "After Zernplot add"
      call zernplot%setDataColorCode(2+ii)
      !call zernplot%setLineStyleCode(4)
      zLegend(ii) = 'Z'//trim(int2str(minZ+ii-1))
      PRINT *, "value is ", 'Z'//trim(int2str(minZ+ii-1))

    end do

    !Test
    
    call logDataVsField(xdat, ydat, zLegend)
    
    call zernplot%addLegend(zLegend)
    call mplt%set(1,1,zernplot)

    !Update for multiple plots planning
    ! change doesPlotExist to get plot number
       ! logic
       ! use PSM to get base CMD.  
       ! if PX, then look for Xth plot of certain code
       ! if it exists
    ! Do I need replot?  

    ! Try #2
    ! If baseCMD is Xx P1, then look for nth plot
    ! if no, then new plot needed
    ! if yes, then replot

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
      print *, "pIdx is ", pIdx
      psm%plotNum = pIdx+1
      !TODO:  Fix this.  need to check if basecmd is multiple pieces or not
      psm%baseCmd = trim(psm%baseCmd)//" P"//int2str(psm%plotNum)
      print *, "baseCmd is ", psm%baseCmd
      tabName = "Zernike vs Field" 
      if  (psm%plotNum > 1) then
        tabName = trim(tabName)//" "//int2str(psm%plotNum)
      end if  
      objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_ZERN_VS_FIELD, &
      & trim(tabName)//c_null_char, mplt)

      call LogTermFOR("ObjIdx is "//int2str(objIdx))

      ! Set plotNum if needed.  TODO:  Is this always true?
    !   if (pIdx == -1) then 
    !     psm%plotNum = 1
    !     psm%baseCmd = trim(psm%baseCmd)//" P1"
    !   end if

      call zoaTabMgr%finalize_with_psm_new(objIdx, psm, trim(inputCmd))
        ! Create Plot + settings tab
      call zoaTabMgr%finalizeNewPlotTab(objIdx)

    end if

end subroutine

end module