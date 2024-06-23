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
    logical :: replot
    type(multiplot) :: mplt
    type(zoaplot) :: zernplot
    type(c_ptr) :: canvas
    type(zoaplot_setting_manager) :: psm
    character(len=10) :: zernTxt

    character(len=5), allocatable :: zLegend(:)

    REAL, allocatable :: xdat(:), ydat(:,:)
    

    REAL*8 X(1:96)
    COMMON/SOLU/X


    ! Max 3 terms.  Terms 1 and 2 are min and max zernikes
    ! Term 3 is the wavelength.  

    ! Hard code some things for now
    print *, "About to crash?"
    numPoints = psm%getDensitySetting_new()
    call psm%getZernikeSetting_min_and_max(minZ, maxZ)
    call LogTermFOR("MinZ "//trim(int2str(minZ)))
    call LogTermFOR("MaxZ "//trim(int2str(maxZ)))

    !minZ = 5
    !maxZ = 9
    !numTermsToPlot = 5    
    numTermsToPlot = maxZ-minZ+1

    lambda = psm%getWavelengthSetting_new()
    inputCmd = trim(psm%generatePlotCommand())
    call LogTermFOR("inputCmd is "//inputCmd)
    !inputCmd = trim(psm%sp%getCommand())      

    allocate(xdat(numPoints))
    allocate(ydat(numPoints,maxZ-minZ+1))
    allocate(zLegend(size(ydat,2)))

    PRINT *, "number of data columns is ", size(ydat,2)

 
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
    
    !PRINT *, "zLegend is ", (zLegend)
    !PRINT *, "Final errors are ", ydat(10,:)
    PRINT *, "Before mplot set"
    call mplt%set(1,1,zernplot)
    PRINT *, "After mplot set"

    replot = zoatabMgr%doesPlotExist(ID_PLOTTYPE_ZERN_VS_FIELD, objIdx)
    PRINT *, "After replot check"
    PRINT *, "objIdx is ", objIdx
    PRINT *, "replot check is ", replot

    if (replot) then
      PRINT *, "Zernike REPLOT REQUESTED"
      PRINT *, "Input Command was ", inputCmd
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      !zoaTabMgr%tabInfo(objIdx)%tabObj%plotCommand = inputCmd
     
      call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)
     
     else
     
     
       !call mplt%draw()
     
     
      objIdx = zoatabMgr%addGenericMultiPlotTab(ID_PLOTTYPE_ZERN_VS_FIELD, &
      & "Zernike vs Field"//c_null_char, mplt)

      call LogTermFOR("ObjIdx is "//int2str(objIdx))

      ! Add settings
     ! call psm%finalize(objIdx, trim(inputCmd))

      !call zoaTabMgr%finalize_with_psm(objIdx, psm, trim(inputCmd))

      call zoaTabMgr%finalize_with_psm_new(objIdx, psm, trim(inputCmd))

    
    
    ! Create Plot + settings tab
    call zoaTabMgr%finalizeNewPlotTab(objIdx)


    end if

end subroutine

end module