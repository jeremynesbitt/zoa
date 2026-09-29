! collection of functions to call during "GO" to finish off plots
! Description of current plot design
! It is a lock and key design.  
! To create a new plot:
! Add a key in zoa-ui as an integer constant parameter.  Needs to be unique compared to other plots
! Add a new cmd with an exec function in cmd-plot
! add a cmd to switch to plot in executeGo
! add a function here to actually do the plot work


module plot_functions
  use global_widgets, only: sysConfig, ioConfig
  use zoa_ui
  use plot_setting_manager
  use plot_command_utils, only: getKDPSpotPlotCommand
  use iso_c_binding, only:  c_ptr, c_null_char
  use zoa_plot
  use data_registers

   use iso_fortran_env, only: real64
  implicit none

    interface
      module subroutine psf_go(psm)
        type(zoaplot_setting_manager) :: psm
      end subroutine psf_go

      module subroutine pma_go(psm)
        type(zoaplot_setting_manager) :: psm
      end subroutine pma_go

      module subroutine mtf_go(psm)
        type(zoaplot_setting_manager) :: psm
      end subroutine mtf_go
    end interface

    contains


subroutine zern_go(psm)

    USE GLOBALS
    use command_utils
    use handlers, only: zoatabMgr
    use zoa_output, only: zoa_emit
    use zoa_plot
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int

    use DATMAI


    character(len=23) :: ffieldstr
    character(len=1024) :: inputCmd
    integer :: ii, objIdx, minZ, maxZ, lambda, k, iz
    integer :: maxPlotZ = 9, numTermsToPlot
    integer :: numPoints = 10
    integer :: pupilGrid
    integer, allocatable :: zlist(:)
    integer :: pIdx
    logical :: replot
    type(multiplot) :: mplt
    type(zoaplot) :: zernplot
    type(c_ptr) :: canvas
    type(zoaplot_setting_manager) :: psm
    character(len=80) :: tabName


    character(len=5), allocatable :: zLegend(:)

    REAL, allocatable :: xdat(:), ydat(:,:)
    


    real(real64) X(1:96)
    COMMON/SOLU/X


    ! Create/find the tab up front (sets objIdx) so the data tab can be routed,
    ! matching the working rmsfield_go pattern.  No-op in headless.
    call initializeGoPlot(psm, ID_PLOTTYPE_ZERN_VS_FIELD, "Zernike vs Field", replot, objIdx)

    ! Field points across the sweep, and the pupil grid for each CAPFN.  Kept
    ! separate: the grid must be passed explicitly, or CAPFN reuses whatever
    ! the last caller left in KDP's CAPDEF global.
    numPoints = INT(psm%getSettingValueByCode(ID_NUMPOINTS))
    pupilGrid = psm%getDensitySetting()
    ! A "vs field" sweep needs a real field extent.  For a single on-axis field
    ! (no field height/angle -> refFieldValue == 0) every relative sample lands
    ! on axis, so a density sweep just repeats the same row.  Collapse to one.
    if (sysConfig%refFieldValue(2) == 0.0d0) numPoints = 1

    ! Terms to plot: supports "5..9" (range) and "9,16,25" (explicit list).
    call psm%getZernikeSetting_list(zlist)
    numTermsToPlot = size(zlist)
    if (numTermsToPlot == 0) then
      call zoa_emit("No valid Zernike terms specified (use e.g. 5..9 or 9,16)", "red")
      return
    end if
    ! A plot holds at most MAX_PLOT_SERIES curves.  Asking for more used to
    ! run off the end of zoaplot%plotdatalist and abort the program with a
    ! Fortran bounds error, so keep the first few and say so.
    if (numTermsToPlot > MAX_PLOT_SERIES) then
      call zoa_emit("Zernike vs Field plots at most "//trim(int2str(MAX_PLOT_SERIES))// &
      & " terms; showing the first "//trim(int2str(MAX_PLOT_SERIES))//" of "// &
      & trim(int2str(numTermsToPlot))//" requested", "red")
      zlist = zlist(1:MAX_PLOT_SERIES)
      numTermsToPlot = MAX_PLOT_SERIES
    end if

    lambda = psm%getWavelengthSetting()
    inputCmd = trim(psm%generatePlotCommand())

    ! Compute Values
    allocate(xdat(numPoints))
    allocate(ydat(numPoints,numTermsToPlot))
    allocate(zLegend(numTermsToPlot))


    do ii = 0, numPoints-1
      if (numPoints > 1) then
        xdat(ii+1) = REAL(ii)/REAL(numPoints-1)
      else
        xdat(ii+1) = 0.0
      end if
      write(ffieldstr, *) xdat(ii+1)
      ! Silent, like every other plot routine: this loop runs once per field,
      ! and with bare PROCESKDP each pass dumped the full CAPFN/FITZERN report
      ! into the console -- including whenever this tab was replotted.
      CALL PROCESSILENT("FOB "// ffieldstr)
      CALL PROCESSILENT("CAPFN, "//trim(int2str(pupilGrid)))
      write(ffieldstr, *) lambda
      CALL PROCESSILENT("FITZERN, "//ffieldstr)

      !CALL PROCESKDP("SHO RMSOPD")
      xdat(ii+1) = REAL(xdat(ii+1)*sysConfig%refFieldValue(2))
      do k=1,numTermsToPlot
        iz = zlist(k)
        if (iz >= 1 .and. iz <= size(X)) then
          ydat(ii+1,k) = real(X(iz),4)
        else
          ydat(ii+1,k) = 0.0
        end if
      end do
    end do

    do k=1,numTermsToPlot
      zLegend(k) = 'Z'//trim(int2str(zlist(k)))
    end do

    ! Log the table in BOTH modes, like rmsfield_go: this routine used to
    ! return before logging when headless, which left the coefficient table
    ! -- the actual content of this plot -- with no test coverage at all.
    if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    call logDataVsField(xdat, ydat, zLegend)
    if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

    if (HEADLESS_MODE) return

    ! Prep PLot
    canvas = hl_gtk_drawing_area_new(size=[1200,500], &
    & has_alpha=FALSE)

    call mplt%initialize(canvas, 1,1)
   
    call zernplot%initialize(c_null_ptr, xdat,ydat(:,1), &
    & xlabel=trim(sysConfig%getFieldText())//c_null_char, &
    & ylabel="Coefficient [waves]"//c_null_char, &
    & title='Zernike Coefficients vs Field'//c_null_char)
    do ii=2,numTermsToPlot
      call zernplot%addXYPlot(xdat, ydat(:,ii))
      call zernplot%setDataColorCode(2+ii)
    end do

    call zernplot%addLegend(zLegend)
    call mplt%set(1,1,zernplot)

    call finalizeGoPlot_new(mplt, psm, replot, objIdx)

end subroutine


subroutine vie_go(psm)
    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use plot_setting_manager
    use DATMAI
    use g
    use handlers, only: zoatabMgr
    use global_widgets, only: currVieData, curr_lens_data
    use mod_lens_data_manager, only: ldm
    use kdp_data_types, only: check_clear_apertures
    use zoa_output, only: zoa_suppress_output

   use iso_fortran_env, only: real64
    implicit none
    type(zoaplot_setting_manager) :: psm
    integer :: plot_code = ID_PLOTTYPE_LENSDRAW
    character(len=20) :: plotName = 'Lens Drawing'

    character(len=1024) :: inputCmd, tabName
    integer :: objIdx, pIdx
    logical :: replot, prevSup, discardSup

    ! Size the typed surfaces and fill each surface's ray-traced (display-only)
    ! clear-aperture extent, so the lens drawing can size surfaces without an
    ! explicit clear aperture from the real ray footprint (see CAOJK).  Muted: this
    ! is internal sizing and its ray trace must not pollute captured plot output.
    prevSup = zoa_suppress_output(.true.)
    call ldm%load_surfaces_from_alens()
    call check_clear_apertures(curr_lens_data, ldm%surfaces)
    discardSup = zoa_suppress_output(prevSup)

    !if (allocated(NEUTARRAY)) then
    !  call LogTermDebug("NEUTARRAYSize before vie_psm is "//int2str(size(NEUTARRAY)))
    !end if
    call vie_psm(psm)
    ! This is a temporary fix.  In my attempts to leave the legacy code intact, I cannot
    ! seem to figure out all the conditions where it wipes NEUTARRAY clean which is causing
    ! numerous plotting problems.  So for now will manage this by storing the data in NEUTARRAY
    ! separately only after this call is completed and access it in DRAWOPTICALSYSTEM
    if (allocated(NEUTARRAY)) then
      if (allocated(currVieData)) deallocate(currVieData)
      allocate(currVieData(size(NEUTARRAY)))
      currVieData(1:size(NEUTARRAY)) = NEUTARRAY(1:size(NEUTARRAY))
    end if

    if (HEADLESS_MODE) then
      ! Headless: render directly to PNG
      block
        use zoa_headless_plot, only: render_vie_to_png
        use zoa_plot_output, only: next_plot_path
        character(len=512) :: png_path
        png_path = next_plot_path()
        call render_vie_to_png(trim(png_path))
      end block
      return
    end if

    ! GUI path: create/update plot tab
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
      ! A command that names a plot number that does not exist yet (a .zin
      ! REPLAY record, a session restore) keeps that number; otherwise take
      ! the lowest free one.
      if (psm%plotNum < 1) psm%plotNum = zoatabMgr%getLowestAvailablePlotNum(plot_code)
      psm%baseCmd = withPlotNum(psm%baseCmd, psm%plotNum)
      inputCmd = trim(psm%generatePlotCommand())
      tabName = plotName
      if  (psm%plotNum > 1) then
        tabName = trim(tabName)//" "//int2str(psm%plotNum)
      end if  
      objIdx = zoatabMgr%addKDPPlotTab(plot_code, &
      & trim(tabName)//c_null_char)
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
      !objIdx = zoatabMgr%addGenericMultiPlotTab(plot_code, &
      !& trim(tabName)//c_null_char, mplt)

      call zoaTabMgr%finalize_with_psm(objIdx, psm, trim(inputCmd))
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
    end if

    


end subroutine

function getTabTextView(objIdx) result (dataTextView)
  use handlers, only: zoatabMgr
  use zoa_tab
   use iso_fortran_env, only: real64
  implicit none
  integer :: objIdx
  type (c_ptr) :: dataTextView

  type (zoaplotdatatab) :: tabTmp

  dataTextView = c_null_ptr

  select type (tabTmp => zoatabMgr%tabInfo(objIdx)%tabObj)
  type is (zoaplotdatatab)
     dataTextView = tabTmp%textView
     type is (zoadatatab)
     dataTextView = tabTmp%textView     

end select

end function

subroutine spo_fieldPoint_go(psm)

  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only:  sysConfig, ioConfig
  use zoa_ui
  use kdp_utils, only: OUTKDP, logDataVsField
  use type_utils, only: int2str, str2int
  use plot_setting_manager
  use DATMAI
  use g


   use iso_fortran_env, only: real64
  IMPLICIT NONE

  type(multiplot) :: mplt
  type(zoaplot) :: xyscat1
  type(c_ptr) :: canvas

  type(zoaplot_setting_manager) :: psm

  integer :: iField, iLambda, iMethod, nRect, nRand, nRing
  integer :: objIdx
  logical :: replot
  real :: plotScale


  call psm%getSpotDiagramSettings(iField, iLambda, iMethod, nRect, nRand, nRing, plotScale)

  call initializeGoPlot(psm,ID_PLOTTYPE_SPOT_NEW, "Spot Diagram", replot, objIdx)

  ! Hopefully temporary 
  call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  

  call PROCESKDP(trim(getKDPSpotPlotCommand(iField, iLambda, iMethod, nRect, nRand, nRing)))
  call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

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

  !Pseudo
  ! if (psm%autoScale == FALSE ) then
  ! xyscat1%autoScale = .FALSE.
  ! xyscat1%minY = 0
  ! xyscat1%maxY = psm%getScaleFactorInLensUnits()
  ! xyScat%minX = minY
  ! xyScat1%maxX = maxY




  call mplt%set(1,1,xyscat1)
  call finalizeGoPlot_new(mplt, psm, replot, objIdx)


  !call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_SPOT_NEW, "Spot Diagram")

end subroutine

subroutine getSpotData(xData,yData)
  use GLOBALS
  use iso_fortran_env
  real(kind=real64), allocatable, intent(inout) :: xData(:), yData(:)

  
  allocate(xData, source=pack(DSPOTT(1,:),DSPOTT(1,:) /= 0 .and. DSPOTT(2,:) /=0))
  allocate(yData, source=pack(DSPOTT(2,:), DSPOTT(1,:) /= 0 .and. DSPOTT(2,:) /=0))

end subroutine

subroutine spo_go(psm)

    use iso_fortran_env
    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use global_widgets, only:  sysConfig, ioConfig, curr_par_ray_trace
    use zoa_ui
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use plot_setting_manager
    use kdp_data_types, only: LENS_UNITS_INCHES, LENS_UNITS_CM, LENS_UNITS_M
    use DATMAI
    use DATSPD
    use DATLEN
    use g
    use type_utils


    IMPLICIT NONE

    type(multiplot) :: mplt
    type(zoaplot), dimension(sysConfig%numFields) :: xyscat
    type(c_ptr) :: canvas

    type(zoaplot_setting_manager) :: psm

    integer :: iField, iLambda, iMethod, nRect, nRand, nRing
    integer :: objIdx, i, j, iPanel, nPanels
    logical :: replot, allWL

    real(kind=real64), allocatable :: xSpot(:), ySpot(:)
    real(kind=real64) :: yAvg
    real :: plotScale
    logical :: drawAiry
    integer, parameter :: AIRY_NPTS = 181
    real :: airyX(AIRY_NPTS), airyY(AIRY_NPTS), airyRadius, wlLensUnits, theta
    real :: spotExtent, airyScale
    ! TODO:  Distable field here
    call psm%getSpotDiagramSettings(iField, iLambda, iMethod, nRect, nRand, nRing, plotScale)

    allWL = .FALSE.
    if (iLambda == ID_SETTING_WAVELENGTH_ALL) then
      allWL = .TRUE.
      iLambda = 1
    end if

    call initializeGoPlot(psm,ID_PLOTTYPE_SPOT_NEW, "Spot Diagram", replot, objIdx)

    if (.not. HEADLESS_MODE) then
      call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    end if

    ! Field Point selects one field or, with ALL, every field stacked.  Decide
    ! this before sizing the canvas: the drawing area has to match the panel
    ! count or the plot is drawn into the wrong aspect ratio (a single field in
    ! a 3-field-tall area came out stretched into an ellipse).
    if (iField == ID_SETTING_FIELD_ALL) then
      nPanels = sysConfig%numFields
    else
      nPanels = 1
    end if

    ! Prep PLot
    if (HEADLESS_MODE) then
      canvas = c_null_ptr
    else
      canvas = hl_gtk_drawing_area_new(size=[400,400*nPanels], &
      & has_alpha=FALSE)
    end if

    ! Todo:  change initialization to sepcify size, and then don't need to
    ! call gtk_drawing_area
    call mplt%initialize(canvas, nPanels,1)
    mplt%height = 400*nPanels
    mplt%width = 400

    ! Airy disk radius = 1.22 * lambda * F/#, in lens units.  sysConfig returns
    ! the wavelength in micrometres, so convert it to whatever the lens is
    ! dimensioned in; the F-number is the working (image-space) one.
    drawAiry = psm%getAirySetting()
    airyRadius = 0.0
    if (drawAiry) then
      select case (sysConfig%currLensUnitsID)
      case (LENS_UNITS_INCHES)
        wlLensUnits = real(sysConfig%getWavelength(iLambda)*3.93700787402d-5)
      case (LENS_UNITS_CM)
        wlLensUnits = real(sysConfig%getWavelength(iLambda)*1.0d-4)
      case (LENS_UNITS_M)
        wlLensUnits = real(sysConfig%getWavelength(iLambda)*1.0d-6)
      case default   ! mm
        wlLensUnits = real(sysConfig%getWavelength(iLambda)*1.0d-3)
      end select
      airyRadius = 1.22*wlLensUnits*real(curr_par_ray_trace%FNUM)
      if (airyRadius > 0.0) then
        do j=1,AIRY_NPTS
          theta = real(TWOPII)*real(j-1)/real(AIRY_NPTS-1)
          airyX(j) = airyRadius*cos(theta)
          airyY(j) = airyRadius*sin(theta)
        end do
        call OUTKDP("Airy radius = "//trim(real2str(airyRadius,6))//" "// &
        & trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)// &
        & "   (1.22 * "//trim(real2str(sysConfig%getWavelength(iLambda),5))// &
        & "um * F/"//trim(real2str(curr_par_ray_trace%FNUM,4))//")")
      else
        call zoa_emit("Airy radius is not computable for this system", "red")
        drawAiry = .FALSE.
      end if
    end if

    do iPanel=1,nPanels
      if (iField == ID_SETTING_FIELD_ALL) then
        i = iPanel
      else
        i = iField
      end if
      call PROCESKDP(trim(getKDPSpotPlotCommand(i, iLambda, iMethod, nRect, nRand, nRing)))
      if(allocated(xSpot)) deallocate(xSpot)
      if(allocated(ySpot)) deallocate(ySpot)

      call getSpotData(xSpot, ySpot)  
      
      !Subtract Avg
      yAvg = (sum(ySpot)/size(ySpot))
      ySpot = ySpot - yAvg
      spotExtent = real(max(maxval(abs(xSpot)), maxval(abs(ySpot))))
                                  

    ! TODO:  remove dependency on canvas here after checking it doesn't break anything.
    call xyscat(i)%initialize(c_null_ptr, REAL(xSpot), REAL(ySpot), &
    & xlabel='RMS = '//trim(real2str(RMS))//' '// &
    & sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, &
    & ylabel=''//c_null_char, &
    & title=''//c_null_char)


    call xyscat(i)%setLineStyleCode(-1)
    
    if (plotScale /= 0) then 
      call xyscat(i)%setYScale(real(plotScale,8))
      call xyscat(i)%setXScale(real(plotScale,8))
    end if
    call xyscat(i)%removeGrids()
    call xyscat(i)%removeLabels()

    ! Airy disk overlay (AIRY ON): a black circle of radius 1.22*lambda*F/#,
    ! drawn on every field point.  F/# is the working (image-space) F-number,
    ! since that is the cone that sets the diffraction limit.
    if (drawAiry .and. airyRadius > 0.0) then
      call xyscat(i)%addXYPlot(airyX, airyY)
      call xyscat(i)%setDataColorCode(PL_PLOT_BLACK)
      call xyscat(i)%setLineStyleCode(1)
    end if

    if (iPanel==1) call xyscat(i)%addScaleBar(POS_LOWER_RIGHT)      

    if (allWL) then
      do j=2,sysConfig%numWavelengths
        call PROCESKDP(trim(getKDPSpotPlotCommand(i, j, iMethod, nRect, nRand, nRing)))
        if(allocated(xSpot)) deallocate(xSpot)
        if(allocated(ySpot)) deallocate(ySpot)
        call getSpotData(xSpot, ySpot)  
        ! Subtract relative to first wavelength.  Should probably be relative to
        ! ref wavelength; revisit this after testing
        ySpot = ySpot - yAvg
        spotExtent = max(spotExtent, &
        & real(max(maxval(abs(xSpot)), maxval(abs(ySpot)))))
        call xyscat(i)%addXYPlot(real(xSpot), real(ySpot))
        call xyscat(i)%setDataColorCode(sysConfig%wavelengthColorCodes(j))
        call xyscat(i)%setLineStyleCode(-1)
      end do
    end if

    ! The Airy disk is only a circle if the two axes share a scale, and the
    ! spot diagram otherwise autoscales X and Y independently.  Square the
    ! window around the data (and the disk, so it cannot be clipped) when the
    ! overlay is on and the user has not set an explicit scale.
    if (drawAiry .and. airyRadius > 0.0 .and. plotScale == 0) then
      airyScale = max(spotExtent, airyRadius)*1.05
      call xyscat(i)%setYScale(real(airyScale,8))
      call xyscat(i)%setXScale(real(airyScale,8))
    end if

    call mplt%set(nPanels-iPanel+1,1,xyscat(i))

    

    end do



    !call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    call ioConfig%restoreTextView()

    call finalizeGoPlot_new(mplt, psm, replot, objIdx)


    !call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_SPOT_NEW, "Spot Diagram")

end subroutine


subroutine seidel_go(psm)
    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use global_widgets, only:  curr_par_ray_trace, curr_lens_data, ioConfig
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use DATMAI
    use iso_c_binding, only: c_ptr, c_null_ptr
    interface
        subroutine MMAB3_NEW(YFLAG, idxWV, printTable)
            logical, intent(in) :: YFLAG
            integer, intent(in) :: idxWV
            logical, optional, intent(in) :: printTable
        end subroutine
    end interface

    type(zoaplot_setting_manager) :: psm
    integer, parameter :: nS = 7 ! number of seidel terms to plot
    real, allocatable, dimension(:,:) :: seidel
    real, allocatable, dimension(:) :: surfIdx
    
    character(len=23) :: ffieldstr
    character(len=40) :: inputCmd
    integer :: ii, objIdx, jj
    logical :: replot
    type(c_ptr) :: canvas
    type(barchart), dimension(nS) :: barGraphs
    integer, dimension(nS) :: graphColors
    type(multiplot) :: mplt
    character(len=100) :: strTitle
    character(len=20), dimension(nS) :: yLabels
    character(len=23) :: cmdTxt

    call initializeGoPlot(psm,ID_PLOTTYPE_SEIDEL, "Seidel Aberrations", replot, objIdx)

    if (.not. HEADLESS_MODE) then
      call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    end if
    call MMAB3_NEW(.TRUE., psm%getWavelengthSetting(), .TRUE.)
    if (.not. HEADLESS_MODE) then
      call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    end if

    allocate(seidel(nS,curr_lens_data%num_surfaces+1))
    allocate(surfIdx(curr_lens_data%num_surfaces+1))



    yLabels(1) = "Spherical"
    yLabels(2) = "Coma"
    yLabels(3) = "Astigmatism"
    yLabels(4) = "Distortion"
    yLabels(5) = "Curvature"
    yLabels(6) = "Axial Chromatic"
    yLabels(7) = "Lateral Chromatic"

    graphColors = [PL_PLOT_RED, PL_PLOT_BLUE, PL_PLOT_GREEN, &
    & PL_PLOT_MAGENTA, PL_PLOT_CYAN, PL_PLOT_GREY, PL_PLOT_BROWN]

    surfIdx =  (/ (ii,ii=0,curr_lens_data%num_surfaces)/)
    seidel(:,:) = curr_par_ray_trace%CSeidel(:,0:curr_lens_data%num_surfaces)

    if (HEADLESS_MODE) then
      canvas = c_null_ptr
    else
      canvas = hl_gtk_drawing_area_new(size=[1200,800], &
      & has_alpha=FALSE)
    end if

     call mplt%initialize(canvas, nS,1)
    
     do jj=1,nS
      call barGraphs(jj)%initialize(c_null_ptr, real(surfIdx),seidel(jj,:), &
      & xlabel='Surface No (last item actually sum)'//c_null_char, & 
      & ylabel=trim(yLabels(jj))//c_null_char, &
      & title=' '//c_null_char)
      call barGraphs(jj)%setDataColorCode(graphColors(jj))
      barGraphs(jj)%useGridLines = .FALSE.
     end do
    
     do ii=1,nS
      call mplt%set(ii,1,barGraphs(ii))
     end do
    
     call finalizeGoPlot_new(mplt, psm, replot, objIdx)
     !call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_SEIDEL, "Seidel Aberrations")
  
    


end subroutine

subroutine ast_go(psm)

    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use zoa_plot
    use kdp_utils, only: OUTKDP, logDataVsField
    use type_utils, only: int2str, str2int
    use DATMAI


   use iso_fortran_env, only: real64
    IMPLICIT NONE

    type(multiplot) :: mplt
    type(c_ptr) :: canvas
    type(zoaplot_setting_manager) :: psm
    character(len=80) :: ftext
    character(len=80) :: ffieldstr

    !integer(c_int), value, intent(in) :: win_width, win_height
    type(zoaplot) :: lin1, lin2, lin3

    integer :: numPts, numPtsDist, numPtsFC, idxFieldXY
    integer :: objIdx, numPlots, j
    real :: astMax, dstMax
    logical :: replot, lsa

     REAL:: DDTA(0:50), xDist(0:50), yDist(0:50), x1FC(0:50), x2FC(0:50), yFC(0:50)

     REAL:: FLDAN(0:50)

     !COMMON FLDAN, DDTA

     lsa = .FALSE.

     call psm%getAstigSettings(idxFieldXY, numPts, astMax, dstMax)

     if (.not. HEADLESS_MODE) then
       call initializeGoPlot(psm,ID_PLOTTYPE_AST, "Field Curv / Dist", replot, objIdx)
     end if

     select case (idxFieldXY)
     case (ID_AST_FIELD_Y)
      ftext = ",0,,  "
     case (ID_AST_FIELD_X)
      ftext = ",90,, "
     case default
      ftext = ",0,,  "
     end select

     ! Need to compute this to get the FC plot I want
     ! PROCESSILENT for AST/FLDCV/DIST: each command prints its full table
     ! (ASTIGMATISM TABLE, field-curvature and distortion tables) which
     ! flooded the console on every settings change and replot.  The plot
     ! data comes from the COMMON arrays the commands fill (ABSSS/FIFI), read
     ! by getFieldCalcResult, so nothing depends on the printed output.
     CALL PROCESSILENT('ASTK'//trim(ftext)//int2str(numPts))
     call getFieldCalcResult(DDTA, X2FC, FLDAN, numPts, 1)

     if (.not. HEADLESS_MODE) then
       call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
     end if

    if (HEADLESS_MODE) then
      canvas = c_null_ptr
    else
      canvas = hl_gtk_drawing_area_new(size=[800,500], &
      & has_alpha=FALSE)
    end if
    numPlots = 2
    if (lsa) numPlots = 3
  call mplt%initialize(canvas, 1,numPlots)


  ! TODO:  Copy or mod the base function to
  ! compute FLDCV and output the way I want it
  !CALL FLDCRV(2,DWORD1,DWORD2,ERROR)
  CALL PROCESSILENT('FLDCV'//trim(ftext)//int2str(numPts))
  call getFieldCalcResult(x1FC, x2FC, yFC, numPtsFC, 3)


   call lin3%initialize(c_null_ptr, REAL(x1FC(0:numPtsFC)),yFC(0:numPtsFC), &
   & xlabel='Field Curvature '//c_null_char, &
   & ylabel=sysConfig%getFieldText()//c_null_char, &
   & title=''//c_null_char)
   call lin3%addXYPlot(X2FC(0:numPtsFC),FLDAN(0:numPtsFC))
   call lin3%setDataColorCode(PL_PLOT_BLUE)
   call lin3%setLineStyleCode(4)

CALL PROCESSILENT('DIST'//trim(ftext)//int2str(numPts))

 call getFieldCalcResult(xDist, x2FC, yDist, numPtsDist, 2)


  call lin2%initialize(c_null_ptr, REAL(xDist(0:numPtsDist)),yDist(0:numPtsDist), &
  & xlabel='Distortion (%)'//c_null_char, &
  & ylabel=sysConfig%getFieldText()//c_null_char, &
  & title=''//c_null_char)



  ! Prep lsa plot
  if (lsa) then
  end if



  ! Manual x-axis scale (AST / DST settings); 0 leaves the autoscale.
  ! setXScale takes the half-width: AST .01 -> field curvature over -.01..+.01.
  if (astMax /= 0.0) call lin3%setXScale(real(astMax, pl_test_flt))
  if (dstMax /= 0.0) call lin2%setXScale(real(dstMax, pl_test_flt))

  if (lsa) call mplt%set(1,1,lin1)
  call mplt%set(1,numPlots-1,lin3)
  call mplt%set(1,numPlots,lin2)
  !call mplt%set(1,3,lin3)

  if (HEADLESS_MODE) then
    call mplt%draw()
  else
    call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    call finalizeGoPlot_new(mplt, psm, replot, objIdx)
  end if


end subroutine


subroutine rayaberration_old_go(psm)
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets
  use type_utils, only: int2str
  use plplot, PI => PL_PI
  use plplot_extra

  character(len=80) :: ffieldstr
  CHARACTER(LEN=*), PARAMETER  :: FMTFAN = "(I1, A1, I3)"
  integer :: lambda, fldIdx, objIdx, numPoints
  
  integer, parameter :: nlevel = 10
  logical :: replot

  type(c_ptr) :: canvas
  !type(zoaPlot3d) :: zp3d 
  type(zoaplot) :: lineplot
  type(multiplot) :: mplt
  type(zoaplot_setting_manager) :: psm
  REAL, allocatable :: x(:), y(:)

  call initializeGoPlot(psm,ID_PLOTTYPE_RIM, "Ray Aberration Fan", replot, objIdx)



  lambda = psm%getWavelengthSetting()
  fldIdx = psm%getFieldSetting()
  numPoints = psm%getDensitySetting()

  
  allocate(x(numPoints))
  allocate(y(numPoints))
  
  ! Set Field
  WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,fldIdx) &
  & , ' ' , sysConfig%relativeFields(1, fldIdx)
  CALL PROCESKDP(trim(ffieldstr))

  ! Set Fan Input - TODO:  add setting to change fan type
  write(ffieldstr, FMTFAN) lambda,',',numPoints
  

  !CALL PROCESKDP("XFAN, -1, 1, "//ffieldstr)
  !x = curr_ray_fan_data%relAper
  !y(1:numPoints,1) = curr_ray_fan_data%xyfan(1:numPoints,1)
  call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
  call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
  x = curr_ray_fan_data%relAper
  y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,2)
  
  ! FOB 1
  ! CALL PROCESKDP("FOB 1")
  ! CALL PROCESKDP("XFAN, -1, 1, "//ffieldstr)
  ! y(1:numPoints,3) = curr_ray_fan_data%xyfan(1:numPoints,1)
  ! CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
  ! y(1:numPoints,4) = curr_ray_fan_data%xyfan(1:numPoints,2)
  
  
  
  
   canvas = hl_gtk_drawing_area_new(size=[1200,500], &
   & has_alpha=FALSE)
  
  
   call mplt%initialize(canvas, 1,1)
  

   call lineplot%initialize(c_null_ptr, x,y, &
   & xlabel='Relative '//'Y'//' Pupil Position'//c_null_char, & 
   & ylabel='Y'//' Error ['// &
   & trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)//']'//c_null_char, &
   & title='Y '//c_null_char)
   !PRINT *, "Bar chart color code is ", bar1%dataColorCode
   
   
   call mplt%set(1,1,lineplot)

  
   !call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_RIM, "Ray Aberration Fan")
   call finalizeGoPlot_new(mplt, psm, replot, objIdx)

   
  


end subroutine

subroutine rayaberration_go(psm)
    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use global_widgets
    use type_utils, only: int2str, real2str
    use plplot, PI => PL_PI
    use plplot_extra

   use iso_fortran_env, only: real64
    implicit none

    character(len=80) :: ffieldstr
    character(len=10), allocatable :: wlLegend(:)
    CHARACTER(LEN=*), PARAMETER  :: FMTFAN = "(I1, A1, I3)"
    integer :: lambda, fldIdx, objIdx, numPoints

    real :: plotScale
    
    integer, parameter :: nlevel = 10
    logical :: replot, allWL
    integer :: i, j
    type(c_ptr) :: canvas
    !type(zoaPlot3d) :: zp3d 
    type(zoaplot), dimension(sysConfig%numFields) :: lineplot
    type(zoaplot), dimension(sysConfig%numFields) :: sagplots
    type(multiplot) :: mplt
    type(zoaplot_setting_manager) :: psm
    REAL, allocatable :: x(:), y(:)
    real:: yOff(sysConfig%numFields)

    character(len=100) :: xlabel, ylabel, title

    call initializeGoPlot(psm,ID_PLOTTYPE_RIM, "Ray Aberration Fan", replot, objIdx)


    allWL = .FALSE.
    lambda = psm%getWavelengthComboSetting()
    if (lambda == ID_SETTING_WAVELENGTH_ALL) then
        allWL = .TRUE.
        lambda = 1
        allocate(character(len=10) :: wlLegend(sysConfig%numWavelengths))
    else
      allocate(character(len=10) :: wlLegend(1))
    end if


    ! Probably better to do this in FANS, but compute offset at ref wavelength
    ! for each field point 
    do i=1,sysConfig%numFields

      ! Set Field
      WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,i) &
      & , ' ' , sysConfig%relativeFields(1, i)
      CALL PROCESKDP(trim(ffieldstr))
      ! Set Fan Input - TODO:  add setting to change fan type
      write(ffieldstr, FMTFAN) sysConfig%refWavelengthIndex ,',',3
      
      CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
      yOff(i) = curr_ray_fan_data%xyfan(2,2) ! only 3 points computed.  Min requireed to get ref value  
    end do
    print *, "yOff is ", yOff
  
    plotScale = psm%getSettingValueByCode(SETTING_SCALE)
    numPoints = psm%getDensitySetting()

    
    allocate(x(numPoints))
    allocate(y(numPoints))

    if (.not. HEADLESS_MODE) then
      call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    end if

    if (HEADLESS_MODE) then
      canvas = c_null_ptr
    else
      canvas = hl_gtk_drawing_area_new(size=[1200,1200], &
      & has_alpha=FALSE)
    end if

    call mplt%initialize(canvas, sysConfig%numFields,2)

    do i=1,sysConfig%numFields

      ! Set Field
      WRITE(ffieldstr, *) "FOB ", sysConfig%relativeFields(2,i) &
      & , ' ' , sysConfig%relativeFields(1, i)
      CALL PROCESKDP(trim(ffieldstr))


      ! Set Fan Input - TODO:  add setting to change fan type
      write(ffieldstr, FMTFAN) lambda,',',numPoints
      
      CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
      x = curr_ray_fan_data%relAper
      y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,2)-yOff(i)
            
      
      ! Hide labels for off axis points
      if (i /= 1) then 
        xlabel = ''
        ylabel = ''
      else
        xlabel = 'Rel. Pupil Position'
        ylabel = 'Error '//trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)
      end if

      if (i==sysConfig%numFields) then
        title = 'Tangential'
      else
        title = ''
      end if

        call lineplot(i)%initialize(c_null_ptr, x,y, &
        & xlabel=trim(xlabel)//c_null_char, & 
        & ylabel=trim(ylabel)//c_null_char, &
        & title = trim(title)//c_null_char)     

        call lineplot(i)%addText('Field '//sysConfig%getAbsYFieldText(i), POS_UPPER_RIGHT)
        if (plotScale /= 0) call lineplot(i)%setYScale(real(plotScale,8))

        if (allWL) then
          do j=2,sysConfig%numWavelengths
            write(ffieldstr, FMTFAN) j,',',numPoints
            CALL PROCESKDP("YFAN, -1, 1, "//ffieldstr) 
            x = curr_ray_fan_data%relAper
            y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,2)-yOff(i)
            call lineplot(i)%addXYPlot(x,y)
            call lineplot(i)%setDataColorCode(sysConfig%wavelengthColorCodes(j))                  
          end do
          ! Create legend
          do j=1,sysConfig%numWavelengths
            wlLegend(j) = trim(real2str(1000.0*sysConfig%getWavelength(j),2))// " nm"   
          end do          
        else
           wlLegend(1) = trim(real2str(1000.0*sysConfig%getWavelength(lambda),2))// " nm"
        end if
        ! Add legend then set the flag to false for now
        call lineplot(i)%addLegend(wlLegend)
        lineplot(i)%useLegend = .FALSE.
   


      !call mplt%set(1,i,lineplot(i))
      call mplt%set(sysConfig%numFields-i+1,1,lineplot(i))

      !Saggatial
      write(ffieldstr, FMTFAN) lambda,',',numPoints
      CALL PROCESKDP("XFAN, 0, 1, "//ffieldstr) 
      x = curr_ray_fan_data%relAper
      y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,1)       

      if (i==sysConfig%numFields) then
        title = 'Sagittal'
      else
        title = ''
      end if

      call sagplots(i)%initialize(c_null_ptr, x,y, &
      & xlabel=trim(xlabel)//c_null_char, & 
      & ylabel=trim(ylabel)//c_null_char, &
      & title = trim(title)//c_null_char)     

      if (plotScale /= 0) call sagplots(i)%setYScale(real(plotScale,8))
      
      if (allWL) then
        do j=2,sysConfig%numWavelengths
          write(ffieldstr, FMTFAN) j,',',numPoints
          CALL PROCESKDP("XFAN, 0, 1, "//ffieldstr) 
          y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,1)  
          call sagplots(i)%addXYPlot(x,y)
          call sagplots(i)%setDataColorCode(sysConfig%wavelengthColorCodes(j))                  
        end do
      end if

      call mplt%set(sysConfig%numFields-i+1,2,sagplots(i))
      

     !call mplt%set(2,i,sagplots(i))

     end do

     call mplt%addBottomPanel(trim(sysConfig%lensTitle),  &
     & "Ray Aberrations ("//sysConfig%getDimensions()//")",trim(real2str(1000.0*sysConfig%getWavelength(lambda),2))// " nm")
     !call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
     if (.not. HEADLESS_MODE) call ioConfig%restoreTextView()

     !call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_RIM, "Ray Aberration Fan")
     call finalizeGoPlot_new(mplt, psm, replot, objIdx)
  
     
    


end subroutine


subroutine rmsfield_go(psm)
  USE GLOBALS
  use command_utils
  use zoa_output, only: zoa_emit
  use global_widgets, only:  sysConfig, curr_ray_fan_data, ioConfig
  use kdp_utils, only: log2DData
  use type_utils, only: int2str
  use DATSP1, only: SPDTYPE, NRECT
  use DATLEN, only: REFLOC
  use plplot, PI => PL_PI
  use plplot_extra
  use iso_c_binding, only: c_ptr, c_null_ptr

use DATMAI
   use iso_fortran_env, only: real64
IMPLICIT NONE

character(len=23) :: ffieldstr
integer :: ii, objIdx, iData, iLambda
logical :: replot
integer :: numPoints, pupilGrid, savedSpdType, savedNRect, savedRefLoc
type(zoaplot) :: xyscat
type(c_ptr) :: canvas

REAL, allocatable :: x(:), y(:)
type(zoaplot_setting_manager) :: psm
type(multiplot) :: mplt


 !call checkCommandInput(ID_CMD_ALPHA)

call initializeGoPlot(psm,ID_PLOTTYPE_RMSFIELD, "RMS vs Field", replot, objIdx)


 !call updateTerminalLog(INPUT, "blue")

 call psm%getRMSFieldSettings(iData, iLambda, numPoints)
 pupilGrid = psm%getDensitySetting()

! Reference sphere centre (the legacy RSPH global).  REFLOC is read inside
! CAPFN -- COMPAP/WAVESLP1 rewrite the stored OPD in place -- so it has to be
! set before the loop, not after.  Bracket it: every other CAPFN consumer
! (ZRNFLD, PMA, PSF, diffraction MTF) reads the same global, and the state
! is fully reversible, so restoring it afterwards keeps this plot's choice
! from following the user around.
savedRefLoc = REFLOC
REFLOC = psm%getRSPHSetting()

! The spot branch drives SPD, whose sampling comes from mutable KDP globals
! (SPDTYPE/NRECT/ring pattern).  Left alone, this plot's numbers depended on
! whatever a previous command had set -- e.g. an earlier "SPOT RECT; RECT 30"
! moved the on-axis RMS from 0.01224 to 0.00890 on the same lens.  Pin it to
! the plot's own Density setting here and put the globals back afterwards, so
! neither this plot nor any later spot diagram is at the mercy of the other.
if (iData == ID_RMS_DATA_SPOT) then
  savedSpdType = SPDTYPE
  savedNRect   = NRECT
  SPDTYPE = 1          ! rectangular grid
  NRECT   = pupilGrid
end if

! A "vs field" sweep needs a real field extent.  For a single on-axis field
! (refFieldValue == 0) every relative sample lands on axis, so collapse the
! density sweep to a single row instead of numPoints identical copies.
if (sysConfig%refFieldValue(2) == 0.0d0) numPoints = 1

allocate(x(numPoints))
allocate(y(numPoints))

do ii = 0, numPoints-1
 if (numPoints > 1) then
   x(ii+1) = REAL(ii)/REAL(numPoints-1)
 else
   x(ii+1) = 0.0
 end if
 write(ffieldstr, *) x(ii+1)
 CALL PROCESSILENT("FOB "// ffieldstr)
 select case(iData)

 case(ID_RMS_DATA_WAVE)
    ! Explicit grid (default 16, KDP's own): a bare CAPFN inherits the last
    ! caller's CAPDEF global, so this plot's numbers would change with
    ! whatever grid another plot had used.
    CALL PROCESSILENT("CAPFN SILENT, "//trim(int2str(pupilGrid)))
    CALL PROCESSILENT("SHO RMSOPD")
    y(ii+1) = 1000.0*REG(9)
 case(ID_RMS_DATA_SPOT)
  ! PROCESSILENT, matching the wavefront branch above: a bare PROCESKDP here
  ! dumped the full KDP spot-diagram report (aperture assignment, ray-trace
  ! progress, SPOT DIAGRAM SUMMARY) into the terminal once per field point.
  CALL PROCESSILENT("SPD")
  CALL PROCESSILENT("SHO RMS")
  y(ii+1) = REG(9)
 end select


 x(ii+1) = x(ii+1)*sysConfig%refFieldValue(2)

end do

if (iData == ID_RMS_DATA_SPOT) then
  SPDTYPE = savedSpdType
  NRECT   = savedNRect
end if
REFLOC = savedRefLoc



if (HEADLESS_MODE) then
  canvas = c_null_ptr
else
  canvas = hl_gtk_drawing_area_new(size=[1200,500], &
  & has_alpha=FALSE)
  call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
end if

! Header must follow the selected data type, like the plot's own y label below:
! wavefront error is in milliwaves, spot size in the current lens units.
select case (iData)
case(ID_RMS_DATA_SPOT)
  call log2DData(real(x,8), real(y,8), xHeader='Field', &
  & yHeader='RMS['//trim(sysConfig%getLensUnitsText())//']')
case default
  call log2DData(real(x,8), real(y,8), xHeader='Field', yHeader='RMS[mWaves]')
end select

if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

call mplt%initialize(canvas, 1,1)

select case (iData)
case(ID_RMS_DATA_WAVE)

call xyscat%initialize(c_null_ptr, x,y, &
& xlabel=sysConfig%getFieldText()//c_null_char, &
!sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, & 
& ylabel='RMS Error [mWaves]'//c_null_char, &
& title='RMS Error vs Field '//c_null_char)
case(ID_RMS_DATA_SPOT)
  call xyscat%initialize(c_null_ptr, x,y, &
  & xlabel=sysConfig%getFieldText()//c_null_char, &
  !& xlabel=sysConfig%lensUnits(sysConfig%currLensUnitsID)%text//c_null_char, & 
  & ylabel="RMS ["//trim(sysConfig%getLensUnitsText())//"]"//c_null_char, &
  & title='Spot RMS Size vs Field'//c_null_char)
end select  

call mplt%set(1,1,xyscat)

call finalizeGoPlot_new(mplt, psm, replot, objIdx)
end subroutine

! FFT/image plot implementations live in plot-functions-fft.f90

! Rebuild a plot base command with exactly ONE trailing plot-number token.
! A replayed or restored command already carries its P<n>; blindly appending
! another produced "VIE P1 P1", which the next replot pass no longer recognized
! as an existing plot (execVIE checks for exactly two tokens) and so opened a
! duplicate tab.
function withPlotNum(baseCmd, plotNum) result(cmd)
    use strings, only: parse
    use type_utils, only: int2str
    implicit none
    character(len=*), intent(in) :: baseCmd
    integer, intent(in) :: plotNum
    character(len=len(baseCmd)) :: cmd
    character(len=80) :: tokens(40)
    integer :: n, i
    call parse(trim(baseCmd), ' ', tokens, n)
    cmd = ''
    do i = 1, n
      if (isPlotNumToken(trim(tokens(i)))) cycle
      cmd = trim(cmd)//' '//trim(tokens(i))
    end do
    cmd = trim(adjustl(cmd))//' P'//int2str(plotNum)
end function withPlotNum

! True for a plot-number token: P followed only by digits (P1, P12).
logical function isPlotNumToken(tok)
    implicit none
    character(len=*), intent(in) :: tok
    integer :: k
    isPlotNumToken = .false.
    if (len_trim(tok) < 2) return
    if (tok(1:1) /= 'P' .and. tok(1:1) /= 'p') return
    do k = 2, len_trim(tok)
      if (index('0123456789', tok(k:k)) == 0) return
    end do
    isPlotNumToken = .true.
end function isPlotNumToken

subroutine initializeGoPlot(psm, plot_code, plotName, replot, objIdx)

    use zoa_plot
    use plot_setting_manager
    use handlers, only: zoatabMgr
    use type_utils, only: int2str
    USE GLOBALS, only: HEADLESS_MODE

   use iso_fortran_env, only: real64
    implicit none
    type(zoaplot_setting_manager) :: psm
    integer :: plot_code
    character(len=*) :: plotName

    character(len=1024) :: inputCmd, tabName
    integer :: pIdx
    integer, intent(out) :: objIdx
    logical, intent(out) :: replot

    ! In headless mode skip all GTK tab machinery
    if (HEADLESS_MODE) then
      objIdx = -1
      replot = .FALSE.
      return
    end if

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
      ! Keep a requested plot number that does not exist yet (replay/restore);
      ! otherwise number the new plot as before.
      if (psm%plotNum < 1) then
        pIdx = zoatabMgr%getNumberOfPlotsByCode(plot_code)
        psm%plotNum = pIdx+1 ! Noreplot so this is the next num
      end if
      psm%baseCmd = withPlotNum(psm%baseCmd, psm%plotNum)
      inputCmd = trim(psm%generatePlotCommand())
      tabName = plotName
      if  (psm%plotNum > 1) then
        tabName = trim(tabName)//" "//int2str(psm%plotNum)
      end if

      objIdx = zoatabMgr%addMultiPlotTab(plot_code, &
      & trim(tabName)//c_null_char)
      call zoatabMgr%updateInputCommand(objIdx, inputCmd)
    end if


  end subroutine

  subroutine finalizeGoPlot_new(mplt,psm, replot, objIdx)
    use type_utils
    use zoa_plot
    use plot_setting_manager
    use handlers, only: zoatabMgr
    USE GLOBALS, only: HEADLESS_MODE

   use iso_fortran_env, only: real64
    implicit none
    type(multiplot) :: mplt
    type(zoaplot_setting_manager) :: psm

    integer :: objIdx
    logical :: replot

    ! In headless mode render directly to PNG via plplot; skip GTK tab machinery
    if (HEADLESS_MODE) then
      call mplt%draw()
      call mplt%clear()
      return
    end if

    call zoatabMgr%updateGenericMultiPlotTab(objIdx, mplt)
    if(replot .EQV. .FALSE. ) then
      call zoaTabMgr%finalize_with_psm(objIdx, psm)
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
    end if
    call mplt%clear()

  end subroutine


! This sub checks for whether a replot is needed or whether 
! this is a new plot, 
! If new plot, andcalls the finalize subs in 
! Zoa tab manager
subroutine finalizeGoPlot(mplt, psm, plot_code, plotName)
    use zoa_plot
    use plot_setting_manager
    use handlers, only: zoatabMgr
    use type_utils, only: int2str
    USE GLOBALS, only: HEADLESS_MODE

   use iso_fortran_env, only: real64
    implicit none
    type(multiplot) :: mplt
    type(zoaplot_setting_manager) :: psm
    integer :: plot_code
    character(len=*) :: plotName

    character(len=1024) :: inputCmd, tabName
    integer :: objIdx, pIdx
    logical :: replot

    ! In headless mode render directly to PNG via plplot; skip GTK tab machinery
    if (HEADLESS_MODE) then
      call mplt%draw()
      return
    end if

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
      ! Keep a requested plot number that does not exist yet (replay/restore);
      ! otherwise number the new plot as before.
      if (psm%plotNum < 1) then
        pIdx = zoatabMgr%getNumberOfPlotsByCode(plot_code)
        psm%plotNum = pIdx+1 ! Noreplot so this is the next num
      end if
      psm%baseCmd = withPlotNum(psm%baseCmd, psm%plotNum)
      inputCmd = trim(psm%generatePlotCommand())
      tabName = plotName
      if  (psm%plotNum > 1) then
        tabName = trim(tabName)//" "//int2str(psm%plotNum)
      end if
      objIdx = zoatabMgr%addGenericMultiPlotTab(plot_code, &
      & trim(tabName)//c_null_char, mplt)

      call zoaTabMgr%finalize_with_psm(objIdx, psm, trim(inputCmd))
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
    end if

end subroutine

! FFT/image plot implementations live in plot-functions-fft.f90

end module
