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
  use plot_command_utils, only: runKDPSpot
  use iso_c_binding, only:  c_ptr, c_null_char
  use zoa_plot
  use data_registers
  use mod_kdp_api, only: kdp_fob, kdp_ray, kdp_spd, kdp_capfn, kdp_get, kdp_silent_begin, kdp_silent_end
  use plot_composer, only: comp_active, comp_store

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

      module subroutine mtf_thf_go(psm)
        type(zoaplot_setting_manager) :: psm
      end subroutine mtf_thf_go
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
    ! the same data in double precision, for the Data tab
    real(real64), allocatable :: xd64(:), yd64(:,:)



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
    allocate(xd64(numPoints), yd64(numPoints,numTermsToPlot))


    do ii = 0, numPoints-1
      if (numPoints > 1) then
        xdat(ii+1) = REAL(ii)/REAL(numPoints-1)
      else
        xdat(ii+1) = 0.0
      end if
      ! Silent, like every other plot routine: this loop runs once per field,
      ! and with bare PROCESKDP each pass dumped the full CAPFN/FITZERN report
      ! into the console -- including whenever this tab was replotted.
      call kdp_silent_begin()
      if (numPoints > 1) then
        call kdp_fob(real(ii, real64)/real(numPoints-1, real64))
      else
        call kdp_fob(0.0_real64)
      end if
      call kdp_capfn(pupilGrid)
      call kdp_silent_end()
      write(ffieldstr, *) lambda
      CALL PROCESSILENT("FITZERN, "//ffieldstr)

      !CALL PROCESKDP("SHO RMSOPD")
      if (numPoints > 1) then
        xd64(ii+1) = (real(ii, real64)/real(numPoints-1, real64))*sysConfig%refFieldValue(2)
      else
        xd64(ii+1) = 0.0_real64
      end if
      xdat(ii+1) = real(xd64(ii+1))
      do k=1,numTermsToPlot
        iz = zlist(k)
        if (iz >= 1 .and. iz <= size(X)) then
          yd64(ii+1,k) = X(iz)
        else
          yd64(ii+1,k) = 0.0_real64
        end if
        ydat(ii+1,k) = real(yd64(ii+1,k))
      end do
    end do

    do k=1,numTermsToPlot
      zLegend(k) = 'Z'//trim(int2str(zlist(k)))
    end do

    ! Log the table in BOTH modes, like rmsfield_go: this routine used to
    ! return before logging when headless, which left the coefficient table
    ! -- the actual content of this plot -- with no test coverage at all.
    if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    call logDataVsField(xd64, yd64, zLegend)
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
      call setPlotNum(psm%baseCmd, psm%plotNum)
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
  

  call runKDPSpot(iField, iLambda, iMethod, nRect, nRand, nRing)
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

! The image positions of the rays the last SPD traced successfully.  Records
! 1..ITOT-1 are this trace's; a failed ray has a non-zero failure code
! (DSPOTT(7)) and no wavelength weight (DSPOTT(17)).  Testing the position
! instead (X /= 0 and Y /= 0) also dropped every good ray lying on an axis --
! the chief ray of a ring pattern, a rectangular grid's centre row/column.
subroutine getSpotData(xData,yData, delta)
  use GLOBALS
  use DATSP1, only: ITOT
  use iso_fortran_env
  real(kind=real64), allocatable, intent(inout) :: xData(:), yData(:)
  ! focus shift: each ray carried on along its slope to the plane delta
  ! from the image surface (DSPOTT 9/10 are its XZ/YZ slope angles)
  real(kind=real64), intent(in), optional :: delta
  logical, allocatable :: ok(:)
  integer :: n

  n = min(ITOT - 1, size(DSPOTT, 2))
  if (n < 1) then
    allocate(xData(0), yData(0))
    return
  end if
  allocate(ok(n))
  ok = DSPOTT(7,1:n) == 0.0d0 .and. DSPOTT(17,1:n) /= 0.0d0
  if (present(delta)) then
    allocate(xData, source=pack(DSPOTT(1,1:n) + delta*tan(DSPOTT(9,1:n)), ok))
    allocate(yData, source=pack(DSPOTT(2,1:n) + delta*tan(DSPOTT(10,1:n)), ok))
  else
    allocate(xData, source=pack(DSPOTT(1,1:n), ok))
    allocate(yData, source=pack(DSPOTT(2,1:n), ok))
  end if

end subroutine

! The RMS spot size SPD reports (focal systems), for the current spot data
! moved to focus delta: about the energy-weighted centroid, RSS of X and Y,
! as SPOT1 computes it (its RMS at delta = 0).
real(kind=real64) function spotRmsAtFocus(delta) result(rms)
  use GLOBALS
  use DATSP1, only: ITOT
  use DATLEN, only: COS_A_ANG, COS_B_ANG
  use iso_fortran_env
  real(kind=real64), intent(in) :: delta
  real(kind=real64) :: w, spt, ap, x, y, cx, cy, mx, my, rmsx, rmsy
  integer :: k, n

  rms = 0.0d0
  n = min(ITOT - 1, size(DSPOTT, 2))
  w = 0.0d0; cx = 0.0d0; cy = 0.0d0
  do k = 1, n
    if (DSPOTT(7,k) /= 0.0d0) cycle
    if (DSPOTT(12,k) /= 0.0d0) w = w + DSPOTT(12,k)
    spt = DSPOTT(17,k)
    if (spt == 0.0d0) cycle
    ap = DSPOTT(11,k)
    cx = cx + spt*(DSPOTT(1,k) + delta*tan(DSPOTT(9,k)))*ap
    cy = cy + spt*(DSPOTT(2,k) + delta*tan(DSPOTT(10,k)))*ap
  end do
  if (w == 0.0d0) return
  cx = cx/w; cy = cy/w
  mx = 0.0d0; my = 0.0d0
  do k = 1, n
    if (DSPOTT(7,k) /= 0.0d0) cycle
    spt = DSPOTT(17,k)
    if (spt == 0.0d0) cycle
    ap = DSPOTT(11,k)
    x = DSPOTT(1,k) + delta*tan(DSPOTT(9,k))
    y = DSPOTT(2,k) + delta*tan(DSPOTT(10,k))
    mx = mx + spt*(x - cx)**2*ap
    my = my + spt*(y - cy)**2*ap
  end do
  rmsx = 2.0d0*sqrt(mx/w)/COS_B_ANG
  rmsy = 2.0d0*sqrt(my/w)/COS_A_ANG
  rms = sqrt(rmsx*rmsx + rmsy*rmsy)
end function

! SPO THF (spots through focus) rather than SPO / SPO FLD (per field)
logical function spotIsThroughFocus(psm)
  use strings, only: parse
  use plot_setting_manager, only: zoaplot_setting_manager
  type(zoaplot_setting_manager), intent(in) :: psm
  character(len=80) :: tokens(40)
  character(len=len(psm%baseCmd)) :: cmd
  integer :: n, k
  spotIsThroughFocus = .false.
  cmd = psm%baseCmd
  call parse(cmd, ' ', tokens, n)
  do k = 2, n
    if (trim(tokens(k)) == 'THF') spotIsThroughFocus = .true.
  end do
end function

! SPO [FLD]: one spot per field (or the chosen field), at focus FFO.
! SPO THF:   the chosen field at each focus position FFO + (k-1)*IFO,
!            k = 1..NFO, in a row or a column (ORI), on one scale.
! The rays are traced once per field and wavelength and moved to each focus
! along their slopes (the lens is not changed).
subroutine spo_go(psm)

    use iso_fortran_env
    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use global_widgets, only:  sysConfig, ioConfig, curr_par_ray_trace
    use zoa_ui
    use kdp_utils, only: OUTKDP, logDataVsField, logColumns
    use type_utils, only: int2str, str2int
    use plot_setting_manager
    use kdp_data_types, only: LENS_UNITS_INCHES, LENS_UNITS_CM, LENS_UNITS_M
    use focus_settings, only: focus_first, focus_count, focus_position
    use mod_system, only: sys_mode
    use DATMAI
    use DATSPD
    use DATLEN
    use g
    use type_utils


    IMPLICIT NONE

    type(multiplot) :: mplt
    type(zoaplot), allocatable :: xyscat(:)
    type(c_ptr) :: canvas

    type(zoaplot_setting_manager) :: psm

    integer :: iField, iLambda, iMethod, nRect, nRand, nRing
    integer :: objIdx, i, j, iPanel, nPanels, plotCode
    logical :: replot, allWL, thf, horiz

    real(kind=real64), allocatable :: xSpot(:), ySpot(:)
    real(kind=real64) :: yAvg, delta, rmsPanel, commonExtent
    real :: plotScale
    logical :: drawAiry
    integer, parameter :: AIRY_NPTS = 181
    real :: airyX(AIRY_NPTS), airyY(AIRY_NPTS), airyRadius, wlLensUnits, theta
    real :: spotExtent, airyScale
    character(len=40) :: unitsTxt
    ! Data tab rows: field, wavelength, focus, X, Y of every plotted ray
    real(kind=real64), allocatable :: rays(:,:)
    integer :: nRays
    call psm%getSpotDiagramSettings(iField, iLambda, iMethod, nRect, nRand, nRing, plotScale)
    nRays = 0
    thf = spotIsThroughFocus(psm)
    unitsTxt = sysConfig%lensUnits(sysConfig%currLensUnitsID)%text

    allWL = .FALSE.
    if (iLambda == ID_SETTING_WAVELENGTH_ALL) then
      allWL = .TRUE.
      iLambda = 1
    end if
    ! focus shifts are along the image-space ray slopes: focal systems only
    if (sys_mode() > 2.0d0 .and. (thf .or. focus_first() /= 0.0d0)) then
      call zoa_emit("SPO: focus positions (FFO/IFO/NFO) need a focal system; nominal focus used", "red")
    end if

    plotCode = ID_PLOTTYPE_SPOT_NEW
    if (thf) plotCode = ID_PLOTTYPE_SPOT_THF
    if (thf) then
      call initializeGoPlot(psm, plotCode, "Spot Through Focus", replot, objIdx)
    else
      call initializeGoPlot(psm, plotCode, "Spot Diagram", replot, objIdx)
    end if

    if (.not. HEADLESS_MODE) then
      call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    end if

    ! Field Point selects one field or, with ALL, every field stacked; THF
    ! has one panel per focus position.  Decide this before sizing the
    ! canvas: the drawing area has to match the panel count or the plot is
    ! drawn into the wrong aspect ratio (a single field in a 3-field-tall
    ! area came out stretched into an ellipse).
    horiz = .false.
    if (thf) then
      nPanels = focus_count()
      horiz = psm%getSpotHorizontal()
    else if (iField == ID_SETTING_FIELD_ALL) then
      nPanels = sysConfig%numFields
    else
      nPanels = 1
    end if
    allocate(xyscat(nPanels))

    ! Prep PLot
    if (HEADLESS_MODE) then
      canvas = c_null_ptr
    else if (horiz) then
      canvas = hl_gtk_drawing_area_new(size=[400*nPanels,400], has_alpha=FALSE)
    else
      canvas = hl_gtk_drawing_area_new(size=[400,400*nPanels], &
      & has_alpha=FALSE)
    end if

    ! Todo:  change initialization to sepcify size, and then don't need to
    ! call gtk_drawing_area
    if (horiz) then
      call mplt%initialize(canvas, 1, nPanels)
      mplt%height = 400
      mplt%width = 400*nPanels
    else
      call mplt%initialize(canvas, nPanels,1)
      mplt%height = 400*nPanels
      mplt%width = 400
    end if

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
        & trim(unitsTxt)// &
        & "   (1.22 * "//trim(real2str(sysConfig%getWavelength(iLambda),5))// &
        & "um * F/"//trim(real2str(curr_par_ray_trace%FNUM,4))//")")
      else
        call zoa_emit("Airy radius is not computable for this system", "red")
        drawAiry = .FALSE.
      end if
    end if

    ! THF: one trace for every panel, and one scale across them (the point
    ! is to compare the spots), unless SSI sets it
    commonExtent = 0.0d0
    if (thf) then
      call kdp_silent_begin()
      call runKDPSpot(iField, iLambda, iMethod, nRect, nRand, nRing)
      call kdp_silent_end()
      do iPanel = 1, nPanels
        call getSpotData(xSpot, ySpot, panelFocus(iPanel))
        if (size(ySpot) > 0) then
          ySpot = ySpot - sum(ySpot)/size(ySpot)
          commonExtent = max(commonExtent, maxval(abs(xSpot)), maxval(abs(ySpot)))
        end if
        deallocate(xSpot, ySpot)
      end do
    end if

    do iPanel=1,nPanels
      if (thf .or. iField /= ID_SETTING_FIELD_ALL) then
        i = iField
      else
        i = iPanel
      end if
      delta = panelFocus(iPanel)
      if (.not. thf) then
        ! silent: the Data tab gets the plotted rays below, not SPD's report
        call kdp_silent_begin()
        call runKDPSpot(i, iLambda, iMethod, nRect, nRand, nRing)
        call kdp_silent_end()
      end if
      if(allocated(xSpot)) deallocate(xSpot)
      if(allocated(ySpot)) deallocate(ySpot)

      call getSpotData(xSpot, ySpot, delta)
      ! SPD's own RMS at nominal focus; moved, the same statistic there
      rmsPanel = RMS
      if (delta /= 0.0d0) rmsPanel = spotRmsAtFocus(delta)

      !Subtract Avg
      yAvg = (sum(ySpot)/size(ySpot))
      ySpot = ySpot - yAvg
      call addRays(i, iLambda, delta)
      spotExtent = real(max(maxval(abs(xSpot)), maxval(abs(ySpot))))
                                  

    ! TODO:  remove dependency on canvas here after checking it doesn't break anything.
    if (thf .or. delta /= 0.0d0) then
      ! the focus above the spot, its RMS below (one line was as wide as a
      ! panel of a row of focus positions)
      call xyscat(iPanel)%initialize(c_null_ptr, REAL(xSpot), REAL(ySpot), &
      & xlabel='RMS '//trim(real2str(rmsPanel))//' '// &
      & trim(unitsTxt)//c_null_char, &
      & ylabel=''//c_null_char, &
      & title='Focus '//trim(compactReal(delta, 6))//c_null_char)
    else
      call xyscat(iPanel)%initialize(c_null_ptr, REAL(xSpot), REAL(ySpot), &
      & xlabel='RMS = '//trim(real2str(rmsPanel))//' '// &
      & trim(unitsTxt)//c_null_char, &
      & ylabel=''//c_null_char, &
      & title=''//c_null_char)
    end if


    call xyscat(iPanel)%setLineStyleCode(-1)
    
    if (plotScale /= 0) then 
      call xyscat(iPanel)%setYScale(real(plotScale,8))
      call xyscat(iPanel)%setXScale(real(plotScale,8))
    else if (thf .and. commonExtent > 0.0d0) then
      call xyscat(iPanel)%setYScale(commonExtent*1.05d0)
      call xyscat(iPanel)%setXScale(commonExtent*1.05d0)
    end if
    call xyscat(iPanel)%removeGrids()
    call xyscat(iPanel)%removeLabels()
    ! True shape: one length unit is the same on both axes, whatever the
    ! panel's shape (a MULTIPLOT cell is rarely square; nor quite is the
    ! default viewport) and however X and Y autoscale.
    call xyscat(iPanel)%setIsotropic()

    ! Airy disk overlay (AIRY ON): a black circle of radius 1.22*lambda*F/#,
    ! drawn on every field point.  F/# is the working (image-space) F-number,
    ! since that is the cone that sets the diffraction limit.
    if (drawAiry .and. airyRadius > 0.0) then
      call xyscat(iPanel)%addXYPlot(airyX, airyY)
      call xyscat(iPanel)%setDataColorCode(PL_PLOT_BLACK)
      call xyscat(iPanel)%setLineStyleCode(1)
    end if

    if (iPanel==1) call xyscat(iPanel)%addScaleBar(POS_LOWER_RIGHT)      

    if (allWL) then
      do j=2,sysConfig%numWavelengths
        call kdp_silent_begin()
        call runKDPSpot(i, j, iMethod, nRect, nRand, nRing)
        call kdp_silent_end()
        if(allocated(xSpot)) deallocate(xSpot)
        if(allocated(ySpot)) deallocate(ySpot)
        call getSpotData(xSpot, ySpot, delta)
        ! Subtract relative to first wavelength.  Should probably be relative to
        ! ref wavelength; revisit this after testing
        ySpot = ySpot - yAvg
        call addRays(i, j, delta)
        spotExtent = max(spotExtent, &
        & real(max(maxval(abs(xSpot)), maxval(abs(ySpot)))))
        call xyscat(iPanel)%addXYPlot(real(xSpot), real(ySpot))
        call xyscat(iPanel)%setDataColorCode(sysConfig%wavelengthColorCodes(j))
        call xyscat(iPanel)%setLineStyleCode(-1)
      end do
    end if

    ! The Airy disk is only a circle if the two axes share a scale, and the
    ! spot diagram otherwise autoscales X and Y independently.  Square the
    ! window around the data (and the disk, so it cannot be clipped) when the
    ! overlay is on and the user has not set an explicit scale.
    if (drawAiry .and. airyRadius > 0.0 .and. plotScale == 0) then
      airyScale = max(spotExtent, real(commonExtent), airyRadius)*1.05
      call xyscat(iPanel)%setYScale(real(airyScale,8))
      call xyscat(iPanel)%setXScale(real(airyScale,8))
    end if

    ! fields from the bottom up; focus positions left to right or top down
    if (horiz) then
      call mplt%set(1, iPanel, xyscat(iPanel))
    else if (thf) then
      call mplt%set(iPanel, 1, xyscat(iPanel))
    else
      call mplt%set(nPanels-iPanel+1,1,xyscat(iPanel))
    end if

    end do



    ! Data tab: every plotted ray, Y relative to the panel's first-wavelength
    ! centroid exactly as drawn.
    if (nRays > 0) then
      if (thf) then
        call OUTKDP('Spot diagram rays through focus; focus, X, Y in '//trim(unitsTxt)// &
        &           ', Y relative to the Y centroid at each focus')
        call logColumns([character(len=20) :: 'Field', 'Wavelength', 'Focus', 'X', 'Y'], &
        &               rays(1:nRays, :))
      else
        if (focus_first() /= 0.0d0) call OUTKDP('At focus FFO = '// &
        &  trim(real2str(focus_first()))//' '//trim(unitsTxt))
        call OUTKDP('Spot diagram rays; X, Y in '// &
        &           trim(unitsTxt)// &
        &           ', Y relative to the Y centroid of the first wavelength drawn')
        call logColumns([character(len=20) :: 'Field', 'Wavelength', 'X', 'Y'], &
        &               rays(1:nRays, [1, 2, 4, 5]))
      end if
    end if

    !call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    call ioConfig%restoreTextView()

    call finalizeGoPlot_new(mplt, psm, replot, objIdx)


    !call finalizeGoPlot(mplt, psm, ID_PLOTTYPE_SPOT_NEW, "Spot Diagram")

  contains

    ! the focus of panel k: THF's k-th position; otherwise FFO
    real(kind=real64) function panelFocus(k)
      integer, intent(in) :: k
      panelFocus = 0.0d0
      if (sys_mode() > 2.0d0) return
      if (thf) then
        panelFocus = focus_position(k)
      else
        panelFocus = focus_first()
      end if
    end function

    ! Append the rays now in xSpot/ySpot (field f, wavelength w, focus d).
    subroutine addRays(f, w, d)
      integer, intent(in) :: f, w
      real(kind=real64), intent(in) :: d
      integer :: n
      real(kind=real64), allocatable :: grown(:,:)
      n = size(xSpot)
      if (.not. allocated(rays)) then
        allocate(rays(max(n, 1024), 5))
        nRays = 0
      end if
      if (nRays + n > size(rays, 1)) then
        allocate(grown(max(2*size(rays, 1), nRays + n), 5))
        grown(1:nRays, :) = rays(1:nRays, :)
        call move_alloc(grown, rays)
      end if
      rays(nRays+1:nRays+n, 1) = real(f, real64)
      rays(nRays+1:nRays+n, 2) = real(w, real64)
      rays(nRays+1:nRays+n, 3) = d
      rays(nRays+1:nRays+n, 4) = xSpot
      rays(nRays+1:nRays+n, 5) = ySpot
      nRays = nRays + n
    end subroutine

end subroutine


subroutine seidel_go(psm)
    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use global_widgets, only:  curr_par_ray_trace, curr_lens_data, ioConfig, sysConfig
    use kdp_utils, only: OUTKDP, logDataVsField, logColumns
    use type_utils, only: int2str, str2int, real2str
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
    ! Compute without the THO-style table (its 5 fixed decimals are THO's
    ! format); the Data tab gets the plotted bars at full precision instead.
    call MMAB3_NEW(.TRUE., psm%getWavelengthSetting(), .FALSE.)
    block
      real(real64), allocatable :: tbl(:,:)
      integer :: s, n
      n = curr_lens_data%num_surfaces
      allocate(tbl(n+1, nS+1))
      do s = 0, n
        tbl(s+1, 1) = real(s, real64)
        tbl(s+1, 2:) = curr_par_ray_trace%CSeidel(1:nS, s)
      end do
      call OUTKDP(trim(sysConfig%lensTitle)//'; Seidel aberrations, wavelength '// &
      &           trim(real2str(1000.0*sysConfig%getWavelength(psm%getWavelengthSetting()),1))// &
      &           ' nm; the last row (surface '//trim(int2str(n))//') is the sum')
      call logColumns([character(len=8) :: 'Surface', 'SA3', 'CMA3', 'AST3', 'DIS3', &
      &                'PTZ3', 'AC3', 'LC3'], tbl)
    end block
    if (.not. HEADLESS_MODE) then
      call checkSeidelDataTab()
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

  contains

    ! The Seidel Data tab was once seen blank after a lens change, but the
    ! failure could not be reproduced.  Leave a trace in the log file (only)
    ! if it happens: the table should be in the tab once MMAB3 has printed.
    subroutine checkSeidelDataTab()
      use gtk, only: gtk_text_view_get_buffer, gtk_text_buffer_get_char_count
      use zoa_output, only: zoa_output_is_suppressed
      type(c_ptr) :: tv
      integer :: nch
      character(len=160) :: ln
      tv = getTabTextView(objIdx)
      nch = -1
      if (c_associated(tv)) nch = gtk_text_buffer_get_char_count(gtk_text_view_get_buffer(tv))
      if (nch > 0 .and. c_associated(ioConfig%textView, tv)) return
      write(ln,'(A,L1,A,I0,A,L1,A,L1,A,I0,A,L1,A,L1)') 'WARNING Seidel Data tab not filled: replot=', replot, &
      &  ' objIdx=', objIdx, ' tabTV=', c_associated(tv), &
      &  ' outputToTab=', c_associated(ioConfig%textView, tv), ' chars=', nch, &
      &  ' dumpText=', ioConfig%dumpText, ' suppressed=', zoa_output_is_suppressed()
      call LogTermFOR(trim(ln))
    end subroutine
  
    


end subroutine

subroutine ast_go(psm)

    USE GLOBALS
    use command_utils
    use zoa_output, only: zoa_emit
    use zoa_plot
    use kdp_utils, only: OUTKDP, logDataVsField, logColumns
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
     ! the plotted curves at full precision, for the Data tab
     real(real64) :: fcTan(0:50), fcSag(0:50), fldFC(0:50), dist64(0:50), fldDist(0:50), unused(0:50)

     !COMMON FLDAN, DDTA

     lsa = .FALSE.

     call psm%getAstigSettings(idxFieldXY, numPts, astMax, dstMax)

     call initializeGoPlot(psm,ID_PLOTTYPE_AST, "Field Curv / Dist", replot, objIdx)

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
  call getFieldCalcResult64(fcTan, fcSag, fldFC, numPtsFC, 3)


   call lin3%initialize(c_null_ptr, REAL(x1FC(0:numPtsFC)),yFC(0:numPtsFC), &
   & xlabel='Field Curvature '//c_null_char, &
   & ylabel=sysConfig%getFieldText()//c_null_char, &
   & title=''//c_null_char)
   call lin3%addXYPlot(X2FC(0:numPtsFC),FLDAN(0:numPtsFC))
   call lin3%setDataColorCode(PL_PLOT_BLUE)
   call lin3%setLineStyleCode(4)

CALL PROCESSILENT('DIST'//trim(ftext)//int2str(numPts))

 call getFieldCalcResult(xDist, x2FC, yDist, numPtsDist, 2)
 call getFieldCalcResult64(dist64, unused, fldDist, numPtsDist, 2)

 ! Data tab: the field-curvature (tangential = YZ, sagittal = XZ) and
 ! distortion curves the two panels draw, one row per field point.
 call OUTKDP('Field curvature and distortion; field = '//trim(sysConfig%getFieldText()))
 if (numPtsDist == numPtsFC .and. all(fldDist(0:numPtsFC) == fldFC(0:numPtsFC))) then
   call logColumns([character(len=20) :: 'Field', 'FCurvTan['//trim(fcUnits())//']', &
   &               'FCurvSag['//trim(fcUnits())//']', 'Distortion[%]'], &
   &               reshape([fldFC(0:numPtsFC), fcTan(0:numPtsFC), fcSag(0:numPtsFC), &
   &                        dist64(0:numPtsFC)], [numPtsFC+1, 4]))
 else
   call logColumns([character(len=20) :: 'Field', 'FCurvTan['//trim(fcUnits())//']', &
   &               'FCurvSag['//trim(fcUnits())//']'], &
   &               reshape([fldFC(0:numPtsFC), fcTan(0:numPtsFC), fcSag(0:numPtsFC)], &
   &                       [numPtsFC+1, 3]))
   call logColumns([character(len=20) :: 'Field', 'Distortion[%]'], &
   &               reshape([fldDist(0:numPtsDist), dist64(0:numPtsDist)], [numPtsDist+1, 2]))
 end if


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

  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
  ! (headless, finalizeGoPlot_new draws the PNG -- or hands the plot to a
  ! MULTIPLOT page)
  call finalizeGoPlot_new(mplt, psm, replot, objIdx)

contains

  ! field curvature is in lens units, or diopters for an afocal system
  function fcUnits() result(u)
    use mod_system, only: sys_mode
    character(len=20) :: u
    if (sys_mode() > 2.0d0) then
      u = 'diopters'
    else
      u = sysConfig%getLensUnitsText()
    end if
  end function

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
    use kdp_utils, only: OUTKDP, logColumns
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
    ! the plotted fans at full precision, for the Data tab: pupil coordinate,
    ! then one column per wavelength drawn
    real(real64) :: yOff64(sysConfig%numFields)
    real(real64), allocatable :: tanD(:,:), sagD(:,:)
    character(len=20), allocatable :: tanHdr(:), sagHdr(:)
    integer :: nW

    character(len=100) :: xlabel, ylabel, title
    ! WFR: wave aberration (the fans' OPD, waves) instead of the transverse
    ! aberration (CODE V RIM WFR)
    logical :: wfr
    character(len=20) :: yCmd, xCmd, valPfxT, valPfxS
    integer :: colT, colS

    call initializeGoPlot(psm,ID_PLOTTYPE_RIM, "Ray Aberration Fan", replot, objIdx)

    wfr = psm%getRimWfr()
    if (wfr) then
      ! the fans' OPD qualifier: column 3 is the OPD in waves (relative to
      ! the chief ray, so no offset is subtracted)
      yCmd = "YFAN OPD, -1, 1, "; xCmd = "XFAN OPD, 0, 1, "
      colT = 3; colS = 3
      valPfxT = 'W_'; valPfxS = 'W_'
    else
      yCmd = "YFAN, -1, 1, "; xCmd = "XFAN, 0, 1, "
      colT = 2; colS = 1
      valPfxT = 'DY_'; valPfxS = 'DX_'
    end if


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
      call kdp_fob(sysConfig%relativeFields(2,i), sysConfig%relativeFields(1,i))
      ! Set Fan Input - TODO:  add setting to change fan type
      write(ffieldstr, FMTFAN) sysConfig%refWavelengthIndex ,',',3
      
      CALL PROCESSILENT("YFAN, -1, 1, "//ffieldstr) 
      yOff(i) = curr_ray_fan_data%xyfan(2,2) ! only 3 points computed.  Min requireed to get ref value
      yOff64(i) = curr_ray_fan_data%xyfan(2,2)
      if (wfr) then
        yOff(i) = 0.0
        yOff64(i) = 0.0_real64
      end if
    end do

    plotScale = psm%getSettingValueByCode(SETTING_SCALE)
    numPoints = psm%getDensitySetting()


    allocate(x(numPoints))
    allocate(y(numPoints))
    nW = 1
    if (allWL) nW = sysConfig%numWavelengths
    allocate(tanD(numPoints, nW+1), sagD(numPoints, nW+1), tanHdr(nW+1), sagHdr(nW+1))
    tanHdr(1) = 'RelPupilY'
    sagHdr(1) = 'RelPupilX'
    do j = 1, nW
      if (allWL) then
        tanHdr(j+1) = trim(valPfxT)//trim(real2str(1000.0*sysConfig%getWavelength(j),2))//'nm'
        sagHdr(j+1) = trim(valPfxS)//trim(real2str(1000.0*sysConfig%getWavelength(j),2))//'nm'
      else
        tanHdr(j+1) = trim(valPfxT)//trim(real2str(1000.0*sysConfig%getWavelength(lambda),2))//'nm'
        sagHdr(j+1) = trim(valPfxS)//trim(real2str(1000.0*sysConfig%getWavelength(lambda),2))//'nm'
      end if
    end do

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
    ! the canvas's size (headless used the 1200 x 500 default, which leaves
    ! the bottom panel too little height)
    mplt%width = 1200
    mplt%height = 1200

    do i=1,sysConfig%numFields

      ! Set Field
      ! Silent from here on: output points at the Data tab, which gets the
      ! plotted fans below rather than the legacy fan reports.
      call kdp_silent_begin()
      call kdp_fob(sysConfig%relativeFields(2,i), sysConfig%relativeFields(1,i))
      call kdp_silent_end()


      ! Set Fan Input - TODO:  add setting to change fan type
      write(ffieldstr, FMTFAN) lambda,',',numPoints

      CALL PROCESSILENT(trim(yCmd)//" "//ffieldstr)
      x = curr_ray_fan_data%relAper
      y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,colT)-yOff(i)
      tanD(:,1) = curr_ray_fan_data%relAper(1:numPoints)
      tanD(:,2) = curr_ray_fan_data%xyfan(1:numPoints,colT) - yOff64(i)

      
      ! Hide labels for off axis points
      if (i /= 1) then 
        xlabel = ''
        ylabel = ''
      else
        xlabel = 'Rel. Pupil Position'
        ylabel = 'Error '//trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)
        if (wfr) ylabel = 'OPD waves'
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
            CALL PROCESSILENT(trim(yCmd)//" "//ffieldstr)
            x = curr_ray_fan_data%relAper
            y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,colT)-yOff(i)
            tanD(:,j+1) = curr_ray_fan_data%xyfan(1:numPoints,colT) - yOff64(i)
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
      CALL PROCESSILENT(trim(xCmd)//" "//ffieldstr)
      x = curr_ray_fan_data%relAper
      y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,colS)
      sagD(:,1) = curr_ray_fan_data%relAper(1:numPoints)
      sagD(:,2) = curr_ray_fan_data%xyfan(1:numPoints,colS)

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
          CALL PROCESSILENT(trim(xCmd)//" "//ffieldstr)
          y(1:numPoints) = curr_ray_fan_data%xyfan(1:numPoints,colS)
          sagD(:,j+1) = curr_ray_fan_data%xyfan(1:numPoints,colS)
          call sagplots(i)%addXYPlot(x,y)
          call sagplots(i)%setDataColorCode(sysConfig%wavelengthColorCodes(j))
        end do
      end if

      call mplt%set(sysConfig%numFields-i+1,2,sagplots(i))

      ! Data tab: this field's two fans, as drawn (DY less the chief-ray
      ! offset at the reference wavelength).
      if (wfr) then
        call OUTKDP('Field '//trim(int2str(i))//' ('//trim(sysConfig%getAbsYFieldText(i))// &
        &           '): tangential fan, OPD [waves]')
        call logColumns(tanHdr, tanD)
        call OUTKDP('Field '//trim(int2str(i))//' ('//trim(sysConfig%getAbsYFieldText(i))// &
        &           '): sagittal fan, OPD [waves]')
        call logColumns(sagHdr, sagD)
      else
      call OUTKDP('Field '//trim(int2str(i))//' ('//trim(sysConfig%getAbsYFieldText(i))// &
      &           '): tangential fan, DY ['// &
      &           trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)//']')
      call logColumns(tanHdr, tanD)
      call OUTKDP('Field '//trim(int2str(i))//' ('//trim(sysConfig%getAbsYFieldText(i))// &
      &           '): sagittal fan, DX ['// &
      &           trim(sysConfig%lensUnits(sysConfig%currLensUnitsID)%text)//']')
      call logColumns(sagHdr, sagD)
      end if
      

     !call mplt%set(2,i,sagplots(i))

     end do

     if (wfr) then
       call mplt%addBottomPanel(trim(sysConfig%lensTitle),  &
       & "Wave Aberration (waves)",trim(real2str(1000.0*sysConfig%getWavelength(lambda),2))// " nm")
     else
       call mplt%addBottomPanel(trim(sysConfig%lensTitle),  &
       & "Ray Aberrations ("//sysConfig%getDimensions()//")",trim(real2str(1000.0*sysConfig%getWavelength(lambda),2))// " nm")
     end if
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
! the same data in double precision, for the Data tab
real(real64), allocatable :: x64(:), y64(:)
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

allocate(x(numPoints), x64(numPoints))
allocate(y(numPoints), y64(numPoints))

do ii = 0, numPoints-1
 if (numPoints > 1) then
   x(ii+1) = REAL(ii)/REAL(numPoints-1)
   x64(ii+1) = real(ii, real64)/real(numPoints-1, real64)
 else
   x(ii+1) = 0.0
   x64(ii+1) = 0.0_real64
 end if
 call kdp_silent_begin()
 call kdp_fob(x64(ii+1))
 call kdp_silent_end()
 select case(iData)

 case(ID_RMS_DATA_WAVE)
    ! Explicit grid (default 16, KDP's own): a bare CAPFN inherits the last
    ! caller's CAPDEF global, so this plot's numbers would change with
    ! whatever grid another plot had used.
    call kdp_silent_begin()
    call kdp_capfn(pupilGrid, qual='SILENT')
    call kdp_silent_end()
    y64(ii+1) = 1000.0_real64*kdp_get('RMSOPD')
 case(ID_RMS_DATA_SPOT)
  ! PROCESSILENT, matching the wavefront branch above: a bare PROCESKDP here
  ! dumped the full KDP spot-diagram report (aperture assignment, ray-trace
  ! progress, SPOT DIAGRAM SUMMARY) into the terminal once per field point.
  call kdp_silent_begin()
  call kdp_spd()
  call kdp_silent_end()
  y64(ii+1) = kdp_get('RMS')
 end select
 y(ii+1) = real(y64(ii+1))

 x64(ii+1) = x64(ii+1)*sysConfig%refFieldValue(2)
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
  call log2DData(x64, y64, xHeader='Field', &
  & yHeader='RMS['//trim(sysConfig%getLensUnitsText())//']')
case default
  call log2DData(x64, y64, xHeader='Field', yHeader='RMS[mWaves]')
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

! RAD -- geometric radial (encircled) energy, CODE V's RAD option.  For each
! field: trace the polychromatic spot (rectangular grid, NRD rays across the
! pupil, wavelength-weighted), sort the rays by distance from the energy-
! weighted centroid, and accumulate their energy.  Plots the enclosed energy
! (%) vs radius per field; the Data tab holds those curves on a common radius
! grid (paste into MATLAB/Python as columns); the radius enclosing each PER
! percentage is listed in the terminal.
subroutine rad_go(psm)
  USE GLOBALS
  use global_widgets, only: sysConfig, ioConfig
  use kdp_utils, only: OUTKDP
  use type_utils, only: int2str
  use DATSP1, only: ITOT, SPDTYPE, NRECT
  use zoa_output, only: zoa_emit
  use plplot, PI => PL_PI
  use plplot_extra
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_fortran_env, only: real64
  implicit none
  type(zoaplot_setting_manager) :: psm
  type(zoaplot) :: xyscat
  type(multiplot) :: mplt
  type(c_ptr) :: canvas
  logical :: replot
  integer :: objIdx, nrd, nper, nf, f, i, j, k, n, savedSpdType, savedNRect
  integer, parameter :: NGRID = 101
  real(real64) :: per(10), cx, cy, wsum, acc, target, rmax
  real(real64), allocatable :: r(:,:), w(:,:), rads(:,:), grid(:), enc(:,:)
  integer, allocatable :: nRays(:), order(:)
  character(len=1024) :: line
  character(len=16) :: cell
  character(len=30), allocatable :: legend(:)
  integer, parameter :: colors(6) = [PL_PLOT_BLUE, PL_PLOT_RED, PL_PLOT_GREEN, &
  &                                  PL_PLOT_BLACK, PL_PLOT_MAGENTA, PL_PLOT_CYAN]

  call initializeGoPlot(psm, ID_PLOTTYPE_RAD, "Radial Energy", replot, objIdx)
  call psm%getRADSettings(nrd, per, nper)
  nf = sysConfig%numFields

  ! Pin the spot sampling for this plot and restore it after (as RMS vs
  ! Field does), so neither this plot nor a later spot diagram inherits the
  ! other's sampling.
  savedSpdType = SPDTYPE
  savedNRect   = NRECT
  SPDTYPE = 1
  NRECT   = nrd

  allocate(nRays(nf), rads(10, nf))
  nRays = 0
  rads = 0.0d0
  allocate(r(1, nf), w(1, nf))
  r = 0.0d0; w = 0.0d0
  do f = 1, nf
    call kdp_silent_begin()
    call kdp_fob(sysConfig%relativeFields(2,f), sysConfig%relativeFields(1,f))
    call kdp_spd()
    call kdp_silent_end()
    if (.not. allocated(DSPOTT)) cycle
    if (size(r, 1) < ITOT) then
      ! grow the per-field ray storage (fields share one array)
      block
        real(real64), allocatable :: r2(:,:), w2(:,:)
        allocate(r2(ITOT, nf), w2(ITOT, nf))
        r2 = 0.0d0; w2 = 0.0d0
        r2(1:size(r,1), :) = r; w2(1:size(w,1), :) = w
        call move_alloc(r2, r); call move_alloc(w2, w)
      end block
    end if
    cx = 0.0d0; cy = 0.0d0; wsum = 0.0d0
    do i = 1, ITOT - 1
      if (DSPOTT(17,i) == 0.0d0 .or. DSPOTT(12,i) == 0.0d0) cycle
      wsum = wsum + DSPOTT(12,i)
      cx = cx + DSPOTT(12,i)*DSPOTT(1,i)
      cy = cy + DSPOTT(12,i)*DSPOTT(2,i)
    end do
    if (wsum <= 0.0d0) cycle
    cx = cx/wsum; cy = cy/wsum
    n = 0
    do i = 1, ITOT - 1
      if (DSPOTT(17,i) == 0.0d0 .or. DSPOTT(12,i) == 0.0d0) cycle
      n = n + 1
      r(n, f) = hypot(DSPOTT(1,i) - cx, DSPOTT(2,i) - cy)
      w(n, f) = DSPOTT(12,i)/wsum
    end do
    ! sort this field's rays by radius (insertion sort; a few thousand rays)
    allocate(order(n))
    order = [(i, i = 1, n)]
    do i = 2, n
      k = order(i); j = i - 1
      do while (j >= 1)
        if (r(order(j), f) <= r(k, f)) exit
        order(j+1) = order(j); j = j - 1
      end do
      order(j+1) = k
    end do
    r(1:n, f) = r(order, f)
    w(1:n, f) = w(order, f)
    deallocate(order)
    nRays(f) = n
    ! radius enclosing each percentage (linear within a ray's energy share)
    do k = 1, nper
      target = per(k)/100.0d0
      acc = 0.0d0
      rads(k, f) = r(n, f)
      do i = 1, n
        if (acc + w(i, f) >= target - 1.0d-12) then
          if (i == 1) then
            rads(k, f) = r(1, f)*target/max(w(1, f), tiny(1.0d0))
          else
            rads(k, f) = r(i-1, f) + (r(i, f) - r(i-1, f))*(target - acc)/max(w(i, f), tiny(1.0d0))
          end if
          exit
        end if
        acc = acc + w(i, f)
      end do
    end do
  end do
  SPDTYPE = savedSpdType
  NRECT   = savedNRect

  ! Enclosed energy (%) on a common radius grid, for the plot and Data tab.
  rmax = 0.0d0
  do f = 1, nf
    if (nRays(f) > 0) rmax = max(rmax, r(nRays(f), f))
  end do
  if (rmax <= 0.0d0) then
    call zoa_emit("RAD: no rays reached the image", "red")
    return
  end if
  allocate(grid(NGRID), enc(NGRID, nf))
  grid = [((rmax*(i-1))/(NGRID-1), i = 1, NGRID)]
  enc = 0.0d0
  do f = 1, nf
    acc = 0.0d0
    j = 1
    do i = 1, NGRID
      do while (j <= nRays(f))
        if (r(j, f) > grid(i)) exit
        acc = acc + w(j, f)
        j = j + 1
      end do
      enc(i, f) = 100.0d0*min(acc, 1.0d0)
    end do
  end do

  ! Terminal: the percentage table (CODE V's RAD listing).
  call zoa_emit("GEOMETRIC RADIAL ENERGY (polychromatic, about the spot centroid; NRD "// &
  &  trim(int2str(nrd))//")", "black")
  call zoa_emit("Radius enclosing the energy, "//trim(sysConfig%getLensUnitsText()), "black")
  line = '  Energy %'
  do f = 1, nf
    write(cell, '(A,I0)') 'F', f
    line = trim(line)//repeat(' ', 12 - len_trim(cell))//trim(cell)
  end do
  call zoa_emit(trim(line), "black")
  do k = 1, nper
    write(line, '(F10.1)') per(k)
    do f = 1, nf
      write(cell, '(ES12.4)') rads(k, f)
      line = trim(line)//cell(1:12)
    end do
    call zoa_emit(trim(line), "black")
  end do

  ! Data tab: radius and enclosed energy (%) per field, as columns.
  if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  line = '    Radius['//trim(sysConfig%getLensUnitsText())//']'
  do f = 1, nf
    write(cell, '(A,I0,A)') 'F', f, '[%]'
    line = trim(line)//repeat(' ', 12 - len_trim(cell))//trim(cell)
  end do
  call OUTKDP(trim(line))
  do i = 1, NGRID
    write(line, '(ES14.6)') grid(i)
    do f = 1, nf
      write(cell, '(F12.4)') enc(i, f)
      line = trim(line)//cell(1:12)
    end do
    call OUTKDP(trim(line))
  end do
  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

  if (HEADLESS_MODE) then
    canvas = c_null_ptr
  else
    canvas = hl_gtk_drawing_area_new(size=[1200,800], has_alpha=FALSE)
  end if
  call mplt%initialize(canvas, 1, 1)
  call xyscat%initialize(c_null_ptr, real(grid), real(enc(:,1)), &
  & xlabel='Radius ['//trim(sysConfig%getLensUnitsText())//']'//c_null_char, &
  & ylabel='Enclosed Energy [%]'//c_null_char, &
  & title='Geometric Radial Energy'//c_null_char)
  call xyscat%setDataColorCode(colors(1))
  allocate(legend(nf))
  do f = 1, nf
    write(legend(f), '(A,I0)') 'F', f
    if (f > 1) then
      ! (setDataColorCode colours the most recently added series)
      call xyscat%addXYPlot(real(grid), real(enc(:,f)))
      call xyscat%setDataColorCode(colors(mod(f-1, size(colors)) + 1))
    end if
  end do
  call xyscat%addLegend(legend)
  call mplt%set(1, 1, xyscat)
  call finalizeGoPlot_new(mplt, psm, replot, objIdx)
end subroutine rad_go

! FMA (CODE V field map, full-field display): one metric on a grid of field
! points -- terminal summary (and table with LIS Y), a Data tab of
! X field, Y field, value (plus the real/ideal image points for DST), and a
! colour map.
subroutine fma_go(psm)
  USE GLOBALS
  use global_widgets, only: sysConfig, ioConfig
  use kdp_utils, only: OUTKDP
  use type_utils, only: int2str, real2str
  use zoa_output, only: zoa_emit
  use field_map, only: FMA_DST, fma_metric_name, fma_metric_unit, fma_evaluate
  use plplot, PI => PL_PI
  use plplot_extra
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_fortran_env, only: real64
  implicit none
  type(zoaplot_setting_manager) :: psm
  type(zoaPlotImg) :: zimg
  type(multiplot) :: mplt
  type(c_ptr) :: canvas
  logical :: replot, lis
  logical :: hasLim(4)
  integer :: objIdx, metric, nfx, nfy, nrd, i, j, k, nOk
  real(real64) :: lim(4), fmax, vmin, vmax, vrms, vmaxAbs, at(2)
  real(real64), allocatable :: xs(:), ys(:), val(:,:), img(:,:,:)
  logical, allocatable :: ok(:,:)
  real, allocatable :: px(:), py(:), pz(:)
  character(len=1024) :: line
  character(len=20) :: unit
  character(len=3) :: mname

  call initializeGoPlot(psm, ID_PLOTTYPE_FMA, "Field Map", replot, objIdx)
  call psm%getFMASettings(metric, nfx, nfy, nrd, lim, hasLim, lis)
  mname = fma_metric_name(metric)
  unit = fma_metric_unit(metric)

  ! Default range: +/- the largest reference field, both directions.
  fmax = max(abs(sysConfig%refFieldValue(1)), abs(sysConfig%refFieldValue(2)))
  if (.not. hasLim(1)) lim(1) = -fmax
  if (.not. hasLim(2)) lim(2) =  fmax
  if (.not. hasLim(3)) lim(3) = -fmax
  if (.not. hasLim(4)) lim(4) =  fmax
  if (fmax == 0.0_real64 .and. .not. all(hasLim)) then
    call zoa_emit("FMA: the lens has no field defined", "red"); return
  end if
  if (lim(2) <= lim(1) .or. lim(4) <= lim(3)) then
    call zoa_emit("FMA: XMA/YMA must be above XMI/YMI", "red"); return
  end if

  allocate(xs(nfx), ys(nfy), val(nfx,nfy), img(4,nfx,nfy), ok(nfx,nfy))
  xs = [(lim(1) + (lim(2) - lim(1))*(i-1)/real(nfx-1, real64), i = 1, nfx)]
  ys = [(lim(3) + (lim(4) - lim(3))*(j-1)/real(nfy-1, real64), j = 1, nfy)]
  call fma_evaluate(metric, nrd, xs, ys, val, ok, img)
  nOk = count(ok)
  if (nOk == 0) then
    call zoa_emit("FMA: no field point could be traced", "red"); return
  end if
  vmin = minval(val, mask=ok); vmax = maxval(val, mask=ok)
  vrms = sqrt(sum(val**2, mask=ok)/nOk)
  vmaxAbs = 0.0_real64; at = 0.0_real64
  do j = 1, nfy
    do i = 1, nfx
      if (ok(i,j) .and. abs(val(i,j)) >= vmaxAbs) then
        vmaxAbs = abs(val(i,j)); at = [xs(i), ys(j)]
      end if
    end do
  end do

  ! Terminal: summary (and the grid with LIS Y).
  line = "FIELD MAP ("//mname//", "//trim(unit)//"; "//trim(int2str(nfx))//" x "// &
  &  trim(int2str(nfy))//" field points"
  if (metric /= FMA_DST) line = trim(line)//"; NRD "//trim(int2str(nrd))
  call zoa_emit(trim(line)//")", "black")
  call zoa_emit("Field: "//trim(sysConfig%getFieldText()), "black")
  write(line, '(A,ES12.4,A,ES12.4,A,ES12.4)') 'Min ', vmin, '   Max ', vmax, '   RMS ', vrms
  call zoa_emit(trim(line), "black")
  write(line, '(A,ES12.4,A,F0.4,A,F0.4,A)') 'Largest |value| ', vmaxAbs, ' at X ', at(1), ', Y ', at(2), ''
  call zoa_emit(trim(line), "black")
  if (nOk < nfx*nfy) call zoa_emit(trim(int2str(nfx*nfy - nOk))//" field point(s) failed", "red")
  if (lis) then
    line = '   Y \ X'
    do i = 1, nfx
      write(line(len_trim(line)+1:), '(F10.4)') xs(i)
    end do
    call zoa_emit(trim(line), "black")
    do j = nfy, 1, -1
      write(line, '(F8.4)') ys(j)
      do i = 1, nfx
        if (ok(i,j)) then
          write(line(len_trim(line)+1:), '(ES10.2)') val(i,j)
        else
          line = trim(line)//'      FAIL'
        end if
      end do
      call zoa_emit(trim(line), "black")
    end do
  end if

  ! Data tab: one row per field point (failed points omitted).
  if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  line = '        XField        YField   '//mname//'['//trim(unit)//']'
  if (metric == FMA_DST) line = trim(line)//'        RealX         RealY        IdealX        IdealY'
  call OUTKDP(trim(line))
  do j = 1, nfy
    do i = 1, nfx
      if (.not. ok(i,j)) cycle
      write(line, '(3ES14.6)') xs(i), ys(j), val(i,j)
      if (metric == FMA_DST) write(line(len_trim(line)+1:), '(4ES14.6)') (img(k,i,j), k = 1, 4)
      call OUTKDP(trim(line))
    end do
  end do
  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

  ! Colour map (x fastest); failed points drawn at the minimum.
  allocate(px(nfx*nfy), py(nfx*nfy), pz(nfx*nfy))
  k = 0
  do j = 1, nfy
    do i = 1, nfx
      k = k + 1
      px(k) = real(xs(i)); py(k) = real(ys(j))
      pz(k) = real(merge(val(i,j), vmin, ok(i,j)))
    end do
  end do
  if (HEADLESS_MODE) then
    canvas = c_null_ptr
  else
    canvas = hl_gtk_drawing_area_new(size=[800,800], has_alpha=FALSE)
  end if
  call mplt%initialize(canvas, 1, 1)
  call zimg%init3d(c_null_ptr, px, py, pz, nfx, nfy, &
  & xlabel='X '//trim(sysConfig%getFieldText())//c_null_char, &
  & ylabel='Y '//trim(sysConfig%getFieldText())//c_null_char, &
  & title='Field Map: '//mname//' ['//trim(unit)//'], '//trim(real2str(vmin, 4))// &
  & ' to '//trim(real2str(vmax, 4))//c_null_char)
  call mplt%set(1, 1, zimg)
  call finalizeGoPlot_new(mplt, psm, replot, objIdx)
end subroutine fma_go

! FOO (CODE V footprint): each field's beam where it crosses the designated
! surfaces, in surface coordinates -- one panel per surface, one colour per
! field, the aperture outline with DAP Y.  Terminal: each beam's extent per
! surface (and every ray with LIS Y); Data tab: one row per ray and surface.
subroutine foo_go(psm)
  USE GLOBALS
  use global_widgets, only: sysConfig, ioConfig
  use kdp_utils, only: OUTKDP
  use type_utils, only: int2str, real2str
  use zoa_output, only: zoa_emit
  use footprint, only: foo_beam, foo_trace, foo_default_nrd, foo_lens_obscured, &
                       foo_resolve_surfaces, foo_aperture_outline
  use plplot, PI => PL_PI
  use plplot_extra
  use iso_c_binding, only: c_ptr, c_null_ptr
  use iso_fortran_env, only: real64
  implicit none
  type(zoaplot_setting_manager) :: psm
  type(zoaplot), allocatable :: panel(:)
  type(multiplot) :: mplt
  type(c_ptr) :: canvas
  type(foo_beam), allocatable :: beam(:)
  logical :: replot, boundary, dap, lis, first
  logical, allocatable :: fok(:)
  integer :: objIdx, bou, nrd, s0, s1, ns, nf, f, j, k, n, nrow, ncol, na, nser
  real(real64) :: rar, azi(3), ori(2), e, lo(2), hi(2), rbeam
  real(real64), allocatable :: ax(:), ay(:)
  real, allocatable :: px(:), py(:)
  integer, allocatable :: surfs(:)
  character(len=80) :: sur, alabel
  character(len=1024) :: line
  character(len=30) :: legend(12)
  character(len=20) :: units
  integer, parameter :: colors(6) = [PL_PLOT_BLUE, PL_PLOT_RED, PL_PLOT_GREEN, &
  &                                  PL_PLOT_BLACK, PL_PLOT_MAGENTA, PL_PLOT_CYAN]
  integer, parameter :: MAXSURF = 16

  call initializeGoPlot(psm, ID_PLOTTYPE_FOO, "Footprint", replot, objIdx)
  call psm%getFOOSettings(sur, bou, nrd, rar, azi, dap, ori, lis)
  if (.not. foo_resolve_surfaces(sur, s0, s1)) then
    call zoa_emit("FOO: bad surface "//trim(sur), "red"); return
  end if
  ns = s1 - s0 + 1
  if (ns > MAXSURF) then
    call zoa_emit("FOO: at most "//trim(int2str(MAXSURF))//" surfaces at once", "red"); return
  end if
  surfs = [(s0 + j - 1, j = 1, ns)]
  select case (bou)
  case (2);      boundary = .true.
  case (3);      boundary = .false.
  case default;  boundary = .not. foo_lens_obscured()
  end select
  if (nrd <= 0) nrd = foo_default_nrd()
  nf = sysConfig%numFields
  units = sysConfig%getLensUnitsText()

  allocate(beam(nf), fok(nf))
  do f = 1, nf
    call foo_trace(f, surfs, boundary, nrd, rar, azi, beam(f), fok(f))
  end do
  if (.not. any(fok)) then
    call zoa_emit("FOO: no ray reached the image", "red"); return
  end if

  ! Terminal: each beam's extent on each surface.
  if (boundary) then
    line = "FOOTPRINT (beam boundary, reference wavelength; RAR "//trim(real2str(rar, 4))//")"
  else
    line = "FOOTPRINT (ray grid, NRD "//trim(int2str(nrd))//", all weighted wavelengths)"
  end if
  call zoa_emit(trim(line), "black")
  call zoa_emit("Beam extent in surface coordinates, "//trim(units), "black")
  call zoa_emit("  Surf Field       X min       X max       Y min       Y max   Rays", "black")
  do j = 1, ns
    do f = 1, nf
      if (.not. fok(f)) then
        write(line, '(I6,A,I0,A)') surfs(j), '    F', f, '   no ray reached the image'
        call zoa_emit(trim(line), "red"); cycle
      end if
      n = beam(f)%n
      lo = minval(beam(f)%xy(:, 1:n, j), dim=2) - ori
      hi = maxval(beam(f)%xy(:, 1:n, j), dim=2) - ori
      write(line, '(I6,A,I0,4F12.5,I7)') surfs(j), '    F', f, lo(1), hi(1), lo(2), hi(2), n
      call zoa_emit(trim(line), "black")
    end do
  end do
  if (lis) then
    call zoa_emit("  Surf Field  W     Pupil X     Pupil Y           X           Y", "black")
    do j = 1, ns
      do f = 1, nf
        do k = 1, beam(f)%n
          write(line, '(I6,A,I0,I4,4F12.5)') surfs(j), '    F', f, beam(f)%iw(k), &
          &  beam(f)%pupil(:, k), beam(f)%xy(:, k, j) - ori
          call zoa_emit(trim(line), "black")
        end do
      end do
    end do
  end if

  ! Data tab: one row per ray and surface.
  if (.not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
  call OUTKDP('  Surf Field Wave      PupilX      PupilY       X['//trim(units)// &
  &  ']       Y['//trim(units)//']')
  do j = 1, ns
    do f = 1, nf
      do k = 1, beam(f)%n
        write(line, '(I6,I6,I5,2F12.6,2ES14.6)') surfs(j), f, beam(f)%iw(k), &
        &  beam(f)%pupil(:, k), beam(f)%xy(:, k, j) - ori
        call OUTKDP(trim(line))
      end do
    end do
  end do
  if (.not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

  ! One panel per surface, the same scale in X and Y.  The grid must be
  ! full (multiplot cannot draw an empty cell): the most nearly square
  ! rows x columns that is exactly ns.
  ncol = ceiling(sqrt(real(ns)) - 1.0e-6)
  do while (mod(ns, ncol) /= 0)
    ncol = ncol + 1
  end do
  nrow = ns/ncol
  if (HEADLESS_MODE) then
    canvas = c_null_ptr
  else
    canvas = hl_gtk_drawing_area_new(size=[1000,1000], has_alpha=FALSE)
  end if
  call mplt%initialize(canvas, nrow, ncol)
  allocate(panel(ns))
  do j = 1, ns
    e = 0.0_real64
    first = .true.
    nser = 0
    do f = 1, nf
      if (.not. fok(f)) cycle
      n = beam(f)%n
      if (boundary) then
        ! close the outline
        px = [real(beam(f)%xy(1, 1:n, j) - ori(1)), real(beam(f)%xy(1, 1, j) - ori(1))]
        py = [real(beam(f)%xy(2, 1:n, j) - ori(2)), real(beam(f)%xy(2, 1, j) - ori(2))]
      else
        px = real(beam(f)%xy(1, 1:n, j) - ori(1))
        py = real(beam(f)%xy(2, 1:n, j) - ori(2))
      end if
      e = max(e, real(maxval(abs(px)), real64), real(maxval(abs(py)), real64))
      if (first) then
        call panel(j)%initialize(c_null_ptr, px, py, &
        & xlabel='X ['//trim(units)//']'//c_null_char, &
        & ylabel='Y ['//trim(units)//']'//c_null_char, &
        & title='Footprint S'//trim(int2str(surfs(j)))//c_null_char)
        first = .false.
      else
        call panel(j)%addXYPlot(px, py)
      end if
      call panel(j)%setDataColorCode(colors(mod(f-1, size(colors)) + 1))
      if (.not. boundary) call panel(j)%setLineStyleCode(-1)
      nser = nser + 1
      if (nser <= size(legend)) write(legend(nser), '(A,I0)') 'F', f
    end do
    if (dap) then
      rbeam = 0.0_real64
      do f = 1, nf
        if (fok(f)) rbeam = max(rbeam, maxval(hypot(beam(f)%xy(1, 1:beam(f)%n, j), &
        &                                          beam(f)%xy(2, 1:beam(f)%n, j))))
      end do
      call foo_aperture_outline(surfs(j), rbeam, ax, ay, na, alabel)
      if (na > 1) then
        call panel(j)%addXYPlot(real(ax(1:na) - ori(1)), real(ay(1:na) - ori(2)))
        call panel(j)%setDataColorCode(PL_PLOT_BLACK)
        call panel(j)%setLineStyleCode(2)
        e = max(e, maxval(abs(ax(1:na) - ori(1))), maxval(abs(ay(1:na) - ori(2))))
        nser = nser + 1
        if (nser <= size(legend)) legend(nser) = alabel
      else
        call zoa_emit("FOO: S"//trim(int2str(surfs(j)))// &
        &  ": aperture outline not drawn (polygon aperture)", "black")
      end if
    end if
    if (e <= 0.0_real64) e = 1.0_real64
    call panel(j)%setIsotropic()
    call panel(j)%setXScale(real(1.08_real64*e, 8))
    call panel(j)%setYScale(real(1.08_real64*e, 8))
    if (j == 1) call panel(j)%addLegend(legend(1:min(nser, size(legend))))
    call mplt%set((j-1)/ncol + 1, mod(j-1, ncol) + 1, panel(j))
  end do
  call finalizeGoPlot_new(mplt, psm, replot, objIdx)
end subroutine foo_go

! FFT/image plot implementations live in plot-functions-fft.f90

! Rebuild a plot base command with exactly ONE trailing plot-number token.
! A replayed or restored command already carries its P<n>; blindly appending
! another produced "VIE P1 P1", which the next replot pass no longer recognized
! as an existing plot (execVIE checks for exactly two tokens) and so opened a
! duplicate tab.
! Use an in-place subroutine: ifx 2024.2 miscompiles the dependent-length
! character function result at these GUI call sites.
subroutine setPlotNum(baseCmd, plotNum)
    use strings, only: parse
    use type_utils, only: int2str
    implicit none
    character(len=*), intent(inout) :: baseCmd
    integer, intent(in) :: plotNum
    character(len=len(baseCmd)) :: cmd, parseCmd
    character(len=80) :: tokens(40)
    integer :: n, i
    ! parse modifies its first argument, even though it restores it on return.
    parseCmd = baseCmd
    call parse(parseCmd, ' ', tokens, n)
    cmd = ''
    do i = 1, n
      if (isPlotNumToken(trim(tokens(i)))) cycle
      cmd = trim(cmd)//' '//trim(tokens(i))
    end do
    cmd = trim(adjustl(cmd))//' P'//int2str(plotNum)
    baseCmd = cmd
end subroutine setPlotNum

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

! MULTIPLOT page tab, at MULTIPLOT: find the page's tab P<plotNum>, or open a
! new one (plotNum < 1: the next free number, returned), before the subplots
! run, so they can write to its Data tab.  A new tab holds no command until
! the page is finished (multiplot_page_end): a redraw in between must not
! run half a script.  objIdx -1 headless.
subroutine multiplot_page_begin(plotNum, objIdx, replot)
    use handlers, only: zoatabMgr
    use type_utils, only: int2str
    USE GLOBALS, only: HEADLESS_MODE
    implicit none
    integer, intent(inout) :: plotNum
    integer, intent(out) :: objIdx
    logical, intent(out) :: replot
    character(len=40) :: tabName

    objIdx = -1
    replot = .false.
    if (HEADLESS_MODE) return
    if (plotNum >= 1) replot = zoatabMgr%doesPlotExist_new(ID_PLOTTYPE_MULTIPLOT, objIdx, plotNum)
    if (replot) then
      call zoatabMgr%clearDataTab(objIdx)
      return
    end if
    if (plotNum < 1) plotNum = zoatabMgr%getNumberOfPlotsByCode(ID_PLOTTYPE_MULTIPLOT) + 1
    tabName = 'Multiplot'
    if (plotNum > 1) tabName = trim(tabName)//' '//int2str(plotNum)
    objIdx = zoatabMgr%addMultiPlotTab(ID_PLOTTYPE_MULTIPLOT, trim(tabName)//c_null_char)
    call zoatabMgr%updateInputCommand(objIdx, ' ')
end subroutine multiplot_page_begin

! MULTIPLOT page, at its closing GO: draw the page (its tab, or headless the
! PNG), and make the page's script the tab's command, which redraws it.
subroutine multiplot_page_end(page, objIdx, replot, plotNum, script)
    use handlers, only: zoatabMgr
    use kdp_utils, only: OUTKDP
    USE GLOBALS, only: HEADLESS_MODE
    implicit none
    type(multiplot) :: page
    integer, intent(in) :: objIdx, plotNum
    logical, intent(in) :: replot
    character(len=*), intent(in) :: script
    type(zoaplot_setting_manager) :: psm
    logical :: haveTab

    haveTab = pageTabExists(objIdx)
    ! The Data tab ends with the page's commands: run them (a macro) to
    ! draw the page again.
    if (haveTab) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    call OUTKDP('')
    ! one command per line (a macro; a single line overflows the output line)
    call OUTKDP('Page commands:')
    block
      integer :: a, b
      a = 1
      do while (a <= len_trim(script))
        b = index(script(a:), ';')
        if (b == 0) then
          b = len_trim(script) + 1
        else
          b = a + b - 1
        end if
        if (len_trim(script(a:b-1)) > 0) call OUTKDP(trim(adjustl(script(a:b-1))))
        a = b + 1
      end do
    end block
    if (haveTab) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)

    if (HEADLESS_MODE) then
      call page%draw()
      return
    end if
    ! (its tab was closed while the page was composed: a lens was loaded)
    if (.not. haveTab) return
    call zoatabMgr%updateInputCommand(objIdx, script)
    ! A new tab takes its drawing area from the plot (each analysis makes its
    ! own in its go routine); the page makes one at its size.  A redraw
    ! keeps the tab's.
    if (.not. replot) page%area = hl_gtk_drawing_area_new( &
    &   size=[page%width, page%height], has_alpha=FALSE)
    call zoatabMgr%updateGenericMultiPlotTab(objIdx, page)
    if (.not. replot) then
      ! no settings panel: the page is changed by editing its script
      call psm%initialize('MULTIPLOT')
      psm%plotNum = plotNum
      call zoaTabMgr%finalize_with_psm(objIdx, psm)
      call zoaTabMgr%finalizeNewPlotTab(objIdx)
      ! Full size, scrolling: fitted to the tab, a page of several plots
      ! has unreadably small text (PLTSIZE AUTO fits it again).
      block
        use zoa_tab, only: zoaplottab
        use iso_c_binding, only: c_int
        select type (t => zoatabMgr%tabInfo(objIdx)%tabObj)
        class is (zoaplottab)
          call t%setFixedSize(int(page%width, c_int), int(page%height, c_int))
        end select
      end block
    end if
end subroutine multiplot_page_end

! The MULTIPLOT page's tab objIdx is still open (GUI only).
logical function pageTabExists(objIdx)
    use handlers, only: zoatabMgr
    USE GLOBALS, only: HEADLESS_MODE
    implicit none
    integer, intent(in) :: objIdx
    pageTabExists = .false.
    if (HEADLESS_MODE .or. objIdx < 1) return
    if (objIdx > size(zoatabMgr%tabInfo)) return
    if (.not. allocated(zoatabMgr%tabInfo(objIdx)%tabObj)) return
    pageTabExists = zoatabMgr%tabInfo(objIdx)%tabObj%ID_PLOTTYPE == ID_PLOTTYPE_MULTIPLOT
end function pageTabExists

! An analysis starting while a MULTIPLOT page is composed: claim its region
! and head its block of the page's Data tab.  objIdx = the page's tab, so
! the analysis writes its data there (-1 headless: to the console).
subroutine composeSubplotBegin(psm, objIdx)
    use plot_composer, only: comp_claim_slot, comp_obj_idx, comp_set_page
    use kdp_utils, only: OUTKDP
    use zoa_output, only: zoa_emit
    use type_utils, only: int2str
    USE GLOBALS, only: HEADLESS_MODE
    implicit none
    type(zoaplot_setting_manager) :: psm
    integer, intent(out) :: objIdx
    integer :: r, c, rs, cs, plotNum
    logical :: replot
    character(len=1024) :: cmd

    objIdx = comp_obj_idx()
    if (.not. HEADLESS_MODE .and. .not. pageTabExists(objIdx)) then
      ! its tab was closed (a lens loaded mid-page): the page gets a new one,
      ! since the analysis writes its data to the tab
      plotNum = -1
      call multiplot_page_begin(plotNum, objIdx, replot)
      call comp_set_page(objIdx, plotNum, .false.)
    end if
    if (.not. comp_claim_slot(r, c, rs, cs)) then
      call zoa_emit('MULTIPLOT: the page is full; this plot is not added (use SUBPLOT r c)', 'red')
      return
    end if
    ! the analysis's command, without its GO
    cmd = psm%generatePlotCommand()
    if (len_trim(cmd) > 5) then
      if (cmd(len_trim(cmd)-4:len_trim(cmd)) == ' ; GO') cmd = cmd(1:len_trim(cmd)-5)
    end if
    if (objIdx > 0 .and. .not. HEADLESS_MODE) call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    call OUTKDP('')
    call OUTKDP('Subplot '//trim(int2str(r))//','//trim(int2str(c))//': '//trim(cmd))
    if (objIdx > 0 .and. .not. HEADLESS_MODE) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
end subroutine composeSubplotBegin

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

    ! A MULTIPLOT page is being composed: this plot fills a region of it (no
    ! tab of its own); its data goes to the page's Data tab under a heading.
    if (comp_active()) then
      call composeSubplotBegin(psm, objIdx)
      replot = .TRUE.
      return
    end if

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
      call setPlotNum(psm%baseCmd, psm%plotNum)
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

    ! A MULTIPLOT page is being composed: the plot goes to its region
    if (comp_active()) then
      call comp_store(mplt)
      call mplt%clear()
      return
    end if

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
      call setPlotNum(psm%baseCmd, psm%plotNum)
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
