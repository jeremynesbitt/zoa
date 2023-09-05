module zoa_tab_manager
  use zoa_ui
  use zoa_tab
  use gtk
  use gtk_hl_container
  use collections


type zoatabData
  integer :: typeCode
  !class(*), pointer :: plotObj
  type(c_ptr) :: canvas
  class(ui_settings), allocatable :: settings
  class(zoatab), allocatable :: tabObj

end type

type  zoatabManager

  type(zoatabData), dimension(99) :: tabInfo
  type(c_ptr) :: buffer
  type(c_ptr) :: textView
  integer :: tabNum = 0
  type(c_ptr) :: notebook

 contains

   procedure :: addMsgTab
   procedure :: addPlotTab
   procedure :: newPlotIfNeeded
   procedure :: rePlotIfNeeded
   procedure :: removePlotTab
   procedure :: doesPlotExist
   procedure :: addPlotTabFromObj
   procedure :: addGenericPlotTab
   procedure :: updateGenericPlotTab
   procedure :: finalizeNewPlotTab
   procedure :: updateInputCommand
   procedure :: findTabIndex

 end type



contains

! This doubles as an init routine
subroutine addMsgTab(self, notebook, winTitle)

    class(zoatabManager) :: self
    type(c_ptr) :: notebook, buffer, scrollWin, label
    character(len=*) :: winTitle
    integer(c_int) :: tabPos

    self%notebook = notebook

    !self%textView = gtk_text_view_new ()
    !call gtk_text_view_set_editable(self%textView, FALSE)

    !self%buffer = gtk_text_view_get_buffer (self%textView)
    !call gtk_text_buffer_set_text (self%buffer, &
    !    & "ZOA Log Message Window"//C_NEW_LINE//c_null_char,&
    !    & -1_c_int)
    !scrollWin = gtk_scrolled_window_new()
    !label = gtk_label_new(winTitle//c_null_char)

    !call gtk_scrolled_window_set_child(scrollWin, self%textView)
    !tabPos = gtk_notebook_append_page (self%notebook, scrollWin, label)
    !PRINT *, "Notebook ptr is ", self%notebook


end subroutine

function  findTabIndex(self) result(newTabIndex)
  class(zoatabManager) :: self
  integer :: i
  integer :: newTabIndex

  do i=1,self%tabNum
    if (.not.ALLOCATED(self%tabInfo(i)%tabObj)) then
        !PRINT *, "Found empty slot at ", i
        newTabIndex = i
        return
      end if
  end do

  ! If we get here then add at the end
  !PRINT *, "Increment max number of tabs"
  self%tabNum = self%tabNum + 1
  newTabIndex = self%tabNum

end function

subroutine addPlotTab(self, PLOT_CODE, inputTitle, extcanvas)
  use zoa_ui
  use mod_plotrayfan
  use mod_plotopticalsystem
  use ui_rmsfield
  use ui_ast_fc_dist
  use ui_spot
  use ROUTEMOD
  use GLOBALS
  implicit none

    class(zoatabManager) :: self
    character(len=80), optional :: inputTitle
    character(len=80) :: winTitle
    character(len=3) :: outChar
    type(c_ptr), optional :: extcanvas
    class(*), pointer :: tabObj
    integer, intent(in) :: PLOT_CODE
    type(c_ptr) :: currPage
    integer(kind=c_int) :: currPageIndex
    !type(zoatab) :: new_tab
    class(zoatab), allocatable :: new_tab
    integer, target :: TARGET_NEWPLOT_RAYFAN   = ID_NEWPLOT_RAYFAN
    integer, target :: TARGET_NEWPLOT_LENSDRAW   = ID_NEWPLOT_LENSDRAW
    integer, target :: TARGET_PLOTTYPE_RMSFIELD   = ID_PLOTTYPE_RMSFIELD
    integer :: idx


    call logger%logText('Adding Tab (addPlotTab Sub)')

    idx = self%findTabIndex()


    !self%tabNum = self%tabNum+1

      if (.not.present(inputTitle)) THEN
        winTitle = "Generic Plot"
      else
        winTitle = inputTitle
      end if

    if (allocated(self%tabInfo(idx)%tabObj)) THEN
      call logger%logText("tabObj already allocated?" )

    end if

    select case (PLOT_CODE)

    case (ID_NEWPLOT_LENSDRAW)
        call logger%logText('Lens Draw New Plot Starting')
        if (.not.present(inputTitle)) THEN
          winTitle = "Lens Draw"
        else
          winTitle = inputTitle
        end if

        !PRINT *, "winTitle is ", winTitle

        allocate(lensdrawtab :: self%tabInfo(idx)%tabObj)
        call self%tabInfo(idx)%tabObj%initialize(self%notebook, trim(winTitle), ID_NEWPLOT_LENSDRAW)
        call self%tabInfo(idx)%tabObj%newPlot()
        call gtk_drawing_area_set_draw_func(self%tabInfo(idx)%tabObj%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_LENSDRAW), c_null_funptr)
        allocate(lens_draw_settings :: self%tabInfo(idx)%settings )
        self%tabInfo(idx)%settings = ld_settings


    case (ID_NEWPLOT_RAYFAN)
      call logger%logText('Ray Fan New Plot Starting')
        if (.not.present(inputTitle)) THEN
          winTitle = "Ray Fan"
        else
          winTitle = inputTitle
        end if

        PRINT *, "winTitle is ", winTitle

        allocate(rayfantab :: self%tabInfo(idx)%tabObj)
        call self%tabInfo(idx)%tabObj%initialize(self%notebook, trim(winTitle), ID_NEWPLOT_RAYFAN)
        call self%tabInfo(idx)%tabObj%newPlot()

        call gtk_drawing_area_set_draw_func(self%tabInfo(idx)%tabObj%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_RAYFAN), c_null_funptr)
      allocate(ray_fan_settings :: self%tabInfo(idx)%settings )
      self%tabInfo(idx)%settings = rf_settings


    case (-1) ! This means we just add to
        PRINT *, "Generic Plot being added"
        !PRINT *, "Notebook ptr is ", self%notebook
        call new_tab%initialize(self%notebook, winTitle, PLOT_CODE)


        if (present(extcanvas)) new_tab%canvas = extcanvas

    case (ID_PLOTTYPE_AST)
        call logger%logText('Astig FC Dist Tab being added')
        winTitle = "Astig Field Curv Dist"
        allocate(astfcdist_tab :: self%tabInfo(idx)%tabObj)
        call self%tabInfo(idx)%tabObj%initialize(self%notebook, trim(winTitle), ID_PLOTTYPE_AST)
        call self%tabInfo(idx)%tabObj%newPlot()
        allocate(ast_fc_dist_settings :: self%tabInfo(idx)%settings )
        self%tabInfo(idx)%settings = ast_settings
        !self%tabInfo(idx)%settings%canvas = self%tabInfo(idx)%tabObj%canvas

    case (ID_PLOTTYPE_SPOT)
        call logger%logText('Spot Diagram Starting')
        winTitle = "Spot Diagram"
        allocate(spot_tab :: self%tabInfo(idx)%tabObj)
        call self%tabInfo(idx)%tabObj%initialize(self%notebook, trim(winTitle), ID_PLOTTYPE_SPOT)
        call self%tabInfo(idx)%tabObj%newPlot()
        allocate(spot_settings :: self%tabInfo(idx)%settings )
        self%tabInfo(idx)%settings = spot_struct_settings
        !self%tabInfo(idx)%settings%canvas = self%tabInfo(idx)%tabObj%canvas

    case (ID_PLOTTYPE_RMSFIELD)
        call logger%logText('New RMS Field Diagram Starting')
        winTitle = "RMS vs Field"
        allocate(zoatab :: self%tabInfo(idx)%tabObj)
        self%tabInfo(idx)%tabObj%newGenericSinglePlot => genPlot_sandbox
        !self%tabInfo%(idx)%tabObj%newPlot()
        call self%tabInfo(idx)%tabObj%initialize(self%notebook, trim(winTitle), ID_PLOTTYPE_RMSFIELD)
        !call self%tabInfo(idx)%tabObj%newGenericSinglePlot()
        call self%tabInfo(idx)%tabObj%newGenericSinglePlot()
        allocate(rmsfield_settings :: self%tabInfo(idx)%settings )
        self%tabInfo(idx)%settings = rmsfield_struct_settings


    ! case (ID_PLOTTYPE_RMSFIELD)
    !     call logger%logText('New RMS Field Diagram Starting')
    !     winTitle = "RMS vs Field"
    !     allocate(rmsfieldtab :: self%tabInfo(idx)%tabObj)
    !     call self%tabInfo(idx)%tabObj%initialize(self%notebook, trim(winTitle), ID_PLOTTYPE_RMSFIELD)
    !     call self%tabInfo(idx)%tabObj%newPlot()
    !     allocate(rmsfield_settings :: self%tabInfo(idx)%settings )
    !     self%tabInfo(idx)%settings = rmsfield_struct_settings

    end select

    !call plotObj%new_plot()
    !call tabObj%initialize(self%notebook, winTitle, ID_NEWPLOT_RAYFAN)
    !call newPlot()
    !self%tabInfo(idx)%plotObj = plotObj
    !PRINT *, "DEBUG:  PLOT_CODE is ", PLOT_CODE
    self%tabInfo(idx)%typeCode = PLOT_CODE
    !self%tabInfo(idx)%canvas = new_tab%canvas
    self%tabInfo(idx)%canvas = self%tabInfo(idx)%tabObj%canvas
    !PRINT *, "DEBUG:  typeCode stored is ", self%tabInfo(idx)%typeCode
    call logger%logText('Tab Info finished populating in addPlotTab ')

    currPageIndex = gtk_notebook_get_current_page(self%notebook)
    currPage = gtk_notebook_get_nth_page(self%notebook, currPageIndex)
    WRITE(outChar, '(I0.3)') idx
    call gtk_widget_set_name(currPage, outChar)

end subroutine

subroutine finalizeNewPlotTab(self, idx)
    class(zoatabManager) :: self
    integer :: idx

    type(c_ptr) :: currPage
    integer(kind=c_int) :: currPageIndex
    character(len=3) :: outChar

    call self%tabInfo(idx)%tabObj%finalizeWindow()

    ! This part is to enable close tab functionality
    currPageIndex = gtk_notebook_get_current_page(self%notebook)
    currPage = gtk_notebook_get_nth_page(self%notebook, currPageIndex)
    WRITE(outChar, '(I0.3)') idx
    call gtk_widget_set_name(currPage, outChar//c_null_char)


end subroutine

function addGenericPlotTab(self, PLOT_CODE, tabTitle, x, y, xlabel, ylabel, title, linetypecode) result(idx)
  class(zoatabManager) :: self
  integer :: PLOT_CODE
  real :: x(:), y(:)
  character(len=*) :: tabTitle, xlabel, ylabel, title
  integer :: linetypecode
  integer :: idx


   idx = self%findTabIndex()
    !PRINT *, "idx is ", idx
    call logger%logText('New Generic Tab Starting')

    allocate(zoatab :: self%tabInfo(idx)%tabObj)
    call self%tabInfo(idx)%tabObj%initialize(self%notebook, tabTitle, PLOT_CODE)
    self%tabInfo(idx)%tabObj%cmdBasedPlot = .TRUE.
    call self%tabInfo(idx)%tabObj%createGenericSinglePlot(x,y,xlabel,ylabel,title, linetypecode)
    allocate(ui_settings :: self%tabInfo(idx)%settings )
    ! Right now there is no settings object.  This object is only
    ! used for replot.  Need a better solution for this
    !self%tabInfo(idx)%settings = tabObj%settings

    self%tabInfo(idx)%typeCode = PLOT_CODE
    self%tabInfo(idx)%canvas = self%tabInfo(idx)%tabObj%canvas
    !call self%tabInfo(idx)%tabObj%finalizeWindow()



end function

subroutine updateGenericPlotTab(self, objIdx, x, y)
  class(zoatabManager) :: self
  real :: x(:), y(:)
  integer :: objIdx

  call self%tabInfo(objIdx)%tabObj%updateGenericSinglePlot(x,y)

end subroutine


subroutine addPlotTabFromObj(self, tabObj)
   implicit none
   class(zoatabManager) :: self
   class(zoatab) :: tabObj
   integer :: PLOT_CODE
   integer :: idx
    type(c_ptr) :: currPage
    integer(kind=c_int) :: currPageIndex
    character(len=3) :: outChar

   PLOT_CODE = tabObj%ID_PLOTTYPE
   idx = self%findTabIndex()
    !PRINT *, "idx is ", idx
    call logger%logText('New RMS Field Diagram Starting')
    !PRINT *, "Allocated tabObj before allocation ", allocated(self%tabInfo(idx)%tabObj)
    !allocate(zoatab :: self%tabInfo(idx)%tabObj)

    self%tabInfo(idx)%tabObj = tabObj
    !PRINT *, "Allocated tabObj after allocation ", allocated(self%tabInfo(idx)%tabObj)

    allocate(ui_settings :: self%tabInfo(idx)%settings )
    ! Right now there is no settings object.  This object is only
    ! used for replot.  Need a better solution for this
    !self%tabInfo(idx)%settings = tabObj%settings

    self%tabInfo(idx)%typeCode = PLOT_CODE
    self%tabInfo(idx)%canvas = self%tabInfo(idx)%tabObj%canvas
    call self%tabInfo(idx)%tabObj%finalizeWindow()

    ! This part is to enable close tab functionality
    currPageIndex = gtk_notebook_get_current_page(self%notebook)
    currPage = gtk_notebook_get_nth_page(self%notebook, currPageIndex)
    WRITE(outChar, '(I0.3)') idx
    call gtk_widget_set_name(currPage, outChar//c_null_char)

end subroutine

 function doesPlotExist(self, PLOT_CODE, idxObj) result(plotFound)
   class(zoatabManager) :: self
   integer, intent(in) :: PLOT_CODE
   logical :: plotFound
   integer, intent(inout) :: idxObj
   integer :: i

    !PRINT *, "Searching for existing plot... with plot code ", PLOT_CODE
    plotFound = .FALSE.
    idxObj = -1
    DO i = 1,self%tabNum
       !PRINT *, "i = ",i, " typeCODE = ", self%tabInfo(i)%typeCode
      if(self%tabInfo(i)%typeCode == PLOT_CODE) THEN
          !PRINT *, "Found existing plot at tab ", i
          idxObj = i
          !PRINT *, "Type code is ", self%tabInfo(i)%typeCode
          !PRINT *, "PLOT_CODE is ", PLOT_CODE
         plotFound = .TRUE.
         tabPos = i

       end if

    END DO
    !PRINT *, "After search, plotFound is ", plotFound

 end function

 subroutine newPlotIfNeeded(self, PLOT_CODE)

    class(zoatabManager) :: self
    integer, intent(in) :: PLOT_CODE
    logical :: plotFound
    integer :: i, tabPos
    type(zoatab) :: newtab

    plotFound = self%doesPlotExist(PLOT_CODE, tabPos)
    if (.not.plotFound) THEN
      !PRINT *, "New plot needed! for PLOT_CODE ", PLOT_CODE
      call self%addPlotTab(PLOT_CODE)
    else
      !if (PLOT_CODE.EQ.ID_PLOTTYPE_AST.OR.PLOT_CODE.EQ.ID_PLOTTYPE_SPOT ) then
      if (PLOT_CODE.EQ.ID_PLOTTYPE_SPOT.OR.PLOT_CODE.EQ.ID_PLOTTYPE_RMSFIELD) then
        call self%tabInfo(tabPos)%settings%replot()
      else
        call gtk_widget_queue_draw(self%tabInfo(tabPos)%canvas)
      end if

      !
    end if

 end subroutine

  subroutine rePlotIfNeeded(self)

     class(zoatabManager) :: self
     integer :: i

     DO i = 1,self%tabNum
           if (self%tabInfo(i)%tabObj%cmdBasedPlot) then
              !PRINT *, "CMD Based REPLOT REQUESTED"
              call PROCESKDP(self%tabInfo(i)%tabObj%plotCommand)
          else
            call self%tabInfo(i)%settings%replot()
          end if
     END DO


  end subroutine

  subroutine removePlotTab(self, tabIndex, tabInfoIndex)

    class(zoatabManager) :: self
    integer, intent(in) :: tabIndex, tabInfoIndex

    call gtk_notebook_remove_page(self%notebook, tabIndex)
    !PRINT *, "typeCode is ", self%tabInfo(tabInfoIndex)%typeCode
    !PRINT *, "About to deallocate tabObj for index ", tabInfoIndex
    !PRINT *, "allocated test ", allocated(self%tabInfo(tabInfoIndex)%tabObj)
    if (allocated(self%tabInfo(tabInfoIndex)%tabObj)) then
       DEALLOCATE(self%tabInfo(tabInfoIndex)%tabObj)
    end if
    self%tabInfo(tabInfoIndex)%typeCode = -1
    !PRINT *, "About to deallocate ui settings obj"
    if (allocated(self%tabInfo(tabInfoIndex)%settings)) then
       DEALLOCATE(self%tabInfo(tabInfoIndex)%settings)
    end if
    !PRINT *, "Set canvas to NULL"
    self%tabInfo(tabInfoIndex)%canvas = c_null_ptr



  end subroutine

  subroutine updateInputCommand(self, objIdx, inputCmd)
    use global_widgets, only: uiSettingCommands, uiSetCmdsIdx
    implicit none

    class(zoatabManager) :: self
    integer, intent(in) :: objIdx
    character(len=*) :: inputCmd
    character(len=150) :: cmdOnly

    integer :: i, cmdLoc, tokLoc

    self%tabInfo(objIdx)%tabObj%plotCommand = inputCmd

     cmdLoc = index(inputCmd, ",")
     if (cmdLoc.GT.0) cmdOnly = inputCmd(1:cmdLoc-1)

    do i=1,uiSetCmdsIdx
      cmdLoc = index(uiSettingCommands(i), trim(cmdOnly))
      if (cmdLoc.GT.0) then
         !PRINT *, "Update command ", uiSettingCommands(i)! //" to "//inputCmd
         tokLoc = index(uiSettingCommands(i), "--")

         uiSettingCommands(i) = uiSettingCommands(i)(1:tokLoc+1)//trim(inputCmd)
         !PRINT *, "New Command is ", uiSettingCommands(i)
         return
       end if
    end do


  end subroutine

end module
