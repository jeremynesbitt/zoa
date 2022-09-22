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
    PRINT *, "Notebook ptr is ", self%notebook


end subroutine

subroutine addPlotTab(self, PLOT_CODE, inputTitle, extcanvas)
  use zoa_ui
  use mod_plotrayfan
  use mod_plotopticalsystem
  use ui_ast_fc_dist
  use ROUTEMOD

    class(zoatabManager) :: self
    character(len=80), optional :: inputTitle
    character(len=80) :: winTitle
    type(c_ptr), optional :: extcanvas
    class(*), pointer :: tabObj
    integer, intent(in) :: PLOT_CODE
    !type(zoatab) :: new_tab
    class(zoatab), allocatable :: new_tab
    integer, target :: TARGET_NEWPLOT_RAYFAN   = ID_NEWPLOT_RAYFAN
    integer, target :: TARGET_NEWPLOT_LENSDRAW   = ID_NEWPLOT_LENSDRAW



    self%tabNum = self%tabNum+1

      if (.not.present(inputTitle)) THEN
        winTitle = "Generic Plot"
      else
        winTitle = inputTitle
      end if



    select case (PLOT_CODE)

    case (ID_NEWPLOT_LENSDRAW)
        if (.not.present(inputTitle)) THEN
          winTitle = "Lens Draw"
        else
          winTitle = inputTitle
        end if
        PRINT *, "Lens Draw NEW PLOT STARTING "

        PRINT *, "winTitle is ", winTitle

        allocate(lensdrawtab :: self%tabInfo(self%tabNum)%tabObj)
        call self%tabInfo(self%tabNum)%tabObj%initialize(self%notebook, trim(winTitle), ID_NEWPLOT_LENSDRAW)
        call self%tabInfo(self%tabNum)%tabObj%newPlot()
        call gtk_drawing_area_set_draw_func(self%tabInfo(self%tabNum)%tabObj%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_LENSDRAW), c_null_funptr)
        allocate(lens_draw_settings :: self%tabInfo(self%tabNum)%settings )
        self%tabInfo(self%tabNum)%settings = ld_settings

    case (ID_NEWPLOT_RAYFAN)
        if (.not.present(inputTitle)) THEN
          winTitle = "Ray Fan"
        else
          winTitle = inputTitle
        end if
        PRINT *, "RAY FAN NEW PLOT STARTING "

        PRINT *, "winTitle is ", winTitle

        allocate(rayfantab :: self%tabInfo(self%tabNum)%tabObj)
        call self%tabInfo(self%tabNum)%tabObj%initialize(self%notebook, trim(winTitle), ID_NEWPLOT_RAYFAN)
        call self%tabInfo(self%tabNum)%tabObj%newPlot()

        call gtk_drawing_area_set_draw_func(self%tabInfo(self%tabNum)%tabObj%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_RAYFAN), c_null_funptr)
      allocate(ray_fan_settings :: self%tabInfo(self%tabNum)%settings )

    case (-1) ! This means we just add to
        PRINT *, "Generic Plot being added"
        !PRINT *, "Notebook ptr is ", self%notebook
        call new_tab%initialize(self%notebook, winTitle, PLOT_CODE)


        if (present(extcanvas)) new_tab%canvas = extcanvas

    case (ID_PLOTTYPE_AST)
        winTitle = "Astig Field Curv Dist"
        allocate(astfcdist_tab :: self%tabInfo(self%tabNum)%tabObj)
        call self%tabInfo(self%tabNum)%tabObj%initialize(self%notebook, trim(winTitle), ID_PLOTTYPE_AST)
        call self%tabInfo(self%tabNum)%tabObj%newPlot()
        allocate(ast_fc_dist_settings :: self%tabInfo(self%tabNum)%settings )
        self%tabInfo(self%tabNum)%settings = ast_settings
        !self%tabInfo(self%tabNum)%settings%canvas = self%tabInfo(self%tabNum)%tabObj%canvas



    end select

    !call plotObj%new_plot()
    !call tabObj%initialize(self%notebook, winTitle, ID_NEWPLOT_RAYFAN)
    !call newPlot()
    !self%tabInfo(self%tabNum)%plotObj = plotObj
    PRINT *, "DEBUG:  PLOT_CODE is ", PLOT_CODE
    self%tabInfo(self%tabNum)%typeCode = PLOT_CODE
    !self%tabInfo(self%tabNum)%canvas = new_tab%canvas
    self%tabInfo(self%tabNum)%canvas = self%tabInfo(self%tabNum)%tabObj%canvas
    PRINT *, "DEBUG:  typeCode stored is ", self%tabInfo(self%tabNum)%typeCode


end subroutine

 subroutine newPlotIfNeeded(self, PLOT_CODE)

    class(zoatabManager) :: self
    integer, intent(in) :: PLOT_CODE
    logical :: plotFound
    integer :: i, tabPos
    type(zoatab) :: newtab

    PRINT *, "Searching for existing plot... with plot code ", PLOT_CODE
    plotFound = .FALSE.
    DO i = 1,self%tabNum
       PRINT *, "i = ",i, " typeCODE = ", self%tabInfo(i)%typeCode
      if(self%tabInfo(i)%typeCode == PLOT_CODE) THEN
          PRINT *, "Found existing plot at tab ", i
          PRINT *, "Type code is ", self%tabInfo(i)%typeCode
          PRINT *, "PLOT_CODE is ", PLOT_CODE
         plotFound = .TRUE.
         tabPos = i

       end if

    END DO
    PRINT *, "After search, plotFound is ", plotFound
    if (.not.plotFound) THEN
      PRINT *, "New plot needed! for PLOT_CODE ", PLOT_CODE
      call self%addPlotTab(PLOT_CODE)
    else
      if (PLOT_CODE.EQ.ID_PLOTTYPE_AST) then
        !call self%tabInfo(tabPos)%settings%replot()
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
           call self%tabInfo(i)%settings%replot()
     END DO


  end subroutine

end module
