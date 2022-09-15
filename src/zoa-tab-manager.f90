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
  class(zoatab), allocatable :: settings

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

function addPlotTab(self, PLOT_CODE, inputTitle, extcanvas) result(new_tab)
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
    type(zoatab) :: new_tab
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

        !plotObj = ray_fan_settings()
        !tabObj => rayfantab

        !newPlot => ray_fan_new(tabObj) ! not sure how to legally do this
        !call lens_draw_new_2(new_tab)

        !call lens_draw_new_2(self%notebook)

       PRINT *, "About to allocate type in new lens draw plot"
       allocate(lens_draw_settings :: self%tabInfo(self%tabNum)%settings )
       self%tabInfo(self%tabNum)%settings = lens_draw_settings()
       call self%tabInfo(self%tabNum)%settings%newPlot(self%notebook)
       self%tabInfo(self%tabNum)%canvas = self%tabInfo(self%tabNum)%settings%canvas
        call gtk_drawing_area_set_draw_func(self%tabInfo(self%tabNum)%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_LENSDRAW), c_null_funptr)

    case (ID_NEWPLOT_RAYFAN)
        if (.not.present(inputTitle)) THEN
          winTitle = "Ray Fan"
        else
          winTitle = inputTitle
        end if
        PRINT *, "RAY FAN NEW PLOT STARTING "

        PRINT *, "winTitle is ", winTitle

        !plotObj = ray_fan_settings()
        !tabObj => rayfantab
        call new_tab%initialize(self%notebook, trim(winTitle), ID_NEWPLOT_RAYFAN)
        !newPlot => ray_fan_new(tabObj) ! not sure how to legally do this
        call ray_fan_new(new_tab)
        call gtk_drawing_area_set_draw_func(new_tab%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_RAYFAN), c_null_funptr)

    case (-1) ! This means we just add to
        PRINT *, "Generic Plot being added"
        !PRINT *, "Notebook ptr is ", self%notebook
        call new_tab%initialize(self%notebook, winTitle, PLOT_CODE)


        if (present(extcanvas)) new_tab%canvas = extcanvas

    case (ID_PLOTTYPE_AST)
        winTitle = "Astig Field Curv Dist"
        !call astfcdist_tab%initialize() = zoatabMgr%addPlotTab(ID_PLOTTYPE_AST, "Astig FC Dist")
        call new_tab%initialize(self%notebook, trim(winTitle), PLOT_CODE)
        call ast_fc_dist_new(new_tab)


    end select

    !call plotObj%new_plot()
    !call tabObj%initialize(self%notebook, winTitle, ID_NEWPLOT_RAYFAN)
    !call newPlot()
    !self%tabInfo(self%tabNum)%plotObj = plotObj
    self%tabInfo(self%tabNum)%typeCode = PLOT_CODE
    !self%tabInfo(self%tabNum)%canvas = new_tab%canvas
    PRINT *, "DEBUG:  typeCode stored is ", self%tabInfo(self%tabNum)%typeCode

    ! Temp Code to test replotting
    ! if (PLOT_CODE.EQ.ID_NEWPLOT_LENSDRAW) then
    !    PRINT *, "Defining Lens Type Settings"
    !    allocate(lens_draw_settings :: self%tabInfo(self%tabNum)%settings )
    !  end if


end function

 subroutine newPlotIfNeeded(self, PLOT_CODE)

    class(zoatabManager) :: self
    integer, intent(in) :: PLOT_CODE
    logical :: plotFound
    integer :: i, tabPos
    type(zoatab) :: newtab

    PRINT *, "Searching for existing plot... with plot code ", PLOT_CODE
    plotFound = .FALSE.
    DO i = 1,self%tabNum
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
      newtab = self%addPlotTab(PLOT_CODE)
    else
      call gtk_widget_queue_draw(self%tabInfo(tabPos)%canvas)
    end if

 end subroutine

  subroutine rePlotIfNeeded(self)

     class(zoatabManager) :: self
     !integer, intent(in) :: PLOT_CODE
     logical :: plotFound
     integer :: i, tabPos
     type(zoatab) :: newtab

     !plotFound = .FALSE.
     DO i = 1,self%tabNum
       if(self%tabInfo(i)%typeCode == ID_NEWPLOT_LENSDRAW) THEN
           PRINT *, "Found lens draw plot at tab ", i
           call self%tabInfo(i)%settings%replot()

!          plotFound = .TRUE.
!          tabPos = i

        end if

     END DO


  end subroutine

end module
