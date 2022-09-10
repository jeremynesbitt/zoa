module zoa_tab_manager
  use zoa_tab
  use gtk
  use gtk_hl_container
  use collections


type zoatabData
  integer :: typeCode
  !class(*), pointer :: plotObj
  type(c_ptr) :: canvas

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
   procedure :: updateMsgBuffer
   procedure :: newPlotIfNeeded

 end type

contains

! This doubles as an init routine
subroutine addMsgTab(self, notebook, winTitle)

    class(zoatabManager) :: self
    type(c_ptr) :: notebook, buffer, scrollWin, label
    character(len=*) :: winTitle
    integer(c_int) :: tabPos

    self%notebook = notebook

    self%textView = gtk_text_view_new ()
    call gtk_text_view_set_editable(self%textView, FALSE)

    self%buffer = gtk_text_view_get_buffer (self%textView)
    call gtk_text_buffer_set_text (self%buffer, &
        & "ZOA Log Message Window"//C_NEW_LINE//c_null_char,&
        & -1_c_int)
    scrollWin = gtk_scrolled_window_new()
    label = gtk_label_new(winTitle//c_null_char)

    call gtk_scrolled_window_set_child(scrollWin, self%textView)
    tabPos = gtk_notebook_append_page (self%notebook, scrollWin, label)


end subroutine

  subroutine updateMsgBuffer(self, ftext, txtColor)
      USE GLOBALS

      IMPLICIT NONE

      ! This routine is to update the terminal log, and is
      ! abstracted in case the method (font color, bold) needs to be changed

      ! The way implemented is to use the pango / markup interface
      ! See for some examples
      ! https://basic-converter.proboards.com/thread/314/pango-markup-text-examples

      class(zoatabManager) :: self
      character(len=*), intent(in) :: ftext
      character(len=*), intent(in)  :: txtColor

      type(gtktextiter), target :: iter, startIter, endIter
      logical :: scrollResult
      type(c_ptr) ::  buffInsert

      !PRINT *, "TERMINAL LOG COLOR ARGUMENT IS ", txtColor

      call gtk_text_buffer_get_end_iter(self%buffer, c_loc(endIter))

      !PRINT *, "ABOUT TO CALL MARKUP "
      !TODO Sometimes an empty ftext is sent to this function and GTK throws a
      !warnting.  Need to figure out how to detect empty string (this is not working)
    if (ftext.ne."  ") THEN
      call gtk_text_buffer_insert_markup(self%buffer, c_loc(endIter), &
      & "<span foreground='"//trim(txtColor)//"'>"//ftext//"</span>"//C_NEW_LINE &
      & //c_null_char, -1_c_int)
    END IF

      !PRINT *, "MARKUP TEXT IS ", "<span foreground='"//trim(txtColor)//"'>"//trim(ftext)//"</span>"//C_NEW_LINE &
      !& //c_null_char
      !PRINT *, "LEN of ftext is ", len(trim(ftext))

      !call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))

      !scrollResult = gtk_text_view_scroll_to_iter(textView, c_loc(endIter), 0.5_c_double, &
      !&  True, 0.0_c_double, 1.0_c_double)
      ! Make sure we are at the bottom of the scroll window


      ! Way that wasn't working, originally in name_enter
      ! Make sure we are at the bottom of the scroll window
      buffInsert = gtk_text_buffer_get_insert(self%buffer)
     !gBool = g_variant_new_boolean(True)
      call gtk_text_view_scroll_to_mark(self%textView, buffInsert, 0.0_c_double, &
      &  True, 0.0_c_double, 1.0_c_double)

      !PRINT *, trim(txtColor)
      !PRINT *, "blue"

      !call gtk_text_buffer_insert_markup(buffer, c_loc(endIter), &
      !& "<span foreground='blue'>"//">> "//ftext//"</span>"//C_NEW_LINE &
      !& //c_null_char, -1_c_int)

      ! create a new property tag and change the color
      !tag = gtk_text_tag_new("blue_fg")
      !call g_value_set_interned_string(gColor, "blue"//c_null_char)
      !call g_object_set_property(tag, "foreground"//c_null_char, gColor)
      !call gtk_text_buffer_get_start_iter(buffer, c_loc(startIter))

      !Insert text
      !call gtk_text_buffer_insert_at_cursor(buffer, &
      ! & ">> "//ftext//C_NEW_LINE//c_null_char, -1_c_int)

      ! Get gtkIter and update text with nbew property tag
      ! call gtk_text_buffer_get_end_iter(buffer, c_loc(endIter))
      !call gtk_text_buffer_apply_tag(buffer, tag, c_loc(startIter), c_loc(endIter))

      !When I ran this, nothing changed.  Suspect there is some issue
      !with start and end Iter, but did not spend enough time debugging

  end

subroutine addPlotTab(self, PLOT_CODE, extcanvas)
  use zoa_ui
  use mod_plotrayfan
  use ROUTEMOD

    class(zoatabManager) :: self
    character(len=80) :: winTitle
    type(c_ptr), optional :: extcanvas
    class(*), pointer :: tabObj
    integer :: PLOT_CODE
    type(zoatab) :: rf_tab
    integer, target :: TARGET_NEWPLOT_RAYFAN   = ID_NEWPLOT_RAYFAN


    
    self%tabNum = self%tabNum+1

    select case (PLOT_CODE)

    case (ID_NEWPLOT_RAYFAN)
        winTitle = "Ray Fan"

        !plotObj = ray_fan_settings()
        !tabObj => rayfantab
        call rf_tab%initialize(self%notebook, winTitle, ID_NEWPLOT_RAYFAN)
        !newPlot => ray_fan_new(tabObj) ! not sure how to legally do this
        call ray_fan_new(rf_tab)
        call gtk_drawing_area_set_draw_func(rf_tab%canvas, &
                    & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT_RAYFAN), c_null_funptr)




    end select

    !call plotObj%new_plot()
    !call tabObj%initialize(self%notebook, winTitle, ID_NEWPLOT_RAYFAN)
    !call newPlot()
    !self%tabInfo(self%tabNum)%plotObj = plotObj
    self%tabInfo(self%tabNum)%typeCode = PLOT_CODE
    self%tabInfo(self%tabNum)%canvas = rf_tab%canvas
    !self%tabInfo(self%tabNum)%typeCode

end subroutine

 subroutine newPlotIfNeeded(self, PLOT_CODE)

    class(zoatabManager) :: self
    integer, intent(in) :: PLOT_CODE
    logical :: plotFound = .FALSE.
    integer :: i, tabPos


    DO i = 1,self%tabNum
      if(self%tabInfo(i)%typeCode == PLOT_CODE) THEN
         plotFound = .TRUE.
         tabPos = i

       end if

    END DO

    if (.not.plotFound) THEN
      PRINT *, "New plot needed!"
      call self%addPlotTab(PLOT_CODE)
    else
      call gtk_widget_queue_draw(self%tabInfo(tabPos)%canvas)
    end if

 end subroutine

end module
