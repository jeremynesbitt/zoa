module zoa_tab
  use gtk
  use global_widgets
  use iso_c_binding
  use collections

! pseudocode for zoatab

!Example 1 - PowSymPlot
! obj = new zoatabplot (child of zoatab?)
! obj.addPlots(2,1) % Multiplot support
! obj.Plot(1,z,y) % mimic PLPLOT here
! obj.addSetting(WAVELENGTH) % common WL setting
! obj.replotviacommand('POWSYM') % tell zoa how to replot in case data change
! eg lens edit

!Example 2 - Ray Fan plot
! obj = new zoatabplot
! obj. addKDPPLot (direct from NEUTARRAY)
! obj.addCustomSetting(fanarray, selections) % need to think  about this some more
! obj.replotviafunction(funcname) ! this already exists
type zoatab
     type(c_ptr) :: canvas, box1, tab_label
     integer(c_int)  :: width = 1000
     integer(c_int)  ::  height = 700
     integer :: numSettings = 0
     type(list) :: labels
     type(list) :: widgets
     integer(kind=c_int) :: ID_PLOTTYPE
     integer(kind=c_int), pointer :: DEBUG_PLOTTYPE


 contains
   procedure, public, pass(self) :: initialize
   !procedure, private, pass(self) :: createCairoDrawingArea
   procedure, public, pass(self) :: addListBoxSetting
   procedure, private, pass(self) :: buildSettings
   procedure, private, pass(self) :: settingobj_get
   procedure, public, pass(self) :: finalizeWindow
   procedure, private, pass(self) :: addLabelandWidget
   procedure, public, pass(self) :: addSpinBoxSetting




end type
contains

 subroutine initialize(self, parent_window, tabTitle, ID_PLOTTYPE)

    use ROUTEMOD
    implicit none

    class(zoatab) :: self

    type(c_ptr) :: parent_window ! currently unused
    integer(kind=c_int) :: ID_PLOTTYPE
    character(len=*) :: tabTitle
    type(c_ptr) :: tab_label, scrolled_tab
    integer :: i


    self%tab_label = gtk_label_new(tabTitle//c_null_char)
    self%ID_PLOTTYPE = ID_PLOTTYPE

    self%box1 = gtk_box_new (GTK_ORIENTATION_VERTICAL, 10_c_int);

    !call self%createCairoDrawingArea()
    call createCairoDrawingAreaForDraw(self%canvas, self%width, self%height, ID_PLOTTYPE)
    ! Initialize list of settings.  At present, support no more than 16 settings
    do i = 1, 16
        call self%labels%push(i)
        call self%widgets%push(i)
    end do


 end subroutine

 subroutine addSpinBoxSetting(self, labelText, spinButton, callbackFunc, callbackData)
   implicit none

   class(zoatab) :: self
   character(len=*), intent(in) :: labelText
   type(c_funptr), intent(in)   :: callbackFunc
   type(c_ptr), optional, intent(in)   :: callbackData
   type(c_ptr), intent(in) :: spinButton

  ! Local only
   type(c_ptr) :: newlabel

   ! Create Objects
   newlabel = gtk_label_new(labelText//c_null_char)
   call g_signal_connect (spinButton, "changed"//c_null_char, callbackFunc, callbackData)

   ! Update settings lists
   call self%addLabelandWidget(newlabel, spinButton)




 end subroutine

 subroutine addLabelandWidget(self, newlabel, newwidget)
   implicit none
   class(zoatab) :: self
   type(c_ptr) :: newlabel, newwidget

   ! Update settings
   self%numSettings = self%numSettings + 1
   call self%labels%set(self%numSettings, newlabel)
   call self%widgets%set(self%numSettings, newwidget)

 end subroutine
 ! Should this go in a separate type?
 subroutine addListBoxSetting(self, labelText, refArray, valArray, callbackFunc, callbackData)

   use hl_gtk_zoa
   implicit none

   class(zoatab) :: self
   character(len=*), intent(in) :: labelText
   type(c_funptr), intent(in)   :: callbackFunc
   type(c_ptr), optional, intent(in)   :: callbackData
    character(kind=c_char, len=*) :: valArray(:)
    integer(c_int), dimension(:) :: refArray

  ! Local only
   type(c_ptr) :: newlabel, newwidget

   ! Create Objects
   newlabel = gtk_label_new(labelText//c_null_char)
   call hl_gtk_combo_box_list2_new(newwidget, refArray, valArray)
   call g_signal_connect (newwidget, "changed"//c_null_char, callbackFunc, callbackData)

   ! Update settings lists
   call self%addLabelandWidget(newlabel, newwidget)


 end subroutine

 subroutine buildSettings(self)

   implicit none
   class(zoatab) :: self

   type(c_ptr) :: table, expander
   integer(kind=c_int) :: i
   integer :: maxRow

    table = gtk_grid_new ()
    call gtk_grid_set_column_homogeneous(table, TRUE)
    call gtk_grid_set_row_homogeneous(table, TRUE)

    if (self%numSettings.EQ.0) RETURN

    if (mod(self%numSettings,2).EQ.0) THEN
        maxRow = self%numSettings/2
      else
        maxRow = (self%numSettings+1)/2
    end if

   do i=1, maxRow
     ! Label
     call gtk_grid_attach(table, self%settingobj_get(i,0), 0_c_int, i-1, 1_c_int, 1_c_int)
     ! Widget
     call gtk_grid_attach(table, self%settingobj_get(i,1), 1_c_int, i-1, 1_c_int, 1_c_int)
   end do
   do i=maxRow+1,self%numSettings
     ! Label
     call gtk_grid_attach(table, self%settingobj_get(i,0), 2_c_int, i-maxRow-1, 1_c_int, 1_c_int)
     ! Widget
     call gtk_grid_attach(table, self%settingobj_get(i,1), 3_c_int, i-maxRow-1, 1_c_int, 1_c_int)
   end do

    ! Create Settings Box
    ! The table is contained in an expander, which is contained in the vertical box:
    expander = gtk_expander_new_with_mnemonic ("_The parameters:"//c_null_char)
    call gtk_expander_set_child(expander, table)
    call gtk_expander_set_expanded(expander, FALSE)

    ! We create a vertical box container:
    call gtk_box_append(self%box1, expander)
    call gtk_widget_set_vexpand (self%box1, FALSE)


 end subroutine

 subroutine finalizeWindow(self)
   use g
   implicit none
   class(zoatab) :: self

   type(c_ptr) :: scrolled_tab
    type(c_ptr) :: dcname
    character(len=80) :: dname

   integer :: location
    PRINT *, "FINALIZING WINDOW in ZOATAB"
    call self%buildSettings()

    call gtk_box_append(self%box1, self%canvas)


    !call gtk_box_append(self%box1, self%zoatab_cda)
    call gtk_widget_set_vexpand (self%box1, FALSE)

    scrolled_tab = gtk_scrolled_window_new()
    PRINT *, "SETTING CHILD FOR SCROLLED TAB"
    call gtk_scrolled_window_set_child(scrolled_tab, self%box1)
    location = gtk_notebook_append_page(notebook, scrolled_tab, self%tab_label)
    call gtk_notebook_set_current_page(notebook, location)


    call gtk_widget_set_name(self%box1, "TestLabel"//c_null_char)
    !dcname = g_type_name(gtk_widget_get_type(self%zoatab_cda))
    !dcname = g_type_name_from_instance(self%zoatab_cda)
    !call c_f_string(dcname, dname)
    !PRINT *, "WIDGET NAME IS ", dname


 end subroutine

 function settingobj_get(self,ind,flag) result(c_object)
        ! Arguments

        class(zoatab), intent(in) :: self
        integer, intent(in) :: ind, flag
        type(c_ptr), pointer :: c_object

        ! Local Variables
        class(*), pointer :: item


        ! Process
        if (flag.eq.0) THEN

           item => self%labels%get(ind)
         else
           item => self%widgets%get(ind)
         end if

        select type (item)
        class is (c_ptr)
            c_object => item
        class default
            nullify(c_object)
        end select

    end function
 !
 ! subroutine createCairoDrawingArea(self)
 !
 !     !use kdp_interfaces, only : ROUTEDRAWING
 !     !use kdp_draw, only: DRAWOPTICALSYSTEM
 !     use ROUTEMOD
 !
 !     class(zoatab), intent(inout) :: self
 !     ! This save parameter may be a hack.  I added this because I need to pass
 !     ! the ID_SETTING to the drawing function, but if I just define this as
 !     ! a target, the value the pointer references dhanges after I end this
 !     ! routine.  Fortran does not support defining a variable in a type as
 !     ! target, so the way I found around this is to save this value, which
 !     ! forces it to persist.
 !     integer(kind=c_int), target, save :: TARGET_NEWPLOT
 !
 !     type(c_ptr) :: fordebug
 !     integer(kind=c_int), pointer :: debugInt
 !
 !
 !     self%canvas = gtk_drawing_area_new()
 !     TARGET_NEWPLOT = self%ID_PLOTTYPE
 !
 !     call gtk_drawing_area_set_content_width(self%canvas, self%width)
 !     call gtk_drawing_area_set_content_height(self%canvas, self%height)
 !
 !     ! Does this go in a finalize tab routine?
 !     !call gtk_drawing_area_set_draw_func(self%zoatab_cda, &
 !    !            & c_funloc(DRAWOPTICALSYSTEM), c_loc(TARGET_NEWPLOT_RAYFAN), c_null_funptr)
 !    if (self%ID_PLOTTYPE > 0) THEN
 !      PRINT *, "TARGET TO SEND TO ROUTEDRAWING IS ", TARGET_NEWPLOT
 !      !fordebug = c_loc(TARGET_NEWPLOT)
 !      !PRINT *, "PRINT FOR PTR IS ", fordebug
 !      !call c_f_pointer(fordebug, debugInt)
 !      !PRINT *, "After pointer copy, Integer is ", debugInt\
 !      !allocate(self%DEBUG_PLOTTYPE)
 !      !self%DEBUG_PLOTTYPE = ID_NEWPLOT_RAYFAN
 !      !PRINT *, "DEBUG_PLOTTYPE IS ", c_loc(self%DEBUG_PLOTTYPE)
 !     !call gtk_drawing_area_set_draw_func(self%canvas, &
 !    !            & c_funloc(ROUTEDRAWING), c_loc(self%DEBUG_PLOTTYPE), c_null_funptr)
 !    !PRINT *, "AFTER GTK DRAWING "
 !
 !     call gtk_drawing_area_set_draw_func(self%canvas, &
 !                & c_funloc(ROUTEDRAWING), c_loc(TARGET_NEWPLOT), c_null_funptr)
 !
 !    end if
 !
 !
 ! end subroutine


end module zoa_tab
