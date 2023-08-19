module ui_ast_fc_dist
  use gtk
  use global_widgets
  use zoa_ui
  use iso_c_binding
  use zoa_tab


type, extends(ui_settings) :: ast_fc_dist_settings
   integer ast_field_dir
   integer ast_numRays
   integer changed
   integer wavelength
   logical autoplotupdate
   type(c_ptr) :: canvas
   character(len=140) ::astcalccmd
   character(len=140) ::distcalccmd
   character(len=140) ::fccalccmd



 contains
    procedure, public :: is_changed => ast_fc_dist_is_changed
    procedure, public, pass(self) :: set_ast_field_dir
    procedure, public, pass(self) :: set_ast_num_rays
    !procedure, public, pass(self) :: set_ray_fan_wavelength
    procedure, public :: replot => ast_fc_dist_replot


end type ast_fc_dist_settings

interface ast_fc_dist_settings
  module procedure :: ast_fc_dist_settings_constructor
end interface ast_fc_dist_settings


type, extends(zoatab) :: astfcdist_tab
contains
  procedure :: newPlot => ast_fc_dist_new
end type

  type(ast_fc_dist_settings) :: ast_settings

contains

type(ast_fc_dist_settings) function ast_fc_dist_settings_constructor(canvas) result(self)

    type(c_ptr) :: canvas

    self%ast_field_dir = ID_AST_FIELD_Y
    self%ast_numRays = 10
    self%autoplotupdate = .TRUE.
    self%wavelength = 2 ! TODO NEED TO GET DEFAULT WAVELENGTH FROM PRESCRIPTION
    self%astcalccmd = "AST"
    self%distcalccmd = "DIST"
    self%fccalccmd = "FLDCV"

    self%canvas = canvas



end function

function ast_fc_dist_is_changed(self) result(flag)
  class(ast_fc_dist_settings), intent(in) :: self
  integer :: flag
  flag = self%changed
end function

subroutine set_ast_field_dir(self, ID_SETTING)
  class(ast_fc_dist_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING

  if (self%ast_field_dir.ne.ID_SETTING) THEN
     self%ast_field_dir = ID_SETTING
     self%changed = 1
     PRINT *, "AST FIELD DIR CHANGED!"
     if (self%autoplotupdate) then
        call self%replot()
        !call plot_ast_fc_dist(self%canvas)
      end if
  end if

end subroutine

subroutine set_ast_num_rays(self, ID_SETTING)
  class(ast_fc_dist_settings), intent(inout) :: self
  integer, intent(in) :: ID_SETTING
  integer :: val

  PRINT *, "NUM RAYS TO SET IS ", ID_SETTING
  PRINT *, "NUM OF CURRENT RAYS IS ", self%ast_numRays

  if (ID_SETTING.GT.10.AND.ID_SETTING.LT.50) Then
     val = ID_SETTING
   else
     if (ID_SETTING < 10 ) val = 10
     if (ID_SETTING > 50 ) val = 50
  end if


  if (self%ast_numRays.ne.val) THEN

     self%ast_numRays = val


     self%changed = 1
     PRINT *, "NUM AST RAYS CHANGED!"
     if (self%autoplotupdate) then
         call self%replot()
         !call plot_ast_fc_dist(self%canvas)
       end if
  end if

end subroutine

subroutine ast_fc_dist_replot(self)
  class(ast_fc_dist_settings) :: self

  character PART1*5, PART2*5, AJ*3, A6*3, AW1*23, AW2*23
  character(len=5) :: ftext

        ! CASE(IDF_AST)
        ! CALL WDIALOGGETRADIOBUTTON(IDF_PL3,ISET)
        ! CALL WDIALOGGETINTEGER(IDF_N2,JSET)
        ! CALL WDIALOGGETCHECKBOX(IDF_PLOTIT2,KSET)
        ! IF(JSET.LT.10) JSET=10
        ! IF(JSET.GT.50) JSET=50
        ! WRITE(OUTLYNE,*) ' '
        ! CALL SHOWIT(1)
        ! IF(ISET.EQ.1) WRITE(INPUT,*) 'AST,0,,',JSET
        ! IF(ISET.EQ.2) WRITE(INPUT,*) 'AST,90,,',JSET
        ! CALL PROCES
        ! IF(KSET.EQ.1) THEN
        ! INPUT='PLTAST,,1'
        ! CALL PROCES
        ! CALL GRAPHOUTPUT
        !
       select case (self%ast_field_dir)
       case (ID_AST_FIELD_Y)
         ftext = ",0,,  "
       case (ID_AST_FIELD_X)
         ftext = ",90,, "
       case default
         ftext = ",0,,  "
       end select
       CALL ITOAA(self%ast_numRays, A6)
       self%astcalccmd = 'AST'//trim(ftext)//A6
       PRINT *, "Num rays is ", self%ast_numRays
       PRINT *, "ftext ", trim(ftext), " A6 ", A6
       PRINT *, "COMMAND SENT TO KDP IN AST REPLOT IS ", 'AST'//trim(ftext)//A6
       CALL PROCESKDP('AST'//trim(ftext)//A6)

       self%distcalccmd = 'DIST'//trim(ftext)//A6
       self%fccalccmd   = 'FLDCV'//trim(ftext)//A6

       PRINT *, "ABOUT TO TRIGGER REPLOT!"
       CALL PROCESKDP('PLTAST 1')
       call plot_ast_fc_dist(self%canvas)
!

end subroutine



subroutine ast_fc_dist_new(self)
  use zoa_tab
  !use ROUTEMOD
  !use kdp_draw, only: plot_ast_fc_dist
  use gtk_draw_hl
  use g
  !use handlers, only: plot_04debug
  implicit none

  type(c_ptr) :: parent_window, localcanvas, isurface
  class(astfcdist_tab) :: self

  type(c_ptr) :: spinButton_numRays, spinButton_wavelength
  integer :: usePLPLOT = 1
  ! Added these target parameters to have only one callback function and satisfy
  ! requirement to have a target attribute for a pointer for gtk.  I could not
  ! find a more elegant solution, and this seems better than a bunch of small
  ! callback functions
  !integer, target :: TARGET_RAYFAN_WAVELENGTH  = ID_WAVELENGTH
  integer, target :: TARGET_AST_FIELDXY = ID_AST_FIELDXY
  integer, target :: TARGET_AST_NUMRAYS  = ID_RAYFAN_NUMRAYS
  integer, target :: TARGET_PLOTTYPE_AST  = ID_PLOTTYPE_AST

  character(kind=c_char, len=20), dimension(2) :: vals_ast_fieldxy
  integer(c_int), dimension(2) :: refs_ast_fieldxy




  vals_ast_fieldxy = [character(len=20) :: "Y Field", "X Field"]

  refs_ast_fieldxy = [ID_AST_FIELD_Y, ID_AST_FIELD_X]


  !call astfcdist_tab%initialize(notebook, "Astig FC Dist", ID_PLOTTYPE_AST)
  ! Create backing surface

  !isurface = cairo_image_surface_create(s_type, szx, szy)
  !isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
  !call g_object_set_data(astfcdist_tab%canvas, "backing-surface", isurface)

    !isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200_c_int, 500_c_int)
    !isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
    !call g_object_set_data(astfcdist_tab%canvas, "backing-surface", isurface)


  !ast_cairo_drawing_area = astfcdist_tab%canvas
  if (usePLPLOT == 1) THEN
    PRINT *, "Plotting Astig via PL PLOT!"
    self%canvas = hl_gtk_drawing_area_new(size=[1200,500], &
          & has_alpha=FALSE)

          ast_settings = ast_fc_dist_settings(self%canvas)
    !ast_cairo_drawing_area = astfcdist_tab%canvas
          !& has_alpha=FALSE, expose_event=c_funloc(plot_ast_fc_dist), &
          !& data_expose=c_loc(TARGET_PLOTTYPE_AST))
          !ast_cairo_drawing_area = astfcdist_tab%canvas
    !call plot_04debug(astfcdist_tab%canvas)
          !call debugPLPLOT(astfcdist_tab%canvas)
          call plot_ast_fc_dist(self%canvas)

    ! isurface = g_object_get_data(astfcdist_tab%canvas, "backing-surface")
    ! PRINT *, "isurface is ", isurface
    ! if (.not. c_associated(isurface)) then
    !    PRINT *, "new astig plot :: Backing surface is NULL"
    !    return
    ! end if
    !   PRINT *, "ASTIG CANVAS IS ", astfcdist_tab%canvas
     !call gtk_drawing_area_set_draw_func(astfcdist_tab%canvas, &
      !               & c_funloc(plot_ast_fc_dist), c_loc(TARGET_PLOTTYPE_AST), c_null_funptr)

!
  ! astfcdist_tab%canvas = localcanvas
  else

    !call gtk_drawing_area_set_draw_func(astfcdist_tab%canvas, &
    !                & c_funloc(ROUTEDRAWING), c_loc(TARGET_PLOTTYPE_AST), c_null_funptr)
  end if

  call self%addListBoxSetting("Field Type", refs_ast_fieldxy, vals_ast_fieldxy, &
  & c_funloc(callback_ast_fc_dist_settings), c_loc(TARGET_AST_FIELDXY))


  spinButton_numRays = gtk_spin_button_new (gtk_adjustment_new(value=10d0, &
                                                              & lower=10d0, &
                                                              & upper=50d0, &
                                                              & step_increment=1d0, &
                                                              & page_increment=1d0, &
                                                              & page_size=1d0),climb_rate=1d0, &
                                                              & digits=0_c_int)



 call self%addSpinBoxSetting("Number of Rays", spinButton_numRays, &
 & c_funloc(callback_ast_fc_dist_settings), c_loc(TARGET_AST_NUMRAYS))


  call self%finalizeWindow()
  !ast_window = astfcdist_tab%box1
  !PRINT *, "AT END OF new astig plot, canvas ptr is ", astfcdist_tab%canvas



end subroutine


subroutine callback_ast_fc_dist_settings (widget, gdata ) bind(c)
   use iso_c_binding
   use hl_gtk_zoa
   use zoa_ui
   implicit none
   type(c_ptr), value, intent(in) :: widget, gdata
   integer :: int_value

  integer(kind=c_int), pointer :: ID_SETTING

  call c_f_pointer(gdata, ID_SETTING)

  select case (ID_SETTING)

  case (ID_AST_FIELDXY)
    call ast_settings % set_ast_field_dir(hl_zoa_combo_get_selected_list2_id(widget))

  case (ID_RAYFAN_NUMRAYS)
    call ast_settings % set_ast_num_rays(INT(gtk_spin_button_get_value (widget)))

  end select

end subroutine

 subroutine plot_ast_fc_dist(localcanvas)

        USE GLOBALS
        !use handlers
        use zoa_plot
        use g


     IMPLICIT NONE

     type(c_ptr)   :: localcanvas, my_cairo_context
     !type(c_ptr), value :: gdata
     type(c_ptr) ::  isurface
     !integer(c_int), value, intent(in) :: win_width, win_height
     type(zoaplot) :: lin1, lin2, lin3
     type(multiplot) :: mplt

     integer :: numPts, numPtsDist, numPtsFC

      REAL:: DDTA(0:50), xDist(0:50), yDist(0:50), x1FC(0:50), x2FC(0:50), yFC(0:50)

      REAL:: FLDAN(0:50)

      !COMMON FLDAN, DDTA


   !type(zoatab) :: astfcdist_tab
!
!   PRINT *, "About to init POWSYM Tab"
!
!   call powsym_tab%initialize(notebook, "Power and Symmetry", -1)
!

!
  ! astfcdist_tab%canvas = localcanvas
!
!   PRINT *, "POWSYM Initialized!"
!
!   WRITE(strTitle, "(A15, F10.3)"), "Power:  w = ", w_sum


    ! isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200_c_int, 500_c_int)
    ! isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
    ! call g_object_set_data(ast_cairo_drawing_area, "backing-surface", isurface)
    !

    isurface = g_object_get_data(localcanvas, "backing-surface")
    PRINT *, "isurface is ", LOC(isurface)
    if (.not. c_associated(isurface)) then
       PRINT *, "new astig plot :: Backing surface in ast_cairo_ is NULL"
       return
    end if


  PRINT *, "PLOT_AST_FC_DIST Called! "
  PRINT *, "Canvas PTR is ", LOC(localcanvas)
  call getFieldCalcResult(DDTA, X2FC, FLDAN, numPts, 1)
   !PRINT *, "DDTA is ", DDTA
   !PRINT *, "FLDAN is ", FLDAN
   call mplt%initialize(localcanvas, 3,1)
   !mplt%cc = my_cairo_context
   !call mplt%initialize(my_cairo_context, 2,1)
   !
 !call mplt%initialize(drawing_area_plot, 2,1)
!   PRINT *, "MPLOT INITIALIZED!"
   call lin1%initialize(c_null_ptr, REAL(DDTA(0:numPts)),FLDAN(0:numPts), &
   & xlabel='Astigmatism (in)'//c_null_char, ylabel='FOV (deg)'//c_null_char, &
   & title='tmp'//c_null_char)


 ! Temporary hack to plot distortion
 CALL PROCESKDP(ast_settings%distcalccmd)

  call getFieldCalcResult(xDist, x2FC, yDist, numPtsDist, 2)


   call lin2%initialize(c_null_ptr, REAL(xDist(0:numPtsDist)),yDist(0:numPtsDist), &
   & xlabel='Distortion (%)'//c_null_char, ylabel='FOV (deg)'//c_null_char, &
   & title='tmp'//c_null_char)

  CALL PROCESKDP(ast_settings%fccalccmd)
  call getFieldCalcResult(x1FC, x2FC, yFC, numPtsFC, 3)


   call lin3%initialize(c_null_ptr, REAL(x1FC(0:numPtsFC)),yFC(0:numPtsFC), &
   & xlabel='Field Curvature '//c_null_char, ylabel='FOV (deg)'//c_null_char, &
   & title='tmp'//c_null_char)
   call lin3%addXYPlot(X2FC(0:numPtsFC),FLDAN(0:numPtsFC))
   call lin3%setDataColorCode(PL_PLOT_BLUE)
   call lin3%setLineStyleCode(4)

   !call lin2%initialize(c_null_ptr, REAL(DDTA(0:numPts)),FLDAN(0:numPts), &
   !& xlabel='Astigmatism (in)'//c_null_char, ylabel='FOV (deg)'//c_null_char, &
   !& title='tmp'//c_null_char)


   call mplt%set(1,1,lin1)
   call mplt%set(2,1,lin2)
   call mplt%set(3,1,lin3)



   call mplt%draw()

    !call gtk_widget_queue_draw(localcanvas)
    !call hl_gtk_drawing_area_cairo_destroy(cc)

!
!   PRINT *, "ABOUT TO FINALIZE NEW TAB!"
!   call powsym_tab%finalizeWindow()
!
!
!                        XMINJK1=1.0E20
!                        XMAXJK1=-1.0E20
!                        DO I=0,PNTNUM
!                        Y(I+1)=FLDAN(I)
!                X1(I+1)=REAL(DDTA(I))
!           IF(REAL(DDTA(I)).GT.XMAXJK1) XMAXJK1=REAL(DDTA(I))
!           IF(REAL(DDTA(I)).LT.XMINJK1) XMINJK1=REAL(DDTA(I))
!                        X(I+1)=X1(I+1)
!                        END DO
!
! end subroutine

end subroutine



end module
