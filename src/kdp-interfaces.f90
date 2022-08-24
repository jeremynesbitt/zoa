module kdp_interfaces

contains

subroutine POWSYM


   use global_widgets
   use handlers, only : updateTerminalLog
    use zoa_plot
    !use mod_plotopticalsystem


  integer(kind=c_int) :: ipick, i, endSurface, ii
 character(len=100) :: ftext, strTitle
 CHARACTER(LEN=*), PARAMETER  :: FMT1 = "(I5, F10.3, F10.3)"
 CHARACTER(LEN=*), PARAMETER  :: FMTHDR = "(A12, A5, A5)"
 character(len=100) :: logText
 real, ALLOCATABLE :: w(:), symcalc(:)
 real :: w_sum, s_sum, aplanatic, imageNA
 integer :: totalSurfaces
 integer, ALLOCATABLE ::  surfaceno(:)


   PRINT *, "New Command POWSYM in kdp_interfaces!"


  !PRINT *, "Before Mod Call, ", my_window

  !ipick = hl_gtk_file_chooser_show(new_files, &
  !       & create=FALSE, multiple=TRUE, filter=["image/*"], &
  !       & parent=my_window, all=TRUE)

  !ftext = 'LIB GET 1 '
  !ftext = 'CV2PRG DoubleGauss.seq'
  ! ftext = 'CV2PRG LithoBraat.seq'
  !
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'RTG ALL'
  ! CALL PROCESKDP(ftext)
  !
  !
  ! ftext = 'COLORSET RAYS 6'
  ! CALL PROCESKDP(ftext)
  ! call getOpticalSystemLastSurface(endSurface)
  ! call ld_settings%set_end_surface(endSurface)
  ! ftext = 'VIECO'
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'PXTY ALL'
  ! CALL PROCESKDP(ftext)
  !
  ! ftext = 'OCDY'
  ! CALL PROCESKDP(ftext)


PRINT *, "Magnification is ", curr_par_ray_trace%t_mag




  ! Compute lens weight and symmetry
  allocate(w(curr_lens_data % num_surfaces-2))
  allocate(symcalc(curr_lens_data % num_surfaces-2))
  allocate(surfaceno(curr_lens_data % num_surfaces-2))

  PRINT *, "SIZE OF w is ", size(w)
  PRINT *, "SIZE of no_surfaces is ", size(surfaceno)

  WRITE (logText, FMTHDR), "Surface", "w_j", "s_j"
  call updateTerminalLog(logText, "black")


  do ii = 2, curr_lens_data % num_surfaces - 1
     w(ii-1) = -1/(1-curr_par_ray_trace%t_mag)
     w(ii-1) = w(ii-1) * (curr_lens_data % surf_index(ii) - curr_lens_data % surf_index(ii-1)) &
     & * curr_par_ray_trace % marginal_ray_height(ii) * curr_lens_data % curvatures(ii) &
     & / curr_lens_data % surf_index(curr_lens_data % num_surfaces) &
     & / curr_par_ray_trace % marginal_ray_angle(curr_lens_data % num_surfaces)
     w_sum = w_sum + w(ii-1)*w(ii-1)

     !PRINT *, "Check Ref Stop ", curr_lens_data % ref_stop
    !PRINT *, "SURF INDEX ", curr_lens_data % surf_index(ii)
    !PRINT *, "AOI is ", curr_par_ray_trace % chief_ray_aoi(ii)
    !allocate(symcalc(curr_lens_data % num_surfaces-2))

      totalSurfaces = curr_lens_data % num_surfaces
      aplanatic = curr_par_ray_trace % marginal_ray_angle(ii) / curr_lens_data % surf_index(ii)
      aplanatic = aplanatic - curr_par_ray_trace % marginal_ray_angle(ii-1) / curr_lens_data % surf_index(ii-1)
      imageNA = curr_lens_data%surf_index(totalSurfaces) * curr_par_ray_trace%marginal_ray_angle(totalSurfaces)

      !PRINT *, "IMAGE NA is ", imageNA
      !PRINT *, "APLANATIC IS ", aplanatic
      !PRINT *, "Marginal Ray Angle is ", curr_par_ray_trace % marginal_ray_angle(ii)
      !PRINT *, "Surface Index is ", curr_lens_data % surf_index(ii)
      symcalc(ii-1) = 1/(1-curr_par_ray_trace%t_mag)
      ! symcalc(ii-1) = symcalc(ii-1) * aplanatic * curr_lens_data % surf_index(ii) * curr_par_ray_trace % chief_ray_aoi(ii) &
      ! & / ((curr_lens_data % surf_index(curr_lens_data%ref_stop)* curr_par_ray_trace % chief_ray_aoi(curr_lens_data%ref_stop)) &
      ! & * imageNA)

      symcalc(ii-1) = symcalc(ii-1) * curr_lens_data % surf_index(ii-1) * curr_par_ray_trace % chief_ray_aoi(ii)

      !PRINT *, "FIrst Term is ", symcalc(ii-1)

      symcalc(ii-1) = symcalc(ii-1) /curr_lens_data%surf_index(curr_lens_data%ref_stop)
      symcalc(ii-1) = symcalc(ii-1) /curr_par_ray_trace%chief_ray_aoi(curr_lens_data%ref_stop)
      symcalc(ii-1) = symcalc(ii-1)*aplanatic/imageNA


      s_sum = s_sum + symcalc(ii-1)*symcalc(ii-1)

      surfaceno(ii-1) = ii-1

      WRITE(logText, FMT1), surfaceno(ii-1), w(ii-1), symcalc(ii-1)
      call updateTerminalLog(logText, "black")


  end do

  w_sum = SQRT(w_sum/(curr_lens_data % num_surfaces-2))
  s_sum = SQRT(s_sum/(curr_lens_data % num_surfaces-2))

  !PRINT *, " w is ", w
  WRITE(logText, *), " w_sum is ", w_sum
  call updateTerminalLog(logText, "black")

  !PRINT *, " s is ", symcalc
  WRITE(logText, *), " s_sum is ", s_sum
  call updateTerminalLog(logText, "black")

    !call barchart2(x,y)
    ! print *, "Calling Plotter"
     !plotter = barchart(drawing_area_plot,surfaceno,abs(w))
     !call plotter % initialize(drawing_area_plot,surfaceno,abs(w))

     ! call plotter % setxlabel
     ! call plotter % setMajorGridLines(TRUE)

     !call plotter % drawPlot()
    ! print *, "Done calling plotter"

  !PRINT *, "Paraxial Data tst ", curr_par_ray_trace%marginal_ray_height

! Multiplot
  call POWSYM_PLOT(surfaceno, w, w_sum, symcalc, s_sum)


end subroutine POWSYM

subroutine POWSYM_PLOT(surfaceno, w, w_sum, symcalc, s_sum)

   use global_widgets

  use zoa_plot
  use zoa_tab
  use gtk_draw_hl
  implicit none

 real, intent(in) :: w(:), symcalc(:)
 real, intent(in) :: w_sum, s_sum
 integer, intent(in) ::  surfaceno(:)

 character(len=100) :: strTitle
  type(barchart) :: bar1, bar2
  type(multiplot) :: mplt
  type(c_ptr) :: localcanvas
  type(zoatab) :: powsym_tab

  PRINT *, "About to init POWSYM Tab"

  call powsym_tab%initialize(notebook, "Power and Symmetry", -1)

    localcanvas = hl_gtk_drawing_area_new(size=[1200,500], &
         & has_alpha=FALSE)

  powsym_tab%canvas = localcanvas

  PRINT *, "POWSYM Initialized!"

  WRITE(strTitle, "(A15, F10.3)"), "Power:  w = ", w_sum
  call mplt%initialize(powsym_tab%canvas, 2,1)
  !call mplt%initialize(drawing_area_plot, 2,1)
  PRINT *, "MPLOT INITIALIZED!"
  call bar1%initialize(c_null_ptr, real(surfaceno),abs(w), &
  & xlabel='Surface No'//c_null_char, ylabel='w'//c_null_char, &
  & title=trim(strTitle)//c_null_char)
  !PRINT *, "Bar chart color code is ", bar1%dataColorCode
  WRITE(strTitle, "(A15, F10.3)"), "Symmetry:  s = ", s_sum
  call bar2%initialize(c_null_ptr, real(surfaceno),abs(symcalc), &
  & xlabel='Surface No'//c_null_char, ylabel='s'//c_null_char, &
  & title=trim(strTitle)//c_null_char)
  call bar2%setDataColorCode(PL_PLOT_BLUE)
  call mplt%set(1,1,bar1)
  call mplt%set(2,1,bar2)
  call mplt%draw()

  PRINT *, "ABOUT TO FINALIZE NEW TAB!"
  call powsym_tab%finalizeWindow()

end subroutine

subroutine EDITOR
  use lens_editor
  use global_widgets
  use handlers

    if (.not. c_associated(lens_editor_window))  THEN
       PRINT *, "Call New Lens Editor Window"
       call lens_editor_new(my_window)
    else
      PRINT *, "Do nothing..lens editor exists. "

    end if



end subroutine EDITOR

SUBROUTINE RUN_WDRAWOPTICALSYSTEM
!     THIS IS THE DRIVER ROUTINE FOR SENDING GRAPHICS TO
!     A GRAPHIC WINDOW
      !USE WINTERACTER
      IMPLICIT NONE
      LOGICAL EXISD
      !INCLUDE 'DATMAI.INC'
      !CALL WDRAWOPTICALSYSTEM
      RETURN
END SUBROUTINE RUN_WDRAWOPTICALSYSTEM



end module kdp_interfaces
