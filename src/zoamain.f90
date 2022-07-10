! ZOA MAIN
! This file contains the main program and the GUI modules

module lens_analysis

   !use global_widgets
   use plplot
   use plplot_extra
   use gtk_draw_hl
  use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width


contains

subroutine plotLensData

    USE GLOBALS
    use global_widgets
    !use handlers
    use mod_barchart
    use mod_plotopticalsystem

    use, intrinsic :: iso_c_binding

    implicit none

    character(len=100) :: ftext
    integer :: i
    REAL*8 raydata(1:50,0:499)
    REAL*8 x(1:10),y(1:10)
    type(barchart) :: plotter




    ftext = 'LIB GET 1 '

    PRINT *, ftext

    CALL PROCESKDP(ftext)

    ftext = trim("FOB 1")

    CALL PROCESKDP(ftext)

    ftext = trim("PRXYZ ALL")

    CALL PROCESKDP(ftext)

    CALL getRayTraceOutput(raydata)

    PRINT *, "Test RAYRAY Output ", rayData(2,1:10)

    y = rayData(2,1:10)
    i = 1
    do while (i<=10)
     x(i) = i
     i = i+1
     end do
    !x = 1:10

    !call barchart2(x,y)
    print *, "Calling Plotter"
    plotter = barchart(drawing_area_plot,x,y)
    call plotter % drawPlot()
    print *, "Done calling plotter"

    !print *, "ylabel = ", plotter%ylabel
    ! Pseudocode
    ! New Bar Plot (x,y,title,xlabel,ylabel)
    ! This plot will


end subroutine plotLensData

subroutine barchart2(x,y)

    use global_widgets
    real*8 :: x(10), y(10)

    call x12f(drawing_area_plot,x,y)


end subroutine barchart2

subroutine x12f(area, x, y)

    use plplot
    implicit none

    type(c_ptr), intent(in) :: area
    type(c_ptr)  :: cc, cs
    character(len=20) :: string
    character(len=25) :: geometry
    integer :: i
    integer :: plparseopts_rc
    integer :: plsetopt_rc
    real*8 :: x(10), y(10)

    real(kind=pl_test_flt) :: y0(10)
    real(kind=pl_test_flt) :: pos(5)   = (/0.0_pl_test_flt, 0.25_pl_test_flt, 0.5_pl_test_flt, 0.75_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: red(5)   = (/0.0_pl_test_flt, 0.25_pl_test_flt, 0.5_pl_test_flt, 1.0_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: green(5) = (/1.0_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: blue(5)  = (/1.0_pl_test_flt, 1.0_pl_test_flt, 0.5_pl_test_flt, 0.25_pl_test_flt, 0.0_pl_test_flt/)

    !   Process command-line arguments
    !plparseopts_rc = plparseopts(PL_PARSE_FULL)
    !if(plparseopts_rc .ne. 0) stop "plparseopts error"

    cc = hl_gtk_drawing_area_cairo_new(area)
    cs = cairo_get_target(cc)

    !  Initialize plplot
    !call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc .ne. 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") cairo_image_surface_get_width(cs), &
         & cairo_image_surface_get_height(cs)
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc .ne. 0) stop "plsetopt error"

    call plinit()

    call pl_cmd(PLESC_DEVINIT, cc)

    call pladv(0)
    call plvsta
    !call plwind( 1980._pl_test_flt, 1990._pl_test_flt, -15._pl_test_flt, 40._pl_test_flt )
    call plwind( -1._pl_test_flt, 11._pl_test_flt, -15._pl_test_flt, 40._pl_test_flt )
    call plbox( 'bcnt', 1._pl_test_flt, 0, 'bcnv', 10._pl_test_flt, 0 )
    !call plbox('bcfghlnst', 0.0_plflt, 0, 'bcghnstv', 0.0_plflt, 0)
    call plcol0(2)
    call pllab( 'Surface', 'Y Position [mm]', '#frPLplot Example 12' )

    y0 = (/ 5, 15, 12, 24, 28, 30, 20, 8, 12, 3 /)

    call plscmap1l(.true.,pos,red,green,blue)

    do i = 0, 9
        !       call plcol0(i + 1)
        call plcol1(real(i,kind=pl_test_flt)/9.0_pl_test_flt)
        call plpsty(0)
        !call plfbox( 1980._pl_test_flt+i, y(i+1) )
        call plfbox( x(i+1), y(i+1) )
        write (string, '(i0)') int(y0(i+1))

        !call plptex( x(i+1), y(i+1), &
        !       1._pl_test_flt, 0._pl_test_flt, 0.5_pl_test_flt, string )
        !call plptex( 1980._pl_test_flt+i+0.5_pl_test_flt, y0(i+1)+1._pl_test_flt, &
        !       1._pl_test_flt, 0._pl_test_flt, 0.5_pl_test_flt, string )

        !write (string, '(i0)')1980+i
        !call plmtex( 'b', 1._pl_test_flt, (i+1)*0.1_pl_test_flt-0.05_pl_test_flt, 0.5_pl_test_flt, string )
    enddo

    !    Don't forget to call PLEND to finish off!
    call plend
    call hl_gtk_drawing_area_cairo_destroy(cc)
    end subroutine

    subroutine plfbox(x0, y0)
        real(kind=pl_test_flt) x0, y0, x(4), y(4)

        x(1) = x0
        y(1) = 0._pl_test_flt
        x(2) = x0
        y(2) = y0
        !x(3) = x0+1._pl_test_flt
        x(3) = x0+.5_pl_test_flt
        y(3) = y0
        !x(4) = x0+1._pl_test_flt
        x(4) = x0+.5_pl_test_flt
        y(4) = 0._pl_test_flt
        call plfill(x, y)
        call plcol0(1)
        call pllsty(1)
        call plline(x, y)
    end subroutine plfbox








end module lens_analysis




!*******************************************************************************
! In the main program, we declare the GTK application, connect it to its
! "activate" function where we will create the GUI,
! and finally call the GLib main loop.
!*******************************************************************************
program zoa_program
 !subroutine julia_pixbuf
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  ! We will use those GTK functions and values. The "only" statement can improve
  ! significantly the compilation time:
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers
  use zoa_ui
  use mod_ray_fan_settings
  use mod_lens_draw_settings
  use global_widgets

  !use GLOBALS




  implicit none



  integer(c_int)     :: status
  !type(c_ptr)        :: app



  ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("gtk-fortran.examples.julia_pixbuf"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)

  ! UI Settings Initialization

  ld_settings = lens_draw_settings()
  rf_settings = ray_fan_settings()
  curr_lens_data = lens_data()


  PRINT *, "APP IN Main Program is ", app


  ! The activate signal will be sent by g_application_run().
  ! The c_funloc() function returns the C address of the callback function.
  ! The c_null_ptr means no data is transfered to the callback function.
  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)



  ! Now, the whole application will be managed by GLib (=> main loop).
  ! Note that commandline arguments argc, argv are not passed.
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-run
  status = g_application_run(app, 0_c_int, c_null_ptr)

  print *, "You have exited the GLib main loop, bye, bye..."

  ! Memory is freed:
  call g_object_unref(app)

end program zoa_program
