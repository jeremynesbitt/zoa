!Module for bar chart plotting
module mod_barchart

    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_float, c_null_char

    !use handlers

   !use global_widgets
   use plplot
   use plplot_extra
   use gtk_draw_hl
    use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width

   implicit none



type :: barchart

    !integer, allocatable :: surface(:)
    type(c_ptr) :: area

    real*8 ::  x(11), y(11)
    character(len=100) :: title = 'untitled'
    character(len=100) :: xlabel = ' x '
    character(len=100) :: ylabel = ' y '
    character(len=20)  :: labelFontColor = "BLACK"
    !character(kind=plchar_vector) :: title, xlabel, ylabel
    !character(len=100) :: xlabel
    !character(len=100) :: ylabel
    !character(len=100) :: title = 'untitled'
    !character(len=100) :: xlabel = 'x'
    !character(len=100) :: ylabel = 'y'

contains
    procedure, public, pass(self) :: drawPlot
    procedure, public, pass(self) :: setLabelFont
    procedure, private, pass(self) :: getAxesLimits
    procedure, private, pass(self) :: getLabelFontCode

end type barchart


interface barchart
    module procedure :: barchart_constructor
end interface barchart

contains

type(barchart) function barchart_constructor(area, x, y) result(self)

    implicit none

    type(c_ptr), intent(in) :: area
    real*8, intent(in) ::  x(11), y(11)



    !character(len=100) :: title, xlabel, ylabel


    !   Process command-line arguments
    !plparseopts_rc = plparseopts(PL_PARSE_FULL)
    !if(plparseopts_rc .ne. 0) stop "plparseopts error"

    self%area = area
    self%x = x
    self%y = y

    self % title = trim("untitled")
    self % xlabel = trim("           x axis")

    self % ylabel = "y axis"

    self % labelFontColor = trim("BLACK")



    !if (present(title)) then
    !    self%title = title
    !else
    !    self%title = 'untitled'
    !end if


    !if (present(xlabel)) then
    !    self%xlabel = xlabel
    !else
    !    self%xlabel = 'x'
    !end if

    !if (present(ylabel)) then
    !    self%ylabel = ylabel
    !else
    !    self%ylabel = 'y'
    !end if

    !call drawPlot(self)




end function barchart_constructor

    subroutine drawPlot(self)
        class(barchart), intent(in) :: self

    type(c_ptr)  :: cc, cs
    character(len=20) :: string
    character(len=25) :: geometry
    integer :: i
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax

    real(kind=pl_test_flt) :: y0(10)
    real(kind=pl_test_flt) :: pos(5)   = (/0.0_pl_test_flt, 0.25_pl_test_flt, 0.5_pl_test_flt, 0.75_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: red(5)   = (/0.0_pl_test_flt, 0.25_pl_test_flt, 0.5_pl_test_flt, 1.0_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: green(5) = (/1.0_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: blue(5)  = (/1.0_pl_test_flt, 1.0_pl_test_flt, 0.5_pl_test_flt, 0.25_pl_test_flt, 0.0_pl_test_flt/)

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
         & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
         & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
         & 0, 0, 85, 170/), &
         & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
         & 127, 85, 170/)


    cc = hl_gtk_drawing_area_cairo_new(self%area)
    cs = cairo_get_target(cc)

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
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
    call getAxesLimits(self, xmin, xmax, ymin, ymax)

    !call plwind( -1._pl_test_flt, 11._pl_test_flt, -15._pl_test_flt, 40._pl_test_flt )
    call plwind(xmin, xmax, ymin, ymax)
    call plbox( 'bcgnt', 1._pl_test_flt, 0, 'bcgnv', 10._pl_test_flt, 0 )
    !call plbox('bcfghlnst', 0.0_plflt, 0, 'bcghnstv', 0.0_plflt, 0)
    !call plcol0(1)
    call plcol0(getLabelFontCode(self))
    !call pllab( 'Surface', 'Y Position [mm]', '#frPLplot Example 12' )
    !call pllab( 'Surface', trim('Y Position [mm]')//c_null_char, '#frPLplot Example 12' )
    call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)


    y0 = (/ 5, 15, 12, 24, 28, 30, 20, 8, 12, 3 /)

    call plscmap1l(.true.,pos,red,green,blue)

    do i = 0, 9
        !       call plcol0(i + 1)
        !call plcol1(real(i,kind=pl_test_flt)/9.0_pl_test_flt)
        call plcol0(2)
        !call plcol1(real(1,kind=pl_test_flt))
        !call plcol1(1.0_pl_test_fit)
        call plpsty(0)
        !call plfbox( 1980._pl_test_flt+i, y(i+1) )
        call barChartBox( self%x(i+1), self%y(i+1) )
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

    end subroutine drawPlot

    subroutine setLabelFont(self, desiredColor)

    class(barChart), intent(inout) :: self
    character(len=20), intent(in) :: desiredColor

    !if trim(desiredColor) == "RED"
    self % labelFontColor = trim(desiredColor)

    end subroutine setLabelFont

    function getLabelFontCode(self) result(r)

       ! Currently this function does not work correctly
       ! The string in labelFontColor is not being stored
       ! correctly

       class(barChart), intent(in) :: self
       integer :: r

       r = 1

       if (trim(self % labelFontColor) == trim("BLACK")) then
           r = 1
       else if (trim(self % labelFontColor) == trim("RED")) then
           r = 2
       end if
       print *, "labelFontColor ", trim(self%labelFontColor)
    end function getLabelFontCode

    subroutine getAxesLimits(self, xmin, xmax, ymin, ymax)

    class(barChart), intent(in) :: self
    real(kind=pl_test_flt), intent(inout) :: xmin, xmax, ymin, ymax

    xmin = minval(self%x)
    xmax = maxval(self%x)
    ymin = minval(self%y)
    ymax = maxval(self%y)

    end subroutine getAxesLimits

    subroutine barChartBox(x0, y0)
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
    end subroutine barChartBox

end module mod_barchart
