!Module for bar chart
module mod_barchart

    use plplot
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_float
    implicit none


type :: barchart

    integer, allocatable :: surface(:)
    real, allocatable ::  x(:), y(:)

    character(len=100) :: xlabel, ylabel, title

    type(c_ptr), intent(in) :: area

    real*8 :: x(10), y(10)


end type barchart


interface barchart
    module procedure :: barchart_constructor
end interface barchart

contains

type(barchart) function barchart_constructor(area, x,y)

    type(c_ptr)  :: cc, cs
    character(len=20) :: string
    character(len=25) :: geometry
    integer :: i
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    real(kind=pl_test_flt) :: y0(10)
    real(kind=pl_test_flt) :: pos(5)   = (/0.0_pl_test_flt, 0.25_pl_test_flt, 0.5_pl_test_flt, 0.75_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: red(5)   = (/0.0_pl_test_flt, 0.25_pl_test_flt, 0.5_pl_test_flt, 1.0_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: green(5) = (/1.0_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 1.0_pl_test_flt/)
    real(kind=pl_test_flt) :: blue(5)  = (/1.0_pl_test_flt, 1.0_pl_test_flt, 0.5_pl_test_flt, 0.25_pl_test_flt, 0.0_pl_test_flt/)


end function barchart_constructor

end module mod_raytraceoutput
