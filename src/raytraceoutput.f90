!Module to handle raytrace data
module mod_raytraceoutput

type :: raytraceoutput

    integer, allocatable :: surface(:)
    real, allocatable ::  x(:), y(:), z(:), x_angle(:), y_angle(:)
end type raytraceoutput


interface raytraceoutput
    module procedure :: raytraceoutput_constructor
end interface raytraceoutput

subroutine raytraceoutput_constructor(surface, x,y,z, x_angle, y_angle)

end subroutine raytraceoutput_constructor

end module mod_raytraceoutput
