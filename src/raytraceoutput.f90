!Module to handle raytrace data
module mod_raytraceoutput

type :: raytraceoutput

    integer, allocatable :: surface(:)
    real, allocatable ::  x(:), y(:), z(:), x_angle(:), y_angle(:)
end type raytraceoutput


interface raytraceoutput
    module procedure :: raytraceoutput_constructor
end interface raytraceoutput

contains

type(raytraceoutput) function raytraceoutput_constructor(surface, x,y,z, x_angle, y_angle)

end function raytraceoutput_constructor

end module mod_raytraceoutput
