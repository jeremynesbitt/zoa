module mod_surface_type
    use iso_fortran_env, only: real64
    use global_widgets, only: curr_lens_data, curr_par_ray_trace, sysConfig
    use globals, only: long
    use zoa_ui
    use type_utils, only: string

    implicit none

    type surface_type

    type(string) :: name    ! the name of the surface in ui
    real(long) :: radius ! in lens units
    real(long) :: thickness ! thickness in lens units
    real(long) :: conic.    ! conic constant
    type(clear_aperture) :: clap ! type to handle clear apererture types
    type(glass_type) :: glass    ! define as type for now.  Could be as simple as a glass name initiall
    real(long), dimension(21)    :: data ! extra data per surface type.  For asphere, it would hold coefficients
    type(string), dimension(21)  :: param_names ! names of parameters for UI display



    contains
     procedure :: initialize => init_surface_type ! receive 
     procedure :: get_data ! returns data
     procedure :: get_param_names ! returm parameter names
     procedure :: real_ray_trace !  
     procedure :: refract ! get direction cosines and indices of refraction, and determine exit direction cosines
     procedure :: paraxial_ray_trace

 
     end type 

   type surf_ray_data

   real(long) :: x,y,z ! coordinates
   real(long) :: l,m,n ! direction cosines
   real(long) :: ln,mn,nn  ! surface normal
   real(long) :: path 
   real(long) :: n1, n2 ! index of refraction before and after surface
   integer :: surfNum, wavNum 
   real(long) :: wavelength 
   real(long), dimension(21) :: data
   type(string) :: glassName 



   end type

end module