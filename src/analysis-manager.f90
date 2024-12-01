! This is meant to be a partner to lens data manager where you can get analysis in one place
! Will take a long time to add everything so for now will just add as needed
! Whether something should be in lens data manager or analysis manager should be separated by whether it is calcualted or not.
! FOr example effective focal length should be here, but I initially set it up in ldm.  
! radii, curvature, glass type, pickups, solves should be in LDM
module mod_analysis_manager
    use globals, only: long
    use global_widgets, only: curr_par_ray_trace, sysConfig

    type analysis_manager
   
    contains
     procedure :: getTransverseComa

    end type

    type(analysis_manager) :: am

    contains    

    function getTransverseComa(self) result (res)
        implicit none
        class(analysis_manager) :: self
        real(long) :: res

        res = 0.0_long

        CALL PROCESSILENT('MAB3 ALL')
        call PROCESKDP("MAB3 ALL")
  
        call MMAB3_NEW(.TRUE., sysConfig%refWavelengthIndex)
        ! This essentially serves as documentation for which index is which term
        res = curr_par_ray_trace%CSeidel(2,ubound(curr_par_ray_trace%CSeidel, dim=2))

    end function


end module