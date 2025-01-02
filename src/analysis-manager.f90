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
     procedure :: getTransverseComa, getTransverseAstigmatism, getPetzvalBlur, getTransverseSpherical
     procedure :: getPSF

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

    function getTransverseSpherical(self) result (res)
        implicit none
        class(analysis_manager) :: self
        real(long) :: res

        res = 0.0_long

        CALL PROCESSILENT('MAB3 ALL')
        call PROCESKDP("MAB3 ALL")
  
        call MMAB3_NEW(.TRUE., sysConfig%refWavelengthIndex)
        ! This essentially serves as documentation for which index is which term
        res = curr_par_ray_trace%CSeidel(1,ubound(curr_par_ray_trace%CSeidel, dim=2))

    end function    

    ! TODO - refactor with Coma (pass first index to single func)
    function getTransverseAstigmatism(self) result (res)
        implicit none
        class(analysis_manager) :: self
        real(long) :: res

        res = 0.0_long

        CALL PROCESSILENT('MAB3 ALL')
        call PROCESKDP("MAB3 ALL")
  
        call MMAB3_NEW(.TRUE., sysConfig%refWavelengthIndex)
        ! This is conversion of the Smith definition to the CodeV definition.
        res = 3*curr_par_ray_trace%CSeidel(3,ubound(curr_par_ray_trace%CSeidel, dim=2))- &
        & curr_par_ray_trace%CSeidel(5,ubound(curr_par_ray_trace%CSeidel, dim=2))

    end function    

    function getPetzvalBlur(self) result (res)
        implicit none
        class(analysis_manager) :: self
        real(long) :: res

        res = 0.0_long

        CALL PROCESSILENT('MAB3 ALL')
        call PROCESKDP("MAB3 ALL")
  
        call MMAB3_NEW(.TRUE., sysConfig%refWavelengthIndex)
        ! This is conversion of the Smith definition to the CodeV definition.
        res = curr_par_ray_trace%CSeidel(5,ubound(curr_par_ray_trace%CSeidel, dim=2))

    end function        

    function getPSF(self) result(psfData)
        use global_widgets, only: curr_psf
        use DATSPD
        implicit none
        class(analysis_manager) :: self
        real(long), allocatable  :: psfData(:,:)

        allocate(psfData(TGR,TGR))

        call PROCESSILENT('PLOTPSF OFF')
        call PROCESSILENT('PSF')

        psfData = curr_psf


    end function


end module