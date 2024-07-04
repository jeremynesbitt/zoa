module mod_lens_data_manager
    use iso_fortran_env, only: real64
    use global_widgets, only: curr_lens_data
    use global_widgets, only: sysConfig    

    type lens_data_manager
   
    contains
     procedure, public, pass(self) :: getSurfThi
     procedure, public, pass(self) :: isThiSolveOnSurf
     procedure, public, pass(self) :: isYZCurvSolveOnSurf
     procedure, public, pass(self) :: getSurfCurv
     procedure, public, pass(self) :: getSurfIndex
     procedure, public, pass(self) :: getLastSurf



    end type

    type(lens_data_manager) :: ldm

    contains

    function getLastSurf(self) result(Sf)
        class(lens_data_manager) :: self
        integer :: Sf

        Sf = curr_lens_data%num_surfaces

    end function

    function getSurfThi(self, surfIdx) result(thi)
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: thi

        thi = curr_lens_data%thicknesses(surfIdx+1)

    end function

    function isThiSolveOnSurf(self, surfIdx) result(boolResult)
        use DATLEN, only: SOLVE
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical :: boolResult

        boolResult = .FALSE.
        IF(SOLVE(6,surfIdx).NE.0.0D0) boolResult = .TRUE.

    end function

    function isYZCurvSolveOnSurf(self, surfIdx) result(boolResult)
        use DATLEN, only: SOLVE
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical :: boolResult

        boolResult = .FALSE.
        IF(SOLVE(8,surfIdx).NE.0.0D0) boolResult = .TRUE.

    end function

    function getSurfCurv(self, surfIdx, useXZPlane) result(curv)
        use DATLEN, only: ALENS
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical, optional :: useXZPlane
        real(kind=real64) :: curv

        ! TODO: clean this up, add XZ logic

    !       CHECK FOR X-TORIC. IF FOUND SET CURV=ALENS(24,-)
    !       ELSE SET CURV=ALENS(1,-)
        IF(ALENS(23,surfIdx).EQ.2.0D0) THEN
            curv=ALENS(24,surfIdx)
        ELSE
            IF(ALENS(1,surfIdx).EQ.0.0D0.AND.ALENS(43,surfIdx).NE.0.0D0) THEN
                curv=ALENS(43,surfIdx)*2.0D0
            ELSE
                curv=ALENS(1,surfIdx)
            END IF
        END IF

    end function 
    
    function getSurfIndex(self, surfIdx, lambdaIdx) result(index)        
        use DATLEN, only: ALENS
        class(lens_data_manager) :: self
        integer :: surfIdx
        integer, optional :: lambdaIdx
        real(kind=real64) :: index

        INTEGER :: WWVN

        ! TODO:  CLean up 

        if(present(lambdaIdx) .EQV. .FALSE. ) then
            WWVN = sysConfig%refWavelengthIndex
        else
           if (lambdaIdx.GT.0.AND.lambdaIdx.LT.6) then
            WWVN = lambdaIdx+45 ! From 46-50
           end if
           if (lambdaIdx.GT.5.AND.lambdaIdx.LT.11) then
            WWVN = lambdaIdx+65 ! From 71-75
           end if           
        end if

        index = ALENS(WWVN,surfIdx)

    end function


end module