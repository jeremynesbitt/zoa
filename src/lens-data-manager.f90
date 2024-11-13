module mod_lens_data_manager
    use iso_fortran_env, only: real64
    use global_widgets, only: curr_lens_data, curr_par_ray_trace
    use global_widgets, only: sysConfig    

    type lens_data_manager
   
    contains
     procedure, public, pass(self) :: getSurfThi
     procedure, public, pass(self) :: isThiSolveOnSurf
     procedure, public, pass(self) :: isYZCurvSolveOnSurf
     procedure, public, pass(self) :: getSurfCurv
     procedure, public, pass(self) :: getSurfIndex
     procedure, public, pass(self) :: getLastSurf
     procedure, public, pass(self) :: getEFL
     procedure, public, pass(self) :: getTrackLength
     procedure :: getCurrentConfig
     procedure :: getSurfName
     procedure :: getStopSurf
     procedure :: isGlassSurf
     procedure :: getGlassName
     procedure :: updateThiOptimVars
     procedure :: updateCurvOptimVars



    end type

    type(lens_data_manager) :: ldm

    contains

    function getGlassName(self, idx) result(strGlassName)
        use DATLEN, only: GLANAM
        class(lens_data_manager) :: self
        integer :: idx
        character(len=30) :: strGlassName

        strGlassName = trim(GLANAM(idx,2))//'_'//trim(GLANAM(idx,1))

    end function

    function isGlassSurf(self, idx) result(boolResult)
        use DATLEN, only: GLANAM
        class(lens_data_manager) :: self
        integer :: idx
        logical :: boolResult

        boolResult = .TRUE.
        if (GLANAM(idx,2) == 'AIR') boolResult = .FALSE.
        if (GLANAM(idx,2).EQ.'LAST SURFACE') boolResult = .FALSE.

    end function

    function getStopSurf(self) result(iStop)
        class(lens_data_manager) :: self
        integer :: iStop

        iStop = curr_lens_data%ref_stop


    end function

    function getSurfName(self, idx) result(strName)
        class(lens_data_manager) :: self
        character(len=3) :: strName

        write(strName, '(I0.3)')  idx
        ! Handle Special surfaces
        if (idx==0) strName = 'OBJ'
        if (idx==self%getStopSurf()) strName = 'STO'
        if (idx==self%getLastSurf()) strName = 'IMG'

    end function

    function getCurrentConfig(self) result(cfg)
        class(lens_data_manager) :: self
        integer :: cfg

        cfg = 1 ! TODO:  Update when configs are fully supported

    end function

    function getLastSurf(self) result(Sf)
        class(lens_data_manager) :: self
        integer :: Sf

        Sf = curr_lens_data%num_surfaces-1

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

    function getEFL(self) result(EFL)
        implicit none
        class(lens_data_manager) :: self
        real(kind=real64) :: EFL

        EFL = curr_par_ray_trace%EFL

    end function

    function getTrackLength(self) result (OAL)
        implicit none
        class(lens_data_manager) :: self
        real(kind=real64) :: OAL

        OAL = curr_par_ray_trace%OAL


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

    subroutine updateThiOptimVars(self, s0, sf, intCode)
        use type_utils
        !use optim_types
        class(lens_data_manager) :: self
        integer, intent(in) :: s0, sf, intCode
        integer :: i

        select case (intCode)

        case(0) ! Make Variable
            if (s0==sf) then
                CALL PROCESKDP('UPDATE VARIABLE ; TH, '//trim(int2str(s0))//'; EOS ')
            else
                call PROCESKDP('UPDATE VARIABLE')
                do i=s0,sf
                    CALL PROCESKDP('TH, '//trim(int2str(i)))
                end do
                call PROCESKDP('EOS')
            end if

        end select


        ! New Code
        ! select case (intCode)

        ! case(0) ! Make Variable
        !     if (s0==sf) then
        !         call addOptimVariable(s0,VAR_THI)
        !         !CALL PROCESKDP('UPDATE VARIABLE ; TH, '//trim(int2str(s0))//'; EOS ')
        !     else
        !         !call PROCESKDP('UPDATE VARIABLE')
        !         do i=s0,sf
        !             call addOptimVariable(i,VAR_THI)
        !             !CALL PROCESKDP('TH, '//trim(int2str(i)))
        !         end do
        !         !call PROCESKDP('EOS')
        !     end if

        ! end select

    end subroutine

    !TODO:  Refactor with updateThiOptimVars
    subroutine updateCurvOptimVars(self, s0, sf, intCode)
        use type_utils
        !use optim_types
        class(lens_data_manager) :: self
        integer, intent(in) :: s0, sf, intCode
        integer :: i

        select case (intCode)

        case(0) ! Make Variable
            if (s0==sf) then
                CALL PROCESKDP('UPDATE VARIABLE ; CV, '//trim(int2str(s0))//'; EOS ')
            else
                call PROCESKDP('UPDATE VARIABLE')
                do i=s0,sf
                    CALL PROCESKDP('CV, '//trim(int2str(i)))
                end do
                call PROCESKDP('EOS')
            end if

        end select

        ! New Code
        !select case (intCode)

        ! case(0) ! Make Variable
        !     if (s0==sf) then
        !         call addOptimVariable(s0,VAR_CURV)
        !         !CALL PROCESKDP('UPDATE VARIABLE ; TH, '//trim(int2str(s0))//'; EOS ')
        !     else
        !         !call PROCESKDP('UPDATE VARIABLE')
        !         do i=s0,sf
        !             call addOptimVariable(i,VAR_CURV)
        !             !CALL PROCESKDP('TH, '//trim(int2str(i)))
        !         end do
        !         !call PROCESKDP('EOS')
        !     end if

        ! end select


    end subroutine    


end module