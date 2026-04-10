submodule (optim_types) optim_types_callbacks

contains

    module function getSPO(self) result(res)
        use DATSPD, only: RMSX, RMSY
        use plot_functions,only: getKDPSpotPlotCommand
        use global_widgets, only: sysConfig
        implicit none
        class(operand) :: self
        real(long), dimension(2,sysConfig%numFields) :: rmsxyData
        real(long) :: res
        integer :: i
        integer :: nRect , iLambda

        iLambda = sysConfig%refWavelengthIndex
        nRect = 20

        do i=1,sysConfig%numFields
           call PROCESSILENT(trim(getKDPSpotPlotCommand(i, iLambda, ID_SPOT_RECT, nRect, -1, -1)))
           rmsxyData(1,i) = RMSX
           rmsxyData(2,i) = RMSY
        end do

        res = sum(rmsxyData)/size(rmsxyData)
    end function

    module function getEFLConstraint(self) result(res)
        use mod_lens_data_manager
        class(constraint) :: self
        real(long) :: res

        res = ldm%getEFL()
    end function

    module function getTransverseComaConstraint(self) result(res)
        use mod_analysis_manager
        class(constraint) :: self
        real(long) :: res

        res = am%getTransverseComa()
    end function

    module function getSphericalConstraint(self) result(res)
        use mod_analysis_manager
        class(constraint) :: self
        real(long) :: res

        res = am%getTransverseSpherical()
    end function

    module function getTransverseAstigmatismConstraint(self) result(res)
        use mod_analysis_manager
        class(constraint) :: self
        real(long) :: res

        res = am%getTransverseAstigmatism()
    end function

    module function getPetzvalBlurConstraint(self) result(res)
        use mod_analysis_manager
        class(constraint) :: self
        real(long) :: res

        res = am%getPetzvalBlur()
    end function

    module function setDistanceToImagePlaneConstraint(self) result(res)
        use mod_lens_data_manager
        class(constraint) :: self
        real(long) :: res

        res = ldm%getSurfThi(ldm%getLastSurf()-1)

        if (self%conType == ID_CON_EXACT .or. self%conType == ID_CON_GREATER_THAN) then
           res = res - self%targ
        else
            res = res - self%targ
        end if
    end function

    module function getConstraintTypeAsText(self) result (strType)
        class(constraint) :: self
        character(len=1) :: strType

        select case (self%conType)
            case(ID_CON_EXACT)
                strType = '='
            case(ID_CON_GREATER_THAN)
                strType = '>'
            case(ID_CON_LESS_THAN)
                strType = '<'
        end select
    end function

end submodule optim_types_callbacks
