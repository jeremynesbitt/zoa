submodule (optim_types) optim_types_callbacks

contains

    module function getSPO(self) result(res)
        use DATSPD, only: RMSX, RMSY
        use plot_command_utils, only: getKDPSpotPlotCommand
        use global_widgets, only: sysConfig
        implicit none
        class(merit_entry) :: self
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
        class(merit_entry) :: self
        real(long) :: res

        res = ldm%getEFL()
    end function

    module function getTransverseComaConstraint(self) result(res)
        use mod_analysis_manager
        class(merit_entry) :: self
        real(long) :: res

        res = am%getTransverseComa()
    end function

    module function getSphericalConstraint(self) result(res)
        use mod_analysis_manager
        class(merit_entry) :: self
        real(long) :: res

        res = am%getTransverseSpherical()
    end function

    ! PTZ: Petzval curvature, 1/R of the Petzval surface (lens units^-1).
    module function getPetzvalCurvatureConstraint(self) result(res)
        use mod_analysis_manager
        class(merit_entry) :: self
        real(long) :: res

        res = am%getPetzvalCurvature()
    end function

    ! Paraxial ray trace data (UMY, HCX, IMY, ...) at surface iS (default the
    ! image), wavelength iW (default the reference wavelength).  At the
    ! reference wavelength the values are the lens's own paraxial trace
    ! (PXTRAY / PXTRAX); at another wavelength the same object-space rays are
    ! retraced with that wavelength's indices.
    module function getParaxialRayConstraint(self) result(res)
        use DATLEN, only: PXTRAY, PXTRAX
        use mod_lens_data_manager
        use mod_system, only: sys_wl_ref
        class(merit_entry) :: self
        real(long) :: res
        real(long) :: ray(8)
        integer :: k, lam
        logical :: useX

        k = self%iS
        if (k < 0) k = ldm%getLastSurf()
        lam = self%iW
        if (lam <= 0) lam = int(sys_wl_ref())
        useX = (self%name(3:3) == 'X')

        call paraxialRayAt(k, lam, useX, ray)

        select case (self%name(1:2))
        case ('UM'); res = ray(2)
        case ('HM'); res = ray(1)
        case ('IM'); res = incidentIndex(k, lam)*ray(3)
        case ('UC'); res = ray(6)
        case ('HC'); res = ray(5)
        case ('IC'); res = incidentIndex(k, lam)*ray(7)
        case default; res = 0.0_long
        end select

    contains

        ! Index of the medium the ray arrives from (before surface k).
        real(long) function incidentIndex(k, lam)
            integer, intent(in) :: k, lam
            if (k <= 0) then
                incidentIndex = ldm%getSurfIndex(0, lam)
            else
                incidentIndex = ldm%getSurfIndex(k-1, lam)
            end if
        end function

        ! Marginal (1:4) and chief (5:8) ray at surface k: height, slope,
        ! incidence slope, refracted incidence slope.
        subroutine paraxialRayAt(k, lam, useX, ray)
            use paraxial_ray_trace_test, only: traNextSurf
            integer, intent(in) :: k, lam
            logical, intent(in) :: useX
            real(long), intent(out) :: ray(8)
            real(long) :: p0(8), p1(8)
            integer :: L

            if (useX) then
                p0 = PXTRAX(1:8, 0); p1 = PXTRAX(1:8, 1)
                if (lam == int(sys_wl_ref())) then
                    ray = PXTRAX(1:8, k); return
                end if
            else
                p0 = PXTRAY(1:8, 0); p1 = PXTRAY(1:8, 1)
                if (lam == int(sys_wl_ref())) then
                    ray = PXTRAY(1:8, k); return
                end if
            end if

            if (k == 0) then
                ray = p0; return
            end if
            ! Surface 1: same height as the reference trace (object space is
            ! wavelength independent), refracted at this wavelength.
            ray(1:4) = traNextSurf(p0(1:4), 1, lam, useX, overridePos=p1(1))
            ray(5:8) = traNextSurf(p0(5:8), 1, lam, useX, overridePos=p1(5))
            do L = 2, k
                ray(1:4) = traNextSurf(ray(1:4), L, lam, useX)
                ray(5:8) = traNextSurf(ray(5:8), L, lam, useX)
            end do
        end subroutine

    end function

    module function getTransverseAstigmatismConstraint(self) result(res)
        use mod_analysis_manager
        class(merit_entry) :: self
        real(long) :: res

        res = am%getTransverseAstigmatism()
    end function

    module function getPetzvalBlurConstraint(self) result(res)
        use mod_analysis_manager
        class(merit_entry) :: self
        real(long) :: res

        res = am%getPetzvalBlur()
    end function

    ! IMC: distance from the last real surface to the image plane.  Returns
    ! the RAW distance -- the residual/constraint math subtracts the target
    ! centrally.  (Historically this evaluator subtracted self%targ itself,
    ! and optimizerFunc subtracted it AGAIN, so "IMC > 2" actually enforced
    ! distance >= 2*targ.  Fixed with the weighted-residual rework.)
    module function setDistanceToImagePlaneConstraint(self) result(res)
        use mod_lens_data_manager
        class(merit_entry) :: self
        real(long) :: res

        res = ldm%getSurfThi(ldm%getLastSurf()-1)
    end function

    module function getConstraintTypeAsText(self) result (strType)
        class(merit_entry) :: self
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
