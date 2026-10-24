submodule (optim_types) optim_types_callbacks

contains

    module function getSPO(self) result(res)
        use DATSPD, only: RMSX, RMSY
        use plot_command_utils, only: getKDPSpotPlotCommand
        use global_widgets, only: sysConfig
        implicit none
        class(merit_entry) :: self
        real(long), dimension(2,sysConfig%numFields) :: rmsxyData
        real(long), dimension(sysConfig%numFields) :: wt
        real(long) :: res
        integer :: i
        integer :: nRect , iLambda

        iLambda = sysConfig%refWavelengthIndex
        nRect = 20

        ! Field weights (WTF), relative: scaled so the largest is 1, which
        ! keeps equal weights exactly the plain average.  A weight-0 field is
        ! not traced.  No positive weight at all -> equal weights.
        wt = max(sysConfig%fieldWeights(1:sysConfig%numFields), 0.0_long)
        if (maxval(wt) > 0.0_long) then
           wt = wt/maxval(wt)
        else
           wt = 1.0_long
        end if

        rmsxyData = 0.0_long
        do i=1,sysConfig%numFields
           if (wt(i) == 0.0_long) cycle
           call PROCESSILENT(trim(getKDPSpotPlotCommand(i, iLambda, ID_SPOT_RECT, nRect, -1, -1)))
           rmsxyData(1,i) = wt(i)*RMSX
           rmsxyData(2,i) = wt(i)*RMSY
        end do

        res = sum(rmsxyData)/(2.0_long*sum(wt))
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

    ! AST3: third-order astigmatism sum, the AST3 SUM that THO prints.
    module function getSeidelAST3Constraint(self) result(res)
        use mod_analysis_manager
        class(merit_entry) :: self
        real(long) :: res

        res = am%getSeidelAST3()
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

    ! CT / ET / SD / ATC / ATE Sk, OAL [Si..j] (CODE V).
    !   CT  center thickness after Sk;  ET  edge thickness of that gap at the
    !   larger of the two semi-diameters (as MNE/MAE);  SD  semi-diameter of
    !   Sk (clear aperture if set, else the ray-traced extent);  ATC / ATE
    !   center / edge thickness divided by the diameter;  OAL  distance from
    !   Si to Sj, the thicknesses Si..j-1 (default S1 to the last surface
    !   before the image, so the image distance is not included).
    module function getSurfaceGeomConstraint(self) result(res)
        use mod_lens_data_manager
        use mod_surface, only: surf_thickness
        class(merit_entry) :: self
        real(long) :: res
        integer :: k, s0, s1, s
        real(long) :: semi

        res = 0.0_long
        k = self%iS
        ! The ray-traced semi-diameters must be current for the current lens.
        select case (trim(self%name))
        case ('SD', 'ET', 'ATC', 'ATE')
            block
                use kdp_data_types, only: check_clear_apertures
                use global_widgets, only: curr_lens_data
                call check_clear_apertures(curr_lens_data, ldm%surfaces)
            end block
        end select
        select case (trim(self%name))
        case ('OAL')
            ! Thicknesses Si .. Sj-1 (Sj's own thickness not included).
            s0 = merge(self%iS, 1, self%iS >= 0)
            s1 = merge(self%iS2, ldm%getLastSurf() - 1, self%iS2 >= 0)
            do s = s0, s1 - 1
                res = res + surf_thickness(s)
            end do
        case ('CT')
            res = surf_thickness(k)
        case ('SD')
            res = semiDia(k)
        case ('ET', 'ATC', 'ATE')
            semi = max(semiDia(k), semiDia(k+1))
            if (trim(self%name) == 'ATC') then
                res = surf_thickness(k)
            else
                res = ldm%edge_thickness(k, semi)
            end if
            if (trim(self%name) /= 'ET') then
                if (semi > 0.0_long) then
                    res = res/(2.0_long*semi)
                else
                    res = 0.0_long
                end if
            end if
        end select

    contains

        ! An assigned clear aperture (its Y semi-width, read live: the typed
        ! copy is not refreshed after CIR/REX/...), else the ray-traced extent.
        real(long) function semiDia(s)
            use mod_surface, only: surf_clap_type, surf_clap_dim
            integer, intent(in) :: s
            semiDia = 0.0_long
            if (surf_clap_type(s) >= 1 .and. surf_clap_type(s) <= 3) then
                semiDia = surf_clap_dim(s, 1)
                return
            end if
            if (.not. allocated(ldm%surfaces)) return
            if (s < lbound(ldm%surfaces,1) .or. s > ubound(ldm%surfaces,1)) return
            if (.not. allocated(ldm%surfaces(s)%s)) return
            semiDia = ldm%surfaces(s)%s%clap%display_semi_y()
        end function
    end function

    ! EFY / EFX [Si..j], TT, ENP, EXP (CODE V, paraxial).
    !   EFY  effective focal length of the whole system, or of surfaces
    !        Si..j (an axial ray traced through the group alone, reference
    !        wavelength: -1/(n'u') for unit height); EFX the same in X-Z
    !        (equal to EFY: torics are not traced);
    !   TT   total track, object to image;  ENP  entrance pupil position from
    !   S1;  EXP  exit pupil position from the last surface before the image.
    module function getFirstOrderConstraint(self) result(res)
        use global_widgets, only: curr_par_ray_trace
        use mod_lens_data_manager
        use mod_surface, only: surf_curvature, surf_thickness
        use mod_system, only: sys_wl_ref
        class(merit_entry) :: self
        real(long) :: res
        integer :: s, s0, s1, lam
        real(long) :: y, nu, n, np

        res = 0.0_long
        select case (trim(self%name))
        case ('TT');  res = curr_par_ray_trace%TT
        case ('ENP'); res = curr_par_ray_trace%ENPUPPOS
        case ('EXP'); res = curr_par_ray_trace%EXPUPPOS
        case ('EFY', 'EFX')
            if (self%iS < 0) then
                res = curr_par_ray_trace%EFL
                return
            end if
            s0 = self%iS
            s1 = merge(self%iS2, ldm%getLastSurf() - 1, self%iS2 >= 0)
            lam = int(sys_wl_ref())
            y = 1.0_long
            nu = 0.0_long
            do s = s0, s1
                n  = ldm%getSurfIndex(s-1, lam)
                np = ldm%getSurfIndex(s, lam)
                nu = nu - y*surf_curvature(s)*(np - n)
                if (s < s1) y = y + surf_thickness(s)*nu/np
            end do
            if (nu /= 0.0_long) then
                res = -1.0_long/nu
            else
                res = 1.0e20_long        ! afocal group
            end if
        end select
    end function

end submodule optim_types_callbacks
