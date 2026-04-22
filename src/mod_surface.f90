module mod_surface
    ! Accessor layer for per-surface data stored in ALENS(1:160, 0:MAXSUR).
    ! All reads and writes go through these functions; direct ALENS indexing
    ! should not appear in new code.  Grouped by logical category to mirror
    ! the eventual Option-C derived-type layout.
    use DATLEN, only: ALENS, GLANAM
    use iso_fortran_env, only: real64
    implicit none
    private

    public :: &
        surf_curvature,       set_surf_curvature,       &
        surf_radius,          set_surf_radius,           &
        surf_conic,           set_surf_conic,            &
        surf_thickness,       set_surf_thickness,        &
        surf_is_asphere,      set_surf_asphere_flag,     &
        surf_asphere_coeff,   set_surf_asphere_coeff,    &
        surf_refractive_index, set_surf_refractive_index, &
        surf_glass_name,      set_surf_glass_name,       &
        surf_fict_n,          set_surf_fict_n,           &
        surf_fict_v,          set_surf_fict_v,           &
        surf_clap_type,       set_surf_clap_type,        &
        surf_clap_dim,        set_surf_clap_dim,         &
        surf_clap_tilt,       set_surf_clap_tilt,        &
        surf_tilt_flag,       set_surf_tilt_flag,        &
        surf_alpha,           set_surf_alpha,            &
        surf_beta,            set_surf_beta,             &
        surf_gamma,           set_surf_gamma,            &
        surf_decenter_flag,   set_surf_decenter_flag,    &
        surf_decenter_y,      set_surf_decenter_y,       &
        surf_decenter_x,      set_surf_decenter_x,       &
        surf_decenter_z,      set_surf_decenter_z,       &
        surf_pivot_flag,      set_surf_pivot_flag,       &
        surf_pivot_x,         set_surf_pivot_x,          &
        surf_pivot_y,         set_surf_pivot_y,          &
        surf_pivot_z,         set_surf_pivot_z,          &
        surf_special_type,    set_surf_special_type,     &
        surf_is_dummy,        set_surf_dummy_flag,       &
        surf_is_paraxial,     set_surf_paraxial_flag,    &
        surf_reflection_mode, set_surf_reflection_mode,  &
        surf_asi_flag,        set_surf_asi_flag,         &
        surf_toric_flag,      set_surf_toric_flag,       &
        surf_toric_curvature, set_surf_toric_curvature,  &
        surf_pickup_count,    set_surf_pickup_count,     &
        surf_solve_flag,      set_surf_solve_flag

contains

    ! ── Basic geometry ───────────────────────────────────────────────────────

    real(real64) function surf_curvature(s)
        integer, intent(in) :: s
        surf_curvature = ALENS(1, s)
    end function

    subroutine set_surf_curvature(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(1, s) = val
    end subroutine

    ! Returns radius of curvature; huge(0d0) when curvature is zero (flat).
    real(real64) function surf_radius(s)
        integer, intent(in) :: s
        if (ALENS(1, s) /= 0.0_real64) then
            surf_radius = 1.0_real64 / ALENS(1, s)
        else
            surf_radius = huge(0.0_real64)
        end if
    end function

    ! Stores reciprocal; setting to 0 means flat (infinite radius).
    subroutine set_surf_radius(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        if (val /= 0.0_real64) then
            ALENS(1, s) = 1.0_real64 / val
        else
            ALENS(1, s) = 0.0_real64
        end if
    end subroutine

    real(real64) function surf_conic(s)
        integer, intent(in) :: s
        surf_conic = ALENS(2, s)
    end function

    subroutine set_surf_conic(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(2, s) = val
    end subroutine

    real(real64) function surf_thickness(s)
        integer, intent(in) :: s
        surf_thickness = ALENS(3, s)
    end function

    subroutine set_surf_thickness(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(3, s) = val
    end subroutine

    logical function surf_is_asphere(s)
        integer, intent(in) :: s
        surf_is_asphere = (ALENS(8, s) /= 0.0_real64)
    end function

    subroutine set_surf_asphere_flag(s, val)
        integer, intent(in) :: s
        logical, intent(in) :: val
        ALENS(8, s) = merge(1.0_real64, 0.0_real64, val)
    end subroutine

    ! order = 2 (plano only), 4, 6, 8, 10, 12, 14, 16, 18, 20
    real(real64) function surf_asphere_coeff(s, order)
        integer, intent(in) :: s, order
        select case (order)
        case (2);  surf_asphere_coeff = ALENS(43, s)
        case (4);  surf_asphere_coeff = ALENS(4, s)
        case (6);  surf_asphere_coeff = ALENS(5, s)
        case (8);  surf_asphere_coeff = ALENS(6, s)
        case (10); surf_asphere_coeff = ALENS(7, s)
        case (12); surf_asphere_coeff = ALENS(81, s)
        case (14); surf_asphere_coeff = ALENS(82, s)
        case (16); surf_asphere_coeff = ALENS(83, s)
        case (18); surf_asphere_coeff = ALENS(84, s)
        case (20); surf_asphere_coeff = ALENS(85, s)
        case default; surf_asphere_coeff = 0.0_real64
        end select
    end function

    subroutine set_surf_asphere_coeff(s, order, val)
        integer, intent(in) :: s, order
        real(real64), intent(in) :: val
        select case (order)
        case (2);  ALENS(43, s) = val
        case (4);  ALENS(4, s)  = val
        case (6);  ALENS(5, s)  = val
        case (8);  ALENS(6, s)  = val
        case (10); ALENS(7, s)  = val
        case (12); ALENS(81, s) = val
        case (14); ALENS(82, s) = val
        case (16); ALENS(83, s) = val
        case (18); ALENS(84, s) = val
        case (20); ALENS(85, s) = val
        end select
    end subroutine

    ! ── Glass / refractive index ─────────────────────────────────────────────

    ! wavenum = 1..10
    real(real64) function surf_refractive_index(s, wavenum)
        integer, intent(in) :: s, wavenum
        select case (wavenum)
        case (1);  surf_refractive_index = ALENS(46, s)
        case (2);  surf_refractive_index = ALENS(47, s)
        case (3);  surf_refractive_index = ALENS(48, s)
        case (4);  surf_refractive_index = ALENS(49, s)
        case (5);  surf_refractive_index = ALENS(50, s)
        case (6);  surf_refractive_index = ALENS(71, s)
        case (7);  surf_refractive_index = ALENS(72, s)
        case (8);  surf_refractive_index = ALENS(73, s)
        case (9);  surf_refractive_index = ALENS(74, s)
        case (10); surf_refractive_index = ALENS(75, s)
        case default; surf_refractive_index = 1.0_real64
        end select
    end function

    subroutine set_surf_refractive_index(s, wavenum, val)
        integer, intent(in) :: s, wavenum
        real(real64), intent(in) :: val
        select case (wavenum)
        case (1);  ALENS(46, s) = val
        case (2);  ALENS(47, s) = val
        case (3);  ALENS(48, s) = val
        case (4);  ALENS(49, s) = val
        case (5);  ALENS(50, s) = val
        case (6);  ALENS(71, s) = val
        case (7);  ALENS(72, s) = val
        case (8);  ALENS(73, s) = val
        case (9);  ALENS(74, s) = val
        case (10); ALENS(75, s) = val
        end select
    end subroutine

    ! Primary glass name (GLANAM(s,1)); max 13 characters.
    character(len=13) function surf_glass_name(s)
        integer, intent(in) :: s
        surf_glass_name = GLANAM(s, 1)
    end function

    subroutine set_surf_glass_name(s, name)
        integer, intent(in) :: s
        character(len=*), intent(in) :: name
        GLANAM(s, 1) = name
    end subroutine

    real(real64) function surf_fict_n(s)
        integer, intent(in) :: s
        surf_fict_n = ALENS(86, s)
    end function

    subroutine set_surf_fict_n(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(86, s) = val
    end subroutine

    real(real64) function surf_fict_v(s)
        integer, intent(in) :: s
        surf_fict_v = ALENS(87, s)
    end function

    subroutine set_surf_fict_v(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(87, s) = val
    end subroutine

    ! ── Clear aperture ───────────────────────────────────────────────────────
    ! type: 0=none, 1=circular, 2=rectangular, 3=elliptical,
    !       4=racetrack, 5=regular polygon, 6=irregular polygon

    integer function surf_clap_type(s)
        integer, intent(in) :: s
        surf_clap_type = nint(ALENS(9, s))
    end function

    subroutine set_surf_clap_type(s, val)
        integer, intent(in) :: s, val
        ALENS(9, s) = real(val, real64)
    end subroutine

    ! dim_index 1..5 → ALENS(10..14, s).
    ! Meaning varies by clap_type; see LENSTORE.DOC for per-type layout.
    real(real64) function surf_clap_dim(s, dim_index)
        integer, intent(in) :: s, dim_index
        surf_clap_dim = ALENS(9 + dim_index, s)
    end function

    subroutine set_surf_clap_dim(s, dim_index, val)
        integer, intent(in) :: s, dim_index
        real(real64), intent(in) :: val
        ALENS(9 + dim_index, s) = val
    end subroutine

    real(real64) function surf_clap_tilt(s)
        integer, intent(in) :: s
        surf_clap_tilt = ALENS(15, s)
    end function

    subroutine set_surf_clap_tilt(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(15, s) = val
    end subroutine

    ! ── Tilt / decenter ──────────────────────────────────────────────────────
    ! tilt_flag: 0=none, 1=TILT, -1=RTILT, 2=TILT AUTO, 3=TILT AUTOM,
    !            4=TILT BEN, 5=TILT DAR, 6=TILT RET, 7=TILT REV

    integer function surf_tilt_flag(s)
        integer, intent(in) :: s
        surf_tilt_flag = nint(ALENS(25, s))
    end function

    subroutine set_surf_tilt_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(25, s) = real(val, real64)
    end subroutine

    real(real64) function surf_alpha(s)
        integer, intent(in) :: s
        surf_alpha = ALENS(26, s)
    end function

    subroutine set_surf_alpha(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(26, s) = val
    end subroutine

    real(real64) function surf_beta(s)
        integer, intent(in) :: s
        surf_beta = ALENS(27, s)
    end function

    subroutine set_surf_beta(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(27, s) = val
    end subroutine

    real(real64) function surf_gamma(s)
        integer, intent(in) :: s
        surf_gamma = ALENS(28, s)
    end function

    subroutine set_surf_gamma(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(28, s) = val
    end subroutine

    ! decenter_flag: 0=no decenter, 1=decenter
    integer function surf_decenter_flag(s)
        integer, intent(in) :: s
        surf_decenter_flag = nint(ALENS(29, s))
    end function

    subroutine set_surf_decenter_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(29, s) = real(val, real64)
    end subroutine

    real(real64) function surf_decenter_y(s)
        integer, intent(in) :: s
        surf_decenter_y = ALENS(30, s)
    end function

    subroutine set_surf_decenter_y(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(30, s) = val
    end subroutine

    real(real64) function surf_decenter_x(s)
        integer, intent(in) :: s
        surf_decenter_x = ALENS(31, s)
    end function

    subroutine set_surf_decenter_x(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(31, s) = val
    end subroutine

    ! Z-decenter lives at index 69 (documented separately in LENSTORE.DOC)
    real(real64) function surf_decenter_z(s)
        integer, intent(in) :: s
        surf_decenter_z = ALENS(69, s)
    end function

    subroutine set_surf_decenter_z(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(69, s) = val
    end subroutine

    ! pivot_flag: 0=origin, 1=alternate pivot defined in pivot_x/y/z
    integer function surf_pivot_flag(s)
        integer, intent(in) :: s
        surf_pivot_flag = nint(ALENS(59, s))
    end function

    subroutine set_surf_pivot_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(59, s) = real(val, real64)
    end subroutine

    real(real64) function surf_pivot_x(s)
        integer, intent(in) :: s
        surf_pivot_x = ALENS(78, s)
    end function

    subroutine set_surf_pivot_x(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(78, s) = val
    end subroutine

    real(real64) function surf_pivot_y(s)
        integer, intent(in) :: s
        surf_pivot_y = ALENS(79, s)
    end function

    subroutine set_surf_pivot_y(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(79, s) = val
    end subroutine

    real(real64) function surf_pivot_z(s)
        integer, intent(in) :: s
        surf_pivot_z = ALENS(80, s)
    end function

    subroutine set_surf_pivot_z(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(80, s) = val
    end subroutine

    ! ── Surface type flags ───────────────────────────────────────────────────

    ! Special surface type (1..50); negative means type is disabled/off.
    integer function surf_special_type(s)
        integer, intent(in) :: s
        surf_special_type = nint(ALENS(34, s))
    end function

    subroutine set_surf_special_type(s, val)
        integer, intent(in) :: s, val
        ALENS(34, s) = real(val, real64)
    end subroutine

    ! ALENS(68,s): 0=dummy surface, 1=not dummy (inverted flag in storage)
    logical function surf_is_dummy(s)
        integer, intent(in) :: s
        surf_is_dummy = (ALENS(68, s) == 0.0_real64)
    end function

    subroutine set_surf_dummy_flag(s, is_dummy)
        integer, intent(in) :: s
        logical, intent(in) :: is_dummy
        ALENS(68, s) = merge(0.0_real64, 1.0_real64, is_dummy)
    end subroutine

    logical function surf_is_paraxial(s)
        integer, intent(in) :: s
        surf_is_paraxial = (ALENS(124, s) /= 0.0_real64)
    end function

    subroutine set_surf_paraxial_flag(s, val)
        integer, intent(in) :: s
        logical, intent(in) :: val
        ALENS(124, s) = merge(1.0_real64, 0.0_real64, val)
    end subroutine

    ! reflection_mode: 0=REFL, 1=REFLTIRO, 2=REFLTIR
    integer function surf_reflection_mode(s)
        integer, intent(in) :: s
        surf_reflection_mode = nint(ALENS(125, s))
    end function

    subroutine set_surf_reflection_mode(s, val)
        integer, intent(in) :: s, val
        ALENS(125, s) = real(val, real64)
    end subroutine

    ! asi_flag: 0=no ASI, 1=ASI (alternate surface interface)
    integer function surf_asi_flag(s)
        integer, intent(in) :: s
        surf_asi_flag = nint(ALENS(35, s))
    end function

    subroutine set_surf_asi_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(35, s) = real(val, real64)
    end subroutine

    ! toric_flag: 0=not toric, 1=Y-toric, 2=X-toric
    integer function surf_toric_flag(s)
        integer, intent(in) :: s
        surf_toric_flag = nint(ALENS(23, s))
    end function

    subroutine set_surf_toric_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(23, s) = real(val, real64)
    end subroutine

    real(real64) function surf_toric_curvature(s)
        integer, intent(in) :: s
        surf_toric_curvature = ALENS(24, s)
    end function

    subroutine set_surf_toric_curvature(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(24, s) = val
    end subroutine

    ! ── Pickups / solves ─────────────────────────────────────────────────────

    ! Number of pickups on this surface (0 = none)
    integer function surf_pickup_count(s)
        integer, intent(in) :: s
        surf_pickup_count = nint(ALENS(32, s))
    end function

    subroutine set_surf_pickup_count(s, val)
        integer, intent(in) :: s, val
        ALENS(32, s) = real(val, real64)
    end subroutine

    ! Solve flag is composite real (e.g. 3.3 = YZ+XZ curvature+thickness)
    real(real64) function surf_solve_flag(s)
        integer, intent(in) :: s
        surf_solve_flag = ALENS(33, s)
    end function

    subroutine set_surf_solve_flag(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(33, s) = val
    end subroutine

end module mod_surface
