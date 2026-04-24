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
        surf_solve_flag,      set_surf_solve_flag,       &
        surf_anamorphic_flag,   set_surf_anamorphic_flag,  &
        surf_anamorphic_coeff,  set_surf_anamorphic_coeff, &
        surf_anamorphic_conic,  set_surf_anamorphic_conic, &
        surf_label_flag,        set_surf_label_flag,       &
        surf_ret_surf_num,      set_surf_ret_surf_num,     &
        surf_inr_value,         set_surf_inr_value,        &
        surf_tilt_return_flag,  set_surf_tilt_return_flag, &
        surf_global_dx,         set_surf_global_dx,        &
        surf_global_dy,         set_surf_global_dy,        &
        surf_global_dz,         set_surf_global_dz,        &
        surf_global_alpha,      set_surf_global_alpha,     &
        surf_global_beta,       set_surf_global_beta,      &
        surf_global_gamma,      set_surf_global_gamma,     &
        surf_mirror_thickness,  set_surf_mirror_thickness, &
        surf_ideal_efl,         set_surf_ideal_efl,        &
        surf_ccr_flag,          set_surf_ccr_flag,         &
        surf_multi_clap_flag,   set_surf_multi_clap_flag,  &
        surf_multi_cobs_flag,   set_surf_multi_cobs_flag,  &
        surf_array_dx,          set_surf_array_dx,         &
        surf_array_dy,          set_surf_array_dy,         &
        surf_array_parity,      set_surf_array_parity,     &
        surf_dummy_val,         set_surf_dummy_val,        &
        surf_paraxial_val,      set_surf_paraxial_val,     &
        surf_coat_type,         set_surf_coat_type,        &
        surf_default_flag,      set_surf_default_flag,     &
        surf_pivot_axis,        set_surf_pivot_axis,       &
        surf_focus_dx,          set_surf_focus_dx,         &
        surf_focus_dy,          set_surf_focus_dy,         &
        surf_focus_dz,          set_surf_focus_dz,         &
        surf_cobs_poly,         set_surf_cobs_poly,        &
        surf_cobs_ape_type,     set_surf_cobs_ape_type,    &
        surf_cobs_ape_data,     set_surf_cobs_ape_data,    &
        surf_cobs_era_type,     set_surf_cobs_era_type,    &
        surf_cobs_era_data,     set_surf_cobs_era_data,    &
        surf_fict_w,            set_surf_fict_w,           &
        surf_aux88,             set_surf_aux88,            &
        surf_alpha_deg,         set_surf_alpha_deg,        &
        surf_beta_deg,          set_surf_beta_deg,         &
        surf_gamma_deg,         set_surf_gamma_deg,        &
        surf_mtracei_nx,        set_surf_mtracei_nx,       &
        surf_mtracei_ny,        set_surf_mtracei_ny,       &
        surf_psfbin_data,       set_surf_psfbin_data,      &
        surf_profit_data,       set_surf_profit_data,      &
        surf_footblok_flag,     set_surf_footblok_flag,    &
        surf_diffraction_flag,  set_surf_diffraction_flag, &
        surf_grating_spacing,   set_surf_grating_spacing,  &
        surf_coating_index,     set_surf_coating_index,    &
        surf_grating_order,     set_surf_grating_order,    &
        surf_grating_vx,        set_surf_grating_vx,       &
        surf_grating_vy,        set_surf_grating_vy,       &
        surf_grating_vz,        set_surf_grating_vz,       &
        surf_ray_error,         set_surf_ray_error,        &
        surf_focus_flag,        set_surf_focus_flag

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

    ! Anamorphic asphere group (ALENS 36-41)
    integer function surf_anamorphic_flag(s)
        integer, intent(in) :: s
        surf_anamorphic_flag = nint(ALENS(36, s))
    end function

    subroutine set_surf_anamorphic_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(36, s) = real(val, real64)
    end subroutine

    ! order: 4,6,8,10 → ALENS(37,38,39,40); conic → ALENS(41)
    real(real64) function surf_anamorphic_coeff(s, order)
        integer, intent(in) :: s, order
        select case (order)
        case (4);  surf_anamorphic_coeff = ALENS(37, s)
        case (6);  surf_anamorphic_coeff = ALENS(38, s)
        case (8);  surf_anamorphic_coeff = ALENS(39, s)
        case (10); surf_anamorphic_coeff = ALENS(40, s)
        case default; surf_anamorphic_coeff = 0.0_real64
        end select
    end function

    subroutine set_surf_anamorphic_coeff(s, order, val)
        integer, intent(in) :: s, order
        real(real64), intent(in) :: val
        select case (order)
        case (4);  ALENS(37, s) = val
        case (6);  ALENS(38, s) = val
        case (8);  ALENS(39, s) = val
        case (10); ALENS(40, s) = val
        end select
    end subroutine

    real(real64) function surf_anamorphic_conic(s)
        integer, intent(in) :: s
        surf_anamorphic_conic = ALENS(41, s)
    end function

    subroutine set_surf_anamorphic_conic(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(41, s) = val
    end subroutine

    ! Surface label flag (ALENS 44): 0=no label, 1=label exists
    integer function surf_label_flag(s)
        integer, intent(in) :: s
        surf_label_flag = nint(ALENS(44, s))
    end function

    subroutine set_surf_label_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(44, s) = real(val, real64)
    end subroutine

    ! RET surface number for TILT RET (ALENS 70)
    integer function surf_ret_surf_num(s)
        integer, intent(in) :: s
        surf_ret_surf_num = nint(ALENS(70, s))
    end function

    subroutine set_surf_ret_surf_num(s, val)
        integer, intent(in) :: s, val
        ALENS(70, s) = real(val, real64)
    end subroutine

    ! Current INR surface value (ALENS 76)
    real(real64) function surf_inr_value(s)
        integer, intent(in) :: s
        surf_inr_value = ALENS(76, s)
    end function

    subroutine set_surf_inr_value(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(76, s) = val
    end subroutine

    ! Tilt return resolution flag (ALENS 77)
    integer function surf_tilt_return_flag(s)
        integer, intent(in) :: s
        surf_tilt_return_flag = nint(ALENS(77, s))
    end function

    subroutine set_surf_tilt_return_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(77, s) = real(val, real64)
    end subroutine

    ! Global transform group (ALENS 89-96)
    real(real64) function surf_global_dx(s)
        integer, intent(in) :: s
        surf_global_dx = ALENS(90, s)
    end function

    real(real64) function surf_global_dy(s)
        integer, intent(in) :: s
        surf_global_dy = ALENS(91, s)
    end function

    real(real64) function surf_global_dz(s)
        integer, intent(in) :: s
        surf_global_dz = ALENS(92, s)
    end function

    real(real64) function surf_global_alpha(s)
        integer, intent(in) :: s
        surf_global_alpha = ALENS(93, s)
    end function

    real(real64) function surf_global_beta(s)
        integer, intent(in) :: s
        surf_global_beta = ALENS(94, s)
    end function

    real(real64) function surf_global_gamma(s)
        integer, intent(in) :: s
        surf_global_gamma = ALENS(95, s)
    end function

    subroutine set_surf_global_dx(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(90, s) = val
    end subroutine

    subroutine set_surf_global_dy(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(91, s) = val
    end subroutine

    subroutine set_surf_global_dz(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(92, s) = val
    end subroutine

    subroutine set_surf_global_alpha(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(93, s) = val
    end subroutine

    subroutine set_surf_global_beta(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(94, s) = val
    end subroutine

    subroutine set_surf_global_gamma(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(95, s) = val
    end subroutine

    ! Mirror thickness (ALENS 110)
    real(real64) function surf_mirror_thickness(s)
        integer, intent(in) :: s
        surf_mirror_thickness = ALENS(110, s)
    end function

    subroutine set_surf_mirror_thickness(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(110, s) = val
    end subroutine

    ! Focal length of ideal lens (ALENS 121)
    real(real64) function surf_ideal_efl(s)
        integer, intent(in) :: s
        surf_ideal_efl = ALENS(121, s)
    end function

    subroutine set_surf_ideal_efl(s, val)
        integer, intent(in) :: s
        real(real64), intent(in) :: val
        ALENS(121, s) = val
    end subroutine

    ! CCR/roof flag (ALENS 126): 0=none, 1=roof, 2=CCR
    integer function surf_ccr_flag(s)
        integer, intent(in) :: s
        surf_ccr_flag = nint(ALENS(126, s))
    end function

    subroutine set_surf_ccr_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(126, s) = real(val, real64)
    end subroutine

    ! Multi-clap flag (ALENS 127): 0=none, 1=multi-clap
    integer function surf_multi_clap_flag(s)
        integer, intent(in) :: s
        surf_multi_clap_flag = nint(ALENS(127, s))
    end function

    subroutine set_surf_multi_clap_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(127, s) = real(val, real64)
    end subroutine

    ! Multi-cobs flag (ALENS 128): 0=none, 1=multi-cobs
    integer function surf_multi_cobs_flag(s)
        integer, intent(in) :: s
        surf_multi_cobs_flag = nint(ALENS(128, s))
    end function

    subroutine set_surf_multi_cobs_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(128, s) = real(val, real64)
    end subroutine

    ! Array lens parameters (ALENS 131-133)
    real(real64) function surf_array_dx(s)
        integer, intent(in) :: s
        surf_array_dx = ALENS(131, s)
    end function

    real(real64) function surf_array_dy(s)
        integer, intent(in) :: s
        surf_array_dy = ALENS(132, s)
    end function

    ! Array lens parity: -1=odd, 1=even, 0=none (ALENS 133)
    integer function surf_array_parity(s)
        integer, intent(in) :: s
        surf_array_parity = nint(ALENS(133, s))
    end function

    subroutine set_surf_array_dx(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(131, s) = val
    end subroutine

    subroutine set_surf_array_dy(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(132, s) = val
    end subroutine

    subroutine set_surf_array_parity(s, val)
        integer, intent(in) :: s, val
        ALENS(133, s) = real(val, real64)
    end subroutine

    ! Raw dummy flag (ALENS 68): 0=dummy surface, 1=not dummy
    integer function surf_dummy_val(s)
        integer, intent(in) :: s
        surf_dummy_val = nint(ALENS(68, s))
    end function

    subroutine set_surf_dummy_val(s, val)
        integer, intent(in) :: s, val
        ALENS(68, s) = real(val, real64)
    end subroutine

    ! Raw paraxial flag (ALENS 124): 0=real ray trace, 1=paraxial
    integer function surf_paraxial_val(s)
        integer, intent(in) :: s
        surf_paraxial_val = nint(ALENS(124, s))
    end function

    subroutine set_surf_paraxial_val(s, val)
        integer, intent(in) :: s, val
        ALENS(124, s) = real(val, real64)
    end subroutine

    ! Coat type (ALENS 16): 0=none, 1-6 coating type codes
    integer function surf_coat_type(s)
        integer, intent(in) :: s
        surf_coat_type = nint(ALENS(16, s))
    end function

    subroutine set_surf_coat_type(s, val)
        integer, intent(in) :: s, val
        ALENS(16, s) = real(val, real64)
    end subroutine

    ! Default flag (ALENS 103): 1 when surface inherits default parameters
    integer function surf_default_flag(s)
        integer, intent(in) :: s
        surf_default_flag = nint(ALENS(103, s))
    end function

    subroutine set_surf_default_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(103, s) = real(val, real64)
    end subroutine

    ! Pivot axis mode (ALENS 113): 0=vertex, 1=normal
    integer function surf_pivot_axis(s)
        integer, intent(in) :: s
        surf_pivot_axis = nint(ALENS(113, s))
    end function

    subroutine set_surf_pivot_axis(s, val)
        integer, intent(in) :: s, val
        ALENS(113, s) = real(val, real64)
    end subroutine

    ! Focus offset x/y/z (ALENS 114-116)
    real(real64) function surf_focus_dx(s)
        integer, intent(in) :: s
        surf_focus_dx = ALENS(114, s)
    end function

    subroutine set_surf_focus_dx(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(114, s) = val
    end subroutine

    real(real64) function surf_focus_dy(s)
        integer, intent(in) :: s
        surf_focus_dy = ALENS(115, s)
    end function

    subroutine set_surf_focus_dy(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(115, s) = val
    end subroutine

    real(real64) function surf_focus_dz(s)
        integer, intent(in) :: s
        surf_focus_dz = ALENS(116, s)
    end function

    subroutine set_surf_focus_dz(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(116, s) = val
    end subroutine

    ! ── COBS surface parameters ──────────────────────────────────────────────
    ! ALENS(17..22,s): COBS polynomial parameters (n=1..6)
    real(real64) function surf_cobs_poly(s, n)
        integer, intent(in) :: s, n
        surf_cobs_poly = ALENS(16 + n, s)
    end function
    subroutine set_surf_cobs_poly(s, n, val)
        integer, intent(in) :: s, n; real(real64), intent(in) :: val
        ALENS(16 + n, s) = val
    end subroutine

    ! ALENS(51,s): COBS aperture shape type (0=none, 1=erase, 2=recte, 3=elipe, ...)
    integer function surf_cobs_ape_type(s)
        integer, intent(in) :: s
        surf_cobs_ape_type = nint(ALENS(51, s))
    end function
    subroutine set_surf_cobs_ape_type(s, val)
        integer, intent(in) :: s, val
        ALENS(51, s) = real(val, real64)
    end subroutine

    ! ALENS(52..57,s): COBS aperture shape data (n=1..6)
    real(real64) function surf_cobs_ape_data(s, n)
        integer, intent(in) :: s, n
        surf_cobs_ape_data = ALENS(51 + n, s)
    end function
    subroutine set_surf_cobs_ape_data(s, n, val)
        integer, intent(in) :: s, n; real(real64), intent(in) :: val
        ALENS(51 + n, s) = val
    end subroutine

    ! ALENS(61,s): COBS erase/obscuration shape type
    integer function surf_cobs_era_type(s)
        integer, intent(in) :: s
        surf_cobs_era_type = nint(ALENS(61, s))
    end function
    subroutine set_surf_cobs_era_type(s, val)
        integer, intent(in) :: s, val
        ALENS(61, s) = real(val, real64)
    end subroutine

    ! ALENS(62..67,s): COBS erase/obscuration data (n=1..6)
    real(real64) function surf_cobs_era_data(s, n)
        integer, intent(in) :: s, n
        surf_cobs_era_data = ALENS(61 + n, s)
    end function
    subroutine set_surf_cobs_era_data(s, n, val)
        integer, intent(in) :: s, n; real(real64), intent(in) :: val
        ALENS(61 + n, s) = val
    end subroutine

    ! ── Glass model / auxiliary ───────────────────────────────────────────────
    ! ALENS(89,s): third glass model parameter (alongside fict_n/fict_v)
    real(real64) function surf_fict_w(s)
        integer, intent(in) :: s
        surf_fict_w = ALENS(89, s)
    end function
    subroutine set_surf_fict_w(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(89, s) = val
    end subroutine

    ! ALENS(88,s): per-surface auxiliary reset/init flag
    real(real64) function surf_aux88(s)
        integer, intent(in) :: s
        surf_aux88 = ALENS(88, s)
    end function
    subroutine set_surf_aux88(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(88, s) = val
    end subroutine

    ! ── Tilt angles in degrees (separate from radians-based alpha/beta/gamma) ─
    ! ALENS(118,s): alpha tilt angle in degrees
    real(real64) function surf_alpha_deg(s)
        integer, intent(in) :: s
        surf_alpha_deg = ALENS(118, s)
    end function
    subroutine set_surf_alpha_deg(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(118, s) = val
    end subroutine

    ! ALENS(119,s): beta tilt angle in degrees
    real(real64) function surf_beta_deg(s)
        integer, intent(in) :: s
        surf_beta_deg = ALENS(119, s)
    end function
    subroutine set_surf_beta_deg(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(119, s) = val
    end subroutine

    ! ALENS(120,s): gamma tilt angle in degrees
    real(real64) function surf_gamma_deg(s)
        integer, intent(in) :: s
        surf_gamma_deg = ALENS(120, s)
    end function
    subroutine set_surf_gamma_deg(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(120, s) = val
    end subroutine

    ! ── MTRACEI / PSF / PROFIT surface parameters ─────────────────────────────
    ! ALENS(104,s): MTRACEI output — grid size X
    real(real64) function surf_mtracei_nx(s)
        integer, intent(in) :: s
        surf_mtracei_nx = ALENS(104, s)
    end function
    subroutine set_surf_mtracei_nx(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(104, s) = val
    end subroutine

    ! ALENS(105,s): MTRACEI binary irradiance — grid size Y
    real(real64) function surf_mtracei_ny(s)
        integer, intent(in) :: s
        surf_mtracei_ny = ALENS(105, s)
    end function
    subroutine set_surf_mtracei_ny(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(105, s) = val
    end subroutine

    ! ALENS(106,s): PSFBIN data
    real(real64) function surf_psfbin_data(s)
        integer, intent(in) :: s
        surf_psfbin_data = ALENS(106, s)
    end function
    subroutine set_surf_psfbin_data(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(106, s) = val
    end subroutine

    ! ALENS(107,s): PROFIT data (ALENS 107-109 are profit-related)
    real(real64) function surf_profit_data(s)
        integer, intent(in) :: s
        surf_profit_data = ALENS(107, s)
    end function
    subroutine set_surf_profit_data(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(107, s) = val
    end subroutine

    ! ALENS(58,s): FOOTBLOK flag (1=on, 0=off)
    integer function surf_footblok_flag(s)
        integer, intent(in) :: s
        surf_footblok_flag = nint(ALENS(58, s))
    end function
    subroutine set_surf_footblok_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(58, s) = real(val, real64)
    end subroutine

    ! ALENS(96,s): diffraction grating flag (1=active, 0=none)
    integer function surf_diffraction_flag(s)
        integer, intent(in) :: s
        surf_diffraction_flag = nint(ALENS(96, s))
    end function
    subroutine set_surf_diffraction_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(96, s) = real(val, real64)
    end subroutine

    ! ALENS(98,s): grating spacing
    real(real64) function surf_grating_spacing(s)
        integer, intent(in) :: s
        surf_grating_spacing = ALENS(98, s)
    end function
    subroutine set_surf_grating_spacing(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(98, s) = val
    end subroutine

    ! ALENS(112,s): coating table index
    integer function surf_coating_index(s)
        integer, intent(in) :: s
        surf_coating_index = nint(ALENS(112, s))
    end function
    subroutine set_surf_coating_index(s, val)
        integer, intent(in) :: s, val
        ALENS(112, s) = real(val, real64)
    end subroutine

    ! ALENS(97,s): diffraction grating order (GRO)
    real(real64) function surf_grating_order(s)
        integer, intent(in) :: s
        surf_grating_order = ALENS(97, s)
    end function
    subroutine set_surf_grating_order(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(97, s) = val
    end subroutine

    ! ALENS(99,s): grating vector X component (GRX)
    real(real64) function surf_grating_vx(s)
        integer, intent(in) :: s
        surf_grating_vx = ALENS(99, s)
    end function
    subroutine set_surf_grating_vx(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(99, s) = val
    end subroutine

    ! ALENS(100,s): grating vector Y component (GRY)
    real(real64) function surf_grating_vy(s)
        integer, intent(in) :: s
        surf_grating_vy = ALENS(100, s)
    end function
    subroutine set_surf_grating_vy(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(100, s) = val
    end subroutine

    ! ALENS(101,s): grating vector Z component (GRZ)
    real(real64) function surf_grating_vz(s)
        integer, intent(in) :: s
        surf_grating_vz = ALENS(101, s)
    end function
    subroutine set_surf_grating_vz(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(101, s) = val
    end subroutine

    ! ALENS(144,s): ray angular error tolerance (RAYERROR command)
    real(real64) function surf_ray_error(s)
        integer, intent(in) :: s
        surf_ray_error = ALENS(144, s)
    end function
    subroutine set_surf_ray_error(s, val)
        integer, intent(in) :: s; real(real64), intent(in) :: val
        ALENS(144, s) = val
    end subroutine

    ! ALENS(117,s): focus-offset present flag (1 if any of focus_dx/dy/dz != 0)
    integer function surf_focus_flag(s)
        integer, intent(in) :: s
        surf_focus_flag = nint(ALENS(117, s))
    end function
    subroutine set_surf_focus_flag(s, val)
        integer, intent(in) :: s, val
        ALENS(117, s) = real(val, real64)
    end subroutine

end module mod_surface
