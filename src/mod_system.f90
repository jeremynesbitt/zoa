module mod_system
    ! Accessor layer for system-level data stored in SYSTEM(1:150).
    ! All reads and writes should go through these functions; direct SYSTEM
    ! indexing should not appear in new code.
    !
    ! Source of truth: Library/OPTMAN/LENSTORE.DOC
    use DATLEN, only: SYSTEM
    use iso_fortran_env, only: real64
    implicit none
    private

    public :: &
        sys_last_surf,      set_sys_last_surf,      &
        sys_units,          set_sys_units,           &
        sys_wl_ref,         set_sys_wl_ref,          &
        sys_wl_pri1,        set_sys_wl_pri1,         &
        sys_wl_pri2,        set_sys_wl_pri2,         &
        sys_wl_sec1,        set_sys_wl_sec1,         &
        sys_wl_sec2,        set_sys_wl_sec2,         &
        sys_wavelength,     set_sys_wavelength,      &
        sys_wl_weight,      set_sys_wl_weight,       &
        sys_wv,             set_sys_wv,              &
        sys_say,            set_sys_say,             &
        sys_sax,            set_sys_sax,             &
        sys_scy,            set_sys_scy,             &
        sys_scy_y1,         set_sys_scy_y1,          &
        sys_scx,            set_sys_scx,             &
        sys_scx_x1,         set_sys_scx_x1,          &
        sys_scy_fang_set,   set_sys_scy_fang_set,    &
        sys_scx_fang_set,   set_sys_scx_fang_set,    &
        sys_fang_y,         set_sys_fang_y,          &
        sys_fang_y_y1,      set_sys_fang_y_y1,       &
        sys_fang_x,         set_sys_fang_x,          &
        sys_fang_x_x1,      set_sys_fang_x_x1,       &
        sys_ref_surf,       set_sys_ref_surf,        &
        sys_astop,          set_sys_astop,           &
        sys_astop_adj,      set_sys_astop_adj,       &
        sys_xz_sym,         set_sys_xz_sym,          &
        sys_yz_sym,         set_sys_yz_sym,          &
        sys_gen_parax,      set_sys_gen_parax,       &
        sys_gen_parax_trace,set_sys_gen_parax_trace, &
        sys_mode,           set_sys_mode,            &
        sys_yz_mag,         set_sys_yz_mag,          &
        sys_xz_mag,         set_sys_xz_mag,          &
        sys_yz_des_mag,     set_sys_yz_des_mag,      &
        sys_xz_des_mag,     set_sys_xz_des_mag,      &
        sys_mag_beg_y,      set_sys_mag_beg_y,       &
        sys_mag_beg_x,      set_sys_mag_beg_x,       &
        sys_mag_end_y,      set_sys_mag_end_y,       &
        sys_mag_end_x,      set_sys_mag_end_x,       &
        sys_fno_hold_y,     set_sys_fno_hold_y,      &
        sys_fno_hold_x,     set_sys_fno_hold_x,      &
        sys_fno_val_y,      set_sys_fno_val_y,       &
        sys_fno_val_x,      set_sys_fno_val_x,       &
        sys_xz_data,        set_sys_xz_data,         &
        sys_curr_cfg,                                &
        sys_scy_y1_set,     set_sys_scy_y1_set,      &
        sys_scx_x1_set,     set_sys_scx_x1_set,      &
        sys_fang_y_y1_set,  set_sys_fang_y_y1_set,   &
        sys_fang_x_x1_set,  set_sys_fang_x_x1_set,   &
        sys_high_cfg,       set_sys_high_cfg,        &
        sys_ref_orient,     set_sys_ref_orient,      &
        sys_scy_explicit,   set_sys_scy_explicit,    &
        sys_scx_explicit,   set_sys_scx_explicit,    &
        sys_ray_aiming,     set_sys_ray_aiming,      &
        sys_telecentric,    set_sys_telecentric,     &
        sys_nao_flag,       set_sys_nao_flag,        &
        sys_nao_y,          set_sys_nao_y,           &
        sys_nao_x,          set_sys_nao_x,           &
        sys_fno_flag,       set_sys_fno_flag,        &
        sys_fno_y,          set_sys_fno_y,           &
        sys_fno_x,          set_sys_fno_x,           &
        sys_aplanatic,      set_sys_aplanatic,       &
        sys_aim_offset_x,   set_sys_aim_offset_x,    &
        sys_aim_offset_y,   set_sys_aim_offset_y,    &
        sys_say_float,      set_sys_say_float,       &
        sys_sax_float,      set_sys_sax_float,       &
        sys_wrx,            set_sys_wrx,             &
        sys_wry,            set_sys_wry,             &
        sys_bdx,            set_sys_bdx,             &
        sys_bdy,            set_sys_bdy,             &
        sys_aim_offset_z,   set_sys_aim_offset_z,    &
        sys_tilt_ret,       set_sys_tilt_ret,        &
        sys_autofunc,       set_sys_autofunc,        &
        sys_pxim,           set_sys_pxim,            &
        sys_pyim,           set_sys_pyim,            &
        sys_pxim_fang_set,  set_sys_pxim_fang_set,   &
        sys_pyim_fang_set,  set_sys_pyim_fang_set,   &
        sys_rxim,           set_sys_rxim,            &
        sys_ryim,           set_sys_ryim,            &
        sys_rxim_fang_set,  set_sys_rxim_fang_set,   &
        sys_ryim_fang_set,  set_sys_ryim_fang_set,   &
        sys_reverse,        set_sys_reverse,         &
        sys_verbose_optim,  set_sys_verbose_optim,   &
        sys_nss,            set_sys_nss,             &
        sys_screen,         set_sys_screen,          &
        sys_screen_surf,    set_sys_screen_surf,     &
        sys_screen_d,       set_sys_screen_d,        &
        sys_screen_h,       set_sys_screen_h,        &
        sys_screen_s,       set_sys_screen_s,        &
        sys_pixel_nx,       set_sys_pixel_nx,        &
        sys_pixel_ny,       set_sys_pixel_ny,        &
        sys_pixel_pitch_x,  set_sys_pixel_pitch_x,   &
        sys_pixel_pitch_y,  set_sys_pixel_pitch_y,   &
        sys_det_center_x,   set_sys_det_center_x,    &
        sys_det_center_y,   set_sys_det_center_y,    &
        sys_spot_surf,      set_sys_spot_surf,       &
        sys_flip_ref_x,     set_sys_flip_ref_x,      &
        sys_flip_ref_y,     set_sys_flip_ref_y

contains

    ! ── Surface count ────────────────────────────────────────────────────────

    ! Number of the last (image) surface.
    integer function sys_last_surf()
        sys_last_surf = nint(SYSTEM(20))
    end function

    subroutine set_sys_last_surf(val)
        integer, intent(in) :: val
        SYSTEM(20) = real(val, real64)
    end subroutine

    ! ── Units ─────────────────────────────────────────────────────────────────

    ! Units identifier: 1=inches, 2=cm, 3=mm, 4=m.
    integer function sys_units()
        sys_units = nint(SYSTEM(6))
    end function

    subroutine set_sys_units(val)
        integer, intent(in) :: val
        SYSTEM(6) = real(val, real64)
    end subroutine

    ! ── Wavelengths ───────────────────────────────────────────────────────────

    ! Reference (control) wavelength number (1-based).
    integer function sys_wl_ref()
        sys_wl_ref = nint(SYSTEM(11))
    end function

    subroutine set_sys_wl_ref(val)
        integer, intent(in) :: val
        SYSTEM(11) = real(val, real64)
    end subroutine

    ! First wavelength of the primary pair.
    integer function sys_wl_pri1()
        sys_wl_pri1 = nint(SYSTEM(7))
    end function

    subroutine set_sys_wl_pri1(val)
        integer, intent(in) :: val
        SYSTEM(7) = real(val, real64)
    end subroutine

    ! Second wavelength of the primary pair.
    integer function sys_wl_pri2()
        sys_wl_pri2 = nint(SYSTEM(8))
    end function

    subroutine set_sys_wl_pri2(val)
        integer, intent(in) :: val
        SYSTEM(8) = real(val, real64)
    end subroutine

    ! First wavelength of the secondary pair.
    integer function sys_wl_sec1()
        sys_wl_sec1 = nint(SYSTEM(9))
    end function

    subroutine set_sys_wl_sec1(val)
        integer, intent(in) :: val
        SYSTEM(9) = real(val, real64)
    end subroutine

    ! Second wavelength of the secondary pair.
    integer function sys_wl_sec2()
        sys_wl_sec2 = nint(SYSTEM(10))
    end function

    subroutine set_sys_wl_sec2(val)
        integer, intent(in) :: val
        SYSTEM(10) = real(val, real64)
    end subroutine

    ! Wavelength value in microns; i = 1..10.
    ! i=1..5 stored at SYSTEM(i); i=6..10 stored at SYSTEM(65+i).
    real(real64) function sys_wavelength(i)
        integer, intent(in) :: i
        if (i <= 5) then
            sys_wavelength = SYSTEM(i)
        else
            sys_wavelength = SYSTEM(65 + i)
        end if
    end function

    subroutine set_sys_wavelength(i, val)
        integer, intent(in) :: i
        real(real64), intent(in) :: val
        if (i <= 5) then
            SYSTEM(i) = val
        else
            SYSTEM(65 + i) = val
        end if
    end subroutine

    ! Spectral weighting factor; i = 1..10.
    ! i=1..5 → SYSTEM(30+i); i=6..10 → SYSTEM(70+i).
    real(real64) function sys_wl_weight(i)
        integer, intent(in) :: i
        if (i <= 5) then
            sys_wl_weight = SYSTEM(30 + i)
        else
            sys_wl_weight = SYSTEM(70 + i)
        end if
    end function

    subroutine set_sys_wl_weight(i, val)
        integer, intent(in) :: i
        real(real64), intent(in) :: val
        if (i <= 5) then
            SYSTEM(30 + i) = val
        else
            SYSTEM(70 + i) = val
        end if
    end subroutine

    ! WV (alternate wavelength storage) value in microns; i = 1..10 → SYSTEM(110+i).
    real(real64) function sys_wv(i)
        integer, intent(in) :: i
        sys_wv = SYSTEM(110 + i)
    end function

    subroutine set_sys_wv(i, val)
        integer, intent(in) :: i
        real(real64), intent(in) :: val
        SYSTEM(110 + i) = val
    end subroutine

    ! ── Aperture ──────────────────────────────────────────────────────────────

    ! Semi-aperture Y (SAY).
    real(real64) function sys_say()
        sys_say = SYSTEM(12)
    end function

    subroutine set_sys_say(val)
        real(real64), intent(in) :: val
        SYSTEM(12) = val
    end subroutine

    ! Semi-aperture X (SAX).
    real(real64) function sys_sax()
        sys_sax = SYSTEM(13)
    end function

    subroutine set_sys_sax(val)
        real(real64), intent(in) :: val
        SYSTEM(13) = val
    end subroutine

    ! 1 = SAY allowed to float, 0 = fixed.
    integer function sys_say_float()
        sys_say_float = nint(SYSTEM(83))
    end function

    subroutine set_sys_say_float(val)
        integer, intent(in) :: val
        SYSTEM(83) = real(val, real64)
    end subroutine

    ! 1 = SAX allowed to float, 0 = fixed.
    integer function sys_sax_float()
        sys_sax_float = nint(SYSTEM(84))
    end function

    subroutine set_sys_sax_float(val)
        integer, intent(in) :: val
        SYSTEM(84) = real(val, real64)
    end subroutine

    ! ── Field ─────────────────────────────────────────────────────────────────

    ! Semi-field Y (SCY).
    real(real64) function sys_scy()
        sys_scy = SYSTEM(14)
    end function

    subroutine set_sys_scy(val)
        real(real64), intent(in) :: val
        SYSTEM(14) = val
    end subroutine

    ! Y1 companion value for SCY.
    real(real64) function sys_scy_y1()
        sys_scy_y1 = SYSTEM(15)
    end function

    subroutine set_sys_scy_y1(val)
        real(real64), intent(in) :: val
        SYSTEM(15) = val
    end subroutine

    ! Semi-field X (SCX).
    real(real64) function sys_scx()
        sys_scx = SYSTEM(16)
    end function

    subroutine set_sys_scx(val)
        real(real64), intent(in) :: val
        SYSTEM(16) = val
    end subroutine

    ! X1 companion value for SCX.
    real(real64) function sys_scx_x1()
        sys_scx_x1 = SYSTEM(17)
    end function

    subroutine set_sys_scx_x1(val)
        real(real64), intent(in) :: val
        SYSTEM(17) = val
    end subroutine

    ! 1.0 if SCY FANG was explicitly set, else 0.0.
    integer function sys_scy_fang_set()
        sys_scy_fang_set = nint(SYSTEM(18))
    end function

    subroutine set_sys_scy_fang_set(val)
        integer, intent(in) :: val
        SYSTEM(18) = real(val, real64)
    end subroutine

    ! 1.0 if SCX FANG was explicitly set, else 0.0.
    integer function sys_scx_fang_set()
        sys_scx_fang_set = nint(SYSTEM(19))
    end function

    subroutine set_sys_scx_fang_set(val)
        integer, intent(in) :: val
        SYSTEM(19) = real(val, real64)
    end subroutine

    ! SCY FANG value.
    real(real64) function sys_fang_y()
        sys_fang_y = SYSTEM(21)
    end function

    subroutine set_sys_fang_y(val)
        real(real64), intent(in) :: val
        SYSTEM(21) = val
    end subroutine

    ! Y1 companion value for SCY FANG.
    real(real64) function sys_fang_y_y1()
        sys_fang_y_y1 = SYSTEM(22)
    end function

    subroutine set_sys_fang_y_y1(val)
        real(real64), intent(in) :: val
        SYSTEM(22) = val
    end subroutine

    ! SCX FANG value.
    real(real64) function sys_fang_x()
        sys_fang_x = SYSTEM(23)
    end function

    subroutine set_sys_fang_x(val)
        real(real64), intent(in) :: val
        SYSTEM(23) = val
    end subroutine

    ! X1 companion value for SCX FANG.
    real(real64) function sys_fang_x_x1()
        sys_fang_x_x1 = SYSTEM(24)
    end function

    subroutine set_sys_fang_x_x1(val)
        real(real64), intent(in) :: val
        SYSTEM(24) = val
    end subroutine

    ! 1 = SCY or SCY FANG explicitly set, 0 = not.
    integer function sys_scy_explicit()
        sys_scy_explicit = nint(SYSTEM(60))
    end function

    subroutine set_sys_scy_explicit(val)
        integer, intent(in) :: val
        SYSTEM(60) = real(val, real64)
    end subroutine

    ! 1 = SCX or SCX FANG explicitly set, 0 = not.
    integer function sys_scx_explicit()
        sys_scx_explicit = nint(SYSTEM(61))
    end function

    subroutine set_sys_scx_explicit(val)
        integer, intent(in) :: val
        SYSTEM(61) = real(val, real64)
    end subroutine

    ! 1 = Y1(SCY) explicitly set, 0 = not.
    integer function sys_scy_y1_set()
        sys_scy_y1_set = nint(SYSTEM(51))
    end function

    subroutine set_sys_scy_y1_set(val)
        integer, intent(in) :: val
        SYSTEM(51) = real(val, real64)
    end subroutine

    ! 1 = X1(SCX) explicitly set, 0 = not.
    integer function sys_scx_x1_set()
        sys_scx_x1_set = nint(SYSTEM(52))
    end function

    subroutine set_sys_scx_x1_set(val)
        integer, intent(in) :: val
        SYSTEM(52) = real(val, real64)
    end subroutine

    ! 1 = Y1(SCY FANG) explicitly set, 0 = not.
    integer function sys_fang_y_y1_set()
        sys_fang_y_y1_set = nint(SYSTEM(53))
    end function

    subroutine set_sys_fang_y_y1_set(val)
        integer, intent(in) :: val
        SYSTEM(53) = real(val, real64)
    end subroutine

    ! 1 = X1(SCX FANG) explicitly set, 0 = not.
    integer function sys_fang_x_x1_set()
        sys_fang_x_x1_set = nint(SYSTEM(54))
    end function

    subroutine set_sys_fang_x_x1_set(val)
        integer, intent(in) :: val
        SYSTEM(54) = real(val, real64)
    end subroutine

    ! ── Surfaces ──────────────────────────────────────────────────────────────

    ! Reference surface number (default = 1).
    integer function sys_ref_surf()
        sys_ref_surf = nint(SYSTEM(25))
    end function

    subroutine set_sys_ref_surf(val)
        integer, intent(in) :: val
        SYSTEM(25) = real(val, real64)
    end subroutine

    ! Aperture stop surface number (-99 if none defined).
    integer function sys_astop()
        sys_astop = nint(SYSTEM(26))
    end function

    subroutine set_sys_astop(val)
        integer, intent(in) :: val
        SYSTEM(26) = real(val, real64)
    end subroutine

    ! ASTOP adjustment flag.
    integer function sys_astop_adj()
        sys_astop_adj = nint(SYSTEM(27))
    end function

    subroutine set_sys_astop_adj(val)
        integer, intent(in) :: val
        SYSTEM(27) = real(val, real64)
    end subroutine

    ! ── System mode / symmetry ────────────────────────────────────────────────

    ! Mode: 1=FOCAL, 2=UFOCAL, 3=AFOCAL, 4=UAFOCAL.
    integer function sys_mode()
        sys_mode = nint(SYSTEM(30))
    end function

    subroutine set_sys_mode(val)
        integer, intent(in) :: val
        SYSTEM(30) = real(val, real64)
    end subroutine

    ! XZ bilateral symmetry flag (1=yes, 0=no).
    integer function sys_xz_sym()
        sys_xz_sym = nint(SYSTEM(28))
    end function

    subroutine set_sys_xz_sym(val)
        integer, intent(in) :: val
        SYSTEM(28) = real(val, real64)
    end subroutine

    ! YZ bilateral symmetry flag (1=yes, 0=no).
    integer function sys_yz_sym()
        sys_yz_sym = nint(SYSTEM(48))
    end function

    subroutine set_sys_yz_sym(val)
        integer, intent(in) :: val
        SYSTEM(48) = real(val, real64)
    end subroutine

    ! XZ data entered flag: 0=none, 1=aperture only, 2=field only, 3=both.
    integer function sys_xz_data()
        sys_xz_data = nint(SYSTEM(49))
    end function

    subroutine set_sys_xz_data(val)
        integer, intent(in) :: val
        SYSTEM(49) = real(val, real64)
    end subroutine

    ! Generalized paraxial ray trace flag (1=on, 0=off).
    integer function sys_gen_parax()
        sys_gen_parax = nint(SYSTEM(29))
    end function

    subroutine set_sys_gen_parax(val)
        integer, intent(in) :: val
        SYSTEM(29) = real(val, real64)
    end subroutine

    ! Generalized paraxial trace flag (0=off, 1=on) — SYSTEM(58).
    integer function sys_gen_parax_trace()
        sys_gen_parax_trace = nint(SYSTEM(58))
    end function

    subroutine set_sys_gen_parax_trace(val)
        integer, intent(in) :: val
        SYSTEM(58) = real(val, real64)
    end subroutine

    ! ── Magnification / F/# ───────────────────────────────────────────────────

    real(real64) function sys_yz_mag()
        sys_yz_mag = SYSTEM(36)
    end function

    subroutine set_sys_yz_mag(val)
        real(real64), intent(in) :: val
        SYSTEM(36) = val
    end subroutine

    real(real64) function sys_xz_mag()
        sys_xz_mag = SYSTEM(37)
    end function

    subroutine set_sys_xz_mag(val)
        real(real64), intent(in) :: val
        SYSTEM(37) = val
    end subroutine

    real(real64) function sys_yz_des_mag()
        sys_yz_des_mag = SYSTEM(38)
    end function

    subroutine set_sys_yz_des_mag(val)
        real(real64), intent(in) :: val
        SYSTEM(38) = val
    end subroutine

    real(real64) function sys_xz_des_mag()
        sys_xz_des_mag = SYSTEM(39)
    end function

    subroutine set_sys_xz_des_mag(val)
        real(real64), intent(in) :: val
        SYSTEM(39) = val
    end subroutine

    real(real64) function sys_mag_beg_y()
        sys_mag_beg_y = SYSTEM(40)
    end function

    subroutine set_sys_mag_beg_y(val)
        real(real64), intent(in) :: val
        SYSTEM(40) = val
    end subroutine

    real(real64) function sys_mag_beg_x()
        sys_mag_beg_x = SYSTEM(41)
    end function

    subroutine set_sys_mag_beg_x(val)
        real(real64), intent(in) :: val
        SYSTEM(41) = val
    end subroutine

    real(real64) function sys_mag_end_y()
        sys_mag_end_y = SYSTEM(42)
    end function

    subroutine set_sys_mag_end_y(val)
        real(real64), intent(in) :: val
        SYSTEM(42) = val
    end subroutine

    real(real64) function sys_mag_end_x()
        sys_mag_end_x = SYSTEM(43)
    end function

    subroutine set_sys_mag_end_x(val)
        real(real64), intent(in) :: val
        SYSTEM(43) = val
    end subroutine

    ! F/# hold flag YZ: 1 = hold F/#, -1 = hold Expup.
    integer function sys_fno_hold_y()
        sys_fno_hold_y = nint(SYSTEM(44))
    end function

    subroutine set_sys_fno_hold_y(val)
        integer, intent(in) :: val
        SYSTEM(44) = real(val, real64)
    end subroutine

    ! F/# hold flag XZ.
    integer function sys_fno_hold_x()
        sys_fno_hold_x = nint(SYSTEM(45))
    end function

    subroutine set_sys_fno_hold_x(val)
        integer, intent(in) :: val
        SYSTEM(45) = real(val, real64)
    end subroutine

    real(real64) function sys_fno_val_y()
        sys_fno_val_y = SYSTEM(46)
    end function

    subroutine set_sys_fno_val_y(val)
        real(real64), intent(in) :: val
        SYSTEM(46) = val
    end subroutine

    real(real64) function sys_fno_val_x()
        sys_fno_val_x = SYSTEM(47)
    end function

    subroutine set_sys_fno_val_x(val)
        real(real64), intent(in) :: val
        SYSTEM(47) = val
    end subroutine

    ! ── Configurations ────────────────────────────────────────────────────────

    ! Current configuration (read-only — do not modify directly).
    real(real64) function sys_curr_cfg()
        sys_curr_cfg = SYSTEM(50)
    end function

    ! Highest non-blank alternate configuration number.
    integer function sys_high_cfg()
        sys_high_cfg = nint(SYSTEM(56))
    end function

    subroutine set_sys_high_cfg(val)
        integer, intent(in) :: val
        SYSTEM(56) = real(val, real64)
    end subroutine

    ! ── Ray aiming ────────────────────────────────────────────────────────────

    ! Reference surface orientation angle.
    real(real64) function sys_ref_orient()
        sys_ref_orient = SYSTEM(59)
    end function

    subroutine set_sys_ref_orient(val)
        real(real64), intent(in) :: val
        SYSTEM(59) = val
    end subroutine

    ! Regular ray aiming flag (1=on, 0=off).
    integer function sys_ray_aiming()
        sys_ray_aiming = nint(SYSTEM(62))
    end function

    subroutine set_sys_ray_aiming(val)
        integer, intent(in) :: val
        SYSTEM(62) = real(val, real64)
    end subroutine

    ! Telecentric ray aiming flag (1=on, 0=off).
    integer function sys_telecentric()
        sys_telecentric = nint(SYSTEM(63))
    end function

    subroutine set_sys_telecentric(val)
        integer, intent(in) :: val
        SYSTEM(63) = real(val, real64)
    end subroutine

    ! NAO flag: 1=NAOY explicitly set, 2=NAOX, 3=both.
    integer function sys_nao_flag()
        sys_nao_flag = nint(SYSTEM(64))
    end function

    subroutine set_sys_nao_flag(val)
        integer, intent(in) :: val
        SYSTEM(64) = real(val, real64)
    end subroutine

    real(real64) function sys_nao_y()
        sys_nao_y = SYSTEM(65)
    end function

    subroutine set_sys_nao_y(val)
        real(real64), intent(in) :: val
        SYSTEM(65) = val
    end subroutine

    real(real64) function sys_nao_x()
        sys_nao_x = SYSTEM(66)
    end function

    subroutine set_sys_nao_x(val)
        real(real64), intent(in) :: val
        SYSTEM(66) = val
    end subroutine

    ! FNO flag: 1=FNOY explicitly set, 2=FNOX, 3=both.
    integer function sys_fno_flag()
        sys_fno_flag = nint(SYSTEM(67))
    end function

    subroutine set_sys_fno_flag(val)
        integer, intent(in) :: val
        SYSTEM(67) = real(val, real64)
    end subroutine

    real(real64) function sys_fno_y()
        sys_fno_y = SYSTEM(68)
    end function

    subroutine set_sys_fno_y(val)
        real(real64), intent(in) :: val
        SYSTEM(68) = val
    end subroutine

    real(real64) function sys_fno_x()
        sys_fno_x = SYSTEM(69)
    end function

    subroutine set_sys_fno_x(val)
        real(real64), intent(in) :: val
        SYSTEM(69) = val
    end subroutine

    ! Aplanatic ray aiming flag (1=on, 0=off).
    integer function sys_aplanatic()
        sys_aplanatic = nint(SYSTEM(70))
    end function

    subroutine set_sys_aplanatic(val)
        integer, intent(in) :: val
        SYSTEM(70) = real(val, real64)
    end subroutine

    ! ── Gaussian beam ─────────────────────────────────────────────────────────

    real(real64) function sys_wrx()
        sys_wrx = SYSTEM(85)
    end function

    subroutine set_sys_wrx(val)
        real(real64), intent(in) :: val
        SYSTEM(85) = val
    end subroutine

    real(real64) function sys_wry()
        sys_wry = SYSTEM(86)
    end function

    subroutine set_sys_wry(val)
        real(real64), intent(in) :: val
        SYSTEM(86) = val
    end subroutine

    real(real64) function sys_bdx()
        sys_bdx = SYSTEM(87)
    end function

    subroutine set_sys_bdx(val)
        real(real64), intent(in) :: val
        SYSTEM(87) = val
    end subroutine

    real(real64) function sys_bdy()
        sys_bdy = SYSTEM(88)
    end function

    subroutine set_sys_bdy(val)
        real(real64), intent(in) :: val
        SYSTEM(88) = val
    end subroutine

    ! ── Aim offsets ───────────────────────────────────────────────────────────

    real(real64) function sys_aim_offset_x()
        sys_aim_offset_x = SYSTEM(81)
    end function

    subroutine set_sys_aim_offset_x(val)
        real(real64), intent(in) :: val
        SYSTEM(81) = val
    end subroutine

    real(real64) function sys_aim_offset_y()
        sys_aim_offset_y = SYSTEM(82)
    end function

    subroutine set_sys_aim_offset_y(val)
        real(real64), intent(in) :: val
        SYSTEM(82) = val
    end subroutine

    real(real64) function sys_aim_offset_z()
        sys_aim_offset_z = SYSTEM(89)
    end function

    subroutine set_sys_aim_offset_z(val)
        real(real64), intent(in) :: val
        SYSTEM(89) = val
    end subroutine

    ! ── Miscellaneous flags ───────────────────────────────────────────────────

    ! Lens tilt return resolution flag.
    integer function sys_tilt_ret()
        sys_tilt_ret = nint(SYSTEM(90))
    end function

    subroutine set_sys_tilt_ret(val)
        integer, intent(in) :: val
        SYSTEM(90) = real(val, real64)
    end subroutine

    ! Autofunc setting (0=no call, 1-10=function number to call).
    integer function sys_autofunc()
        sys_autofunc = nint(SYSTEM(91))
    end function

    subroutine set_sys_autofunc(val)
        integer, intent(in) :: val
        SYSTEM(91) = real(val, real64)
    end subroutine

    real(real64) function sys_pxim()
        sys_pxim = SYSTEM(92)
    end function

    subroutine set_sys_pxim(val)
        real(real64), intent(in) :: val
        SYSTEM(92) = val
    end subroutine

    real(real64) function sys_pyim()
        sys_pyim = SYSTEM(93)
    end function

    subroutine set_sys_pyim(val)
        real(real64), intent(in) :: val
        SYSTEM(93) = val
    end subroutine

    ! 1.0 if PXIM FANG explicitly set, else -1.0.
    integer function sys_pxim_fang_set()
        sys_pxim_fang_set = nint(SYSTEM(94))
    end function

    subroutine set_sys_pxim_fang_set(val)
        integer, intent(in) :: val
        SYSTEM(94) = real(val, real64)
    end subroutine

    ! 1.0 if PYIM FANG explicitly set, else -1.0.
    integer function sys_pyim_fang_set()
        sys_pyim_fang_set = nint(SYSTEM(95))
    end function

    subroutine set_sys_pyim_fang_set(val)
        integer, intent(in) :: val
        SYSTEM(95) = real(val, real64)
    end subroutine

    real(real64) function sys_rxim()
        sys_rxim = SYSTEM(96)
    end function

    subroutine set_sys_rxim(val)
        real(real64), intent(in) :: val
        SYSTEM(96) = val
    end subroutine

    real(real64) function sys_ryim()
        sys_ryim = SYSTEM(97)
    end function

    subroutine set_sys_ryim(val)
        real(real64), intent(in) :: val
        SYSTEM(97) = val
    end subroutine

    ! 1.0 if RXIM FANG explicitly set, else -1.0.
    integer function sys_rxim_fang_set()
        sys_rxim_fang_set = nint(SYSTEM(98))
    end function

    subroutine set_sys_rxim_fang_set(val)
        integer, intent(in) :: val
        SYSTEM(98) = real(val, real64)
    end subroutine

    ! 1.0 if RYIM FANG explicitly set, else -1.0.
    integer function sys_ryim_fang_set()
        sys_ryim_fang_set = nint(SYSTEM(99))
    end function

    subroutine set_sys_ryim_fang_set(val)
        integer, intent(in) :: val
        SYSTEM(99) = real(val, real64)
    end subroutine

    ! Reverse trace flag (0=no, 1=yes).
    integer function sys_reverse()
        sys_reverse = nint(SYSTEM(100))
    end function

    subroutine set_sys_reverse(val)
        integer, intent(in) :: val
        SYSTEM(100) = real(val, real64)
    end subroutine

    ! Verbose optimization output flag (0=off).
    integer function sys_verbose_optim()
        sys_verbose_optim = nint(SYSTEM(101))
    end function

    subroutine set_sys_verbose_optim(val)
        integer, intent(in) :: val
        SYSTEM(101) = real(val, real64)
    end subroutine

    ! ── NSS / screen surface ──────────────────────────────────────────────────

    ! 1 if NSS surfaces are present, 0 otherwise.
    integer function sys_nss()
        sys_nss = nint(SYSTEM(102))
    end function

    subroutine set_sys_nss(val)
        integer, intent(in) :: val
        SYSTEM(102) = real(val, real64)
    end subroutine

    ! Screen surface flag (0=none, 1=present).
    integer function sys_screen()
        sys_screen = nint(SYSTEM(103))
    end function

    subroutine set_sys_screen(val)
        integer, intent(in) :: val
        SYSTEM(103) = real(val, real64)
    end subroutine

    ! Surface number of the screen surface.
    integer function sys_screen_surf()
        sys_screen_surf = nint(SYSTEM(104))
    end function

    subroutine set_sys_screen_surf(val)
        integer, intent(in) :: val
        SYSTEM(104) = real(val, real64)
    end subroutine

    ! Diameter of holes in screen surface.
    real(real64) function sys_screen_d()
        sys_screen_d = SYSTEM(105)
    end function

    subroutine set_sys_screen_d(val)
        real(real64), intent(in) :: val
        SYSTEM(105) = val
    end subroutine

    ! Depth of holes in screen surface.
    real(real64) function sys_screen_h()
        sys_screen_h = SYSTEM(106)
    end function

    subroutine set_sys_screen_h(val)
        real(real64), intent(in) :: val
        SYSTEM(106) = val
    end subroutine

    ! Hole spacing in screen surface.
    real(real64) function sys_screen_s()
        sys_screen_s = SYSTEM(107)
    end function

    subroutine set_sys_screen_s(val)
        real(real64), intent(in) :: val
        SYSTEM(107) = val
    end subroutine

    ! ── Spot diagram / pixel detector ─────────────────────────────────────────

    ! Number of pixels in X dimension.
    integer function sys_pixel_nx()
        sys_pixel_nx = nint(SYSTEM(121))
    end function

    subroutine set_sys_pixel_nx(val)
        integer, intent(in) :: val
        SYSTEM(121) = real(val, real64)
    end subroutine

    ! Number of pixels in Y dimension.
    integer function sys_pixel_ny()
        sys_pixel_ny = nint(SYSTEM(122))
    end function

    subroutine set_sys_pixel_ny(val)
        integer, intent(in) :: val
        SYSTEM(122) = real(val, real64)
    end subroutine

    real(real64) function sys_pixel_pitch_x()
        sys_pixel_pitch_x = SYSTEM(123)
    end function

    subroutine set_sys_pixel_pitch_x(val)
        real(real64), intent(in) :: val
        SYSTEM(123) = val
    end subroutine

    real(real64) function sys_pixel_pitch_y()
        sys_pixel_pitch_y = SYSTEM(124)
    end function

    subroutine set_sys_pixel_pitch_y(val)
        real(real64), intent(in) :: val
        SYSTEM(124) = val
    end subroutine

    real(real64) function sys_det_center_x()
        sys_det_center_x = SYSTEM(125)
    end function

    subroutine set_sys_det_center_x(val)
        real(real64), intent(in) :: val
        SYSTEM(125) = val
    end subroutine

    real(real64) function sys_det_center_y()
        sys_det_center_y = SYSTEM(126)
    end function

    subroutine set_sys_det_center_y(val)
        real(real64), intent(in) :: val
        SYSTEM(126) = val
    end subroutine

    ! Surface number of the spot diagram surface.
    integer function sys_spot_surf()
        sys_spot_surf = nint(SYSTEM(127))
    end function

    subroutine set_sys_spot_surf(val)
        integer, intent(in) :: val
        SYSTEM(127) = real(val, real64)
    end subroutine

    ! ── Reference orientation flip flags ─────────────────────────────────────

    ! Flip reference X flag (0=off, 1=on).
    integer function sys_flip_ref_x()
        sys_flip_ref_x = nint(SYSTEM(128))
    end function

    subroutine set_sys_flip_ref_x(val)
        integer, intent(in) :: val
        SYSTEM(128) = real(val, real64)
    end subroutine

    ! Flip reference Y flag (0=off, 1=on).
    integer function sys_flip_ref_y()
        sys_flip_ref_y = nint(SYSTEM(129))
    end function

    subroutine set_sys_flip_ref_y(val)
        integer, intent(in) :: val
        SYSTEM(129) = real(val, real64)
    end subroutine

end module mod_system
