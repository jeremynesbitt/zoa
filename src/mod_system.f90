! mod_system.f90
! Getter and setter functions for the SYSTEM(1:150) array.
!
! Source of truth: Library/OPTMAN/LENSTORE.DOC
!
! Usage:  use mod_system
!
! All getters return real(8) — the native type of SYSTEM — so callers
! that do INT(SYSTEM(N)) become INT(sys_xxx()) with identical semantics.
!
! NAMING NOTE: Fortran is case-insensitive. The integer constants in
! surface_params (SYS_WL_REF, SYS_LAST_SURF, etc.) share names with these
! functions. Do NOT use both surface_params and mod_system in the same scope
! until those constants are removed from surface_params.
!
! Indexed accessors:
!   sys_wavelength(n)      — wavelength in microns for wl n (1-10)
!   sys_wl_weight(n)       — spectral weight for wl n (1-10)
!   sys_set_wavelength(n,v) — set wavelength n
!   sys_set_wl_weight(n,v)  — set spectral weight n
!   sys_set_wv_default(n,v) — set default wavelength WVn: n=1..10 -> SYSTEM(110+n)

module mod_system
  use DATLEN, only: SYSTEM
  implicit none
  private
  public :: sys_wavelength, sys_wl_weight, sys_wv_default
  public :: sys_units, sys_wl_pri1, sys_wl_pri2, sys_wl_sec1, sys_wl_sec2
  public :: sys_wl_ref
  public :: clearSysArray
  public :: sys_say, sys_sax, sys_scy, sys_y1_scy, sys_scx, sys_x1_scx
  public :: sys_scy_fang_set, sys_scx_fang_set
  public :: sys_last_surf
  public :: sys_scy_fang, sys_y1_scy_fang, sys_scx_fang, sys_x1_scx_fang
  public :: sys_ref_surf, sys_astop, sys_astop_adj
  public :: sys_xz_bilateral, sys_gen_parax, sys_mode
  public :: sys_yz_mag_reset, sys_xz_mag_reset, sys_yz_mag_desired, sys_xz_mag_desired
  public :: sys_yz_mag_start, sys_xz_mag_start, sys_yz_mag_end, sys_xz_mag_end
  public :: sys_fno_flag_y, sys_fno_flag_x, sys_fno_hold_y, sys_fno_hold_x
  public :: sys_yz_bilateral, sys_xz_data_flag
  public :: sys_current_cfg
  public :: sys_y1_scy_set, sys_x1_scx_set, sys_y1_scy_fang_set, sys_x1_scx_fang_set
  public :: sys_sth_temp
  public :: sys_high_cfg
  public :: sys_gen_parax_flag, sys_ref_orient
  public :: sys_scy_set, sys_scx_set
  public :: sys_ray_aiming, sys_telecentric
  public :: sys_na_set, sys_naoy, sys_naox
  public :: sys_fno_val_set, sys_fno_val_y, sys_fno_val_x
  public :: sys_aplanatic_aim
  public :: sys_aim_offset_x, sys_aim_offset_y, sys_aim_offset_z
  public :: sys_say_float, sys_sax_float
  public :: sys_wrx, sys_wry, sys_bdx, sys_bdy
  public :: sys_tilt_ret, sys_autofunc
  public :: sys_pxim, sys_pyim, sys_pxim_fang_set, sys_pyim_fang_set
  public :: sys_rxim, sys_ryim, sys_rxim_fang_set, sys_ryim_fang_set
  public :: sys_reverse_trace, sys_verbose_optim, sys_nss_present
  public :: sys_screen, sys_screen_surf, sys_screen_d, sys_screen_h, sys_screen_s
  public :: sys_screen_excl_angle
  public :: sys_fliprefx, sys_fliprefy
  ! Setters
  public :: sys_set_wavelength, sys_set_wl_weight, sys_set_wv_default
  public :: sys_set_units, sys_set_wl_pri1, sys_set_wl_pri2, sys_set_wl_sec1, sys_set_wl_sec2
  public :: sys_set_wl_ref
  public :: sys_set_say, sys_set_sax, sys_set_scy, sys_set_y1_scy, sys_set_scx, sys_set_x1_scx
  public :: sys_set_scy_fang_set, sys_set_scx_fang_set
  public :: sys_set_last_surf
  public :: sys_set_scy_fang, sys_set_y1_scy_fang, sys_set_scx_fang, sys_set_x1_scx_fang
  public :: sys_set_ref_surf, sys_set_astop, sys_set_astop_adj
  public :: sys_set_xz_bilateral, sys_set_gen_parax, sys_set_mode
  public :: sys_set_yz_mag_reset, sys_set_xz_mag_reset, sys_set_yz_mag_desired, sys_set_xz_mag_desired
  public :: sys_set_yz_mag_start, sys_set_xz_mag_start, sys_set_yz_mag_end, sys_set_xz_mag_end
  public :: sys_set_fno_flag_y, sys_set_fno_flag_x, sys_set_fno_hold_y, sys_set_fno_hold_x
  public :: sys_set_yz_bilateral, sys_set_xz_data_flag, sys_set_current_cfg
  public :: sys_set_y1_scy_set, sys_set_x1_scx_set, sys_set_y1_scy_fang_set, sys_set_x1_scx_fang_set
  public :: sys_set_sth_temp
  public :: sys_set_high_cfg, sys_set_ref_orient
  public :: sys_set_scy_set, sys_set_scx_set
  public :: sys_set_ray_aiming, sys_set_telecentric
  public :: sys_set_na_set, sys_set_naoy, sys_set_naox
  public :: sys_set_fno_val_set, sys_set_fno_val_y, sys_set_fno_val_x
  public :: sys_set_aplanatic_aim
  public :: sys_set_aim_offset_x, sys_set_aim_offset_y, sys_set_aim_offset_z
  public :: sys_set_say_float, sys_set_sax_float
  public :: sys_set_wrx, sys_set_wry, sys_set_bdx, sys_set_bdy
  public :: sys_set_tilt_ret, sys_set_autofunc
  public :: sys_set_pxim, sys_set_pyim, sys_set_pxim_fang_set, sys_set_pyim_fang_set
  public :: sys_set_rxim, sys_set_ryim, sys_set_rxim_fang_set, sys_set_ryim_fang_set
  public :: sys_set_reverse_trace, sys_set_verbose_optim, sys_set_nss_present
  public :: sys_set_screen, sys_set_screen_surf, sys_set_screen_d, sys_set_screen_h, sys_set_screen_s
  public :: sys_set_screen_excl_angle
  public :: sys_set_fliprefx, sys_set_fliprefy

contains

  ! Wavelength in microns for wavelength number n (1-10).
  ! n=1..5  -> SYSTEM(n); n=6..10 -> SYSTEM(65+n) = SYSTEM(71..75)
  function sys_wavelength(n) result(r)
    integer, intent(in) :: n
    real(8) :: r
    if (n <= 5) then
      r = SYSTEM(n)
    else
      r = SYSTEM(65 + n)
    end if
  end function

  ! Spectral weighting factor for wavelength number n (1-10).
  ! n=1..5  -> SYSTEM(30+n) = SYSTEM(31..35)
  ! n=6..10 -> SYSTEM(70+n) = SYSTEM(76..80)
  function sys_wl_weight(n) result(r)
    integer, intent(in) :: n
    real(8) :: r
    if (n <= 5) then
      r = SYSTEM(30 + n)
    else
      r = SYSTEM(70 + n)
    end if
  end function

  ! SYSTEM(6)  — units (1=inches, 2=cm, 3=mm, 4=meters)
  function sys_units() result(r)
    real(8) :: r
    r = SYSTEM(6)
  end function

  ! SYSTEM(7)  — first wavelength of primary color pair
  function sys_wl_pri1() result(r)
    real(8) :: r
    r = SYSTEM(7)
  end function

  ! SYSTEM(8)  — second wavelength of primary color pair
  function sys_wl_pri2() result(r)
    real(8) :: r
    r = SYSTEM(8)
  end function

  ! SYSTEM(9)  — first wavelength of secondary color pair
  function sys_wl_sec1() result(r)
    real(8) :: r
    r = SYSTEM(9)
  end function

  ! SYSTEM(10) — second wavelength of secondary color pair
  function sys_wl_sec2() result(r)
    real(8) :: r
    r = SYSTEM(10)
  end function

  ! SYSTEM(11) — reference (control) wavelength number
  function sys_wl_ref() result(r)
    real(8) :: r
    r = SYSTEM(11)
  end function

  ! SYSTEM(12) — SAY (semi-aperture Y)
  function sys_say() result(r)
    real(8) :: r
    r = SYSTEM(12)
  end function

  ! SYSTEM(13) — SAX (semi-aperture X)
  function sys_sax() result(r)
    real(8) :: r
    r = SYSTEM(13)
  end function

  ! SYSTEM(14) — SCY (semi-field Y)
  function sys_scy() result(r)
    real(8) :: r
    r = SYSTEM(14)
  end function

  ! SYSTEM(15) — Y1 value for SCY
  function sys_y1_scy() result(r)
    real(8) :: r
    r = SYSTEM(15)
  end function

  ! SYSTEM(16) — SCX (semi-field X)
  function sys_scx() result(r)
    real(8) :: r
    r = SYSTEM(16)
  end function

  ! SYSTEM(17) — X1 value for SCX
  function sys_x1_scx() result(r)
    real(8) :: r
    r = SYSTEM(17)
  end function

  ! SYSTEM(18) — SCY FANG explicitly-set flag (1=set, 0=not set)
  function sys_scy_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(18)
  end function

  ! SYSTEM(19) — SCX FANG explicitly-set flag (1=set, 0=not set)
  function sys_scx_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(19)
  end function

  ! SYSTEM(20) — surface number of the last surface
  function sys_last_surf() result(r)
    real(8) :: r
    r = SYSTEM(20)
  end function

  ! SYSTEM(21) — SCY FANG value
  function sys_scy_fang() result(r)
    real(8) :: r
    r = SYSTEM(21)
  end function

  ! SYSTEM(22) — Y1 value for SCY FANG
  function sys_y1_scy_fang() result(r)
    real(8) :: r
    r = SYSTEM(22)
  end function

  ! SYSTEM(23) — SCX FANG value
  function sys_scx_fang() result(r)
    real(8) :: r
    r = SYSTEM(23)
  end function

  ! SYSTEM(24) — X1 value for SCX FANG
  function sys_x1_scx_fang() result(r)
    real(8) :: r
    r = SYSTEM(24)
  end function

  ! SYSTEM(25) — reference surface number (default=1)
  function sys_ref_surf() result(r)
    real(8) :: r
    r = SYSTEM(25)
  end function

  ! SYSTEM(26) — ASTOP surface number (-99 if no ASTOP)
  function sys_astop() result(r)
    real(8) :: r
    r = SYSTEM(26)
  end function

  ! SYSTEM(27) — ASTOP adjustment flag (1=EN, -1=EX, 0=none, 2=ENEX)
  function sys_astop_adj() result(r)
    real(8) :: r
    r = SYSTEM(27)
  end function

  ! SYSTEM(28) — XZ bilateral symmetry flag (1=yes, 0=no)
  function sys_xz_bilateral() result(r)
    real(8) :: r
    r = SYSTEM(28)
  end function

  ! SYSTEM(29) — generalized paraxial ray trace flag (1=on, 0=off)
  function sys_gen_parax() result(r)
    real(8) :: r
    r = SYSTEM(29)
  end function

  ! SYSTEM(30) — mode flag (1=FOCAL, 2=UFOCAL, 3=AFOCAL, 4=UAFOCAL)
  function sys_mode() result(r)
    real(8) :: r
    r = SYSTEM(30)
  end function

  ! SYSTEM(36) — YZ transverse MAG reset value
  function sys_yz_mag_reset() result(r)
    real(8) :: r
    r = SYSTEM(36)
  end function

  ! SYSTEM(37) — XZ transverse MAG reset value
  function sys_xz_mag_reset() result(r)
    real(8) :: r
    r = SYSTEM(37)
  end function

  ! SYSTEM(38) — YZ desired magnification
  function sys_yz_mag_desired() result(r)
    real(8) :: r
    r = SYSTEM(38)
  end function

  ! SYSTEM(39) — XZ desired magnification
  function sys_xz_mag_desired() result(r)
    real(8) :: r
    r = SYSTEM(39)
  end function

  ! SYSTEM(40) — beginning surface for YZ mag
  function sys_yz_mag_start() result(r)
    real(8) :: r
    r = SYSTEM(40)
  end function

  ! SYSTEM(41) — beginning surface for XZ mag
  function sys_xz_mag_start() result(r)
    real(8) :: r
    r = SYSTEM(41)
  end function

  ! SYSTEM(42) — ending surface for YZ mag
  function sys_yz_mag_end() result(r)
    real(8) :: r
    r = SYSTEM(42)
  end function

  ! SYSTEM(43) — ending surface for XZ mag
  function sys_xz_mag_end() result(r)
    real(8) :: r
    r = SYSTEM(43)
  end function

  ! SYSTEM(44) — F/# flag YZ (1=FNO hold, -1=FNO, 2=expup hold, -2=expup)
  function sys_fno_flag_y() result(r)
    real(8) :: r
    r = SYSTEM(44)
  end function

  ! SYSTEM(45) — F/# flag XZ
  function sys_fno_flag_x() result(r)
    real(8) :: r
    r = SYSTEM(45)
  end function

  ! SYSTEM(46) — F/# hold value YZ
  function sys_fno_hold_y() result(r)
    real(8) :: r
    r = SYSTEM(46)
  end function

  ! SYSTEM(47) — F/# hold value XZ
  function sys_fno_hold_x() result(r)
    real(8) :: r
    r = SYSTEM(47)
  end function

  ! SYSTEM(48) — YZ bilateral symmetry flag (1=yes, 0=no)
  function sys_yz_bilateral() result(r)
    real(8) :: r
    r = SYSTEM(48)
  end function

  ! SYSTEM(49) — XZ data entry flag (0=none, 1=aperture, 2=field, 3=both)
  function sys_xz_data_flag() result(r)
    real(8) :: r
    r = SYSTEM(49)
  end function

  ! SYSTEM(50) — current configuration tracker (do not set directly)
  function sys_current_cfg() result(r)
    real(8) :: r
    r = SYSTEM(50)
  end function

  ! SYSTEM(51) — =1 if Y1(SCY) was explicitly set
  function sys_y1_scy_set() result(r)
    real(8) :: r
    r = SYSTEM(51)
  end function

  ! SYSTEM(52) — =1 if X1(SCX) was explicitly set
  function sys_x1_scx_set() result(r)
    real(8) :: r
    r = SYSTEM(52)
  end function

  ! SYSTEM(53) — =1 if Y1(SCY FANG) was explicitly set
  function sys_y1_scy_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(53)
  end function

  ! SYSTEM(54) — =1 if X1(SCX FANG) was explicitly set
  function sys_x1_scx_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(54)
  end function

  ! SYSTEM(55) — temporary thickness used by STH
  function sys_sth_temp() result(r)
    real(8) :: r
    r = SYSTEM(55)
  end function

  ! SYSTEM(56) — highest non-blank alternate configuration number
  function sys_high_cfg() result(r)
    real(8) :: r
    r = SYSTEM(56)
  end function

  ! SYSTEM(58) — generalized paraxial trace flag (0=off, 1=on)
  function sys_gen_parax_flag() result(r)
    real(8) :: r
    r = SYSTEM(58)
  end function

  ! SYSTEM(59) — reference surface orientation angle
  function sys_ref_orient() result(r)
    real(8) :: r
    r = SYSTEM(59)
  end function

  ! SYSTEM(60) — =1 if SCY or SCY FANG explicitly set
  function sys_scy_set() result(r)
    real(8) :: r
    r = SYSTEM(60)
  end function

  ! SYSTEM(61) — =1 if SCX or SCX FANG explicitly set
  function sys_scx_set() result(r)
    real(8) :: r
    r = SYSTEM(61)
  end function

  ! SYSTEM(62) — ray aiming flag (1=regular aiming on, 0=off)
  function sys_ray_aiming() result(r)
    real(8) :: r
    r = SYSTEM(62)
  end function

  ! SYSTEM(63) — telecentric aiming flag (1=telecentric on, 0=off)
  function sys_telecentric() result(r)
    real(8) :: r
    r = SYSTEM(63)
  end function

  ! SYSTEM(64) — NA explicitly-set flag (1=NAOY set, 2=NAOX set, 3=both)
  function sys_na_set() result(r)
    real(8) :: r
    r = SYSTEM(64)
  end function

  ! SYSTEM(65) — NAOY value
  function sys_naoy() result(r)
    real(8) :: r
    r = SYSTEM(65)
  end function

  ! SYSTEM(66) — NAOX value
  function sys_naox() result(r)
    real(8) :: r
    r = SYSTEM(66)
  end function

  ! SYSTEM(67) — FNO explicitly-set flag (1=FNOY set, 2=FNOX set, 3=both)
  function sys_fno_val_set() result(r)
    real(8) :: r
    r = SYSTEM(67)
  end function

  ! SYSTEM(68) — FNOY value
  function sys_fno_val_y() result(r)
    real(8) :: r
    r = SYSTEM(68)
  end function

  ! SYSTEM(69) — FNOX value
  function sys_fno_val_x() result(r)
    real(8) :: r
    r = SYSTEM(69)
  end function

  ! SYSTEM(70) — aplanatic ray aiming flag (0=off, 1=on)
  function sys_aplanatic_aim() result(r)
    real(8) :: r
    r = SYSTEM(70)
  end function

  ! SYSTEM(81) — aim offset in X
  function sys_aim_offset_x() result(r)
    real(8) :: r
    r = SYSTEM(81)
  end function

  ! SYSTEM(82) — aim offset in Y
  function sys_aim_offset_y() result(r)
    real(8) :: r
    r = SYSTEM(82)
  end function

  ! SYSTEM(83) — =1 means SAY floats
  function sys_say_float() result(r)
    real(8) :: r
    r = SYSTEM(83)
  end function

  ! SYSTEM(84) — =1 means SAX floats
  function sys_sax_float() result(r)
    real(8) :: r
    r = SYSTEM(84)
  end function

  ! SYSTEM(85) — WRX gaussian beam setting
  function sys_wrx() result(r)
    real(8) :: r
    r = SYSTEM(85)
  end function

  ! SYSTEM(86) — WRY gaussian beam setting
  function sys_wry() result(r)
    real(8) :: r
    r = SYSTEM(86)
  end function

  ! SYSTEM(87) — BDX gaussian beam setting
  function sys_bdx() result(r)
    real(8) :: r
    r = SYSTEM(87)
  end function

  ! SYSTEM(88) — BDY gaussian beam setting
  function sys_bdy() result(r)
    real(8) :: r
    r = SYSTEM(88)
  end function

  ! SYSTEM(89) — aim offset in Z
  function sys_aim_offset_z() result(r)
    real(8) :: r
    r = SYSTEM(89)
  end function

  ! SYSTEM(90) — lens tilt ret resolution flag (0=unresolved, 1=resolved)
  function sys_tilt_ret() result(r)
    real(8) :: r
    r = SYSTEM(90)
  end function

  ! SYSTEM(91) — autofunc setting (0=no call, 1-10=func# to call)
  function sys_autofunc() result(r)
    real(8) :: r
    r = SYSTEM(91)
  end function

  ! SYSTEM(92) — PXIM or PXIM FANG value
  function sys_pxim() result(r)
    real(8) :: r
    r = SYSTEM(92)
  end function

  ! SYSTEM(93) — PYIM or PYIM FANG value
  function sys_pyim() result(r)
    real(8) :: r
    r = SYSTEM(93)
  end function

  ! SYSTEM(94) — PXIM FANG set flag (1=FANG, -1=PXIM, 0=neither)
  function sys_pxim_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(94)
  end function

  ! SYSTEM(95) — PYIM FANG set flag (1=FANG, -1=PYIM, 0=neither)
  function sys_pyim_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(95)
  end function

  ! SYSTEM(96) — RXIM or RXIM FANG value
  function sys_rxim() result(r)
    real(8) :: r
    r = SYSTEM(96)
  end function

  ! SYSTEM(97) — RYIM or RYIM FANG value
  function sys_ryim() result(r)
    real(8) :: r
    r = SYSTEM(97)
  end function

  ! SYSTEM(98) — RXIM FANG set flag (1=FANG, -1=RXIM, 0=neither)
  function sys_rxim_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(98)
  end function

  ! SYSTEM(99) — RYIM FANG set flag (1=FANG, -1=RYIM, 0=neither)
  function sys_ryim_fang_set() result(r)
    real(8) :: r
    r = SYSTEM(99)
  end function

  ! SYSTEM(100) — reverse trace flag (0=forward, 1=reverse)
  function sys_reverse_trace() result(r)
    real(8) :: r
    r = SYSTEM(100)
  end function

  ! SYSTEM(101) — verbose optimization output flag (0=quiet, 1=verbose)
  function sys_verbose_optim() result(r)
    real(8) :: r
    r = SYSTEM(101)
  end function

  ! SYSTEM(102) — NSS surfaces present flag (1=yes, 0=no)
  function sys_nss_present() result(r)
    real(8) :: r
    r = SYSTEM(102)
  end function

  ! SYSTEM(103) — screen surface flag (0=none, 1=screen present)
  function sys_screen() result(r)
    real(8) :: r
    r = SYSTEM(103)
  end function

  ! SYSTEM(104) — surface number of the screen surface
  function sys_screen_surf() result(r)
    real(8) :: r
    r = SYSTEM(104)
  end function

  ! SYSTEM(105) — diameter of holes in screen surface
  function sys_screen_d() result(r)
    real(8) :: r
    r = SYSTEM(105)
  end function

  ! SYSTEM(106) — thickness of screen surface (depth of holes)
  function sys_screen_h() result(r)
    real(8) :: r
    r = SYSTEM(106)
  end function

  ! SYSTEM(107) — X and Y spacing between holes in screen surface
  function sys_screen_s() result(r)
    real(8) :: r
    r = SYSTEM(107)
  end function

  ! SYSTEM(108) — screen exclusion angle
  function sys_screen_excl_angle() result(r)
    real(8) :: r
    r = SYSTEM(108)
  end function

  ! SYSTEM(128) — fliprefx flag (0=off, 1=on)
  function sys_fliprefx() result(r)
    real(8) :: r
    r = SYSTEM(128)
  end function

  ! SYSTEM(129) — fliprefy flag (0=off, 1=on)
  function sys_fliprefy() result(r)
    real(8) :: r
    r = SYSTEM(129)
  end function

  !---------------------------------------------------------------------------
  ! Setters
  !---------------------------------------------------------------------------

  ! Set wavelength n (1-10): n=1..5 -> SYSTEM(n), n=6..10 -> SYSTEM(65+n)
  subroutine sys_set_wavelength(n, val)
    integer, intent(in) :: n
    real(8), intent(in) :: val
    if (n <= 5) then
      SYSTEM(n) = val
    else
      SYSTEM(65 + n) = val
    end if
  end subroutine

  ! Set spectral weight n (1-10): n=1..5 -> SYSTEM(30+n), n=6..10 -> SYSTEM(70+n)
  subroutine sys_set_wl_weight(n, val)
    integer, intent(in) :: n
    real(8), intent(in) :: val
    if (n <= 5) then
      SYSTEM(30 + n) = val
    else
      SYSTEM(70 + n) = val
    end if
  end subroutine

  ! Set default wavelength WVn: n=1..10 -> SYSTEM(110+n)
  subroutine sys_set_wv_default(n, val)
    integer, intent(in) :: n
    real(8), intent(in) :: val
    SYSTEM(110 + n) = val
  end subroutine

  ! Get default wavelength WVn: n=1..10 -> SYSTEM(110+n)
  function sys_wv_default(n) result(r)
    integer, intent(in) :: n
    real(8) :: r
    r = SYSTEM(110 + n)
  end function

  ! Zero the entire SYSTEM array (matches SYSTEM(1:SSIZ)=0.0D0 in LDM1/LDM10,
  ! where SSIZ is always 150; SYSTEM is declared SYSTEM(1:150) in DATLEN).
  subroutine clearSysArray()
    SYSTEM = 0.0D0
  end subroutine

  subroutine sys_set_units(val)
    real(8), intent(in) :: val
    SYSTEM(6) = val
  end subroutine

  subroutine sys_set_wl_pri1(val)
    real(8), intent(in) :: val
    SYSTEM(7) = val
  end subroutine

  subroutine sys_set_wl_pri2(val)
    real(8), intent(in) :: val
    SYSTEM(8) = val
  end subroutine

  subroutine sys_set_wl_sec1(val)
    real(8), intent(in) :: val
    SYSTEM(9) = val
  end subroutine

  subroutine sys_set_wl_sec2(val)
    real(8), intent(in) :: val
    SYSTEM(10) = val
  end subroutine

  subroutine sys_set_wl_ref(val)
    real(8), intent(in) :: val
    SYSTEM(11) = val
  end subroutine

  subroutine sys_set_say(val)
    real(8), intent(in) :: val
    SYSTEM(12) = val
  end subroutine

  subroutine sys_set_sax(val)
    real(8), intent(in) :: val
    SYSTEM(13) = val
  end subroutine

  subroutine sys_set_scy(val)
    real(8), intent(in) :: val
    SYSTEM(14) = val
  end subroutine

  subroutine sys_set_y1_scy(val)
    real(8), intent(in) :: val
    SYSTEM(15) = val
  end subroutine

  subroutine sys_set_scx(val)
    real(8), intent(in) :: val
    SYSTEM(16) = val
  end subroutine

  subroutine sys_set_x1_scx(val)
    real(8), intent(in) :: val
    SYSTEM(17) = val
  end subroutine

  subroutine sys_set_scy_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(18) = val
  end subroutine

  subroutine sys_set_scx_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(19) = val
  end subroutine

  subroutine sys_set_last_surf(val)
    real(8), intent(in) :: val
    SYSTEM(20) = val
  end subroutine

  subroutine sys_set_scy_fang(val)
    real(8), intent(in) :: val
    SYSTEM(21) = val
  end subroutine

  subroutine sys_set_y1_scy_fang(val)
    real(8), intent(in) :: val
    SYSTEM(22) = val
  end subroutine

  subroutine sys_set_scx_fang(val)
    real(8), intent(in) :: val
    SYSTEM(23) = val
  end subroutine

  subroutine sys_set_x1_scx_fang(val)
    real(8), intent(in) :: val
    SYSTEM(24) = val
  end subroutine

  subroutine sys_set_ref_surf(val)
    real(8), intent(in) :: val
    SYSTEM(25) = val
  end subroutine

  subroutine sys_set_astop(val)
    real(8), intent(in) :: val
    SYSTEM(26) = val
  end subroutine

  subroutine sys_set_astop_adj(val)
    real(8), intent(in) :: val
    SYSTEM(27) = val
  end subroutine

  subroutine sys_set_xz_bilateral(val)
    real(8), intent(in) :: val
    SYSTEM(28) = val
  end subroutine

  subroutine sys_set_gen_parax(val)
    real(8), intent(in) :: val
    SYSTEM(29) = val
  end subroutine

  subroutine sys_set_mode(val)
    real(8), intent(in) :: val
    SYSTEM(30) = val
  end subroutine

  subroutine sys_set_yz_mag_reset(val)
    real(8), intent(in) :: val
    SYSTEM(36) = val
  end subroutine

  subroutine sys_set_xz_mag_reset(val)
    real(8), intent(in) :: val
    SYSTEM(37) = val
  end subroutine

  subroutine sys_set_yz_mag_desired(val)
    real(8), intent(in) :: val
    SYSTEM(38) = val
  end subroutine

  subroutine sys_set_xz_mag_desired(val)
    real(8), intent(in) :: val
    SYSTEM(39) = val
  end subroutine

  subroutine sys_set_yz_mag_start(val)
    real(8), intent(in) :: val
    SYSTEM(40) = val
  end subroutine

  subroutine sys_set_xz_mag_start(val)
    real(8), intent(in) :: val
    SYSTEM(41) = val
  end subroutine

  subroutine sys_set_yz_mag_end(val)
    real(8), intent(in) :: val
    SYSTEM(42) = val
  end subroutine

  subroutine sys_set_xz_mag_end(val)
    real(8), intent(in) :: val
    SYSTEM(43) = val
  end subroutine

  subroutine sys_set_fno_flag_y(val)
    real(8), intent(in) :: val
    SYSTEM(44) = val
  end subroutine

  subroutine sys_set_fno_flag_x(val)
    real(8), intent(in) :: val
    SYSTEM(45) = val
  end subroutine

  subroutine sys_set_fno_hold_y(val)
    real(8), intent(in) :: val
    SYSTEM(46) = val
  end subroutine

  subroutine sys_set_fno_hold_x(val)
    real(8), intent(in) :: val
    SYSTEM(47) = val
  end subroutine

  subroutine sys_set_yz_bilateral(val)
    real(8), intent(in) :: val
    SYSTEM(48) = val
  end subroutine

  subroutine sys_set_xz_data_flag(val)
    real(8), intent(in) :: val
    SYSTEM(49) = val
  end subroutine

  subroutine sys_set_current_cfg(val)
    real(8), intent(in) :: val
    SYSTEM(50) = val
  end subroutine

  subroutine sys_set_y1_scy_set(val)
    real(8), intent(in) :: val
    SYSTEM(51) = val
  end subroutine

  subroutine sys_set_x1_scx_set(val)
    real(8), intent(in) :: val
    SYSTEM(52) = val
  end subroutine

  subroutine sys_set_y1_scy_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(53) = val
  end subroutine

  subroutine sys_set_x1_scx_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(54) = val
  end subroutine

  subroutine sys_set_sth_temp(val)
    real(8), intent(in) :: val
    SYSTEM(55) = val
  end subroutine

  subroutine sys_set_high_cfg(val)
    real(8), intent(in) :: val
    SYSTEM(56) = val
  end subroutine

  subroutine sys_set_ref_orient(val)
    real(8), intent(in) :: val
    SYSTEM(59) = val
  end subroutine

  subroutine sys_set_scy_set(val)
    real(8), intent(in) :: val
    SYSTEM(60) = val
  end subroutine

  subroutine sys_set_scx_set(val)
    real(8), intent(in) :: val
    SYSTEM(61) = val
  end subroutine

  subroutine sys_set_ray_aiming(val)
    real(8), intent(in) :: val
    SYSTEM(62) = val
  end subroutine

  subroutine sys_set_telecentric(val)
    real(8), intent(in) :: val
    SYSTEM(63) = val
  end subroutine

  subroutine sys_set_na_set(val)
    real(8), intent(in) :: val
    SYSTEM(64) = val
  end subroutine

  subroutine sys_set_naoy(val)
    real(8), intent(in) :: val
    SYSTEM(65) = val
  end subroutine

  subroutine sys_set_naox(val)
    real(8), intent(in) :: val
    SYSTEM(66) = val
  end subroutine

  subroutine sys_set_fno_val_set(val)
    real(8), intent(in) :: val
    SYSTEM(67) = val
  end subroutine

  subroutine sys_set_fno_val_y(val)
    real(8), intent(in) :: val
    SYSTEM(68) = val
  end subroutine

  subroutine sys_set_fno_val_x(val)
    real(8), intent(in) :: val
    SYSTEM(69) = val
  end subroutine

  subroutine sys_set_aplanatic_aim(val)
    real(8), intent(in) :: val
    SYSTEM(70) = val
  end subroutine

  subroutine sys_set_aim_offset_x(val)
    real(8), intent(in) :: val
    SYSTEM(81) = val
  end subroutine

  subroutine sys_set_aim_offset_y(val)
    real(8), intent(in) :: val
    SYSTEM(82) = val
  end subroutine

  subroutine sys_set_say_float(val)
    real(8), intent(in) :: val
    SYSTEM(83) = val
  end subroutine

  subroutine sys_set_sax_float(val)
    real(8), intent(in) :: val
    SYSTEM(84) = val
  end subroutine

  subroutine sys_set_wrx(val)
    real(8), intent(in) :: val
    SYSTEM(85) = val
  end subroutine

  subroutine sys_set_wry(val)
    real(8), intent(in) :: val
    SYSTEM(86) = val
  end subroutine

  subroutine sys_set_bdx(val)
    real(8), intent(in) :: val
    SYSTEM(87) = val
  end subroutine

  subroutine sys_set_bdy(val)
    real(8), intent(in) :: val
    SYSTEM(88) = val
  end subroutine

  subroutine sys_set_aim_offset_z(val)
    real(8), intent(in) :: val
    SYSTEM(89) = val
  end subroutine

  subroutine sys_set_tilt_ret(val)
    real(8), intent(in) :: val
    SYSTEM(90) = val
  end subroutine

  subroutine sys_set_autofunc(val)
    real(8), intent(in) :: val
    SYSTEM(91) = val
  end subroutine

  subroutine sys_set_pxim(val)
    real(8), intent(in) :: val
    SYSTEM(92) = val
  end subroutine

  subroutine sys_set_pyim(val)
    real(8), intent(in) :: val
    SYSTEM(93) = val
  end subroutine

  subroutine sys_set_pxim_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(94) = val
  end subroutine

  subroutine sys_set_pyim_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(95) = val
  end subroutine

  subroutine sys_set_rxim(val)
    real(8), intent(in) :: val
    SYSTEM(96) = val
  end subroutine

  subroutine sys_set_ryim(val)
    real(8), intent(in) :: val
    SYSTEM(97) = val
  end subroutine

  subroutine sys_set_rxim_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(98) = val
  end subroutine

  subroutine sys_set_ryim_fang_set(val)
    real(8), intent(in) :: val
    SYSTEM(99) = val
  end subroutine

  subroutine sys_set_reverse_trace(val)
    real(8), intent(in) :: val
    SYSTEM(100) = val
  end subroutine

  subroutine sys_set_verbose_optim(val)
    real(8), intent(in) :: val
    SYSTEM(101) = val
  end subroutine

  subroutine sys_set_nss_present(val)
    real(8), intent(in) :: val
    SYSTEM(102) = val
  end subroutine

  subroutine sys_set_screen(val)
    real(8), intent(in) :: val
    SYSTEM(103) = val
  end subroutine

  subroutine sys_set_screen_surf(val)
    real(8), intent(in) :: val
    SYSTEM(104) = val
  end subroutine

  subroutine sys_set_screen_d(val)
    real(8), intent(in) :: val
    SYSTEM(105) = val
  end subroutine

  subroutine sys_set_screen_h(val)
    real(8), intent(in) :: val
    SYSTEM(106) = val
  end subroutine

  subroutine sys_set_screen_s(val)
    real(8), intent(in) :: val
    SYSTEM(107) = val
  end subroutine

  subroutine sys_set_screen_excl_angle(val)
    real(8), intent(in) :: val
    SYSTEM(108) = val
  end subroutine

  subroutine sys_set_fliprefx(val)
    real(8), intent(in) :: val
    SYSTEM(128) = val
  end subroutine

  subroutine sys_set_fliprefy(val)
    real(8), intent(in) :: val
    SYSTEM(129) = val
  end subroutine

end module mod_system
