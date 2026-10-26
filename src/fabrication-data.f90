! CODE V FAB: fabrication data listing.
!
!   default  element listing: each glass piece (a glass-filled gap k..k+1;
!            a cemented doublet is two pieces) with its radii, centre and
!            edge thickness, clear aperture and edge diameters, glass and
!            thick-lens focal length in air (reference wavelength)
!   SUR Y    surface listing instead (radius, thickness, glass, clear
!            aperture diameter and the sag there)
!   IND Y    refractive index of each glass at each weighted wavelength
!   SPA Y    spacer table: the air gaps between elements, centre and edge
!   SAG ..   sag table of aspheric (or conic) surfaces against a reference
!            sphere (CUY, or the best fitting sphere with BES Y), with the
!            departure in fringes (FRI Y) and the slope (SLO Y)
!
! Diameters: the clear aperture is the surface's own (Y size for a
! non-circular one, marked *), else the ray-traced beam size; the edge
! diameter is the one the lens drawing uses for the element (CIR EDG, else
! the system edge factor).  Edge thicknesses are taken at the edge diameter.
module fab_data
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: fab_reset, fab_set_flag, fab_set_sag, fab_go

  logical :: surList = .false., indTab = .false., spaTab = .false.
  logical :: sagOn = .false., sagBes = .false., sagFri = .false., sagSlo = .false.
  integer :: sagSurf = -1                 ! -1 = every aspheric surface
  ! max_ht incr_ht ref_curv delta_excess min_ht; hasSag(k) false = default
  real(real64) :: sagPar(5) = 0.0_real64
  logical :: hasSag(5) = .false.

contains

  subroutine fab_reset()
    surList = .false.; indTab = .false.; spaTab = .false.
    sagOn = .false.; sagBes = .false.; sagFri = .false.; sagSlo = .false.
    sagSurf = -1; sagPar = 0.0_real64; hasSag = .false.
  end subroutine

  ! which = SUR | IND | SPA | BES | FRI | SLO
  subroutine fab_set_flag(which, on)
    character(len=*), intent(in) :: which
    logical, intent(in) :: on
    select case (which)
    case ('SUR'); surList = on
    case ('IND'); indTab = on
    case ('SPA'); spaTab = on
    case ('BES'); sagBes = on
    case ('FRI'); sagFri = on
    case ('SLO'); sagSlo = on
    end select
  end subroutine

  ! SAG [Sk] [max incr ref delta min]: surf -1 = all aspheres.
  subroutine fab_set_sag(surf, par, has)
    integer, intent(in) :: surf
    real(real64), intent(in) :: par(5)
    logical, intent(in) :: has(5)
    sagOn = .true.; sagSurf = surf; sagPar = par; hasSag = has
  end subroutine

  ! A glass piece follows surface k (not air, a mirror or the image).
  logical function glass_gap(k)
    use DATLEN, only: GLANAM
    integer, intent(in) :: k
    character(len=13) :: g
    g = adjustl(GLANAM(k, 2))
    select case (trim(g))
    case ('AIR', 'REFL', 'REFLTIR', 'REFLTIRO', 'LAST SURFACE', 'PERFECT', 'IDEAL', '')
      glass_gap = .false.
    case default
      glass_gap = .true.
    end select
  end function

  ! Clear aperture semi-diameter (Y) of surface s; star = non-circular.
  real(real64) function ca_semi(s, star)
    use mod_surface, only: surf_clap_type, surf_clap_dim
    use mod_lens_data_manager, only: ldm
    integer, intent(in) :: s
    logical, intent(out) :: star
    star = .false.
    if (surf_clap_type(s) == 0) then
      ca_semi = abs(ldm%getSurfAutoSemiY(s))
    else
      ca_semi = abs(surf_clap_dim(s, 1))
      star = surf_clap_type(s) /= 1
    end if
  end function

  ! Sag of surface s at radial height h (typed surface kernels).
  real(real64) function sag_at(s, h)
    use mod_lens_data_manager, only: ldm
    integer, intent(in) :: s
    real(real64), intent(in) :: h
    sag_at = 0.0_real64
    if (.not. allocated(ldm%surfaces)) return
    if (s < lbound(ldm%surfaces, 1) .or. s > ubound(ldm%surfaces, 1)) return
    if (.not. allocated(ldm%surfaces(s)%s)) return
    sag_at = ldm%surfaces(s)%s%sag(h*h)
  end function

  ! Sphere of curvature c, sag at height h (0 beyond its hemisphere).
  real(real64) function sphere_sag(c, h)
    real(real64), intent(in) :: c, h
    real(real64) :: q
    q = 1.0_real64 - (c*h)**2
    if (q < 0.0_real64) then
      sphere_sag = 0.0_real64
    else
      sphere_sag = c*h*h/(1.0_real64 + sqrt(q))
    end if
  end function

  character(len=12) function radius_text(c)
    real(real64), intent(in) :: c
    if (abs(c) < 1.0e-14_real64) then
      radius_text = '    INFINITY'
    else
      write(radius_text, '(F12.4)') 1.0_real64/c
    end if
  end function

  subroutine fab_go()
    use global_widgets, only: curr_lens_data, sysConfig
    use kdp_data_types, only: check_clear_apertures
    use mod_lens_data_manager, only: ldm
    use mod_surface, only: surf_curvature, surf_thickness, surf_refractive_index
    use mod_system, only: sys_wavelength, sys_wl_weight
    use DATLEN, only: GLANAM
    use lens_draw_apertures, only: lensDrawElementSemi
    use zoa_output, only: zoa_emit
    use type_utils, only: int2str
    character(len=1024) :: line
    character(len=20) :: units
    character(len=1) :: mk1, mk2
    integer :: last, k, n, iref, w, first, lastEl
    real(real64) :: c1, c2, t, nn, phi, edge, rho, ca1, ca2
    logical :: s1, s2

    ! Typed store and ray-traced apertures current (as ETH/CLI do).
    call ldm%load_surfaces_from_alens()
    call check_clear_apertures(curr_lens_data, ldm%surfaces)
    last = ldm%getLastSurf()
    units = sysConfig%getLensUnitsText()
    iref = sysConfig%getRefWavelengthIndex()
    if (iref < 1 .or. iref > 10) iref = 1

    call zoa_emit("FABRICATION DATA ("//trim(units)//"; * = non-circular aperture, Y size)", "black")
    if (.not. surList) then
      call zoa_emit("ELEMENT  SURFS         R FRONT      R BACK   CTR THI  EDGE THI"// &
      &  "  CA DIA F  CA DIA B  EDGE DIA  GLASS           EFL", "black")
      n = 0
      do k = 1, last - 1
        if (.not. glass_gap(k)) cycle
        n = n + 1
        c1 = surf_curvature(k); c2 = surf_curvature(k+1); t = surf_thickness(k)
        rho = lensDrawElementSemi(k, 1)
        edge = ldm%edge_thickness(k, rho)
        ca1 = ca_semi(k, s1); ca2 = ca_semi(k+1, s2)
        mk1 = merge('*', ' ', s1); mk2 = merge('*', ' ', s2)
        nn = abs(surf_refractive_index(k, iref))
        phi = (nn - 1.0_real64)*(c1 - c2) + (nn - 1.0_real64)**2*t*c1*c2/nn
        write(line, '(I7,2X,A6,2A12,2F10.4,F9.4,A1,F9.4,A1,F10.4,2X,A13)') n, &
        &  'S'//trim(int2str(k))//'-'//trim(int2str(k+1)), radius_text(c1), radius_text(c2), &
        &  t, edge, 2*ca1, mk1, 2*ca2, mk2, 2*rho, GLANAM(k, 2)
        if (abs(phi) > 1.0e-14_real64) then
          write(line(105:), '(F12.4)') 1.0_real64/phi
        else
          line(105:) = '    INFINITY'
        end if
        call zoa_emit(trim(line), "black")
      end do
      if (n == 0) call zoa_emit("  (no glass elements)", "black")
    else
      call zoa_emit("SURF       RADIUS   THICKNESS  GLASS            CA DIA    SAG AT CA", "black")
      do k = 0, last
        ca1 = ca_semi(k, s1)
        write(line, '(I4,A12,A12,2X,A13,F11.4,A1,F12.6)') k, radius_text(surf_curvature(k)), &
        &  thickness_text(surf_thickness(k)), GLANAM(k, 2), 2*ca1, merge('*', ' ', s1), sag_at(k, ca1)
        call zoa_emit(trim(line), "black")
      end do
    end if

    if (indTab) then
      call zoa_emit("REFRACTIVE INDICES", "black")
      line = 'SURF GLASS'
      n = 18
      do w = 1, 10
        if (sys_wl_weight(w) <= 0.0_real64) cycle
        write(line(n+1:n+11), '(F9.1,A)') 1000.0_real64*sys_wavelength(w), 'nm'
        n = n + 11
      end do
      call zoa_emit(trim(line), "black")
      do k = 1, last - 1
        if (.not. glass_gap(k)) cycle
        write(line, '(A4,1X,A13)') 'S'//trim(int2str(k)), adjustl(GLANAM(k, 2))
        n = 18
        do w = 1, 10
          if (sys_wl_weight(w) <= 0.0_real64) cycle
          write(line(n+1:n+11), '(F11.6)') abs(surf_refractive_index(k, w))
          n = n + 11
        end do
        call zoa_emit(trim(line), "black")
      end do
    end if

    if (spaTab) then
      ! the air gaps from the first element's back surface to the last's front
      first = -1; lastEl = -1
      do k = 1, last - 1
        if (glass_gap(k)) then
          if (first < 0) first = k
          lastEl = k
        end if
      end do
      call zoa_emit("SPACERS (air gaps between elements)", "black")
      call zoa_emit("  GAP        CTR THI   EDGE THI   AT DIA", "black")
      n = 0
      do k = first + 1, lastEl - 1
        if (first < 0) exit
        if (glass_gap(k)) cycle
        rho = max(lensDrawElementSemi(k, 1), lensDrawElementSemi(k+1, 1))
        write(line, '(A9,3F11.4)') 'S'//trim(int2str(k))//'-'//trim(int2str(k+1)), &
        &  surf_thickness(k), ldm%edge_thickness(k, rho), 2*rho
        call zoa_emit(trim(line), "black")
        n = n + 1
      end do
      if (n == 0) call zoa_emit("  (no air gaps between elements)", "black")
    end if

    if (sagOn) call sag_tables()

  contains

    character(len=12) function thickness_text(v)
      real(real64), intent(in) :: v
      if (abs(v) >= 1.0e10_real64) then
        thickness_text = '    INFINITY'
      else
        write(thickness_text, '(F12.4)') v
      end if
    end function

    subroutine sag_tables()
      use mod_surface, only: surf_is_asphere, surf_conic
      real(real64) :: hmax, hmin, dh, cref, de, h, z, zs, dep, lamr, slope, eps
      integer :: s, s0, s1x, i, nh, found
      logical :: star

      lamr = sys_wavelength(iref)*lens_units_per_micron()
      if (sagSurf >= 0) then
        s0 = sagSurf; s1x = sagSurf
      else
        s0 = 1; s1x = last - 1
      end if
      found = 0
      do s = s0, s1x
        if (sagSurf < 0 .and. .not. (surf_is_asphere(s) .or. surf_conic(s) /= 0.0_real64)) cycle
        found = found + 1
        hmax = ca_semi(s, star)
        if (hasSag(1)) hmax = sagPar(1)
        hmin = 0.0_real64
        if (hasSag(5)) hmin = sagPar(5)
        de = 0.0_real64
        if (hasSag(4)) de = sagPar(4)
        if (hmax <= hmin) then
          call zoa_emit("FAB: S"//trim(int2str(s))//": no sag table (zero height range)", "red")
          cycle
        end if
        if (hasSag(2)) then
          dh = sagPar(2)
        else
          dh = nice_step((hmax - hmin)/30.0_real64)
        end if
        nh = int((hmax - hmin)/dh + 1.0e-9_real64)
        if (nh > 1000) then
          call zoa_emit("FAB: S"//trim(int2str(s))//": more than 1000 sag steps", "red"); cycle
        end if
        cref = surf_curvature(s)
        if (hasSag(3)) cref = sagPar(3)
        ! best fitting sphere: through the vertex and the sag at max_ht
        if (sagBes) then
          z = sag_at(s, hmax) - de
          cref = 0.0_real64
          if (z /= 0.0_real64) cref = 2.0_real64*z/(hmax*hmax + z*z)
        end if
        call zoa_emit("SAG TABLE S"//trim(int2str(s))//" (reference sphere radius "// &
        &  trim(adjustl(radius_text(cref)))//"; departure = sag - sphere sag - delta)", "black")
        line = '     HEIGHT          SAG  SPHERE SAG    DEPARTURE'
        if (sagFri) line = trim(line)//'    FRINGES'
        if (sagSlo) line = trim(line)//'    SLOPE(RAD)'
        call zoa_emit(trim(line), "black")
        do i = 0, nh
          h = hmin + i*dh
          z = sag_at(s, h)
          zs = sphere_sag(cref, h)
          dep = z - zs - de
          write(line, '(F11.4,3F13.7)') h, z, zs, dep
          if (sagFri .and. lamr > 0.0_real64) write(line(len_trim(line)+1:), '(F11.3)') 2.0_real64*dep/lamr
          if (sagSlo) then
            eps = 1.0e-6_real64*max(1.0_real64, hmax)
            slope = (sag_at(s, h + eps) - sag_at(s, h - eps))/(2.0_real64*eps)
            write(line(len_trim(line)+1:), '(F14.7)') atan(slope)
          end if
          call zoa_emit(trim(line), "black")
        end do
        if (h < hmax - 1.0e-9_real64*max(1.0_real64, hmax)) then
          ! close the table at max_ht when the step does not land on it
          z = sag_at(s, hmax); zs = sphere_sag(cref, hmax); dep = z - zs - de
          write(line, '(F11.4,3F13.7)') hmax, z, zs, dep
          if (sagFri .and. lamr > 0.0_real64) write(line(len_trim(line)+1:), '(F11.3)') 2.0_real64*dep/lamr
          if (sagSlo) then
            eps = 1.0e-6_real64*max(1.0_real64, hmax)
            slope = (sag_at(s, hmax + eps) - sag_at(s, hmax - eps))/(2.0_real64*eps)
            write(line(len_trim(line)+1:), '(F14.7)') atan(slope)
          end if
          call zoa_emit(trim(line), "black")
        end if
      end do
      if (found == 0) call zoa_emit("FAB: no aspheric surfaces for a sag table", "black")
    end subroutine sag_tables

  end subroutine fab_go

  ! A round step (1, 2 or 5 times a power of ten) near x.
  real(real64) function nice_step(x)
    real(real64), intent(in) :: x
    real(real64) :: p, m
    if (x <= 0.0_real64) then
      nice_step = 1.0_real64; return
    end if
    p = 10.0_real64**floor(log10(x))
    m = x/p
    if (m < 1.5_real64) then
      nice_step = p
    else if (m < 3.5_real64) then
      nice_step = 2.0_real64*p
    else if (m < 7.5_real64) then
      nice_step = 5.0_real64*p
    else
      nice_step = 10.0_real64*p
    end if
  end function

  real(real64) function lens_units_per_micron()
    use mod_system, only: sys_units
    select case (nint(sys_units()))
    case (1); lens_units_per_micron = 3.93700787402d-5
    case (2); lens_units_per_micron = 1.0d-4
    case (4); lens_units_per_micron = 1.0d-6
    case default; lens_units_per_micron = 1.0d-3
    end select
  end function

end module fab_data
