module mod_surface_type
  use iso_fortran_env, only: real64
  implicit none

  integer, parameter :: SURF_DATA_SIZE      = 21
  integer, parameter :: SURF_PARAM_NAME_LEN = 24

  ! Clear aperture descriptor — shape + half-widths
  type :: aperture_data
    integer      :: shape  = 0
    real(real64) :: semi_x = 0.0_real64
    real(real64) :: semi_y = 0.0_real64
  end type

  ! Ray state at one surface — passed into/out of intersect and refract methods.
  ! Replaces the RAYRAY(1:50, surf) rows used by the legacy ray tracers.
  type :: surf_ray_data
    real(real64) :: x = 0.0_real64, y = 0.0_real64, z = 0.0_real64
    real(real64) :: l = 0.0_real64, m = 0.0_real64, n = 1.0_real64
    real(real64) :: ln = 0.0_real64, mn = 0.0_real64, nn = 1.0_real64
    real(real64) :: path = 0.0_real64, opl = 0.0_real64
    real(real64) :: n_in = 1.0_real64, n_out = 1.0_real64
    integer      :: surf_num = 0, wav_num = 1
    real(real64) :: wavelength = 0.55_real64
    real(real64) :: data(8) = 0.0_real64  ! extensibility: polarization, grating phase, etc.
  end type

  ! Abstract base type — every surface has these common fields.
  ! Surface-type-specific extra parameters live in data(1:num_params);
  ! param_names(i) gives the UI label for data(i).
  !
  ! Interfaces for the deferred procedures are declared below the concrete types
  ! (gfortran requires the type to be defined before the abstract interface
  ! body that references it via 'import').
  type, abstract :: surface_type
    real(real64)        :: radius    = huge(0.0_real64)  ! inf => flat surface
    real(real64)        :: thickness = 0.0_real64
    real(real64)        :: conic     = 0.0_real64        ! conic constant K
    type(aperture_data) :: clap
    real(real64)        :: n_pre(10)  = 1.0_real64       ! index before, per wavelength
    real(real64)        :: n_post(10) = 1.0_real64       ! index after, per wavelength
    character(len=SURF_PARAM_NAME_LEN) :: glass_name    = ' '
    character(len=SURF_PARAM_NAME_LEN) :: glass_catalog = ' '
    ! Surface-type-specific extra parameters (asphere coeffs, toric cv, etc.)
    real(real64)        :: data(SURF_DATA_SIZE)          = 0.0_real64
    character(len=SURF_PARAM_NAME_LEN) :: param_names(SURF_DATA_SIZE) = ' '
    integer             :: num_params = 0
  contains
    procedure(intersect_iface), deferred :: intersect
    procedure(refract_iface),   deferred :: refract
    procedure(paraxial_iface),  deferred :: paraxial_trace
  end type

  ! Sphere (or conic, or flat when radius=inf).
  ! No extra data() slots beyond radius/conic on the base type.
  type, extends(surface_type) :: sphere_surface
  contains
    procedure :: intersect      => sphere_intersect
    procedure :: refract        => sphere_refract
    procedure :: paraxial_trace => sphere_paraxial
  end type

  ! Even asphere: base conic + polynomial terms in rho^2.
  ! data slot convention (set by make_asphere):
  !   data(1)=A4   data(2)=A6   data(3)=A8   data(4)=A10
  !   data(5)=A12  data(6)=A14  data(7)=A16  data(8)=A18
  !   data(9)=A20  data(10)=A2  (A2 for plano-asphere correction)
  type, extends(surface_type) :: asphere_surface
  contains
    procedure :: intersect      => asphere_intersect
    procedure :: refract        => asphere_refract
    procedure :: paraxial_trace => asphere_paraxial
  end type

  ! Container wrapper required for heterogeneous polymorphic arrays.
  ! Fortran does not allow allocating CLASS arrays with mixed dynamic types;
  ! each element must be allocated individually with a concrete type.
  ! Usage: ldm%surfaces(i)%s%intersect(ray, tol)
  type :: surf_slot
    class(surface_type), allocatable :: s
  end type

  ! Abstract interfaces for the deferred procedures.
  ! These appear AFTER surface_type so that 'import :: surface_type' resolves.
  abstract interface
    subroutine intersect_iface(self, ray, tol)
      import :: surface_type, surf_ray_data, real64
      class(surface_type), intent(in)    :: self
      type(surf_ray_data), intent(inout) :: ray
      real(real64),        intent(in)    :: tol
    end subroutine
    subroutine refract_iface(self, ray)
      import :: surface_type, surf_ray_data
      class(surface_type), intent(in)    :: self
      type(surf_ray_data), intent(inout) :: ray
    end subroutine
    ! Paraxial refraction at a surface (surface refraction only; transfer is
    ! handled by the caller).  n_in and n_out are provided explicitly so the
    ! method works both with ALENS-backed data and self%n_pre/n_post.
    subroutine paraxial_iface(self, h_in, u_in, n_in, n_out, h_out, u_out)
      import :: surface_type, real64
      class(surface_type), intent(in)  :: self
      real(real64), intent(in)  :: h_in, u_in, n_in, n_out
      real(real64), intent(out) :: h_out, u_out
    end subroutine
  end interface

contains

  ! ---------------------------------------------------------------------------
  ! Factories
  ! ---------------------------------------------------------------------------

  function make_sphere(radius, thickness, conic, glass_name, glass_catalog) result(s)
    real(real64),     intent(in)           :: radius, thickness, conic
    character(len=*), intent(in), optional :: glass_name, glass_catalog
    type(sphere_surface) :: s
    s%radius     = radius
    s%thickness  = thickness
    s%conic      = conic
    s%num_params = 0
    if (present(glass_name))    s%glass_name    = glass_name
    if (present(glass_catalog)) s%glass_catalog = glass_catalog
  end function make_sphere

  function make_asphere(radius, thickness, conic, glass_name, glass_catalog, coeffs) result(s)
    real(real64),     intent(in)           :: radius, thickness, conic
    character(len=*), intent(in), optional :: glass_name, glass_catalog
    real(real64),     intent(in), optional :: coeffs(10)
    type(asphere_surface) :: s
    s%radius     = radius
    s%thickness  = thickness
    s%conic      = conic
    s%num_params = 10
    s%param_names(1)  = "A4";   s%param_names(2)  = "A6";   s%param_names(3)  = "A8"
    s%param_names(4)  = "A10";  s%param_names(5)  = "A12";  s%param_names(6)  = "A14"
    s%param_names(7)  = "A16";  s%param_names(8)  = "A18";  s%param_names(9)  = "A20"
    s%param_names(10) = "A2"
    if (present(coeffs))        s%data(1:10)    = coeffs
    if (present(glass_name))    s%glass_name    = glass_name
    if (present(glass_catalog)) s%glass_catalog = glass_catalog
  end function make_asphere

  ! ---------------------------------------------------------------------------
  ! Shared computational kernels (not type-bound)
  ! ---------------------------------------------------------------------------

  ! Evaluate conic surface sag and its derivative with respect to rho^2.
  ! sag(rho^2) = cv * rho^2 / (1 + sqrt(1 - (K+1) * cv^2 * rho^2))
  subroutine compute_conic_sag(cv, conic, rho2, sag, dsag_drho2)
    real(real64), intent(in)  :: cv, conic, rho2
    real(real64), intent(out) :: sag, dsag_drho2
    real(real64) :: q
    q = sqrt(max(1.0_real64 - (conic + 1.0_real64)*cv*cv*rho2, 1.0e-30_real64))
    sag = cv * rho2 / (1.0_real64 + q)
    dsag_drho2 = cv / (1.0_real64 + q)**2 * &
                 ((1.0_real64 + q) + (conic + 1.0_real64)*cv*cv*rho2 / (2.0_real64*q))
  end subroutine

  ! Newton-Raphson intersection with a conic surface.
  subroutine intersect_conic(radius, conic, ray, tol)
    real(real64),        intent(in)    :: radius, conic
    type(surf_ray_data), intent(inout) :: ray
    real(real64),        intent(in)    :: tol
    real(real64) :: cv, rho2, sag, ds, ft, dft, t, xi, yi, mag
    integer :: iter
    integer, parameter :: MAXITER = 50

    if (abs(radius) > 1.0e15_real64) then
      if (abs(ray%n) > 1.0e-15_real64) then
        t = -ray%z / ray%n
        ray%x = ray%x + ray%l*t;  ray%y = ray%y + ray%m*t;  ray%z = 0.0_real64
        ray%path = ray%path + abs(t)
        ray%ln = 0.0_real64;  ray%mn = 0.0_real64;  ray%nn = 1.0_real64
      end if
      return
    end if

    cv = 1.0_real64 / radius
    t  = -ray%z / ray%n

    do iter = 1, MAXITER
      xi = ray%x + ray%l*t;  yi = ray%y + ray%m*t
      rho2 = xi*xi + yi*yi
      call compute_conic_sag(cv, conic, rho2, sag, ds)
      ft = ray%z + ray%n*t - sag
      if (abs(ft) <= tol) exit
      dft = ray%n - 2.0_real64*ds*(xi*ray%l + yi*ray%m)
      if (abs(dft) < 1.0e-30_real64) exit
      t = t - ft/dft
    end do

    ray%x = ray%x + ray%l*t;  ray%y = ray%y + ray%m*t;  ray%z = ray%z + ray%n*t
    ray%path = ray%path + abs(t)

    rho2 = ray%x*ray%x + ray%y*ray%y
    call compute_conic_sag(cv, conic, rho2, sag, ds)
    ray%ln = -2.0_real64*ds*ray%x;  ray%mn = -2.0_real64*ds*ray%y;  ray%nn = 1.0_real64
    mag = sqrt(ray%ln**2 + ray%mn**2 + ray%nn**2)
    ray%ln = ray%ln/mag;  ray%mn = ray%mn/mag;  ray%nn = ray%nn/mag
  end subroutine intersect_conic

  ! Newton-Raphson intersection with an even asphere.
  ! coeffs(1:9)=A4,A6,A8,A10,A12,A14,A16,A18,A20;  coeffs(10)=A2 (plano term)
  subroutine intersect_asphere(radius, conic, coeffs, ray, tol)
    real(real64),        intent(in)    :: radius, conic, coeffs(10)
    type(surf_ray_data), intent(inout) :: ray
    real(real64),        intent(in)    :: tol
    real(real64) :: cv, rho2, rho4, rho6, rho8, rho10
    real(real64) :: sag, ds, poly, dpoly, ft, dft, t, xi, yi, mag
    integer :: iter
    integer, parameter :: MAXITER = 50

    cv = merge(0.0_real64, 1.0_real64/radius, abs(radius) > 1.0e15_real64)
    t  = merge(0.0_real64, -ray%z/ray%n, abs(ray%n) < 1.0e-15_real64)

    do iter = 1, MAXITER
      xi = ray%x + ray%l*t;  yi = ray%y + ray%m*t
      rho2  = xi*xi + yi*yi
      rho4  = rho2*rho2;  rho6 = rho4*rho2;  rho8 = rho6*rho2;  rho10 = rho8*rho2

      call compute_conic_sag(cv, conic, rho2, sag, ds)

      poly  = coeffs(10)*rho2 + coeffs(1)*rho4 + coeffs(2)*rho6 + &
              coeffs(3)*rho8 + coeffs(4)*rho10 + coeffs(5)*rho2*rho10 + &
              coeffs(6)*rho4*rho10 + coeffs(7)*rho6*rho10 + &
              coeffs(8)*rho8*rho10 + coeffs(9)*rho10*rho10

      dpoly = coeffs(10) + 2.0_real64*coeffs(1)*rho2  + 3.0_real64*coeffs(2)*rho4  + &
              4.0_real64*coeffs(3)*rho6  + 5.0_real64*coeffs(4)*rho8  + &
              6.0_real64*coeffs(5)*rho10 + 7.0_real64*coeffs(6)*rho2*rho10 + &
              8.0_real64*coeffs(7)*rho4*rho10 + 9.0_real64*coeffs(8)*rho6*rho10 + &
              10.0_real64*coeffs(9)*rho8*rho10

      ft = ray%z + ray%n*t - (sag + poly)
      if (abs(ft) <= tol) exit
      dft = ray%n - 2.0_real64*(ds + dpoly)*(xi*ray%l + yi*ray%m)
      if (abs(dft) < 1.0e-30_real64) exit
      t = t - ft/dft
    end do

    ray%x = ray%x + ray%l*t;  ray%y = ray%y + ray%m*t;  ray%z = ray%z + ray%n*t
    ray%path = ray%path + abs(t)

    rho2  = ray%x*ray%x + ray%y*ray%y
    rho4  = rho2*rho2;  rho6 = rho4*rho2;  rho8 = rho6*rho2;  rho10 = rho8*rho2
    call compute_conic_sag(cv, conic, rho2, sag, ds)
    dpoly = coeffs(10) + 2.0_real64*coeffs(1)*rho2  + 3.0_real64*coeffs(2)*rho4  + &
            4.0_real64*coeffs(3)*rho6  + 5.0_real64*coeffs(4)*rho8  + &
            6.0_real64*coeffs(5)*rho10 + 7.0_real64*coeffs(6)*rho2*rho10 + &
            8.0_real64*coeffs(7)*rho4*rho10 + 9.0_real64*coeffs(8)*rho6*rho10 + &
            10.0_real64*coeffs(9)*rho8*rho10

    ray%ln = -2.0_real64*(ds + dpoly)*ray%x
    ray%mn = -2.0_real64*(ds + dpoly)*ray%y
    ray%nn = 1.0_real64
    mag = sqrt(ray%ln**2 + ray%mn**2 + ray%nn**2)
    ray%ln = ray%ln/mag;  ray%mn = ray%mn/mag;  ray%nn = ray%nn/mag
  end subroutine intersect_asphere

  ! Vector Snell's law: updates ray direction cosines using surface normal and
  ! n_in/n_out stored in the ray record.  Flags TIR by negating n_out.
  subroutine apply_snell(ray)
    type(surf_ray_data), intent(inout) :: ray
    real(real64) :: mu, cos_i, cos_t, disc
    mu    = ray%n_in / ray%n_out
    cos_i = -(ray%l*ray%ln + ray%m*ray%mn + ray%n*ray%nn)
    disc  = 1.0_real64 - mu*mu*(1.0_real64 - cos_i*cos_i)
    if (disc < 0.0_real64) then
      ray%n_out = -abs(ray%n_out)
      return
    end if
    cos_t = sqrt(disc)
    ray%l = mu*ray%l + (mu*cos_i - cos_t)*ray%ln
    ray%m = mu*ray%m + (mu*cos_i - cos_t)*ray%mn
    ray%n = mu*ray%n + (mu*cos_i - cos_t)*ray%nn
    ray%opl = ray%opl + ray%path * ray%n_in
  end subroutine apply_snell

  ! Standard paraxial refraction: n'u' = nu - y(n'-n)c
  subroutine paraxial_refract(radius, h_in, u_in, n_in, n_out, h_out, u_out)
    real(real64), intent(in)  :: radius, h_in, u_in, n_in, n_out
    real(real64), intent(out) :: h_out, u_out
    real(real64) :: cv
    cv    = merge(0.0_real64, 1.0_real64/radius, abs(radius) > 1.0e15_real64)
    h_out = h_in
    u_out = (n_in*u_in - h_in*(n_out - n_in)*cv) / n_out
  end subroutine paraxial_refract

  ! ---------------------------------------------------------------------------
  ! sphere_surface methods
  ! ---------------------------------------------------------------------------

  subroutine sphere_intersect(self, ray, tol)
    class(sphere_surface), intent(in)    :: self
    type(surf_ray_data),   intent(inout) :: ray
    real(real64),          intent(in)    :: tol
    call intersect_conic(self%radius, self%conic, ray, tol)
  end subroutine

  subroutine sphere_refract(self, ray)
    class(sphere_surface), intent(in)    :: self
    type(surf_ray_data),   intent(inout) :: ray
    call apply_snell(ray)
  end subroutine

  subroutine sphere_paraxial(self, h_in, u_in, n_in, n_out, h_out, u_out)
    class(sphere_surface), intent(in) :: self
    real(real64), intent(in)  :: h_in, u_in, n_in, n_out
    real(real64), intent(out) :: h_out, u_out
    call paraxial_refract(self%radius, h_in, u_in, n_in, n_out, h_out, u_out)
  end subroutine

  ! ---------------------------------------------------------------------------
  ! asphere_surface methods
  ! ---------------------------------------------------------------------------

  subroutine asphere_intersect(self, ray, tol)
    class(asphere_surface), intent(in)    :: self
    type(surf_ray_data),    intent(inout) :: ray
    real(real64),           intent(in)    :: tol
    call intersect_asphere(self%radius, self%conic, self%data(1:10), ray, tol)
  end subroutine

  subroutine asphere_refract(self, ray)
    class(asphere_surface), intent(in)    :: self
    type(surf_ray_data),    intent(inout) :: ray
    call apply_snell(ray)
  end subroutine

  subroutine asphere_paraxial(self, h_in, u_in, n_in, n_out, h_out, u_out)
    class(asphere_surface), intent(in) :: self
    real(real64), intent(in)  :: h_in, u_in, n_in, n_out
    real(real64), intent(out) :: h_out, u_out
    ! A2 (data(10)) gives an effective curvature correction for plano-aspheres
    real(real64) :: cv_base, cv_eff, eff_radius
    cv_base    = merge(0.0_real64, 1.0_real64/self%radius, abs(self%radius) > 1.0e15_real64)
    cv_eff     = cv_base + 2.0_real64*self%data(10)
    eff_radius = merge(huge(0.0_real64), 1.0_real64/cv_eff, abs(cv_eff) < 1.0e-30_real64)
    call paraxial_refract(eff_radius, h_in, u_in, n_in, n_out, h_out, u_out)
  end subroutine

end module mod_surface_type
