! Lens module (CODE V MOD, "eikonal-based lens"): the ray mapping, with no
! global state, shared by the ray engine (pure) and the paraxial traces.
!
! A module occupies surfaces Sk (front vertex) and Sk+1 (rear vertex).  Its
! front principal plane H lies MFF + MFL behind the front vertex and its rear
! principal plane H' lies MBF - MFL from the rear vertex (both 0 by default,
! MFF = -MFL, MBF = +MFL).  Between H and H' a ray keeps its height; at H it
! turns as an ideal lens of focal length f does, tan u' = tan u - y/f, which
! images an object at infinity perfectly onto the rear focal plane.
!
! Aberrations are a wavefront W(H, rho) in waves at the reference wavelength,
! H the field (relative to the maximum field MFD) and rho the pupil coordinate
! (relative to the radius MED/2 of the module's entrance pupil, MEN behind the
! front vertex):
!   W = C11 rho^4 + C12 (H.rho) rho^2 + C13 (H.rho)^2 + C14 |H|^2 (H.rho)
!     + C15 |H|^2 rho^2                                    (image terms)
!     + Cax(lambda) rho^2 + Clat(lambda) (H.rho)           (colour)
! with Cax through (MWL1..3, C1..C3) and Clat through (MWL1..3, C6..C8),
! quadratic in wavelength.  W is the OPD Zoa reports (chief path less ray
! path), so the module shortens the path by W*lambda; the ray errors are the
! textbook third-order ones, -(R/r') lambda dW/drho on the image plane (as in
! Designing Optics Using CODE V 7.1.1), so a fan is exactly the derivative
! of the OPD.
!
! Only an object at infinity is modelled (MRD 0): the field of a ray is its
! direction, tan(theta) = (L/N, M/N).
module lens_module_math
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: lm_params, lm_trace, lm_wavefront, lm_par_u, lm_par_y

  integer, parameter, public :: LM_FIELD_ANGLE = 0, LM_FIELD_IMG = 2

  type :: lm_params
    logical :: active = .false.
    real(real64) :: efl = 0.0_real64         ! MFL
    real(real64) :: mff = 0.0_real64         ! front vertex to front focal point
    real(real64) :: mbf = 0.0_real64         ! rear vertex to rear focal point
    logical :: mff_set = .false., mbf_set = .false.
    real(real64) :: men = 0.0_real64         ! front vertex to entrance pupil
    integer :: mfd_kind = LM_FIELD_ANGLE     ! MFD angle (deg) or IMG height
    real(real64) :: mfd = 0.0_real64
    real(real64) :: med = 0.0_real64         ! entrance pupil diameter
    real(real64) :: mwl(3) = [656.27_real64, 587.56_real64, 486.13_real64]   ! nm
    real(real64) :: c(15) = 0.0_real64       ! MCO C1..C15 (C4, C5, C9, C10 unused)
    ! set by the caller before tracing (lens units)
    real(real64) :: lam_ref = 0.0_real64     ! reference wavelength
  end type

contains

  ! Offsets of the principal planes: d1 = H from the front vertex,
  ! d2 = H' from the rear vertex.
  pure subroutine principal(p, d1, d2)
    type(lm_params), intent(in) :: p
    real(real64), intent(out) :: d1, d2
    d1 = 0.0_real64; d2 = 0.0_real64
    if (p%mff_set) d1 = p%mff + p%efl
    if (p%mbf_set) d2 = p%mbf - p%efl
  end subroutine

  ! Quadratic (Lagrange) value at w of the curve through (x(i), y(i)).
  pure real(real64) function quad3(x, y, w) result(v)
    real(real64), intent(in) :: x(3), y(3), w
    integer :: i, j
    real(real64) :: t
    v = 0.0_real64
    do i = 1, 3
      t = y(i)
      do j = 1, 3
        if (j /= i) then
          if (x(i) == x(j)) then
            t = 0.0_real64
          else
            t = t*(w - x(j))/(x(i) - x(j))
          end if
        end if
      end do
      v = v + t
    end do
  end function

  ! W (waves) and its gradient over rho, at field h and pupil rho, for a
  ! ray of wavelength lam_nm.
  pure subroutine lm_wavefront(p, h, rho, lam_nm, w, g)
    type(lm_params), intent(in) :: p
    real(real64), intent(in) :: h(2), rho(2), lam_nm
    real(real64), intent(out) :: w, g(2)
    real(real64) :: r2, hr, h2, cax, clat

    r2 = rho(1)**2 + rho(2)**2
    hr = h(1)*rho(1) + h(2)*rho(2)
    h2 = h(1)**2 + h(2)**2
    cax = quad3(p%mwl, p%c(1:3), lam_nm)
    clat = quad3(p%mwl, p%c(6:8), lam_nm)
    w = p%c(11)*r2*r2 + p%c(12)*hr*r2 + p%c(13)*hr*hr + p%c(14)*h2*hr + &
        p%c(15)*h2*r2 + cax*r2 + clat*hr
    g = p%c(11)*4.0_real64*r2*rho + p%c(12)*(h*r2 + 2.0_real64*hr*rho) + &
        p%c(13)*2.0_real64*hr*h + p%c(14)*h2*h + p%c(15)*2.0_real64*h2*rho + &
        cax*2.0_real64*rho + clat*h
  end subroutine

  ! Trace a ray across the module.  In: the ray at the front vertex plane
  ! (x, y, direction l m n, in the module's frame, travelling +z) and its
  ! wavelength lam_nm.  Out: direction after the module (lo mo no), the
  ! landing point on the rear vertex plane (xr, yr) and the optical path from
  ! the front vertex to the rear vertex.  ok = .false. if the ray cannot
  ! pass (travelling backwards, or no focal length).
  pure subroutine lm_trace(p, x, y, l, m, n, lam_nm, lo, mo, no, xr, yr, opl, ok)
    type(lm_params), intent(in) :: p
    real(real64), intent(in) :: x, y, l, m, n, lam_nm
    real(real64), intent(out) :: lo, mo, no, xr, yr, opl
    logical, intent(out) :: ok
    real(real64) :: d1, d2, tx, ty, xh, yh, f, sgn, d, tmax, h(2), rho(2), xp, yp
    real(real64) :: rp, w, g(2), ze, sp, mp, tnx, tny

    lo = l; mo = m; no = n; xr = x; yr = y; opl = 0.0_real64
    ok = .false.
    f = p%efl
    if (f == 0.0_real64 .or. n <= 0.0_real64) return
    call principal(p, d1, d2)
    tx = l/n
    ty = m/n
    ! to the front principal plane
    xh = x + d1*tx
    yh = y + d1*ty
    ! ideal lens: towards the image of the field point on the rear focal plane
    tnx = tx - xh/f
    tny = ty - yh/f
    sgn = sign(1.0_real64, f)
    d = sqrt((f*tx - xh)**2 + (f*ty - yh)**2 + f*f)
    ! the path that makes the outgoing wave a perfect sphere; zero for the
    ! ray through the centre of H
    opl = d1/n + sgn*abs(f)/n - (l*xh + m*yh) - sgn*d

    ! aberrations
    if (any(p%c /= 0.0_real64) .and. p%med > 0.0_real64) then
      select case (p%mfd_kind)
      case (LM_FIELD_IMG)
        tmax = p%mfd/f
      case default
        tmax = tan(p%mfd*acos(-1.0_real64)/180.0_real64)
      end select
      h = 0.0_real64
      if (tmax /= 0.0_real64) h = [tx, ty]/tmax
      xp = x + p%men*tx
      yp = y + p%men*ty
      rho = [xp, yp]/(p%med/2.0_real64)
      call lm_wavefront(p, h, rho, lam_nm, w, g)
      ! exit pupil radius: the module's image of the entrance pupil
      ! (sp from H', ze from H)
      ze = p%men - d1
      sp = 0.0_real64
      mp = 1.0_real64
      if (ze /= 0.0_real64) then
        sp = 1.0_real64/(1.0_real64/f + 1.0_real64/ze)
        mp = sp/ze
      end if
      rp = abs(mp)*p%med/2.0_real64
      if (rp > 0.0_real64) then
        ! Zoa's OPD is the chief ray's path less the ray's, so W waves of
        ! aberration (as MCO states it) is a path W*lambda shorter
        opl = opl - w*p%lam_ref
        ! the textbook third-order ray error on the image (rear focal) plane,
        ! eps = -(R/r') lambda dW/drho, R the exit pupil to image distance and
        ! r' the exit pupil radius: a slope change over the distance f from H'
        tnx = tnx - (f - sp)/f*p%lam_ref*g(1)/rp
        tny = tny - (f - sp)/f*p%lam_ref*g(2)/rp
      end if
    end if

    no = 1.0_real64/sqrt(1.0_real64 + tnx**2 + tny**2)
    lo = tnx*no
    mo = tny*no
    ! from H' back (or on) to the rear vertex
    xr = xh - d2*tnx
    yr = yh - d2*tny
    opl = opl - d2/no
    ok = .true.
  end subroutine

  ! Paraxial: slope after the module at front surface k, from the height
  ! at its front vertex and the slope arriving there.
  pure real(real64) function lm_par_u(p, y, u_in) result(u)
    type(lm_params), intent(in) :: p
    real(real64), intent(in) :: y, u_in
    real(real64) :: d1, d2
    call principal(p, d1, d2)
    u = u_in
    if (p%efl /= 0.0_real64) u = u_in - (y + d1*u_in)/p%efl
  end function

  ! Paraxial: height at the rear vertex from the front-vertex height and the
  ! slopes before and after the module (heights carry from H to H').
  pure real(real64) function lm_par_y(p, y, u_in, u_out) result(yr)
    type(lm_params), intent(in) :: p
    real(real64), intent(in) :: y, u_in, u_out
    real(real64) :: d1, d2
    call principal(p, d1, d2)
    yr = y + d1*u_in - d2*u_out
  end function

end module lens_module_math
