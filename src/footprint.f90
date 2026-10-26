! CODE V FOO: the beam footprint of every field on designated surfaces.
!
! A ray belongs to a beam only if it reaches the image (clear apertures and
! obscurations checked on every surface), and is drawn where it crosses each
! designated surface, in that surface's local coordinates.
!
!   BOU Y  boundary of each beam: along each pupil azimuth (AZI), the
!          outermost transmitted pupil radius, found to RAR (fraction of the
!          pupil); reference wavelength.
!   BOU N  a grid of NRD rays across the pupil diameter, every weighted
!          wavelength (CODE V's default for obscured systems).
module footprint
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: foo_beam, foo_trace, foo_default_nrd, foo_lens_obscured, &
            foo_resolve_surfaces, foo_aperture_outline

  ! One field's beam: ray k crosses designated surface j at xy(1:2, k, j);
  ! iw(k) is the ray's wavelength slot and pupil(1:2, k) its relative pupil
  ! point (x, y).
  type :: foo_beam
    integer :: n = 0
    integer, allocatable :: iw(:)
    real(real64), allocatable :: pupil(:,:), xy(:,:,:)
  end type

  real(real64), parameter :: COARSE = 0.05_real64

contains

  ! CODE V's NRD default: 20*sqrt(largest weight / sum of the weights).
  integer function foo_default_nrd() result(n)
    use mod_system, only: sys_wl_weight
    real(real64) :: wmax, wsum
    integer :: w
    wmax = 0.0_real64; wsum = 0.0_real64
    do w = 1, 10
      if (sys_wl_weight(w) <= 0.0_real64) cycle
      wmax = max(wmax, sys_wl_weight(w)); wsum = wsum + sys_wl_weight(w)
    end do
    n = 20
    if (wsum > 0.0_real64) n = max(2, nint(20.0_real64*sqrt(wmax/wsum)))
  end function

  ! True when any surface carries an obscuration (CODE V then defaults to
  ! BOU N, since a boundary search finds only each beam's outer edge).
  logical function foo_lens_obscured() result(obs)
    use mod_lens_data_manager, only: ldm
    ! (ALENS 16, the COBS shape, is behind the misnamed surf_coat_type; the
    ! ray-trace builder reads the obscuration type the same way)
    use mod_surface, only: surf_coat_type
    integer :: s
    obs = .false.
    do s = 0, ldm%getLastSurf()
      if (surf_coat_type(s) /= 0) obs = .true.
    end do
  end function

  ! A surface spec (Sk, SO/S0, SI, Si..j) as first/last surface; .false. if
  ! malformed or outside the lens.
  logical function foo_resolve_surfaces(spec, s0, s1) result(ok)
    use mod_lens_data_manager, only: ldm
    character(len=*), intent(in) :: spec
    integer, intent(out) :: s0, s1
    character(len=80) :: t
    integer :: d, ios
    ok = .false.; s0 = -1; s1 = -1
    t = adjustl(spec)
    if (t(1:1) /= 'S' .or. len_trim(t) < 2) return
    d = index(t, '..')
    if (d > 0) then
      s0 = surf_word(t(2:d-1))
      s1 = surf_word(t(d+2:))
    else
      s0 = surf_word(t(2:)); s1 = s0
    end if
    if (s0 < 0 .or. s1 < s0 .or. s1 > ldm%getLastSurf()) return
    ok = .true.
  contains
    integer function surf_word(w) result(s)
      character(len=*), intent(in) :: w
      s = -1
      select case (trim(w))
      case ('O'); s = 0
      case ('I'); s = ldm%getLastSurf()
      case default
        read(w, *, iostat=ios) s
        if (ios /= 0) s = -1
      end select
    end function
  end function

  ! Trace field f's beam (FOB at relativeFields(:, f)) and record where each
  ! transmitted ray crosses the surfaces surfs(:).  azi = first, last,
  ! increment (degrees) of the boundary search; ok false when no ray reached
  ! the image.
  subroutine foo_trace(f, surfs, boundary, nrd, rar, azi, beam, ok)
    use global_widgets, only: sysConfig
    use mod_system, only: sys_wl_weight
    use mod_ray_trace_engine, only: trace_context
    use mod_ray_trace_builder, only: build_trace_context
    integer, intent(in) :: f, surfs(:), nrd
    logical, intent(in) :: boundary
    real(real64), intent(in) :: rar, azi(3)
    type(foo_beam), intent(out) :: beam
    logical, intent(out) :: ok
    type(trace_context) :: ctx
    character(len=200) :: line
    real(real64) :: xy(2, size(surfs)), th, c, s, r, lo, hi, p, q, rlast
    real(real64), parameter :: PI = acos(-1.0_real64)
    integer :: na, k, i, j, w, iwr, maxn
    logical :: pass

    ok = .false.
    write(line, '(A,F0.8,1X,F0.8)') 'FOB ', sysConfig%relativeFields(2,f), &
    &  sysConfig%relativeFields(1,f)
    call PROCESSILENT(trim(line))
    call build_trace_context(ctx, check_apertures=.true.)

    if (boundary) then
      na = max(1, int((azi(2) - azi(1))/azi(3) + 1.0e-9_real64) + 1)
      maxn = na
    else
      maxn = nrd*nrd*10
    end if
    allocate(beam%iw(maxn), beam%pupil(2, maxn), beam%xy(2, maxn, size(surfs)))

    if (boundary) then
      iwr = sysConfig%getRefWavelengthIndex()
      do k = 1, na
        th = (azi(1) + (k-1)*azi(3))*PI/180.0_real64
        c = cos(th); s = sin(th)
        ! outermost transmitted radius along this azimuth: try the full
        ! pupil, else step inwards to the first transmitted radius, then
        ! bisect the edge between it and the step outside it
        call trace_one(ctx, s, c, iwr, surfs, xy, pass)
        if (pass) then
          call add(c, s, iwr, xy)
          cycle
        end if
        lo = -1.0_real64
        r = 1.0_real64 - COARSE
        do while (r > -0.5_real64*COARSE)
          call trace_one(ctx, max(r, 0.0_real64)*s, max(r, 0.0_real64)*c, iwr, surfs, xy, pass)
          if (pass) then
            lo = max(r, 0.0_real64); exit
          end if
          r = r - COARSE
        end do
        if (lo < 0.0_real64) cycle
        hi = min(1.0_real64, lo + COARSE)
        rlast = lo
        do while (hi - lo > rar)
          r = 0.5_real64*(lo + hi)
          call trace_one(ctx, r*s, r*c, iwr, surfs, xy, pass)
          if (pass) then
            lo = r
          else
            hi = r
          end if
        end do
        call trace_one(ctx, lo*s, lo*c, iwr, surfs, xy, pass)
        if (pass) call add(lo*c, lo*s, iwr, xy)
      end do
    else
      do w = 1, 10
        if (sys_wl_weight(w) <= 0.0_real64) cycle
        do j = 1, nrd
          q = -1.0_real64 + (2.0_real64*j - 1.0_real64)/nrd
          do i = 1, nrd
            p = -1.0_real64 + (2.0_real64*i - 1.0_real64)/nrd
            if (p*p + q*q > 1.0_real64) cycle
            call trace_one(ctx, q, p, w, surfs, xy, pass)
            if (pass) call add(p, q, w, xy)
          end do
        end do
      end do
    end if
    ok = beam%n > 0

  contains
    subroutine add(px, py, iw, pts)
      real(real64), intent(in) :: px, py, pts(:,:)
      integer, intent(in) :: iw
      if (beam%n >= maxn) return
      beam%n = beam%n + 1
      beam%iw(beam%n) = iw
      beam%pupil(:, beam%n) = [px, py]
      beam%xy(:, beam%n, :) = pts
    end subroutine
  end subroutine

  ! One ray at relative pupil (py, px), wavelength slot iw, to the image with
  ! apertures checked; xy(:, j) where it crosses surfs(j).  The engine when
  ! the lens is supported, else the legacy RAY CAOB command.
  subroutine trace_one(ctx, py, px, iw, surfs, xy, pass)
    use mod_ray_trace_engine, only: trace_context, ray_request, ray_result, trace_ray, &
                                    RAY_OK, RR_X, RR_Y
    use DATLEN, only: RAYRAY, RAYCOD
    type(trace_context), intent(in) :: ctx
    real(real64), intent(in) :: py, px
    integer, intent(in) :: iw, surfs(:)
    real(real64), intent(out) :: xy(:,:)
    logical, intent(out) :: pass
    type(ray_request) :: req
    type(ray_result) :: res
    character(len=200) :: line
    integer :: j

    xy = 0.0_real64
    if (ctx%supported) then
      req%py = py; req%px = px; req%iwl = iw
      call trace_ray(ctx, req, res)
      pass = res%status == RAY_OK
      if (.not. pass) return
      do j = 1, size(surfs)
        xy(1, j) = res%rr(RR_X, surfs(j))
        xy(2, j) = res%rr(RR_Y, surfs(j))
      end do
    else
      write(line, '(A,F0.10,1X,F0.10,1X,I0)') 'RAY CAOB ', py, px, iw
      call PROCESSILENT(trim(line))
      pass = RAYCOD(1) == 0
      if (.not. pass) return
      do j = 1, size(surfs)
        xy(:, j) = RAYRAY(1:2, surfs(j))
      end do
    end if
  end subroutine

  ! Closed outline of surface s's clear aperture in its local coordinates
  ! (circle, rectangle, ellipse, racetrack, with decenters and tilt).  A
  ! surface without one gets the circle about the vertex that just holds
  ! the traced beams (rbeam, the largest radius the caller's rays reach) --
  ! not the stored auto semi-aperture, which is half the beams' Y span from
  ! the last lens sizing.  n = 0 for the polygon types (not drawn).  label
  ! says which.
  subroutine foo_aperture_outline(s, rbeam, x, y, n, label)
    use mod_surface, only: surf_clap_type, surf_clap_dim, surf_clap_tilt
    integer, intent(in) :: s
    real(real64), intent(in) :: rbeam
    real(real64), allocatable, intent(out) :: x(:), y(:)
    integer, intent(out) :: n
    character(len=*), intent(out) :: label
    integer, parameter :: NP = 181
    real(real64), parameter :: PI = acos(-1.0_real64)
    real(real64) :: a, b, dx, dy, t, u, v, rc, cx, cy, ph
    integer :: k, q

    allocate(x(NP), y(NP))
    n = 0; label = ''
    a = abs(surf_clap_dim(s, 2))      ! X half-size
    b = abs(surf_clap_dim(s, 1))      ! Y half-size (radius for a circle)
    dy = surf_clap_dim(s, 3); dx = surf_clap_dim(s, 4)
    t = surf_clap_tilt(s)*PI/180.0_real64
    select case (surf_clap_type(s))
    case (0)
      b = rbeam
      if (b <= 0.0_real64) return
      a = b; dx = 0.0_real64; dy = 0.0_real64; t = 0.0_real64
      call ellipse()
      label = 'Beam'
    case (1)
      a = b
      call ellipse()
      label = 'CA'
    case (3)
      call ellipse()
      label = 'CA'
    case (2, 4)
      rc = 0.0_real64
      if (surf_clap_type(s) == 4) rc = min(abs(surf_clap_dim(s, 5)), a, b)
      ! rounded rectangle (rc = 0: plain rectangle), 45 points per corner
      n = 0
      do q = 0, 3
        cx = merge(a - rc, -(a - rc), q == 0 .or. q == 3)
        cy = merge(b - rc, -(b - rc), q <= 1)
        do k = 0, 44
          ph = (q*90.0_real64 + k*2.0_real64)*PI/180.0_real64
          n = n + 1
          call place(cx + rc*cos(ph), cy + rc*sin(ph), x(n), y(n))
        end do
      end do
      n = n + 1
      x(n) = x(1); y(n) = y(1)
      label = 'CA'
    end select
  contains
    subroutine ellipse()
      integer :: kk
      n = NP
      do kk = 1, NP
        ph = 2.0_real64*PI*(kk-1)/(NP-1)
        call place(a*cos(ph), b*sin(ph), x(kk), y(kk))
      end do
    end subroutine
    ! aperture frame -> surface frame (inverse of the blockage test's
    ! decenter-then-rotate, surface-apertures rotate_xy)
    subroutine place(xa, ya, xs, ys)
      real(real64), intent(in) :: xa, ya
      real(real64), intent(out) :: xs, ys
      u = xa; v = ya
      xs = dx + u*cos(t) - v*sin(t)
      ys = dy + u*sin(t) + v*cos(t)
    end subroutine
  end subroutine

end module footprint
