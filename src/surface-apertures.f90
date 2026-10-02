! Surface apertures: the clear-aperture / obscuration check of the legacy ray
! tracer as a pure routine on plain data.
!
! A surface's apertures are everything CACHEK (raytra7.f90) and the erase
! routines it calls (CAERRS, COERRS in LDM12.f90) read about that surface: the
! clear aperture (CLAP, types 1..6) with its decenters and tilt, the
! obscuration (COBS), the CLAP ERASE and COBS ERASE regions, the vertex tables
! of irregular polygons (IPOLYX/IPOLYY), the footprint-block flag, and the
! MULTCLAP/MULTCOBS offset tables that the caller (the CACOCH block of
! real-ray-trace.f90) feeds to CACHEK as JK1/JK2/JK3.  The routines here are
! exact ports and are meant to give IDENTICAL results:
!
!   check_apertures  <-  CACHEK    (decision logic; RAYCOD, STOPP, /CACO/, SPDCD)
!   clap_erase       <-  CAERRS
!   cobs_erase       <-  COERRS
!   inside_closed    <-  INSID1    (point in polygon, boundary counts as inside)
!   inside_open      <-  INSID2    (point in polygon, boundary counts as outside)
!
! The branch order, the arithmetic order and the tolerance handling are the
! legacy ones.  The legacy code adds AIMTOL to the aperture dimensions; it is
! an argument here.  All printing (the MSG-gated RAY_FAILURE/SHOWIT calls) is
! output only and has been dropped.  The legacy routines also leave scratch
! values in the J_INSIDE COMMON (X0, Y0, XT, YT, NP); nothing reads them after
! the call and they are local here.
!
! Legacy quirks that are kept on purpose are marked "legacy quirk" below.
!
! This module may use ONLY iso_fortran_env: no legacy data module, ever.  The
! builder (mod_ray_trace_builder) fills surface_apertures from the legacy
! accessors.
module mod_surface_apertures
   use iso_fortran_env, only: real64
   implicit none
   private

   public :: surface_apertures, check_apertures, clap_erase, cobs_erase, &
             inside_closed, inside_open

   ! Legacy PII and TWOPII (DATMAI, set in INITKDP.f90) -- digit for digit.
   real(real64), parameter, public :: APER_PII = 3.14159265358979323846_real64
   real(real64), parameter, public :: APER_TWOPII = 2.0_real64*APER_PII

   ! Most vertices a polygon can have (legacy IPOLYX(1:200,...), ANGL(1:200)).
   integer, parameter, public :: APER_MAXPTS = 200

   ! Slots of ipoly_x / ipoly_y (the third index of legacy IPOLYX / IPOLYY).
   integer, parameter, public :: IPOLY_CLAP = 1, IPOLY_CLAP_ERASE = 2, &
                                 IPOLY_COBS = 3, IPOLY_COBS_ERASE = 4

   type :: surface_apertures
      integer :: surface = 0          ! R_I; reported back as RAYCOD(2)
      integer :: special_type = 0     ! ldm%getSurfSpecialType (CACOCH skips |24|)
      integer :: footblok_flag = 0    ! surf_footblok_flag (1 = skip the check)
      ! Clear aperture
      integer :: clap_type = 0                       ! surf_clap_type (0 none, 1..6)
      real(real64) :: clap_dim(5) = 0.0_real64       ! surf_clap_dim(s,1:5)
      real(real64) :: clap_tilt = 0.0_real64         ! surf_clap_tilt
      ! Obscuration
      integer :: cobs_type = 0                       ! surf_coat_type (0 none, 1..6)
      real(real64) :: cobs_dim(6) = 0.0_real64       ! surf_cobs_poly(s,1:6)
      ! CLAP ERASE
      integer :: clap_erase_type = 0                 ! surf_cobs_ape_type
      real(real64) :: clap_erase_dim(6) = 0.0_real64 ! surf_cobs_ape_data(s,1:6)
      ! COBS ERASE
      integer :: cobs_erase_type = 0                 ! surf_cobs_era_type
      real(real64) :: cobs_erase_dim(6) = 0.0_real64 ! surf_cobs_era_data(s,1:6)
      ! Irregular polygon vertices: IPOLYX(1:200,s,1:4), IPOLYY(1:200,s,1:4)
      real(real64) :: ipoly_x(APER_MAXPTS, 4) = 0.0_real64
      real(real64) :: ipoly_y(APER_MAXPTS, 4) = 0.0_real64
      ! Multiple apertures: surf_multi_clap_flag / MULTCLAP(1:n,1:3,s) stored as
      ! (1:3, 1:n) = (JK1, JK2, JK3) per entry, likewise for MULTCOBS.  CACHEK
      ! itself never reads them; the caller loops over them (see CACOCH in
      ! real-ray-trace.f90) and passes each entry as JK1/JK2/JK3.
      integer :: multi_clap_n = 0
      real(real64), allocatable :: multi_clap(:,:)
      integer :: multi_cobs_n = 0
      real(real64), allocatable :: multi_cobs(:,:)
   end type surface_apertures

contains

   ! ------------------------------------------------------------------------
   ! Exact port of INSID1: .true. if (x0,y0) is inside the np-gon (xt,yt) or on
   ! its boundary or on one of its points.  True means "not blocked".
   ! ------------------------------------------------------------------------
   pure logical function inside_closed(x0, y0, xt, yt, np)
      real(real64), intent(in) :: x0, y0
      real(real64), intent(in) :: xt(:), yt(:)
      integer, intent(in) :: np
      real(real64) :: tupi, angl(0:APER_MAXPTS), arg
      integer :: i, n

      n = min(np, APER_MAXPTS)
      angl = 0.0_real64
      tupi = APER_TWOPII
      if (real(y0) == real(yt(1)) .and. real(x0) == real(xt(1))) then
         inside_closed = .true.
         return
      else
         if (abs(yt(1) - y0) <= 1.0e-15_real64 .and. abs(xt(1) - x0) <= 1.0e-15_real64) then
            angl(1) = 0.0_real64
         else
            angl(1) = atan2(yt(1) - y0, xt(1) - x0)
         end if
         if (angl(1) < 0.0_real64) angl(1) = tupi + angl(1)
      end if
      do i = 2, n
         if (real(y0) == real(yt(i)) .and. real(x0) == real(xt(i))) then
            inside_closed = .true.
            return
         else
            if (abs(yt(i) - y0) <= 1.0e-15_real64 .and. abs(xt(i) - x0) <= 1.0e-15_real64) then
               angl(i) = 0.0_real64
            else
               angl(i) = atan2(yt(i) - y0, xt(i) - x0)
            end if
            if (angl(i) < 0.0_real64) angl(i) = tupi + angl(i)
         end if
      end do
      inside_closed = .true.
      arg = angl(1) - angl(n)
      if (arg < 0.0_real64) arg = arg + tupi
      if (arg > (APER_PII + 1.0e-10_real64)) then
         inside_closed = .false.
         return
      end if
      do i = n, 2, -1
         arg = angl(i) - angl(i - 1)
         if (arg < 0.0_real64) arg = arg + tupi
         if (arg > (APER_PII + 1.0e-10_real64)) then
            inside_closed = .false.
            return
         end if
      end do
   end function inside_closed

   ! ------------------------------------------------------------------------
   ! Exact port of INSID2: .true. if (x0,y0) is strictly inside the np-gon
   ! (false on its boundary or corner points).  True means "blocked".
   ! ------------------------------------------------------------------------
   pure logical function inside_open(x0, y0, xt, yt, np)
      real(real64), intent(in) :: x0, y0
      real(real64), intent(in) :: xt(:), yt(:)
      integer, intent(in) :: np
      real(real64) :: tupi, angl(0:APER_MAXPTS), arg
      integer :: i, n

      n = min(np, APER_MAXPTS)
      angl = 0.0_real64
      tupi = APER_TWOPII
      if (real(y0) == real(yt(1)) .and. real(x0) == real(xt(1))) then
         inside_open = .false.
         return
      else
         if (abs(yt(1) - y0) <= 1.0e-15_real64 .and. abs(xt(1) - x0) <= 1.0e-15_real64) then
            angl(1) = 0.0_real64
         else
            angl(1) = atan2(yt(1) - y0, xt(1) - x0)
         end if
         if (angl(1) < 0.0_real64) angl(1) = tupi + angl(1)
      end if
      do i = 2, n
         if (real(y0) == real(yt(i)) .and. real(x0) == real(xt(i))) then
            inside_open = .false.
            return
         else
            if (abs(yt(i) - y0) <= 1.0e-15_real64 .and. abs(xt(i) - x0) <= 1.0e-15_real64) then
               angl(i) = 0.0_real64
            else
               angl(i) = atan2(yt(i) - y0, xt(i) - x0)
            end if
            if (angl(i) < 0.0_real64) angl(i) = tupi + angl(i)
         end if
      end do
      inside_open = .true.
      arg = angl(1) - angl(n)
      if (arg < 0.0_real64) arg = arg + tupi
      if (arg > (APER_PII - 1.0e-10_real64)) then
         inside_open = .false.
         return
      end if
      do i = n, 2, -1
         arg = angl(i) - angl(i - 1)
         if (arg < 0.0_real64) arg = arg + tupi
         if (arg > (APER_PII - 1.0e-10_real64)) then
            inside_open = .false.
            return
         end if
      end do
   end function inside_open

   ! Rotate (xrd,yrd) by the angle a (radians): the XR/YR statements that every
   ! legacy aperture branch repeats, in the same arithmetic order.
   pure subroutine rotate_xy(a, xrd, yrd, xr, yr)
      real(real64), intent(in) :: a, xrd, yrd
      real(real64), intent(out) :: xr, yr
      xr = (xrd*cos(a)) + (yrd*sin(a))
      yr = (yrd*cos(a)) - (xrd*sin(a))
   end subroutine rotate_xy

   ! ------------------------------------------------------------------------
   ! Exact port of CAERRS: the ray (x,y) was blocked by the clear aperture; does
   ! the CLAP ERASE region cancel the block?  caeras is the erase type; ls is
   ! the /CACO/ LS value, which the legacy routine overwrites with 0 or 10.
   ! ------------------------------------------------------------------------
   pure subroutine clap_erase(ap, x, y, aimtol, caeras, ls)
      type(surface_apertures), intent(in) :: ap
      real(real64), intent(in) :: x, y, aimtol
      integer, intent(in) :: caeras
      real(real64), intent(inout) :: ls

      real(real64) :: xt(APER_MAXPTS), yt(APER_MAXPTS)
      real(real64) :: xr, yr, ls1, rs, xrd, yrd
      real(real64) :: x1, x2, x3, x4, y1, y2, y3, y4, x5, x6, x7, x8, y5, y6, y7, y8
      real(real64) :: xc1, xc2, xc3, xc4, yc1, yc2, yc3, yc4, rad2, maxsid
      real(real64) :: cs1, cs2, cs3, cs4, angle
      real(real64) :: x0, y0
      integer :: n, np, iii
      logical :: ins

      xt = 0.0_real64
      yt = 0.0_real64
      x5 = 0.0_real64; x6 = 0.0_real64; x7 = 0.0_real64; x8 = 0.0_real64
      y5 = 0.0_real64; y6 = 0.0_real64; y7 = 0.0_real64; y8 = 0.0_real64

      ! CAERAS=1 circular clap erase
      if (caeras == 1) then
         ls1 = 0.0_real64
         xr = x - ap%clap_erase_dim(4)
         yr = y - ap%clap_erase_dim(3)
         ls1 = sqrt((xr**2) + (yr**2))
         rs = sqrt(ap%clap_erase_dim(1)**2) + aimtol
         if (real(ls1) > real(rs)) then
            ls1 = 10.0_real64
         else
            ls1 = 0.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! CAERAS=2 rectangular clap erase
      if (caeras == 2) then
         ls1 = 0.0_real64
         x1 = -ap%clap_erase_dim(2) - aimtol
         y1 = ap%clap_erase_dim(1) + aimtol
         x2 = -ap%clap_erase_dim(2) - aimtol
         y2 = -ap%clap_erase_dim(1) - aimtol
         x3 = ap%clap_erase_dim(2) + aimtol
         y3 = -ap%clap_erase_dim(1) - aimtol
         x4 = ap%clap_erase_dim(2) + aimtol
         y4 = ap%clap_erase_dim(1) + aimtol
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_erase_dim(4)
         yrd = yrd - ap%clap_erase_dim(3)
         ! legacy quirk: the erase tilt (dim 6) is used as radians, not degrees
         call rotate_xy(ap%clap_erase_dim(6), xrd, yrd, xr, yr)
         xt(1) = x1; yt(1) = y1
         xt(2) = x2; yt(2) = y2
         xt(3) = x3; yt(3) = y3
         xt(4) = x4; yt(4) = y4
         x0 = xr
         y0 = yr
         np = 4
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! CAERAS=3 elliptical clap erase
      if (caeras == 3) then
         ls1 = 0.0_real64
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_erase_dim(4)
         yrd = yrd - ap%clap_erase_dim(3)
         call rotate_xy(ap%clap_erase_dim(6), xrd, yrd, xr, yr)
         ls = ((xr**2)/(ap%clap_erase_dim(2)**2)) + ((yr**2)/(ap%clap_erase_dim(1)**2))
         if (real(ls) > (1.0 + (aimtol**2))) then
            ls1 = 10.0_real64
         else
            ls1 = 0.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! CAERAS=4 racetrack clap erase
      if (caeras == 4) then
         ls1 = 0.0_real64
         if (ap%clap_erase_dim(1) <= ap%clap_erase_dim(2)) then
            maxsid = ap%clap_erase_dim(2)
         else
            maxsid = ap%clap_erase_dim(1)
         end if
         if (ap%clap_erase_dim(5) < maxsid) then
            n = 8
            x1 = -ap%clap_erase_dim(2) + ap%clap_erase_dim(5) - aimtol
            y1 = ap%clap_erase_dim(1) + aimtol
            x2 = -ap%clap_erase_dim(2) - aimtol
            y2 = ap%clap_erase_dim(1) - ap%clap_erase_dim(5) + aimtol
            x3 = -ap%clap_erase_dim(2) - aimtol
            y3 = -ap%clap_erase_dim(1) + ap%clap_erase_dim(5) - aimtol
            x4 = -ap%clap_erase_dim(2) + ap%clap_erase_dim(5) - aimtol
            y4 = -ap%clap_erase_dim(1) - aimtol
            x5 = ap%clap_erase_dim(2) - ap%clap_erase_dim(5) + aimtol
            y5 = -ap%clap_erase_dim(1) - aimtol
            x6 = ap%clap_erase_dim(2) + aimtol
            y6 = -ap%clap_erase_dim(1) + ap%clap_erase_dim(5) - aimtol
            x7 = ap%clap_erase_dim(2) + aimtol
            y7 = ap%clap_erase_dim(1) - ap%clap_erase_dim(5) + aimtol
            x8 = ap%clap_erase_dim(2) - ap%clap_erase_dim(5) + aimtol
            y8 = ap%clap_erase_dim(1) + aimtol
         else
            n = 4
            x1 = -ap%clap_erase_dim(2) - aimtol
            y1 = ap%clap_erase_dim(1) + aimtol
            x2 = -ap%clap_erase_dim(2) - aimtol
            y2 = -ap%clap_erase_dim(1) - aimtol
            x3 = ap%clap_erase_dim(2) + aimtol
            y3 = -ap%clap_erase_dim(1) - aimtol
            x4 = ap%clap_erase_dim(2) + aimtol
            y4 = ap%clap_erase_dim(1) + aimtol
         end if
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_erase_dim(4)
         yrd = yrd - ap%clap_erase_dim(3)
         call rotate_xy(ap%clap_erase_dim(6), xrd, yrd, xr, yr)
         if (n == 4) then
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
         else
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
            xt(5) = x5; yt(5) = y5
            xt(6) = x6; yt(6) = y6
            xt(7) = x7; yt(7) = y7
            xt(8) = x8; yt(8) = y8
         end if
         np = n
         x0 = xr
         y0 = yr
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         xc1 = -ap%clap_erase_dim(2) + ap%clap_erase_dim(5)
         yc1 = ap%clap_erase_dim(1) - ap%clap_erase_dim(5)
         cs1 = sqrt(((xr - xc1)**2) + ((yr - yc1)**2))
         xc2 = -ap%clap_erase_dim(2) + ap%clap_erase_dim(5)
         yc2 = -ap%clap_erase_dim(1) + ap%clap_erase_dim(5)
         cs2 = sqrt(((xr - xc2)**2) + ((yr - yc2)**2))
         xc3 = ap%clap_erase_dim(2) - ap%clap_erase_dim(5)
         yc3 = -ap%clap_erase_dim(1) + ap%clap_erase_dim(5)
         cs3 = sqrt(((xr - xc3)**2) + ((yr - yc3)**2))
         xc4 = ap%clap_erase_dim(2) - ap%clap_erase_dim(5)
         yc4 = ap%clap_erase_dim(1) - ap%clap_erase_dim(5)
         cs4 = sqrt(((xr - xc4)**2) + ((yr - yc4)**2))
         rad2 = sqrt(ap%clap_erase_dim(5)**2) + aimtol
         if (.not. ins .and. real(cs1) > real(rad2) .and. real(cs2) > real(rad2) .and. &
             real(cs3) > real(rad2) .and. real(cs4) > real(rad2)) then
            ls1 = 10.0_real64
         else
            ls1 = 0.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! CAERAS=5 polygon clap erase
      if (caeras == 5) then
         ls1 = 0.0_real64
         angle = 0.0_real64
         do iii = 1, min(int(ap%clap_erase_dim(2)), APER_MAXPTS)
            xt(iii) = ap%clap_erase_dim(1)*cos(angle + (APER_PII/2.0_real64))
            yt(iii) = ap%clap_erase_dim(1)*sin(angle + (APER_PII/2.0_real64))
            angle = angle + ((APER_TWOPII)/ap%clap_erase_dim(2))
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_erase_dim(4)
         yrd = yrd - ap%clap_erase_dim(3)
         call rotate_xy(ap%clap_erase_dim(6), xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%clap_erase_dim(2))
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! CAERAS=6 irregular polygon clap erase
      if (caeras == 6) then
         ls1 = 0.0_real64
         angle = 0.0_real64
         do iii = 1, min(int(ap%clap_erase_dim(2)), APER_MAXPTS)
            xt(iii) = ap%ipoly_x(iii, IPOLY_CLAP_ERASE)
            yt(iii) = ap%ipoly_y(iii, IPOLY_CLAP_ERASE)
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_erase_dim(4)
         yrd = yrd - ap%clap_erase_dim(3)
         call rotate_xy(ap%clap_erase_dim(6), xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%clap_erase_dim(2))
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if
   end subroutine clap_erase

   ! ------------------------------------------------------------------------
   ! Exact port of COERRS: the ray (x,y) was blocked by the obscuration; does
   ! the COBS ERASE region cancel the block?  Same conventions as clap_erase.
   ! ------------------------------------------------------------------------
   pure subroutine cobs_erase(ap, x, y, aimtol, coeras, ls)
      type(surface_apertures), intent(in) :: ap
      real(real64), intent(in) :: x, y, aimtol
      integer, intent(in) :: coeras
      real(real64), intent(inout) :: ls

      real(real64) :: xt(APER_MAXPTS), yt(APER_MAXPTS)
      real(real64) :: xr, yr, ls1, rs, xrd, yrd
      real(real64) :: x1, x2, x3, x4, y1, y2, y3, y4, x5, x6, x7, x8, y5, y6, y7, y8
      real(real64) :: xc1, xc2, xc3, xc4, yc1, yc2, yc3, yc4, rad2, maxsid
      real(real64) :: cs1, cs2, cs3, cs4, angle
      real(real64) :: x0, y0
      integer :: n, np, iii
      logical :: ins

      xt = 0.0_real64
      yt = 0.0_real64
      x5 = 0.0_real64; x6 = 0.0_real64; x7 = 0.0_real64; x8 = 0.0_real64
      y5 = 0.0_real64; y6 = 0.0_real64; y7 = 0.0_real64; y8 = 0.0_real64

      ! COERAS=1 circular cobs erase
      if (coeras == 1) then
         ls1 = 0.0_real64
         xr = x - ap%cobs_erase_dim(4)
         yr = y - ap%cobs_erase_dim(3)
         ls1 = sqrt((xr**2) + (yr**2))
         rs = sqrt(ap%cobs_erase_dim(1)**2) + aimtol
         if (real(ls1) > real(rs)) then
            ls1 = 10.0_real64
         else
            ls1 = 0.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! COERAS=2 rectangular cobs erase
      if (coeras == 2) then
         ls1 = 0.0_real64
         x1 = -ap%cobs_erase_dim(2) - aimtol
         y1 = ap%cobs_erase_dim(1) + aimtol
         x2 = -ap%cobs_erase_dim(2) - aimtol
         y2 = -ap%cobs_erase_dim(1) - aimtol
         x3 = ap%cobs_erase_dim(2) + aimtol
         y3 = -ap%cobs_erase_dim(1) - aimtol
         x4 = ap%cobs_erase_dim(2) + aimtol
         y4 = ap%cobs_erase_dim(1) + aimtol
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_erase_dim(4)
         yrd = yrd - ap%cobs_erase_dim(3)
         ! legacy quirk: the erase tilt (dim 6) is used as radians, not degrees
         call rotate_xy(ap%cobs_erase_dim(6), xrd, yrd, xr, yr)
         xt(1) = x1; yt(1) = y1
         xt(2) = x2; yt(2) = y2
         xt(3) = x3; yt(3) = y3
         xt(4) = x4; yt(4) = y4
         np = 4
         x0 = xr
         y0 = yr
         ins = inside_open(x0, y0, xt, yt, np)
         ! legacy quirk: INSID2 (true = strictly inside) is read as "not blocked"
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! COERAS=3 elliptical cobs erase
      if (coeras == 3) then
         ls1 = 0.0_real64
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_erase_dim(4)
         yrd = yrd - ap%cobs_erase_dim(3)
         call rotate_xy(ap%cobs_erase_dim(6), xrd, yrd, xr, yr)
         ls = ((xr**2)/(ap%cobs_erase_dim(2)**2)) + ((yr**2)/(ap%cobs_erase_dim(1)**2))
         ! legacy quirk: 1.0*(AIMTOL**2), where the clap version has 1.0+(AIMTOL**2)
         if (real(ls) > (1.0*(aimtol**2))) then
            ls1 = 10.0_real64
         else
            ls1 = 0.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! COERAS=4 racetrack cobs erase
      if (coeras == 4) then
         ls1 = 0.0_real64
         if (ap%cobs_erase_dim(1) <= ap%cobs_erase_dim(2)) then
            maxsid = ap%cobs_erase_dim(2)
         else
            maxsid = ap%cobs_erase_dim(1)
         end if
         if (ap%cobs_erase_dim(5) < maxsid) then
            n = 8
            x1 = -ap%cobs_erase_dim(2) + ap%cobs_erase_dim(5) - aimtol
            y1 = ap%cobs_erase_dim(1) + aimtol
            x2 = -ap%cobs_erase_dim(2) - aimtol
            y2 = ap%cobs_erase_dim(1) - ap%cobs_erase_dim(5) + aimtol
            x3 = -ap%cobs_erase_dim(2) - aimtol
            y3 = -ap%cobs_erase_dim(1) + ap%cobs_erase_dim(5) - aimtol
            x4 = -ap%cobs_erase_dim(2) + ap%cobs_erase_dim(5) - aimtol
            y4 = -ap%cobs_erase_dim(1) - aimtol
            x5 = ap%cobs_erase_dim(2) - ap%cobs_erase_dim(5) + aimtol
            y5 = -ap%cobs_erase_dim(1) - aimtol
            x6 = ap%cobs_erase_dim(2) + aimtol
            y6 = -ap%cobs_erase_dim(1) + ap%cobs_erase_dim(5) - aimtol
            x7 = ap%cobs_erase_dim(2) + aimtol
            y7 = ap%cobs_erase_dim(1) - ap%cobs_erase_dim(5) + aimtol
            x8 = ap%cobs_erase_dim(2) - ap%cobs_erase_dim(5) + aimtol
            y8 = ap%cobs_erase_dim(1) + aimtol
         else
            n = 4
            x1 = -ap%cobs_erase_dim(2) - aimtol
            y1 = ap%cobs_erase_dim(1) + aimtol
            x2 = -ap%cobs_erase_dim(2) - aimtol
            y2 = -ap%cobs_erase_dim(1) - aimtol
            x3 = ap%cobs_erase_dim(2) + aimtol
            y3 = -ap%cobs_erase_dim(1) - aimtol
            x4 = ap%cobs_erase_dim(2) + aimtol
            y4 = ap%cobs_erase_dim(1) + aimtol
         end if
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_erase_dim(4)
         yrd = yrd - ap%cobs_erase_dim(3)
         call rotate_xy(ap%cobs_erase_dim(6), xrd, yrd, xr, yr)
         if (n == 4) then
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
         else
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
            xt(5) = x5; yt(5) = y5
            xt(6) = x6; yt(6) = y6
            xt(7) = x7; yt(7) = y7
            xt(8) = x8; yt(8) = y8
         end if
         np = n
         x0 = xr
         ! legacy quirk: COERRS sets Y0=XR here (not YR) for the racetrack erase
         y0 = xr
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         xc1 = -ap%cobs_erase_dim(2) + ap%cobs_erase_dim(5)
         yc1 = ap%cobs_erase_dim(1) - ap%cobs_erase_dim(5)
         cs1 = sqrt(((xr - xc1)**2) + ((yr - yc1)**2))
         xc2 = -ap%cobs_erase_dim(2) + ap%cobs_erase_dim(5)
         yc2 = -ap%cobs_erase_dim(1) + ap%cobs_erase_dim(5)
         cs2 = sqrt(((xr - xc2)**2) + ((yr - yc2)**2))
         xc3 = ap%cobs_erase_dim(2) - ap%cobs_erase_dim(5)
         yc3 = -ap%cobs_erase_dim(1) + ap%cobs_erase_dim(5)
         cs3 = sqrt(((xr - xc3)**2) + ((yr - yc3)**2))
         xc4 = ap%cobs_erase_dim(2) - ap%cobs_erase_dim(5)
         yc4 = ap%cobs_erase_dim(1) - ap%cobs_erase_dim(5)
         cs4 = sqrt(((xr - xc4)**2) + ((yr - yc4)**2))
         rad2 = sqrt(ap%cobs_erase_dim(5)**2) + aimtol
         if (ins .or. real(cs1) > real(rad2) .or. real(cs2) > real(rad2) .or. &
             real(cs3) > real(rad2) .or. real(cs4) > real(rad2)) then
            ls1 = 10.0_real64
         else
            ls1 = 0.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! COERAS=5 polygon cobs erase
      if (coeras == 5) then
         ls1 = 0.0_real64
         angle = 0.0_real64
         do iii = 1, min(int(ap%cobs_erase_dim(2)), APER_MAXPTS)
            xt(iii) = ap%cobs_erase_dim(1)*cos(angle + (APER_PII/2.0_real64))
            yt(iii) = ap%cobs_erase_dim(1)*sin(angle + (APER_PII/2.0_real64))
            angle = angle + ((APER_TWOPII)/ap%cobs_erase_dim(2))
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_erase_dim(4)
         yrd = yrd - ap%cobs_erase_dim(3)
         call rotate_xy(ap%cobs_erase_dim(6), xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%cobs_erase_dim(2))
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if

      ! COERAS=6 irregular polygon cobs erase
      if (coeras == 6) then
         ls1 = 0.0_real64
         angle = 0.0_real64
         do iii = 1, min(int(ap%cobs_erase_dim(2)), APER_MAXPTS)
            xt(iii) = ap%ipoly_x(iii, IPOLY_COBS_ERASE)
            yt(iii) = ap%ipoly_y(iii, IPOLY_COBS_ERASE)
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_erase_dim(4)
         yrd = yrd - ap%cobs_erase_dim(3)
         call rotate_xy(ap%cobs_erase_dim(6), xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%cobs_erase_dim(2))
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls1 = 0.0_real64
         else
            ls1 = 10.0_real64
         end if
         if (ls1 == 0.0_real64) ls = 0.0_real64
         if (ls1 == 10.0_real64) ls = 10.0_real64
         return
      end if
   end subroutine cobs_erase

   ! ------------------------------------------------------------------------
   ! Exact port of CACHEK(JK1,JK2,JK3,CACOCA).
   !
   ! Inputs
   !   ap          the surface's apertures (ap%surface plays the role of R_I)
   !   x, y        the ray point on that surface (legacy R_X, R_Y)
   !   jk1..jk3    x offset, y offset and extra tilt (degrees) of a MULTCLAP /
   !               MULTCOBS entry; 0,0,0 for the plain check
   !   cacoca      0 = clear aperture and obscuration, 1 = clear aperture only,
   !               2 = obscuration only
   !   aimtol      legacy AIMTOL, added to the aperture dimensions
   !   nocobspsf   legacy NOCOBSPSF (/PSFCOBS/): ignore the obscuration
   ! Outputs
   !   code        RAYCOD(1): 0 pass, 6 blocked by the clear aperture, 7 blocked
   !               by the obscuration
   !   fail_surface RAYCOD(2): always ap%surface
   ! In/out: values the legacy routine leaves UNTOUCHED on some paths, so the
   ! caller's current value must go in and the legacy semantics come out.
   !   stopp       legacy STOPP: set to 0 on the early returns and to 1 when
   !               blocked; unchanged when the ray passes the full check
   !   ls          /CACO/ LS (reset to 0 first; 10 or 0 on a block, see below)
   !   caeras      /CACO/ CAERAS: set after the early returns, else unchanged
   !   coeras      /CACO/ COERAS: likewise
   !   spdcd1/2    /SPRA2/ SPDCD1, SPDCD2: set to (code, surface) when blocked
   !
   ! Dropped: the MSG-gated RAY_FAILURE/SHOWIT calls (output only).  CACHEK
   ! also declares SPDTRA/TCLPRF/INS-style locals it never reads.
   ! ------------------------------------------------------------------------
   pure subroutine check_apertures(ap, x, y, jk1, jk2, jk3, cacoca, aimtol, nocobspsf, &
                                   code, fail_surface, stopp, ls, caeras, coeras, &
                                   spdcd1, spdcd2)
      type(surface_apertures), intent(in) :: ap
      real(real64), intent(in) :: x, y, jk1, jk2, jk3
      integer, intent(in) :: cacoca
      real(real64), intent(in) :: aimtol
      logical, intent(in) :: nocobspsf
      integer, intent(out) :: code, fail_surface
      integer, intent(inout) :: stopp, caeras, coeras, spdcd1, spdcd2
      real(real64), intent(inout) :: ls

      real(real64) :: xt(APER_MAXPTS), yt(APER_MAXPTS)
      real(real64) :: xr, yr, rs, xrd, yrd, angle, a15, a22
      real(real64) :: x1, x2, x3, x4, y1, y2, y3, y4, x5, x6, x7, x8, y5, y6, y7, y8
      real(real64) :: xc1, xc2, xc3, xc4, yc1, yc2, yc3, yc4, rad2, maxsid
      real(real64) :: cs1, cs2, cs3, cs4
      real(real64) :: x0, y0
      integer :: i, caflg, coflg, n, np, iii
      logical :: ins

      xt = 0.0_real64
      yt = 0.0_real64
      x5 = 0.0_real64; x6 = 0.0_real64; x7 = 0.0_real64; x8 = 0.0_real64
      y5 = 0.0_real64; y6 = 0.0_real64; y7 = 0.0_real64; y8 = 0.0_real64

      i = ap%surface
      ls = 0
      code = 0
      fail_surface = i
      ! VIIRS footprints: footblok on this surface skips the CLAP/COBS check
      if (ap%footblok_flag == 1) then
         stopp = 0
         return
      end if

      if (ap%clap_type == 0 .and. ap%cobs_type == 0) then
         ! no claps or cobs, just return
         stopp = 0
         return
      end if

      ! CAERAS, COERAS resolution
      if (ap%clap_erase_type > 0) then
         caeras = int(abs(ap%clap_erase_type))
      else
         caeras = 0
      end if
      if (ap%cobs_erase_type > 0) then
         coeras = int(abs(ap%cobs_erase_type))
      else
         coeras = 0
      end if

      ! CAFLG and COFLG
      if (ap%clap_type /= 0 .and. cacoca == 0 .or. ap%clap_type /= 0 .and. cacoca == 1) then
         caflg = int(abs(ap%clap_type))
      else
         caflg = 0
      end if
      if (ap%cobs_type /= 0 .and. cacoca == 0 .or. ap%cobs_type /= 0 .and. cacoca == 2) then
         coflg = int(abs(ap%cobs_type))
      else
         coflg = 0
      end if
      if (nocobspsf) coflg = 0

      !*****************************************************************
      ! CAFLG non-zero: resolve all CLAP blockages
      !*****************************************************************

      ! CAFLG=1 circular clap
      if (caflg == 1) then
         ls = 0.0_real64
         xr = x - ap%clap_dim(4) - jk1
         yr = y - ap%clap_dim(3) - jk2
         ls = sqrt((xr**2) + (yr**2))
         if (abs(ap%clap_dim(1)) <= abs(ap%clap_dim(2))) then
            rs = sqrt(ap%clap_dim(1)**2) + aimtol
         else
            rs = sqrt(ap%clap_dim(2)**2) + aimtol
         end if
         if (real(ls) > real(rs)) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (caeras /= 0 .and. ls == 10.0_real64) call clap_erase(ap, x, y, aimtol, caeras, ls)
         if (ls == 10.0_real64) then
            code = 6
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            stopp = 1
            return
         end if
      end if

      ! CAFLG=2 rectangular clap
      if (caflg == 2) then
         ls = 0.0_real64
         x1 = -ap%clap_dim(2) - aimtol
         y1 = ap%clap_dim(1) + aimtol
         x2 = -ap%clap_dim(2) - aimtol
         y2 = -ap%clap_dim(1) - aimtol
         x3 = ap%clap_dim(2) + aimtol
         y3 = -ap%clap_dim(1) - aimtol
         x4 = ap%clap_dim(2) + aimtol
         y4 = ap%clap_dim(1) + aimtol
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_dim(4) - jk1
         yrd = yrd - ap%clap_dim(3) - jk2
         a15 = (ap%clap_tilt + jk3)*APER_PII/180.0_real64
         call rotate_xy(a15, xrd, yrd, xr, yr)
         xt(1) = x1; yt(1) = y1
         xt(2) = x2; yt(2) = y2
         xt(3) = x3; yt(3) = y3
         xt(4) = x4; yt(4) = y4
         np = 4
         x0 = xr
         y0 = yr
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls = 0.0_real64
         else
            ls = 10.0_real64
         end if
         if (caeras /= 0 .and. ls == 10.0_real64) call clap_erase(ap, x, y, aimtol, caeras, ls)
         if (ls == 10.0_real64) then
            code = 6
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            stopp = 1
            return
         end if
      end if

      ! CAFLG=3 elliptical clap
      if (caflg == 3) then
         ls = 0.0_real64
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_dim(4) - jk1
         yrd = yrd - ap%clap_dim(3) - jk2
         ! legacy: A15 is computed twice; the first value is overwritten
         a15 = ap%clap_tilt*APER_PII/180.0_real64
         a15 = (ap%clap_tilt + jk3)*APER_PII/180.0_real64
         call rotate_xy(a15, xrd, yrd, xr, yr)
         ls = ((xr**2)/(ap%clap_dim(2)**2)) + ((yr**2)/(ap%clap_dim(1)**2))
         if (real(ls) > (1.0 + (aimtol**2))) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (caeras /= 0 .and. ls == 10.0_real64) call clap_erase(ap, x, y, aimtol, caeras, ls)
         if (ls == 10.0_real64) then
            code = 6
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            stopp = 1
            return
         end if
      end if

      ! CAFLG=4 racetrack clap
      if (caflg == 4) then
         ls = 0.0_real64
         if (ap%clap_dim(1) <= ap%clap_dim(2)) then
            maxsid = ap%clap_dim(2)
         else
            maxsid = ap%clap_dim(1)
         end if
         if (ap%clap_dim(5) < maxsid) then
            n = 8
            x1 = -ap%clap_dim(2) + ap%clap_dim(5) - aimtol
            y1 = ap%clap_dim(1) + aimtol
            x2 = -ap%clap_dim(2) - aimtol
            y2 = ap%clap_dim(1) - ap%clap_dim(5) + aimtol
            x3 = -ap%clap_dim(2) - aimtol
            y3 = -ap%clap_dim(1) + ap%clap_dim(5) - aimtol
            x4 = -ap%clap_dim(2) + ap%clap_dim(5) - aimtol
            y4 = -ap%clap_dim(1) - aimtol
            x5 = ap%clap_dim(2) - ap%clap_dim(5) + aimtol
            y5 = -ap%clap_dim(1) - aimtol
            x6 = ap%clap_dim(2) + aimtol
            y6 = -ap%clap_dim(1) + ap%clap_dim(5) - aimtol
            x7 = ap%clap_dim(2) + aimtol
            y7 = ap%clap_dim(1) - ap%clap_dim(5) + aimtol
            x8 = ap%clap_dim(2) - ap%clap_dim(5) + aimtol
            y8 = ap%clap_dim(1) + aimtol
         else
            n = 4
            x1 = -ap%clap_dim(2) - aimtol
            y1 = ap%clap_dim(1) + aimtol
            x2 = -ap%clap_dim(2) - aimtol
            y2 = -ap%clap_dim(1) - aimtol
            x3 = ap%clap_dim(2) + aimtol
            y3 = -ap%clap_dim(1) - aimtol
            x4 = ap%clap_dim(2) + aimtol
            y4 = ap%clap_dim(1) + aimtol
         end if
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_dim(4) - jk1
         yrd = yrd - ap%clap_dim(3) - jk2
         a15 = ap%clap_tilt*APER_PII/180.0_real64
         a15 = (ap%clap_tilt + jk3)*APER_PII/180.0_real64
         call rotate_xy(a15, xrd, yrd, xr, yr)
         if (n == 4) then
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
         else
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
            xt(5) = x5; yt(5) = y5
            xt(6) = x6; yt(6) = y6
            xt(7) = x7; yt(7) = y7
            xt(8) = x8; yt(8) = y8
         end if
         np = n
         x0 = xr
         y0 = yr
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls = 0.0_real64
         else
            ls = 10.0_real64
         end if
         ! is the point inside any of the four corner circles
         xc1 = -ap%clap_dim(2) + ap%clap_dim(5)
         yc1 = ap%clap_dim(1) - ap%clap_dim(5)
         cs1 = sqrt(((xr - xc1)**2) + ((yr - yc1)**2))
         xc2 = -ap%clap_dim(2) + ap%clap_dim(5)
         yc2 = -ap%clap_dim(1) + ap%clap_dim(5)
         cs2 = sqrt(((xr - xc2)**2) + ((yr - yc2)**2))
         xc3 = ap%clap_dim(2) - ap%clap_dim(5)
         yc3 = -ap%clap_dim(1) + ap%clap_dim(5)
         cs3 = sqrt(((xr - xc3)**2) + ((yr - yc3)**2))
         xc4 = ap%clap_dim(2) - ap%clap_dim(5)
         yc4 = ap%clap_dim(1) - ap%clap_dim(5)
         cs4 = sqrt(((xr - xc4)**2) + ((yr - yc4)**2))
         rad2 = sqrt(ap%clap_dim(5)**2) + aimtol
         if (.not. ins .and. real(cs1) > real(rad2) .and. real(cs2) > real(rad2) .and. &
             real(cs3) > real(rad2) .and. real(cs4) > real(rad2)) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (caeras /= 0 .and. ls == 10.0_real64) call clap_erase(ap, x, y, aimtol, caeras, ls)
         if (ls == 10.0_real64) then
            code = 6
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            ls = 0.0_real64
            stopp = 1
            return
         end if
      end if

      ! CAFLG=5 polygon clap
      if (caflg == 5) then
         ls = 0.0_real64
         angle = 0.0_real64
         do iii = 1, min(int(ap%clap_dim(2)), APER_MAXPTS)
            xt(iii) = ap%clap_dim(1)*cos(angle + (APER_PII/2.0_real64))
            yt(iii) = ap%clap_dim(1)*sin(angle + (APER_PII/2.0_real64))
            angle = angle + ((APER_TWOPII)/ap%clap_dim(2))
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_dim(4) - jk1
         yrd = yrd - ap%clap_dim(3) - jk2
         a15 = ap%clap_tilt*APER_PII/180.0_real64
         a15 = (ap%clap_tilt + jk3)*APER_PII/180.0_real64
         call rotate_xy(a15, xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%clap_dim(2))
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls = 0.0_real64
         else
            ls = 10.0_real64
         end if
         if (caeras /= 0 .and. ls == 10.0_real64) call clap_erase(ap, x, y, aimtol, caeras, ls)
         if (ls == 10.0_real64) then
            code = 6
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            ls = 0.0_real64
            stopp = 1
            return
         end if
      end if

      ! CAFLG=6 irregular polygon clap
      if (caflg == 6) then
         ls = 0.0_real64
         do iii = 1, min(int(ap%clap_dim(2)), APER_MAXPTS)
            xt(iii) = ap%ipoly_x(iii, IPOLY_CLAP)
            yt(iii) = ap%ipoly_y(iii, IPOLY_CLAP)
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_dim(4) - jk1
         yrd = yrd - ap%clap_dim(3) - jk2
         a15 = ap%clap_tilt*APER_PII/180.0_real64
         a15 = (ap%clap_tilt + jk3)*APER_PII/180.0_real64
         call rotate_xy(a15, xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%clap_dim(2))
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls = 0.0_real64
         else
            ls = 10.0_real64
         end if
         if (caeras /= 0 .and. ls == 10.0_real64) call clap_erase(ap, x, y, aimtol, caeras, ls)
         if (ls == 10.0_real64) then
            code = 6
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            ls = 0.0_real64
            stopp = 1
            return
         end if
      end if

      !*****************************************************************
      ! COFLG non-zero: resolve all COBS blockages
      !*****************************************************************

      ! COFLG=1 circular cobs
      if (coflg == 1) then
         ls = 0.0_real64
         xr = x - ap%cobs_dim(4) - jk1
         yr = y - ap%cobs_dim(3) - jk2
         ls = sqrt((xr**2) + (yr**2))
         rs = sqrt(ap%cobs_dim(1)**2) - aimtol
         if (real(ls) < real(rs)) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (coeras /= 0 .and. ls == 10.0_real64) call cobs_erase(ap, x, y, aimtol, coeras, ls)
         if (ls == 10.0_real64) then
            code = 7
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            stopp = 1
            return
         end if
      end if

      ! COFLG=2 rectangular cobs
      if (coflg == 2) then
         ls = 0.0_real64
         x1 = -ap%cobs_dim(2) - aimtol
         y1 = ap%cobs_dim(1) + aimtol
         x2 = -ap%cobs_dim(2) - aimtol
         y2 = -ap%cobs_dim(1) - aimtol
         x3 = ap%cobs_dim(2) + aimtol
         y3 = -ap%cobs_dim(1) - aimtol
         x4 = ap%cobs_dim(2) + aimtol
         y4 = ap%cobs_dim(1) + aimtol
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_dim(4) - jk1
         yrd = yrd - ap%cobs_dim(3) - jk2
         a22 = (ap%cobs_dim(6) + jk3)*APER_PII/180.0_real64
         call rotate_xy(a22, xrd, yrd, xr, yr)
         xt(1) = x1; yt(1) = y1
         xt(2) = x2; yt(2) = y2
         xt(3) = x3; yt(3) = y3
         xt(4) = x4; yt(4) = y4
         np = 4
         x0 = xr
         y0 = yr
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (coeras /= 0 .and. ls == 10.0_real64) call cobs_erase(ap, x, y, aimtol, coeras, ls)
         if (ls == 10.0_real64) then
            code = 7
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            stopp = 1
            return
         end if
      end if

      ! COFLG=3 elliptical cobs
      if (coflg == 3) then
         ls = 0.0_real64
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_dim(4) - jk1
         yrd = yrd - ap%cobs_dim(3) - jk2
         a22 = (ap%cobs_dim(6) + jk3)*APER_PII/180.0_real64
         call rotate_xy(a22, xrd, yrd, xr, yr)
         ls = ((xr**2)/(ap%cobs_dim(2)**2)) + ((yr**2)/(ap%cobs_dim(1)**2))
         if (real(ls) < (1.0 - (aimtol**2))) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (coeras /= 0 .and. ls == 10.0_real64) call cobs_erase(ap, x, y, aimtol, coeras, ls)
         if (ls == 10.0_real64) then
            code = 7
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            stopp = 1
            return
         end if
      end if

      ! COFLG=4 racetrack cobs
      if (coflg == 4) then
         ls = 0.0_real64
         if (ap%cobs_dim(1) <= ap%cobs_dim(2)) then
            maxsid = ap%cobs_dim(2)
         else
            maxsid = ap%cobs_dim(1)
         end if
         ! legacy quirk: the 8-sided cobs box shrinks by AIMTOL (+AIMTOL on the
         ! low side, -AIMTOL on the high side), the opposite of the clap box
         if (ap%cobs_dim(5) < maxsid) then
            n = 8
            x1 = -ap%cobs_dim(2) + ap%cobs_dim(5) + aimtol
            y1 = ap%cobs_dim(1) - aimtol
            x2 = -ap%cobs_dim(2) + aimtol
            y2 = ap%cobs_dim(1) - ap%cobs_dim(5) - aimtol
            x3 = -ap%cobs_dim(2) + aimtol
            y3 = -ap%cobs_dim(1) + ap%cobs_dim(5) + aimtol
            x4 = -ap%cobs_dim(2) + ap%cobs_dim(5) + aimtol
            y4 = -ap%cobs_dim(1) + aimtol
            x5 = ap%cobs_dim(2) - ap%cobs_dim(5) - aimtol
            y5 = -ap%cobs_dim(1) + aimtol
            x6 = ap%cobs_dim(2) - aimtol
            y6 = -ap%cobs_dim(1) + ap%cobs_dim(5) + aimtol
            x7 = ap%cobs_dim(2) - aimtol
            y7 = ap%cobs_dim(1) - ap%cobs_dim(5) - aimtol
            x8 = ap%cobs_dim(2) - ap%cobs_dim(5) - aimtol
            y8 = ap%cobs_dim(1) - aimtol
         else
            n = 4
            x1 = -ap%cobs_dim(2) - aimtol
            y1 = ap%cobs_dim(1) + aimtol
            x2 = -ap%cobs_dim(2) - aimtol
            y2 = -ap%cobs_dim(1) - aimtol
            x3 = ap%cobs_dim(2) + aimtol
            y3 = -ap%cobs_dim(1) - aimtol
            x4 = ap%cobs_dim(2) + aimtol
            y4 = ap%cobs_dim(1) + aimtol
         end if
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_dim(4) - jk1
         yrd = yrd - ap%cobs_dim(3) - jk2
         a22 = (ap%cobs_dim(6) + jk3)*APER_PII/180.0_real64
         call rotate_xy(a22, xrd, yrd, xr, yr)
         if (n == 4) then
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
         else
            xt(1) = x1; yt(1) = y1
            xt(2) = x2; yt(2) = y2
            xt(3) = x3; yt(3) = y3
            xt(4) = x4; yt(4) = y4
            xt(5) = x5; yt(5) = y5
            xt(6) = x6; yt(6) = y6
            xt(7) = x7; yt(7) = y7
            xt(8) = x8; yt(8) = y8
         end if
         np = n
         x0 = xr
         y0 = yr
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         ! is the point outside or on any of the four corner circles
         xc1 = -ap%cobs_dim(2) + ap%cobs_dim(5)
         yc1 = ap%cobs_dim(1) - ap%cobs_dim(5)
         cs1 = sqrt(((xr - xc1)**2) + ((yr - yc1)**2))
         xc2 = -ap%cobs_dim(2) + ap%cobs_dim(5)
         yc2 = -ap%cobs_dim(1) + ap%cobs_dim(5)
         cs2 = sqrt(((xr - xc2)**2) + ((yr - yc2)**2))
         xc3 = ap%cobs_dim(2) - ap%cobs_dim(5)
         yc3 = -ap%cobs_dim(1) + ap%cobs_dim(5)
         cs3 = sqrt(((xr - xc3)**2) + ((yr - yc3)**2))
         xc4 = ap%cobs_dim(2) - ap%cobs_dim(5)
         yc4 = ap%cobs_dim(1) - ap%cobs_dim(5)
         cs4 = sqrt(((xr - xc4)**2) + ((yr - yc4)**2))
         rad2 = sqrt(ap%cobs_dim(5)**2) - aimtol
         if (ins .or. real(cs1) < real(rad2) .or. real(cs2) < real(rad2) .or. &
             real(cs3) < real(rad2) .or. real(cs4) < real(rad2)) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (coeras /= 0 .and. ls == 10.0_real64) call cobs_erase(ap, x, y, aimtol, coeras, ls)
         if (ls == 10.0_real64) then
            code = 7
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            ls = 0.0_real64
            stopp = 1
            return
         end if
      end if

      ! COFLG=5 polygon cobs
      if (coflg == 5) then
         ls = 0.0_real64
         angle = 0.0_real64
         do iii = 1, min(int(ap%cobs_dim(2)), APER_MAXPTS)
            xt(iii) = ap%cobs_dim(1)*cos(angle + (APER_PII/2.0_real64))
            yt(iii) = ap%cobs_dim(1)*sin(angle + (APER_PII/2.0_real64))
            angle = angle + ((APER_TWOPII)/ap%cobs_dim(2))
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_dim(4) - jk1
         yrd = yrd - ap%cobs_dim(3) - jk2
         a22 = (ap%cobs_dim(6) + jk3)*APER_PII/180.0_real64
         call rotate_xy(a22, xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%cobs_dim(2))
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (coeras /= 0 .and. ls == 10.0_real64) call cobs_erase(ap, x, y, aimtol, coeras, ls)
         if (ls == 10.0_real64) then
            code = 7
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            ls = 0.0_real64
            stopp = 1
            return
         end if
      end if

      ! COFLG=6 irregular polygon cobs
      if (coflg == 6) then
         ls = 0.0_real64
         do iii = 1, min(int(ap%cobs_dim(2)), APER_MAXPTS)
            xt(iii) = ap%ipoly_x(iii, IPOLY_COBS)
            yt(iii) = ap%ipoly_y(iii, IPOLY_COBS)
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%cobs_dim(4) - jk1
         yrd = yrd - ap%cobs_dim(3) - jk2
         a22 = (ap%cobs_dim(6) + jk3)*APER_PII/180.0_real64
         call rotate_xy(a22, xrd, yrd, xr, yr)
         x0 = xr
         y0 = yr
         np = int(ap%cobs_dim(2))
         ins = inside_open(x0, y0, xt, yt, np)
         if (ins) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (coeras /= 0 .and. ls == 10.0_real64) call cobs_erase(ap, x, y, aimtol, coeras, ls)
         if (ls == 10.0_real64) then
            code = 7
            fail_surface = i
            spdcd1 = code
            spdcd2 = i
            ls = 0.0_real64
            stopp = 1
            return
         end if
      end if
   end subroutine check_apertures

end module mod_surface_apertures
