! Ray trace engine: a real ray tracer with no global state.
!
! Everything a trace needs arrives in a trace_context (built once, serially,
! by mod_ray_trace_builder) and a ray_request, and everything it produces goes
! into a ray_result.  trace_ray is PURE: the compiler rejects any write to a
! module variable or COMMON block and any I/O, so the same context can be
! traced from many threads at once.  This module must never use the legacy
! data modules (DATLEN, DATMAI, GLOBALS, mod_system, ...) -- the builder is the
! only bridge from that world.
!
! The engine re-implements real_ray_trace_core (src/real-ray-trace.f90) on the
! typed surface model in mod_surface_type.  Until a feature is ported, the
! builder marks the context unsupported and callers keep using the legacy
! tracer.  TRACECMP compares the two engine-for-engine.
!
! Per-surface results are kept in the same layout as the legacy RAYRAY array
! (rr(1:RR_N, obj:img), field meanings documented at the top of
! real-ray-trace.f90), so parity checks compare slot for slot, analyses read
! the indices they always have, and copying a result into RAYRAY for legacy
! callers is a single assignment.  Use the RR_* names below rather than bare
! numbers in new code.
module mod_ray_trace_engine
   use iso_fortran_env, only: real64
   use mod_surface_type, only: surface_type
   use mod_surface_placement, only: surface_placement, place_into_surface, &
                                    back_to_object, PLACE_PII
   use mod_surface_apertures, only: surface_apertures, check_apertures
   use mod_surface_interaction, only: surface_optics, hit_state, hit_and_interact, &
                                      HIT_OK
   use mod_ray_aiming, only: aim_settings, aim_state, compute_aim_target, getzee1, &
                             rayderiv, newdel, missref, adjust_last_surface
   implicit none
   private

   public :: trace_context, trace_surface, ray_request, ray_result, trace_ray

   ! ---- rr(:,s) slots: the legacy RAYRAY layout -------------------------
   integer, parameter, public :: RR_N         = 50
   integer, parameter, public :: RR_X         = 1    ! local position at surface
   integer, parameter, public :: RR_Y         = 2
   integer, parameter, public :: RR_Z         = 3
   integer, parameter, public :: RR_L         = 4    ! direction cosines after
   integer, parameter, public :: RR_M         = 5
   integer, parameter, public :: RR_N_DIR     = 6
   integer, parameter, public :: RR_OPL       = 7    ! OPL from s-1 to s
   integer, parameter, public :: RR_LEN       = 8    ! physical length s-1 to s
   integer, parameter, public :: RR_COSI      = 9    ! cos(incidence)
   integer, parameter, public :: RR_COSIP     = 10   ! cos(refraction)
   integer, parameter, public :: RR_UX        = 11   ! XZ slope angle (rad)
   integer, parameter, public :: RR_UY        = 12   ! YZ slope angle (rad)
   integer, parameter, public :: RR_LN        = 13   ! surface normal
   integer, parameter, public :: RR_MN        = 14
   integer, parameter, public :: RR_NN        = 15
   integer, parameter, public :: RR_XOLD      = 16   ! ray at s-1 in frame of s
   integer, parameter, public :: RR_YOLD      = 17
   integer, parameter, public :: RR_ZOLD      = 18
   integer, parameter, public :: RR_LOLD      = 19
   integer, parameter, public :: RR_MOLD      = 20
   integer, parameter, public :: RR_NOLD      = 21
   integer, parameter, public :: RR_OPL_TOTAL = 22   ! OPL from obj to s
   integer, parameter, public :: RR_RV        = 23   ! 1 normal, -1 reversed
   integer, parameter, public :: RR_POSRAY    = 24   ! 1 positive, -1 negative
   integer, parameter, public :: RR_ENERGY    = 25

   ! ---- status codes ------------------------------------------------------
   ! Non-negative values carry the legacy RAYCOD(1) meaning, so callers and
   ! the parity harness can compare them directly.  Negative values are the
   ! engine's own and never come from the legacy tracer.
   integer, parameter, public :: RAY_OK            = 0
   integer, parameter, public :: RAY_NOT_SUPPORTED = -1   ! context declined

   ! One surface of the lens, as the engine sees it.  Filled by the builder.
   type :: trace_surface
      class(surface_type), allocatable :: geom   ! copy of ldm%surfaces(s)%s
      real(real64) :: n_after(10) = 1.0_real64   ! index after s, per wavelength
      type(surface_placement) :: place           ! tilts/decenters/thickness (TRNSF2 data)
      type(surface_apertures) :: aper            ! clear aperture/obscuration/erase (CACHEK data)
      type(surface_optics) :: optics             ! indices, modes, flags (HITSUR/INTERACK data)
   end type

   ! Read-only inputs shared by every ray of one trace job (one field, one
   ! lens state).  Built by mod_ray_trace_builder::build_trace_context.
   type :: trace_context
      logical :: supported = .false.
      character(len=256) :: reason = 'context not built'
      integer :: obj = 0, ref = 1, img = 1      ! NEWOBJ, NEWREF, NEWIMG
      type(trace_surface), allocatable :: surf(:)    ! (obj:img)
      ! Chief ray of the current field (legacy REFRY), used to seed the
      ! launch/aim and as the OPD reference.
      logical :: chief_exists = .false.            ! REFEXT
      real(real64), allocatable :: chief(:,:)      ! (1:RR_N, obj:img)
      ! Ray aiming
      logical :: aim_on = .false.
      logical :: telecentric = .false.
      real(real64) :: aim_tol = 1.0e-10_real64     ! AIMTOL
      integer :: max_aim_iter = 100                ! NRAITR
      type(aim_settings) :: aim                    ! inputs of the aiming leaf routines (mod_ray_aiming)
      ! Clear aperture / obscuration blockage pass (legacy CACOCH = 1)
      logical :: check_apertures = .true.
      logical :: no_cobs_psf = .false.             ! NOCOBSPSF (/PSFCOBS/)
      real(real64) :: surtol = 0.0_real64          ! SURTOL, intersection tolerance
      ! Launch: paraxial marginal heights/slopes and the field (legacy PXTRAX,
      ! PXTRAY, LFOB), used to place the first aim point.
      real(real64) :: px_x1 = 0.0_real64           ! PXTRAX(1,NEWOBJ+1)
      real(real64) :: px_y1 = 0.0_real64           ! PXTRAY(1,NEWOBJ+1)
      real(real64) :: lfob1 = 0.0_real64           ! LFOB(1), relative Y field
      real(real64) :: lfob2 = 0.0_real64           ! LFOB(2), relative X field
      real(real64) :: px_x5_obj = 0.0_real64       ! PXTRAX(5,NEWOBJ)
      real(real64) :: px_y5_obj = 0.0_real64       ! PXTRAY(5,NEWOBJ)
      logical :: scx_set = .false.                 ! sys_scx() /= 0
      logical :: scy_set = .false.                 ! sys_scy() /= 0
      ! Legacy state that survives from one ray to the next.  Every engine ray
      ! starts from this snapshot instead, so results never depend on ray order.
      logical :: rvstart0 = .false.                ! RVSTART
      logical, allocatable :: dum0(:)              ! DUM(obj:img)
   end type

   ! One ray.  The pupil coordinates follow the legacy convention: px is the
   ! relative X aperture (legacy WW2), py the relative Y aperture (WW1).
   type :: ray_request
      real(real64) :: px = 0.0_real64
      real(real64) :: py = 0.0_real64
      integer      :: iwl = 1                      ! wavelength slot 1..10 (WW3)
      real(real64) :: weight = 1.0_real64          ! starting energy (WW4)
   end type

   type :: ray_result
      integer :: status = RAY_NOT_SUPPORTED        ! RAY_OK, legacy RAYCOD(1), or < 0
      integer :: fail_surface = -1                 ! legacy RAYCOD(2)
      integer :: aim_iterations = 0
      ! The exact legacy RAYCOD pair, for handing results back to legacy
      ! callers.  Unlike status it is not normalised on success (with the
      ! aperture pass off, legacy leaves RAYCOD(1) at -1 for a good ray).
      integer :: raycod(2) = -1
      ! Legacy NEWDEL calls MACFAL and clears REFEXT when every aiming
      ! derivative vanishes.  The engine only reports it; the caller decides,
      ! serially, whether to reproduce those global side effects.
      logical :: macfal_requested = .false.
      real(real64), allocatable :: rr(:,:)         ! (1:RR_N, obj:img)
   end type

contains

   ! Trace one ray through the context.  On return res%status is RAY_OK when
   ! the ray reached the image; otherwise it carries the failure code (legacy
   ! RAYCOD(1) meaning) and res%fail_surface the surface it failed at.
   ! res%rr is always allocated to the context's surface range, so callers can
   ! index it unconditionally.
   !
   ! This is real_ray_trace_core (src/real-ray-trace.f90) on its RAYTRA2 path
   ! (for_optimization = .true., the one CAPFN and the spot diagram use),
   ! assembled from the pure ports of its leaves.  Variable names follow the
   ! legacy ones so the two can be read side by side.  Not reproduced: the
   ! illumination-trace and NULL launch branches, global-ray output, coatings,
   ! screens and multi-hit surfaces (the builder declines those lenses), the
   ! polarization slots 26-40 beyond the launch vectors, and two dead paths:
   ! the STOPP retry after the surface loop (STOPP from HITSUR returns first
   ! and adjustLastSurface never sets it) and the outer_retry loop it fed.
   pure subroutine trace_ray(ctx, req, res)
      type(trace_context), intent(in)  :: ctx
      type(ray_request),   intent(in)  :: req
      type(ray_result),    intent(out) :: res

      real(real64), parameter :: PII = PLACE_PII
      integer :: obj, ref, img, iwl, kkk, i, jk, status
      integer :: code, failsurf, stopp, caeras, coeras, spdcd1, spdcd2
      real(real64) :: ww1, ww2, ww3, twopii
      real(real64) :: xstrt, ystrt, zstrt, jkx, jky
      real(real64) :: ddelx, ddely, large
      real(real64) :: x1one, y1one, x1last, y1last, rxone, ryone, rxlast, rylast
      real(real64) :: xc1, yc1, zc1, mag, lstart, mstart, nstart, xang, yang
      real(real64) :: x, y, z, l, m, n, xold, yold, zold, lold, mold, nold
      real(real64) :: xl, xm, xn, yl, ym, yn
      real(real64) :: rn1, snind2, tarx, tary, test, d11, d12, d21, d22, mf1, mf2
      real(real64) :: ls, pn(3)
      logical :: revstr, ninty, refmiss, delfail, macfal, blocked
      type(hit_state) :: st
      type(aim_state) :: ast
      logical, allocatable :: dum(:)

      obj = ctx%obj
      ref = ctx%ref
      img = ctx%img
      allocate(res%rr(RR_N, obj:img))
      res%rr = 0.0_real64
      res%aim_iterations = 0
      res%raycod = -1
      res%macfal_requested = .false.
      res%status = RAY_NOT_SUPPORTED
      res%fail_surface = obj

      if (.not. ctx%supported) return
      iwl = req%iwl
      if (iwl < 1 .or. iwl > 10) return

      twopii = 2.0_real64*PII
      pn = 0.0_real64
      ww1 = req%py
      ww2 = req%px
      ww3 = real(iwl, real64)
      allocate(dum(obj:img))
      dum = ctx%dum0
      st%rvstart = ctx%rvstart0
      ast%refext = ctx%chief_exists

      kkk = 0
      ddelx = 0.001_real64
      ddely = 0.001_real64
      res%raycod = -1
      large = -99999.9_real64
      x1one = large
      y1one = large
      x1last = large
      y1last = large
      rxone = large
      ryone = large
      rxlast = large
      rylast = large

      xstrt = ctx%chief(1, obj)
      ystrt = ctx%chief(2, obj)
      zstrt = ctx%chief(3, obj)

      ! ---- pupil scale on surface NEWOBJ+1 ------------------------------
      if (.not. ctx%telecentric) then
         jkx = ctx%px_x1
         jky = ctx%px_y1
         associate (ap => ctx%surf(obj+1)%aper)
         if (ap%clap_type == 1) then
            if (ap%clap_dim(1) <= ap%clap_dim(2)) then
               if (abs(ap%clap_dim(1)) < abs(ctx%px_x1)) jkx = abs(ap%clap_dim(1))
               if (abs(ap%clap_dim(1)) < abs(ctx%px_y1)) jky = abs(ap%clap_dim(1))
            else
               if (abs(ap%clap_dim(2)) < abs(ctx%px_x1)) jkx = abs(ap%clap_dim(2))
               if (abs(ap%clap_dim(2)) < abs(ctx%px_y1)) jky = abs(ap%clap_dim(2))
            end if
         end if
         if (ap%clap_type == 5) then
            if (abs(ap%clap_dim(1)) < abs(ctx%px_x1)) jkx = abs(ap%clap_dim(1))
            if (abs(ap%clap_dim(1)) < abs(ctx%px_y1)) jky = abs(ap%clap_dim(1))
         end if
         if (ap%clap_type == 6) then
            if (abs(ap%clap_dim(5)) < abs(ctx%px_x1)) jkx = abs(ap%clap_dim(5))
            if (abs(ap%clap_dim(5)) < abs(ctx%px_y1)) jky = abs(ap%clap_dim(5))
         end if
         if (ap%clap_type > 1 .and. ap%clap_type <= 4) then
            if (abs(ap%clap_dim(2)) < abs(ctx%px_x1)) jkx = abs(ap%clap_dim(2))
            if (abs(ap%clap_dim(1)) < abs(ctx%px_y1)) jky = abs(ap%clap_dim(1))
         end if
         end associate
      else
         jkx = ctx%px_x1
         jky = ctx%px_y1
      end if

      ! ---- first aim point (a chief ray always exists here) --------------
      if (.not. ctx%aim_on) then
         if (.not. ctx%telecentric) then
            ast%x1aim = ctx%chief(1, obj+1) + ww2*jkx
            ast%y1aim = ctx%chief(2, obj+1) + ww1*jky
            ast%z1aim = ctx%chief(3, obj+1)
         else
            ast%x1aim = ctx%chief(1, obj) + ww2*jkx
            ast%y1aim = ctx%chief(2, obj) + ww1*jky
            ast%z1aim = 0.0_real64
         end if
         ast%xc = ast%x1aim
         ast%yc = ast%y1aim
         ast%zc = ast%z1aim
         xc1 = ast%xc
         yc1 = ast%yc
         zc1 = ast%zc
      else
         ast%x1aim = ctx%chief(1, obj+1)
         ast%y1aim = ctx%chief(2, obj+1)
         ast%z1aim = ctx%chief(3, obj+1)
         ast%xc = ast%x1aim
         ast%yc = ast%y1aim
         ast%zc = ast%z1aim
         xc1 = ast%xc
         yc1 = ast%yc
         zc1 = ast%zc
         if (ast%xc < 0.0_real64) ddelx = -ddelx
         if (ast%yc < 0.0_real64) ddely = -ddely
      end if
      ast%xaimol = xc1
      ast%yaimol = yc1
      ast%zaimol = zc1
      ast%r_tx = ast%x1aim
      ast%r_ty = ast%y1aim
      ast%r_tz = ast%z1aim
      call back_to_object(ctx%surf(obj+1)%place, ctx%surf(obj)%place%thickness, &
                          ast%r_tx, ast%r_ty, ast%r_tz)
      ast%x1aim = ast%r_tx
      ast%y1aim = ast%r_ty
      ast%z1aim = ast%r_tz

      ! ---- aim iteration: trace, check the landing point at NEWREF, step --
      newton_raphson: do
         revstr = ctx%surf(obj)%place%thickness < 0.0_real64
         st%rv = .false.
         kkk = kkk + 1
         res%aim_iterations = kkk
         if (kkk > ctx%max_aim_iter) then
            res%raycod = [3, ref]
            res%status = 3
            res%fail_surface = ref
            return
         end if

         ! launch direction: from the object point towards the aim point
         mag = sqrt(((xstrt - ast%x1aim)**2) + ((ystrt - ast%y1aim)**2) + &
                    ((zstrt - ast%z1aim)**2))
         lstart = (ast%x1aim - xstrt)/mag
         mstart = (ast%y1aim - ystrt)/mag
         nstart = (ast%z1aim - zstrt)/mag
         ninty = .false.
         if (nstart < 0.0_real64) ninty = .true.
         if (ninty) st%rvstart = .true.
         if (nstart == 0.0_real64) then
            yang = PII/2.0_real64
            xang = PII/2.0_real64
         else
            if (abs(mstart) == 0.0_real64 .and. abs(nstart) == 0.0_real64) then
               yang = 0.0_real64
            else
               yang = atan2(mstart, nstart)
            end if
            if (abs(lstart) == 0.0_real64 .and. abs(nstart) == 0.0_real64) then
               xang = 0.0_real64
            else
               xang = atan2(lstart, nstart)
            end if
         end if

         res%rr(34:35, obj) = 1.0_real64
         res%rr(36:38, obj) = 0.0_real64
         res%rr(32, obj) = ww3
         res%rr(RR_X, obj) = xstrt
         res%rr(RR_Y, obj) = ystrt
         res%rr(RR_Z, obj) = zstrt
         res%rr(RR_L, obj) = lstart
         res%rr(RR_M, obj) = mstart
         res%rr(RR_N_DIR, obj) = nstart
         res%rr(RR_OPL, obj) = 0.0_real64
         res%rr(RR_LEN, obj) = 0.0_real64
         rn1 = ctx%surf(obj)%n_after(iwl)
         snind2 = abs(rn1)/rn1
         if (snind2 > 0.0_real64) res%rr(RR_POSRAY, obj) = 1.0_real64
         if (snind2 < 0.0_real64) res%rr(RR_POSRAY, obj) = -1.0_real64
         res%rr(RR_COSI, obj) = nstart
         res%rr(RR_COSIP, obj) = nstart
         res%rr(RR_UX, obj) = xang
         if (res%rr(RR_UX, obj) < 0.0_real64) res%rr(RR_UX, obj) = res%rr(RR_UX, obj) + twopii
         res%rr(RR_UY, obj) = yang
         if (res%rr(RR_UY, obj) < 0.0_real64) res%rr(RR_UY, obj) = res%rr(RR_UY, obj) + twopii
         res%rr(RR_LN, obj) = 0.0_real64
         res%rr(RR_MN, obj) = 0.0_real64
         res%rr(RR_NN, obj) = 1.0_real64
         res%rr(RR_XOLD, obj) = xstrt
         res%rr(RR_YOLD, obj) = ystrt
         res%rr(RR_ZOLD, obj) = zstrt
         res%rr(RR_LOLD, obj) = lstart
         res%rr(RR_MOLD, obj) = mstart
         res%rr(RR_NOLD, obj) = nstart
         res%rr(RR_OPL_TOTAL, obj) = 0.0_real64
         ! Polarization basis.  Legacy uses cos(XANG) for both the X and the Y
         ! vector's middle term (RAYRAY(30) and YM) -- kept for parity.
         res%rr(26, obj) = cos(xang)
         res%rr(27, obj) = 0.0_real64
         res%rr(28, obj) = -sin(xang)
         res%rr(29, obj) = 0.0_real64
         res%rr(30, obj) = cos(xang)
         res%rr(31, obj) = -sin(yang)

         x = xstrt
         y = ystrt
         z = zstrt
         l = lstart
         m = mstart
         n = nstart
         xl = cos(xang)
         xm = 0.0_real64
         xn = -sin(xang)
         yl = 0.0_real64
         ym = cos(xang)
         yn = -sin(yang)

         surface_loop: do i = obj+1, img
            call place_into_surface(ctx%surf(i-1)%place, ctx%surf(i)%place, x, y, z, l, m, n)
            xold = x
            yold = y
            zold = z
            lold = l
            mold = m
            nold = n

            st%x = x
            st%y = y
            st%z = z
            st%l = l
            st%m = m
            st%n = n
            st%dum = dum(i)
            st%raycod = res%raycod
            call hit_and_interact(ctx%surf(i)%geom, ctx%surf(i)%optics, ctx%surf(i-1)%optics, &
                                  i, obj, img, ww3, ctx%surtol, revstr, &
                                  ast%xaimol, ast%yaimol, ast%zaimol, st, status)
            if (status /= HIT_OK) then
               ! the builder gates these surfaces out; reaching here is a bug
               res%status = RAY_NOT_SUPPORTED
               res%fail_surface = i
               return
            end if
            dum(i) = st%dum
            res%raycod = st%raycod
            if (st%stopp == 1) then
               res%status = res%raycod(1)
               res%fail_surface = res%raycod(2)
               return
            end if
            x = st%x
            y = st%y
            z = st%z
            l = st%l
            m = st%m
            n = st%n
            if (st%rv) res%rr(RR_RV, i) = -1.0_real64
            if (.not. st%rv) res%rr(RR_RV, i) = 1.0_real64

            res%rr(32, i) = ww3
            res%rr(RR_X, i) = x
            res%rr(RR_Y, i) = y
            res%rr(RR_Z, i) = z
            res%rr(RR_L, i) = l
            res%rr(RR_M, i) = m
            res%rr(RR_N_DIR, i) = n
            res%rr(26, i) = (m*yn) - (n*ym)
            res%rr(27, i) = -((l*yn) - (n*yl))
            res%rr(28, i) = (l*ym) - (m*yl)
            res%rr(RR_COSI, i) = st%cosi
            res%rr(RR_COSIP, i) = st%cosip

            snind2 = abs(ctx%surf(i)%n_after(iwl))/ctx%surf(i)%n_after(iwl)
            res%rr(29, i) = yl
            res%rr(30, i) = ym
            res%rr(31, i) = yn
            res%rr(RR_LN, i) = st%ln
            res%rr(RR_MN, i) = st%mn
            res%rr(RR_NN, i) = st%nn
            res%rr(RR_XOLD, i) = xold
            res%rr(RR_YOLD, i) = yold
            res%rr(RR_ZOLD, i) = zold
            res%rr(RR_LOLD, i) = lold
            res%rr(RR_MOLD, i) = mold
            res%rr(RR_NOLD, i) = nold

            ! single-hit surfaces only (the builder gates NUMHITS /= 1)
            res%rr(RR_LEN, i) = sqrt(((res%rr(RR_Z, i) - zold)**2) + &
                                     ((res%rr(RR_Y, i) - yold)**2) + &
                                     ((res%rr(RR_X, i) - xold)**2))
            if (st%rv) res%rr(RR_LEN, i) = -res%rr(RR_LEN, i)
            if (abs(res%rr(RR_LEN, i)) >= 1.0e10_real64) res%rr(RR_LEN, i) = 0.0_real64

            if (snind2 > 0.0_real64) res%rr(RR_POSRAY, i) = 1.0_real64
            if (snind2 < 0.0_real64) res%rr(RR_POSRAY, i) = -1.0_real64

            res%rr(RR_OPL, i) = res%rr(RR_LEN, i)*abs(ctx%surf(i-1)%n_after(iwl))
            if (.not. st%rv) res%rr(RR_OPL, i) = res%rr(RR_OPL, i) + st%phase
            if (st%rv) res%rr(RR_OPL, i) = res%rr(RR_OPL, i) - st%phase

            if (l == 0.0_real64) then
               if (n >= 0.0_real64) res%rr(RR_UX, i) = 0.0_real64
               if (n < 0.0_real64) res%rr(RR_UX, i) = PII
            else
               if (abs(l) >= abs(1.0e35_real64*n)) then
                  if (l >= 0.0_real64) res%rr(RR_UX, i) = PII/2.0_real64
                  if (l < 0.0_real64) res%rr(RR_UX, i) = (3.0_real64*PII)/2.0_real64
               else
                  if (abs(l) == 0.0_real64 .and. abs(n) == 0.0_real64) then
                     res%rr(RR_UX, i) = 0.0_real64
                  else
                     res%rr(RR_UX, i) = atan2(l, n)
                  end if
                  if (res%rr(RR_UX, i) < 0.0_real64) res%rr(RR_UX, i) = res%rr(RR_UX, i) + twopii
               end if
            end if
            if (m == 0.0_real64) then
               if (n >= 0.0_real64) res%rr(RR_UY, i) = 0.0_real64
               if (n < 0.0_real64) res%rr(RR_UY, i) = PII
            else
               if (abs(m) >= abs(1.0e35_real64*n)) then
                  if (m >= 0.0_real64) res%rr(RR_UY, i) = PII/2.0_real64
                  if (m < 0.0_real64) res%rr(RR_UY, i) = (3.0_real64*PII)/2.0_real64
               else
                  if (abs(m) == 0.0_real64 .and. abs(n) == 0.0_real64) then
                     res%rr(RR_UY, i) = 0.0_real64
                  else
                     res%rr(RR_UY, i) = atan2(m, n)
                  end if
                  if (res%rr(RR_UY, i) < 0.0_real64) res%rr(RR_UY, i) = res%rr(RR_UY, i) + twopii
               end if
            end if
            res%rr(RR_OPL_TOTAL, i) = res%rr(RR_OPL_TOTAL, i-1) + res%rr(RR_OPL, i)
            if (i == img .and. ctx%surf(i)%place%thickness /= 0.0_real64) then
               call adjust_last_surface(i, obj, ctx%surf(i)%place%thickness, &
                                        ctx%surf(i-1)%n_after(iwl), st%phase, res%rr)
            end if

            if (i == ref) then
               call compute_aim_target(ctx%surf(ref)%aper, ctx%aim, ww1, ww2, tarx, tary)
               test = sqrt(((tarx - x)**2) + ((tary - y)**2))
               if (test <= ctx%aim_tol .or. .not. ctx%aim_on) then
                  refmiss = .false.
                  call missref(ctx%surf(ref)%aper, x, y, ctx%aim_tol, refmiss, ls)
                  cycle surface_loop
               end if

               x1one = x1last
               y1one = y1last
               x1last = ast%xaimol
               y1last = ast%yaimol
               rxone = rxlast
               ryone = rylast
               rxlast = x
               rylast = y

               if (kkk == 1) then
                  ast%x1aim = ast%xaimol + ddelx
                  ast%y1aim = ast%yaimol + ddely
                  ast%z1aim = ast%zaimol
                  ast%xc = ast%x1aim
                  ast%yc = ast%y1aim
                  ast%zc = ast%z1aim
                  xc1 = ast%xc
                  yc1 = ast%yc
                  zc1 = ast%zc
                  if (ctx%aim%surf1_curvature /= 0.0_real64) &
                     call getzee1(ctx%aim, ctx%surf(obj+1)%place, ctx%surf(obj)%place%thickness, &
                                  pn, xstrt, ystrt, zstrt, ast)
                  ast%x1aim = ast%xc
                  ast%y1aim = ast%yc
                  ast%z1aim = ast%zc
                  ast%xaimol = xc1
                  ast%yaimol = yc1
                  ast%zaimol = zc1
                  ast%r_tx = ast%x1aim
                  ast%r_ty = ast%y1aim
                  ast%r_tz = ast%z1aim
                  call back_to_object(ctx%surf(obj+1)%place, ctx%surf(obj)%place%thickness, &
                                      ast%r_tx, ast%r_ty, ast%r_tz)
                  ast%x1aim = ast%r_tx
                  ast%y1aim = ast%r_ty
                  ast%z1aim = ast%r_tz
                  cycle newton_raphson
               end if

               call rayderiv(x1last, y1last, x1one, y1one, rxone, ryone, rxlast, rylast, &
                             d11, d12, d21, d22)
               mf1 = tarx - rxlast
               mf2 = tary - rylast
               delfail = .false.
               macfal = .false.
               ast%raycod = res%raycod
               call newdel(ctx%aim, ctx%surf(obj+1)%place, ctx%surf(obj)%place%thickness, pn, &
                           xstrt, ystrt, zstrt, obj, mf1, mf2, d11, d12, d21, d22, &
                           ast, delfail, macfal)
               if (delfail) then
                  res%raycod = ast%raycod
                  res%status = res%raycod(1)
                  res%fail_surface = res%raycod(2)
                  res%macfal_requested = macfal
                  return
               end if
               cycle newton_raphson
            end if
         end do surface_loop
         exit newton_raphson
      end do newton_raphson

      ! ---- clear aperture / obscuration blockage pass (legacy CACOCH) -----
      blocked = .false.
      if (ctx%check_apertures) then
         stopp = 0
         ls = 0.0_real64
         caeras = 0
         coeras = 0
         spdcd1 = 0
         spdcd2 = 0
         cacoch_loop: do i = obj+1, img-1
            associate (ap => ctx%surf(i)%aper)
            if (abs(ap%special_type) /= 24) then
               if (ap%multi_clap_n == 0 .and. ap%multi_cobs_n == 0) then
                  call check_apertures(ap, res%rr(RR_X, i), res%rr(RR_Y, i), &
                                       0.0_real64, 0.0_real64, 0.0_real64, 0, ctx%aim_tol, &
                                       ctx%no_cobs_psf, code, failsurf, stopp, ls, &
                                       caeras, coeras, spdcd1, spdcd2)
                  res%raycod = [code, failsurf]
               else
                  if (ap%multi_clap_n /= 0) then
                     do jk = 1, ap%multi_clap_n
                        call check_apertures(ap, res%rr(RR_X, i), res%rr(RR_Y, i), &
                                             ap%multi_clap(1, jk), ap%multi_clap(2, jk), &
                                             ap%multi_clap(3, jk), 1, ctx%aim_tol, &
                                             ctx%no_cobs_psf, code, failsurf, stopp, ls, &
                                             caeras, coeras, spdcd1, spdcd2)
                        res%raycod = [code, failsurf]
                        if (res%raycod(1) == 0) then
                           stopp = 0
                           exit
                        end if
                     end do
                  end if
                  if (ap%multi_cobs_n /= 0) then
                     do jk = 1, ap%multi_cobs_n
                        call check_apertures(ap, res%rr(RR_X, i), res%rr(RR_Y, i), &
                                             ap%multi_cobs(1, jk), ap%multi_cobs(2, jk), &
                                             ap%multi_cobs(3, jk), 2, ctx%aim_tol, &
                                             ctx%no_cobs_psf, code, failsurf, stopp, ls, &
                                             caeras, coeras, spdcd1, spdcd2)
                        res%raycod = [code, failsurf]
                        if (res%raycod(1) /= 0) then
                           stopp = 1
                           exit
                        end if
                     end do
                  end if
               end if
            end if
            end associate
            if (stopp == 1) then
               blocked = .true.
               exit cacoch_loop
            end if
            stopp = 0
         end do cacoch_loop
      end if

      ! ---- energy: the starting weight carried through unchanged ---------
      ! (coatings, screens, gratings and grid surfaces are gated out)
      res%rr(RR_ENERGY, obj:img) = 0.0_real64
      res%rr(34:38, obj:img) = 0.0_real64
      do i = obj, img
         if (i == obj) then
            if (ast%refext) then
               res%rr(RR_ENERGY, i) = req%weight*ctx%chief(9, obj)
            else
               res%rr(RR_ENERGY, i) = req%weight
            end if
         else
            res%rr(RR_ENERGY, i) = res%rr(RR_ENERGY, i-1)
         end if
      end do

      if (blocked) then
         res%status = res%raycod(1)
         res%fail_surface = res%raycod(2)
      else
         res%status = RAY_OK
         res%fail_surface = -1
      end if
   end subroutine trace_ray

end module mod_ray_trace_engine
