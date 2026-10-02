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
   use mod_surface_placement, only: surface_placement
   use mod_surface_apertures, only: surface_apertures
   use mod_surface_interaction, only: surface_optics
   use mod_ray_aiming, only: aim_settings
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
      real(real64), allocatable :: rr(:,:)         ! (1:RR_N, obj:img)
   end type

contains

   ! Trace one ray through the context.  On return res%status is RAY_OK when
   ! the ray reached the image; otherwise it carries the failure code and
   ! res%fail_surface the surface it failed at.  res%rr is always allocated
   ! to the context's surface range, so callers can index it unconditionally.
   pure subroutine trace_ray(ctx, req, res)
      type(trace_context), intent(in)  :: ctx
      type(ray_request),   intent(in)  :: req
      type(ray_result),    intent(out) :: res

      allocate(res%rr(RR_N, ctx%obj:ctx%img))
      res%rr = 0.0_real64
      res%aim_iterations = 0

      if (.not. ctx%supported) then
         res%status = RAY_NOT_SUPPORTED
         res%fail_surface = ctx%obj
         return
      end if

      ! The surface loop lands in P2 (see the plan); until then the builder
      ! never marks a context supported, so this is unreachable.
      res%status = RAY_NOT_SUPPORTED
      res%fail_surface = ctx%obj
      if (.false.) res%status = req%iwl   ! keep req referenced until P2
   end subroutine trace_ray

end module mod_ray_trace_engine
