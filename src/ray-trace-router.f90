! Routes the legacy ray-trace entry points (RAYTRA, RAYTRA2) through the
! global-free engine.
!
! Every legacy caller sets its ray up in globals (WW1..WW4, WVN, CACOCH,
! ANAAIM, ...) and reads the result back from globals (RAYRAY, RAYCOD,
! STOPP, RAYEXT, ...).  route_legacy_trace sits at the top of RAYTRA and
! RAYTRA2: it builds a context, traces the ray with trace_ray, and writes
! the result back exactly where and how the legacy core would -- including
! leaving RAYRAY untouched beyond the last surface an early failure reached,
! and reproducing the legacy side effects (MACFAL, the debug PRINT, the
! global ray output GLBRAY, and the plot-ray capture GLVERT/GLPRY that feeds
! the lens drawing) serially.  (DXFSET, the DXF drawing flag, does not reach
! the tracer: DXF output reads the same GRASET capture.)
!
! It declines -- and the caller runs the legacy core as before -- for an
! irregular wavelength setup or anything build_trace_context declines.
!
! The engine is pure and cannot print.  Where the legacy tracer prints a
! failure diagnostic (MSG: " RAY FAILURE OCCURRED AT SURFACE n" and a reason),
! the engine records the message ids (mod_ray_messages) in the result, and the
! router prints them serially with print_ray_messages -- the same output
! routines, order and text as legacy, so legacy's output suppression applies
! unchanged.
!
! RAYENGINE CHECK traces every routed ray with both tracers, keeps the
! legacy answer, and counts mismatches; RAYENGINE (no argument) reports the
! counters.  Not reproduced, and so left as the previous ray left them:
! RAYRAY slots 33 and 38-50 (polarization angles and reserved slots -- no
! caller reads them after a trace) and the tracer's scratch globals.
module mod_ray_trace_router
   use iso_fortran_env, only: real64
   implicit none
   private

   public :: route_legacy_trace, router_reset, router_report

   ! counters, reported by RAYENGINE
   integer, public :: rt_engine = 0          ! traced by the engine (ON)
   integer, public :: rt_checked = 0         ! traced by both (CHECK)
   integer, public :: rt_mismatch = 0        ! CHECK disagreements
   integer, public :: rt_decl_wave = 0       ! declined: WW3/WVN not a plain slot
   integer, public :: rt_decl_lens = 0       ! declined: build_trace_context
   ! ... broken down by build_trace_context's reason (surface numbers folded
   ! to '#'), so RAYENGINE shows what keeps rays on the legacy tracer
   integer, parameter :: MAX_REASONS = 16
   character(len=120) :: rt_reason(MAX_REASONS) = ''
   integer :: rt_reason_n(MAX_REASONS) = 0
   character(len=200), public :: rt_first_mismatch = ''

   ! RAYENGINE CHECK's copy of the global-ray / plot-capture globals
   real(real64), parameter :: PLOT_SENTINEL = -7.25e30_real64
   type :: plot_state
      logical :: globe = .false.
      integer :: glsurf = 0
      real(real64) :: off(6) = 0.0_real64
      real(real64) :: vertex(12, 0:499) = 0.0_real64
      real(real64) :: glray(12, 0:499) = 0.0_real64
      real(real64) :: glpray(9, 0:499) = 0.0_real64
      logical :: glvirt(0:499) = .false.
   end type

contains

   subroutine router_reset()
      rt_engine = 0
      rt_checked = 0
      rt_mismatch = 0
      rt_decl_wave = 0
      rt_decl_lens = 0
      rt_reason = ''
      rt_reason_n = 0
      rt_first_mismatch = ''
   end subroutine router_reset

   ! Tally one lens decline under its reason.
   subroutine count_reason(reason)
      character(len=*), intent(in) :: reason
      character(len=120) :: key
      integer :: k
      key = reason
      do k = 1, len_trim(key)
         if (key(k:k) >= '0' .and. key(k:k) <= '9') key(k:k) = '#'
      end do
      do k = 1, MAX_REASONS
         if (rt_reason(k) == key) then
            rt_reason_n(k) = rt_reason_n(k) + 1
            return
         end if
         if (len_trim(rt_reason(k)) == 0) then
            rt_reason(k) = key
            rt_reason_n(k) = 1
            return
         end if
      end do
   end subroutine count_reason

   subroutine router_report(lines, n)
      use type_utils, only: int2str
      character(len=*), intent(out) :: lines(:)
      integer, intent(out) :: n
      integer :: k
      n = 3
      lines(1) = 'legacy RAYTRA/RAYTRA2 calls: engine '//trim(int2str(rt_engine))// &
                 ', checked '//trim(int2str(rt_checked))// &
                 ', mismatches '//trim(int2str(rt_mismatch))
      lines(2) = 'declined: wavelength '//trim(int2str(rt_decl_wave))// &
                 ', lens '//trim(int2str(rt_decl_lens))
      lines(3) = ''
      if (rt_mismatch > 0) then
         lines(3) = 'first mismatch: '//trim(rt_first_mismatch)
      else
         n = 2
      end if
      do k = 1, MAX_REASONS
         if (rt_reason_n(k) == 0 .or. n >= size(lines)) exit
         n = n + 1
         lines(n) = '  lens '//trim(int2str(rt_reason_n(k)))//': '//trim(rt_reason(k))
      end do
   end subroutine router_report

   ! Called first by RAYTRA (for_opt = .false.) and RAYTRA2 (.true.).
   ! .true.: the ray is fully handled -- traced by the engine (ON), or by
   ! both tracers with the legacy answer kept (CHECK).  .false.: the caller
   ! must run the legacy core.
   logical function route_legacy_trace(for_opt) result(handled)
      use mod_ray_trace_builder, only: ray_engine_mode, ENGINE_OFF, ENGINE_CHECK, &
                                       build_trace_context
      use mod_ray_trace_engine, only: trace_context, ray_request, ray_result, trace_ray, &
                                      RAY_NOT_SUPPORTED
      use mod_ray_messages, only: print_ray_messages
      use real_ray_trace, only: real_ray_trace_core
      use DATLEN, only: WW1, WW2, WW3, WW4, WVN, CACOCH, ANAAIM, MSG
      logical, intent(in) :: for_opt
      type(trace_context) :: ctx
      type(ray_request) :: req
      type(ray_result) :: res
      integer :: iwl

      handled = .false.
      if (ray_engine_mode == ENGINE_OFF) return

      iwl = int(WW3)
      if (real(iwl, real64) /= WW3 .or. iwl < 1 .or. iwl > 10 .or. int(WVN) /= iwl) then
         rt_decl_wave = rt_decl_wave + 1
         return
      end if

      ! Built before any tracing: it snapshots the carried legacy state (DUM,
      ! RVSTART, REFEXT ...) that this ray starts from.
      call build_trace_context(ctx, check_apertures=(CACOCH == 1), ana_aim=ANAAIM)
      if (.not. ctx%supported) then
         rt_decl_lens = rt_decl_lens + 1
         call count_reason(ctx%reason)
         return
      end if

      req%py = WW1
      req%px = WW2
      req%iwl = iwl
      req%weight = WW4
      req%for_optimization = for_opt

      if (ray_engine_mode == ENGINE_CHECK) then
         call check_one(ctx, req, for_opt)
         handled = .true.
         return
      end if

      call trace_ray(ctx, req, res)
      if (res%status == RAY_NOT_SUPPORTED) then
         rt_decl_lens = rt_decl_lens + 1
         return
      end if
      ! legacy's diagnostics, printed before the ray's results are handed back
      ! (as legacy prints them while tracing; hand_back's own PRINT follows)
      if (res%n_msg > 0) call print_ray_messages(res%n_msg, res%msg_id, res%msg_surface)
      call hand_back(ctx, res, for_opt, .true.)
      rt_engine = rt_engine + 1
      handled = .true.
   end function route_legacy_trace

   ! Write an engine result into the legacy globals as the legacy core would
   ! leave them.  side_effects: also reproduce MACFAL and the debug PRINT
   ! (off when CHECK has already let the legacy core do them).
   subroutine hand_back(ctx, res, for_opt, side_effects)
      use mod_ray_trace_engine, only: trace_context, ray_result, FAIL_NONE, FAIL_WAVELENGTH, &
                                      FAIL_SURFACE, FAIL_AIM, FAIL_BLOCKED
      use DATLEN, only: RAYRAY, RAYCOD, STOPP, RAYEXT, POLEXT, FAIL, REFMISS, RVSTART, &
                        DUM, REFEXT, GLOBE, GRASET, GLSURF, OFFX, OFFY, OFFZ, OFFA, OFFB, OFFC
      use DATMAI, only: F34, F58
      use mod_lens_data_manager, only: ldm
      use zoa_output, only: zoa_emit
      type(trace_context), intent(in) :: ctx
      type(ray_result), intent(in) :: res
      logical, intent(in) :: for_opt, side_effects
      integer :: obj, img, ls, s, glsurf_found
      logical :: complete, want_macfal, capture
      logical :: SPDTRA
      COMMON/SPRA1/SPDTRA

      obj = ctx%obj
      img = ctx%img
      ls = res%last_surface
      complete = res%fail_stage == FAIL_NONE .or. res%fail_stage == FAIL_BLOCKED

      ! RAYTRA's plot-ray capture (GRASET, the lens drawing and DXF): only the
      ! RAYTRA flavour, only a ray that reached the end.  It plots from the
      ! first surface of finite thickness; when there is none legacy reports
      ! that and returns -- before the energy pass.
      capture = GRASET .and. .not. for_opt .and. complete
      glsurf_found = -99
      if (capture) then
         do s = 0, img
            if (abs(ldm%getSurfThi(s)) <= 1.0e10_real64) then
               glsurf_found = s
               exit
            end if
         end do
      end if

      ! The surfaces the final pass reached: geometry, OPL, cosines, slopes,
      ! normals, the pre-surface ray, RV/POSRAY and the polarization basis.
      if (ls >= obj) then
         RAYRAY(1:24, obj:ls) = res%rr(1:24, obj:ls)
         RAYRAY(26:32, obj:ls) = res%rr(26:32, obj:ls)
         RAYRAY(34:38, obj) = res%rr(34:38, obj)
      end if
      ! Only a ray that reached the end runs the energy pass, over every surface.
      if (complete .and. .not. (capture .and. glsurf_found == -99)) then
         RAYRAY(25, obj:img) = res%rr(25, obj:img)
         RAYRAY(34:37, obj:img) = 0.0_real64
      end if

      RAYCOD = res%raycod
      want_macfal = .false.
      select case (res%fail_stage)
      case (FAIL_NONE)
         STOPP = 0
         RAYEXT = .true.
         FAIL = .false.
      case (FAIL_BLOCKED)
         STOPP = 1
         RAYEXT = .false.
         POLEXT = .false.
         FAIL = .true.
         if (for_opt .and. .not. SPDTRA .and. F34 == 0 .and. F58 == 0) want_macfal = .true.
      case (FAIL_SURFACE)
         STOPP = 1
         RAYEXT = .false.
         POLEXT = .false.
         FAIL = .false.
         if (.not. for_opt .and. side_effects) print *, "RAYTRA Failed 2370"
      case (FAIL_WAVELENGTH)
         STOPP = 1
         RAYEXT = .false.
         POLEXT = .false.
      case (FAIL_AIM)
         if (res%raycod(1) == 16) then
            ! NEWDEL's failure: legacy leaves RAYEXT as the iteration set it
            STOPP = 1
            RAYEXT = .true.
            FAIL = .false.
            REFEXT = .false.
            want_macfal = .true.
         else
            STOPP = 1
            RAYEXT = .false.
            POLEXT = .false.
            FAIL = .true.
            if (for_opt .and. .not. SPDTRA .and. F34 == 0 .and. F58 == 0) want_macfal = .true.
         end if
      end select

      if (res%refmiss_set) REFMISS = res%refmiss
      RVSTART = res%rvstart_out
      DUM(obj:img) = res%dum_out

      if (want_macfal .and. side_effects) call MACFAL
      ! Global ray output (GLOBE): legacy converts the finished ray to global
      ! coordinates (GLRAY) after the aperture pass, for a ray that reached
      ! the end whether or not it was then blocked.  The in-loop GLBRAY call of
      ! the legacy core sits behind a STOPP test that HITSUR's own failure
      ! return makes unreachable, so no other ray gets one.  (Recomputing it
      ! is harmless, so CHECK runs it too and compares the result.)
      if (GLOBE .and. complete) call GLBRAY

      ! The plot-ray capture itself, as RAYTRA's tail does it: vertex data
      ! from the plot surface without offsets (GLVERT), then the ray in global
      ! coordinates for the drawing (GLPRY).  It always leaves GLOBE off.
      ! GLVERT and GLPRY only recompute from the lens and RAYRAY/DUM, so CHECK
      ! runs them too and compares; only the messages are side effects.
      if (capture) then
         if (GLOBE .and. side_effects) then
            call zoa_emit('GLOBAL RAY TRACING HAS BEEN SHUT OFF IN PREPARATION', 'black')
            call zoa_emit('FOR RAY PLOTTING', 'black')
         end if
         GLSURF = glsurf_found
         if (GLSURF == -99) then
            GLOBE = .false.
            if (side_effects) then
               call zoa_emit('ALL SURFACES WERE OF INFINITE THICKNESS', 'black')
               call zoa_emit('NO OPTICAL SYSTEM PLOT COULD BE MADE', 'black')
            end if
            return
         end if
         GLOBE = .true.
         OFFX = 0.0_real64
         OFFY = 0.0_real64
         OFFZ = 0.0_real64
         OFFA = 0.0_real64
         OFFB = 0.0_real64
         OFFC = 0.0_real64
         call GLVERT
         call GLPRY
         GLOBE = .false.
      end if
   end subroutine hand_back

   ! The legacy globals that global ray output (GLBRAY) and the plot-ray
   ! capture (GLVERT/GLPRY) read and write, for RAYENGINE CHECK.
   subroutine get_plot_state(p)
      use DATLEN, only: GLOBE, GLSURF, OFFX, OFFY, OFFZ, OFFA, OFFB, OFFC, VERTEX, GLRAY, &
                        GLPRAY, GLVIRT
      type(plot_state), intent(out) :: p
      p%globe = GLOBE
      p%glsurf = GLSURF
      p%off = [OFFX, OFFY, OFFZ, OFFA, OFFB, OFFC]
      p%vertex = VERTEX
      p%glray = GLRAY
      p%glpray = GLPRAY
      p%glvirt = GLVIRT
   end subroutine get_plot_state

   ! sentinel: set the pure outputs GLRAY/GLPRAY to PLOT_SENTINEL instead.
   ! before: where p holds PLOT_SENTINEL (a slot the trace did not write),
   ! restore that state's value.
   subroutine put_plot_state(p, sentinel, before)
      use DATLEN, only: GLOBE, GLSURF, OFFX, OFFY, OFFZ, OFFA, OFFB, OFFC, VERTEX, GLRAY, &
                        GLPRAY, GLVIRT
      type(plot_state), intent(in) :: p
      logical, intent(in) :: sentinel
      type(plot_state), intent(in), optional :: before
      GLOBE = p%globe
      GLSURF = p%glsurf
      OFFX = p%off(1); OFFY = p%off(2); OFFZ = p%off(3)
      OFFA = p%off(4); OFFB = p%off(5); OFFC = p%off(6)
      VERTEX = p%vertex
      GLVIRT = p%glvirt
      if (sentinel) then
         GLRAY = PLOT_SENTINEL
         GLPRAY = PLOT_SENTINEL
      else if (present(before)) then
         GLRAY = merge(p%glray, before%glray, p%glray /= PLOT_SENTINEL)
         GLPRAY = merge(p%glpray, before%glpray, p%glpray /= PLOT_SENTINEL)
      else
         GLRAY = p%glray
         GLPRAY = p%glpray
      end if
   end subroutine put_plot_state

   ! '' when the current globals match legacy's state leg, else what differs
   function plot_state_difference(leg) result(what)
      use DATLEN, only: GLOBE, GLSURF, OFFX, OFFY, OFFZ, OFFA, OFFB, OFFC, VERTEX, GLRAY, &
                        GLPRAY, GLVIRT
      type(plot_state), intent(in) :: leg
      character(len=60) :: what
      what = ''
      if (any(GLRAY /= leg%glray)) then
         what = 'GLRAY (global ray output)'
      else if (any(GLPRAY /= leg%glpray) .or. any(GLVIRT .neqv. leg%glvirt)) then
         what = 'GLPRAY/GLVIRT (plot-ray capture)'
      else if (any(VERTEX /= leg%vertex) .or. GLSURF /= leg%glsurf .or. &
               any([OFFX, OFFY, OFFZ, OFFA, OFFB, OFFC] /= leg%off) .or. &
               (GLOBE .neqv. leg%globe)) then
         what = 'VERTEX/GLSURF/OFF*/GLOBE (plot capture)'
      end if
   end function plot_state_difference

   ! RAYENGINE CHECK: run the legacy core, keep its result, and compare the
   ! engine's hand-back against it.
   subroutine check_one(ctx, req, for_opt)
      use mod_ray_trace_engine, only: trace_context, ray_request, ray_result, trace_ray
      use real_ray_trace, only: real_ray_trace_core
      use DATLEN, only: RAYRAY, RAYCOD, STOPP, RAYEXT, POLEXT, FAIL, REFMISS, RVSTART, &
                        DUM, REFEXT, GLOBE, GRASET
      use type_utils, only: int2str
      type(trace_context), intent(in) :: ctx
      type(ray_request), intent(in) :: req
      logical, intent(in) :: for_opt
      type(ray_result) :: res
      real(real64), allocatable :: rr_leg(:,:)
      integer :: cod_leg(2), stopp_leg, obj, img
      logical :: rayext_leg, polext_leg, fail_leg, refmiss_leg, rvstart_leg, refext_leg
      logical, allocatable :: dum_leg(:)
      logical :: plot
      type(plot_state) :: pl_pre, pl_leg
      character(len=60) :: what

      obj = ctx%obj
      img = ctx%img
      ! Global ray output and the plot-ray capture: both tracers start from
      ! the same state, with the output arrays set to a sentinel so a write
      ! made by one side only is caught even when the values would agree.
      plot = GLOBE .or. GRASET
      if (plot) then
         call get_plot_state(pl_pre)
         call put_plot_state(pl_pre, sentinel=.true.)
      end if
      call real_ray_trace_core(for_opt)
      if (plot) then
         call get_plot_state(pl_leg)
         call put_plot_state(pl_pre, sentinel=.true.)
      end if
      allocate(rr_leg(size(RAYRAY, 1), obj:img), dum_leg(obj:img))
      rr_leg = RAYRAY(:, obj:img)
      cod_leg = RAYCOD
      stopp_leg = STOPP
      rayext_leg = RAYEXT
      polext_leg = POLEXT
      fail_leg = FAIL
      refmiss_leg = REFMISS
      rvstart_leg = RVSTART
      refext_leg = REFEXT
      dum_leg = DUM(obj:img)

      call trace_ray(ctx, req, res)
      call hand_back(ctx, res, for_opt, .false.)

      what = ''
      if (any(RAYCOD /= cod_leg)) then
         what = 'RAYCOD'
      else if (STOPP /= stopp_leg .or. (RAYEXT .neqv. rayext_leg) .or. &
               (POLEXT .neqv. polext_leg) .or. (FAIL .neqv. fail_leg)) then
         what = 'STOPP/RAYEXT/POLEXT/FAIL'
      else if ((REFMISS .neqv. refmiss_leg) .or. (RVSTART .neqv. rvstart_leg) .or. &
               (REFEXT .neqv. refext_leg) .or. any(DUM(obj:img) .neqv. dum_leg)) then
         what = 'REFMISS/RVSTART/REFEXT/DUM'
      else if (any(RAYRAY(1:32, obj:img) /= rr_leg(1:32, :))) then
         what = 'RAYRAY slots 1-32'
      else if (plot) then
         what = plot_state_difference(pl_leg)
      end if
      ! keep legacy's plot state; output slots it did not write keep their
      ! earlier values
      if (plot) call put_plot_state(pl_leg, sentinel=.false., before=pl_pre)

      ! keep the legacy answer
      RAYRAY(:, obj:img) = rr_leg
      RAYCOD = cod_leg
      STOPP = stopp_leg
      RAYEXT = rayext_leg
      POLEXT = polext_leg
      FAIL = fail_leg
      REFMISS = refmiss_leg
      RVSTART = rvstart_leg
      REFEXT = refext_leg
      DUM(obj:img) = dum_leg

      rt_checked = rt_checked + 1
      if (len_trim(what) > 0) then
         rt_mismatch = rt_mismatch + 1
         if (len_trim(rt_first_mismatch) == 0) then
            write(rt_first_mismatch, '(A,A,F9.5,A,F9.5,A,I0,A,L1)') trim(what), &
               ' at px=', req%px, ' py=', req%py, ' iwl=', req%iwl, ' RAYTRA2=', for_opt
         end if
      end if
   end subroutine check_one

end module mod_ray_trace_router
