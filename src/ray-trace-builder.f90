! Builds a trace_context for mod_ray_trace_engine from the current lens and
! field.  This is the one bridge between the engine and the legacy globals:
! it runs serially, reads whatever it needs (typed surfaces, the chief ray
! REFRY, aiming settings), and decides whether the engine supports this lens
! at all.  The engine itself never sees a global.
!
! Call it after FOB has traced the chief ray for the field, and rebuild it
! whenever the lens, field or aiming settings change -- the context is a
! snapshot, never kept across commands.
module mod_ray_trace_builder
   use iso_fortran_env, only: real64
   use mod_ray_trace_engine, only: trace_context, RR_N
   implicit none
   private

   public :: build_trace_context

contains

   subroutine build_trace_context(ctx, check_apertures)
      use DATLEN, only: NEWOBJ, NEWREF, NEWIMG, REFEXT, REFRY, AIMTOL, NRAITR
      use mod_lens_data_manager, only: ldm
      use mod_system, only: sys_ray_aiming, sys_telecentric
      type(trace_context), intent(out) :: ctx
      logical, intent(in), optional :: check_apertures
      integer :: s, w

      ctx%obj = NEWOBJ
      ctx%ref = NEWREF
      ctx%img = NEWIMG
      ctx%check_apertures = .true.
      if (present(check_apertures)) ctx%check_apertures = check_apertures

      ctx%aim_on = sys_ray_aiming() /= 0.0_real64
      ctx%telecentric = sys_telecentric() /= 0.0_real64
      ctx%aim_tol = AIMTOL
      ctx%max_aim_iter = NRAITR

      ctx%chief_exists = REFEXT
      allocate(ctx%chief(RR_N, ctx%obj:ctx%img))
      ctx%chief = REFRY(1:RR_N, ctx%obj:ctx%img)

      if (.not. allocated(ldm%surfaces)) then
         ctx%supported = .false.
         ctx%reason = 'typed surfaces have not been built'
         return
      end if

      allocate(ctx%surf(ctx%obj:ctx%img))
      do s = ctx%obj, ctx%img
         if (allocated(ldm%surfaces(s)%s)) then
            allocate(ctx%surf(s)%geom, source=ldm%surfaces(s)%s)
         end if
         do w = 1, 10
            ctx%surf(s)%n_after(w) = ldm%getSurfIndex(s, w)
         end do
      end do

      ! Support gate.  Each phase of the engine widens this; until the
      ! surface loop exists (P2) nothing is supported.
      ctx%supported = .false.
      ctx%reason = 'engine surface loop not implemented yet'
   end subroutine build_trace_context

end module mod_ray_trace_builder
