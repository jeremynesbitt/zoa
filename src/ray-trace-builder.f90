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

   public :: build_trace_context, placement_of, apertures_of, optics_of, aim_settings_of

   ! Which tracer the analyses that have an engine path (CAPFN's COMPAP) use.
   ! RAYENGINE sets it.  CHECK runs the legacy loop and the engine loop on
   ! the same grid and compares what they store, entry for entry.
   integer, parameter, public :: ENGINE_OFF = 0, ENGINE_ON = 1, ENGINE_CHECK = 2
   integer, public :: ray_engine_mode = ENGINE_ON

contains

   ! check_apertures: run the clear-aperture/obscuration pass (legacy CACOCH=1).
   ! ana_aim: the legacy ANAAIM flag the caller traces with.  COMPAP clears it
   ! around each RAYTRA2 call and sets it again afterwards, so it cannot be
   ! read from the global here -- the caller states it.
   subroutine build_trace_context(ctx, check_apertures, ana_aim)
      use DATLEN, only: NEWOBJ, NEWREF, NEWIMG, REFEXT, REFRY, AIMTOL, NRAITR, &
                        SURTOL, PXTRAX, PXTRAY, LFOB, RVSTART, DUM, ITRACE, GLOBE, &
                        COATSET, ANAAIM
      use GLOBALS, only: NUMHITS
      use mod_lens_data_manager, only: ldm
      use mod_system, only: sys_ray_aiming, sys_telecentric, sys_scx, sys_scy, &
                            sys_screen
      use mod_surface_placement, only: pivot_normal_needed
      use mod_surface_interaction, only: hit_supported
      use type_utils, only: int2str
      type(trace_context), intent(out) :: ctx
      logical, intent(in), optional :: check_apertures, ana_aim
      integer :: s, w
      logical :: NOCOBSPSF
      COMMON/PSFCOBS/NOCOBSPSF

      ctx%obj = NEWOBJ
      ctx%ref = NEWREF
      ctx%img = NEWIMG
      ctx%check_apertures = .true.
      if (present(check_apertures)) ctx%check_apertures = check_apertures

      ctx%aim_on = sys_ray_aiming() /= 0.0_real64
      ctx%telecentric = sys_telecentric() /= 0.0_real64
      ctx%aim_tol = AIMTOL
      ctx%max_aim_iter = NRAITR
      ctx%aim = aim_settings_of(NEWREF)
      ctx%aim%ana_aim = ANAAIM
      if (present(ana_aim)) ctx%aim%ana_aim = ana_aim
      ctx%surtol = SURTOL
      ctx%no_cobs_psf = NOCOBSPSF
      ctx%px_x1 = PXTRAX(1, NEWOBJ+1)
      ctx%px_y1 = PXTRAY(1, NEWOBJ+1)
      ctx%px_x5_obj = PXTRAX(5, NEWOBJ)
      ctx%px_y5_obj = PXTRAY(5, NEWOBJ)
      ctx%lfob1 = LFOB(1)
      ctx%lfob2 = LFOB(2)
      ctx%scx_set = sys_scx() /= 0.0_real64
      ctx%scy_set = sys_scy() /= 0.0_real64
      ctx%rvstart0 = RVSTART
      allocate(ctx%dum0(ctx%obj:ctx%img))
      ctx%dum0 = DUM(ctx%obj:ctx%img)

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
         ! (the typed array can be stale -- shorter than the lens -- right after a
         ! structural edit; legacy HITSUR guards the same way)
         if (s >= lbound(ldm%surfaces, 1) .and. s <= ubound(ldm%surfaces, 1)) then
            if (allocated(ldm%surfaces(s)%s)) then
               allocate(ctx%surf(s)%geom, source=ldm%surfaces(s)%s)
            end if
         end if
         ctx%surf(s)%place = placement_of(s)
         ctx%surf(s)%aper = apertures_of(s)
         ctx%surf(s)%optics = optics_of(s)
         do w = 1, 10
            ctx%surf(s)%n_after(w) = ldm%getSurfIndex(s, w)
         end do
      end do

      ! Support gate: decline anything the engine does not reproduce yet, so
      ! the caller falls back to the legacy tracer.  Reasons are reported by
      ! TRACECMP, so keep them specific.
      ctx%supported = .false.
      if (ctx%img < ctx%obj + 1) then
         ctx%reason = 'lens has no surfaces to trace'
         return
      end if
      if (.not. REFEXT) then
         ctx%reason = 'no chief ray for this field (run FOB)'
         return
      end if
      if (ITRACE) then
         ctx%reason = 'illumination tracing is not supported'
         return
      end if
      if (GLOBE) then
         ctx%reason = 'global ray output is not supported'
         return
      end if
      if (COATSET) then
         ctx%reason = 'coatings (COATSET) are not supported'
         return
      end if
      if (sys_screen() == 1.0_real64) then
         ctx%reason = 'screen surfaces are not supported'
         return
      end if
      if (pivot_normal_needed(ctx%surf(ctx%obj+1)%place)) then
         ctx%reason = 'a pivot on surface '//trim(int2str(ctx%obj+1))//' is not supported'
         return
      end if
      do s = ctx%obj, ctx%img - 1
         if (allocated(NUMHITS)) then
            if (s >= lbound(NUMHITS, 1) .and. s <= ubound(NUMHITS, 1)) then
               if (NUMHITS(s) /= 1) then
                  ctx%reason = 'surface '//trim(int2str(s))//' is a multi-hit surface'
                  return
               end if
            end if
         end if
      end do
      do s = ctx%obj + 1, ctx%img
         if (.not. hit_supported(ctx%surf(s)%geom, ctx%surf(s)%optics, 1.0_real64)) then
            ctx%reason = 'surface '//trim(int2str(s))//' has a type the engine does not support'
            return
         end if
      end do
      ctx%supported = .true.
      ctx%reason = ''
   end subroutine build_trace_context

   ! Fill a surface_placement for surface s from the legacy accessors: every
   ! quantity TRNSF2 / BAKONE / FORONEL read for that surface.
   function placement_of(s) result(p)
      use mod_surface, only: surf_tilt_flag, surf_decenter_flag, surf_alpha, &
         surf_beta, surf_gamma, surf_decenter_x, surf_decenter_y, surf_decenter_z, &
         surf_thickness, surf_global_dx, surf_global_dy, surf_global_dz, &
         surf_global_alpha, surf_global_beta, surf_global_gamma, &
         surf_pivot_flag, surf_pivot_axis, surf_pivot_x, surf_pivot_y
      use mod_surface_placement, only: surface_placement
      integer, intent(in) :: s
      type(surface_placement) :: p

      p%tilt_flag = surf_tilt_flag(s)
      p%decenter_flag = surf_decenter_flag(s)
      p%alpha = surf_alpha(s)
      p%beta = surf_beta(s)
      p%gamma = surf_gamma(s)
      p%dx = surf_decenter_x(s)
      p%dy = surf_decenter_y(s)
      p%dz = surf_decenter_z(s)
      p%thickness = surf_thickness(s)
      p%global_dx = surf_global_dx(s)
      p%global_dy = surf_global_dy(s)
      p%global_dz = surf_global_dz(s)
      p%global_alpha = surf_global_alpha(s)
      p%global_beta = surf_global_beta(s)
      p%global_gamma = surf_global_gamma(s)
      p%pivot_flag = surf_pivot_flag(s)
      p%pivot_axis = surf_pivot_axis(s)
      p%pivot_x = surf_pivot_x(s)
      p%pivot_y = surf_pivot_y(s)
   end function placement_of


   ! Fill a surface_apertures for surface s from the legacy accessors and
   ! arrays: every quantity CACHEK / CAERRS / COERRS read for that surface, plus
   ! the MULTCLAP / MULTCOBS tables the CACOCH loop feeds to CACHEK.
   function apertures_of(s) result(a)
      use DATLEN, only: IPOLYX, IPOLYY, MULTCLAP, MULTCOBS
      use mod_lens_data_manager, only: ldm
      use mod_surface, only: surf_footblok_flag, surf_clap_type, surf_clap_dim, &
         surf_clap_tilt, surf_coat_type, surf_cobs_poly, surf_cobs_ape_type, &
         surf_cobs_ape_data, surf_cobs_era_type, surf_cobs_era_data, &
         surf_multi_clap_flag, surf_multi_cobs_flag
      use mod_surface_apertures, only: surface_apertures, APER_MAXPTS
      integer, intent(in) :: s
      type(surface_apertures) :: a
      integer :: k, n

      a%surface = s
      a%special_type = ldm%getSurfSpecialType(s)
      a%footblok_flag = surf_footblok_flag(s)
      a%clap_type = surf_clap_type(s)
      do k = 1, 5
         a%clap_dim(k) = surf_clap_dim(s, k)
      end do
      a%clap_tilt = surf_clap_tilt(s)
      a%cobs_type = surf_coat_type(s)
      a%clap_erase_type = surf_cobs_ape_type(s)
      a%cobs_erase_type = surf_cobs_era_type(s)
      do k = 1, 6
         a%cobs_dim(k) = surf_cobs_poly(s, k)
         a%clap_erase_dim(k) = surf_cobs_ape_data(s, k)
         a%cobs_erase_dim(k) = surf_cobs_era_data(s, k)
      end do
      a%ipoly_x(1:APER_MAXPTS, 1:4) = IPOLYX(1:APER_MAXPTS, s, 1:4)
      a%ipoly_y(1:APER_MAXPTS, 1:4) = IPOLYY(1:APER_MAXPTS, s, 1:4)

      n = max(0, min(surf_multi_clap_flag(s), 1000))
      a%multi_clap_n = n
      allocate(a%multi_clap(3, n))
      do k = 1, n
         a%multi_clap(1:3, k) = MULTCLAP(k, 1:3, s)
      end do
      n = max(0, min(surf_multi_cobs_flag(s), 1000))
      a%multi_cobs_n = n
      allocate(a%multi_cobs(3, n))
      do k = 1, n
         a%multi_cobs(1:3, k) = MULTCOBS(k, 1:3, s)
      end do
   end function apertures_of


   ! Fill a surface_optics for surface s from the legacy accessors and arrays:
   ! everything HITSUR / INTERACK read about that surface (see
   ! surface-interaction.f90).
   function optics_of(s) result(o)
      use DATLEN, only: DUMMMY, GLANAM
      use mod_lens_data_manager, only: ldm
      use mod_surface, only: surf_refractive_index, surf_special_type, &
         surf_array_parity, surf_paraxial_val, surf_diffraction_flag, &
         surf_reflection_mode, surf_dummy_val, surf_ray_error, surf_thickness, &
         surf_clap_dim, surf_toric_flag
      use mod_surface_interaction, only: surface_optics, GLASS_ORDINARY, &
         GLASS_PERFECT, GLASS_IDEAL
      integer, intent(in) :: s
      type(surface_optics) :: o
      integer :: w

      do w = 1, 10
         o%index(w) = surf_refractive_index(s, w)
      end do
      o%special_type = surf_special_type(s)
      o%toric_flag = surf_toric_flag(s)
      o%array_parity = surf_array_parity(s)
      o%paraxial = surf_paraxial_val(s)
      o%diffraction_flag = surf_diffraction_flag(s)
      o%reflection_mode = surf_reflection_mode(s)
      o%dummy_val = surf_dummy_val(s)
      o%dummy_ok = DUMMMY(s)
      o%glass_class = GLASS_ORDINARY
      if (GLANAM(s, 2) == 'PERFECT      ') o%glass_class = GLASS_PERFECT
      if (GLANAM(s, 2) == 'IDEAL        ') o%glass_class = GLASS_IDEAL
      o%ray_error = surf_ray_error(s)
      o%thickness = surf_thickness(s)
      o%clap_dim4 = surf_clap_dim(s, 4)

      ! HITSUR's guard for the typed-surface path
      o%typed_valid = .false.
      if (allocated(ldm%surfaces)) then
         if (ubound(ldm%surfaces, 1) == ldm%getLastSurf() .and. &
             s >= lbound(ldm%surfaces, 1) .and. s <= ubound(ldm%surfaces, 1)) then
            o%typed_valid = allocated(ldm%surfaces(s)%s)
         end if
      end if
   end function optics_of


   ! Fill an aim_settings for reference surface ref from the legacy globals:
   ! everything the ray-aiming leaf routines (compute_aim_target/APLANA, GETZEE1,
   ! NEWDEL, MISSREF) read that is not a per-surface placement or aperture.
   function aim_settings_of(ref) result(a)
      use DATLEN, only: SYSTEM, ANAAIM, AIMTOL, PXTRAX, PXTRAY
      use mod_system, only: sys_aplanatic_aim, sys_ref_orient
      use mod_surface, only: surf_curvature, surf_conic, surf_array_parity
      use surface_params, only: SYS_FLIPREFX, SYS_FLIPREFY
      use mod_ray_aiming, only: aim_settings
      integer, intent(in) :: ref
      type(aim_settings) :: a

      a%aplanatic = sys_aplanatic_aim() == 1.0_real64
      a%ref_orient = sys_ref_orient()
      a%flip_x = SYSTEM(SYS_FLIPREFX) /= 0.0_real64
      a%flip_y = SYSTEM(SYS_FLIPREFY) /= 0.0_real64
      a%ana_aim = ANAAIM
      a%aim_tol = AIMTOL
      a%ref_curvature = surf_curvature(ref)
      a%ref_array_parity = surf_array_parity(ref)
      a%ref_pxtrax1 = PXTRAX(1, ref)
      a%ref_pxtray1 = PXTRAY(1, ref)
      a%ref_pxtray5 = PXTRAY(5, ref)
      ! GETZEE1 / NEWDEL read surface 1 itself (not NEWOBJ+1)
      a%surf1_curvature = surf_curvature(1)
      a%surf1_conic = surf_conic(1)
   end function aim_settings_of

end module mod_ray_trace_builder
