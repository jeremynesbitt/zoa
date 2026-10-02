! Ray aiming: the leaf routines of the legacy real-ray tracer's reference-ray
! aiming loop, as pure routines on plain data.
!
! The legacy aiming loop lives in real_ray_trace_core (real-ray-trace.f90).  It
! computes the target point on the reference surface, traces a trial ray, and
! corrects the aim point on surface NEWOBJ+1 with a secant/Newton step until the
! ray lands on the target.  The routines here are line-for-line ports of its
! leaves and are meant to give IDENTICAL results, bit for bit:
!
!   compute_aim_target  <-  compute_aim_target (RAYTRA5.f90), with
!   aplana              <-  APLANA             (RAYTRA2.f90)
!   getzee1             <-  GETZEE1            (RAYTRA2.f90)
!   rayderiv            <-  RAYDERIV           (RAYTRA13.f90)
!   newdel              <-  NEWDEL             (RAYTRA13.f90)
!   missref             <-  MISSREF            (INSIDER.f90)
!   adjust_last_surface <-  adjustLastSurface  (real-ray-trace.f90)
!
! Branch order, arithmetic order and the single-precision literals/compares of
! the legacy code are kept.  Output only code is dropped: the DEBUGZEE PRINT
! blocks in GETZEE1 (they run only while FOBBS sets DEBUGZEE around its own
! GETZEE1 call, never on a trace) and the LogTermFOR calls of adjustLastSurface
! (the aiming result never depends on them).  NEWDEL's MSG-gated
! RAY_FAILURE/SHOWIT of its failure branch is not printed here: newdel returns
! a message id (mod_ray_messages) for the caller to print.
!
! State.  The legacy routines communicate through COMMON blocks.  Everything
! they read or write there is an argument here, grouped in aim_state (the
! globals they write) and aim_settings (read-only inputs that are not per
! surface).  A port updates an aim_state exactly as the legacy routine updates
! the globals, including scratch values (R_X..R_N, R_TX..R_TZ, INTERS) that
! GETZEE1 leaves behind.
!
! Side effects that are not state.  NEWDEL's failure branch calls MACFAL, which
! changes unrelated global flags.  A pure routine cannot, so newdel only reports
! that MACFAL is due (macfal_requested) and the caller reproduces it serially.
!
! This module may use ONLY iso_fortran_env, the engine-side modules
! (mod_surface_placement, mod_surface_apertures) and the parameters of
! mod_ray_messages: no legacy data module, ever.
! The builder (mod_ray_trace_builder::aim_settings_of) fills aim_settings from
! the legacy accessors.
module mod_ray_aiming
   use iso_fortran_env, only: real64
   use mod_surface_placement, only: surface_placement, back_to_object, &
                                    forward_from_object, PLACE_PII
   use mod_surface_apertures, only: surface_apertures, inside_closed, APER_MAXPTS, &
                                    APER_PII, APER_TWOPII
   use mod_ray_messages, only: MSG_NONE, MSG_NO_AIM_SOLUTION
   implicit none
   private

   public :: aim_settings, aim_state, compute_aim_target, aplana, getzee1, &
             rayderiv, newdel, missref, adjust_last_surface

   ! Clear aperture shape codes (surface_params AP_*, ALENS(9)).
   integer, parameter :: AIM_AP_CIRC = 1, AIM_AP_RECT = 2, AIM_AP_ELLIP = 3, &
                         AIM_AP_RCTK = 4, AIM_AP_POLY = 5, AIM_AP_IPOLY = 6

   ! Read-only inputs of the aiming routines that are not per-surface data of
   ! the ray path.  Field -> legacy variable:
   type :: aim_settings
      logical :: aplanatic = .false.            ! sys_aplanatic_aim() == 1.0 (APLANATIC aiming on)
      real(real64) :: ref_orient = 0.0_real64   ! sys_ref_orient() (degrees)
      logical :: flip_x = .false.               ! SYSTEM(SYS_FLIPREFX) /= 0
      logical :: flip_y = .false.               ! SYSTEM(SYS_FLIPREFY) /= 0
      logical :: ana_aim = .true.               ! ANAAIM (DATLEN; false only in CAPFN tracing)
      real(real64) :: aim_tol = 1.0e-10_real64  ! AIMTOL
      ! Reference surface NEWREF
      real(real64) :: ref_curvature = 0.0_real64   ! surf_curvature(NEWREF)
      integer :: ref_array_parity = 0              ! surf_array_parity(NEWREF)
      real(real64) :: ref_pxtrax1 = 0.0_real64     ! PXTRAX(1,NEWREF)
      real(real64) :: ref_pxtray1 = 0.0_real64     ! PXTRAY(1,NEWREF)
      real(real64) :: ref_pxtray5 = 0.0_real64     ! PXTRAY(5,NEWREF)
      ! Surface 1 -- literally index 1, as GETZEE1 and NEWDEL read it, NOT NEWOBJ+1
      real(real64) :: surf1_curvature = 0.0_real64 ! surf_curvature(1)
      real(real64) :: surf1_conic = 0.0_real64     ! surf_conic(1)
   end type aim_settings

   ! The legacy globals GETZEE1 / NEWDEL write (and read, for the aim point).
   ! Field -> legacy variable (DATLEN):
   type :: aim_state
      real(real64) :: xc = 0.0_real64, yc = 0.0_real64, zc = 0.0_real64         ! XC, YC, ZC (/ZEEEEE/)
      real(real64) :: x1aim = 0.0_real64, y1aim = 0.0_real64, z1aim = 0.0_real64   ! X1AIM, Y1AIM, Z1AIM
      real(real64) :: xaimol = 0.0_real64, yaimol = 0.0_real64, zaimol = 0.0_real64 ! XAIMOL, YAIMOL, ZAIMOL
      real(real64) :: r_x = 0.0_real64, r_y = 0.0_real64, r_z = 0.0_real64      ! R_X, R_Y, R_Z (GETZEE1 scratch)
      real(real64) :: r_l = 0.0_real64, r_m = 0.0_real64, r_n = 1.0_real64      ! R_L, R_M, R_N (GETZEE1 scratch)
      real(real64) :: r_tx = 0.0_real64, r_ty = 0.0_real64, r_tz = 0.0_real64   ! R_TX, R_TY, R_TZ (/BAKK1/)
      integer :: inters = 0                         ! INTERS
      logical :: zeeerr = .false.                   ! ZEEERR (/ERRZEE/)
      integer :: stopp = 0                          ! STOPP (/RAYSTP/)
      integer :: raycod(2) = 0                      ! RAYCOD(1:2) (/RAYC/)
      integer :: spdcd1 = 0, spdcd2 = 0             ! SPDCD1, SPDCD2 (/SPRA2/)
      logical :: refext = .false.                   ! REFEXT (/RFEXIT/)
   end type aim_state

contains

   ! ------------------------------------------------------------------------
   ! Port of APLANA: aplanatic adjustment of the relative field heights.
   !   clap_type     legacy surf_clap_type(I).  Legacy uses the clear aperture
   !                 TYPE CODE as the ray height HGT when the surface has no
   !                 array (HGT = 1 for the circular aperture compute_aim_target
   !                 calls it for) -- a legacy quirk, kept.
   !   array_parity  surf_array_parity(I)
   !   pxtray1/5     PXTRAY(1,I), PXTRAY(5,I) (used only when array_parity /= 0)
   !   curvature     surf_curvature(I)
   !   ww1,ww2       WWWW1, WWWW2        o1,o2   WWWWW1, WWWWW2
   ! Legacy leaves WX/WY undefined when a field height is NaN; they start at +1
   ! here.
   ! ------------------------------------------------------------------------
   pure subroutine aplana(clap_type, array_parity, pxtray1, pxtray5, curvature, ww1, ww2, o1, o2)
      integer, intent(in) :: clap_type, array_parity
      real(real64), intent(in) :: pxtray1, pxtray5, curvature, ww1, ww2
      real(real64), intent(out) :: o1, o2
      real(real64) :: wx, wy, partx, party, rd, hgt, full

      wx = 1.0_real64
      wy = 1.0_real64
      if (ww1 >= 0.0_real64) wy = 1.0_real64
      if (ww1 < 0.0_real64) wy = -1.0_real64
      if (ww2 >= 0.0_real64) wx = 1.0_real64
      if (ww2 < 0.0_real64) wx = -1.0_real64
      ! determine the new WW1 and WW2 values
      if (array_parity == 0) then
         hgt = clap_type
      else
         hgt = abs(pxtray1) + abs(pxtray5)
      end if
      rd = 1.0_real64/curvature
      ! determine the angle of the full ray
      full = asin(hgt/rd)
      partx = full*abs(ww2)
      party = full*abs(ww1)
      o1 = sin(party)*rd*wy
      o2 = sin(partx)*rd*wx
   end subroutine aplana

   ! ------------------------------------------------------------------------
   ! Port of compute_aim_target: the XY target on the reference surface for the
   ! relative pupil coordinates (ww1 = Y, ww2 = X), in the surface's frame.
   !   aper   apertures_of(NEWREF) (the clear aperture record, cap%from_alens)
   !   aim    the aim settings, incl. the reference surface's curvature, array
   !          parity and paraxial heights
   ! Not ported exactly: "surf_multi_clap_flag(ref) == 0" is tested as
   ! aper%multi_clap_n == 0, which differs only for a negative flag (the
   ! builder clamps the flag to 0..1000).
   ! ------------------------------------------------------------------------
   pure subroutine compute_aim_target(aper, aim, ww1_in, ww2_in, tarx, tary)
      type(surface_apertures), intent(in) :: aper
      type(aim_settings), intent(in) :: aim
      real(real64), intent(in) :: ww1_in, ww2_in
      real(real64), intent(out) :: tarx, tary

      real(real64) :: www1, www2, yval, xval, gamma, tarrx, tarry
      real(real64) :: shape_dim1, shape_dim2, dec_y, dec_x, dim5, tilt
      integer :: clap_shape
      logical :: clapt

      www1 = ww1_in
      www2 = ww2_in
      yval = 0.0_real64
      xval = 0.0_real64

      clap_shape = aper%clap_type
      shape_dim1 = aper%clap_dim(1)
      shape_dim2 = aper%clap_dim(2)
      dec_y = aper%clap_dim(3)
      dec_x = aper%clap_dim(4)
      dim5 = aper%clap_dim(5)
      tilt = aper%clap_tilt

      if (clap_shape >= 1 .and. clap_shape <= 6 .and. aper%multi_clap_n == 0) then

         ! Surface has a single clear aperture: decentered/tilted?
         clapt = (dec_y /= 0.0_real64 .or. dec_x /= 0.0_real64 .or. tilt /= 0.0_real64)

         ! Aplanatic aiming adjustment for centred circular apertures
         if (aim%aplanatic .and. aim%ref_curvature /= 0.0_real64 .and. clap_shape == 1 .and. &
             dec_y == 0.0_real64 .and. dec_x == 0.0_real64 .and. tilt == 0.0_real64) then
            if (abs(1.0_real64/aim%ref_curvature) >= abs(shape_dim1) .and. &
                abs(1.0_real64/aim%ref_curvature) >= abs(shape_dim2)) &
               call aplana(aper%clap_type, aim%ref_array_parity, aim%ref_pxtray1, aim%ref_pxtray5, &
                           aim%ref_curvature, ww1_in, ww2_in, www1, www2)
         end if

         select case (clap_shape)

         case (AIM_AP_CIRC)  ! circular
            if (clapt) then
               if (shape_dim1 <= shape_dim2) then
                  tary = shape_dim1*www1
                  tarx = shape_dim1*www2
               else
                  tary = dec_y + shape_dim2*www1
                  tarx = dec_x + shape_dim2*www2
               end if
               if (aim%flip_x) tarx = -tarx
               if (aim%flip_y) tary = -tary
               tarx = tarx + dec_x
               tary = tary + dec_y
               gamma = (tilt*PLACE_PII)/180.0_real64
            else
               if (shape_dim1 <= shape_dim2) then
                  tary = shape_dim1*www1
                  tarx = shape_dim1*www2
               else
                  tary = shape_dim2*www1
                  tarx = shape_dim2*www2
               end if
               if (aim%flip_x) tarx = -tarx
               if (aim%flip_y) tary = -tary
               gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
            end if

         case (AIM_AP_RECT)  ! rectangular
            if (clapt) then
               if (aim%ana_aim) then
                  tary = shape_dim1*ww1_in
                  tarx = shape_dim2*ww2_in
               else if (abs(shape_dim1) > abs(shape_dim2)) then
                  tary = shape_dim1*ww1_in
                  tarx = shape_dim1*ww2_in
               else
                  tary = dec_y + shape_dim2*ww1_in
                  tarx = dec_x + shape_dim2*ww2_in
               end if
               if (aim%flip_x) tarx = -tarx
               if (aim%flip_y) tary = -tary
               tarx = tarx + dec_x
               tary = tary + dec_y
               gamma = (tilt*PLACE_PII)/180.0_real64
            else
               tary = shape_dim1*ww1_in
               tarx = shape_dim2*ww2_in
               if (aim%flip_x) tarx = -tarx
               if (aim%flip_y) tary = -tary
               gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
            end if

         case (AIM_AP_ELLIP)  ! elliptical
            yval = shape_dim1
            xval = shape_dim2
            tary = yval*ww1_in
            tarx = xval*ww2_in
            if (aim%flip_x) tarx = -tarx
            if (aim%flip_y) tary = -tary
            if (clapt) then
               tarx = tarx + dec_x
               tary = tary + dec_y
               gamma = (tilt*PLACE_PII)/180.0_real64
            else
               gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
            end if

         case (AIM_AP_RCTK)  ! racetrack
            yval = shape_dim1
            xval = shape_dim2
            tary = yval*ww1_in
            tarx = xval*ww2_in
            if (aim%flip_x) tarx = -tarx
            if (aim%flip_y) tary = -tary
            if (clapt) then
               tarx = tarx + dec_x
               tary = tary + dec_y
               gamma = (tilt*PLACE_PII)/180.0_real64
            else
               gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
            end if

         case (AIM_AP_POLY)  ! regular polygon: radius to corner is dim1
            yval = shape_dim1
            xval = shape_dim1
            tary = yval*ww1_in
            tarx = xval*ww2_in
            if (aim%flip_x) tarx = -tarx
            if (aim%flip_y) tary = -tary
            if (clapt) then
               tarx = tarx + dec_x
               tary = tary + dec_y
               gamma = (tilt*PLACE_PII)/180.0_real64
            else
               gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
            end if

         case (AIM_AP_IPOLY)  ! irregular polygon: dim5 (decentered) or dim2 (centred)
            if (clapt) then
               yval = dim5
               xval = dim5
            else
               yval = shape_dim2
               xval = shape_dim2
            end if
            tary = yval*ww1_in
            tarx = xval*ww2_in
            if (aim%flip_x) tarx = -tarx
            if (aim%flip_y) tary = -tary
            if (clapt) then
               tarx = tarx + dec_x
               tary = tary + dec_y
               gamma = (tilt*PLACE_PII)/180.0_real64
            else
               gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
            end if

         case default
            tarx = 0.0_real64
            tary = 0.0_real64
            gamma = 0.0_real64
         end select

         ! Apply rotation (either clap tilt or reference surface orientation)
         tarrx = (tarx*cos(gamma)) - (tary*sin(gamma))
         tarry = (tarx*sin(gamma)) + (tary*cos(gamma))
         tarx = tarrx
         tary = tarry

      else
         ! No clear aperture on the reference surface, or a multi-clap: aim at
         ! the paraxial ray height scaled by the relative field
         tary = aim%ref_pxtray1*ww1_in
         tarx = aim%ref_pxtrax1*ww2_in
         if (aim%flip_x) tarx = -tarx
         if (aim%flip_y) tary = -tary
         gamma = (aim%ref_orient*PLACE_PII)/180.0_real64
         tarrx = (tarx*cos(gamma)) - (tary*sin(gamma))
         tarry = (tarx*sin(gamma)) + (tary*cos(gamma))
         tarx = tarrx
         tary = tarry
      end if
   end subroutine compute_aim_target

   ! ------------------------------------------------------------------------
   ! Port of GETZEE1: the best intersection point with surface 1 (a sphere or
   ! conic) for the aim point (xc,yc,zc), which is given in the frame of surface
   ! NEWOBJ+1.  Reads surface 1's curvature and conic literally (aim%surf1_*),
   ! as legacy does, even though BAKONE/FORONEL work on NEWOBJ+1.
   !   p1             placement of surface NEWOBJ+1 (BAKONE / FORONEL data)
   !   obj_thickness  thickness of surface NEWOBJ (BAKONE)
   !   pn             the normal SAGINT returns at the pivot of NEWOBJ+1; read
   !                  only on FORONEL's pivot branch (see pivot_normal_needed)
   !   xstrt..zstrt   XSTRT, YSTRT, ZSTRT: the object point
   ! In/out: st (xc..zc, r_x..r_n, r_tx..r_tz, inters, zeeerr).
   ! ZTEST of the legacy routine is computed there and never used; it is not
   ! computed here.
   ! ------------------------------------------------------------------------
   pure subroutine getzee1(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, st)
      type(aim_settings), intent(in) :: aim
      type(surface_placement), intent(in) :: p1
      real(real64), intent(in) :: obj_thickness, pn(3), xstrt, ystrt, zstrt
      type(aim_state), intent(inout) :: st

      real(real64) :: a, b, c, cv, cc, mag, q, signb, arg, hv0, hv1, hv2, xa, ya, za
      integer :: jim

      signb = 1.0_real64   ! legacy leaves it undefined until the first non-NaN b
      st%zeeerr = .false.

      hv0 = 0.0_real64
      hv1 = 0.0_real64
      hv2 = 0.0_real64

      ! surface is a conic or sphere
      cv = aim%surf1_curvature
      cc = aim%surf1_conic

      ! compute the direction cosines directly from positions in all cases
      xa = st%xc
      ya = st%yc
      za = st%zc
      ! these are in the local coordinate system of NEWOBJ+1; convert to NEWOBJ
      st%r_tx = xa
      st%r_ty = ya
      st%r_tz = za
      call back_to_object(p1, obj_thickness, st%r_tx, st%r_ty, st%r_tz)
      xa = st%r_tx
      ya = st%r_ty
      za = st%r_tz
      mag = sqrt(((xstrt - xa)**2) + ((ystrt - ya)**2) + ((zstrt - za)**2))

      st%r_l = (xa - xstrt)/mag
      st%r_m = (ya - ystrt)/mag
      st%r_n = (za - zstrt)/mag
      ! now put the direction cosines into the NEWOBJ+1 coordinate system
      st%r_tx = st%r_l
      st%r_ty = st%r_m
      st%r_tz = st%r_n
      call forward_from_object(p1, pn(1), pn(2), pn(3), st%r_tx, st%r_ty, st%r_tz)

      xa = st%xc
      ya = st%yc
      za = st%zc
      do jim = 1, 2
         if (jim == 1) then
            st%r_l = 0.0_real64
            st%r_m = 0.0_real64
            st%r_n = 1.0_real64
         else
            st%r_l = st%r_tx
            st%r_m = st%r_ty
            st%r_n = st%r_tz
         end if

         st%r_x = xa
         st%r_y = ya
         st%r_z = za

         ! now intersect the sphere or conic at NEWOBJ+1
         a = -(cv*((st%r_l**2) + (st%r_m**2) + ((st%r_n**2)*(cc + 1.0_real64))))
         b = st%r_n - (cv*st%r_x*st%r_l) - (cv*st%r_y*st%r_m) - &
             ((cv*st%r_z*st%r_n)*(cc + 1.0_real64))
         b = 2.0_real64*b
         c = -cv*((st%r_x**2) + (st%r_y**2) + ((st%r_z**2)*(cc + 1.0_real64)))
         c = (c + (2.0_real64*st%r_z))
         if (b /= 0.0_real64) signb = ((abs(b))/(b))
         if (b == 0.0_real64) signb = 1.0_real64
         if (a == 0.0_real64) then
            hv0 = -(c/b)
            st%inters = 1
         else
            ! a not zero
            arg = ((b**2) - (4.0_real64*a*c))
            if (arg < 0.0_real64) then
               st%zeeerr = .true.
               return
            end if
            q = (-0.5_real64*(b + (signb*(sqrt((b**2) - (4.0_real64*a*c))))))
            hv1 = c/q
            hv2 = q/a
            st%inters = 2
         end if
         if (st%inters == 1) then
            ! only one intersection point
            st%xc = (st%r_x + (hv0*st%r_l))
            st%yc = (st%r_y + (hv0*st%r_m))
            st%zc = (st%r_z + (hv0*st%r_n))
         end if

         if (st%inters == 2) then
            ! two intersection points
            if (abs(hv1) <= abs(hv2)) then
               st%xc = (st%r_x + (hv1*st%r_l))
               st%yc = (st%r_y + (hv1*st%r_m))
               st%zc = (st%r_z + (hv1*st%r_n))
            else
               st%xc = (st%r_x + (hv2*st%r_l))
               st%yc = (st%r_y + (hv2*st%r_m))
               st%zc = (st%r_z + (hv2*st%r_n))
            end if
         end if
         ! coordinates are in the coordinate system of the NEWOBJ+1 surface
      end do
   end subroutine getzee1

   ! ------------------------------------------------------------------------
   ! Port of RAYDERIV: the four finite-difference derivatives of the landing
   ! point (rx,ry) with respect to the aim point (x1,y1), from the last two
   ! trial rays.  A zero denominator gives a zero derivative.  Legacy quirk:
   ! D12 uses the X landing difference over the Y aim difference and D21 the
   ! Y landing difference over the X aim difference.
   ! ------------------------------------------------------------------------
   pure subroutine rayderiv(x1last, y1last, x1one, y1one, rxone, ryone, rxlast, rylast, &
                            d11, d12, d21, d22)
      real(real64), intent(in) :: x1last, y1last, x1one, y1one, rxone, ryone, rxlast, rylast
      real(real64), intent(out) :: d11, d12, d21, d22

      ! derivative 1 (D11)
      if ((x1last - x1one) == 0.0_real64) then
         d11 = 0.0_real64
      else
         d11 = (rxlast - rxone)/(x1last - x1one)
      end if

      ! derivative 2 (D12)
      if ((y1last - y1one) == 0.0_real64) then
         d12 = 0.0_real64
      else
         d12 = (rxlast - rxone)/(y1last - y1one)
      end if

      ! derivative 3 (D21)
      if ((x1last - x1one) == 0.0_real64) then
         d21 = 0.0_real64
      else
         d21 = (rylast - ryone)/(x1last - x1one)
      end if

      ! derivative 4 (D22)
      if ((y1last - y1one) == 0.0_real64) then
         d22 = 0.0_real64
      else
         d22 = (rylast - ryone)/(y1last - y1one)
      end if
   end subroutine rayderiv

   ! ------------------------------------------------------------------------
   ! The part of every NEWDEL branch after DDELX/DDELY are known: move the aim
   ! point, run GETZEE1 when surface 1 is curved, and convert the aim point to
   ! the NEWOBJ frame.  (Legacy repeats these 24 lines in each branch.)
   ! ------------------------------------------------------------------------
   pure subroutine newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
      type(aim_settings), intent(in) :: aim
      type(surface_placement), intent(in) :: p1
      real(real64), intent(in) :: obj_thickness, pn(3), xstrt, ystrt, zstrt, ddelx, ddely
      type(aim_state), intent(inout) :: st
      real(real64) :: xc1, yc1, zc1

      st%y1aim = st%yaimol + ddely
      st%x1aim = st%xaimol + ddelx
      st%z1aim = st%zaimol
      st%xc = st%x1aim
      st%yc = st%y1aim
      st%zc = st%z1aim
      xc1 = st%xc
      yc1 = st%yc
      zc1 = st%zc
      if (aim%surf1_curvature /= 0.0_real64) &
         call getzee1(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, st)
      st%x1aim = st%xc
      st%y1aim = st%yc
      st%z1aim = st%zc
      ! call BAKONE to convert to NEWOBJ coordinates
      st%xaimol = xc1
      st%yaimol = yc1
      st%zaimol = zc1
      st%r_tx = st%x1aim
      st%r_ty = st%y1aim
      st%r_tz = st%z1aim
      call back_to_object(p1, obj_thickness, st%r_tx, st%r_ty, st%r_tz)
      st%x1aim = st%r_tx
      st%y1aim = st%r_ty
      st%z1aim = st%r_tz
      ! now the coordinates are in the NEWOBJ coordinate system
   end subroutine newdel_step

   ! ------------------------------------------------------------------------
   ! Port of NEWDEL: one Newton step of the aim-point correction.  mf1, mf2 are
   ! the landing errors (target - landed X, Y) and d11..d22 the derivatives from
   ! rayderiv.  The branches are tested in the legacy order:
   !   0  all four derivatives zero: failure (see below)
   !   1  D11 /= 0 and D22 /= 0       2  D11 /= 0 and D12 /= 0
   !   3  D21 /= 0 and D12 /= 0       4  D21 /= 0 and D22 /= 0
   !   5  D11 /= 0 and D21 /= 0       6  D12 /= 0 and D22 /= 0
   !   7  D11 = D12 = D21 = 0 (only D22)
   !   8  D12 = D21 = D22 = 0 (only D11)
   !   9  D11 = D21 = D22 = 0 (only D12)
   !  10  D11 = D12 = D21 = 0: a copy of 7 that can never run (7 catches it)
   !  --  only D21 non-zero: no branch matches, legacy returns having changed
   !      nothing and without setting DELFAIL
   ! Failure branch (0): st%raycod = (16, NEWOBJ), stopp = 1, spdcd1/2 = same,
   ! refext = .false., delfail = .true. and macfal_requested = .true.; legacy
   ! then calls MACFAL (unrelated global flags), which the caller must do.
   ! delfail is only ever set to .true. (the caller clears it first), as legacy.
   !   newobj  legacy NEWOBJ, for RAYCOD(2) of the failure
   !   msg_id  out: MSG_NO_AIM_SOLUTION on the failure branch (legacy prints
   !           RAY_FAILURE(NEWOBJ), then SHOWIT, under MSG); else MSG_NONE
   ! ------------------------------------------------------------------------
   pure subroutine newdel(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, newobj, &
                          mf1, mf2, d11, d12, d21, d22, st, delfail, macfal_requested, msg_id)
      type(aim_settings), intent(in) :: aim
      type(surface_placement), intent(in) :: p1
      real(real64), intent(in) :: obj_thickness, pn(3), xstrt, ystrt, zstrt
      integer, intent(in) :: newobj
      real(real64), intent(in) :: mf1, mf2, d11, d12, d21, d22
      type(aim_state), intent(inout) :: st
      logical, intent(inout) :: delfail
      logical, intent(out) :: macfal_requested
      integer, intent(out) :: msg_id
      real(real64) :: ddelx, ddely

      macfal_requested = .false.
      msg_id = MSG_NONE

      if (d11 == 0.0_real64 .and. d12 == 0.0_real64 .and. d21 == 0.0_real64 &
          .and. d22 == 0.0_real64) then
         ! no solution exists to aim the reference ray to AIMTOL
         msg_id = MSG_NO_AIM_SOLUTION
         st%stopp = 1
         st%raycod(1) = 16
         st%raycod(2) = newobj
         st%spdcd1 = st%raycod(1)
         st%spdcd2 = st%raycod(2)
         st%refext = .false.
         macfal_requested = .true.
         delfail = .true.
         return
      end if

      if (d11 /= 0.0_real64 .and. d22 /= 0.0_real64) then
         ! special solution, non-zero derivative products: the solutions for
         ! DDELY and DDELX are independent of one another
         ddelx = mf1/d11
         ddely = mf2/d22
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      if (d11 /= 0.0_real64 .and. d12 /= 0.0_real64) then
         ddelx = mf1/d11
         ddely = mf1/d12
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      if (d21 /= 0.0_real64 .and. d12 /= 0.0_real64) then
         ddelx = mf2/d21
         ddely = mf1/d12
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      if (d21 /= 0.0_real64 .and. d22 /= 0.0_real64) then
         ddelx = mf2/d21
         ddely = mf2/d22
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      if (d11 /= 0.0_real64 .and. d21 /= 0.0_real64) then
         ddelx = mf1/d11
         ddely = mf2/d21
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      if (d12 /= 0.0_real64 .and. d22 /= 0.0_real64) then
         ddelx = mf1/d12
         ddely = mf2/d22
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if

      ! special solution 1
      if (d11 == 0.0_real64 .and. d12 == 0.0_real64 .and. d21 == 0.0_real64) then
         ddelx = 0.0_real64
         ddely = mf2/d22
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      ! special solution 2
      if (d12 == 0.0_real64 .and. d21 == 0.0_real64 .and. d22 == 0.0_real64) then
         ddelx = mf1/d11
         ddely = 0.0_real64
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      ! special solution 3
      if (d11 == 0.0_real64 .and. d21 == 0.0_real64 .and. d22 == 0.0_real64) then
         ddelx = mf1/d12
         ddely = 0.0_real64
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
      ! special solution 4 (legacy: identical to special solution 1, never reached)
      if (d11 == 0.0_real64 .and. d12 == 0.0_real64 .and. d21 == 0.0_real64) then
         ddelx = 0.0_real64
         ddely = mf2/d22
         call newdel_step(aim, p1, obj_thickness, pn, xstrt, ystrt, zstrt, ddelx, ddely, st)
         return
      end if
   end subroutine newdel

   ! ------------------------------------------------------------------------
   ! Port of MISSREF: is the point (x,y) on the reference surface blocked by its
   ! clear aperture?  If so refmiss is set to .true. (legacy never clears it);
   ! ls is the /CACO/ LS value the legacy routine leaves behind (0, or 10 after
   ! a block).  ap is apertures_of(NEWREF).  Only the clear aperture is checked
   ! (no obscuration, no erase), types 1..6, exactly as legacy.
   !
   ! Legacy bug: MISSREF declares JK1, JK2, JK3 and uses them as offsets in
   ! every shape but never assigns them, so it reads uninitialised stack memory
   ! (measured: whatever an earlier call left there, which does change results).
   ! They are the zero offsets of CACHEK's plain call here, which is what the
   ! routine was written for; ENGINETEST scrubs the stack before each legacy
   ! call so both sides see zero.  Polygon vertex loops stop at APER_MAXPTS (legacy
   ! would overrun its XT array).
   ! ------------------------------------------------------------------------
   pure subroutine missref(ap, x, y, aimtol, refmiss, ls)
      type(surface_apertures), intent(in) :: ap
      real(real64), intent(in) :: x, y, aimtol
      logical, intent(inout) :: refmiss
      real(real64), intent(out) :: ls

      real(real64) :: xt(APER_MAXPTS), yt(APER_MAXPTS)
      real(real64) :: xr, yr, rs, xrd, yrd, jk1, jk2, jk3
      real(real64) :: x1, x2, x3, x4, y1, y2, y3, y4, x5, x6, x7, x8, y5, y6, y7, y8
      real(real64) :: xc1, xc2, xc3, xc4, yc1, yc2, yc3, yc4, rad2, maxsid
      real(real64) :: cs1, cs2, cs3, cs4, angle, a15, x0, y0
      integer :: caflg, n, np, iii
      logical :: ins

      jk1 = 0.0_real64
      jk2 = 0.0_real64
      jk3 = 0.0_real64
      xt = 0.0_real64
      yt = 0.0_real64
      x5 = 0.0_real64; x6 = 0.0_real64; x7 = 0.0_real64; x8 = 0.0_real64
      y5 = 0.0_real64; y6 = 0.0_real64; y7 = 0.0_real64; y8 = 0.0_real64
      ls = 0.0_real64

      ! no clear aperture: nothing to check
      if (ap%clap_type == 0) return

      caflg = ap%clap_type

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
         if (ls == 10.0_real64) then
            refmiss = .true.
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
         xr = (xrd*cos(a15)) + (yrd*sin(a15))
         yr = (yrd*cos(a15)) - (xrd*sin(a15))
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
         if (ls == 10.0_real64) then
            refmiss = .true.
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
         xr = (xrd*cos(a15)) + (yrd*sin(a15))
         yr = (yrd*cos(a15)) - (xrd*sin(a15))
         ls = ((xr**2)/(ap%clap_dim(2)**2)) + ((yr**2)/(ap%clap_dim(1)**2))
         if (real(ls) > (1.0 + (aimtol**2))) then
            ls = 10.0_real64
         else
            ls = 0.0_real64
         end if
         if (ls == 10.0_real64) then
            refmiss = .true.
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
         xr = (xrd*cos(a15)) + (yrd*sin(a15))
         yr = (yrd*cos(a15)) - (xrd*sin(a15))
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
         if (ls == 10.0_real64) then
            refmiss = .true.
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
         xr = (xrd*cos(a15)) + (yrd*sin(a15))
         yr = (yrd*cos(a15)) - (xrd*sin(a15))
         x0 = xr
         y0 = yr
         np = int(ap%clap_dim(2))
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls = 0.0_real64
         else
            ls = 10.0_real64
         end if
         if (ls == 10.0_real64) then
            refmiss = .true.
            return
         end if
      end if

      ! CAFLG=6 irregular polygon clap
      if (caflg == 6) then
         ls = 0.0_real64
         do iii = 1, min(int(ap%clap_dim(2)), APER_MAXPTS)
            xt(iii) = ap%ipoly_x(iii, 1)
            yt(iii) = ap%ipoly_y(iii, 1)
         end do
         xrd = x
         yrd = y
         xrd = xrd - ap%clap_dim(4) - jk1
         yrd = yrd - ap%clap_dim(3) - jk2
         a15 = ap%clap_tilt*APER_PII/180.0_real64
         a15 = (ap%clap_tilt + jk3)*APER_PII/180.0_real64
         xr = (xrd*cos(a15)) + (yrd*sin(a15))
         yr = (yrd*cos(a15)) - (xrd*sin(a15))
         x0 = xr
         y0 = yr
         np = int(ap%clap_dim(2))
         ins = inside_closed(x0, y0, xt, yt, np)
         if (ins) then
            ls = 0.0_real64
         else
            ls = 10.0_real64
         end if
         if (ls == 10.0_real64) then
            refmiss = .true.
            return
         end if
      end if
   end subroutine missref

   ! ------------------------------------------------------------------------
   ! Port of adjustLastSurface: propagate the ray at surface l straight on over
   ! the extra thickness of the last surface and add the path to its length and
   ! OPL.  rr is the per-surface result array (legacy RAYRAY; row layout as in
   ! real-ray-trace.f90), rr(1:50, obj:).
   !   thickness  ldm%getSurfThi(l)
   !   n_prev     ldm%getSurfIndex(l-1, INT(WW3)): index before the surface at the
   !              ray's wavelength
   !   phase      PHASE (DATLEN)
   ! Legacy quirks kept: pi is the single-precision literal 3.14159 (so the
   ! angle is off from the direction cosine's acos by ~1e-6), only the direction
   ! cosines l and m are used, and z is left unchanged.
   ! ------------------------------------------------------------------------
   pure subroutine adjust_last_surface(l, obj, thickness, n_prev, phase, rr)
      integer, intent(in) :: l, obj
      real(real64), intent(in) :: thickness, n_prev, phase
      real(real64), intent(inout) :: rr(1:, obj:)
      logical :: rv
      real(real64) :: angx, angy, newx, newy, dlen, dopl

      ! angle of the ray: stored as a direction cosine, but the actual angle is wanted
      angx = 3.14159/2 - acos(rr(4, l))
      angy = 3.14159/2 - acos(rr(5, l))

      ! propagate to the final plane
      newx = rr(1, l) + tan(angx)*thickness
      newy = rr(2, l) + tan(angy)*thickness

      ! the length change
      dlen = sqrt((rr(1, l) - newx)**2 + (rr(2, l) - newy)**2 + (thickness)**2)

      rr(1, l) = newx
      rr(2, l) = newy

      rv = .false.
      if (thickness < 0) rv = .true.
      if (rv) dlen = -dlen
      if (abs(dlen) >= 1.0d10) dlen = 0.0d0

      dopl = dlen*abs(n_prev)
      if (.not. rv) dopl = dopl + phase
      if (rv) dopl = dopl - phase

      rr(7, l) = dopl + rr(7, l)
      rr(8, l) = dlen + rr(8, l)
   end subroutine adjust_last_surface

end module mod_ray_aiming
