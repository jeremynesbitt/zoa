! Surface placement: the coordinate transforms of the legacy ray tracer as
! pure routines on plain data.
!
! A surface's placement is everything that positions its local frame relative
! to the previous surface's frame: tilt angles, decenters, the tilt-type flag,
! the thickness to the next surface, the global-coordinate offsets/angles that
! TILT AUTO style surfaces carry, and the pivot data.  The routines here are
! exact ports of the legacy TILTER.f90 routines and are meant to be
! BIT-IDENTICAL to them:
!
!   place_into_surface  <-  TRNSF2   (ray: frame of surface I-1 -> frame of I)
!   back_to_object      <-  BAKONE   (point: frame of NEWOBJ+1 -> frame of NEWOBJ)
!   forward_from_object <-  FORONEL  (point: frame of NEWOBJ -> frame of NEWOBJ+1)
!
! The branch structure, order of operations and rotation formulas are the
! legacy ones.  The legacy rotation matrix statement functions (A11..C33) are
! written out literally in rot_a/rot_b/rot_c; the legacy code applies them
! to positions and direction cosines independently, so applying the helper to
! each triple gives the same arithmetic.  Do not replace them by matrices.
!
! This module may use ONLY iso_fortran_env: no legacy data module, ever.  The
! builder (mod_ray_trace_builder) fills surface_placement from the legacy
! accessors.
module mod_surface_placement
   use iso_fortran_env, only: real64
   implicit none
   private

   public :: surface_placement, place_into_surface, back_to_object, &
             forward_from_object, pivot_normal_needed

   ! Legacy PII (DATMAI, set in INITKDP.f90) -- reproduced digit for digit.
   real(real64), parameter, public :: PLACE_PII = 3.14159265358979323846_real64

   ! Tilt flag values, as tested by the legacy code.
   integer, parameter, public :: TILT_RTILT = -1   ! RTILT
   integer, parameter, public :: TILT_NONE  = 0
   integer, parameter, public :: TILT_TILT  = 1    ! TILT
   integer, parameter, public :: TILT_AUTO  = 2    ! TILT AUTO
   integer, parameter, public :: TILT_AUTOM = 3    ! TILT AUTOM
   integer, parameter, public :: TILT_BEN   = 4    ! TILT BEN
   integer, parameter, public :: TILT_DAR   = 5    ! TILT DAR
   integer, parameter, public :: TILT_RETD  = 6    ! TILT RETD
   integer, parameter, public :: TILT_REV   = 7    ! TILT REV

   type :: surface_placement
      integer :: tilt_flag = 0             ! surf_tilt_flag
      integer :: decenter_flag = 0         ! surf_decenter_flag (1 = decenter present)
      real(real64) :: alpha = 0.0_real64   ! surf_alpha  (degrees)
      real(real64) :: beta  = 0.0_real64   ! surf_beta
      real(real64) :: gamma = 0.0_real64   ! surf_gamma
      real(real64) :: dx = 0.0_real64      ! surf_decenter_x
      real(real64) :: dy = 0.0_real64      ! surf_decenter_y
      real(real64) :: dz = 0.0_real64      ! surf_decenter_z
      real(real64) :: thickness = 0.0_real64   ! surf_thickness (to next surface)
      real(real64) :: global_dx = 0.0_real64   ! surf_global_dx
      real(real64) :: global_dy = 0.0_real64   ! surf_global_dy
      real(real64) :: global_dz = 0.0_real64   ! surf_global_dz
      real(real64) :: global_alpha = 0.0_real64   ! surf_global_alpha
      real(real64) :: global_beta  = 0.0_real64   ! surf_global_beta
      real(real64) :: global_gamma = 0.0_real64   ! surf_global_gamma
      integer :: pivot_flag = 0            ! surf_pivot_flag (FORONEL only)
      integer :: pivot_axis = 0            ! surf_pivot_axis (1 = PIVAXIS NORMAL)
      real(real64) :: pivot_x = 0.0_real64 ! surf_pivot_x
      real(real64) :: pivot_y = 0.0_real64 ! surf_pivot_y
   end type surface_placement

contains

   ! ---- legacy rotation statement functions, applied to one triple --------
   ! A: about X.  B: about Y.  C: about Z.  ang is in radians.
   pure subroutine rot_a(ang, x, y, z)
      real(real64), intent(in) :: ang
      real(real64), intent(inout) :: x, y, z
      real(real64) :: x1, y1, z1
      x1 = (x*1.0_real64) + (y*0.0_real64) + (z*0.0_real64)
      y1 = (x*0.0_real64) + (y*cos(ang)) + (z*(-sin(ang)))
      z1 = (x*0.0_real64) + (y*sin(ang)) + (z*cos(ang))
      x = x1; y = y1; z = z1
   end subroutine rot_a

   pure subroutine rot_b(ang, x, y, z)
      real(real64), intent(in) :: ang
      real(real64), intent(inout) :: x, y, z
      real(real64) :: x1, y1, z1
      x1 = (x*cos(ang)) + (y*0.0_real64) + (z*sin(ang))
      y1 = (x*0.0_real64) + (y*1.0_real64) + (z*0.0_real64)
      z1 = (x*(-sin(ang))) + (y*0.0_real64) + (z*cos(ang))
      x = x1; y = y1; z = z1
   end subroutine rot_b

   pure subroutine rot_c(ang, x, y, z)
      real(real64), intent(in) :: ang
      real(real64), intent(inout) :: x, y, z
      real(real64) :: x1, y1, z1
      x1 = (x*cos(ang)) + (y*sin(ang)) + (z*0.0_real64)
      y1 = (x*(-sin(ang))) + (y*cos(ang)) + (z*0.0_real64)
      z1 = (x*0.0_real64) + (y*0.0_real64) + (z*1.0_real64)
      x = x1; y = y1; z = z1
   end subroutine rot_c

   ! Ray versions: rotate position and direction cosines together.
   pure subroutine ray_a(ang, x, y, z, l, m, n)
      real(real64), intent(in) :: ang
      real(real64), intent(inout) :: x, y, z, l, m, n
      call rot_a(ang, x, y, z)
      call rot_a(ang, l, m, n)
   end subroutine ray_a

   pure subroutine ray_b(ang, x, y, z, l, m, n)
      real(real64), intent(in) :: ang
      real(real64), intent(inout) :: x, y, z, l, m, n
      call rot_b(ang, x, y, z)
      call rot_b(ang, l, m, n)
   end subroutine ray_b

   pure subroutine ray_c(ang, x, y, z, l, m, n)
      real(real64), intent(in) :: ang
      real(real64), intent(inout) :: x, y, z, l, m, n
      call rot_c(ang, x, y, z)
      call rot_c(ang, l, m, n)
   end subroutine ray_c

   ! ------------------------------------------------------------------------
   ! Exact port of TRNSF2: move a ray from the frame of surface I-1 (prev)
   ! into the frame of surface I (cur).  x,y,z position; l,m,n direction.
   ! ------------------------------------------------------------------------
   pure subroutine place_into_surface(prev, cur, x, y, z, l, m, n)
      type(surface_placement), intent(in) :: prev, cur
      real(real64), intent(inout) :: x, y, z, l, m, n
      real(real64) :: aeea, beeb, ceec, xeex, yeey, zeez
      real(real64) :: aeeam, beebm, ceecm, xeexm, yeeym, zeezm
      real(real64) :: tdecx, tdecy, tdecz

      aeea = cur%alpha
      beeb = cur%beta
      ceec = cur%gamma
      xeex = cur%dx
      yeey = cur%dy
      zeez = cur%dz
      aeeam = prev%alpha
      beebm = prev%beta
      ceecm = prev%gamma
      xeexm = prev%dx
      yeeym = prev%dy
      zeezm = prev%dz

      if (prev%tilt_flag == 7) then
         ! TILT REV on I-1: do an RTILT on surface I-1
         if (ceecm /= 0.0_real64) call ray_c(-ceecm*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (beebm /= 0.0_real64) call ray_b(-beebm*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (aeeam /= 0.0_real64) call ray_a(-aeeam*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (prev%decenter_flag == 1) then
            x = x + xeexm
            y = y + yeeym
            z = z + zeezm
         end if
      end if

      if (prev%tilt_flag == 4) then
         ! TILT BEN on I-1: one more tilt at surface I-1
         if (aeeam /= 0.0_real64) call ray_a(aeeam*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (beebm /= 0.0_real64) call ray_b(beebm*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (ceecm /= 0.0_real64) call ray_c(ceecm*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
      end if
      if (prev%tilt_flag == 5) then
         ! TILT DAR on I-1: do an RTILT on surface I-1
         if (ceecm /= 0.0_real64) call ray_c(-ceecm*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (beebm /= 0.0_real64) call ray_b(-beebm*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (aeeam /= 0.0_real64) call ray_a(-aeeam*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (prev%decenter_flag == 1) then
            x = x + xeexm
            y = y + yeeym
            z = z + zeezm
         end if
      end if

      if (cur%tilt_flag == 7) then
         z = z - prev%thickness
         return
      end if

      if (cur%tilt_flag == 0) then
         if (cur%decenter_flag == 1) then
            x = x - xeex
            y = y - yeey
            z = z - zeez
         end if
         z = z - prev%thickness
         return
      end if

      if (cur%tilt_flag == 1 .or. cur%tilt_flag == 6 .or. cur%tilt_flag == 2 .or. &
          cur%tilt_flag == 3 .or. cur%tilt_flag == 4 .or. cur%tilt_flag == 5) then
         tdecx = xeex
         tdecy = yeey
         tdecz = zeez
         if (cur%decenter_flag == 1) then
            x = x - tdecx
            y = y - tdecy
            z = z - tdecz
         end if
         z = z - prev%thickness

         if (aeea /= 0.0_real64) call ray_a(aeea*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (beeb /= 0.0_real64) call ray_b(beeb*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (ceec /= 0.0_real64) call ray_c(ceec*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         x = x - cur%global_dx
         y = y - cur%global_dy
         z = z - cur%global_dz
         if (cur%global_alpha /= 0.0_real64) &
            call ray_a(cur%global_alpha*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (cur%global_beta /= 0.0_real64) &
            call ray_b(cur%global_beta*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (cur%global_gamma /= 0.0_real64) &
            call ray_c(cur%global_gamma*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         return
      end if

      if (cur%tilt_flag == -1) then
         ! RTILT
         if (ceec /= 0.0_real64) call ray_c(-ceec*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (beeb /= 0.0_real64) call ray_b(-beeb*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (aeea /= 0.0_real64) call ray_a(-aeea*(PLACE_PII/180.0_real64), x, y, z, l, m, n)
         if (cur%decenter_flag == 1) then
            x = x + xeex
            y = y + yeey
            z = z + zeez
         end if
         z = z - prev%thickness
      end if
   end subroutine place_into_surface

   ! ------------------------------------------------------------------------
   ! Exact port of BAKONE: express the point (tx,ty,tz), given in the frame of
   ! surface NEWOBJ+1 (p1), in the frame of surface NEWOBJ.  obj_thickness is
   ! the thickness of surface NEWOBJ (BAKONE's final surf_thickness(NEWOBJ)).
   ! ------------------------------------------------------------------------
   pure subroutine back_to_object(p1, obj_thickness, tx, ty, tz)
      type(surface_placement), intent(in) :: p1
      real(real64), intent(in) :: obj_thickness
      real(real64), intent(inout) :: tx, ty, tz
      real(real64) :: aeea, beeb, ceec, xeex, yeey, zeez

      aeea = p1%alpha
      beeb = p1%beta
      ceec = p1%gamma
      xeex = p1%dx
      yeey = p1%dy
      zeez = p1%dz

      if (p1%tilt_flag == 0) then
         if (p1%decenter_flag == 1) then
            tx = tx + xeex
            ty = ty + yeey
            tz = tz + zeez
         end if
      else
         if (p1%tilt_flag == -1) then
            ! NEWOBJ+1 was RTILTed: apply a tilt
            if (p1%decenter_flag == 1) then
               tx = tx - xeex
               ty = ty - yeey
               tz = tz - zeez
            end if
            if (aeea /= 0.0_real64) call rot_a(aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (ceec /= 0.0_real64) call rot_c(ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
         end if

         if (p1%tilt_flag == 1 .or. p1%tilt_flag == 4 .or. p1%tilt_flag == 5) then
            ! NEWOBJ+1 was tilted: apply an RTILT
            if (ceec /= 0.0_real64) call rot_c(-ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(-beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (aeea /= 0.0_real64) call rot_a(-aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (p1%decenter_flag == 1) then
               tx = tx + xeex
               ty = ty + yeey
               tz = tz + zeez
            end if
            if (p1%global_gamma /= 0.0_real64) &
               call rot_c(-p1%global_gamma*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (p1%global_beta /= 0.0_real64) &
               call rot_b(-p1%global_beta*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (p1%global_alpha /= 0.0_real64) &
               call rot_a(-p1%global_alpha*(PLACE_PII/180.0_real64), tx, ty, tz)
            tx = tx + p1%global_dx
            ty = ty + p1%global_dy
            tz = tz + p1%global_dz
         end if
         if (p1%tilt_flag == 4) then
            ! TILT BEN: apply an RTILT twice
            if (ceec /= 0.0_real64) call rot_c(-ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(-beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (aeea /= 0.0_real64) call rot_a(-aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (ceec /= 0.0_real64) call rot_c(-ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(-beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (aeea /= 0.0_real64) call rot_a(-aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (p1%decenter_flag == 1) then
               tx = tx + xeex
               ty = ty + yeey
               tz = tz + zeez
            end if
         end if
      end if
      tz = tz + obj_thickness
   end subroutine back_to_object

   ! ------------------------------------------------------------------------
   ! Exact port of FORONEL: express the point (tx,ty,tz), given in the frame
   ! of surface NEWOBJ, in the frame of surface NEWOBJ+1 (p1).
   !
   ! The legacy routine calls SAGINT (surface geometry) when the surface has a
   ! pivot point and PIVAXIS NORMAL; that is the only part which is not pure
   ! placement data.  The caller supplies the result instead: (pn_l,pn_m,pn_n)
   ! is the surface normal SAGINT returns at (pivot_x, pivot_y) of surface
   ! NEWOBJ+1.  The arguments are read only when the pivot branch is taken
   ! (see pivot_normal_needed).
   ! ------------------------------------------------------------------------
   pure logical function pivot_normal_needed(p1)
      type(surface_placement), intent(in) :: p1
      pivot_normal_needed = &
         (p1%tilt_flag == 1 .and. p1%pivot_flag == 1 .and. p1%pivot_axis == 1) .or. &
         (p1%tilt_flag == -1 .and. p1%pivot_flag == 1 .and. p1%pivot_axis == 1) .or. &
         (p1%tilt_flag == 5 .and. p1%pivot_flag == 1 .and. p1%pivot_axis == 1)
   end function pivot_normal_needed

   pure subroutine forward_from_object(p1, pn_l, pn_m, pn_n, tx, ty, tz)
      type(surface_placement), intent(in) :: p1
      real(real64), intent(in) :: pn_l, pn_m, pn_n
      real(real64), intent(inout) :: tx, ty, tz
      real(real64) :: aeea, beeb, ceec
      real(real64) :: alpha, beta
      real(real64) :: lx, mx, nx, ly, my, ny, lz, mz, nz

      aeea = p1%alpha
      beeb = p1%beta
      ceec = p1%gamma

      if (pivot_normal_needed(p1)) then
         ! SAGINT's normal at the pivot, then ALPHA/BETA from it
         alpha = 0.0_real64
         beta = 0.0_real64
         call place_anglecalc(alpha, beta, pn_l, pn_m, pn_n)
         lx = 1.0_real64; mx = 0.0_real64; nx = 0.0_real64
         ly = 0.0_real64; my = 1.0_real64; ny = 0.0_real64
         lz = 0.0_real64; mz = 0.0_real64; nz = 1.0_real64
         if (alpha /= 0.0_real64) then
            call rot_a(alpha*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_a(alpha*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_a(alpha*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         if (beta /= 0.0_real64) then
            call rot_b(beta*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_b(beta*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_b(beta*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         if (aeea /= 0.0_real64) then
            call rot_a(aeea*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_a(aeea*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_a(aeea*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         if (beeb /= 0.0_real64) then
            call rot_b(beeb*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_b(beeb*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_b(beeb*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         if (ceec /= 0.0_real64) then
            call rot_c(ceec*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_c(ceec*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_c(ceec*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         if (beta /= 0.0_real64) then
            call rot_b(-beta*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_b(-beta*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_b(-beta*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         if (alpha /= 0.0_real64) then
            call rot_a(-alpha*(PLACE_PII/180.0_real64), lx, mx, nx)
            call rot_a(-alpha*(PLACE_PII/180.0_real64), ly, my, ny)
            call rot_a(-alpha*(PLACE_PII/180.0_real64), lz, mz, nz)
         end if
         ! Legacy passes NX (not NZ) as the last argument -- faithfully kept.
         call place_newangles(aeea, beeb, ceec, lx, mx, nx, ly, my, ny, lz, mz, nx)
      end if

      if (p1%tilt_flag == 0) then
         ! no tilts
      else
         if (p1%tilt_flag == -1) then
            if (ceec /= 0.0_real64) call rot_c(-ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(-beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (aeea /= 0.0_real64) call rot_a(-aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
         end if
         if (p1%tilt_flag == 1) then
            if (aeea /= 0.0_real64) call rot_a(aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (ceec /= 0.0_real64) call rot_c(ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
         end if
         if (p1%tilt_flag == 4) then
            if (aeea /= 0.0_real64) call rot_a(aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (ceec /= 0.0_real64) call rot_c(ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (aeea /= 0.0_real64) call rot_a(aeea*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (beeb /= 0.0_real64) call rot_b(beeb*(PLACE_PII/180.0_real64), tx, ty, tz)
            if (ceec /= 0.0_real64) call rot_c(ceec*(PLACE_PII/180.0_real64), tx, ty, tz)
         end if
      end if
   end subroutine forward_from_object

   ! Port of ANGLECALC (LDM10.f90).  Legacy leaves ALPHA/SINB undefined on
   ! some degenerate paths; they are initialised to 0 here.
   pure subroutine place_anglecalc(alpha, beta, ll1, mm1, nn1)
      real(real64), intent(inout) :: alpha, beta
      real(real64), intent(in) :: ll1, mm1, nn1
      real(real64) :: d31, d32, d33, cosb
      integer :: sinb
      d31 = ll1
      d32 = mm1
      d33 = nn1
      sinb = 0
      beta = asin(-d31)
      cosb = cos(beta)
      if (cosb /= 0.0_real64) then
         if ((d32/cosb) == 0.0_real64 .and. (d33/cosb) /= 0.0_real64) alpha = 0.0_real64
         if ((d32/cosb) == 0.0_real64 .and. (d33/cosb) == 0.0_real64) alpha = 0.0_real64
         if ((d32/cosb) /= 0.0_real64 .and. (d33/cosb) == 0.0_real64) alpha = PLACE_PII/2.0_real64
         if ((d32/cosb) /= 0.0_real64 .and. (d33/cosb) /= 0.0_real64) alpha = atan2((d32/cosb), (d33/cosb))
      end if
      if (cosb == 0.0_real64) then
         if (d31 == -1.0_real64) sinb = 1
         if (d31 == 1.0_real64) sinb = -1
         if (sinb == 1) beta = PLACE_PII/2.0_real64
         if (sinb == -1) beta = -PLACE_PII/2.0_real64
         if (sinb == 1) then
            if ((d32) == 0.0_real64 .and. (d33) /= 0.0_real64) alpha = 0.0_real64
            if ((d32) == 0.0_real64 .and. (d33) == 0.0_real64) alpha = 0.0_real64
            if ((d32) /= 0.0_real64 .and. (d33) == 0.0_real64) alpha = PLACE_PII/2.0_real64
            if ((d32) /= 0.0_real64 .and. (d33) /= 0.0_real64) alpha = atan2((d32), (d33))
         end if
         if (sinb == -1) then
            if ((d32) == 0.0_real64 .and. (d33) /= 0.0_real64) alpha = 0.0_real64
            if ((d32) == 0.0_real64 .and. (d33) == 0.0_real64) alpha = 0.0_real64
            if ((d32) /= 0.0_real64 .and. (d33) == 0.0_real64) alpha = PLACE_PII/2.0_real64
            if ((d32) /= 0.0_real64 .and. (d33) /= 0.0_real64) alpha = atan2((-d32), (-d33))
         end if
      end if
      alpha = (180.0_real64/PLACE_PII)*alpha
      beta = (180.0_real64/PLACE_PII)*beta
   end subroutine place_anglecalc

   ! Port of NEWANGLES (LDM10.f90).
   pure subroutine place_newangles(aeea, beeb, ceec, lx, mx, nx, ly, my, ny, lz, mz, nz)
      real(real64), intent(inout) :: aeea, beeb, ceec
      ! my, ny (the Y-axis M and N cosines) are unused by the legacy code too
      real(real64), intent(in) :: lx, mx, nx, ly, my, ny, lz, mz, nz
      real(real64) :: d11, d12, d13, d21, d31, d32, d33, cosb
      integer :: sinb
      d11 = lx
      d12 = mx
      d13 = nx
      d21 = ly
      d31 = lz
      d32 = mz
      d33 = nz
      sinb = 0
      beeb = asin(-d31)
      cosb = cos(beeb)
      if (cosb /= 0.0_real64) then
         if ((d32/cosb) == 0.0_real64 .and. (d33/cosb) /= 0.0_real64) aeea = 0.0_real64
         if ((d32/cosb) == 0.0_real64 .and. (d33/cosb) == 0.0_real64) aeea = 0.0_real64
         if ((d32/cosb) /= 0.0_real64 .and. (d33/cosb) == 0.0_real64) aeea = PLACE_PII/2.0_real64
         if ((d32/cosb) /= 0.0_real64 .and. (d33/cosb) /= 0.0_real64) aeea = atan2((d32/cosb), (d33/cosb))
         if ((d21/cosb) == 0.0_real64 .and. (d11/cosb) /= 0.0_real64) ceec = 0.0_real64
         if ((d21/cosb) == 0.0_real64 .and. (d11/cosb) == 0.0_real64) ceec = 0.0_real64
         if ((d21/cosb) /= 0.0_real64 .and. (d11/cosb) == 0.0_real64) ceec = PLACE_PII/2.0_real64
         if ((d21/cosb) /= 0.0_real64 .and. (d11/cosb) /= 0.0_real64) ceec = atan2((-d21/cosb), (d11/cosb))
      end if
      if (cosb == 0.0_real64) then
         if (d31 == -1.0_real64) sinb = 1
         if (d31 == 1.0_real64) sinb = -1
         if (sinb == 1) beeb = PLACE_PII/2.0_real64
         if (sinb == -1) beeb = -PLACE_PII/2.0_real64
         ceec = 0.0_real64
         if (sinb == 1) then
            if ((d12) == 0.0_real64 .and. (d13) /= 0.0_real64) aeea = 0.0_real64
            if ((d12) == 0.0_real64 .and. (d13) == 0.0_real64) aeea = 0.0_real64
            if ((d12) /= 0.0_real64 .and. (d13) == 0.0_real64) aeea = PLACE_PII/2.0_real64
            if ((d12) /= 0.0_real64 .and. (d13) /= 0.0_real64) aeea = atan2((d12), (d13))
         end if
         if (sinb == -1) then
            if ((d12) == 0.0_real64 .and. (d13) /= 0.0_real64) aeea = 0.0_real64
            if ((d12) == 0.0_real64 .and. (d13) == 0.0_real64) aeea = 0.0_real64
            if ((d12) /= 0.0_real64 .and. (d13) == 0.0_real64) aeea = PLACE_PII/2.0_real64
            if ((d12) /= 0.0_real64 .and. (d13) /= 0.0_real64) aeea = atan2((-d12), (-d13))
         end if
      end if
      aeea = (180.0_real64/PLACE_PII)*aeea
      beeb = (180.0_real64/PLACE_PII)*beeb
      ceec = (180.0_real64/PLACE_PII)*ceec
   end subroutine place_newangles

end module mod_surface_placement
