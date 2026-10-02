! Surface interaction: the per-surface "intersect and interact" step of the
! legacy real-ray tracer, for ORDINARY surfaces, as a routine on plain data.
!
! The legacy step is HITSUR (RAYTRA2.f90), which for an ordinary surface
! (typed surface object, no special type, not an array, not paraxial) resets
! its bookkeeping, swaps in the aim point on the first surface, calls the typed
! surface's intersect() and then INTERACK (RAYTRA3.f90), which computes the
! incidence cosine, the refraction or reflection, and the "reversed ray" flag.
! The ports here are meant to give IDENTICAL results, bit for bit:
!
!   hit_and_interact  <-  HITSUR   (typed path) + the part of INTERACK that an
!                         ordinary refracting / reflecting surface runs
!   interact_surface  <-  INTERACK (ordinary-surface path, pure)
!   hit_supported     <-  the conditions under which the above is the whole story
!
! The branch order, the arithmetic order and the clamps are the legacy ones.
! Printing (the MSG-gated RAY_FAILURE/SHOWIT calls) is not done here: where
! legacy would print, hit_state%msg_id records the message's id
! (mod_ray_messages); the surface to report is the one passed in as surf.
!
! What is NOT ported.  hit_supported() is false, and hit_and_interact returns
! HIT_UNSUPPORTED without computing anything, for a surface that has
!   * no allocated typed surface object, or a typed-surface array that is stale
!     for the current lens (HITSUR's ubound/getLastSurf guard): legacy falls
!     through to HITFLA/HITASP/HITANA
!   * a special type (surf_special_type /= 0: HOE 12/13, Fresnel 16, 17, grazing
!     18, phase 6/7/9/10/11/15, 20, 24, ...)
!   * an array parity (surf_array_parity /= 0)
!   * a paraxial surface (surf_paraxial_val == 1)
!   * the diffraction flag (surf_diffraction_flag == 1: gratings)
!   * the glass names PERFECT and IDEAL (ideal lenses, which end in "GO TO 90")
!   * a random ray error (surf_ray_error /= 0: the label 90 block)
!   * a clear-aperture y-decenter of exactly 13 (surf_clap_dim(s,4) == 13):
!     INTERACK's reflection branch tests "surf_special_type == 12 .OR.
!     surf_clap_dim(R_I,4) .EQ. 13.0D0" -- a leftover of a HOE flag -- and then
!     runs the HOE deflection with AX/AY/AZ that nothing has set.  The result is
!     garbage, so it is not ported (the gate is static, so refracting surfaces
!     with that decenter are declined too).
!   * a wavelength slot outside 1..10 (legacy would index the ALENS row with an
!     undefined value)
!
! Purity.  Everything here is PURE, including hit_and_interact's call to the
! typed surface's intersect(): the surface_type interfaces in surface-type.f90
! are declared pure, so the compiler guarantees no global state is touched.
!
! This module may use ONLY iso_fortran_env, mod_surface_type and the parameters
! of mod_ray_messages: no legacy data module, ever.  The builder
! (mod_ray_trace_builder) fills surface_optics from the legacy accessors.
module mod_surface_interaction
   use iso_fortran_env, only: real64
   use mod_surface_type, only: surface_type, surf_ray_data
   use mod_ray_messages, only: MSG_NONE, MSG_TIR, MSG_TIR_NOT_MET
   implicit none
   private

   public :: surface_optics, hit_state, hit_supported, hit_unsupported_reason, hit_and_interact, interact_surface

   ! Engine-only status of hit_and_interact (legacy has no such concept).
   integer, parameter, public :: HIT_OK = 0
   integer, parameter, public :: HIT_UNSUPPORTED = -2

   ! Glass-name classes: legacy GLANAM(s,2) compared with 'PERFECT      ' and
   ! 'IDEAL        '.
   integer, parameter, public :: GLASS_ORDINARY = 0
   integer, parameter, public :: GLASS_PERFECT = 1
   integer, parameter, public :: GLASS_IDEAL = 2

   ! Everything HITSUR / INTERACK read about one surface (and its predecessor)
   ! that is not already in surface_placement or surface_apertures.
   type :: surface_optics
      ! Refractive index at the 10 wavelengths: surf_refractive_index(s,1:10),
      ! i.e. ALENS 46-50 and 71-75; the same numbers ldm%getSurfIndex(s,w)
      ! returns.  Negative after a mirror.
      real(real64) :: index(10) = 1.0_real64
      integer :: special_type = 0        ! surf_special_type
      integer :: toric_flag = 0          ! surf_toric_flag (engine gate only)
      integer :: array_parity = 0        ! surf_array_parity
      integer :: paraxial = 0            ! surf_paraxial_val
      integer :: diffraction_flag = 0    ! surf_diffraction_flag
      integer :: reflection_mode = 0     ! surf_reflection_mode (1 = REFLTIRO)
      integer :: dummy_val = 0           ! surf_dummy_val (0: dummy if same index, 1: never)
      logical :: dummy_ok = .false.      ! DUMMMY(s)
      integer :: glass_class = GLASS_ORDINARY   ! GLANAM(s,2): PERFECT / IDEAL / other
      real(real64) :: ray_error = 0.0_real64    ! surf_ray_error
      ! Also held by surface_placement / surface_apertures; repeated here so this
      ! module needs neither: surf_thickness(s) and surf_clap_dim(s,4).
      real(real64) :: thickness = 0.0_real64
      real(real64) :: clap_dim4 = 0.0_real64
      ! The legacy typed-path guard: ldm%surfaces is allocated, its upper bound
      ! equals ldm%getLastSurf(), s is inside it and ldm%surfaces(s)%s is
      ! allocated.
      logical :: typed_valid = .false.
   end type surface_optics

   ! Every legacy global that HITSUR/INTERACK write or (on some path) leave
   ! alone, and the inputs that the ordinary path reads from them.  The caller
   ! loads the current values in; they are updated exactly as the legacy
   ! routines update the globals.  Field -> legacy variable (DATLEN):
   type :: hit_state
      real(real64) :: x = 0.0_real64, y = 0.0_real64, z = 0.0_real64   ! R_X, R_Y, R_Z
      real(real64) :: l = 0.0_real64, m = 0.0_real64, n = 1.0_real64   ! R_L, R_M, R_N
      real(real64) :: ln = 0.0_real64, mn = 0.0_real64, nn = 1.0_real64  ! LN, MN, NN (surface normal)
      real(real64) :: cosi = 0.0_real64     ! COSI
      real(real64) :: cosip = 0.0_real64    ! COSIP
      real(real64) :: l0 = 0.0_real64, m0 = 0.0_real64, n0 = 0.0_real64  ! R_L0, R_M0, R_N0 (ray without
                                           ! the diffraction deflection; /DPAHSE/)
      real(real64) :: oldl = 0.0_real64, oldm = 0.0_real64, oldn = 0.0_real64  ! OLDL, OLDM, OLDN
      real(real64) :: phase = 0.0_real64    ! PHASE
      logical :: rv = .false.               ! RV     (/RAYREV/)
      logical :: rvstart = .false.          ! RVSTART (/ANGER/)
      logical :: tir = .false.              ! TIR    (/RIT/)
      logical :: dum = .false.              ! DUM(R_I)
      integer :: inters = 0, sec = 0        ! INTERS, SEC
      integer :: stopp = 0                  ! STOPP  (/RAYSTP/)
      integer :: raycod(2) = 0              ! RAYCOD (/RAYC/)
      integer :: spdcd1 = 0, spdcd2 = 0     ! SPDCD1, SPDCD2 (/SPRA2/)
      integer :: hoe_do_it = 0              ! HOE_DO_IT
      ! The message legacy prints (under MSG) for the failure it just reported
      ! in raycod/stopp: an id from mod_ray_messages, MSG_NONE if none.  Reset
      ! by every hit_and_interact; the surface to print is raycod(2).
      integer :: msg_id = MSG_NONE
   end type hit_state

contains

   ! ------------------------------------------------------------------------
   ! True when hit_and_interact can handle the surface (see the list at the top
   ! of this file).  geom is the typed surface object (it must be passed as the
   ! allocatable the caller holds, so "not allocated" can be tested); wvn is the
   ! legacy WVN.
   ! ------------------------------------------------------------------------
   pure logical function hit_supported(geom, opt, wvn)
      class(surface_type), allocatable, intent(in) :: geom
      type(surface_optics), intent(in) :: opt
      real(real64), intent(in) :: wvn

      hit_supported = len_trim(hit_unsupported_reason(geom, opt, wvn)) == 0
   end function hit_supported

   ! Why hit_and_interact would decline this surface ('' when it would not).
   pure function hit_unsupported_reason(geom, opt, wvn) result(why)
      class(surface_type), allocatable, intent(in) :: geom
      type(surface_optics), intent(in) :: opt
      real(real64), intent(in) :: wvn
      character(len=48) :: why

      why = ''
      if (.not. allocated(geom)) then
         why = 'no typed surface object'
      else if (.not. opt%typed_valid) then
         why = 'typed surface store out of date'
      else if (opt%special_type /= 0) then
         why = 'special surface type'
      ! Legacy HITSUR's typed path never reads the toric flag, so it intersects
      ! a toric surface as a rotationally symmetric one -- a known bug.  The
      ! engine declines toric surfaces rather than reproduce it, until there is
      ! a toric surface_type; those lenses keep using the legacy tracer.
      else if (opt%toric_flag /= 0) then
         why = 'toric'
      else if (opt%array_parity /= 0) then
         why = 'lens array'
      else if (opt%paraxial == 1) then
         why = 'paraxial surface'
      else if (opt%diffraction_flag == 1) then
         why = 'diffraction grating'
      else if (opt%glass_class /= GLASS_ORDINARY) then
         why = 'PERFECT or IDEAL glass'
      else if (opt%ray_error /= 0.0_real64) then
         why = 'ray error surface'
      else if (opt%clap_dim4 == 13.0_real64) then
         why = 'legacy HOE flag in the clear aperture'
      else if (int(wvn) < 1 .or. int(wvn) > 10) then
         why = 'wavelength slot out of range'
      end if
   end function hit_unsupported_reason

   ! ------------------------------------------------------------------------
   ! Port of HITSUR for an ordinary surface (the typed-surface block), with the
   ! bookkeeping it does on every call.
   !
   ! Inputs
   !   geom, opt     the surface's typed object and optics (R_I = surf)
   !   prev          the previous surface's optics (R_I-1)
   !   surf          legacy R_I
   !   newobj,newimg legacy NEWOBJ, NEWIMG
   !   wvn           legacy WVN (the wavelength slot; INT(WVN) is used)
   !   surtol        legacy SURTOL
   !   revstr        legacy REVSTR
   !   xaim,yaim,zaim legacy R_XAIM, R_YAIM, R_ZAIM (used on surface newobj+1)
   ! In/out
   !   st            the legacy globals described by hit_state.  On entry the
   !                 incoming ray (R_X..R_N) and the current values of everything
   !                 the legacy routines leave alone on some path (DUM(R_I) when
   !                 surf_dummy_val is neither 0 nor 1, COSIP/R_L,R_M,R_N/R_L0.. on
   !                 a failure, RV/RVSTART when nothing resets them, ...).
   ! Out
   !   status        HIT_OK, or HIT_UNSUPPORTED (nothing computed, st unchanged)
   !
   ! ------------------------------------------------------------------------
   pure subroutine hit_and_interact(geom, opt, prev, surf, newobj, newimg, wvn, surtol, revstr, &
                               xaim, yaim, zaim, st, status)
      class(surface_type), allocatable, intent(in) :: geom
      type(surface_optics), intent(in) :: opt, prev
      integer, intent(in) :: surf, newobj, newimg
      real(real64), intent(in) :: wvn, surtol, xaim, yaim, zaim
      logical, intent(in) :: revstr
      type(hit_state), intent(inout) :: st
      integer, intent(out) :: status

      type(surf_ray_data) :: typed_ray
      real(real64) :: or_n, or_z

      if (.not. hit_supported(geom, opt, wvn)) then
         status = HIT_UNSUPPORTED
         return
      end if
      status = HIT_OK

      st%msg_id = MSG_NONE
      st%phase = 0.0_real64

      if (opt%index(1) == prev%index(1) .and. opt%index(2) == prev%index(2) .and. &
          opt%index(3) == prev%index(3) .and. opt%index(4) == prev%index(4) .and. &
          opt%index(5) == prev%index(5) .and. opt%index(6) == prev%index(6) .and. &
          opt%index(7) == prev%index(7) .and. opt%index(8) == prev%index(8) .and. &
          opt%index(9) == prev%index(9) .and. opt%index(10) == prev%index(10)) then
         ! surface is a dummy
         ! legacy quirk: a dummy_val other than 0 or 1 leaves DUM(R_I) as it was
         if (opt%dummy_val == 0) st%dum = .true.
         if (opt%dummy_val == 1) st%dum = .false.
      else
         st%dum = .false.
      end if
      if (.not. opt%dummy_ok) st%dum = .false.
      ! INTERS keeps track of single or multiple surface intersections
      st%inters = 0
      st%sec = 0

      st%oldl = st%l
      st%oldm = st%m
      st%oldn = st%n
      st%stopp = 0

      ! typed surface dispatch
      or_n = st%n
      or_z = st%z
      if (surf == newobj + 1) then
         st%x = xaim; st%y = yaim; st%z = zaim
      end if
      typed_ray%x = st%x; typed_ray%y = st%y; typed_ray%z = st%z
      typed_ray%l = st%l; typed_ray%m = st%m; typed_ray%n = st%n
      call geom%intersect(typed_ray, surtol)
      st%x = typed_ray%x; st%y = typed_ray%y; st%z = typed_ray%z
      st%ln = typed_ray%ln; st%mn = typed_ray%mn; st%nn = typed_ray%nn
      if (st%stopp == 0) then
         call interact_surface(opt, prev, surf, newimg, wvn, revstr, or_n, or_z, st)
         st%hoe_do_it = 0
      end if
   end subroutine hit_and_interact

   ! ------------------------------------------------------------------------
   ! Port of INTERACK for an ordinary surface: diffraction_flag 0, no special
   ! type, glass neither PERFECT nor IDEAL, no ray error (hit_supported).
   ! Called with st%dum, st%ln/mn/nn and the incoming direction already set, as
   ! HITSUR leaves them.
   !   or_n, or_z   legacy OR_N, OR_Z: the incoming R_N and R_Z, saved by HITSUR
   !                BEFORE it replaces the position by the aim point
   ! Dropped: the type-13 setup, the grating and HOE branches, the phase
   ! surfaces, the PERFECT/IDEAL lenses and the random ray error (all gated),
   ! SNIND2 (computed, never used on this path) and the STOPP.EQ.1 early return
   ! (HITSUR only calls INTERACK with STOPP = 0).
   ! ------------------------------------------------------------------------
   pure subroutine interact_surface(opt, prev, surf, newimg, wvn, revstr, or_n, or_z, st)
      type(surface_optics), intent(in) :: opt, prev
      integer, intent(in) :: surf, newimg
      real(real64), intent(in) :: wvn, or_n, or_z
      logical, intent(in) :: revstr
      type(hit_state), intent(inout) :: st

      real(real64) :: signnu, tirtester, sini, rr_n, testlen, rr_z, mag
      real(real64) :: nusubs, smu, sgnb, snindx, j, arg
      real(real64) :: gam1, bgam, blam, bterm, cterm, dd, px, py, pz, qquu
      integer :: iwv

      iwv = int(wvn)
      st%tir = .false.

      st%l0 = st%l
      st%m0 = st%m
      st%n0 = st%n
      ! the cosine of the angle of incidence
      st%cosi = (st%ln*st%l) + (st%mn*st%m) + (st%nn*st%n)
      if (st%cosi < -1.0_real64) st%cosi = -1.0_real64
      if (st%cosi > +1.0_real64) st%cosi = +1.0_real64
      st%phase = 0.0_real64
      rr_z = or_z
      rr_n = or_n
      snindx = abs(prev%index(iwv))/prev%index(iwv)
      if ((1.0_real64 - (st%cosi**2)) < 0.0_real64) then
         sini = 0.0_real64
      else
         sini = sqrt(1.0_real64 - (st%cosi**2))
      end if
      tirtester = sini*snindx
      if (tirtester > 1.0_real64) st%tir = .true.
      nusubs = (prev%index(iwv))/(opt%index(iwv))
      signnu = abs(nusubs)/nusubs
      nusubs = abs(nusubs)
      nusubs = abs(nusubs)
      smu = nusubs

      ! non-grating pre-calculations
      blam = 0.0_real64
      px = 0.0_real64
      py = 0.0_real64
      pz = 0.0_real64
      smu = nusubs

      bterm = 2.0_real64*smu*((st%l*st%ln) + (st%m*st%mn) + (st%n*st%nn))
      cterm = (smu**2) - 1.0_real64 + (blam**2) &
              - ((2.0_real64*smu*blam)*((st%l*px) + (st%m*py) + (st%n*pz)))
      dd = (bterm**2) - (4.0_real64*cterm)
      if (signnu < 0.0_real64) then
         ! a reflection
         if (opt%reflection_mode == 1 .and. tirtester > 1.0_real64) then
            ! "TIR condition not met"
            st%msg_id = MSG_TIR_NOT_MET
            st%raycod(1) = 20
            st%raycod(2) = surf
            st%spdcd1 = st%raycod(1)
            st%spdcd2 = surf
            st%stopp = 1
            return
         end if
         sgnb = 1.0_real64
         if (bterm /= 0.0_real64) sgnb = bterm/abs(bterm)
         qquu = -0.5_real64*(bterm + (sgnb*sqrt(dd)))
         gam1 = qquu
         bgam = gam1
      else
         ! a refraction
         if (dd < 0.0_real64) then
            ! total internal reflection
            st%msg_id = MSG_TIR
            st%raycod(1) = 4
            st%raycod(2) = surf
            st%spdcd1 = st%raycod(1)
            st%spdcd2 = surf
            st%stopp = 1
            return
         end if
         sgnb = 1.0_real64
         if (bterm /= 0.0_real64) sgnb = bterm/abs(bterm)
         qquu = -0.5_real64*(bterm + (sgnb*sqrt(dd)))
         gam1 = qquu
         if (gam1 == 0.0_real64) then
            st%msg_id = MSG_TIR
            st%raycod(1) = 4
            st%raycod(2) = surf
            st%spdcd1 = st%raycod(1)
            st%spdcd2 = surf
            st%stopp = 1
            return
         end if
         bgam = cterm/gam1
      end if

      ! now deflect the ray
      if (.not. st%dum) then
         if (signnu < 0.0_real64) then
            ! reflection
            st%l = (smu*st%l) - (blam*px) + (bgam*st%ln)
            st%m = (smu*st%m) - (blam*py) + (bgam*st%mn)
            st%n = (smu*st%n) - (blam*pz) + (bgam*st%nn)
            mag = sqrt((st%l**2) + (st%m**2) + (st%n**2))
            st%l = st%l/mag
            st%m = st%m/mag
            st%n = st%n/mag
            st%l0 = (st%l0 - ((2.0_real64*st%cosi)*st%ln))
            st%m0 = (st%m0 - ((2.0_real64*st%cosi)*st%mn))
            st%n0 = (st%n0 - ((2.0_real64*st%cosi)*st%nn))
            mag = sqrt((st%l0**2) + (st%m0**2) + (st%n0**2))
            st%l0 = st%l0/mag
            st%m0 = st%m0/mag
            st%n0 = st%n0/mag
            st%cosip = (st%l*st%ln) + (st%m*st%mn) + (st%n*st%nn)
            if (st%cosip < -1.0_real64) st%cosip = -1.0_real64
            if (st%cosip > +1.0_real64) st%cosip = +1.0_real64
         else
            ! not a reflection
            st%l = (smu*st%l) - (blam*px) + (bgam*st%ln)
            st%m = (smu*st%m) - (blam*py) + (bgam*st%mn)
            st%n = (smu*st%n) - (blam*pz) + (bgam*st%nn)
            mag = sqrt((st%l**2) + (st%m**2) + (st%n**2))
            st%l = st%l/mag
            st%m = st%m/mag
            st%n = st%n/mag
            arg = (1.0_real64 - (nusubs**2)*(1.0_real64 - (st%cosi**2)))
            if (arg < 0.0_real64) then
               ! TIR (st%l/m/n have already been changed, as in legacy)
               st%tir = .true.
               st%msg_id = MSG_TIR
               st%raycod(1) = 4
               st%raycod(2) = surf
               st%spdcd1 = st%raycod(1)
               st%spdcd2 = surf
               st%stopp = 1
               return
            end if
            if (st%cosi <= 0.0_real64) st%cosip = -sqrt(arg)
            if (st%cosi > 0.0_real64) st%cosip = sqrt(arg)
            j = ((abs(opt%index(iwv))*st%cosip) &
                 - (abs(prev%index(iwv))*st%cosi))/ &
                (abs(opt%index(iwv)))
            st%l0 = (nusubs*st%l0) + (j*st%ln)
            st%m0 = (nusubs*st%m0) + (j*st%mn)
            st%n0 = (nusubs*st%n0) + (j*st%nn)
            mag = sqrt((st%l0**2) + (st%m0**2) + (st%n0**2))
            st%l0 = st%l0/mag
            st%m0 = st%m0/mag
            st%n0 = st%n0/mag
            st%cosip = (st%l*st%ln) + (st%m*st%mn) + (st%n*st%nn)
            if (st%cosip < -1.0_real64) st%cosip = -1.0_real64
            if (st%cosip > +1.0_real64) st%cosip = +1.0_real64
         end if
      end if

      ! If at a dummy surface the thickness goes from positive to negative or
      ! the reverse, a reversed ray is un-reversed (and vice versa).  (The next
      ! two blocks overwrite RV unconditionally, so this one has no lasting
      ! effect; it is kept because legacy has it.)
      testlen = st%z - rr_z
      if (opt%thickness > 0.0_real64 .and. prev%thickness < 0.0_real64 .and. st%dum .or. &
          opt%thickness < 0.0_real64 .and. prev%thickness > 0.0_real64 .and. st%dum) then
         if (st%rv) then
            st%rv = .false.
         else
            st%rv = .true.
         end if
      end if
      ! A ray that travelled a negative distance with a positive direction cosine
      ! (or the reverse) and was not reversed at the start is "reversed".
      if (rr_n > 0.0_real64 .and. testlen < 0.0_real64 .and. .not. revstr .or. &
          rr_n < 0.0_real64 .and. testlen > 0.0_real64 .and. .not. revstr .or. &
          rr_n > 0.0_real64 .and. testlen > 0.0_real64 .and. revstr .or. &
          rr_n < 0.0_real64 .and. testlen < 0.0_real64 .and. revstr .or. &
          snindx > 0.0_real64 .and. testlen < 0.0_real64 .and. .not. revstr .or. &
          snindx < 0.0_real64 .and. testlen > 0.0_real64 .and. .not. revstr .or. &
          snindx > 0.0_real64 .and. testlen > 0.0_real64 .and. revstr .or. &
          snindx < 0.0_real64 .and. testlen < 0.0_real64 .and. revstr) then
         st%rv = .true.
      else
         st%rv = .false.
      end if
      if (rr_n > 0.0_real64 .and. testlen < 0.0_real64 .and. st%rvstart .or. &
          rr_n < 0.0_real64 .and. testlen > 0.0_real64 .and. st%rvstart) then
         st%rv = .false.
      end if
      if (rr_n > 0.0_real64 .and. testlen > 0.0_real64 .and. .not. revstr .or. &
          rr_n < 0.0_real64 .and. testlen < 0.0_real64 .and. .not. revstr .or. &
          rr_n > 0.0_real64 .and. testlen < 0.0_real64 .and. revstr .or. &
          rr_n < 0.0_real64 .and. testlen > 0.0_real64 .and. revstr) then
         st%rv = .false.
         st%rvstart = .false.
      end if
      ! ray trace through the surface completed
      if (surf == newimg) then
         st%l = st%oldl
         st%m = st%oldm
         st%n = st%oldn
      end if
      st%cosip = (st%l*st%ln) + (st%m*st%mn) + (st%n*st%nn)
      if (st%cosip < -1.0_real64) st%cosip = -1.0_real64
      if (st%cosip > +1.0_real64) st%cosip = +1.0_real64
      st%stopp = 0
      st%raycod(1) = 0
      st%raycod(2) = surf
      st%spdcd1 = st%raycod(1)
      st%spdcd2 = surf
   end subroutine interact_surface

end module mod_surface_interaction
