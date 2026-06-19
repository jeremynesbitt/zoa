! mod_reference_rays.f90
!
! Typed store for CODE V-style reference rays, kept per field point.
!
! For each defined field point, five reference rays are traced and stored:
!   R1 = chief ray            (pupil center)
!   R2 = upper marginal ray   (+y pupil edge, reduced by VUY vignetting)
!   R3 = lower marginal ray   (-y pupil edge, reduced by VLY vignetting)
!   R4 = +x sagittal ray      (+x pupil edge, reduced by VUX vignetting)
!   R5 = -x sagittal ray      (-x pupil edge, reduced by VLX vignetting)
!
! Each ray is stored as the per-surface real-ray-trace block (RAYRAY/REFRY rows
! 1..RR_NROWS), with surface indices preserved (obj_surf..img_surf) so consumers
! need no re-mapping. Populated by driving the existing FOB/RAY trace path; this
! module never modifies the tracer, only reads its RAYRAY/REFRY outputs.
!
! Singleton `refRays` follows the ldm/sysConfig pattern. A near-future task will
! add VIE subcommands that read this store to plot the rays per field point.

module mod_reference_rays
  use iso_fortran_env, only: real64
  implicit none
  private

  ! Ray indices (1-based) into a field_ray_set%ray(:)
  integer, parameter, public :: R1_CHIEF  = 1
  integer, parameter, public :: R2_UPPER  = 2
  integer, parameter, public :: R3_LOWER  = 3
  integer, parameter, public :: R4_PLUSX  = 4
  integer, parameter, public :: R5_MINUSX = 5
  integer, parameter, public :: NUM_REF_RAYS = 5

  ! Number of RAYRAY/REFRY rows retained per surface (geometry/direction/OPL).
  ! Rows 1-3 X/Y/Z, 4-6 L/M/N, 7 OPL seg, 8 length, 9-10 cos I/I', 11-12 UX/UY,
  ! 13-15 surface normal, 16-21 old X/Y/Z/L/M/N, 22 cumulative OPL.
  integer, parameter, public :: RR_NROWS = 22

  type, public :: ray_trace_record
    integer :: obj_surf = 0
    integer :: img_surf = 0
    logical :: traced_ok = .false.   ! REFEXT (R1) / RAYEXT (R2-R5)
    logical :: vignetted = .false.   ! v1: .not. traced_ok (missed an aperture)
    real(real64), allocatable :: data(:,:)   ! (1:RR_NROWS, obj_surf:img_surf)
  end type

  type, public :: field_ray_set
    real(real64) :: yfrac = 0.0_real64   ! relative field fractions used
    real(real64) :: xfrac = 0.0_real64
    integer      :: wavelength = 1       ! reference wavelength number
    type(ray_trace_record) :: ray(NUM_REF_RAYS)
  end type

  type, public :: reference_ray_manager
    integer :: numFields = 0
    logical :: valid     = .false.       ! store reflects current lens?
    type(field_ray_set), allocatable :: fields(:)
  contains
    procedure :: populate     => rr_populate
    procedure :: clear        => rr_clear
    procedure :: getNumFields => rr_getNumFields
    procedure :: getRay       => rr_getRay
    procedure :: getSurfXYZ   => rr_getSurfXYZ
    procedure :: getImagePoint=> rr_getImagePoint
    procedure :: isVignetted  => rr_isVignetted
    procedure :: genSaveOutputText => rr_genSaveOutputText
  end type

  type(reference_ray_manager), public :: refRays

  ! Re-entrancy guard: populate drives PROCESKDP, and the lens-change auto-refresh
  ! hook calls populate; this prevents any recursion.
  logical :: in_populate = .false.

contains

  ! Empty the store.
  subroutine rr_clear(self)
    class(reference_ray_manager), intent(inout) :: self
    if (allocated(self%fields)) deallocate(self%fields)
    self%numFields = 0
    self%valid = .false.
  end subroutine

  ! (Re)build the reference-ray store for every defined field point by driving the
  ! existing FOB/RAY trace path and copying RAYRAY/REFRY into the typed store. The
  ! tracer is never modified. The user's "current" trace state (last FOB/ray and the
  ! LASTRAY save slot) is snapshotted on entry and restored on exit, so populate is
  ! invisible to subsequent interactive commands.
  subroutine rr_populate(self)
    use DATLEN
    use global_widgets, only: sysConfig
    use type_utils,     only: real2str, int2str
    use zoa_output,     only: zoa_suppress_output
    class(reference_ray_manager), intent(inout) :: self

    integer :: nf, i, k, wl, o, im
    logical :: prevSuppress, discard
    real(real64) :: yf, xf, vig(4), pupY(NUM_REF_RAYS), pupX(NUM_REF_RAYS)

    ! --- snapshot of global trace state (arrays) ---
    real(real64), allocatable :: s_RAYRAY(:,:), s_REFRY(:,:), s_OLREFRY(:,:), &
      s_DIFF(:,:), s_RFDIFF(:,:), s_PXTRAY(:,:), s_PXTRAX(:,:), &
      s_OLD_REF(:,:), s_OLD_RAY(:,:), s_OLD_REF_DIF(:,:), s_OLD_RAY_DIF(:,:), &
      s_O_PXTRAY(:,:), s_O_PXTRAX(:,:), s_LFOB(:), s_LFOBA(:), s_SLFOB(:), s_SLFOBA(:)
    ! --- snapshot (scalars) ---
    real(real64) :: s_WW1, s_WW2, s_WW3, s_WW4, s_WW5, s_RELX, s_RELY, s_CURLAM
    integer :: s_NEWOBJ, s_NEWREF, s_NEWIMG
    logical :: s_REFEXT, s_NULL, s_RAYEXT, s_FAIL, s_FOBYES, s_REVSTR, &
      s_LDIF, s_LDIF2, s_SREFEXT, s_SRAYEXT, s_SLDIF, s_SLDIF2, s_MSG, &
      s_GLOBE, s_GRASET
    character(len=8) :: s_CHLFOB

    if (in_populate) return
    call self%clear()
    nf = sysConfig%numFields
    if (nf < 1) return       ! leaves valid=.false.; caller reports if needed
    in_populate = .true.

    ! Snapshot everything the FOB/RAY path may touch.
    s_RAYRAY = RAYRAY; s_REFRY = REFRY; s_OLREFRY = OLREFRY
    s_DIFF = DIFF; s_RFDIFF = RFDIFF; s_PXTRAY = PXTRAY; s_PXTRAX = PXTRAX
    s_OLD_REF = OLD_REF; s_OLD_RAY = OLD_RAY
    s_OLD_REF_DIF = OLD_REF_DIF; s_OLD_RAY_DIF = OLD_RAY_DIF
    s_O_PXTRAY = O_PXTRAY; s_O_PXTRAX = O_PXTRAX
    s_LFOB = LFOB; s_LFOBA = LFOBA; s_SLFOB = SLFOB; s_SLFOBA = SLFOBA
    s_WW1 = WW1; s_WW2 = WW2; s_WW3 = WW3; s_WW4 = WW4; s_WW5 = WW5
    s_RELX = RELX; s_RELY = RELY; s_CURLAM = CURLAM
    s_NEWOBJ = NEWOBJ; s_NEWREF = NEWREF; s_NEWIMG = NEWIMG
    s_REFEXT = REFEXT; s_NULL = NULL; s_RAYEXT = RAYEXT; s_FAIL = FAIL
    s_FOBYES = FOBYES; s_REVSTR = REVSTR; s_LDIF = LDIF; s_LDIF2 = LDIF2
    s_SREFEXT = SREFEXT; s_SRAYEXT = SRAYEXT; s_SLDIF = SLDIF; s_SLDIF2 = SLDIF2
    s_CHLFOB = CHLFOB
    s_MSG = MSG; s_GLOBE = GLOBE; s_GRASET = GRASET

    ! Silence all output during the housekeeping traces: marginal/sagittal rays at
    ! the full pupil edge legitimately vignette (TIR / aspheric non-convergence) and
    ! their failure messages must not pollute captured output. (MSG alone is
    ! insufficient: the FOB/RAY command path resets it mid-trace.)
    prevSuppress = zoa_suppress_output(.true.)

    allocate(self%fields(nf))
    self%numFields = nf
    wl = sysConfig%getRefWavelengthIndex()

    do i = 1, nf
      xf = sysConfig%relativeFields(1, i)   ! X field fraction
      yf = sysConfig%relativeFields(2, i)   ! Y field fraction
      vig = sysConfig%getVignetting(i)      ! (VUY, VLY, VUX, VLX)

      ! Pupil coordinates (relY, relX) for R1..R5. Vignetting reduces the edge;
      ! factor 0 -> full aperture (+-1). RAY takes (relApeY, relApeX).
      pupY(R1_CHIEF)  = 0.0_real64;          pupX(R1_CHIEF)  = 0.0_real64
      pupY(R2_UPPER)  =  (1.0_real64-vig(1)); pupX(R2_UPPER)  = 0.0_real64
      pupY(R3_LOWER)  = -(1.0_real64-vig(2)); pupX(R3_LOWER)  = 0.0_real64
      pupY(R4_PLUSX)  = 0.0_real64;          pupX(R4_PLUSX)  =  (1.0_real64-vig(3))
      pupY(R5_MINUSX) = 0.0_real64;          pupX(R5_MINUSX) = -(1.0_real64-vig(4))

      self%fields(i)%xfrac = xf
      self%fields(i)%yfrac = yf
      self%fields(i)%wavelength = wl

      ! FOB sets the field and traces the chief ray into REFRY. Pass only the field
      ! (Y X) -- a 3rd FOB argument is NOT the wavelength and corrupts the trace on
      ! some lenses; the wavelength belongs on the RAY command (as execRSI does).
      call PROCESKDP("FOB "//trim(real2str(yf))//" "//trim(real2str(xf)))
      o = NEWOBJ; im = NEWIMG
      call store_record(self%fields(i)%ray(R1_CHIEF), REFRY, o, im, REFEXT)

      ! R2..R5 are real rays at the pupil edges.
      do k = R2_UPPER, R5_MINUSX
        call PROCESKDP("RAY "//trim(real2str(pupY(k)))//" "// &
                       trim(real2str(pupX(k)))//" "//trim(int2str(wl)))
        o = NEWOBJ; im = NEWIMG
        call store_record(self%fields(i)%ray(k), RAYRAY, o, im, RAYEXT)
      end do
    end do

    self%valid = .true.
    discard = zoa_suppress_output(prevSuppress)

    ! Restore the snapshotted state (arrays then scalars).
    RAYRAY = s_RAYRAY; REFRY = s_REFRY; OLREFRY = s_OLREFRY
    DIFF = s_DIFF; RFDIFF = s_RFDIFF; PXTRAY = s_PXTRAY; PXTRAX = s_PXTRAX
    OLD_REF = s_OLD_REF; OLD_RAY = s_OLD_RAY
    OLD_REF_DIF = s_OLD_REF_DIF; OLD_RAY_DIF = s_OLD_RAY_DIF
    O_PXTRAY = s_O_PXTRAY; O_PXTRAX = s_O_PXTRAX
    LFOB = s_LFOB; LFOBA = s_LFOBA; SLFOB = s_SLFOB; SLFOBA = s_SLFOBA
    WW1 = s_WW1; WW2 = s_WW2; WW3 = s_WW3; WW4 = s_WW4; WW5 = s_WW5
    RELX = s_RELX; RELY = s_RELY; CURLAM = s_CURLAM
    NEWOBJ = s_NEWOBJ; NEWREF = s_NEWREF; NEWIMG = s_NEWIMG
    REFEXT = s_REFEXT; NULL = s_NULL; RAYEXT = s_RAYEXT; FAIL = s_FAIL
    FOBYES = s_FOBYES; REVSTR = s_REVSTR; LDIF = s_LDIF; LDIF2 = s_LDIF2
    SREFEXT = s_SREFEXT; SRAYEXT = s_SRAYEXT; SLDIF = s_SLDIF; SLDIF2 = s_SLDIF2
    CHLFOB = s_CHLFOB; MSG = s_MSG; GLOBE = s_GLOBE; GRASET = s_GRASET

    in_populate = .false.
  end subroutine

  ! Copy rows 1..RR_NROWS, surfaces o..im, from a RAYRAY/REFRY-shaped source into a
  ! record; record traced_ok/vignetted from the trace success flag.
  subroutine store_record(rec, src, o, im, ok)
    type(ray_trace_record), intent(inout) :: rec
    real(real64), intent(in) :: src(:, 0:)
    integer, intent(in) :: o, im
    logical, intent(in) :: ok
    rec%obj_surf = o
    rec%img_surf = im
    rec%traced_ok = ok
    rec%vignetted = .not. ok
    if (allocated(rec%data)) deallocate(rec%data)
    allocate(rec%data(RR_NROWS, o:im))
    rec%data(1:RR_NROWS, o:im) = src(1:RR_NROWS, o:im)
  end subroutine

  integer function rr_getNumFields(self) result(n)
    class(reference_ray_manager), intent(in) :: self
    n = self%numFields
  end function

  ! Valid (field,ray) index pair against the current store.
  logical function rr_valid_idx(self, field, rayIdx)
    class(reference_ray_manager), intent(in) :: self
    integer, intent(in) :: field, rayIdx
    rr_valid_idx = allocated(self%fields) .and. &
                   field >= 1 .and. field <= self%numFields .and. &
                   rayIdx >= 1 .and. rayIdx <= NUM_REF_RAYS
  end function

  ! Return a copy of the ray record for (field, ray). On a bad index the returned
  ! record has traced_ok=.false. and no allocated data.
  function rr_getRay(self, field, rayIdx) result(rec)
    class(reference_ray_manager), intent(in) :: self
    integer, intent(in) :: field, rayIdx
    type(ray_trace_record) :: rec
    if (rr_valid_idx(self, field, rayIdx)) then
      rec = self%fields(field)%ray(rayIdx)
    end if
  end function

  ! Per-surface local X/Y/Z (rows 1-3) for (field, ray, surf). ok=.false. if the
  ! index pair or surface is out of range / not traced.
  subroutine rr_getSurfXYZ(self, field, rayIdx, surf, x, y, z, ok)
    class(reference_ray_manager), intent(in) :: self
    integer, intent(in) :: field, rayIdx, surf
    real(real64), intent(out) :: x, y, z
    logical, intent(out) :: ok
    x = 0.0_real64; y = 0.0_real64; z = 0.0_real64; ok = .false.
    if (.not. rr_valid_idx(self, field, rayIdx)) return
    associate (r => self%fields(field)%ray(rayIdx))
      if (.not. allocated(r%data)) return
      if (surf < r%obj_surf .or. surf > r%img_surf) return
      x = r%data(1, surf); y = r%data(2, surf); z = r%data(3, surf)
      ok = .true.
    end associate
  end subroutine

  ! Convenience: ray coordinates at the image surface.
  subroutine rr_getImagePoint(self, field, rayIdx, x, y, ok)
    class(reference_ray_manager), intent(in) :: self
    integer, intent(in) :: field, rayIdx
    real(real64), intent(out) :: x, y
    logical, intent(out) :: ok
    real(real64) :: z
    ok = .false.; x = 0.0_real64; y = 0.0_real64
    if (.not. rr_valid_idx(self, field, rayIdx)) return
    call rr_getSurfXYZ(self, field, rayIdx, self%fields(field)%ray(rayIdx)%img_surf, &
                       x, y, z, ok)
  end subroutine

  logical function rr_isVignetted(self, field, rayIdx) result(v)
    class(reference_ray_manager), intent(in) :: self
    integer, intent(in) :: field, rayIdx
    v = .false.
    if (rr_valid_idx(self, field, rayIdx)) v = self%fields(field)%ray(rayIdx)%vignetted
  end function

  ! Reference rays are derived data, not persisted with the lens (no-op for v1).
  subroutine rr_genSaveOutputText(self, fID)
    class(reference_ray_manager), intent(in) :: self
    integer, intent(in) :: fID
  end subroutine

end module mod_reference_rays
