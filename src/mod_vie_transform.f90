! Captures the VIE lens-drawing world->screen transform so a screen pixel can be
! mapped back to global (world, mm) coordinates -- used by the cursor-hover
! coordinate readout in the plot toolbar.
!
! The forward transform the renderer applies (planar layouts), per
! PLTSC1/PLTRAE in VIENEW.f90 and DRAWOPTICALSYSTEM in kdp-draw.f90:
!
!   ! 1. look/view rotation of each world vertex (XROT/YROT/ZROT == 0):
!        Xr = X*cos(PH) - Z*sin(PH)                       ! ROT1 about VIEPHI
!        Z1 = X*sin(PH) + Z*cos(PH)
!        Hp = Xr                                          ! ROT2 leaves X alone
!        Vp = -Z1*sin(AL) + Y*cos(AL)                     ! ROT2 about VIEALF
!   ! 2. world(mm) -> KDP plot units:
!        Xplot = (Hp/SCFAX)*1000 + PXSHFT + JUSOFF
!        Yplot = (Vp/SCFAY)*1000 + height_mid + PYSHFT
!   ! 3. KDP plot units -> screen pixels (KDP_CAIRO_SCALE, y-flip about KDP_PLOT_HEIGHT):
!        px = KDP_CAIRO_SCALE * Xplot
!        py = KDP_CAIRO_SCALE * (KDP_PLOT_HEIGHT - Yplot)
!
! Inverting (1)-(3) recovers (Hp, Vp); inverting the rotation with the
! out-of-plane depth assumed 0 recovers world (X,Y,Z).  This is unique only for
! a *planar* layout (VIEALF ~ 0 so a pixel maps to a single in-plane point);
! tilted / Orthographic views map a pixel to a 3D ray, so the readout reports
! "not available" there.
module mod_vie_transform
  use iso_fortran_env, only: real64
  ! Device constants shared with the renderer so the inverse can never drift from
  ! the forward transform in DRAWOPTICALSYSTEM (single source of truth).
  use global_widgets, only: KDP_CAIRO_SCALE, KDP_PLOT_HEIGHT
  implicit none
  private

  real(real64), parameter :: DEG2RAD = 3.14159265358979323846d0/180.0d0

  type :: vie_transform_t
    logical       :: valid    = .false.   ! a VIE plot has been rendered
    logical       :: isPlanar = .false.   ! pixel -> unique world point possible
    real(real64)  :: scfax = 1, scfay = 1
    real(real64)  :: pxshft = 0, pyshft = 0, jusoff = 0
    real(real64)  :: height_mid = 3500.0d0
    real(real64)  :: viephi_deg = 0, viealf_deg = 0
    integer       :: plotOrient = -1
  end type vie_transform_t

  type(vie_transform_t), save :: vieXform   ! the last-rendered VIE transform

  public :: vieXform, capture_vie_transform, vie_pixel_to_world, vie_xform_dump

contains

  ! Snapshot the live transform globals after a VIE render.  Read-only w.r.t. the
  ! drawing pipeline.  Called at the end of VIE_psm where everything is in scope.
  subroutine capture_vie_transform(plotOrient, height_mid)
    use DATLEN,       only: SCFAX, SCFAY, PXSHFT, PYSHFT, JUSOFF, VIEALF, VIEPHI
    use zoa_ui,       only: ID_LENSDRAW_YZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XZ_PLOT_ORIENTATION, &
                          & ID_LENSDRAW_XY_PLOT_ORIENTATION
    integer, intent(in)      :: plotOrient
    real(real64), intent(in) :: height_mid   ! passed in to avoid a module cycle

    vieXform%scfax      = SCFAX
    vieXform%scfay      = SCFAY
    vieXform%pxshft     = real(PXSHFT, real64)
    vieXform%pyshft     = real(PYSHFT, real64)
    vieXform%jusoff     = JUSOFF
    vieXform%height_mid = height_mid
    vieXform%viephi_deg = VIEPHI
    vieXform%viealf_deg = VIEALF
    vieXform%plotOrient = plotOrient
    vieXform%valid      = .true.
    ! Planar (invertible) layouts: the three coordinate-plane views.  Ortho and
    ! any custom-tilted view are 3D projections -> not uniquely invertible.
    vieXform%isPlanar = (plotOrient == ID_LENSDRAW_YZ_PLOT_ORIENTATION) .or. &
                      & (plotOrient == ID_LENSDRAW_XZ_PLOT_ORIENTATION) .or. &
                      & (plotOrient == ID_LENSDRAW_XY_PLOT_ORIENTATION)
  end subroutine capture_vie_transform

  ! Map a screen pixel (drawing-area coords) to world coordinates (mm).
  ! Returns the two in-plane axis labels/values; ok=.false. when no valid planar
  ! transform is available.
  subroutine vie_pixel_to_world(px, py, hLabel, hVal, vLabel, vVal, ok)
    real(real64), intent(in)      :: px, py
    character(len=*), intent(out) :: hLabel, vLabel
    real(real64), intent(out)     :: hVal, vVal
    logical, intent(out)          :: ok

    real(real64) :: Xplot, Yplot, Hp, Vp, ph, al
    real(real64) :: Xw, Yw, Zw, Z1

    ok = .false.
    hLabel = '?'; vLabel = '?'; hVal = 0; vVal = 0
    if (.not. vieXform%valid) return
    if (.not. vieXform%isPlanar) return

    ! (3) pixel -> KDP plot units
    Xplot = px / real(KDP_CAIRO_SCALE, real64)
    Yplot = real(KDP_PLOT_HEIGHT, real64) - py / real(KDP_CAIRO_SCALE, real64)
    ! (2) plot units -> rotated world (mm)
    Hp = (Xplot - vieXform%pxshft - vieXform%jusoff) * vieXform%scfax / 1000.0d0
    Vp = (Yplot - vieXform%height_mid - vieXform%pyshft) * vieXform%scfay / 1000.0d0
    ! (1) invert the look/view rotation, depth (out-of-plane) assumed 0
    ph = vieXform%viephi_deg * DEG2RAD
    al = vieXform%viealf_deg * DEG2RAD
    ! Vp = -Z1*sin(al) + Yw*cos(al), with Z_after = Z1*cos(al)+Yw*sin(al) = 0
    Z1 = -Vp * sin(al)
    Yw =  Vp * cos(al)
    ! Hp = Xr ; invert ROT1 on (Xr, Z1)
    Xw = cos(ph)*Hp + sin(ph)*Z1
    Zw = -sin(ph)*Hp + cos(ph)*Z1

    ! Report the two axes that actually vary across the screen.  For the planar
    ! layouts the vertical axis is the one ROT2 leaves as world Y or world Z;
    ! pick horizontal/vertical by which world axis has the larger sensitivity.
    call pick_inplane_axes(Xw, Yw, Zw, ph, al, hLabel, hVal, vLabel, vVal)
    ok = .true.
  end subroutine vie_pixel_to_world

  ! Decide which named world axis the screen horizontal and vertical each trace,
  ! by the sensitivity of each world axis to a change in screen position
  ! (derived from the inverse rotation -- see vie_pixel_to_world).  Robust for
  ! every planar layout, including XZ where the vertical axis is world X.
  subroutine pick_inplane_axes(Xw, Yw, Zw, ph, al, hLabel, hVal, vLabel, vVal)
    real(real64), intent(in)      :: Xw, Yw, Zw, ph, al
    character(len=*), intent(out) :: hLabel, vLabel
    real(real64), intent(out)     :: hVal, vVal
    real(real64) :: dH(3), dV(3), w(3)
    character(len=1) :: lab(3)
    integer :: ih, iv

    lab = ['X','Y','Z']
    w   = [Xw, Yw, Zw]
    ! |d(world axis)/d(screen horizontal Hp)|
    dH = [ abs(cos(ph)), 0.0d0, abs(sin(ph)) ]
    ! |d(world axis)/d(screen vertical Vp)|
    dV = [ abs(sin(ph)*sin(al)), abs(cos(al)), abs(cos(ph)*sin(al)) ]

    ih = maxloc(dH, dim=1)
    iv = maxloc(dV, dim=1)
    hLabel = lab(ih); hVal = w(ih)
    vLabel = lab(iv); vVal = w(iv)
  end subroutine pick_inplane_axes

  ! Diagnostic dump of the captured transform (headless verification).
  subroutine vie_xform_dump(unitNo)
    integer, intent(in) :: unitNo
    write(unitNo,'(A,L2,A,L2,A,I5)') 'valid=', vieXform%valid, &
      & ' planar=', vieXform%isPlanar, ' orient=', vieXform%plotOrient
    write(unitNo,'(A,2F14.5)') ' scfax,scfay =', vieXform%scfax, vieXform%scfay
    write(unitNo,'(A,3F14.5)') ' pxshft,pyshft,jusoff =', vieXform%pxshft, vieXform%pyshft, vieXform%jusoff
    write(unitNo,'(A,F14.5)')  ' height_mid =', vieXform%height_mid
    write(unitNo,'(A,2F14.5)') ' viephi,viealf (deg) =', vieXform%viephi_deg, vieXform%viealf_deg
  end subroutine vie_xform_dump

end module mod_vie_transform
