! Drawn (display-only) semi-apertures for the lens drawing (VIE profiles and
! element edges: CAOJK / CAO in PLOTCAD7.f90).  Ray tracing never uses these.
!
! Element diameters: like OSLO and CODE V drawings, both surfaces of a lens
! element are drawn with the same diameter -- the largest drawn semi-aperture
! among the surfaces bounding one contiguous piece of glass (a cemented group
! is one piece).  A surface whose own aperture is smaller is drawn to its own
! aperture and then continues as a flat ring (perpendicular to the axis) out
! to the element diameter, where the element edge joins it.  CAOJK/CAO get
! this through their existing "flat" mechanism: the outer extent is the
! element semi-aperture, the sag is evaluated no further out than the
! surface's own.
!
! Only automatic apertures and centred circular clear apertures take part;
! decentred, non-circular and flat-specified apertures keep their own size.
module lens_draw_apertures
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: lensDrawAutoSemi, lensDrawOwnSemi, lensDrawElementSemi

contains

  ! Automatic (no clear aperture) drawn half-extent along axis (1 = Y,
  ! 2 = X): |marginal| + |chief| paraxial height (the chief term dropped at an
  ! infinite conjugate where it is non-physical), at least the ray-traced
  ! footprint, then the edge size: an explicit CIR EDG value, else the system
  ! edge factor.  The one definition both CAOJK (profile) and CAO (edges)
  ! use, so the profile ends and the element edges meet.
  real(real64) function lensDrawAutoSemi(i, axis) result(ap)
    use DATLEN, only: PXTRAX, PXTRAY
    use mod_lens_data_manager, only: ldm
    use global_widgets, only: sysConfig
    integer, intent(in) :: i, axis
    real(real64) :: marg, chf

    if (axis == 2) then
      marg = abs(PXTRAX(1,i)); chf = abs(PXTRAX(5,i))
    else
      marg = abs(PXTRAY(1,i)); chf = abs(PXTRAY(5,i))
    end if
    if (chf > 1.0d10 .and. chf > 1.0d6*marg) chf = 0.0d0
    ap = marg + chf
    if (axis == 2) then
      ap = max(ap, abs(ldm%getSurfAutoSemiX(i)))
    else
      ap = max(ap, abs(ldm%getSurfAutoSemiY(i)))
    end if
    if (ldm%getEdgeSemiAperture(i) > 0.0d0) then
      ap = ldm%getEdgeSemiAperture(i)
    else
      ap = ap*sysConfig%defaultEdgeScaleFactor
    end if
  end function

  ! The surface's own drawn semi-aperture along axis, or -1 when it does
  ! not take part in element equalization (non-circular, decentred or
  ! flat-specified clear aperture).
  real(real64) function lensDrawOwnSemi(i, axis) result(ap)
    use mod_lens_data_manager, only: ldm
    use global_widgets, only: sysConfig
    use mod_surface, only: surf_clap_type, surf_clap_dim
    integer, intent(in) :: i, axis

    ap = -1.0d0
    select case (int(surf_clap_type(i)))
    case (0)
      ap = lensDrawAutoSemi(i, axis)
    case (1)
      if (surf_clap_dim(i,3) == 0.0d0 .and. surf_clap_dim(i,4) == 0.0d0 .and. &
          surf_clap_dim(i,5) == 0.0d0) then
        ! Circular: the Y size applies in every direction.
        ap = abs(ldm%getClearApertureForLensDraw(i, 1, sysConfig%defaultEdgeScaleFactor))
      end if
    end select
  end function

  ! Semi-aperture to draw surface i's outer edge at: the largest own drawn
  ! semi-aperture over the element (contiguous glass) it bounds; its own
  ! when it bounds no glass or does not take part.
  real(real64) function lensDrawElementSemi(i, axis) result(ap)
    use mod_system, only: sys_last_surf
    integer, intent(in) :: i, axis
    integer :: s0, s1, j, last

    ap = lensDrawOwnSemi(i, axis)
    if (ap < 0.0d0) return
    last = int(sys_last_surf())
    if (.not. (glassAfter(i) .or. (i > 0 .and. glassAfter(i-1)))) return

    s0 = i
    do while (s0 > 1)
      if (.not. glassAfter(s0-1)) exit
      s0 = s0 - 1
    end do
    s1 = i
    do while (s1 < last)
      if (.not. glassAfter(s1)) exit
      s1 = s1 + 1
    end do
    do j = s0, s1
      ap = max(ap, lensDrawOwnSemi(j, axis))
    end do
  end function

  ! True when the medium after surface j is not air (any wavelength's index
  ! differs from 1), as PLTEDG decides where element edges are drawn.
  logical function glassAfter(j)
    use mod_surface, only: surf_refractive_index
    integer, intent(in) :: j
    integer :: k
    glassAfter = .false.
    if (j < 0) return
    do k = 1, 5
      if (abs(abs(surf_refractive_index(j, k)) - 1.0d0) > 1.0d-12) then
        glassAfter = .true.
        return
      end if
    end do
  end function

end module lens_draw_apertures
