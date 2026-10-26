! CODE V FMA full-field display (FFD): a performance metric evaluated on a
! rectangular grid of field points.
!
! Metrics (CODE V names):
!   RWE  RMS wavefront error, waves (polychromatic; piston and tilt removed,
!        nominal focus -- as WAV NOM)
!   RSD  RMS spot diameter about the centroid, lens units (polychromatic)
!   DST  distortion, %: (real chief ray image point - ideal) radially, ideal
!        = EFL*tan(chief ray angle) for an infinite object, or
!        magnification*object point for a finite one (as DIX/DIY)
!
! Field points are in the lens's field units (degrees or height), x across,
! y up.  KDP traces a field point as a fraction of the reference field in
! each direction, and a rotationally symmetric lens usually has a zero X
! reference (XAN all 0), which makes every x a zero field.  For the duration
! of the map the X reference is then set equal to the Y one (and the system
! data restored afterwards).
module field_map
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: FMA_RWE, FMA_RSD, FMA_DST, fma_metric_name, fma_metric_unit, fma_evaluate

  integer, parameter :: FMA_RWE = 1, FMA_RSD = 2, FMA_DST = 3

contains

  function fma_metric_name(m) result(s)
    integer, intent(in) :: m
    character(len=3) :: s
    select case (m)
    case (FMA_RSD); s = 'RSD'
    case (FMA_DST); s = 'DST'
    case default;   s = 'RWE'
    end select
  end function

  function fma_metric_unit(m) result(s)
    use global_widgets, only: sysConfig
    integer, intent(in) :: m
    character(len=20) :: s
    select case (m)
    case (FMA_RSD); s = trim(sysConfig%getLensUnitsText())
    case (FMA_DST); s = '%'
    case default;   s = 'waves'
    end select
  end function

  ! Evaluate metric m on the nx x ny grid xs(1:nx) x ys(1:ny) (field units).
  ! val(i,j) at (xs(i), ys(j)); ok(i,j) false where the trace failed.  For
  ! DST, img(1:4,i,j) = real x, real y, ideal x, ideal y at the image.
  subroutine fma_evaluate(m, nrd, xs, ys, val, ok, img)
    use global_widgets, only: sysConfig
    use DATLEN, only: SYSTEM
    use DATSP1, only: SPDTYPE, NRECT
    use mod_kdp_api, only: kdp_silent_begin, kdp_silent_end, kdp_lens_begin, &
                           kdp_lens_cmd, kdp_lens_end
    use kdp_data_types, only: FIELD_OBJECT_HEIGHT, FIELD_OBJECT_ANGLE_DEG, &
         FIELD_PARAX_IMAGE_HEIGHT, FIELD_PARAX_IMAGE_SLOPE_TAN, &
         FIELD_REAL_IMAGE_HEIGHT, FIELD_REAL_IMAGE_HEIGHT_DEG
    use wavefront_analysis, only: wav_point_rms
    integer, intent(in) :: m, nrd
    real(real64), intent(in) :: xs(:), ys(:)
    real(real64), intent(out) :: val(:,:), img(:,:,:)
    logical, intent(out) :: ok(:,:)
    real(real64) :: savedSystem(150), refX, refY, savedRefX
    integer :: i, j, savedSpdType, savedNRect
    logical :: tempX

    val = 0.0_real64; img = 0.0_real64; ok = .false.
    refY = sysConfig%refFieldValue(2)
    refX = sysConfig%refFieldValue(1)
    if (refY == 0.0_real64) refY = refX
    if (refY == 0.0_real64) return
    tempX = (refX == 0.0_real64)
    savedRefX = sysConfig%refFieldValue(1)
    savedSystem = SYSTEM(1:150)
    if (tempX) then
      refX = refY
      call kdp_silent_begin()
      call kdp_lens_begin()
      select case (sysConfig%currFieldID)
      case (FIELD_OBJECT_HEIGHT)
        call kdp_lens_cmd('SCX', w1=refX)
      case (FIELD_OBJECT_ANGLE_DEG)
        call kdp_lens_cmd('SCX', w1=refX, wq='FANG')
      case (FIELD_PARAX_IMAGE_HEIGHT, FIELD_PARAX_IMAGE_SLOPE_TAN)
        call kdp_lens_cmd('PXIM', w1=refX)
      case (FIELD_REAL_IMAGE_HEIGHT, FIELD_REAL_IMAGE_HEIGHT_DEG)
        call kdp_lens_cmd('RXIM', w1=refX)
      end select
      call kdp_lens_end()
      call kdp_silent_end()
    end if

    savedSpdType = SPDTYPE
    savedNRect   = NRECT
    do j = 1, size(ys)
      do i = 1, size(xs)
        select case (m)
        case (FMA_RWE)
          call wav_point_rms(ys(j)/refY, xs(i)/refX, nrd, val(i,j), ok(i,j))
        case (FMA_RSD)
          SPDTYPE = 1
          NRECT   = nrd
          call spot_rms_diameter(ys(j)/refY, xs(i)/refX, val(i,j), ok(i,j))
        case (FMA_DST)
          call chief_distortion(ys(j)/refY, xs(i)/refX, val(i,j), img(:,i,j), ok(i,j))
        end select
      end do
    end do
    SPDTYPE = savedSpdType
    NRECT   = savedNRect

    if (tempX) then
      SYSTEM(1:150) = savedSystem
      call kdp_silent_begin()
      call kdp_lens_begin()
      call kdp_lens_end()
      call kdp_silent_end()
      sysConfig%refFieldValue(1) = savedRefX
    end if
  end subroutine

  ! RMS spot diameter (2 x RMS radius about the energy-weighted centroid).
  subroutine spot_rms_diameter(fy, fx, d, ok)
    use GLOBALS, only: DSPOTT
    use DATSP1, only: ITOT
    real(real64), intent(in) :: fy, fx
    real(real64), intent(out) :: d
    logical, intent(out) :: ok
    character(len=200) :: line
    real(real64) :: cx, cy, wsum, r2
    integer :: i, n

    d = 0.0_real64; ok = .false.
    write(line, '(A,F0.8,1X,F0.8,A)') 'FOB ', fy, fx, '; SPD'
    call PROCESSILENT(trim(line))
    if (.not. allocated(DSPOTT)) return
    cx = 0.0_real64; cy = 0.0_real64; wsum = 0.0_real64; n = 0
    do i = 1, ITOT - 1
      if (DSPOTT(17,i) == 0.0_real64 .or. DSPOTT(12,i) == 0.0_real64) cycle
      wsum = wsum + DSPOTT(12,i)
      cx = cx + DSPOTT(12,i)*DSPOTT(1,i)
      cy = cy + DSPOTT(12,i)*DSPOTT(2,i)
      n = n + 1
    end do
    if (n < 1 .or. wsum <= 0.0_real64) return
    cx = cx/wsum; cy = cy/wsum
    r2 = 0.0_real64
    do i = 1, ITOT - 1
      if (DSPOTT(17,i) == 0.0_real64 .or. DSPOTT(12,i) == 0.0_real64) cycle
      r2 = r2 + DSPOTT(12,i)*((DSPOTT(1,i) - cx)**2 + (DSPOTT(2,i) - cy)**2)
    end do
    d = 2.0_real64*sqrt(r2/wsum)
    ok = .true.
  end subroutine

  ! Radial distortion (%) of the chief ray, and its real and ideal image
  ! points (x, y, x_ideal, y_ideal).
  subroutine chief_distortion(fy, fx, pct, pts, ok)
    use DATLEN, only: REFRY, REFEXT, NEWOBJ, NEWIMG
    use global_widgets, only: curr_par_ray_trace
    use mod_surface, only: surf_thickness
    real(real64), intent(in) :: fy, fx
    real(real64), intent(out) :: pct, pts(4)
    logical, intent(out) :: ok
    character(len=200) :: line
    real(real64) :: ri, ra

    pct = 0.0_real64; pts = 0.0_real64; ok = .false.
    write(line, '(A,F0.8,1X,F0.8)') 'FOB ', fy, fx
    call PROCESSILENT(trim(line))
    if (.not. REFEXT) return
    pts(1:2) = REFRY(1:2, NEWIMG)
    if (abs(surf_thickness(NEWOBJ)) >= 1.0e10_real64) then
      if (REFRY(6, NEWOBJ) == 0.0_real64) return
      pts(3:4) = curr_par_ray_trace%EFL*REFRY(4:5, NEWOBJ)/REFRY(6, NEWOBJ)
    else
      pts(3:4) = curr_par_ray_trace%t_mag*REFRY(1:2, NEWOBJ)
    end if
    ri = hypot(pts(3), pts(4))
    ra = hypot(pts(1), pts(2))
    if (ri > 0.0_real64) pct = 100.0_real64*(ra - ri)/ri
    ok = .true.
  end subroutine

end module field_map
