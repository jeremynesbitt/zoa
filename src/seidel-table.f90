! SEIDEL: a data-only tab (the plot infrastructure without a plot) listing
! the third-order aberrations as OpticStudio's Seidel Coefficients does:
! Seidel S1-S5, CL, CT; wavefront W040 ... W220T; transverse and longitudinal
! coefficients; the Petzval radius.  Surface by surface, the last row the sum.
!
! Everything comes from the Seidel terms MMAB3_NEW computes, which are
! transverse coefficients normalised by INV = -2 n'u' (n', u' the index and
! paraxial marginal slope in the last space), SA3 CMA3 AST3 DIS3 PTZ3 AC3 LC3
! = c(1:7).  OpticStudio's relations (user manual, Seidel Coefficients):
!   S1..S5, CL, CT       = INV * c                (lens units)
!   TSPH = -S1/(2n'u')   = c1     TSCO = c2       TTCO = 3 c2
!   TAST = -S3/(n'u')    = 2 c3   TPFC = c5       TSFC = c3 + c5
!   TTFC = c5 + 3 c3              TDIS = c4       TAXC = 2 c6    TLAC = 2 c7
!   longitudinal         = -transverse / u'  (= S/(2n'u'^2) etc.)
!   W040 = S1/8  W131 = S2/2  W222 = S3/2  W220P = S4/4  W311 = S5/2
!   W020 = CL/2  W111 = CT    W220S/M/T = (S3+S4)/4, (2S3+S4)/4, (3S3+S4)/4
! The Seidel and wavefront tables are in lens units or waves (the Units
! setting); transverse and longitudinal ones are ray errors, in lens units.
module seidel_table
  use iso_fortran_env, only: real64
  implicit none
  private
  public :: seidel_table_go, writeSeidelTables

contains

  subroutine seidel_table_go(psm)
    use GLOBALS, only: HEADLESS_MODE
    use tow_functions, only: initializeGoDataTab, finalizeGoDataTab
    use handlers, only: ioConfig, ID_TERMINAL_DEFAULT
    use plot_functions, only: zoaplot_setting_manager, getTabTextView
    use zoa_ui, only: ID_SEIDEL_TAB
    type(zoaplot_setting_manager) :: psm
    integer :: objIdx
    logical :: replot

    if (HEADLESS_MODE) then
      call writeSeidelTables(psm%getSeidelUnitsSetting())
      return
    end if
    call initializeGoDataTab(psm, ID_SEIDEL_TAB, "Seidel Coefficients", replot, objIdx)
    call ioConfig%setTextViewFromPtr(getTabTextView(objIdx))
    call writeSeidelTables(psm%getSeidelUnitsSetting())
    call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
    call finalizeGoDataTab(psm, replot, objIdx)
  end subroutine

  subroutine writeSeidelTables(inWaves)
    use global_widgets, only: curr_lens_data, curr_par_ray_trace, sysConfig
    use kdp_utils, only: OUTKDP, logColumns
    use type_utils, only: int2str, real2str
    use parax_calcs, only: calcInvariant
    use mod_lens_data_manager, only: ldm
    use mod_system, only: sys_last_surf, sys_wl_ref, sys_wavelength, sys_units
    use DATLEN, only: PXTRAY
    logical, intent(in) :: inWaves
    interface
      subroutine MMAB3_NEW(YFLAG, idxWV, printTable)
        logical, intent(in) :: YFLAG
        integer, intent(in) :: idxWV
        logical, optional, intent(in) :: printTable
      end subroutine
    end interface
    real(real64), allocatable :: S(:,:), W(:,:), T(:,:), L(:,:)
    character(len=8), allocatable :: lbl(:)
    real(real64) :: inv, up, c(7), lam, scale, petz, n0, n1, cv
    integer :: n, k, r, iref
    character(len=40) :: unitTxt, lensUnits

    if (curr_lens_data%num_surfaces < 3) then
      call OUTKDP('SEIDEL: no lens is loaded')
      return
    end if
    iref = int(sys_wl_ref())
    call MMAB3_NEW(.TRUE., iref, .FALSE.)
    if (.not. allocated(curr_par_ray_trace%CSeidel)) return
    n = curr_lens_data%num_surfaces           ! column n of CSeidel is the sum
    inv = calcInvariant()
    up = PXTRAY(2, int(sys_last_surf()) - 1)

    select case (nint(sys_units()))
    case (1); lensUnits = 'in'; lam = sys_wavelength(iref)*1.0e-3_real64/25.4_real64
    case (2); lensUnits = 'cm'; lam = sys_wavelength(iref)*1.0e-4_real64
    case (4); lensUnits = 'm';  lam = sys_wavelength(iref)*1.0e-6_real64
    case default; lensUnits = 'mm'; lam = sys_wavelength(iref)*1.0e-3_real64
    end select
    scale = 1.0_real64
    unitTxt = trim(lensUnits)
    if (inWaves) then
      scale = 1.0_real64/lam
      unitTxt = 'waves at '//trim(real2str(1000.0_real64*sys_wavelength(iref), 2))//' nm'
    end if

    ! rows: surfaces 1 .. image (STO and IMG named), then the sum
    allocate(S(n, 7), W(n, 10), T(n, 10), L(n, 10), lbl(n))
    do r = 1, n
      k = r
      lbl(r) = adjustl(ldm%getSurfName(k))
      if (r == n) lbl(r) = 'SUM'
      c = curr_par_ray_trace%CSeidel(1:7, k)
      S(r, 1:7) = inv*c*scale
      W(r, 1)  = S(r, 1)/8.0_real64                    ! W040
      W(r, 2)  = S(r, 2)/2.0_real64                    ! W131
      W(r, 3)  = S(r, 3)/2.0_real64                    ! W222
      W(r, 4)  = S(r, 4)/4.0_real64                    ! W220P
      W(r, 5)  = S(r, 5)/2.0_real64                    ! W311
      W(r, 6)  = S(r, 6)/2.0_real64                    ! W020
      W(r, 7)  = S(r, 7)                               ! W111
      W(r, 8)  = (S(r, 3) + S(r, 4))/4.0_real64        ! W220S
      W(r, 9)  = (2.0_real64*S(r, 3) + S(r, 4))/4.0_real64   ! W220M
      W(r, 10) = (3.0_real64*S(r, 3) + S(r, 4))/4.0_real64   ! W220T
      T(r, 1)  = c(1)                                  ! TSPH
      T(r, 2)  = c(2)                                  ! TSCO
      T(r, 3)  = 3.0_real64*c(2)                       ! TTCO
      T(r, 4)  = 2.0_real64*c(3)                       ! TAST
      T(r, 5)  = c(5)                                  ! TPFC
      T(r, 6)  = c(3) + c(5)                           ! TSFC
      T(r, 7)  = 3.0_real64*c(3) + c(5)                ! TTFC
      T(r, 8)  = c(4)                                  ! TDIS
      T(r, 9)  = 2.0_real64*c(6)                       ! TAXC
      T(r, 10) = 2.0_real64*c(7)                       ! TLAC
      L(r, :) = 0.0_real64
      if (up /= 0.0_real64) L(r, :) = -T(r, :)/up
    end do

    ! Petzval radius (OpticStudio): 1/Rp = -n_m sum (n_i - n_i-1)/(n_i n_i-1 R_i)
    petz = 0.0_real64
    do k = 1, int(sys_last_surf())
      n0 = abs(ldm%getSurfIndex(k-1, iref))
      n1 = abs(ldm%getSurfIndex(k, iref))
      cv = ldm%getSurfCurv(k, .false.)
      if (n0 > 0.0_real64 .and. n1 > 0.0_real64) petz = petz + (n1 - n0)*cv/(n1*n0)
    end do
    petz = -abs(ldm%getSurfIndex(int(sys_last_surf()), iref))*petz

    call OUTKDP(trim(sysConfig%lensTitle)//'; third-order aberrations at '// &
    &  trim(real2str(1000.0_real64*sys_wavelength(iref), 2))//' nm, the last row is the sum')
    call OUTKDP('')
    call OUTKDP('Seidel aberration coefficients ('//trim(unitTxt)//')')
    call logColumns([character(len=8) :: 'Surface', 'S1 SPHA', 'S2 COMA', 'S3 ASTI', 'S4 FCUR', &
    &                'S5 DIST', 'CL CLA', 'CT CTR'], S, lbl)
    call OUTKDP('')
    call OUTKDP('Wavefront aberration coefficients ('//trim(unitTxt)//')')
    call logColumns([character(len=8) :: 'Surface', 'W040', 'W131', 'W222', 'W220P', 'W311', &
    &                'W020', 'W111', 'W220S', 'W220M', 'W220T'], W, lbl)
    call OUTKDP('')
    call OUTKDP('Transverse aberration coefficients ('//trim(lensUnits)//')')
    call logColumns([character(len=8) :: 'Surface', 'TSPH', 'TSCO', 'TTCO', 'TAST', 'TPFC', &
    &                'TSFC', 'TTFC', 'TDIS', 'TAXC', 'TLAC'], T, lbl)
    call OUTKDP('')
    call OUTKDP('Longitudinal aberration coefficients ('//trim(lensUnits)//')')
    call logColumns([character(len=8) :: 'Surface', 'LSPH', 'LSCO', 'LTCO', 'LAST', 'LPFC', &
    &                'LSFC', 'LTFC', 'LDIS', 'LAXC', 'LLAC'], L, lbl)
    call OUTKDP('')
    if (petz /= 0.0_real64) then
      call OUTKDP('Petzval radius ('//trim(lensUnits)//'): '//trim(real2str(1.0_real64/petz, 6)))
    else
      call OUTKDP('Petzval radius: infinite')
    end if
  end subroutine

end module seidel_table
