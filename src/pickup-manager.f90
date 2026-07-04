! pickup-manager.f90
!
! Typed manager for KDP surface-parameter pickups.  Wraps the legacy
! PIKUP(1:6, 0:499, 1:45) COMMON array (mod_DATLEN) as its backing store --
! the same incremental pattern as the typed objects over ALENS -- so the
! ~1000 legacy readers in LDM*/OPTIM*/CONFIG* keep working untouched while
! new-world code (CodeV CLI, lens editor, save path) goes through this API.
!
! PIKUP slot layout (per surface, per kind J):
!   PIKUP(1,s,J) = existence flag (1.0 = pickup defined)
!   PIKUP(2,s,J) = source surface
!   PIKUP(3,s,J) = multiplier (scale)
!   PIKUP(4,s,J) = additive constant (offset)
!   PIKUP(5,s,J) = extra parameter (type-specific, e.g. thickness W4)
!   PIKUP(6,s,J) = config indicator (0 = current lens, 1 = main/permanent)
!
! The kind table below is THE single source of truth for the J index <->
! KDP qualifier word <-> CodeV CLI parameter name mapping.  It mirrors the
! (welded, behavior-defining) qualifier chains in PIKUPS2.f90 -- SPIKD's
! K-map (~line 633) and SPIKUP's dispatch (~line 1495).  If a new pickup
! type is ever added there, add its row here.
!
! Resolution deliberately stays in the legacy PIKRES (PIKUPS1.f90): that is
! where the golden-ref risk lives, and it already refreshes the typed
! surface store at its end.
module pickup_manager
  use iso_fortran_env, only: real64
  implicit none
  private

  public :: pickup_kind, PIKUP_KINDS, NUM_PICKUP_KINDS
  public :: pickup_j_from_qual, pickup_qual_from_j
  public :: pickup_j_from_cli, pickup_cli_from_j

  type :: pickup_kind
    integer           :: j          ! PIKUP array 3rd index
    character(len=8)  :: qual       ! KDP PIKUP/PIKD qualifier word
    character(len=4)  :: cli        ! CodeV param name for PIK/save (blank = not CLI-exposed)
    character(len=24) :: label      ! human-readable description
  end type

  integer, parameter :: NUM_PICKUP_KINDS = 44

  type(pickup_kind), parameter :: PIKUP_KINDS(NUM_PICKUP_KINDS) = [ &
    pickup_kind( 1, 'RD      ', 'RDY ', 'Radius                  '), &
    pickup_kind( 2, 'CV      ', '    ', 'Curvature               '), &
    pickup_kind( 3, 'TH      ', 'THI ', 'Thickness               '), &
    pickup_kind( 4, 'CC      ', 'K   ', 'Conic constant          '), &
    pickup_kind( 5, 'AD      ', 'A   ', 'Asphere A4              '), &
    pickup_kind( 6, 'AE      ', 'B   ', 'Asphere A6              '), &
    pickup_kind( 7, 'AF      ', 'C   ', 'Asphere A8              '), &
    pickup_kind( 8, 'AG      ', 'D   ', 'Asphere A10             '), &
    pickup_kind( 9, 'CVTOR   ', '    ', 'Toric curvature         '), &
    pickup_kind(10, 'RDTOR   ', '    ', 'Toric radius            '), &
    pickup_kind(11, 'PRO     ', '    ', 'Profile                 '), &
    pickup_kind(12, 'NPRO    ', '    ', 'Negated profile         '), &
    pickup_kind(13, 'YD      ', '    ', 'Y decenter              '), &
    pickup_kind(14, 'XD      ', '    ', 'X decenter              '), &
    pickup_kind(15, 'ALPHA   ', '    ', 'Alpha tilt              '), &
    pickup_kind(16, 'BETA    ', '    ', 'Beta tilt               '), &
    pickup_kind(17, 'GAMMA   ', '    ', 'Gamma tilt              '), &
    pickup_kind(18, 'CLAP    ', '    ', 'Clear aperture          '), &
    pickup_kind(19, 'COBS    ', '    ', 'Obscuration             '), &
    pickup_kind(20, 'GLASS   ', 'GLA ', 'Glass                   '), &
    pickup_kind(21, 'CCTOR   ', '    ', 'Toric conic             '), &
    pickup_kind(22, 'ADTOR   ', '    ', 'Toric asphere A4        '), &
    pickup_kind(23, 'AETOR   ', '    ', 'Toric asphere A6        '), &
    pickup_kind(24, 'AFTOR   ', '    ', 'Toric asphere A8        '), &
    pickup_kind(25, 'AGTOR   ', '    ', 'Toric asphere A10       '), &
    pickup_kind(26, 'AC      ', '    ', 'Asphere A2 (plano)      '), &
    pickup_kind(27, 'AH      ', 'E   ', 'Asphere A12             '), &
    pickup_kind(28, 'AI      ', 'F   ', 'Asphere A14             '), &
    pickup_kind(29, 'AJ      ', 'G   ', 'Asphere A16             '), &
    pickup_kind(30, 'AK      ', 'H   ', 'Asphere A18             '), &
    pickup_kind(31, 'AL      ', 'I   ', 'Asphere A20             '), &
    pickup_kind(32, 'THOAL   ', '    ', 'Thickness over range    '), &
    pickup_kind(33, 'ZD      ', '    ', 'Z decenter              '), &
    pickup_kind(34, 'PIVX    ', '    ', 'Pivot X                 '), &
    pickup_kind(35, 'PIVY    ', '    ', 'Pivot Y                 '), &
    pickup_kind(36, 'PIVZ    ', '    ', 'Pivot Z                 '), &
    pickup_kind(37, 'GDX     ', '    ', 'Global decenter X       '), &
    pickup_kind(38, 'GDY     ', '    ', 'Global decenter Y       '), &
    pickup_kind(39, 'GDZ     ', '    ', 'Global decenter Z       '), &
    pickup_kind(40, 'GALPHA  ', '    ', 'Global alpha tilt       '), &
    pickup_kind(41, 'GBETA   ', '    ', 'Global beta tilt        '), &
    pickup_kind(42, 'GGAMMA  ', '    ', 'Global gamma tilt       '), &
    pickup_kind(43, 'GRT     ', '    ', 'Grating                 '), &
    pickup_kind(44, 'COATING ', '    ', 'Coating                 ') ]

contains

  ! PIKUP J index for a KDP qualifier word (0 if unknown)
  function pickup_j_from_qual(qual) result(j)
    character(len=*), intent(in) :: qual
    integer :: j, i
    j = 0
    do i = 1, NUM_PICKUP_KINDS
      if (trim(PIKUP_KINDS(i)%qual) == trim(qual)) then
        j = PIKUP_KINDS(i)%j
        return
      end if
    end do
  end function

  ! KDP qualifier word for a PIKUP J index (blank if out of range)
  function pickup_qual_from_j(j) result(qual)
    integer, intent(in) :: j
    character(len=8) :: qual
    qual = ' '
    if (j >= 1 .and. j <= NUM_PICKUP_KINDS) qual = PIKUP_KINDS(j)%qual
  end function

  ! PIKUP J index for a CodeV CLI parameter name (RDY/THI/K/A..I/GLA; 0 if none)
  function pickup_j_from_cli(cli) result(j)
    character(len=*), intent(in) :: cli
    integer :: j, i
    j = 0
    do i = 1, NUM_PICKUP_KINDS
      if (len_trim(PIKUP_KINDS(i)%cli) > 0 .and. &
      &   trim(PIKUP_KINDS(i)%cli) == trim(cli)) then
        j = PIKUP_KINDS(i)%j
        return
      end if
    end do
  end function

  ! CodeV CLI parameter name for a PIKUP J index (blank if not CLI-exposed)
  function pickup_cli_from_j(j) result(cli)
    integer, intent(in) :: j
    character(len=4) :: cli
    cli = ' '
    if (j >= 1 .and. j <= NUM_PICKUP_KINDS) cli = PIKUP_KINDS(j)%cli
  end function

end module pickup_manager
