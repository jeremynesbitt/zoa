! solve-manager.f90
!
! Typed manager for KDP surface solves.  Wraps the legacy SOLVE(0:9, 0:499)
! COMMON array (mod_DATLEN) as its backing store -- the same incremental
! pattern as pickup_manager over PIKUP and the typed objects over ALENS -- so
! the legacy readers (SLVRS resolution, LDM1/10/12/14/17 handlers) keep
! working untouched while new-world code (ldm queries, lens editor, CLI, save
! path) goes through this API.
!
! SOLVE slot layout (per surface): each solve stores a TYPE CODE in one slot
! and its numeric TARGET in another, one (type,target) pair per plane/class:
!   YZ thickness : code in SOLVE(6,s), target in SOLVE(7,s)
!   XZ thickness : code in SOLVE(4,s), target in SOLVE(3,s)
!   YZ curvature : code in SOLVE(8,s), target in SOLVE(9,s)
!   XZ curvature : code in SOLVE(2,s), target in SOLVE(1,s)
!   slots 0,5 unused.
!
! The kind table below is THE single source of truth for the
! command <-> (slot, code) mapping.  It was cross-checked three ways against
! the welded legacy code: the creation writers (THSOLV in LDM1 ~2721, CVSOLV
! in LDM12 ~533), the resolver (SLVRS, LDM5), and the lens-file writer (LDM17
! ~2446-2578).  If a solve type is ever added there, add its row here.
!
! Known legacy bug (faithfully preserved, not encoded here): CVSOLV writes the
! COCX code (14) into SOLVE(8) (a YZ slot) while LDM17 reads/writes it from
! SOLVE(2) (XZ) -- so COCX is created in the wrong slot and never persists.
! This table uses the writer/XZ-scheme convention (slot 2); see KNOWN_ISSUES.
!
! Resolution deliberately stays in the legacy SLVRS (LDM5.f90): that is where
! the golden-ref risk lives, and it already refreshes the typed surface store
! at its end.
module solve_manager
  use iso_fortran_env, only: real64
  implicit none
  private

  public :: solve_kind, SOLVE_KINDS, NUM_SOLVE_KINDS
  public :: SLV_YZ_THI_SLOT, SLV_XZ_THI_SLOT, SLV_YZ_CURV_SLOT, SLV_XZ_CURV_SLOT
  public :: solve_kind_from_cmd, solve_kind_at
  public :: solve_code, solve_target
  public :: surf_has_thi_solve, surf_has_curv_solve, surf_has_any_solve
  public :: solve_set_cmd, solve_remove_cmd, solve_del_cmd
  public :: solve_kdp_from_codev, solve_save_line

  ! The four type-code slots (each has a partner target slot, see slot layout).
  integer, parameter :: SLV_YZ_THI_SLOT  = 6,  SLV_YZ_THI_TGT  = 7
  integer, parameter :: SLV_XZ_THI_SLOT  = 4,  SLV_XZ_THI_TGT  = 3
  integer, parameter :: SLV_YZ_CURV_SLOT = 8,  SLV_YZ_CURV_TGT = 9
  integer, parameter :: SLV_XZ_CURV_SLOT = 2,  SLV_XZ_CURV_TGT = 1

  ! Decimal places used when writing solve targets to a lens file.
  integer, parameter :: SOLVE_SAVE_DECIMALS = 8

  ! CODE V mapping: solves are set from the CODE V layer as
  !   THI Sk <qual> j   (thickness)      e.g. THI S2 HCY 1.5  -> KDP PCY
  !   CUY Sk <qual> j   (YZ curvature)   e.g. CUY S3 UMY 0.1  -> KDP PUY
  !   CUX Sk <qual> j   (XZ curvature)   e.g. CUX S3 AMX      -> KDP APX
  ! where qual = quantity(H/A/I/U) . ray(M=marginal/C=chief) . plane(Y/X).
  ! REDSLV keeps its own dedicated RED command; PY 0 keeps its PIM shortcut.
  ! CAY/CAX/COCY/COCX have no CODE V spelling (codev_cmd == '') -> GUI/KDP only.
  type :: solve_kind
    character(len=8) :: kdp_cmd    ! KDP command word (PY, PCY, APY, PUX, ...)
    integer          :: code       ! value stored in type_slot for this kind
    integer          :: type_slot  ! SOLVE slot holding the code
    integer          :: tgt_slot   ! SOLVE slot holding the numeric target
    integer          :: nparams    ! 0 (aplanatic: APY/APCY/APX/APCX) else 1
    character(len=4) :: del_cmd    ! delete command: TSD / CSDY / CSDX
    character(len=8) :: plane_cls  ! 'YZ thi' / 'XZ thi' / 'YZ curv' / 'XZ curv'
    character(len=4) :: codev_cmd  ! CODE V verb: THI / CUY / CUX / RED / '' none
    character(len=4) :: codev_qual ! CODE V qualifier: HMY / UMY / AMX / '' none
  end type

  integer, parameter :: NUM_SOLVE_KINDS = 21

  type(solve_kind), parameter :: SOLVE_KINDS(NUM_SOLVE_KINDS) = [ &
    ! --- YZ thickness (code in slot 6, target in slot 7; delete via TSD) ---
    solve_kind('PY      ',  1, SLV_YZ_THI_SLOT,  SLV_YZ_THI_TGT,  1, 'TSD ', 'YZ thi  ', 'THI ', 'HMY '), &
    solve_kind('PCY     ',  2, SLV_YZ_THI_SLOT,  SLV_YZ_THI_TGT,  1, 'TSD ', 'YZ thi  ', 'THI ', 'HCY '), &
    solve_kind('CAY     ',  3, SLV_YZ_THI_SLOT,  SLV_YZ_THI_TGT,  1, 'TSD ', 'YZ thi  ', '    ', '    '), &
    solve_kind('REDSLV  ',  7, SLV_YZ_THI_SLOT,  SLV_YZ_THI_TGT,  1, 'TSD ', 'YZ thi  ', 'RED ', '    '), &
    ! --- XZ thickness (code in slot 4, target in slot 3; delete via TSD) ---
    solve_kind('PX      ',  4, SLV_XZ_THI_SLOT,  SLV_XZ_THI_TGT,  1, 'TSD ', 'XZ thi  ', 'THI ', 'HMX '), &
    solve_kind('PCX     ',  5, SLV_XZ_THI_SLOT,  SLV_XZ_THI_TGT,  1, 'TSD ', 'XZ thi  ', 'THI ', 'HCX '), &
    solve_kind('CAX     ',  6, SLV_XZ_THI_SLOT,  SLV_XZ_THI_TGT,  1, 'TSD ', 'XZ thi  ', '    ', '    '), &
    ! --- YZ curvature (code in slot 8, target in slot 9; delete via CSDY) ---
    solve_kind('APY     ',  1, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 0, 'CSDY', 'YZ curv ', 'CUY ', 'AMY '), &
    solve_kind('PIY     ',  2, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 1, 'CSDY', 'YZ curv ', 'CUY ', 'IMY '), &
    solve_kind('PUY     ',  3, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 1, 'CSDY', 'YZ curv ', 'CUY ', 'UMY '), &
    solve_kind('APCY    ',  4, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 0, 'CSDY', 'YZ curv ', 'CUY ', 'ACY '), &
    solve_kind('PICY    ',  5, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 1, 'CSDY', 'YZ curv ', 'CUY ', 'ICY '), &
    solve_kind('PUCY    ',  6, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 1, 'CSDY', 'YZ curv ', 'CUY ', 'UCY '), &
    solve_kind('COCY    ',  7, SLV_YZ_CURV_SLOT, SLV_YZ_CURV_TGT, 1, 'CSDY', 'YZ curv ', '    ', '    '), &
    ! --- XZ curvature (code in slot 2, target in slot 1; delete via CSDX) ---
    solve_kind('APX     ',  8, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 0, 'CSDX', 'XZ curv ', 'CUX ', 'AMX '), &
    solve_kind('PIX     ',  9, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 1, 'CSDX', 'XZ curv ', 'CUX ', 'IMX '), &
    solve_kind('PUX     ', 10, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 1, 'CSDX', 'XZ curv ', 'CUX ', 'UMX '), &
    solve_kind('APCX    ', 11, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 0, 'CSDX', 'XZ curv ', 'CUX ', 'ACX '), &
    solve_kind('PICX    ', 12, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 1, 'CSDX', 'XZ curv ', 'CUX ', 'ICX '), &
    solve_kind('PUCX    ', 13, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 1, 'CSDX', 'XZ curv ', 'CUX ', 'UCX '), &
    solve_kind('COCX    ', 14, SLV_XZ_CURV_SLOT, SLV_XZ_CURV_TGT, 1, 'CSDX', 'XZ curv ', '    ', '    ') ]

contains

  ! Kind-table index for a KDP command word (0 if unknown).
  function solve_kind_from_cmd(cmd) result(idx)
    character(len=*), intent(in) :: cmd
    integer :: idx, i
    idx = 0
    do i = 1, NUM_SOLVE_KINDS
      if (trim(SOLVE_KINDS(i)%kdp_cmd) == trim(cmd)) then
        idx = i
        return
      end if
    end do
  end function

  ! Kind-table index for the solve currently present in (type_slot, code).
  ! 0 if that slot holds no (or an unknown) code.
  function solve_kind_at(type_slot, code) result(idx)
    integer, intent(in) :: type_slot, code
    integer :: idx, i
    idx = 0
    if (code == 0) return
    do i = 1, NUM_SOLVE_KINDS
      if (SOLVE_KINDS(i)%type_slot == type_slot .and. SOLVE_KINDS(i)%code == code) then
        idx = i
        return
      end if
    end do
  end function

  ! Integer type-code stored in a SOLVE type-slot on surface s (0 => none).
  function solve_code(s, type_slot) result(code)
    use DATLEN, only: SOLVE
    integer, intent(in) :: s, type_slot
    integer :: code
    code = 0
    if (s < 0 .or. s > 499) return
    if (type_slot < 0 .or. type_slot > 9) return
    code = nint(SOLVE(type_slot, s))
  end function

  ! Numeric target for the solve in a given type-slot on surface s.
  function solve_target(s, type_slot) result(tgt)
    use DATLEN, only: SOLVE
    integer, intent(in) :: s, type_slot
    real(real64) :: tgt
    integer :: i, code
    tgt = 0.0_real64
    code = solve_code(s, type_slot)
    i = solve_kind_at(type_slot, code)
    if (i == 0) return
    tgt = SOLVE(SOLVE_KINDS(i)%tgt_slot, s)
  end function

  ! Surface has a thickness solve (either plane)?
  function surf_has_thi_solve(s) result(hasIt)
    integer, intent(in) :: s
    logical :: hasIt
    hasIt = (solve_code(s, SLV_YZ_THI_SLOT) /= 0) .or. &
    &       (solve_code(s, SLV_XZ_THI_SLOT) /= 0)
  end function

  ! Surface has a curvature solve (either plane)?
  function surf_has_curv_solve(s) result(hasIt)
    integer, intent(in) :: s
    logical :: hasIt
    hasIt = (solve_code(s, SLV_YZ_CURV_SLOT) /= 0) .or. &
    &       (solve_code(s, SLV_XZ_CURV_SLOT) /= 0)
  end function

  ! Surface has any solve?
  function surf_has_any_solve(s) result(hasIt)
    integer, intent(in) :: s
    logical :: hasIt
    hasIt = surf_has_thi_solve(s) .or. surf_has_curv_solve(s)
  end function

  ! --- Command builders (the sanctioned new-world way to mutate solves) ------
  ! These emit the SAME KDP command strings the legacy ksolve formatters
  ! (genKDPCMDToSetSolve / genKDPCMDToRemoveSolve) produce, so callers can be
  ! repointed byte-identically.  Resolution still happens in legacy SLVRS after
  ! the command is dispatched.

  ! KDP command to set the solve of kind kindIdx to numeric target.
  ! Form: '<CMD> <target>', e.g. 'PUY 0.1000'.  Aplanatic kinds (nparams==0:
  ! APY/APCY/APX/APCX) take NO explicit value -- CVSOLV rejects them with
  ! "TAKES NO EXPLICIT INPUT" if a numeric word is present (LDM12 ~363) -- so
  ! for those the bare word is emitted, e.g. 'APY'.
  function solve_set_cmd(kindIdx, target) result(cmd)
    use type_utils, only: real2str
    integer, intent(in) :: kindIdx
    real(real64), intent(in) :: target
    character(len=280) :: cmd
    cmd = ''
    if (kindIdx < 1 .or. kindIdx > NUM_SOLVE_KINDS) return
    if (SOLVE_KINDS(kindIdx)%nparams == 0) then
      cmd = trim(SOLVE_KINDS(kindIdx)%kdp_cmd)
    else
      cmd = trim(SOLVE_KINDS(kindIdx)%kdp_cmd)//" "//trim(real2str(target, 4))
    end if
  end function

  ! Delete command word for a given type-slot: TSD (thickness, either plane),
  ! CSDY (YZ curvature), CSDX (XZ curvature).  '' for an unknown slot.
  function solve_del_cmd(type_slot) result(delCmd)
    integer, intent(in) :: type_slot
    character(len=4) :: delCmd
    select case (type_slot)
    case (SLV_YZ_THI_SLOT, SLV_XZ_THI_SLOT)
      delCmd = 'TSD '
    case (SLV_YZ_CURV_SLOT)
      delCmd = 'CSDY'
    case (SLV_XZ_CURV_SLOT)
      delCmd = 'CSDX'
    case default
      delCmd = ''
    end select
  end function

  ! KDP command to remove the solve on surface s for a given type-slot's class.
  ! Form: '<DEL>, <s>,<s>' e.g. 'TSD, 5,5' / 'CSDY, 5,5' (delete is range/class
  ! wide, so it is keyed on the plane/class slot, not an individual kind).
  function solve_remove_cmd(type_slot, s) result(cmd)
    use type_utils, only: int2str
    integer, intent(in) :: type_slot, s
    character(len=280) :: cmd
    character(len=4) :: dc
    cmd = ''
    dc = solve_del_cmd(type_slot)
    if (len_trim(dc) == 0) return
    cmd = trim(dc)//", "//trim(int2str(s))//","//trim(int2str(s))
  end function

  ! --- CODE V <-> KDP solve mapping -----------------------------------------

  ! KDP command word for a CODE V (verb, qualifier) pair, e.g.
  ! ('CUY','UMY') -> 'PUY', ('THI','HCY') -> 'PCY'.  '' if no such solve.
  function solve_kdp_from_codev(codev_cmd, qual) result(kdpCmd)
    character(len=*), intent(in) :: codev_cmd, qual
    character(len=8) :: kdpCmd
    integer :: i
    kdpCmd = ''
    do i = 1, NUM_SOLVE_KINDS
      if (len_trim(SOLVE_KINDS(i)%codev_cmd) == 0) cycle
      if (trim(SOLVE_KINDS(i)%codev_cmd) == trim(codev_cmd) .and. &
      &   trim(SOLVE_KINDS(i)%codev_qual) == trim(qual)) then
        kdpCmd = SOLVE_KINDS(i)%kdp_cmd
        return
      end if
    end do
  end function

  ! CODE V command line that recreates the solve of kind kindIdx sitting on
  ! CODE V surface number surfNum with numeric target -- what the lens-file
  ! writer emits so a solve round-trips.  The target is passed in (not read
  ! from globals) so the caller controls the data source / index convention.
  ! Forms:  'PIM'                     (PY -> paraxial image shortcut)
  !         'RED <val>'               (REDSLV, object-surface magnification)
  !         'THI S<k> HCY <val>'      (thickness solves)
  !         'CUY S<k> UMY <val>'      (curvature solves; aplanatic omits <val>)
  ! Returns '' for kinds with no CODE V spelling (CAY/CAX/COCY/COCX) -- caller
  ! skips them (they persist only through the GUI/KDP path; see KNOWN_ISSUES).
  function solve_save_line(surfNum, kindIdx, target) result(line)
    use type_utils, only: int2str, real2str
    integer, intent(in) :: surfNum, kindIdx
    real(real64), intent(in) :: target
    character(len=280) :: line
    character(len=:), allocatable :: valTxt

    line = ''
    if (kindIdx < 1 .or. kindIdx > NUM_SOLVE_KINDS) return
    if (len_trim(SOLVE_KINDS(kindIdx)%codev_cmd) == 0) return

    valTxt = trim(real2str(target, SOLVE_SAVE_DECIMALS))

    ! PY -> the PIM shortcut (matches existing lens-file convention).
    if (trim(SOLVE_KINDS(kindIdx)%kdp_cmd) == 'PY') then
      line = 'PIM'
      return
    end if
    ! REDSLV -> 'RED <val>' (no surface qualifier; object-surface solve).
    if (trim(SOLVE_KINDS(kindIdx)%codev_cmd) == 'RED') then
      line = 'RED '//valTxt
      return
    end if

    ! General thickness/curvature form: '<VERB> S<k> <QUAL> [<val>]'.
    line = trim(SOLVE_KINDS(kindIdx)%codev_cmd)//' S'//trim(int2str(surfNum))// &
    &      ' '//trim(SOLVE_KINDS(kindIdx)%codev_qual)
    if (SOLVE_KINDS(kindIdx)%nparams > 0) line = trim(line)//' '//valTxt
  end function

end module solve_manager
