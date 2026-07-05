! mod_parsed_command.f90
!
! Typed representation of KDP's parsed-command state -- the ~26 COMMON scalars
! in DATMAI (WC/WQ/WS, W1..W5, S1..S5, DF1..DF5, SQ/SST/SN/STI, SB1-2/SC1-2)
! that the legacy parser (PRO3, UTILITY2.f90) fills and every KDP handler reads.
!
! This module is the first step of retiring communication through those
! globals (refactor #9):
!   - parsed_command holds one complete parse.
!   - capture_command()/apply_command() copy between the record and the
!     DATMAI globals.  The field set mirrors SAVEINPT/RESTINPT (SAVEREST.f90)
!     EXACTLY -- that pair defines what "the parser state" is.  NOTE: the raw
!     INPUT line is deliberately NOT part of the state (SAVEINPT never saved
!     it; every PROCES call rewrites it).
!   - SAVEREST.f90's parallel-array stack is rebuilt on top of this type, so
!     a future parser field can never again be silently dropped from
!     save/restore.
!
! Later phases add kdp_exec (safe re-entrant command execution) and have PRO3
! fill a module-level current record that command_utils and new handlers read
! instead of the COMMONs.
module mod_parsed_command
  use iso_fortran_env, only: real64
  implicit none
  private

  public :: parsed_command, capture_command, apply_command
  public :: kdp_slot, KDP_SLOT_MAX
  public :: kdp_exec, kdp_exec_norestore

  type :: parsed_command
    character(len=8)  :: wc  = ' '      ! command word
    character(len=8)  :: wq  = ' '      ! qualifier word
    character(len=80) :: ws  = ' '      ! string payload (after ':')
    real(real64)      :: w1  = 0.0_real64, w2 = 0.0_real64, w3 = 0.0_real64
    real(real64)      :: w4  = 0.0_real64, w5 = 0.0_real64
    integer           :: s1  = 0, s2 = 0, s3 = 0, s4 = 0, s5 = 0  ! numeric present
    integer           :: df1 = 0, df2 = 0, df3 = 0, df4 = 0, df5 = 0 ! defaulted
    integer           :: sq  = 0        ! qualifier present
    integer           :: sst = 0        ! string present
    integer           :: sn  = 0        ! any numeric present
    integer           :: sti = 0        ! query ('?') flag
    integer           :: sb1 = 0, sb2 = 0  ! blank-status flags
    integer           :: sc1 = 0, sc2 = 0  ! comma-status flags
  end type

  ! Legacy save/restore slots for SAVEINPT/RESTINPT (SAVEREST.f90).  Callers
  ! historically pick their own slot index (1, 31, ...); kept for
  ! compatibility.  New code should use kdp_exec instead of the slot pattern.
  integer, parameter :: KDP_SLOT_MAX = 200
  type(parsed_command), save :: kdp_slot(0:KDP_SLOT_MAX)

  ! True LIFO stack used by kdp_exec -- separate storage from the legacy
  ! slots, so converted and unconverted call sites can nest freely without
  ! slot-index collisions.
  integer, parameter :: EXEC_STACK_MAX = 64
  type(parsed_command), save :: exec_stack(EXEC_STACK_MAX)
  integer,              save :: exec_depth = 0

contains

  ! Snapshot the DATMAI parse globals into a record.
  function capture_command() result(cmd)
    use DATMAI
    type(parsed_command) :: cmd

    cmd%wc  = WC;   cmd%wq  = WQ;   cmd%ws  = WS
    cmd%w1  = W1;   cmd%w2  = W2;   cmd%w3  = W3;  cmd%w4 = W4;  cmd%w5 = W5
    cmd%s1  = S1;   cmd%s2  = S2;   cmd%s3  = S3;  cmd%s4 = S4;  cmd%s5 = S5
    cmd%df1 = DF1;  cmd%df2 = DF2;  cmd%df3 = DF3; cmd%df4 = DF4; cmd%df5 = DF5
    cmd%sq  = SQ;   cmd%sst = SST;  cmd%sn  = SN;  cmd%sti = STI
    cmd%sb1 = SB1;  cmd%sb2 = SB2;  cmd%sc1 = SC1; cmd%sc2 = SC2
  end function

  ! Write a record back into the DATMAI parse globals.
  subroutine apply_command(cmd)
    use DATMAI
    type(parsed_command), intent(in) :: cmd

    WC  = cmd%wc;   WQ  = cmd%wq;   WS  = cmd%ws
    W1  = cmd%w1;   W2  = cmd%w2;   W3  = cmd%w3;  W4 = cmd%w4;  W5 = cmd%w5
    S1  = cmd%s1;   S2  = cmd%s2;   S3  = cmd%s3;  S4 = cmd%s4;  S5 = cmd%s5
    DF1 = cmd%df1;  DF2 = cmd%df2;  DF3 = cmd%df3; DF4 = cmd%df4; DF5 = cmd%df5
    SQ  = cmd%sq;   SST = cmd%sst;  SN  = cmd%sn;  STI = cmd%sti
    SB1 = cmd%sb1;  SB2 = cmd%sb2;  SC1 = cmd%sc1; SC2 = cmd%sc2
  end subroutine

  ! Execute a synthesized KDP command, preserving the caller's parsed-command
  ! state.  Replaces the historical 4-line dance
  !     SAVE_KDP(i)=SAVEINPT(i) / INPUT='...' / CALL PROCES / REST_KDP(i)=RESTINPT(i)
  ! with proper LIFO nesting (no caller-chosen slot indices to collide).
  ! NOTE: like the legacy pattern, the raw INPUT buffer is not restored --
  ! only the parsed state is.
  subroutine kdp_exec(cmdString)
    use DATMAI
    character(len=*), intent(in) :: cmdString

    if (exec_depth >= EXEC_STACK_MAX) then
      ! Should never happen (legacy depth was <= a handful); execute without
      ! save/restore rather than corrupt the stack.
      call kdp_exec_norestore(cmdString)
      return
    end if

    exec_depth = exec_depth + 1
    exec_stack(exec_depth) = capture_command()

    INPUT = cmdString
    CALL PROCES

    call apply_command(exec_stack(exec_depth))
    exec_depth = exec_depth - 1
  end subroutine

  ! Execute a synthesized KDP command WITHOUT restoring the parsed state
  ! afterwards -- for the (deliberate) legacy sites whose whole point is to
  ! leave the new state in place (e.g. mode-entering commands).
  subroutine kdp_exec_norestore(cmdString)
    use DATMAI
    character(len=*), intent(in) :: cmdString

    INPUT = cmdString
    CALL PROCES
  end subroutine

end module mod_parsed_command


! External shim so legacy F77-style code can `CALL KDP_EXEC('...')` without
! adding use statements to hundreds of scattered subroutines -- the same
! implicit-interface convention every legacy file already uses for PROCESKDP.
! (The module procedure and this external have distinct link names; code with
! `use mod_parsed_command` gets the module version, legacy code gets this.)
subroutine KDP_EXEC(cmdString)
  use mod_parsed_command, only: kdp_exec_m => kdp_exec
  implicit none
  character(len=*), intent(in) :: cmdString
  call kdp_exec_m(cmdString)
end subroutine
