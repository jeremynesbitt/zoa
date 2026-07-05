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
  ! compatibility.  New code should use kdp_exec (added in a later phase)
  ! instead of the slot pattern.
  integer, parameter :: KDP_SLOT_MAX = 200
  type(parsed_command), save :: kdp_slot(0:KDP_SLOT_MAX)

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

end module mod_parsed_command
