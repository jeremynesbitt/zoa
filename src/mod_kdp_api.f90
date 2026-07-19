! mod_kdp_api.f90
!
! Typed, text-free entry points into legacy KDP functionality.
!
! The historical pattern for driving KDP from new code is to FORMAT a command
! string and re-parse it:  call PROCESKDP('CHG 3; TH '//real2str(x)).  That
! round-trip costs a tokenizer pass, is capped by the 140-char INPUT buffer,
! and quantizes every real at whatever the text format kept (the optimizer's
! frozen-lens bug was partly real2str's default F9.5).  This module drives the
! same legacy handlers directly: build the parsed-command state (the 26 DATMAI
! scalars) from typed arguments, call the handler, restore the caller's state.
!
! Dependency rule: this module may use ONLY DATMAI + mod_parsed_command.
! Anything that needs the typed-surface store (mod_lens_data_manager) or the
! UI-facing lens copies (global_widgets) would create a module cycle once
! kdp_data_types itself calls into this API -- those actions are reached
! through the procedure-pointer hooks below, registered at startup by
! codeV_commands%initializeCmds (which sits above both).
!
! Lens-edit transaction (mirrors 'U L; ...; EOS'):
!     call kdp_lens_begin()                    ! ULENNS ('U L')
!     call kdp_chg(3)                          ! LENUP 'CHG 3'
!     call kdp_lens_cmd('TH', w1=2.5d0)        ! LENUP 'TH 2.5' -- exact real64
!     call kdp_lens_end(refreshSurf=3)         ! store resync + LENUP 'EOS'
!
! kdp_lens_end reproduces executeCodeVLensUpdateCommand's proven ordering:
! typed-store resync BEFORE the finalizing EOS (so PIM/PY solves resolve
! against new geometry -- the stale-typed-store bug family), then CONTRO's
! post-command refresh pair (curr_lens_data%update / sysConfig%
! updateParameters) via the post-EOS hook.  NOTE: CONTRO's GUI lens-editor
! rebuild on EOS is deliberately NOT replicated here.
module mod_kdp_api
  use iso_fortran_env, only: real64
  use mod_parsed_command, only: parsed_command, capture_command, apply_command
  implicit none
  private

  public :: kdp_lens_begin, kdp_lens_cmd, kdp_chg, kdp_lens_end
  public :: kdp_silent_begin, kdp_silent_end
  public :: kdp_api_set_hooks

  abstract interface
    subroutine surf_hook_ifc(surf)
      integer, intent(in) :: surf
    end subroutine
    subroutine plain_hook_ifc()
    end subroutine
  end interface

  ! Registered by codeV_commands%initializeCmds (see dependency rule above).
  procedure(surf_hook_ifc),  pointer :: refresh_surf_hook => null()
  procedure(plain_hook_ifc), pointer :: refresh_all_hook  => null()
  procedure(plain_hook_ifc), pointer :: post_eos_hook     => null()
  procedure(plain_hook_ifc), pointer :: silence_on_hook   => null()
  procedure(plain_hook_ifc), pointer :: silence_off_hook  => null()

contains

  subroutine kdp_api_set_hooks(refresh_surf, refresh_all, post_eos, &
                               silence_on, silence_off)
    procedure(surf_hook_ifc)  :: refresh_surf
    procedure(plain_hook_ifc) :: refresh_all
    procedure(plain_hook_ifc) :: post_eos
    procedure(plain_hook_ifc) :: silence_on, silence_off
    refresh_surf_hook => refresh_surf
    refresh_all_hook  => refresh_all
    post_eos_hook     => post_eos
    silence_on_hook   => silence_on
    silence_off_hook  => silence_off
  end subroutine

  ! Output suppression around direct handler calls (the PROCESSILENT
  ! equivalent): redirects terminal output to the hidden KDP dump view.
  ! Single-slot set/restore semantics -- do not nest, and do not redirect
  ! again inside the bracket (same rule as ioConfig set/restoreTextView).
  subroutine kdp_silent_begin()
    if (associated(silence_on_hook)) call silence_on_hook()
  end subroutine

  subroutine kdp_silent_end()
    if (associated(silence_off_hook)) call silence_off_hook()
  end subroutine

  ! Build the parse state PRO3 would have produced for "WORD [wq] [w1 w2 w3]".
  ! Present numerics get S*=1 (and SN=1); absent ones get DF*=1 -- exactly the
  ! flag convention every LENUP/CMDER handler tests.
  function build_cmd(word, wq, w1, w2, w3) result(cmd)
    character(len=*), intent(in)           :: word
    character(len=*), intent(in), optional :: wq
    real(real64),     intent(in), optional :: w1, w2, w3
    type(parsed_command) :: cmd

    cmd = parsed_command()      ! all fields blank/zero
    cmd%wc = word
    if (present(wq)) then
      cmd%wq = wq
      cmd%sq = 1
    end if
    if (present(w1)) then
      cmd%w1 = w1;  cmd%s1 = 1;  cmd%sn = 1
    else
      cmd%df1 = 1
    end if
    if (present(w2)) then
      cmd%w2 = w2;  cmd%s2 = 1;  cmd%sn = 1
    else
      cmd%df2 = 1
    end if
    if (present(w3)) then
      cmd%w3 = w3;  cmd%s3 = 1;  cmd%sn = 1
    else
      cmd%df3 = 1
    end if
    cmd%df4 = 1
    cmd%df5 = 1
  end function

  ! Enter lens-update mode ('U L') if not already in a lens input/update level.
  subroutine kdp_lens_begin()
    use DATMAI, only: F1, F5, F6
    type(parsed_command) :: saved

    if (F1.EQ.0 .AND. (F5.EQ.1 .OR. F6.EQ.1)) return   ! already in a lens level
    saved = capture_command()
    call apply_command(build_cmd('U', wq='L'))
    CALL ULENNS
    call apply_command(saved)
  end subroutine

  ! Execute one lens-update-level command (any word LENUP dispatches: TH, CV,
  ! RD, CC, AD..AL, PIKUP, solves, INSK/DELK, ...).  Typed reals go straight
  ! into W1..W3 -- no text formatting, no precision loss.
  subroutine kdp_lens_cmd(word, w1, w2, w3, wq)
    character(len=*), intent(in)           :: word
    real(real64),     intent(in), optional :: w1, w2, w3
    character(len=*), intent(in), optional :: wq
    type(parsed_command) :: saved

    saved = capture_command()
    call apply_command(build_cmd(word, wq=wq, w1=w1, w2=w2, w3=w3))
    CALL LENUP
    call apply_command(saved)
  end subroutine

  subroutine kdp_chg(surf)
    integer, intent(in) :: surf
    call kdp_lens_cmd('CHG', w1=real(surf, real64))
  end subroutine

  ! Commit the transaction ('EOS' -> LNSEOS).  refreshSurf/refreshAll request
  ! the typed-store resync BEFORE the EOS traces (same contract and reasoning
  ! as executeCodeVLensUpdateCommand's arguments of the same names); the
  ! post-EOS hook then runs CONTRO's usual curr_lens_data/sysConfig refresh.
  subroutine kdp_lens_end(refreshSurf, refreshAll)
    integer, intent(in), optional :: refreshSurf
    logical, intent(in), optional :: refreshAll
    type(parsed_command) :: saved

    if (present(refreshAll)) then
      if (refreshAll .and. associated(refresh_all_hook)) call refresh_all_hook()
    else if (present(refreshSurf)) then
      if (associated(refresh_surf_hook)) call refresh_surf_hook(refreshSurf)
    end if

    saved = capture_command()
    call apply_command(build_cmd('EOS'))
    CALL LENUP
    call apply_command(saved)

    if (associated(post_eos_hook)) call post_eos_hook()
  end subroutine

end module mod_kdp_api
