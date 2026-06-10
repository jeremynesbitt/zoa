! undo-manager.f90
!
! Per-session undo/redo for the lens system, implemented as a bounded ring of
! full-lens snapshots written as .zoa files in the temp directory.
!
! A snapshot captures the same content as the SAV command (system config + lens
! + merit function) via sysConfig/ldm/optim genSaveOutputText -- it does NOT
! include plot-tab commands, so restoring never spawns plot tabs; open plots are
! refreshed by the caller's normal replot drain after a restore.
!
! Snapshots are tracked by a monotonically increasing sequence number; the file
! slot for a sequence is mod(seq-1,UNDO_MAX)+1 -> files undo001.zoa..undo100.zoa.
module undo_manager
  use zoa_file_handler, only: getTempDirectory, process_zoa_file
  use global_widgets,   only: sysConfig
  use mod_lens_data_manager, only: ldm
  use optim_types,      only: optim
  use zoom_manager,     only: zoom_genSaveOutputText, zoom_reset
  use zoa_output,       only: zoa_emit
  implicit none
  private
  public :: undo_snapshot, undo_undo, undo_redo, undo_reset_baseline

  integer, parameter :: UNDO_MAX = 100   ! ring size (number of temp snapshot files)

  integer :: head_seq    = 0   ! seq of the newest snapshot (0 = none yet)
  integer :: current_seq = 0   ! seq currently shown
  integer :: oldest_seq  = 0   ! seq of the oldest still-available snapshot
  logical :: in_restore  = .false.            ! true while restoring (suppresses snapshots)
  logical :: suppress_next_snapshot = .false. ! skip exactly one upcoming snapshot

contains

  ! Map a sequence number to its ring file name "undoNNN.zoa".
  function slot_name(seq) result(fname)
    integer, intent(in) :: seq
    character(len=16) :: fname
    integer :: slot
    slot = mod(seq - 1, UNDO_MAX) + 1
    write(fname, '(A,I3.3,A)') 'undo', slot, '.zoa'
  end function slot_name

  ! Write the full current lens to the ring file for `seq`. Mirrors execSAV;
  ! intentionally omits plot-tab commands.
  subroutine write_snapshot(seq)
    integer, intent(in) :: seq
    integer :: fID, ios
    ! Open directly (status='replace') rather than via open_file_to_sav_lens so
    ! snapshots are silent -- that helper emits "FUll Path is ..." to the terminal.
    open(newunit=fID, file=trim(getTempDirectory())//trim(slot_name(seq)), &
         status='replace', action='write', iostat=ios)
    if (ios == 0) then
      call sysConfig%genSaveOutputText(fID)
      call ldm%genSaveOutputText(fID)
      call optim%genSaveOutputText(fID)
      call zoom_genSaveOutputText(fID)
      close(fID)
    end if
  end subroutine write_snapshot

  ! Restore the lens from the ring file for `seq`. `in_restore` blocks any
  ! snapshot attempt triggered while the restore re-runs commands.
  subroutine restore_snapshot(seq)
    integer, intent(in) :: seq
    character(len=1040) :: path
    in_restore = .true.
    ! Clear zoom first; the snapshot's ZOO/POS lines rebuild the exact config
    ! state, so stale operands from the current state don't linger.
    call zoom_reset()
    path = trim(getTempDirectory())//trim(slot_name(seq))
    call process_zoa_file(trim(path))
    call ldm%load_surfaces_from_alens()
    in_restore = .false.
  end subroutine restore_snapshot

  ! Append a snapshot of the current lens as a new history entry. No-op while
  ! restoring, or when a single snapshot has been suppressed (undo/redo/reset).
  subroutine undo_snapshot()
    if (in_restore) return
    if (suppress_next_snapshot) then
      suppress_next_snapshot = .false.
      return
    end if
    ! Advancing from `current` discards any redo-future beyond it.
    current_seq = current_seq + 1
    head_seq    = current_seq
    if (oldest_seq < 1) oldest_seq = 1
    if (head_seq - oldest_seq + 1 > UNDO_MAX) oldest_seq = head_seq - UNDO_MAX + 1
    call write_snapshot(head_seq)
  end subroutine undo_snapshot

  ! Clear the history when a different lens is loaded/created. The loaded lens
  ! is captured as the baseline by the next top-level snapshot (at the replot
  ! drain, after the load command fully completes) -- NOT here: writing a
  ! snapshot mid-load re-runs genSaveOutputText/load_surfaces, which can perturb
  ! solve resolution (e.g. PIM) and corrupt reload determinism.
  subroutine undo_reset_baseline()
    in_restore  = .false.
    suppress_next_snapshot = .false.
    head_seq    = 0
    current_seq = 0
    oldest_seq  = 0
  end subroutine undo_reset_baseline

  subroutine undo_undo()
    if (head_seq == 0 .or. current_seq <= oldest_seq) then
      call zoa_emit("Nothing to undo", "black")
      return
    end if
    current_seq = current_seq - 1
    call restore_snapshot(current_seq)
    ! The restore re-ran commands -> EOS set replot_deferred; the next top-level
    ! drain must refresh plots but NOT record this restore as a new history entry.
    suppress_next_snapshot = .true.
    call zoa_emit("Undo", "black")
  end subroutine undo_undo

  subroutine undo_redo()
    if (current_seq >= head_seq) then
      call zoa_emit("Nothing to redo", "black")
      return
    end if
    current_seq = current_seq + 1
    call restore_snapshot(current_seq)
    suppress_next_snapshot = .true.
    call zoa_emit("Redo", "black")
  end subroutine undo_redo

end module undo_manager
