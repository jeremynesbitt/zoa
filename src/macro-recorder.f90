! Macro recording (RECMACRO): while recording, every command the user issues
! is appended to a temporary file; RECMACRO 0 saves it as a macro (.zoa
! text, one command per line).
!
! What counts as "a command the user issues": a top-level call of PROCESKDP
! (nesting depth 1) -- the console, menu actions, lens-editor and other UI
! edits all arrive that way.  Calls PROCESKDP makes while a command runs
! (depth > 1: a macro's own lines, RES's lens file, internal sub-commands)
! are not recorded, so running a macro records just the command that ran it.
! Zoa's own top-level work -- redrawing plots after a change, refreshing the
! reference rays, restoring plots from a .zin -- runs inside
! rec_suppress_begin/end and is not recorded either.
!
! Leaf module (no Zoa dependencies) so PROCESKDP and the GUI handlers can use
! it without a module cycle.
module macro_recorder
  implicit none
  private
  public :: rec_enter, rec_leave, rec_suppress_begin, rec_suppress_end
  public :: rec_start, rec_stop, rec_is_recording, rec_count

  logical :: recording = .false.
  integer :: recUnit = -1
  integer :: depth = 0          ! PROCESKDP nesting
  integer :: suppress = 0       ! internal top-level work (nesting count)
  integer :: nRecorded = 0
  character(len=2048) :: tmpPath = ''

contains

  ! PROCESKDP entry: record the command if it is the user's (see above).
  subroutine rec_enter(cmd)
    character(len=*), intent(in) :: cmd
    depth = depth + 1
    if (.not. recording .or. depth /= 1 .or. suppress > 0) return
    if (len_trim(cmd) == 0) return
    if (isRecMacro(cmd)) return            ! the recorder's own commands
    write(recUnit, '(A)') trim(adjustl(cmd))
    flush(recUnit)
    nRecorded = nRecorded + 1
  end subroutine

  subroutine rec_leave()
    if (depth > 0) depth = depth - 1
  end subroutine

  subroutine rec_suppress_begin()
    suppress = suppress + 1
  end subroutine

  subroutine rec_suppress_end()
    if (suppress > 0) suppress = suppress - 1
  end subroutine

  logical function rec_is_recording()
    rec_is_recording = recording
  end function

  integer function rec_count()
    rec_count = nRecorded
  end function

  ! Start recording into the temporary file path (overwritten).
  subroutine rec_start(path, ok)
    character(len=*), intent(in) :: path
    logical, intent(out) :: ok
    integer :: ios
    ok = .false.
    if (recording) return
    open(newunit=recUnit, file=trim(path), status='replace', action='write', &
         form='formatted', iostat=ios)
    if (ios /= 0) return
    tmpPath = path
    nRecorded = 0
    recording = .true.
    ok = .true.
  end subroutine

  ! Stop recording and write the macro to dest: a header comment, then the
  ! recorded commands.  An empty dest stops without saving (the temporary
  ! file is kept).
  subroutine rec_stop(dest, header, ok)
    character(len=*), intent(in) :: dest, header
    logical, intent(out) :: ok
    integer :: inU, outU, ios
    character(len=4096) :: line

    ok = .false.
    if (.not. recording) return
    close(recUnit)
    recording = .false.
    if (len_trim(dest) == 0) return

    open(newunit=inU, file=trim(tmpPath), status='old', action='read', iostat=ios)
    if (ios /= 0) return
    open(newunit=outU, file=trim(dest), status='replace', action='write', iostat=ios)
    if (ios /= 0) then
      close(inU)
      return
    end if
    if (len_trim(header) > 0) write(outU, '(A)') trim(header)
    do
      read(inU, '(A)', iostat=ios) line
      if (ios /= 0) exit
      write(outU, '(A)') trim(line)
    end do
    close(inU)
    close(outU)
    ok = .true.
  end subroutine

  logical function isRecMacro(cmd)
    character(len=*), intent(in) :: cmd
    character(len=8) :: w
    integer :: i
    w = adjustl(cmd)
    do i = 1, len(w)
      if (w(i:i) >= 'a' .and. w(i:i) <= 'z') w(i:i) = achar(iachar(w(i:i)) - 32)
    end do
    isRecMacro = (w == 'RECMACRO')
  end function

end module macro_recorder
