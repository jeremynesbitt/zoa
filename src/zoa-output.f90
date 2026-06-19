! zoa-output.f90
! Output abstraction layer for Zoa
!
! This module decouples all program output from the GTK GUI.
! By default, output goes to stdout. The GUI registers its own
! handler (updateTerminalLog) at startup via zoa_set_output_handler.
! Tests and headless mode use alternative handlers (capture, file, etc.)

module zoa_output
  implicit none
  private
  public :: zoa_emit, zoa_set_output_handler, zoa_output_to_stdout
  public :: output_handler_iface
  public :: zoa_set_replot_flush_hook, zoa_invoke_replot_flush
  public :: zoa_suppress_output

  abstract interface
    subroutine output_handler_iface(text, color)
      character(len=*), intent(in) :: text
      character(len=*), intent(in) :: color
    end subroutine

    subroutine noop_iface()
    end subroutine
  end interface

  procedure(output_handler_iface), pointer :: output_handler => zoa_output_to_stdout

  ! When true, zoa_emit silently drops all output. Used to mute internal
  ! housekeeping traces (e.g. reference-ray retracing) that must not pollute
  ! captured output.
  logical :: suppressed = .false.

  ! A low-level hook so that zoa_file_handler (which cannot use zoa_ui_callbacks
  ! without a module cycle) can still trigger the replot-flush after each line.
  ! Registered by zoa_headless_init; null by default (no-op).
  procedure(noop_iface), pointer :: replot_flush_hook => null()

contains

  subroutine zoa_emit(text, color)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: color
    if (suppressed) return
    call output_handler(text, color)
  end subroutine

  ! Toggle global output suppression. Returns the previous state so callers can
  ! restore it (supports safe nesting).
  function zoa_suppress_output(on) result(prev)
    logical, intent(in) :: on
    logical :: prev
    prev = suppressed
    suppressed = on
  end function

  subroutine zoa_set_output_handler(handler)
    procedure(output_handler_iface) :: handler
    output_handler => handler
  end subroutine

  subroutine zoa_output_to_stdout(text, color)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: color
    write(*, '(A)') trim(text)
  end subroutine

  subroutine zoa_set_replot_flush_hook(hook)
    procedure(noop_iface) :: hook
    replot_flush_hook => hook
  end subroutine

  subroutine zoa_invoke_replot_flush()
    if (associated(replot_flush_hook)) call replot_flush_hook()
  end subroutine

end module zoa_output
