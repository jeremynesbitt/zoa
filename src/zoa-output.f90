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

  abstract interface
    subroutine output_handler_iface(text, color)
      character(len=*), intent(in) :: text
      character(len=*), intent(in) :: color
    end subroutine
  end interface

  procedure(output_handler_iface), pointer :: output_handler => zoa_output_to_stdout

contains

  subroutine zoa_emit(text, color)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: color
    call output_handler(text, color)
  end subroutine

  subroutine zoa_set_output_handler(handler)
    procedure(output_handler_iface) :: handler
    output_handler => handler
  end subroutine

  subroutine zoa_output_to_stdout(text, color)
    character(len=*), intent(in) :: text
    character(len=*), intent(in) :: color
    write(*, '(A)') trim(text)
  end subroutine

end module zoa_output
