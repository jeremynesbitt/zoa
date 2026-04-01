! zoa-ui-callbacks.f90
!
! Lightweight callback hooks that let core computation code notify the GUI
! without depending on GTK at compile time.
!
! Pattern mirrors zoa-output.f90: the GUI registers real implementations at
! startup (in zzhandlers.f90 activate()); in headless mode the defaults are
! no-ops and nothing breaks.
!
! Usage in core code:
!   use zoa_ui_callbacks, only: notify_replot, notify_refresh_status, &
!                                notify_close_all_tabs
!   call notify_replot()
!   call notify_close_all_tabs("Opening new lens — all plots will be closed.")

module zoa_ui_callbacks
  implicit none
  private
  public :: notify_replot, notify_refresh_status, notify_close_all_tabs
  public :: zoa_set_replot_callback, zoa_set_refresh_status_callback, &
            zoa_set_close_all_tabs_callback

  ! Abstract interfaces so procedure pointers have explicit types
  abstract interface
    subroutine replot_iface()
    end subroutine

    subroutine refresh_status_iface()
    end subroutine

    subroutine close_tabs_iface(msg)
      character(len=*), intent(in) :: msg
    end subroutine
  end interface

  procedure(replot_iface),        pointer :: replot_cb        => null()
  procedure(refresh_status_iface),pointer :: refresh_status_cb => null()
  procedure(close_tabs_iface),    pointer :: close_tabs_cb    => null()

contains

  subroutine zoa_set_replot_callback(cb)
    procedure(replot_iface) :: cb
    replot_cb => cb
  end subroutine

  subroutine zoa_set_refresh_status_callback(cb)
    procedure(refresh_status_iface) :: cb
    refresh_status_cb => cb
  end subroutine

  subroutine zoa_set_close_all_tabs_callback(cb)
    procedure(close_tabs_iface) :: cb
    close_tabs_cb => cb
  end subroutine

  subroutine notify_replot()
    if (associated(replot_cb)) call replot_cb()
  end subroutine

  subroutine notify_refresh_status()
    if (associated(refresh_status_cb)) call refresh_status_cb()
  end subroutine

  subroutine notify_close_all_tabs(msg)
    character(len=*), intent(in) :: msg
    if (associated(close_tabs_cb)) call close_tabs_cb(msg)
  end subroutine

end module zoa_ui_callbacks
