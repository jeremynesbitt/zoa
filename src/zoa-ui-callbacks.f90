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
  use plot_setting_manager, only: zoaplot_setting_manager
  implicit none
  private
  public :: notify_replot, notify_refresh_status, notify_close_all_tabs
  public :: zoa_set_replot_callback, zoa_set_refresh_status_callback, &
            zoa_set_close_all_tabs_callback
  ! notify_show_optimizer_ui omitted: optimizer_ui uses handlers, creating a cycle
  ! notify_show_editor_ui omitted: lens_editor uses handlers, creating a cycle
  ! notify_show_sysconfig_ui omitted: ui_sys_config uses handlers, creating a cycle
  public :: notify_show_macro_ui, zoa_set_show_macro_ui_callback
  public :: query_confirm, zoa_set_query_confirm_callback
  public :: query_yes_no, zoa_set_query_yes_no_callback
  public :: notify_write_tab_state, zoa_set_write_tab_state_callback
  public :: query_existing_plot, zoa_set_query_existing_plot_callback
  public :: query_save_file, zoa_set_query_save_file_callback

  ! Abstract interfaces so procedure pointers have explicit types
  abstract interface
    subroutine replot_iface()
    end subroutine

    subroutine refresh_status_iface()
    end subroutine

    subroutine close_tabs_iface(msg)
      character(len=*), intent(in) :: msg
    end subroutine

    subroutine show_optimizer_ui_iface()
    end subroutine

    subroutine show_macro_ui_iface()
    end subroutine

    subroutine query_confirm_iface(message, title, confirmed)
      character(len=*), intent(in) :: message, title
      logical, intent(out) :: confirmed
    end subroutine

    subroutine query_yes_no_iface(message, title, yes)
      character(len=*), intent(in) :: message, title
      logical, intent(out) :: yes
    end subroutine

    subroutine write_tab_state_iface(fID)
      integer, intent(in) :: fID
    end subroutine

    subroutine query_existing_plot_iface(plot_code, plot_num, psm, found)
      import zoaplot_setting_manager
      integer, intent(in) :: plot_code, plot_num
      type(zoaplot_setting_manager), intent(out) :: psm
      logical, intent(out) :: found
    end subroutine

    subroutine query_save_file_iface(filename, dir, default_dir, filter, title, selected)
      character(len=*), intent(inout) :: filename
      character(len=*), intent(out)   :: dir
      character(len=*), intent(in)    :: default_dir, filter, title
      logical, intent(out) :: selected
    end subroutine
  end interface

  procedure(replot_iface),              pointer :: replot_cb             => null()
  procedure(refresh_status_iface),      pointer :: refresh_status_cb     => null()
  procedure(close_tabs_iface),          pointer :: close_tabs_cb         => null()
  procedure(show_optimizer_ui_iface),   pointer :: show_optimizer_ui_cb  => null()
  procedure(show_macro_ui_iface),       pointer :: show_macro_ui_cb      => null()
  procedure(query_confirm_iface),       pointer :: query_confirm_cb      => null()
  procedure(query_yes_no_iface),        pointer :: query_yes_no_cb       => null()
  procedure(write_tab_state_iface),     pointer :: write_tab_state_cb    => null()
  procedure(query_existing_plot_iface), pointer :: query_existing_plot_cb => null()
  procedure(query_save_file_iface),     pointer :: query_save_file_cb    => null()

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

  subroutine zoa_set_show_optimizer_ui_callback(cb)
    procedure(show_optimizer_ui_iface) :: cb
    show_optimizer_ui_cb => cb
  end subroutine

  subroutine zoa_set_show_macro_ui_callback(cb)
    procedure(show_macro_ui_iface) :: cb
    show_macro_ui_cb => cb
  end subroutine

  subroutine zoa_set_query_confirm_callback(cb)
    procedure(query_confirm_iface) :: cb
    query_confirm_cb => cb
  end subroutine

  subroutine zoa_set_query_yes_no_callback(cb)
    procedure(query_yes_no_iface) :: cb
    query_yes_no_cb => cb
  end subroutine

  subroutine zoa_set_write_tab_state_callback(cb)
    procedure(write_tab_state_iface) :: cb
    write_tab_state_cb => cb
  end subroutine

  subroutine zoa_set_query_existing_plot_callback(cb)
    procedure(query_existing_plot_iface) :: cb
    query_existing_plot_cb => cb
  end subroutine

  subroutine zoa_set_query_save_file_callback(cb)
    procedure(query_save_file_iface) :: cb
    query_save_file_cb => cb
  end subroutine

  ! --- notify/query routines called by core code ---

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

  subroutine notify_show_optimizer_ui()
    if (associated(show_optimizer_ui_cb)) call show_optimizer_ui_cb()
  end subroutine

  subroutine notify_show_macro_ui()
    if (associated(show_macro_ui_cb)) call show_macro_ui_cb()
  end subroutine

  ! query_confirm: default headless behaviour is confirmed=.TRUE. (proceed without prompting)
  subroutine query_confirm(message, title, confirmed)
    character(len=*), intent(in) :: message, title
    logical, intent(out) :: confirmed
    if (associated(query_confirm_cb)) then
      call query_confirm_cb(message, title, confirmed)
    else
      confirmed = .TRUE.
    end if
  end subroutine

  ! query_yes_no: default headless behaviour is yes=.TRUE. (proceed without prompting)
  subroutine query_yes_no(message, title, yes)
    character(len=*), intent(in) :: message, title
    logical, intent(out) :: yes
    if (associated(query_yes_no_cb)) then
      call query_yes_no_cb(message, title, yes)
    else
      yes = .TRUE.
    end if
  end subroutine

  subroutine notify_write_tab_state(fID)
    integer, intent(in) :: fID
    if (associated(write_tab_state_cb)) call write_tab_state_cb(fID)
  end subroutine

  ! query_existing_plot: default headless behaviour is found=.FALSE. (no GUI tabs exist)
  subroutine query_existing_plot(plot_code, plot_num, psm, found)
    integer, intent(in) :: plot_code, plot_num
    type(zoaplot_setting_manager), intent(out) :: psm
    logical, intent(out) :: found
    if (associated(query_existing_plot_cb)) then
      call query_existing_plot_cb(plot_code, plot_num, psm, found)
    else
      found = .FALSE.
    end if
  end subroutine

  ! query_save_file: default headless behaviour is selected=.FALSE. (no file picker)
  subroutine query_save_file(filename, dir, default_dir, filter, title, selected)
    character(len=*), intent(inout) :: filename
    character(len=*), intent(out)   :: dir
    character(len=*), intent(in)    :: default_dir, filter, title
    logical, intent(out) :: selected
    if (associated(query_save_file_cb)) then
      call query_save_file_cb(filename, dir, default_dir, filter, title, selected)
    else
      selected = .FALSE.
    end if
  end subroutine

end module zoa_ui_callbacks
