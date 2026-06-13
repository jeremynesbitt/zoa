! zoa-headless-init.f90
! Headless initialization for Zoa
!
! Initializes the optical engine without GTK.
! Used by the test runner and ZeroMQ server.
! Output goes through zoa_emit (defaults to stdout unless overridden).

module zoa_headless
  implicit none
  private
  public :: zoa_headless_init

  ! Tracks whether a lens-changing command has been issued since the last flush.
  ! Mirrors the GUI's replot_deferred flag, but for headless mode.
  logical :: lens_dirty = .false.

contains

  ! Called by notify_replot() when a lens-modifying command completes.
  ! Sets the dirty flag so that the next flush call will take a snapshot.
  subroutine headless_notify_replot()
    lens_dirty = .true.
  end subroutine

  ! Called after each processed line (depth==1) to drain pending snapshots.
  ! Mirrors gui_replot_flush in zzhandlers.f90: if dirty, take an undo snapshot.
  subroutine headless_replot_flush()
    use undo_manager, only: undo_snapshot
    if (lens_dirty) then
      lens_dirty = .false.
      call undo_snapshot()
    end if
  end subroutine

  subroutine zoa_headless_init()
    use GLOBALS
    use global_widgets
    use kdp_data_types
    use zoa_file_handler, only: getZoaPath, loadPreferences, applyGlassCatalogDirFromPrefs
    use zoa_ui_callbacks, only: zoa_set_replot_callback, zoa_set_replot_flush_callback
    use zoa_output, only: zoa_set_replot_flush_hook
    implicit none

    HEADLESS_MODE = .TRUE.

    ! Initialize data structures (mirrors zoamain.F90 lines 52-54)
    curr_lens_data = lens_data()
    sysConfig = sys_config()
    ioConfig = io_config()

    ! Set base path for data files (glass catalogs, etc.)
    basePath = getZoaPath()
    call loadPreferences()

    ! Allocate command history (mirrors zoamain.F90 line 60)
    if (.not. associated(uiSettingCommands)) then
      allocate(uiSettingCommands(500))
    end if

    ! Initialize the optical engine (COMMON blocks, glass catalogs, defaults, commands)
    ! Output during init goes to stdout via zoa_output default handler
    call INITKDP

    ! Override LIBGLA if user has a custom glass catalog directory in preferences
    call applyGlassCatalogDirFromPrefs()

    ! Sync modern data structures with legacy COMMON block state
    call refreshLensDataStruct()

    ! Register headless replot/flush callbacks so that lens-modifying commands
    ! trigger undo snapshots (mirrors GUI's gui_replot / gui_replot_flush in zzhandlers.f90).
    call zoa_set_replot_callback(headless_notify_replot)
    call zoa_set_replot_flush_callback(headless_replot_flush)
    ! Also register the low-level hook used by zoa_file_handler's process_zoa_file
    ! (which cannot use zoa_ui_callbacks directly due to a module cycle).
    call zoa_set_replot_flush_hook(headless_replot_flush)

  end subroutine

end module zoa_headless
