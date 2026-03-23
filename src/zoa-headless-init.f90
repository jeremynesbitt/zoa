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

contains

  subroutine zoa_headless_init()
    use GLOBALS
    use global_widgets
    use kdp_data_types
    use zoa_file_handler, only: getZoaPath
    implicit none

    HEADLESS_MODE = .TRUE.

    ! Initialize data structures (mirrors zoamain.F90 lines 52-54)
    curr_lens_data = lens_data()
    sysConfig = sys_config()
    ioConfig = io_config()

    ! Set base path for data files (glass catalogs, etc.)
    basePath = getZoaPath()

    ! Allocate command history (mirrors zoamain.F90 line 60)
    if (.not. associated(uiSettingCommands)) then
      allocate(uiSettingCommands(500))
    end if

    ! Initialize the optical engine (COMMON blocks, glass catalogs, defaults, commands)
    ! Output during init goes to stdout via zoa_output default handler
    call INITKDP

    ! Sync modern data structures with legacy COMMON block state
    call refreshLensDataStruct()

  end subroutine

end module zoa_headless
