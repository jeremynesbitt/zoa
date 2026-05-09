! ZOA MAIN
! This file contains the main program and the GUI modules


program zoa_program
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr, c_int
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE, gtk_application_set_accels_for_action, &
  & gtk_icon_theme_get_for_display
  use gdk, only:  gdk_display_get_default
  use g, only: g_application_run, g_object_unref
  use zoa_file_handler, only: getZoaPath, loadPreferences
  use handlers
  use zoa_ui
  use global_widgets
  use GLOBALS, only: zoaVersion, RELEASE_MODE
  implicit none

#ifdef WINDOWS
  interface
    function GetConsoleWindow() bind(c, name='GetConsoleWindow') result(hwnd)
      import :: c_ptr
      type(c_ptr) :: hwnd
    end function GetConsoleWindow

    function ShowWindow(hwnd, nCmdShow) bind(c, name='ShowWindow') result(status)
      import :: c_ptr, c_int
      type(c_ptr), value :: hwnd
      integer(c_int), value :: nCmdShow
      integer(c_int) :: status
    end function ShowWindow
  end interface

  integer(c_int), parameter :: SW_HIDE = 0_c_int
#endif

  integer(c_int)     :: status
  
!For windows only we need to hide the console window.
!This solution is inspired by:
!https://stackoverflow.com/questions/29763647/how-to-make-a-program-that-does-not-display-the-console-window/29764309#29764309
#ifdef WINDOWS
  type(c_ptr) :: console
  integer(c_int) :: closeWin
#ifdef __RELEASE
          PRINT *, "Hide console window"
          console = GetConsoleWindow()
          closeWin = ShowWindow(console, SW_HIDE)
#endif
#endif
#ifdef __VERSION
  PRINT *, "Found Version!"
#endif
PRINT *, "Version is ", __VERSION
zoaVersion = __VERSION
#ifdef __RELEASE
  RELEASE_MODE = .TRUE.
#endif



  app = gtk_application_new("zoa.optical-analysis"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)
                            
  
  ! UI Settings Initialization


  curr_lens_data = lens_data()
  sysConfig = sys_config()
  ioConfig = io_config()

  ! This also doubles as storing OS
  basePath = getZoaPath()
  call loadPreferences()

  !For saving command history.
  allocate(uiSettingCommands(cmdHistorySize))



  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)

                        ! Set up resources such as icons


  status = g_application_run(app, 0_c_int, c_null_ptr)

  print *, "You have exited the program.  Cleaning up"

  call g_object_unref(app)


end program zoa_program
