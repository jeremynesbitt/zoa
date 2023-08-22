! ZOA MAIN
! This file contains the main program and the GUI modules


program zoa_program
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char, c_null_ptr
  use gtk, only: gtk_application_new, G_APPLICATION_FLAGS_NONE
  use g, only: g_application_run, g_object_unref
  use handlers
  use zoa_ui
  use global_widgets
#ifdef WINDOWS    
    use kernel32
    use user32
    use ifwinty
#endif    
  implicit none

  integer(c_int)     :: status
!For windows only we need to hide the console window.
!This solution is inspired by:
!https://stackoverflow.com/questions/29763647/how-to-make-a-program-that-does-not-display-the-console-window/29764309#29764309  
#ifdef WINDOWS
  integer(handle) :: console
  integer :: closeWin
#ifdef __RELEASE
          PRINT *, "Hide console window"
          console = GetConsoleWindow()         
          closeWin = ShowWindow(console, SW_HIDE)
#endif
#endif
 

  ! First, let's create a GTK application (it will initialize GTK).
  ! The application ID must contain at least one point:
  ! https://developer.gnome.org/gio/stable/GApplication.html#g-application-id-is-valid
  app = gtk_application_new("zoa.optical-analysis"//c_null_char, &
                            & G_APPLICATION_FLAGS_NONE)

  ! UI Settings Initialization

  curr_lens_data = lens_data()
  sysConfig = sys_config()
  ioConfig = io_config()

  !For saving command history.  
  allocate(uiSettingCommands(cmdHistorySize))


  call g_signal_connect(app, "activate"//c_null_char, c_funloc(activate), &
                      & c_null_ptr)


  status = g_application_run(app, 0_c_int, c_null_ptr)

  print *, "You have exited the program.  Cleaning up"

  ! Memory is freed:
  call g_object_unref(app)


end program zoa_program
