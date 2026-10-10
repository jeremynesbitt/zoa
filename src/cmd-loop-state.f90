! Which CLI command loop is open (AUT, TAR, UPD CON, a plot's settings, ...),
! 0 when none.  Owned by codeV_commands, which re-exports it; kept in its own
! module so the GUI handlers can check it (no replot while a loop is open)
! without a module cycle through codeV_commands.
module cmd_loop_state
  implicit none
  integer :: cmd_loop = 0
end module cmd_loop_state
