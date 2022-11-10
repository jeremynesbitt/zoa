! The purpose of this is to have a baseline with a bunch of commands being run
! to catch issues during development
subroutine TestCommands
  use GLOBALS

  CALL PROCESKDP('MFL')
  CALL PROCESKDP('LIB P')


end subroutine
