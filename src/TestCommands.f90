! The purpose of this is to have a baseline with a bunch of commands being run
! to catch issues during development
subroutine TestCommands
  use GLOBALS

  CALL PROCESKDP('MFL')
  CALL PROCESKDP('LIB P')
  CALL PROCESKDP('GLASSP')
  CALL PROCESKDP('GLASSP SCHOTT')
  CALL PROCESKDP('CV2PRG DoubleGauss.seq')
  CALL PROCESKDP('VIECO')
  CALL PROCESKDP('YFAN')
  CALL PROCESKDP('DRAWFAN')
  CALL PROCESKDP('AST 0')
  CALL PROCESKDP('PLTAST')
  CALL PROCESKDP('POWSYM')


! MACRODMP

end subroutine
