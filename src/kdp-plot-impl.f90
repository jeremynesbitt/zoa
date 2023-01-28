module kdp_plot_impl

type point
  real :: x, y
end type

contains

subroutine plot_axes(pixY, pixX, marginY, marginX, axesColor)

  IMPLICIT NONE

  INTEGER :: COLPAS, pixY, pixX, axesColor, numTicks
  integer :: pen_up = 0
  integer :: pen_down = 1
  real :: marginY, marginX, originY, originX, widthX, widthY
  real :: tickScale = .01
  real :: tickSpacingX, tickSpacingY
  integer :: i

  INCLUDE 'DATHGR.INC'
  INCLUDE 'DATLEN.INC'
!     PLOTS A SET OF AXES WITH 10 DIVISION UP AND 5 ACROSS
  COLPAS=COLAXS
  CALL MY_COLTYP(COLPAS)

  ! Figure out Origin in Pixels
  originY = pixY/2.0
  originX = pixX/2.0
  widthY =  pixY-marginY
  widthX =  pixX-marginX

  CALL MY_PLOT(100, -100, 0,0,-10,10010,-10,7010)
  !CALL MY_PLOT(INT(widthX), INT(widthY), 1,0,-10,-10010,10,7010)
  !CALL MY_PLOT(INT(widthX), INT(widthY), 0,0,-10,-10010,10,7010)
  CALL MY_PLOT(INT(100), -1*INT(pixY-100), 1,0,-10,-10010,10,7010)
  CALL MY_PLOT(INT(pixX-100), -1*INT(pixY-100), 1,0,-10,-10010,10,7010)
  CALL MY_PLOT(INT(pixX-100), -1*INT(100), 1,0,-10,-10010,10,7010)
  CALL MY_PLOT(INT(100), -1*INT(100), 1,0,-10,-10010,10,7010)
  CALL MY_PLOT(INT(0), -1*INT(0), 0,0,-10,-10010,10,7010)

  !CALL MY_PLOT(INT(widthY), INT(widthX), 1,0,-10,-10010,10,7010)
  !CALL MY_PLOT(INT(widthY), INT(widthX), 0,0,-10,-10010,10,7010)
  !
  ! CALL MY_PLOT(INT(100*(originY-widthY/2)), INT(100*originX), 0,0,-10,10010,-10,7010)
  ! CALL MY_PLOT(INT(100*(originY+widthY/2)), INT(100*originX), 1,0,-10,-10010,10,7010)
  ! CALL MY_PLOT(INT(100*originY), INT(100*(originX-widthX/2)), 0,0,-10,10010,-10,7010)
  ! CALL MY_PLOT(INT(100*originY), INT(100*(originX+widthX/2)), 1,0,-10,-10010,10,7010)
  !
  ! PRINT *, "Axes X is ", INT(100*(originX-widthX/2))
  ! PRINT *, "Axes X is ", INT(100*(originX+widthX/2))
  !
  ! numTicks = 11
  ! do i = 1, numTicks
  !   tickSpacingX = widthX/(numTicks-1)
  !   tickSpacingY = widthY/(numTicks-1)
  !   CALL MY_PLOT(INT(100*(originY*(1-tickScale))), &
  !   &            INT(100*(originX-widthX/2+i*tickSpacingX)), &
  !   & pen_up,0,-10,10010,-10,7010)
  !   CALL MY_PLOT(INT(100*(originY*(1+tickScale))), &
  !   & INT(100*(originX-widthX/2+i*tickSpacingX)), &
  !   & pen_down,0,-10,10010,-10,7010)
  !
  !   CALL MY_PLOT(INT(100*(originY-widthY/2+i*tickSpacingY)), &
  !   & INT(100*(originX*(1-tickScale))), &
  !   & pen_up,0,-10,10010,-10,7010)
  !   CALL MY_PLOT(INT(100*(originY-widthY/2+i*tickSpacingY)),&
  !   & INT(100*(originX*(1+tickScale))),pen_down,0,-10,10010,-10,7010)
  !
  ! end do

end subroutine

end module
