module plot_command_utils
  implicit none

contains

function getKDPSpotPlotCommand(iField, iLambda, iSpotCalcMethod, nGrid, nRand, nRing) result(plotCmd)
    use type_utils, only: int2str
    use zoa_ui, only: ID_SPOT_RAND, ID_SPOT_RECT, ID_SPOT_RING
    use global_widgets, only: sysConfig
    use DATSP1
    implicit none
    integer, intent(in) :: iField, iLambda, iSpotCalcMethod
    integer, intent(in) :: nGrid, nRand, nRing

    character(len=80) :: charFLD
    character(len=80) :: charTrace
    character(len=1024) :: plotCmd
    integer :: i
    WRITE(charFLD, *) "FOB ", &
    & sysConfig%relativeFields(2,iField) &
    & , ' ' , sysConfig%relativeFields(1,iField)

    select case (iSpotCalcMethod)
    case (ID_SPOT_RAND)
      charTrace = "SPOT RAND;RANNUM "//trim(int2str(nRand))
    case (ID_SPOT_RECT)
      charTrace = "SPOT RECT;RECT "//trim(int2str(nGrid))
    case (ID_SPOT_RING)
      ! One ring pattern for the whole program: evenly spaced radii with six
      ! more rays per successive ring (SPD_SET_RING_PATTERN, WAVSPOT2), the
      ! same rule the startup/SPDRESET defaults use.  This used to build its
      ! own radii with an INT(rho*360) ray count -- ~3780 rays at 20 rings,
      ! and it never set the angular offsets, so it silently inherited
      ! whatever stagger the global pattern happened to hold.
      call SPD_SET_RING_PATTERN(nRing)

      charTrace = "SPOT RING;RINGS "//int2str(nRing)
    end select

    ! NOTE: no tracing here.  This builder is shared by the plot path and the
    ! optimizer's SPO evaluation (optim-types-callbacks getSPO), and it runs as
    ! the argument to PROCESSILENT -- i.e. before that call's silencing applies
    ! -- so anything emitted here leaks into the terminal once per merit-function
    ! evaluation during an optimization.
    plotCmd = trim(charFLD)//'; '//trim(charTrace)//";SPD "//trim(int2str(iLambda))
end function

end module plot_command_utils
