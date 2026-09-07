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
      ! This is a bit of a hack. Redistribute ring number and rays per ring
      ! using KDP vars. This should probably be moved to this type eventually.
      do i=1,nRing
            RINGRAD(i) = (REAL(i)/nRing)*1D0
            RINGPNT(i) = INT(RINGRAD(i)*360)
      end do

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
