module plot_command_utils
  implicit none

contains

! Trace the spot diagram for field iField, wavelength iLambda: the field,
! the ray pattern (iSpotCalcMethod with its density nGrid / nRand / nRing),
! then SPD -- typed calls into the same handlers as the text commands
! "FOB y x; SPOT kind; RECT n; SPD wl".  Results are left where SPD leaves
! them (DSPOTT, the RMS registers).  Wrap in kdp_silent_begin/end to keep
! SPD's report out of the console.
subroutine runKDPSpot(iField, iLambda, iSpotCalcMethod, nGrid, nRand, nRing)
    use zoa_ui, only: ID_SPOT_RAND, ID_SPOT_RECT, ID_SPOT_RING
    use global_widgets, only: sysConfig
    use mod_kdp_api, only: kdp_fob, kdp_spot_pattern, kdp_spd
    implicit none
    integer, intent(in) :: iField, iLambda, iSpotCalcMethod
    integer, intent(in) :: nGrid, nRand, nRing

    call kdp_fob(sysConfig%relativeFields(2,iField), sysConfig%relativeFields(1,iField))
    select case (iSpotCalcMethod)
    case (ID_SPOT_RAND)
      call kdp_spot_pattern('RAND', nRand)
    case (ID_SPOT_RECT)
      call kdp_spot_pattern('RECT', nGrid)
    case (ID_SPOT_RING)
      ! One ring pattern for the whole program: evenly spaced radii with six
      ! more rays per successive ring (SPD_SET_RING_PATTERN, WAVSPOT2), the
      ! same rule the startup/SPDRESET defaults use.
      call SPD_SET_RING_PATTERN(nRing)
      call kdp_spot_pattern('RING', nRing)
    end select
    call kdp_spd(iLambda)
end subroutine

end module plot_command_utils
