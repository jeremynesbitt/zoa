! ray_failure: single entry point for all "RAY FAILURE OCCURRED AT SURFACE"
! messages.  Having one location means you can add a breakpoint, a counter,
! or extra diagnostics here instead of hunting across 11 files.
!
! Usage:
!   CALL RAY_FAILURE(surf_num)   -- always emits the message
!
! Callers are responsible for guarding with IF(MSG) when the failure should
! only be reported in verbose mode.  E.g.:
!
!   IF(MSG) THEN
!     CALL RAY_FAILURE(NEWOBJ)
!     OUTLYNE = 'CHIEF RAY FROM CURRENT OBJECT POINT COULD NOT BE TRACED'
!     CALL SHOWIT(1)
!   END IF

SUBROUTINE RAY_FAILURE(surf_num)
   use DATMAI, only: OUTLYNE
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: surf_num
   WRITE(OUTLYNE,*) 'RAY FAILURE OCCURRED AT SURFACE ', surf_num
   CALL SHOWIT(1)
END SUBROUTINE RAY_FAILURE
