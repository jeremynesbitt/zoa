! Ray trace diagnostics: the message catalog of the real ray tracer.
!
! The legacy tracer (real_ray_trace_core and the routines under it) reports a
! failing ray by printing, when the global MSG flag is on,
!
!     CALL RAY_FAILURE(surf)          " RAY FAILURE OCCURRED AT SURFACE n"
!     OUTLYNE = '<reason>'; CALL SHOWIT(1)      (or zoa_emit('<reason>', 'black'))
!
! The global-free engine (mod_ray_trace_engine) is PURE and cannot print.  It
! records WHICH message legacy would have printed, and at WHICH surface, as an
! id from this catalog; the caller prints the recorded messages serially with
! print_ray_messages, which uses the very same output routines legacy does, so
! every context that suppresses legacy output (PROCESSILENT, the KDPDUMP sink,
! OUT = 98 ...) suppresses the engine's messages identically.
!
! This module has two halves.  The id parameters and MAX_RAY_MSGS are plain
! constants that the pure modules (mod_surface_interaction,
! mod_surface_apertures, mod_ray_aiming, mod_ray_trace_engine) use with an
! `only:` list.  print_ray_messages is the impure half: it reads MSG and writes
! OUTLYNE, so it must never be reached from a pure routine, and nothing in the
! pure modules may use it.
!
! Catalog (id: legacy text, mechanism, surface argument)
!
!   MSG_TIR             'TOTAL INTERNAL REFLECTION'                 INTERACK     hit surface
!   MSG_TIR_NOT_MET     'TIR CONDITION NOT MET'                     INTERACK     hit surface
!   MSG_CLAP_*          'RAY BLOCKED BY <shape> CLEAR APERTURE'     CACHEK       blocking surface
!   MSG_COBS_*          'RAY BLOCKED BY <shape> OBSCURATION'        CACHEK       blocking surface
!   MSG_NO_AIM_SOLUTION RAY_FAILURE(NEWOBJ) only, no reason line    NEWDEL       NEWOBJ
!   MSG_AIM_NOT_CONVERGED   'RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT'
!                       (zoa_emit)    RAYTRA flavour of the core                 NEWREF
!   MSG_ZERO_WAVELENGTH 'RAY CAN NOT BE TRACED AT ZERO WAVELENGTH'
!                       (zoa_emit)    RAYTRA flavour of the core                 NEWOBJ
!
! Every message is printed only while MSG is on.  Legacy also had two
! ungated debug outputs on these paths -- a PRINT in the multiple-obscuration
! check and a log line NEWDEL wrote on every aiming step of one branch, for
! rays that succeed too -- and could leave MSG switched off after a
! multiple-aperture surface.  Those were removed from the legacy tracer
! rather than reproduced here.
module mod_ray_messages
   implicit none
   private

   public :: print_ray_messages

   ! No message.
   integer, parameter, public :: MSG_NONE = 0

   ! INTERACK (surface-interaction.f90)
   integer, parameter, public :: MSG_TIR = 1
   integer, parameter, public :: MSG_TIR_NOT_MET = 2

   ! CACHEK (surface-apertures.f90), clear aperture shapes CAFLG 1..6
   integer, parameter, public :: MSG_CLAP_CIRCLE = 3
   integer, parameter, public :: MSG_CLAP_RECT = 4
   integer, parameter, public :: MSG_CLAP_ELLIPSE = 5
   integer, parameter, public :: MSG_CLAP_RACETRACK = 6
   integer, parameter, public :: MSG_CLAP_POLYGON = 7
   integer, parameter, public :: MSG_CLAP_IPOLY = 8

   ! CACHEK, obscuration shapes COFLG 1..6
   integer, parameter, public :: MSG_COBS_CIRCLE = 9
   integer, parameter, public :: MSG_COBS_RECT = 10
   integer, parameter, public :: MSG_COBS_ELLIPSE = 11
   integer, parameter, public :: MSG_COBS_RACETRACK = 12
   integer, parameter, public :: MSG_COBS_POLYGON = 13
   integer, parameter, public :: MSG_COBS_IPOLY = 14

   ! NEWDEL (ray-aiming.f90)
   integer, parameter, public :: MSG_NO_AIM_SOLUTION = 15

   ! real_ray_trace_core itself (ray-trace-engine.f90, RAYTRA flavour only)
   integer, parameter, public :: MSG_AIM_NOT_CONVERGED = 16
   integer, parameter, public :: MSG_ZERO_WAVELENGTH = 17

contains

   ! Print n recorded messages, in order, exactly as legacy would have:
   ! ids(k) at surface surfs(k).  Like the legacy IF(MSG) it replaces, nothing
   ! is printed while MSG is off.
   subroutine print_ray_messages(n, ids, surfs)
      use DATMAI, only: OUTLYNE
      use DATLEN, only: MSG
      use zoa_output, only: zoa_emit
      integer, intent(in) :: n
      integer, intent(in) :: ids(:), surfs(:)
      integer :: k

      if (.not. MSG) return
      do k = 1, n
         select case (ids(k))
         case (MSG_TIR)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'TOTAL INTERNAL REFLECTION'
            call SHOWIT(1)
         case (MSG_TIR_NOT_MET)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'TIR CONDITION NOT MET'
            call SHOWIT(1)
         case (MSG_CLAP_CIRCLE)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY CIRCULAR CLEAR APERTURE'
            call SHOWIT(1)
         case (MSG_CLAP_RECT)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY RECTANGULAR CLEAR APERTURE'
            call SHOWIT(1)
         case (MSG_CLAP_ELLIPSE)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY ELLIPTICAL CLEAR APERTURE'
            call SHOWIT(1)
         case (MSG_CLAP_RACETRACK)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY RACETRACK CLEAR APERTURE'
            call SHOWIT(1)
         case (MSG_CLAP_POLYGON)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY POLYGON CLEAR APERTURE'
            call SHOWIT(1)
         case (MSG_CLAP_IPOLY)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY IRREGULAR POLYGON CLEAR APERTURE'
            call SHOWIT(1)
         case (MSG_COBS_CIRCLE)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY CIRCULAR OBSCURATION'
            call SHOWIT(1)
         case (MSG_COBS_RECT)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY RECTANGULAR OBSCURATION'
            call SHOWIT(1)
         case (MSG_COBS_ELLIPSE)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY ELLIPTICAL OBSCURATION'
            call SHOWIT(1)
         case (MSG_COBS_RACETRACK)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY RACETRACK OBSCURATION'
            call SHOWIT(1)
         case (MSG_COBS_POLYGON)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY POLYGON OBSCURATION'
            call SHOWIT(1)
         case (MSG_COBS_IPOLY)
            call RAY_FAILURE(surfs(k))
            OUTLYNE = 'RAY BLOCKED BY IRREGULAR POLYGON OBSCURATION'
            call SHOWIT(1)
         case (MSG_NO_AIM_SOLUTION)
            ! NEWDEL: the reason line ('SPECIFIED OBJECT POINT DOES NOT EXIST')
            ! is commented out in legacy
            call RAY_FAILURE(surfs(k))
            call SHOWIT(1)
         case (MSG_AIM_NOT_CONVERGED)
            call RAY_FAILURE(surfs(k))
            call zoa_emit('RAY FAILED TO CONVERGE TO REFERENCE SURFACE RAY-AIM POINT', 'black')
         case (MSG_ZERO_WAVELENGTH)
            call RAY_FAILURE(surfs(k))
            call zoa_emit('RAY CAN NOT BE TRACED AT ZERO WAVELENGTH', 'black')
         end select
      end do
   end subroutine print_ray_messages

end module mod_ray_messages
