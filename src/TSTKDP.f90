

SUBROUTINE TSTKDP
   USE GLOBALS
   use DATSP1
   use DATHGR
   use DATCFG
   use DATSPD
   use DATSUB
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE


   INTEGER CATLIST


   !

   PRINT *, "Done with Init KDP"
!        CALL GCATLOAD(CATLIST)




   WRITE ( *, '(a)' ) '  Hello, world!'



!	    NRECL = 1
!        LIBLEN = 'LIBLEN/'


   CALL INITKDP

   !INPUT = 'LIB GET 1 '
   !CALL PROCES

   !INPUT = 'VIECO'
   !CALL PROCES

   !INPUT = 'GRAOUT BMP'
   !CALL PROCES

   ! INPUT = "COLOR RGB"
   ! CALL PROCES

   ! INPUT = "IIMAGEN,0.4479,0.3356,320.0,240.0,,"
   ! CALL PROCES

   ! INPUT = "OFROMBMP,0.40E+19,,,,,PORT"
   ! CALL PROCES

   ! INPUT = "PLTOBJ,,,,,,"
   ! CALL PROCES

!       INPUT = "IMTRACE1,,,,,"
!        CALL PROCES

!        INPUT = "PLTIMG,,,,,,"
!        CALL PROCES

! TGR,512.0,,,,,
!  NRD,64.0,,,,,
!  PGR,91.0,,,,,
!  IMTRACE1,,,,,,


!       Provide Continuous Command Line input
   DO WHILE (INPUT.NE."QUIT")

      WRITE( *,2)
2     FORMAT ('> ', $ )
      READ (*, '(a)') INPUT
      PRINT *, "INPUT = ", INPUT
      CALL PROCES
   END DO



!        CALL LLIB

!                        END IF
!

!        CALL LIBLOAD(1)
!        STOP
END

SUBROUTINE getOpticalSystemLastSurface(res)
   USE GLOBALS
   use DATLEN
   use DATMAI
   use mod_system, only: sys_last_surf
   use iso_fortran_env, only: real64
   IMPLICIT NONE

   INTEGER, intent(inout) :: res


   res = INT(sys_last_surf())

   !PRINT *, "Last Surface index TSTKDP is ", res


END

SUBROUTINE getFieldCalcResult(X1, X2, Y, numPts, calcCode)
   ! This routine currently assumes that the AST command was just run and
   ! gets the data from the output arrays.
   use DATHGR
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE

   integer, intent(in) :: calcCode

   CHARACTER UNN*9,DUNN*12,NNTT1*80,BLNOTE*80,BL20*20 &
   &,CRANGE*8,B*80,DTY*10,TMY*8,LABX*40,LABY*40
!
   real(real64) WOR1(0:50),WOR2(0:50),RANGE &
   &,FACTY,ORI,DTA11(0:50),DTA22(0:50),DDTA(0:50),ADTA(0:50)
!
   REAL LLIM,ULIM,UFLIM,LFLIM,DELX1,FLDAN(0:50)
!
   COMMON/FIFI/FLDAN
!
   REAL XRAN1,YRAN,YMINJK,XMINJK1,XMAXJK1,YMAXJK &
   &,XMINJK2,XMAXJK2,XRAN2,XRAN,XMAXJK,XMINJK,X(1:51)
!
   INTEGER NX,NY,COLPAS,MYJK,DFLAG,I,PNTNUM,NT1ANG,NT1SIZ
!
   COMMON/NUMPNT/PNTNUM,ORI,FACTY
!
   LOGICAL ASTEXT,FLDEXT,DISEXT,FDISEXT
   COMMON/FIELDEXT/ASTEXT,FLDEXT,DISEXT,FDISEXT
!
   COMMON/ABSSS/WOR1,WOR2,DTA11,DTA22,DDTA,ADTA

   REAL, intent(inout) :: X1(0:50), Y(0:50), X2(0:50)
   integer, intent(inout) :: numPts


!
!

   select case (calcCode)

    case (1)
      X1 = REAL(ADTA)
    case(2)
      X1 = REAL(DDTA)
    case(3)
      X1 = REAL(DTA11)
      X2 = REAL(DTA22)
   END SELECT

   Y  = REAL(FLDAN)
   numPts = PNTNUM
   !PRINT *, "X is ", X1
   !PRINT *, "Y is ", Y

end subroutine

subroutine refreshLensDataStruct()
   use global_widgets,only: curr_lens_data


   use iso_fortran_env, only: real64
   implicit none


   call curr_lens_data%update()


end subroutine

 !TODO:  Check for release flag and don't display if false
subroutine LogTermDebug(newTxt)

   use zoa_output, only : zoa_emit
   use iso_fortran_env, only: real64
   implicit none
   character(len=*) :: newTxt
   call zoa_emit(newTxt, "red")
   ! Also PRint
   PRINT *, newTxt

end subroutine


subroutine LogTermFOR(newTxt)
   use zoa_output, only : zoa_emit
   use iso_fortran_env, only: real64
   implicit none
   character(len=*) :: newTxt
   call zoa_emit(newTxt, "blue")

end subroutine

SUBROUTINE getRayTraceOutput(res)
   USE GLOBALS
   use DATLEN
   use DATMAI
   use iso_fortran_env, only: real64
   IMPLICIT NONE

   real(real64) res(1:50,0:499)


   res = RAYRAY

   !PRINT *, "Test RAYRAY Output TSTKDP", RAYRAY(2,4)

   PRINT *, "Size of RAYRAY is ", size(RAYRAY)

END

SUBROUTINE printMSG(msgText, code)
   use DATMAI, only: OUTLYNE
   character(len=*) :: msgText
   integer :: code

   OUTLYNE = msgText
   call SHOWIT(code)

END SUBROUTINE

SUBROUTINE getVIECOScaleFactor(scaleFactor)
   use DATLEN
   use iso_fortran_env, only: real64
   IMPLICIT NONE

   real(real64), INTENT(INOUT) :: scaleFactor

   scaleFactor = SCFAY

END

SUBROUTINE TSTREAL8()

   use DATLEN

   print *, "Precision is ", precision(PPY1)
   print *, "Range is ", range(PPY1)


END SUBROUTINE

SUBROUTINE PROCESSILENT(ftext)
   use global_widgets, only: ioConfig
   use zoa_ui

   use iso_fortran_env, only: real64
   use iso_c_binding, only: c_ptr
   implicit none

   character(len=*) :: ftext
   type(c_ptr) :: savedView

   ! Return to the exact view we started from: the command's own silent
   ! brackets redirect too, so restoreTextView() (single prev slot) would land
   ! on the hidden KDP dump and leave the console dead until TERM.
   savedView = ioConfig%currentTextView()
   call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)
   call PROCESKDP(ftext)
   call ioConfig%returnToTextView(savedView)

END SUBROUTINE






SUBROUTINE PROCESKDP(ftext)
   use DATMAI, only: INPUT
   use codeV_commands, only: dispatchCodeVSegment
   use iso_fortran_env, only: real64
   IMPLICIT NONE

   character(len=*), intent(in) :: ftext
   integer :: stIdx, scIdx, nLen

   ! Split the line on ';' and route each segment.  The CodeV front door
   ! (dispatchCodeVSegment) handles a registered new command directly -- it
   ! never enters the legacy parser (PROCES/PRO3/CONTRO/NAMES).  Anything it
   ! declines runs the classic way, INPUT = segment; CALL PROCES, which does
   ! its own tokenization and lens-update-level routing.  Splitting here also
   ! sidesteps the 140-char INPUT buffer for long ';'-joined lines (e.g. a
   ! regenerated "VIE P1; ...; GO"), whose trailing pieces would otherwise be
   ! truncated -- dropping the closing GO and leaving the plot loop open.
   nLen = len_trim(ftext)
   stIdx = 1
   do
      if (stIdx > nLen) exit
      scIdx = index(ftext(stIdx:nLen), ';')
      if (scIdx == 0) then
         call routeSegment(adjustl(ftext(stIdx:nLen)))
         exit
      else
         call routeSegment(adjustl(ftext(stIdx:stIdx+scIdx-2)))
         stIdx = stIdx + scIdx
      end if
   end do

contains

   ! One command segment: CodeV front door first, else the legacy parser.
   ! The legacy path keeps the segment's original case (PROCES/PRO2 upcases);
   ! only the front door folds case internally for its zoaCmds lookup.
   subroutine routeSegment(seg)
      character(len=*), intent(in) :: seg
      if (len_trim(seg) == 0) return
      if (dispatchCodeVSegment(seg)) return
      INPUT = seg
      CALL PROCES
   end subroutine

END


