

SUBROUTINE TSTKDP
   USE GLOBALS
   use DATSP1
   use DATHGR
   use DATCFG
   use DATSPD
   use DATSUB
   use DATLEN
   use DATMAI
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
   IMPLICIT NONE

   INTEGER, intent(inout) :: res


   res = INT(SYSTEM(20))

   !PRINT *, "Last Surface index TSTKDP is ", res


END

SUBROUTINE getFieldCalcResult(X1, X2, Y, numPts, calcCode)
   ! This routine currently assumes that the AST command was just run and
   ! gets the data from the output arrays.
   use DATHGR
   use DATLEN
   use DATMAI
   IMPLICIT NONE

   integer, intent(in) :: calcCode

   CHARACTER UNN*9,DUNN*12,NNTT1*80,BLNOTE*80,BL20*20 &
   &,CRANGE*8,B*80,DTY*10,TMY*8,LABX*40,LABY*40
!
   REAL*8 WOR1(0:50),WOR2(0:50),RANGE &
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


   implicit none


   call curr_lens_data%update()


end subroutine

 !TODO:  Check for release flag and don't display if false
subroutine LogTermDebug(newTxt)

   use zoa_output, only : zoa_emit
   implicit none
   character(len=*) :: newTxt
   call zoa_emit(newTxt, "red")
   ! Also PRint
   PRINT *, newTxt

end subroutine


subroutine LogTermFOR(newTxt)
   use zoa_output, only : zoa_emit
   implicit none
   character(len=*) :: newTxt
   call zoa_emit(newTxt, "blue")

end subroutine

SUBROUTINE getRayTraceOutput(res)
   USE GLOBALS
   use DATLEN
   use DATMAI
   IMPLICIT NONE

   REAL*8 res(1:50,0:499)


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
   IMPLICIT NONE

   REAL*8, INTENT(INOUT) :: scaleFactor

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

   implicit none

   character(len=*) :: ftext

   call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)
   call PROCESKDP(ftext)
   call ioConfig%restoreTextView()

END SUBROUTINE






SUBROUTINE PROCESKDP(ftext)
   use DATMAI, only: INPUT
   use strings
   IMPLICIT NONE

   character(len=*), intent(in) :: ftext
   character(len=80) :: tokens(40)
   integer :: numTokens, i

   !WRITE(OUTLYNE,*) "PROCESKDP START F5 = ", F5
   !CALL SHOWIT(19)

   if(len(ftext) > len(INPUT)) then
      PRINT *, "Ftext is ", ftext
      ! Assume this is because of multiple commands.  So split it up
      call parse(ftext, ';', tokens, numTokens)
      if (i > 0) then
         do i = 1,numTokens
            INPUT = trim(tokens(i))
            CALL PROCES
         end do
         RETURN
      else
         !  call LogTermFOR("Warning:  INput string too long!")
      end if
   end if

   INPUT = trim(ftext)
   CALL PROCES

   !WRITE(OUTLYNE,*) "PROCESKDP END F5 = ", F5
   !CALL SHOWIT(19)

END


